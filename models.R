library(tidyverse)
library(magrittr)
library(arrow)
library(ggpubr)
library(tidymodels)
library(glmnet)
library(jsonlite)


main_theme <- theme(
  plot.title = element_text(size = 24, hjust = 0.5),
  axis.title = element_text(size = 20),
  axis.text = element_text(size = 12),
  strip.text = element_text(size = 16, face = "bold"),
  legend.title = element_text(size = 12),
  legend.text = element_text(size = 12)
)

tidymodels::tidymodels_prefer()


# Load data --------------------------------------------------------------
# You must run the data dump script before running this script.
# It can be found at airtable/data_dump.py

df_ <- read_parquet("data/data_dump.parquet") %>%
  select(-data) %>%
  mutate(
    calcs = map(calcs, fromJSON),
    sample_type = case_when(
      str_detect(tolower(sample_type), "nasal") ~ "nasal swab",
      str_detect(tolower(sample_type), "oral") ~ "oral swab",
      TRUE ~ sample_type
    )
  ) %>%
  unnest(calcs) %>%
  filter(
    str_detect(sample_type, "blood|swab", negate = TRUE),
    cutoff > 12
  )

df_ctrl <- df_ %>%
  filter(str_detect(group, "Control") | mortem == "post-mortem" | mpi == 0)

df_unknown <- df_ %>%
  setdiff(df_ctrl)

df_ctrl_sum <- df_ctrl %>%
  summarize(
    across(c(mpr, ms, auc), median),
    .by = c(sample_id, dilution, rxn_name, assay, cutoff, sample_type, mortem, group, mpi, reader)
  ) %>%
  mutate(
    # mpi == 0 samples were collected before any animals were infected.
    positive = factor(
      group != "Negative Control" & !(mpi %in% 0),
      levels = c(FALSE, TRUE),
      labels = c("Negative", "Positive")
    )
  )


# Model ------------------------------------------------------------------


# A small fixed ridge penalty keeps the fit stable when the classes are perfectly
# separated, which plain glm does not. With 3 predictors there is little to tune.
lr_wf <- workflow() %>%
  add_recipe(
    recipe(positive ~ mpr + ms + auc, data = df_ctrl_sum) %>%
      step_zv(all_predictors()) %>% # Remove predictors with zero variance.
      step_normalize(all_numeric_predictors()) # Center and scale numeric predictors.
  ) %>%
  add_model(
    logistic_reg(penalty = 0.01, mixture = 0) %>%
      set_engine("glmnet") %>%
      set_mode("classification")
  )

# roc_auc is the primary ranking metric.
diag_metrics <- metric_set(roc_auc, pr_auc, j_index, sensitivity, specificity)

# 5-fold CV repeated 3 times, grouped by sample so that no sample is in both
# the analysis and assessment sets.
fit_group <- function(data) {
  folds <- group_vfold_cv(data, group = sample_id, v = 5, repeats = 3, strata = positive)
  fit_resamples(
    lr_wf,
    resamples = folds,
    metrics = diag_metrics,
    control = control_resamples(save_pred = TRUE, event_level = "second")
  )
}

# Each sample_type x dilution x assay x cutoff combination gets its own model.
# Combinations need at least 5 samples of each class for 5-fold CV.
df_groups <- df_ctrl_sum %>%
  nest(data = -c(sample_type, dilution, assay, cutoff)) %>%
  mutate(
    n_neg = map_int(data, ~ n_distinct(.x$sample_id[.x$positive == "Negative"])),
    n_pos = map_int(data, ~ n_distinct(.x$sample_id[.x$positive == "Positive"]))
  )

skipped <- df_groups %>%
  filter(n_neg < 5 | n_pos < 5) %>%
  distinct(sample_type, dilution, n_neg, n_pos)
if (nrow(skipped) > 0) {
  message("Skipping combinations with fewer than 5 samples in a class:")
  print(skipped)
}

# Metrics are computed on the pooled out-of-fold predictions of each repeat, then
# averaged across repeats. With only ~2 negatives per fold for some sample types,
# per-fold AUCs are almost always 1 and averaging them overstates performance.
pooled_metrics <- function(res) {
  res %>%
    collect_predictions() %>%
    group_by(id) %>%
    diag_metrics(truth = positive, .pred_Positive, estimate = .pred_class, event_level = "second") %>%
    summarize(mean = mean(.estimate), std_err = sd(.estimate) / sqrt(n()), .by = .metric)
}

set.seed(42)
results <- df_groups %>%
  filter(n_neg >= 5, n_pos >= 5) %>%
  mutate(
    res = map(data, fit_group, .progress = TRUE),
    metrics = map(res, pooled_metrics),
    oof = map(res, collect_predictions, summarize = TRUE) # Out-of-fold predictions.
  )


# Compare combinations ---------------------------------------------------


df_metrics <- results %>%
  select(sample_type, dilution, assay, cutoff, metrics) %>%
  unnest(metrics) %>%
  select(sample_type, dilution, assay, cutoff, .metric, mean, std_err)

# Rank combinations within each sample type. Any combination whose roc_auc is
# within one standard error of the best is reported as equivalent.
ranked <- df_metrics %>%
  filter(.metric == "roc_auc") %>%
  arrange(sample_type, desc(mean))

best_by_type <- ranked %>%
  slice_max(mean, n=2, by = c(sample_type, assay)) %>%
  left_join(
    df_metrics %>%
      filter(.metric != "roc_auc") %>%
      select(-std_err) %>%
      pivot_wider(names_from = .metric, values_from = mean),
    by = c("sample_type", "dilution", "assay", "cutoff")
  )

print(best_by_type, n = Inf)

df_metrics %>%
  filter(.metric == "pr_auc") %>%
  ggplot(aes(factor(cutoff), factor(dilution), fill = mean)) +
  geom_tile() +
  geom_text(aes(label = round(mean, 2)), size = 3) +
  facet_grid(sample_type~assay, scales = "free", space="free") +
  coord_cartesian(expand = FALSE) +
  scale_fill_viridis_c(limits = c(0.5, 1)) +
  labs(
    title = "Cross-validated ROC AUC",
    x = "Cutoff",
    y = "Dilution",
    fill = "ROC AUC"
  ) +
  main_theme
ggsave("model_comparison.png", path = "figures/tissues", width = 14, height = 8, bg = "white")


# Final models -----------------------------------------------------------


# Refit each combination on all of its data for predicting unknowns.
final_fits <- results %>%
  select(sample_type, dilution, assay, cutoff, data) %>%
  mutate(fit = map(data, ~ fit(lr_wf, data = .x))) %>%
  select(-data)


# ROC --------------------------------------------------------------------


# ROC curves use out-of-fold predictions so they are not optimistic. For each
# sample_type x dilution x assay, the cutoff with the best roc_auc is shown.
best_cutoffs <- df_metrics %>%
  filter(.metric == "roc_auc") %>%
  slice_max(mean, n = 1, with_ties = FALSE, by = c(sample_type, dilution, assay))

df_rocs <- results %>%
  semi_join(best_cutoffs, by = c("sample_type", "dilution", "assay", "cutoff")) %>%
  mutate(roc = map(oof, ~ roc_curve(.x, positive, .pred_Positive, event_level = "second"))) %>%
  select(sample_type, dilution, assay, cutoff, roc) %>%
  unnest(roc)

df_rocs %>%
  arrange(desc(specificity), sensitivity) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = assay)) +
  geom_step(direction = "hv") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_grid(sample_type~dilution) +
  scale_color_manual(values = c("red", "navy")) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.25)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) +
  coord_equal() +
  labs(
    x = "1 - Specificity",
    y = "Sensitivity",
  ) +
  main_theme +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.9),
    legend.title = element_blank(),
    # legend.background = element_blank()
  )
ggsave("roc1.png", path = "figures/tissues", width = 10, height = 12)


# Pairwise Metric Correlation --------------------------------------------


df_cor <- df_ctrl_sum %>%
  select(mpr, ms, auc, positive, assay, cutoff, sample_type) %>%
  mutate(across(c(mpr, ms, auc), ~ .x + abs(min(.x)))) %>%
  mutate(across(c(mpr, ms, auc), log)) %>%
  mutate(cutoff = factor(cutoff, levels = sort(unique(cutoff)), labels = paste(sort(unique(cutoff)), "hr"))) %>%
  arrange(desc(positive))

metric_combos <- c("mpr", "ms", "auc") %>%
  combn(2) %>%
  t() %>%
  as.data.frame() %>%
  rename(x = 1, y = 2)

make_cor_plot <- function(df, x, y, color_group, alpha = 0.1) {
  df %>%
    ggplot(aes(.data[[x]], .data[[y]])) +
    geom_point(
      aes(color = .data[[color_group]]),
      alpha = alpha, size = 2
    ) +
    scale_color_manual(values = c("darkblue", "darkorange")) +
    facet_grid(cols=vars(cutoff)) +
    labs(
      x = toupper(x),
      y = toupper(y),
      color = str_to_title(color_group)
    ) +
    guides(
      color = guide_legend(override.aes = list(alpha = 1, size = 6, shape = "square")),
      shape = guide_legend(override.aes = list(alpha = 1, size = 6))
    ) +
    main_theme +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 24),
      legend.key = element_rect(fill = "white", color = "white"),
    )
}

cor_plots <- pmap(
  metric_combos,
  make_cor_plot,
  df = df_cor,
  color_group = "positive",
  alpha = 0.1
)

ggarrange(
  plotlist = cor_plots, align = "hv", legend = "bottom", ncol=1,
  common.legend = TRUE, font.label = list(size = 30)
) 

ggsave("corplot.png", path = "figures/tissues", width = 16, height = 12, bg = "white")


# Apply to unknown data --------------------------------------------------


df_unknown_sum <- df_unknown %>%
  summarize(
    across(c(mpr, ms, auc), median),
    .by = c(sample_id, dilution, rxn_name, assay, cutoff, sample_type, mortem, group, mpi, reader)
  )

# Each unknown is scored by the model for its own combination. Combinations
# without a model (e.g. no labelled positives) are dropped.
df_unknown_preds <- df_unknown_sum %>%
  nest(data = -c(sample_type, dilution, assay, cutoff)) %>%
  inner_join(final_fits, by = c("sample_type", "dilution", "assay", "cutoff")) %>%
  mutate(data = map2(fit, data, ~ augment(.x, new_data = .y))) %>%
  select(-fit) %>%
  unnest(data)

# Only plot the recommended combinations.
df_unknown_preds %>%
  semi_join(best_by_type, by = c("sample_type", "dilution", "assay", "cutoff")) %>%
  arrange(mpi) %>%
  mutate(mpi = as.factor(mpi)) %>%
  ggplot(aes(mpi, .pred_Positive, color = group)) +
  geom_boxplot() +
  facet_wrap(~ sample_type + assay + dilution + cutoff, labeller = label_both) +
  scale_color_manual(values = c("navy", "red")) +
  labs(y = "P(Positive)", x = "MPI") +
  main_theme

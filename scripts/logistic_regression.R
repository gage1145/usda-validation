library(tidyverse)
library(quicR)
library(arrow)
library(magrittr)
library(plotly)
library(ggpubr)
library(airtabler)
library(janitor)
library(tidymodels)
library(car)
library(pROC)
library(latex2exp)
library(glmnet)
library(ranger)
library(kernlab)
library(themis)


main_theme <- theme(
  plot.title = element_text(size=24, hjust=0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=12),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)

tidymodels::tidymodels_prefer()


# Load data --------------------------------------------------------------


APP <- "app7KsgYl2jhOnYg7"
tables <- airtable(APP, c("samples", "animals", "reactions"))
samples <- tables$samples$select_all()
animals <- tables$animals$select_all()
reactions <- tables$reactions$select_all()

df_animals <- animals %>%
  clean_names() %>%
  select(animal_id, group)

df_samples <- samples %>%
  rename(rxn_id = reactions) %>%
  select(-c("id", "results", "concentration", "technician_id", "sample_type_id", "animal", "createdTime")) %>%
  unnest(where(is.list)) %>%
  filter(
    sample_type %in% c("RAMALT", "obex", "RPLN", "PLN")
  ) %>%
  left_join(df_animals, by = "animal_id")

df_reactions <- reactions %>%
  clean_names() %>%
  rename(rxn_id = id, reaction = rxn_name) %>%
  select(rxn_id, reaction, technician, assay, reader, date) %>%
  unnest(where(is.list))

df_samples %<>% left_join(df_reactions, by = "rxn_id")

df_results <- read_parquet("data/processedSamples/calcs.parquet") %>%
  clean_names(replace = c("Sample IDs" = "sample_id", "TtT" = "ttt")) %>%
  mutate(across(c(mpr, ms, auc), log)) 

df_ <- df_results %>%
  inner_join(df_samples, by=c("sample_id", "reaction", "assay")) %>%
  rename(process_tech = tech_name, rxn_tech = technician) %>%
  mutate(
    across(c(sample_type, animal_id, dilutions, mortem, assay, well, process_tech, rxn_tech, reader), as.factor)
  )

df_ctrl <- df_ %>%
  filter(str_detect(group, "Control") | mortem == "post-mortem" | mpi == 0) 

df_unknown <- df_ %>%
  setdiff(df_ctrl)

df_ctrl_sum <- df_ctrl %>%
  summarize(
    across(c(mpr, ms, auc), median), 
    .by = c(sample_id, dilutions, reaction, assay, sample_type, mortem, group, mpi, process_tech, rxn_tech, reader)
  ) %>%
  mutate(
    positive = factor(group != "Negative Control", levels = c(FALSE, TRUE), labels = c("Negative", "Positive"))
  )

# ROC Analysis -----------------------------------------------------------



# df_ctrl_sum_long <- df_ctrl_sum %>%
#   pivot_longer(
#     cols = c(mpr, ms, auc),
#     names_to = "metric",
#     values_to = "value"
#   )

# pca_res <- prcomp(~ mpr + ms + auc, data = df_ctrl_sum)
# summary(pca_res)
# df_ctrl_sum$pc1 <- pca_res$x[,1]

# get_roc <- function(df, dilutions, assay, sample_type, ...) {
#   temp_df <- df %>%
#     filter(dilutions == !!dilutions, assay == !!assay, sample_type == !!sample_type)

#   tryCatch({
#       temp_roc <- roc(positive ~ pc1, data = temp_df, ci = TRUE)
#       temp_roc$assay <- assay
#       temp_roc$sample_type <- sample_type
#       temp_roc$dilutions <- dilutions
#       return(temp_roc)
#     },
#     error = function(e) return(NULL)
#   )
# }

# combos <- distinct(df_ctrl_sum, dilutions, assay, sample_type)

# rocs <- pmap(combos, get_roc, df = df_ctrl_sum, .progress = TRUE)
# df_rocs <- rocs %>%
#   map_dfr(function(x) {
#     tibble(
#       sensitivity = list(x$sensitivities),
#       specificity = list(x$specificities),
#       lower = x$ci[1],
#       area = x$ci[2],
#       upper = x$ci[3],
#       assay = x$assay,
#       sample_type = x$sample_type,
#       dilutions = x$dilutions
#     )
#   }) %>%
#   mutate(
#     label_y = ifelse(assay == "RT-QuIC", 0.4, 0.25),
#     dilutions = factor(
#       dilutions, 
#       levels = c("-4", "-3", "-2"), 
#       labels = latex2exp::TeX(paste0("$10^{", c("-4", "-3", "-2"), "}$"))
#     )
#   ) %>%
#   filter(!is.na(area))

# df_roc_long <- df_rocs %>%
#   select(assay, dilutions, sample_type, sensitivity, specificity) %>%
#   unnest(where(is.list)) %>%
#   arrange(desc(specificity), sensitivity)

# # ROC Curves
# roc_plot <- ggplot(df_rocs) +
#   geom_line(
#     aes(x = 1 - specificity, y = sensitivity, color = assay),
#     data = df_roc_long
#   ) +
#   geom_area(
#     aes(x = 1 - specificity, y = sensitivity, fill = assay),
#     data = df_roc_long,
#     linejoin = "mitre", position = "identity", alpha = 0.2, show.legend=FALSE
#   ) +
#   geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
#   geom_label(
#     aes(x = 0.65, y = label_y, label = paste(assay, "=", signif(area, 3)), color = assay),
#     hjust = 0, label.padding = unit(0.5, "lines"), show.legend = FALSE
#   ) +
#   facet_grid(sample_type ~ dilutions, labeller = label_parsed) +
#   scale_color_manual(values = c("red", "navy")) +
#   scale_x_continuous(breaks = seq(0, 1, by = 0.25)) +
#   scale_y_continuous(breaks = seq(0, 1, by = 0.25), position = "right") +
#   coord_equal() +
#   # coord_cartesian(expand=FALSE) +
#   labs(
#     x = "1 - Specificity",
#     y = "Sensitivity",
#   ) +
#   guides(color = guide_legend(override.aes = list(linewidth = 6))) +
#   main_theme +
#   theme(
#     plot.title = element_text(size = 24, hjust = 0.5),
#     axis.title = element_text(size = 20),
#     axis.title.y = element_text(vjust = 1),
#     axis.text = element_text(size = 16),
#     strip.text = element_text(size = 20, face = "bold"),
#     legend.title = element_blank(),
#     legend.text = element_text(size = 20),
#     legend.position = "inside",
#     legend.position.inside = c(0.05, 0.95),
#     legend.key.spacing.y = unit(0.5, "cm"),
#     legend.background = element_blank(),
#   )
# roc_plot

# # AUC Plot
# auc_plot <- df_rocs %>%
#   mutate(title = "AUC") %>%
#   ggplot(aes(dilutions, area, color = assay, ymax = upper, ymin = lower)) +
#   geom_point(position = position_dodge(width = 0.5), size = 6) +
#   geom_errorbar(position = position_dodge(width = 0.5), width = 0.2) +
#   facet_grid(rows=vars(sample_type), cols=vars(title), labeller = label_parsed, space = "free", scales = "free_x") +
#   # scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
#   scale_color_manual(values = c("red", "navy", "darkgreen")) +
#   scale_x_discrete(labels = label_parse()) +
#   scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(NA, 1)) +
#   # coord_flip() +
#   # coord_cartesian(ylim = c(0.5, 1)) +
#   labs(
#     x = "Log10 Dilution Factor",
#     y = "Area Under the Curve",
#     color = ""
#   ) +
#   guides(color = guide_legend(override.aes = list(linewidth = 6))) +
#   main_theme +
#   theme(
#     plot.title = element_text(size = 24, hjust = 0.5),
#     axis.title = element_text(size = 20),
#     axis.text = element_text(size = 16),
#     # strip.text = element_text(size = 20, face = "bold"),
#     legend.title = element_blank(),
#     legend.text = element_text(size = 20),
#     legend.position = "inside",
#     legend.position.inside = c(0.05, 0.95),
#     legend.key.spacing.y = unit(0.5, "cm"),
#     legend.key.spacing.x = unit(1.5, "cm"),
#     legend.background = element_blank(),
#     strip.text = element_blank()
#   )
# auc_plot

# ggarrange(
#   auc_plot, roc_plot, ncol = 2, common.legend = TRUE, widths = c(1, 1.5), align = "h",
#   legend = "bottom"
# )

# ggsave("roc1.png", path = "figures/tissues", width = 12, height = 8)

# best_performers <- df_rocs %>%
#   filter(area == max(area), .by = c(sample_type))


# Logistic Model ---------------------------------------------------------


# yardstick treats the first factor level as the "positive" event by default.
# Our factor is c("Negative", "Positive"), so without this, roc_auc etc. would
# measure performance at predicting "Negative". This flips the event to "Positive".
options(yardstick.event_first = FALSE)


# 5-fold CV repeated 3 times produces 15 assessment sets, giving stable metric
# estimates. strata = positive ensures each fold preserves the class ratio.
# Bootstraps are used instead if the dataset is too small for 5-fold splitting.
set.seed(42)
if (nrow(df_ctrl_sum) < 60) {
  folds <- bootstraps(df_ctrl_sum, times = 25, strata = positive)
} else {
  folds <- vfold_cv(df_ctrl_sum, v = 5, repeats = 3, strata = positive)
}

# Check class balance before building the recipe. If one class has 3x as many
# samples as the other, add SMOTE to the recipe to oversample the minority class.
class_counts <- count(df_ctrl_sum, positive)
imbalance_ratio <- max(class_counts$n) / min(class_counts$n)

# A recipe is a preprocessing blueprint applied consistently within each CV fold
# (preventing data leakage from the assessment set into the analysis set).
base_recipe <- recipe(positive ~ mpr + ms + auc + dilutions + assay + sample_type,
                      data = df_ctrl_sum) |>
  # Remove any predictor with zero variance (e.g., a single reader in a fold).
  step_zv(all_predictors()) |>
  # Center and scale numeric predictors. Required for glmnet's L1/L2 penalty to
  # treat predictors fairly, and for the SVM's RBF kernel distance to be meaningful.
  step_normalize(all_numeric_predictors()) |>
  # One-hot encode factors (dilutions, assay, sample_type) into numeric columns.
  step_dummy(all_nominal_predictors()) |>
  # Add an ms x auc interaction term. Both metrics capture kinetic curve shape
  # and are correlated, so their interaction captures a non-additive effect.
  step_interact(terms = ~ mpr*ms*auc)

# SMOTE generates synthetic minority-class samples by interpolating between real
# ones in feature space. over_ratio = 0.8 upsamples to 80% of the majority count
# (less aggressive than 1:1, reducing the risk of overfitting on synthetic data).
if (imbalance_ratio > 3) {
  base_recipe <- base_recipe |> step_smote(positive, over_ratio = 0.8)
}

# Elastic net: penalty (lambda) controls regularization strength; mixture (alpha)
# blends L1 (lasso, alpha=1) and L2 (ridge, alpha=0). tune() marks them as
# hyperparameters to be searched over a grid.
lr_spec <- logistic_reg(penalty = tune(), mixture = tune()) |>
  set_engine("glmnet") |>
  set_mode("classification")

# Random forest: mtry = predictors sampled per split (tuned); trees = 500 is
# fixed for stability; min_n = minimum node size before splitting (tuned).
# probability = TRUE tells ranger to output class probabilities, required for roc_auc.
rf_spec <- rand_forest(mtry = tune(), trees = 500, min_n = tune()) |>
  set_engine("ranger", importance = "impurity", probability = TRUE) |>
  set_mode("classification")

# SVM with RBF kernel: cost (C) penalizes margin violations; rbf_sigma (gamma)
# controls kernel width. High gamma = tight fit around each training point (risk
# of overfitting); low gamma = broader, smoother influence.
svm_spec <- svm_rbf(cost = tune(), rbf_sigma = tune()) |>
  set_engine("kernlab") |>
  set_mode("classification")

# workflow_set crosses the one recipe with the three model specs, producing
# base_logistic, base_rf, and base_svm workflows.
# option_add attaches a per-workflow tuning grid:
#   - Logistic: 100-point regular grid (10 levels each for penalty and mixture),
#     spanning penalty 1e-4 to 1e0 on a log scale.
#   - RF/SVM: 20-point Latin hypercube, a space-filling design that covers the
#     parameter space evenly without the exponential blowup of a full grid.
wf_set <- workflow_set(
  preproc = list(base = base_recipe),
  models  = list(logistic = lr_spec, rf = rf_spec, svm = svm_spec)
) |>
  option_add(
    grid = grid_regular(penalty(range = c(-4, 0)), mixture(), levels = 2), #10
    id = "base_logistic"
  ) |>
  option_add(
    grid = grid_space_filling(mtry(range = c(1, 9)), min_n(), size = 4), #20
    id = "base_rf"
  ) |>
  option_add(
    grid = grid_space_filling(cost(), rbf_sigma(), size = 4), #20
    id = "base_svm"
  )

# roc_auc is the primary ranking metric. pr_auc (precision-recall AUC) is more
# informative than roc_auc under class imbalance. 
# j_index (Youden's J = sensitivity + specificity - 1) summarizes the ROC curve 
# at its optimal threshold.
diag_metrics <- metric_set(roc_auc, pr_auc, j_index, sensitivity, specificity)

# Parallelise across all cores minus one. Tuning is embarrassingly parallel over
# folds x parameter combinations, so this can cut runtime by 4-8x.
doParallel::registerDoParallel(parallel::detectCores() - 1)

# workflow_map runs tune_grid on every workflow using the per-workflow grids set
# above. save_pred = TRUE stores held-out predictions from each fold, which are
# needed to compute roc_auc and pr_auc (both require predicted probabilities).
# parallel_over = "everything" parallelises across both resamples and grid points.
tuned_results <- wf_set |>
  workflow_map(
    fn        = "tune_grid",
    resamples = folds,
    metrics   = diag_metrics,
    control   = control_grid(save_pred = TRUE, parallel_over = "everything"),
    verbose   = TRUE
  )

# Summarise the best CV roc_auc for each workflow and rank them. The dot-plot
# from autoplot shows whether one model family is clearly superior or if the
# three models are within noise of each other.
rank_results(tuned_results, rank_metric = "roc_auc", select_best = TRUE) |>
  select(model, .metric, mean) |>
  pivot_wider(id_cols = c(model), names_from = .metric, values_from = c(mean))

autoplot(tuned_results, metric = "roc_auc") +
  main_theme +
  labs(title = "Model Comparison: ROC AUC")
ggsave("model_comparison.png", path = "figures/tissues", width = 10, height = 6)

# Programmatically select the winning workflow ID (e.g., "base_rf") and the
# specific hyperparameter combination within it that had the best mean CV roc_auc.
best_wfs <- tuned_results |>
  rank_results(rank_metric = "roc_auc") |>
  filter(.metric == "roc_auc") |>
  group_by(wflow_id) |>
  slice_min(rank, n = 1) |>
  ungroup()

best_wf_id <- best_wfs |>
  slice_min(rank,) |>
  pull(wflow_id)

get_best_params <- function(id) {
  tuned_results |>
    extract_workflow_set_result(id = id) |>
    select_best(metric = "roc_auc")
}

best_params <- map(unique(best_wfs$wflow_id), get_best_params)

# finalize_workflow replaces the tune() placeholders with the best found values.
# fit() then trains on the entire labelled dataset. CV was only for hyperparameter
# selection and generalisation estimation; the final model uses all available data.
get_final_wf <- function(id, params) {
  tuned_results |>
    extract_workflow(id = id) |>
    finalize_workflow(params)
}

final_wfs <- map2(best_wfs$wflow_id, best_params, get_final_wf)
names(final_wfs) <- best_wfs$wflow_id

final_fits <- map(final_wfs, fit, data = df_ctrl_sum)
final_fit <- final_fits[[best_wf_id]]

# Print the CV performance summary for the winning model: mean and standard error
# of each metric across all folds. These are honest out-of-sample estimates.
# tuned_results |>
#   extract_workflow_set_result(id = best_wf_id) |>
#   collect_metrics() |>
#   filter(.metric %in% c("roc_auc", "pr_auc", "j_index", "sensitivity", "specificity")) |>
#   print(n=Inf)

# Variable importance is only meaningful for tree-based models. glmnet has
# coefficients and SVM has support vectors, neither of which vip plots the same way.
if (grepl("rf", best_wf_id)) {
  final_fit |>
    extract_fit_parsnip() |>
    vip::vip()
  ggsave("variable_importance.png", path = "figures/tissues", width = 8, height = 6)
}


# ROC --------------------------------------------------------------------
get_predictions <- function(mod, df) {
  augment(mod, new_data = df) %>%
    mutate(
      engine = extract_spec_parsnip(mod)$engine,
    )
}

df_pred <- map_dfr(final_fits, get_predictions, df=df_ctrl_sum)

get_filtered_roc <- function(df, assay, sample_type, dilutions, engine, ...) {
  tryCatch(
    {
      df |>
        filter(assay == !!assay, sample_type == !!sample_type, dilutions == !!dilutions, engine == !!engine) |>
        roc(positive, .pred_Positive) |>
        coords() |>
        mutate(assay = assay, sample_type = sample_type, dilutions = dilutions, engine = engine)
    }, 
    error = function(e) return(NULL)
  )
}

combos <- distinct(df_pred, assay, sample_type, dilutions, engine)
df_rocs <- pmap_dfr(combos, get_filtered_roc, df = df_pred, .progress = TRUE)

df_rocs %>%
  arrange(desc(specificity), sensitivity) |>
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = engine, linetype=assay)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_grid(sample_type ~ dilutions, labeller = label_parsed) +
  scale_color_manual(values = c("red", "navy", "darkgreen")) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.25)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), position = "right") +
  coord_equal() +
  # coord_cartesian(expand=FALSE) +
  labs(
    x = "1 - Specificity",
    y = "Sensitivity",
  )

# Explanation for log scaling --------------------------------------------
df_cor <- df_ctrl_sum %>%
  select(mpr, ms, auc, positive, assay) %>%
  rename_with(~ paste0("log_", .), c(mpr, ms, auc)) %>%
  mutate(
    mpr = exp(log_mpr),
    ms = exp(log_ms),
    auc = exp(log_auc),
    positive = as.character(positive)
  ) 

metric_combos <- c("mpr", "ms", "auc") %>%
  combn(2) %>%
  t() %>%
  as.data.frame() %>%
  rename(x = 1, y = 2) %>%
  bind_rows(
    mutate(
      ., 
      across(everything(), ~ paste0("log_", .x)), 
      .keep = "unused"
    )
  )

make_cor_plot <- function(df, x, y, color_group, shape_group, alpha = 0.1) {
  df %>%
    ggplot(aes(.data[[x]], .data[[y]])) +
    geom_point(aes(color = .data[[color_group]], shape = .data[[shape_group]]), alpha = alpha, size=3) +
    scale_color_manual(values = c("navy", "red")) +
    scale_shape_manual(values = c(1, 2)) +
    labs(
      x = toupper(x),
      y = toupper(y),
      color = str_to_title(color_group),
      shape = str_to_title(shape_group)
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
  shape_group = "assay",
  alpha = 0.5
)

ggarrange(
  plotlist = cor_plots, align = "hv", legend="bottom", 
  common.legend = TRUE, font.label = list(size = 30)
) %>%
  annotate_figure(
    top = text_grob(
      "Metric Correlation", 
      color = "black", 
      face = "bold", 
      size = 30
    ),
    left = text_grob(
      "Log-transformed         |         Untransformed", 
      color = "black", 
      face = "bold", 
      size = 30, 
      rot=90
    )
)

ggsave("corplot.png", path = "figures/tissues", width = 16, height = 12, bg = "white")





# Apply to unknown data --------------------------------------------------


df_unknown_sum <- df_unknown |>
  summarize(
    across(c(mpr, ms, auc), median),
    .by = c(sample_id, dilutions, reaction, assay, sample_type, mortem, group, mpi, process_tech, rxn_tech, reader)
  )

df_unknown_preds <- augment(final_fit, new_data = df_unknown_sum)

positioning <- position_jitterdodge(jitter.width = 3, dodge.width = 3, seed = 45)

df_unknown_preds |>
  # filter()
  # summarize(
  #   stdev = sd(.pred_Positive),
  #   .pred_Positive = mean(.pred_Positive),
  #   .by = c(mpi, assay, dilutions, group)
  # ) |> 
  arrange(mpi) |>
  mutate(mpi = as.factor(mpi)) |>
  ggplot(aes(mpi, .pred_Positive, color = group, fill = group)) +
  geom_boxplot() +
  # geom_point(position = positioning) +
  # geom_ribbon(aes(ymin = .pred_Positive - stdev, ymax = .pred_Positive + stdev), position = positioning, width = 0.2) +
  # geom_line(position = positioning) +
  # stat_smooth(se = TRUE) +
  # facet_grid(dilutions ~ assay) +
  scale_color_manual(values = c("navy", "red")) +
  scale_fill_manual(values = c("navy", "red")) +
  labs(y = "P(Positive)", x = "MPI") +
  main_theme

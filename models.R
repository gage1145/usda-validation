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
library(vip)


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

df_ <- read_parquet("data/data_dump.parquet")

df_ctrl <- df_ %>%
  filter(str_detect(group, "Control") | mortem == "post-mortem" | mpi == 0)

df_unknown <- df_ %>%
  setdiff(df_ctrl)

df_ctrl_sum <- df_ctrl %>%
  summarize(
    across(c(mpr, ms, auc), median),
    .by = c(
      sample_id, dilution, rxn_name, assay, sample_type, mortem, group, mpi, reader
    )
  ) %>%
  mutate(
    positive = factor(
      group != "Negative Control",
      levels = c(FALSE, TRUE),
      labels = c("Negative", "Positive")
    )
  )


# Pre-processing ---------------------------------------------------------


options(yardstick.event_first = FALSE)

# 5-fold CV repeated 3 times for 15 assessment sets.
set.seed(42)
folds <- vfold_cv(df_ctrl_sum, v = 5, repeats = 3, strata = positive)

# Generate a recipe that will apply equally to all folds.
base_recipe <- recipe(
  positive ~ mpr + ms + auc + dilution + assay + sample_type,
  data = df_ctrl_sum
) %>%
  step_zv(all_predictors()) %>% # Remove predictors with zero variance.
  step_normalize(all_numeric_predictors()) %>% # Center and scale numeric predictors.
  step_dummy(all_nominal_predictors()) %>% # Encode factors into numeric columns.
  step_interact(terms = ~ mpr * ms * auc) # Add interaction terms between metrics.

# Check class balance before building the recipe.
class_counts <- count(df_ctrl_sum, positive)
imbalance_ratio <- max(class_counts$n) / min(class_counts$n)
print(imbalance_ratio)

# Interpolate values for the minority class if the ratio is greater than 3:1.
if (imbalance_ratio > 3) {
  base_recipe <- base_recipe %>% step_smote(positive, over_ratio = 0.8)
}


# Models -----------------------------------------------------------------


# Logistic Regression
lr_spec <- logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

# Random Forest
rf_spec <- rand_forest(mtry = tune(), trees = 500, min_n = tune()) %>%
  set_engine("ranger", importance = "impurity", probability = TRUE) %>%
  set_mode("classification")

# Support Vector Machine
svm_spec <- svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")


# Workflow ---------------------------------------------------------------


# Apply the recipe to the models.
wf_set <- workflow_set(
  preproc = list(base = base_recipe),
  models  = list(logistic = lr_spec, rf = rf_spec, svm = svm_spec)
) %>%
  option_add( # Options for the logistic model.
    grid = grid_regular(penalty(range = c(-4, 0)), mixture(), levels = 2), # 10
    id = "base_logistic"
  ) %>%
  option_add( # Options for the random forest model.
    grid = grid_space_filling(mtry(range = c(1, 9)), min_n(), size = 4), # 20
    id = "base_rf"
  ) %>%
  option_add( # Options for the SVM model.
    grid = grid_space_filling(cost(), rbf_sigma(), size = 4), # 20
    id = "base_svm"
  )

# roc_auc is the primary ranking metric.
# pr_auc (precision-recall AUC) is more informative than roc_auc under class imbalance.
# j_index (Youden's J) summarizes the ROC curve at its optimal threshold.
diag_metrics <- metric_set(roc_auc, pr_auc, j_index, sensitivity, specificity)

# Parallelise across all cores minus one.
doParallel::registerDoParallel(parallel::detectCores() - 1)


# Run the workflow -------------------------------------------------------


# Run the workflow.
tuned_results <- wf_set %>%
  workflow_map(
    fn        = "tune_grid",
    resamples = folds,
    metrics   = diag_metrics,
    control   = control_grid(save_pred = TRUE, parallel_over = "everything"),
    verbose   = TRUE
  )

# Summarise the best CV roc_auc for each workflow and rank them.
rank_results(tuned_results, rank_metric = "roc_auc", select_best = TRUE) %>%
  select(model, .metric, mean) %>%
  pivot_wider(id_cols = c(model), names_from = .metric, values_from = c(mean))

# Plot performance comparisons of the model families.
autoplot(tuned_results, metric = "roc_auc") +
  main_theme +
  labs(title = "Model Comparison: ROC AUC")
ggsave("model_comparison.png", path = "figures/tissues", width = 10, height = 6)


# Extract best performing workflows --------------------------------------


# Select the winning workflow IDs for each model family.
best_wfs <- tuned_results %>%
  rank_results(rank_metric = "roc_auc") %>%
  filter(.metric == "roc_auc") %>%
  group_by(wflow_id) %>%
  slice_min(rank, n = 1) %>%
  ungroup()

# Select the all-around best workflow ID.
best_wf_id <- best_wfs %>%
  slice_min(rank) %>%
  pull(wflow_id)

# Pull the best parameters for each workflow.
best_params <- map(
  unique(best_wfs$wflow_id),
  function(id) {
    tuned_results %>%
      extract_workflow_set_result(id = id) %>%
      select_best(metric = "roc_auc")
  }
)

# Generalize the final model to all available data.
final_wfs <- map2(
  best_wfs$wflow_id,
  best_params,
  function(id, params) {
    tuned_results %>%
      extract_workflow(id = id) %>%
      finalize_workflow(params)
  }
)
names(final_wfs) <- best_wfs$wflow_id

final_fits <- map(final_wfs, fit, data = df_ctrl_sum)
final_fit <- final_fits[[best_wf_id]]

# Print the CV performance summary for the winning model: mean and standard error
# of each metric across all folds. These are honest out-of-sample estimates.
tuned_results %>%
  extract_workflow_set_result(id = best_wf_id) %>%
  collect_metrics() %>%
  filter(.metric %in% c("roc_auc", "pr_auc", "j_index", "sensitivity", "specificity")) %>%
  print(n = Inf)

# Variable importance is only meaningful for the random forest model.
if (grepl("rf", best_wf_id)) {
  final_fit %>%
    extract_fit_parsnip() %>%
    vip()
  ggsave("variable_importance.png", path = "figures/tissues", width = 8, height = 6)
}


# ROC --------------------------------------------------------------------


df_pred <- map_dfr(
  final_fits,
  function(mod, df) {
    mod %>%
      augment(new_data = df) %>%
      mutate(engine = extract_spec_parsnip(mod)$engine)
  },
  df = df_ctrl_sum
)

get_filtered_roc <- function(df, assay, sample_type, dilution, engine, ...) {
  tryCatch(
    {
      df %>%
        filter(assay == !!assay, sample_type == !!sample_type, dilution == !!dilution, engine == !!engine) %>%
        roc(positive, .pred_Positive) %>%
        coords() %>%
        mutate(assay = assay, sample_type = sample_type, dilution = dilution, engine = engine)
    },
    error = function(e) {
      return(NULL)
    }
  )
}

combos <- distinct(df_pred, assay, sample_type, dilution, engine)
df_rocs <- pmap_dfr(combos, get_filtered_roc, df = df_pred, .progress = TRUE)

df_rocs %>%
  arrange(desc(specificity), sensitivity) %>%
  ggplot(aes(x = 1 - specificity, y = sensitivity, color = engine, linetype = assay)) +
  geom_line() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_grid(sample_type ~ dilution, labeller = label_parsed) +
  scale_color_manual(values = c("red", "navy", "darkgreen")) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.25)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), position = "right") +
  coord_equal() +
  labs(
    x = "1 - Specificity",
    y = "Sensitivity",
  )
ggsave("roc1.png", path = "figures/tissues", width = 10, height = 12)


# Pairwise Metric Correlation --------------------------------------------


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
    geom_point(
      aes(color = .data[[color_group]], shape = .data[[shape_group]]),
      alpha = alpha, size = 3
    ) +
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
  plotlist = cor_plots, align = "hv", legend = "bottom",
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
      rot = 90
    )
  )

ggsave("corplot.png", path = "figures/tissues", width = 16, height = 12, bg = "white")


# Apply to unknown data --------------------------------------------------


df_unknown_sum <- df_unknown %>%
  summarize(
    across(c(mpr, ms, auc), median),
    .by = c(sample_id, dilution, rxn_name, assay, sample_type, mortem, group, mpi, reader)
  )

df_unknown_preds <- augment(final_fit, new_data = df_unknown_sum)

positioning <- position_jitterdodge(jitter.width = 3, dodge.width = 3, seed = 45)

df_unknown_preds %>%
  arrange(mpi) %>%
  mutate(mpi = as.factor(mpi)) %>%
  ggplot(aes(mpi, .pred_Positive, color = group, fill = group)) +
  geom_boxplot() +
  scale_color_manual(values = c("navy", "red")) +
  scale_fill_manual(values = c("navy", "red")) +
  labs(y = "P(Positive)", x = "MPI") +
  main_theme

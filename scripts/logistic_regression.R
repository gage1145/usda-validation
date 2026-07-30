library(tidyverse)
library(quicR)
library(arrow)
library(magrittr)
library(modelr)
library(plotly)
library(emmeans)


df_ <- read_parquet("data/processedSamples/calcs.parquet") %>%
  mutate(across(c(MPR, MS, AUC), log))

df_ctrl <- df_ %>%
  filter(`Sample IDs` %in% c("N", "P")) %>%
  summarize(across(c(MPR, MS, AUC), mean), .by = c(`Sample IDs`, Reaction, Assay)) %>%
  mutate(positive = as.integer(`Sample IDs` == "P"))

multi_mod <- glm(positive ~ MPR + MS + AUC + Assay, data = df_ctrl, family = "binomial")

df_unknown <- df_ %>%
  filter(!(`Sample IDs` %in% c("N", "P"))) %>%
  summarize(across(c(MPR, AUC, MS), median), .by = c(`Sample IDs`, Dilutions, Assay)) %>%
  add_predictions(mpr_mod, type = "response")


# Logistic regression -----------------------------------------------------






# Numerically solve the decision boundary via root-finding.
# Works with any model (categorical predictors, interactions, etc.) because
# it calls predict() directly rather than manipulating coefficients.
#
# For each (MPR, AUC) row in the grid and each combination of fixed_vars,
# uniroot finds the value of solve_var where p = p_target.
solve_boundary <- function(model, grid_vars, solve_var,
                           fixed_vars = list(), n_obs = 100, p_target = 0.5) {
  seqs <- lapply(grid_vars, function(rng) seq(rng[1], rng[2], length.out = n_obs))
  grid <- do.call(expand.grid, seqs)
  interval <- range(model$model[[solve_var]], na.rm = TRUE)

  grid[[solve_var]] <- apply(grid, 1, function(row) {
    tryCatch(
      uniroot(function(v) {
        nd <- as.data.frame(c(as.list(row), setNames(list(v), solve_var), fixed_vars))
        predict(model, newdata = nd, type = "response") - p_target
      }, interval = interval)$root,
      error = function(e) NA_real_
    )
  })
  grid
}

n_obs    <- 2
mpr_seq  <- seq(min(df_ctrl$MPR), max(df_ctrl$MPR), length.out = n_obs)
auc_seq  <- seq(min(df_ctrl$AUC), max(df_ctrl$AUC), length.out = n_obs)

# One boundary surface per Assay level
assay_levels   <- unique(df_ctrl$Assay)
surface_colors <- c("cyan", "orange")

boundaries <- lapply(assay_levels, function(lvl) {
  solve_boundary(
    model      = multi_mod,
    grid_vars  = list(MPR = range(df_ctrl$MPR), AUC = range(df_ctrl$AUC)),
    solve_var  = "MS",
    fixed_vars = list(Assay = lvl),
    n_obs      = n_obs
  )
})

# ms_mat <- t(matrix(boundaries$MS, nrow = n_obs))

# Build plot: add one surface trace per Assay level
# expand.grid varies MPR fastest, so matrix(, nrow=n_obs) gives [MPR, AUC];
# add_surface expects z[i,j] = value at x[j], y[i], so transpose.
plt <- plot_ly() 

for (i in seq_along(assay_levels)) {
  ms_mat <- t(matrix(boundaries[[i]]$MS, nrow = n_obs))
  plt <- plt %>%
    add_surface(
      x          = mpr_seq,
      y          = auc_seq,
      z          = ms_mat,
      colorscale = list(c(0, surface_colors[i]), c(1, surface_colors[i])),
      opacity    = 0.4,
      showscale  = FALSE,
      name       = assay_levels[i]
    )
}

plt %>%
  add_markers(
    data   = df_unknown,
    x = ~MPR, y = ~AUC, z = ~MS,
    type   = "scatter3d",
    mode   = "markers",
    color  = ~Assay,
    colors = c("#3357FF", "#FF5733"),
    marker = list(
      size = 4
    )
  ) %>%
  layout(
    title = "Logistic Regression Decision Boundary (P = 0.5) in MPR \u00d7 AUC \u00d7 MS Space",
    scene = list(
      aspectmode = "cube",
      xaxis = list(title = "MPR"),
      yaxis = list(title = "AUC"),
      zaxis = list(title = "MS")
    )
  )
    
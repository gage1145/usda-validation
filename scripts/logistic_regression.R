library(tidyverse)
library(quicR)
library(arrow)
library(magrittr)
library(modelr)
library(plotly)
library(emmeans)
library(ggeffects)
library(ggpubr)
library(airtabler)
library(janitor)
library(tidymodels)

main_theme <- theme(
  plot.title = element_text(size=24, hjust=0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=12),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)


APP <- "app7KsgYl2jhOnYg7"
tables <- airtable(APP, c("samples", "animals"))
samples <- tables$samples$select_all()
animals <- tables$animals$select_all()

df_animals <- animals %>%
  clean_names() %>%
  select(animal_id, group)

df_samples <- samples %>%
  rename() %>%
  select(2:8) %>%
  unnest(where(is.list)) %>%
  filter(
    sample_type %in% c("RAMALT", "obex", "RPLN", "PLN")
  ) %>%
  left_join(df_animals, by = "animal_id")

df_ <- read_parquet("data/processedSamples/calcs.parquet") %>%
  clean_names(replace = c("Sample IDs" = "sample_id", "TtT" = "ttt")) %>%
  mutate(across(c(mpr, ms, auc), log)) %>%
  inner_join(df_samples, by = "sample_id") %>%
  mutate(
    across(c(sample_type, animal_id, mortem, group, assay, wells), as.factor)
  )

df_ctrl <- df_ %>%
  filter(str_detect(group, "Control") | mortem == "post-mortem" | mpi == 0) 

df_unknown <- df_ %>%
  setdiff(df_ctrl)

df_ctrl_sum <- df_ctrl %>%
  summarize(across(c(mpr, ms, auc), median), .by = c(sample_id, dilutions, reaction, assay, sample_type, mortem, group, mpi)) %>%
  mutate(
    positive = as.integer(group != "Negative Control")
  )

multi_mod <- glm(positive ~ mpr + ms + auc + dilutions + assay + sample_type, data = df_ctrl_sum, family = "binomial")

df_unknown_sum <- df_unknown %>%
  summarize(across(c(mpr, ms, auc), median), .by = c(sample_id, dilutions, assay, sample_type)) %>%
  add_predictions(multi_mod, type = "response")


# Numerically solve the decision boundary via root-finding.
# Works with any model (categorical predictors, interactions, etc.) because
# it calls predict() directly rather than manipulating coefficients.
#
# For each (MPR, AUC) row in the grid and each combination of fixed_vars,
# uniroot finds the value of solve_var where p = p_target.
# solve_boundary <- function(model, grid_vars, solve_var,
#                            fixed_vars = list(), n_obs = 100, p_target = 0.5) {
#   seqs <- lapply(grid_vars, function(rng) seq(rng[1], rng[2], length.out = n_obs))
#   grid <- do.call(expand.grid, seqs)
#   interval <- range(model$model[[solve_var]], na.rm = TRUE)

#   grid[[solve_var]] <- apply(grid, 1, function(row) {
#     tryCatch(
#       uniroot(function(v) {
#         nd <- as.data.frame(c(as.list(row), setNames(list(v), solve_var), fixed_vars))
#         predict(model, newdata = nd, type = "response") - p_target
#       }, interval = interval)$root,
#       error = function(e) NA_real_
#     )
#   })
#   grid
# }

# n_obs    <- 2
# mpr_seq  <- seq(min(df_ctrl_sum$mpr), max(df_ctrl_sum$mpr), length.out = n_obs)
# auc_seq  <- seq(min(df_ctrl_sum$auc), max(df_ctrl_sum$auc), length.out = n_obs)

# # One boundary surface per Assay level
# assay_levels   <- unique(df_ctrl_sum$assay)
# surface_colors <- c("cyan", "orange")

# boundaries <- lapply(assay_levels, function(lvl) {
#   solve_boundary(
#     model      = multi_mod,
#     grid_vars  = list(mpr = range(df_ctrl_sum$mpr), auc = range(df_ctrl_sum$auc)),
#     solve_var  = "ms",
#     fixed_vars = list(assay = lvl),
#     n_obs      = n_obs
#   )
# })

# plt <- plot_ly() 

# for (i in seq_along(assay_levels)) {
#   ms_mat <- t(matrix(boundaries[[i]]$ms, nrow = n_obs))
#   plt <- plt %>%
#     add_surface(
#       x          = mpr_seq,
#       y          = auc_seq,
#       z          = ms_mat,
#       colorscale = list(c(0, surface_colors[i]), c(1, surface_colors[i])),
#       opacity    = 0.4,
#       showscale  = FALSE,
#       name       = assay_levels[i]
#     )
# }

# plt %>%
#   add_markers(
#     data   = df_unknown,
#     x = ~mpr, y = ~auc, z = ~ms,
#     type   = "scatter3d",
#     mode   = "markers",
#     color  = ~assay,
#     colors = c("#3357FF", "#FF5733"),
#     marker = list(
#       size = 4
#     )
#   ) %>%
#   layout(
#     title = "Logistic Regression Decision Boundary (P = 0.5) in MPR \u00d7 AUC \u00d7 MS Space",
#     scene = list(
#       aspectmode = "cube",
#       xaxis = list(title = "mpr"),
#       yaxis = list(title = "auc"),
#       zaxis = list(title = "ms")
#     )
#   )



# Plot this shit ---------------------------------------------------------


combos <- distinct(df_, assay, dilutions, sample_type)

df_pred <- ggpredict(multi_mod, c("mpr [all]", "ms", "auc", "assay", "sample_type"))

plts <- map(levels(df_$sample_type), function(type) {
  df_pred %>%
    as.data.frame() %>%
    mutate(facet = paste("log(AUC) =", as.character(facet))) %>% 
    filter(grid == type) %>%
    ggplot(aes(x, predicted, color = group, fill = group)) +
    geom_line(linewidth = 1) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
    facet_grid(panel ~ facet) +
    labs(
      x = "",
      y = "",
      color = "log(MS)", 
      fill = "log(MS)",
      title = toupper(type)
    ) +
    guides(fill = guide_legend(override.aes = list(alpha = 1))) +
    theme(
      strip.text = element_text(size = 24),
      legend.title = element_text(size = 24),
      legend.text = element_text(size = 16),
      legend.key.height = unit(1, "cm"),
      legend.key.spacing.y = unit(0.5, "cm"),
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 24, hjust = 0.5),
      plot.title = element_text(size = 24, hjust = 0.5),
    )
  }
)

ggarrange(plotlist = plts, nrow = 2, ncol = 2, common.legend = TRUE, legend = "right") %>%
  annotate_figure(
    bottom = text_grob("log(MPR)", size = 24, vjust = 0),
    left = text_grob("Probability Positive", rot = 90, size = 24, vjust = 1)
  ) 

ggsave(filename = "logistic_regression.png", path = "figures/tissues", width = 24, height = 16, bg = "white")

# Explanation for log scaling
df_cor <- df_ctrl_sum %>%
  select(mpr, ms, auc, positive) %>%
  rename_with(~ paste0("log_", .), c(mpr, ms, auc)) %>%
  mutate(
    mpr = exp(log_mpr),
    ms = exp(log_ms),
    auc = exp(log_auc),
    positive = ifelse(positive == 0, "Negative", "Positive")
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

make_cor_plot <- function(df, x, y, group, alpha = 0.1) {
  df %>%
    ggplot(aes(.data[[x]], .data[[y]])) +
    geom_point(aes(color = .data[[group]]), alpha = alpha) +
    scale_color_manual(values = c("navy", "red")) +
    labs(
      x = toupper(x),
      y = toupper(y),
      color = str_to_title(group)
    ) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 6, shape = "square"))) +
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
  group = "positive",
  alpha = 0.25
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

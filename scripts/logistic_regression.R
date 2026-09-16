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
library(car)
library(pROC)
library(latex2exp)


main_theme <- theme(
  plot.title = element_text(size=24, hjust=0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=12),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)


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
    across(c(sample_type, animal_id, dilutions, mortem, group, assay, wells, process_tech, rxn_tech, reader), as.factor)
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
    positive = as.integer(group != "Negative Control")
  )

# ROC Analysis -----------------------------------------------------------



df_ctrl_sum_long <- df_ctrl_sum %>%
  pivot_longer(
    cols = c(mpr, ms, auc),
    names_to = "metric",
    values_to = "value"
  )

pca_res <- prcomp(~ mpr + ms + auc, data = df_ctrl_sum)
summary(pca_res)
df_ctrl_sum$pc1 <- pca_res$x[,1]

get_roc <- function(df, dilutions, assay, sample_type, ...) {
  temp_df <- df %>%
    filter(dilutions == !!dilutions, assay == !!assay, sample_type == !!sample_type)

  tryCatch({
      temp_roc <- roc(positive ~ pc1, data = temp_df, ci = TRUE)
      temp_roc$assay <- assay
      temp_roc$sample_type <- sample_type
      temp_roc$dilutions <- dilutions
      return(temp_roc)
    },
    error = function(e) return(NULL)
  )
}

combos <- distinct(df_ctrl_sum, dilutions, assay, sample_type)

rocs <- pmap(combos, get_roc, df = df_ctrl_sum, .progress = TRUE)
df_rocs <- rocs %>%
  map_dfr(function(x) {
    tibble(
      sensitivity = list(x$sensitivities),
      specificity = list(x$specificities),
      lower = x$ci[1],
      area = x$ci[2],
      upper = x$ci[3],
      assay = x$assay,
      sample_type = x$sample_type,
      dilutions = x$dilutions
    )
  }) %>%
  mutate(
    label_y = ifelse(assay == "RT-QuIC", 0.4, 0.25),
    dilutions = factor(
      dilutions, 
      levels = c("-4", "-3", "-2"), 
      labels = latex2exp::TeX(paste0("$10^{", c("-4", "-3", "-2"), "}$"))
    )
  ) %>%
  filter(!is.na(area))

df_roc_long <- df_rocs %>%
  select(assay, dilutions, sample_type, sensitivity, specificity) %>%
  unnest(where(is.list)) %>%
  arrange(desc(specificity), sensitivity)

# ROC Curves
roc_plot <- ggplot(df_rocs) +
  geom_line(
    aes(x = 1 - specificity, y = sensitivity, color = assay),
    data = df_roc_long
  ) +
  geom_area(
    aes(x = 1 - specificity, y = sensitivity, fill = assay),
    data = df_roc_long,
    linejoin = "mitre", position = "identity", alpha = 0.2, show.legend=FALSE
  ) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_label(
    aes(x = 0.65, y = label_y, label = paste(assay, "=", signif(area, 3)), color = assay),
    hjust = 0, label.padding = unit(0.5, "lines"), show.legend = FALSE
  ) +
  facet_grid(sample_type ~ dilutions, labeller = label_parsed) +
  scale_color_manual(values = c("red", "navy")) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.25)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25), position = "right") +
  coord_equal() +
  # coord_cartesian(expand=FALSE) +
  labs(
    x = "1 - Specificity",
    y = "Sensitivity",
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 6))) +
  main_theme +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.title.y = element_text(vjust = 1),
    axis.text = element_text(size = 16),
    strip.text = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.position = "inside",
    legend.position.inside = c(0.05, 0.95),
    legend.key.spacing.y = unit(0.5, "cm"),
    legend.background = element_blank(),
  )
roc_plot

# AUC Plot
auc_plot <- df_rocs %>%
  mutate(title = "AUC") %>%
  ggplot(aes(dilutions, area, color = assay, ymax = upper, ymin = lower)) +
  geom_point(position = position_dodge(width = 0.5), size = 6) +
  geom_errorbar(position = position_dodge(width = 0.5), width = 0.2) +
  facet_grid(rows=vars(sample_type), cols=vars(title), labeller = label_parsed, space = "free", scales = "free_x") +
  # scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(0, 1)) +
  scale_color_manual(values = c("red", "navy", "darkgreen")) +
  scale_x_discrete(labels = label_parse()) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2), limits = c(NA, 1)) +
  # coord_flip() +
  # coord_cartesian(ylim = c(0.5, 1)) +
  labs(
    x = "Log10 Dilution Factor",
    y = "Area Under the Curve",
    color = ""
  ) +
  guides(color = guide_legend(override.aes = list(linewidth = 6))) +
  main_theme +
  theme(
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    # strip.text = element_text(size = 20, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.position = "inside",
    legend.position.inside = c(0.05, 0.95),
    legend.key.spacing.y = unit(0.5, "cm"),
    legend.key.spacing.x = unit(1.5, "cm"),
    legend.background = element_blank(),
    strip.text = element_blank()
  )
auc_plot

ggarrange(
  auc_plot, roc_plot, ncol = 2, common.legend = TRUE, widths = c(1, 1.5), align = "h",
  legend = "bottom"
)

ggsave("roc1.png", path = "figures/tissues", width = 12, height = 8)

best_performers <- df_rocs %>%
  filter(area == max(area), .by = c(sample_type))


# Logistic Model ---------------------------------------------------------


multi_mod <- glm(
  positive ~ mpr + ms + auc + ms:auc + dilutions + assay + sample_type,
  data = df_ctrl_sum, 
  family = "binomial",
)
summary(multi_mod)

best_aic_model <- stats::step(multi_mod, direction = "both")
summary(best_aic_model)

# Predictors with the lowest p-values contribute the most unique information to 
# the theoretical framework.
Anova(multi_mod, type = "III", test.statistic = "LR")

odds_ratios <- exp(cbind(OR = coef(multi_mod), confint(multi_mod)))
print(odds_ratios)

odds_ratios %>%
  as_tibble() %>%
  mutate(
    across(everything(), log),
    effect = (rownames(odds_ratios))
  ) %>%
  arrange(OR) %>%
  ggplot(aes(fct_inorder(effect), OR)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = `2.5 %`, ymax = `97.5 %`)) +
  labs(
    x = "",
    y = "Log Odds Ratio"
  )

vif(multi_mod)

df_unknown_sum <- df_unknown %>%
  summarize(
    across(c(mpr, ms, auc), median), 
    .by = c(sample_id, dilutions, reaction, assay, sample_type, mortem, group, mpi, process_tech, rxn_tech, reader)
  ) %>%
  add_predictions(multi_mod, type = "response") 


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



# Apply to unknown data --------------------------------------------------


df_unknown_sum %>%
  # add_predictions(multi_mod, type = "response") %>%
  # mutate(mpi = as.factor(mpi)) %>%
  ggplot(aes(mpi, pred, color=group, fill=group)) +
  # geom_boxplot() +
  geom_point(position = position_jitterdodge(jitter.width = 3, dodge.width = 3)) +
  stat_smooth(se = TRUE) +
  facet_grid(dilutions ~ assay) +
  scale_color_manual(values = c("navy", "red")) +
  scale_fill_manual(values = c("navy", "red")) 

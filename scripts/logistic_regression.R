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
library(lme4)

main_theme <- theme(
  plot.title = element_text(size=24, hjust=0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=12),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)


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
    across(c(sample_type, animal_id, mortem, group, assay, wells, process_tech, rxn_tech, reader), as.factor)
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

multi_mod <- glm(
  positive ~ mpr + ms + auc + dilutions + assay + sample_type,
  data = df_ctrl_sum, 
  family = "binomial",
)
summary(multi_mod)

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

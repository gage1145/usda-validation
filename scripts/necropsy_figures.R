library(dplyr)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(forcats)
library(stringr)
library(airtabler)
source("scripts/airtable_functions.R")



# Retrieve data from Airtable ---------------------------------------------



APP <- "app7KsgYl2jhOnYg7"

# Get the necessary tables
tables <- airtable(APP, c("animals", "samples", "results", "reactions"))

animals <- tables$animals$select_all()

# Filter for post-mortem samples
samples <- tables$samples$select_all(
  filterByFormula = "{mortem} = 'post-mortem'"
) %>%
  rename("sample" = "id") %>%
  select("sample", "sample_id", "animal_id")

# Filter results for post-mortem samples
results <- tables$results$select_all(
  filterByFormula = get_formula("sample", samples$sample_id)
)

# Combine data frames
results <- results %>%
  mutate_at(c("sample", "reaction"), as.character) %>%
  left_join(samples, "sample")



# Theme -------------------------------------------------------------------



main_theme <- theme(
  plot.title = element_text(size=24, hjust=0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=16),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)



# Format the data ---------------------------------------------------------



df_ <- results %>%
  mutate(
    sample_id = as.factor(sample_id),
    animal_id = as.factor(as.character(animal_id)),
    assay = factor(assay, level=c("RT-QuIC", "Nano-QuIC")),
    sample_type = as.factor(as.character(sample_type)),
    dilution = factor(
      dilution, 
      levels=c(-2, -3, -4), 
      labels=c("10^{-2}", "10^{-3}", "10^{-4}")
    )
  )

df_sum <- df_ %>%
  group_by(sample_id, animal_id, dilution, assay, sample_type) %>%
  summarize(
    median_raf = median(raf)
  )

df_sum_sum <- df_ %>%
  group_by(sample_type, assay, dilution) %>%
  summarize(
    mean_raf = mean(raf),
    sd_raf = sd(raf),
    max_raf = max(raf),
    min_raf = min(raf)
  )



# Tissue Boxplot ----------------------------------------------------------



df_ %>%
  ggplot(aes(fct_inorder(animal_id), raf, fill = assay)) +
  geom_line(aes(y=median_raf, color=assay, group=assay), data=df_sum, linewidth=1) +
  geom_boxplot(outliers = FALSE, linewidth=0.25) +
  facet_grid(vars(fct_rev(dilution)), vars(sample_type), space = "free", labeller=label_parsed) +
  scale_color_manual(values=c("darkslateblue", "darkorange")) +
  scale_fill_manual(values=c("darkslateblue", "darkorange")) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Dilution Factor", breaks = NULL)) +
  labs(
    title="Comparing Assay Kinetics between Tissues",
    y="Rate of Amyloid Formation (1/s)"
  ) +
  main_theme +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
    legend.position = c(0.85, 0.85),
    legend.title = element_blank(),
    legend.background = element_blank()
  )
ggsave("RAFs.png", path="figures/necropsy", width=12, height=6)



# Tissue Histograms -------------------------------------------------------



df_ %>%
  filter(ttt != 72) %>%
  ggplot(aes(raf, fct_rev(dilution), fill = assay)) +
  geom_density_ridges(
    scale=4, 
    rel_min_height=0.001, 
    panel_scaling=FALSE, 
    alpha=0.6
  ) +
  facet_grid(vars(sample_type), scale = "free") +
  scale_color_manual(values=c("darkslateblue", "darkorange")) +
  scale_fill_manual(values=c("darkslateblue", "darkorange")) +
  scale_y_discrete(expand=c(0.2, 0)) +
  labs(
    title="Density of Assay Kinetics",
    x="Rate of Amyloid Formation (1/h)",
    y="Log Dilution Factors"
  ) +
  main_theme +
  theme(
    axis.text.y = element_text(size=16),
    legend.position.inside = TRUE,
    legend.position = c(0.8, 0.93),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.background = element_blank(),
    panel.background = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color="darkgrey"),
    panel.border = element_blank()
  )
ggsave(
  "histograms.png", 
  path="figures/necropsy", width=16, height=8
)



# Mean RAF figures --------------------------------------------------------



df_sum_sum %>%
  ggplot(aes(
    dilution, 
    mean_raf,
    ymax=mean_raf + sd_raf,
    ymin=mean_raf - sd_raf,
    fill=assay
  )) +
  geom_col(position="dodge", color="black") +
  geom_errorbar(position=position_dodge(0.9), width=0.4) +
  facet_grid(~sample_type, scales="free_x", space="free_x") +
  scale_fill_manual(values=c("darkslateblue", "darkorange")) +
  scale_y_continuous(breaks=seq(0, 0.2, 0.02)) +
  labs(
    title="General Assay Comparisons",
    y="Mean RAF",
    x="Log Dilution Factors"
  ) +
  main_theme +
  theme(
    legend.position = c(0.9, 0.9),
    legend.background = element_blank(),
    legend.title = element_blank()
  )
ggsave("mean_RAF_col.png", path="figures/necropsy", width=16, height=8)

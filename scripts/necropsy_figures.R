library(dplyr)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(forcats)
library(stringr)
library(airtabler)
source("scripts/airtable_functions.R")
library(janitor)



# Retrieve data from Airtable ---------------------------------------------



APP <- "app7KsgYl2jhOnYg7"

# Get the necessary tables
tables <- airtable(APP, c("animals", "samples", "results", "reactions"))

animals <- tables$animals$select_all() %>%
  rename("animal" = "animal_id")

# Filter results for post-mortem samples
results <- tables$results$select_all(
  filterByFormula = "{mortem} = 'post-mortem'"
)



# Theme -------------------------------------------------------------------



main_theme <- theme(
  plot.title = element_text(size=24, hjust=0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=16),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)


# Format the data --------------------------------------------------------



df_ <- results %>%
  mutate(across(everything(), as.character)) %>%
  left_join(animals, by = "animal") %>%
  clean_names() %>%
  mutate(
    across(c(sample_id, animal, sample_type, group, room_number), as.factor),
    across(c(mpr, raf, ttt, ms, auc), as.numeric),
    assay = factor(assay, levels = c("RT-QuIC", "Nano-QuIC")),
    dilution = factor(
      dilution, 
      levels=c(-2, -3, -4), 
      # labels=c(bquote("10^{-2}"), bquote("10^{-3}"), bquote("10^{-4}"))
    )
  )

df_sum <- df_ %>%
  group_by(animal, dilution, assay, sample_type) %>%
  summarize(
    median_raf = median(raf),
    std_err = sd(raf) / sqrt(n()),
    upper = median_raf + std_err,
    lower = median_raf - std_err
  ) %>%
  mutate(dilution = factor(dilution, levels = c("-2", "-3", "-4"), labels=c(bquote("10^{-2}"), bquote("10^{-3}"), bquote("10^{-4}"))))



# Tissue Boxplot ----------------------------------------------------------



df_sum %>%
  ggplot(aes(fct_inorder(animal), y=median_raf, ymin=lower, ymax=upper, color=assay, group=assay, fill = assay)) +
  geom_line(linewidth=1) +
  geom_point() +
  geom_ribbon(alpha = 0.4, color = NA) +
  facet_grid(vars(fct_rev(dilution)), vars(sample_type), space = "free", labeller=label_parsed) +
  scale_color_manual(values=c("darkcyan", "darkorange")) +
  scale_fill_manual(values=c("darkcyan", "darkorange")) +
  labs(
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
  scale_color_manual(values=c("darkcyan", "darkorange")) +
  scale_fill_manual(values=c("darkcyan", "darkorange")) +
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


library(ggsignif)
library(scales)

df_animal <- df_ %>%
  summarize(raf=mean(raf), .by=c(animal, sample_type, assay, dilution))

df_ %>%
  group_by(sample_type, assay, dilution) %>%
  summarize(
    mean_raf = mean(raf),
    sd_raf = sd(raf),
    max_raf = max(raf),
    min_raf = min(raf),
    mean_ttt = paste0("TtT = ", round(mean(ttt), 1), "hr"),
    ttt_pos = mean_raf + 0.01
  ) %>%
  ggplot(aes(
    dilution, 
    mean_raf,
    ymax=mean_raf + sd_raf,
    ymin=mean_raf - sd_raf,
    fill=assay,
    group = assay
  )) +
  geom_col(position="dodge", color="black") +
  geom_errorbar(position=position_dodge(0.9), width=0.4) +
  geom_label(aes(label=mean_ttt, y=ttt_pos), position=position_dodge(1), show.legend = FALSE) +
  stat_compare_means(
    aes(x=dilution, y=raf, group=assay, label=paste("p =", after_stat(p.format))), 
    data = df_animal, label.y = 0.185, inherit.aes = FALSE, size=6
  ) +
  facet_grid(~sample_type, scales="free_x", space="free_x") +
  scale_y_continuous(expand = expansion(c(0, 0.1))) +
  scale_fill_manual(values=c("darkcyan", "darkorange")) +
  labs(
    y="Rate of Amyloid Formation",
    x="Log Dilution Factors"
  ) +
  main_theme +
  theme(
    legend.position = c(0.5, 0.8),
    legend.direction = "horizontal",
    legend.background = element_rect(fill="white", color="black", linewidth=0.5),
    legend.title = element_blank()
  )
ggsave("mean_raf_col.png", path="figures/necropsy", width=16, height=8)

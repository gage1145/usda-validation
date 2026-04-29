library(dplyr)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(forcats)
library(ggridges)
library(arrow)
library(airtabler)
library(janitor)
library(scales)
source("scripts/airtable_functions.R")



# Load data from Airtable -------------------------------------------------



APP <- "app7KsgYl2jhOnYg7"

# Get the necessary tables
tables <- airtable(APP, c("animals", "samples", "results", "reactions"))

results <- tables$results$select_all(
  filterByFormula = get_formula(
    "sample_type", c("'MNPRO oral swab'", "'NADC oral swab'")
  )
) %>%
  mutate(across(everything(), as.character))

animals <- tables$animals$select_all() %>%
  rename(animal = animal_id) %>%
  mutate(across(everything(), as.character))





# Themes ------------------------------------------------------------------



main_theme <- theme(
  plot.title = element_text(size=24, hjust=0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=12),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)



# Load the data -----------------------------------------------------------



df_ <- results %>%
  left_join(animals, by="animal") %>%
  filter(animal != "NULL") %>%
  clean_names() %>%
  mutate(
    across(c(mpr, ms, ttt, raf, auc, mpi), as.numeric),
    assay = factor(assay, level=c("RT-QuIC", "Nano-QuIC"))
  ) %>%
  mutate_at(
    c("sample_id", "animal", "assay"),
    ~as.factor(as.character(.))
  ) %>%
  mutate_at("mpi", as.integer)

df_sum <- df_ %>%
  group_by(sample_id, animal, mpi, dilution, assay) %>%
  summarize(
    median_raf = median(raf),
    mean_raf = mean(raf),
    mean_mpr = mean(mpr)
  )



# Oral Swab Boxplot -------------------------------------------------------



df_ %>%
  ggplot(aes(animal, raf, fill = assay)) +
  geom_line(aes(y=median_raf, color=assay, group=assay), data=df_sum, linewidth=0.6) +
  geom_boxplot(outliers = FALSE, linewidth=0.25) +
  facet_grid(cols=vars(mpi), space = "free") +
  scale_y_log10() +
  scale_color_manual(values=c("darkslateblue", "darkorange")) +
  scale_fill_manual(values=c("darkslateblue", "darkorange")) +
  coord_flip() +
  labs(
    y="Rate of Amyloid Formation (1/s)"
  ) +
  main_theme +
  theme(
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
    legend.position = "top",
    legend.title = element_blank(),
    legend.background = element_blank()
  )
ggsave("RAFs.png", path="figures/oral-swabs", width=16, height=8)



df_sum %>%
  ggplot(aes(mpi, mean_raf, color=assay)) +
  geom_line(linewidth=1, alpha=0.7) +
  facet_wrap(vars(animal), nrow=6) +
  scale_color_manual(values=c("darkorange", "darkcyan")) +
  labs(
    y="Mean RAF",
    x="Months Post-Inoculation"
  ) +
  main_theme +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )

ggsave("rafs_sample_facet.png", path="figures/oral-swabs", width=8, height=10)



# Mean RAF Area Graph -----------------------------------------------------



df_sum %>%
  group_by(mpi, assay) %>%
  summarize(mean_raf = mean(mean_raf)) %>%
  ggplot(aes(
    mpi, 
    mean_raf, 
    color = assay,
    fill = assay
  )) +
  geom_area(position="dodge", alpha=0.2) +
  scale_x_continuous(breaks=seq(0, 60, 3)) +
  coord_transform(ylim=c(min(df_sum$mean_raf), 0.03), expand=FALSE) +
  labs(
    y="Rate of Amyloid Formation (1/s)",
    title="Mean RAFs of RT-QuIC vs. Nano-QuIC"
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    legend.background = element_blank()
  )
ggsave("lines.png", path="figures/oral-swabs", width=16, height=8)



# Density Plot ------------------------------------------------------------



df_ %>%
  filter(mpr > 5) %>%
  ggplot(aes(mpi, ms)) +
  stat_density_2d(
    geom="raster",
    aes(fill=after_stat(density)),
    contour=FALSE,
    # n=1000,
    show.legend=FALSE
  ) + 
  scale_fill_gradientn(
    colors=c("#101010", "#202854", "#006d91", "#00ba92", "#88ffa5")
  ) +
  facet_grid(vars(assay)) +
  scale_x_continuous(breaks=seq(0, 66, 3)) +
  scale_y_log10(breaks=seq(1, 12, 3), limits=c(0.4, 12)) +
  coord_cartesian(expand=FALSE) +
  main_theme
ggsave("figures/oral-swabs/ridges.png", width=12, height=8)


# Cumulative
df_cum <- df_ %>%
  mutate(auc = rescale(auc, c(0, 1)), .by=assay) %>%
  summarize(across(auc, mean), .by = c(assay, group, mpi)) %>%
  group_by(group, assay) %>%
  arrange(mpi, .by_group = T) %>%
  mutate(across(auc, cumsum)) %>%
  ungroup()

df_cum %>%
  ggplot(aes(mpi, auc, color=group, linetype=assay)) +
  geom_point() +
  geom_line() +
  main_theme

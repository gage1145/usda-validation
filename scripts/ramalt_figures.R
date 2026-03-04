library(dplyr)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(forcats)
library(ggridges)
library(airtabler)
library(janitor)
library(psych)
library(ggbiplot)



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



APP <- "app7KsgYl2jhOnYg7"

# Get the necessary tables
tables <- airtable(APP, c("animals", "results"))

results <- tables$results$select_all(
  filterByFormula = "AND({sample_type} = 'RAMALT', OR({mpi} = '0', NOT({mpi} = BLANK())))"
)

animals <- tables$animals$select_all()
animals <- animals %>%
  rename("animal" = "animal_id")

df_ <- results %>%
  mutate(assay = factor(assay, level=c("RT-QuIC", "Nano-QuIC"))) %>%
  mutate_at(
    c("sample_id", "animal", "assay"),
    ~as.factor(as.character(.))
  ) %>%
  mutate_at("mpi", as.integer) %>%
  left_join(animals, by="animal") %>%
  janitor::clean_names()

df_sum <- df_ %>%
  group_by(sample_id, animal, mpi, dilution, assay, room_number, group, sex, genotype, inoculum, dob, dod_collection_date) %>%
  summarize(
    median_raf = median(raf),
    mean_raf = mean(raf),
    mean_mpr = mean(mpr)
  )



# PCA ---------------------------------------------------------------------


pca_data <- df_ %>%
  mutate_at(c("animal", "assay", "room_number", "group", "sex", "genotype", "inoculum", "mpi"), as.factor) %>%
  # mutate_if(is.character, as.factor) %>%
  # select_if(is.numeric) %>%
  filter(dilution == -3) %>%
  select(-c(ttt, raf, result_id, dilution))

set.seed(111)
ind <- sample(2, nrow(pca_data), replace=T, prob=c(0.8, 0.2))
training <- pca_data[ind==1,]
testing <- pca_data[ind==2,]

# pca <- prcomp(pca_data)
# summary(pca)
pairs.panels(
  select_if(training, is.numeric),
  gap = 0,
  bg = c("red", "yellow", "blue", "orange", "cyan")[training$inoculum],
  pch=21
)

pca <- prcomp(select_if(training, is.numeric),
              center=T,
              scale.=T)
attributes(pca)
print(pca)
palet <- colorRampPalette(c("red", "blue"))(length(levels(training$mpi)))
pairs.panels(
  pca$x,
  gap=0,
  bg = palet[training$mpi],
  pch=21
)

pca %>%
  ggbiplot(obs.scale = 1,
           var.scale = 1,
           groups = training$inoculum,
           ellipse = TRUE,
           # circle = TRUE,
           ellipse.prob = 0.68) +
  scale_color_discrete(name='') +
  theme(legend.direction = 'horizontal',
        legend.position = 'top')



# RAMALT Boxplot -------------------------------------------------------



df_sum %>%
  filter(dilution == -3) %>%
  mutate(dilution = as.factor(dilution)) %>%
  ggplot(aes(mpi, mean_raf, color=assay)) +
  geom_line(linewidth=1) +
  facet_wrap(vars(animal), nrow=3) +
  scale_color_manual(values=c("darkslateblue", "darkorange")) +
  scale_x_continuous(breaks=seq(0, 66, 3)) +
  labs(
    title="Mean RAF per Sample at 10^-3",
    y="Mean RAF"
  ) +
  main_theme +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle=90, vjust=0.5, hjust=1, size=8)
  )

ggsave("rafs_sample_facet.png", path="figures/RAMALT", width=14, height=8)



# Mean RAF Area Graph -----------------------------------------------------



df_sum %>%
  group_by(mpi, assay, dilution) %>%
  summarize(mean_raf = mean(mean_raf)) %>%
  ggplot(aes(
    mpi, 
    mean_raf, 
    color = assay,
    fill = assay
  )) +
  geom_area(position="dodge", alpha=0.2) +
  scale_x_continuous(breaks=seq(0, 60, 3)) +
  coord_transform(ylim=c(min(df_sum$mean_raf), 0.16), expand=FALSE) +
  facet_grid(vars(dilution)) +
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
ggsave("lines.png", path="figures/RAMALT", width=8, height=8)



# Density Plot ------------------------------------------------------------



df_ %>%
  filter(mpr > 5) %>%
  ggplot(aes(mpi, mpr)) +
  stat_density_2d(
    geom="raster",
    aes(fill=after_stat(density)),
    contour=FALSE,
    n=50,
    show.legend=FALSE
  ) +
  scale_fill_gradientn(colors=c("#101010", "yellow")) +
  facet_grid(vars(assay), vars(dilution)) +
  scale_x_continuous(breaks=seq(0, 66, 3)) +
  coord_cartesian(expand=FALSE) +
  main_theme
ggsave("figures/RAMALT/ridges.png", width=12, height=8)

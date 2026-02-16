library(dplyr)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(forcats)
library(ggridges)
library(arrow)



# Themes ------------------------------------------------------------------



dark_bool = F

main_theme <- theme(
  plot.title = element_text(size=24, hjust=0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=12),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)

dark_theme <- theme(
  axis.text.x      = element_text(size=12, angle=45, face="bold",
                                  hjust=1, vjust=1, color="white"),
  plot.title       = element_text(hjust = 0.5, vjust=2, size = 16,
                                  face = "bold", color="white"),
  plot.background  = element_rect(fill="#1f1f1f", color="#1f1f1f"),
  panel.grid       = element_line(color="white"),
  axis.text.y      = element_text(size = 12, color="white"),
  axis.title.y     = element_text(size = 14, color="white"),
  axis.title.x     = element_text(size = 14, color="white"),
  strip.background = element_rect(fill="transparent", color="white"),
  strip.text       = element_text(size=10, face="bold", color="white"),
  legend.text      = element_text(size=10, color="white")
)



# Load the data -----------------------------------------------------------



df_ <- read_parquet("data/RAMALT/calcs.parquet") %>%
  na.omit() %>%
  mutate(
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC")),
    Months = as.numeric(Months)
  )

df_sum <- df_ %>%
  group_by(`Sample IDs`, `Animal IDs`, Months, Dilutions, Assay) %>%
  summarize(
    median_RAF = median(RAF),
    mean_RAF = mean(RAF),
    mean_MPR = mean(MPR)
  )

results <- read_parquet("data/RAMALT/summary.parquet") %>%
  na.omit() %>%
  mutate(
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC"))
  )



# RAMALT Boxplot -------------------------------------------------------



df_sum %>%
  filter(Dilutions == -3) %>%
  mutate(Dilutions = as.factor(Dilutions)) %>%
  ggplot(aes(Months, mean_RAF, color=`Assay`)) +
  geom_line(linewidth=1, alpha=0.7) +
  # geom_smooth() +
  # facet_grid(rows=vars(`Animal IDs`), cols=vars(Dilutions)) +
  facet_wrap(vars(`Animal IDs`), nrow=3) +
  scale_color_manual(values=c("darkslateblue", "darkorange")) +
  scale_x_continuous(breaks=seq(0, 66, 3)) +
  labs(
    title="Mean RAF per Sample at 10^-3",
    y="Mean RAF"
  ) +
  main_theme +
  # guides(color = guide_legend(ncol = 1)) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle=90, vjust=0.5, hjust=1, size=8)
  )

ggsave("rafs_sample_facet.png", path="figures/RAMALT", width=14, height=8)



# Mean RAF Area Graph -----------------------------------------------------



df_sum %>%
  mutate_at("Dilutions", as.factor) %>%
  group_by(Months, Assay, Dilutions) %>%
  summarize(mean_RAF = mean(mean_RAF)) %>%
  ggplot(aes(
    Months, 
    mean_RAF, 
    color = Assay,
    fill = Assay
  )) +
  geom_area(position="dodge", alpha=0.2) +
  scale_x_continuous(breaks=seq(0, 60, 3)) +
  coord_transform(ylim=c(min(df_sum$mean_RAF), 0.16), expand=FALSE) +
  facet_grid(vars(Dilutions)) +
  labs(
    y="Rate of Amyloid Formation (1/s)",
    title="Mean RAFs of RT-QuIC vs. Nano-QuIC"
  ) +
  {if (dark_bool) theme_transparent() + dark_theme else main_theme} +
  theme(
    axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
    panel.grid.minor.x = element_blank(),
    legend.position = "bottom",
    legend.background = element_blank()
  )
ggsave(
  ifelse(dark_bool, "dark_lines.png", "light_lines.png"), 
  path="figures/RAMALT", width=8, height=8
)



# Density Plot ------------------------------------------------------------



df_ %>%
  filter(crossed) %>%
  ggplot(aes(Months, RAF)) +
  stat_density_2d(
    geom="raster",
    aes(fill=after_stat(density)),
    contour=FALSE,
    n=50,
    show.legend=FALSE
  ) +
  scale_fill_gradientn(
    colors=c("#101010", "yellow")
  ) +
  facet_grid(vars(Assay), vars(Dilutions)) +
  scale_x_continuous(breaks=seq(0, 66, 3)) +
  # scale_y_log10(breaks=seq(1, 12, 3), limits=c(0.4, 12)) +
  coord_cartesian(expand=FALSE) +
  dark_theme
ggsave("figures/RAMALT/ridges.png", width=12, height=8)

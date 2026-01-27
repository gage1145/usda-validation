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



# df_ %>%
#   ggplot(aes(`Animal IDs`, MPR, fill = Assay)) +
#   geom_line(aes(y=mean_MPR, color=Assay, group=Assay), data=df_sum, linewidth=0.6) +
#   geom_boxplot(outliers = FALSE, color=ifelse(dark_bool, "darkgrey", "black"), linewidth=0.25) +
#   facet_grid(cols=vars(Months), rows=vars(Dilutions), space = "free") +
#   scale_y_log10() +
#   scale_color_manual(values=c("darkslateblue", "darkorange")) +
#   scale_fill_manual(values=c("darkslateblue", "darkorange")) +
#   coord_flip() +
#   labs(
#     y="Rate of Amyloid Formation (1/s)"
#   ) +
#   {if (dark_bool) theme_transparent() + dark_theme else main_theme} +
#   theme(
#     axis.title.y = element_blank(),
#     axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
#     legend.position = "top",
#     legend.title = element_blank(),
#     legend.background = element_blank()
#   )
# ggsave(
#   ifelse(dark_bool, "dark_RAFs.png", "light_RAFs.png"), 
#   path="figures/RAMALT", width=16, height=8
# )

# df_sum %>%
#   mutate(Dilutions = as.factor(Dilutions)) %>%
#   ggplot(aes(Months, mean_RAF, color=Assay)) +
#   geom_line(linewidth=1, alpha=0.7) +
#   facet_wrap(vars(`Animal IDs`, Dilutions), nrow=3) +
#   scale_color_manual(values=c("darkslateblue", "darkorange")) +
#   scale_x_continuous(breaks=seq(0, 66, 3)) +
#   labs(
#     y="Mean RAF"
#   ) +
#   main_theme +
#   theme(
#     legend.position = "bottom"
#   )
# 
# ggsave("rafs_sample_facet.png", path="figures/RAMALT", width=16, height=8)


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
  coord_transform(ylim=c(min(df_sum$mean_RAF), 0.055), expand=FALSE) +
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
    n=250,
    show.legend=FALSE
  ) + 
  scale_fill_gradientn(
    colors=c("#101010", "#202854", "#006d91", "#00ba92", "#88ffa5", "yellow")
  ) +
  facet_grid(vars(Assay), vars(Dilutions)) +
  scale_x_continuous(breaks=seq(0, 66, 3)) +
  # scale_y_log10(breaks=seq(1, 12, 3), limits=c(0.4, 12)) +
  coord_cartesian(expand=FALSE) +
  dark_theme
ggsave("figures/RAMALT/ridges.png", width=12, height=8)

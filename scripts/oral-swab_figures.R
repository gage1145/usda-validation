library(dplyr)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(forcats)
library(ggridges)



# Themes ------------------------------------------------------------------



dark_bool = F

main_theme <- theme(
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



df_ <- read.csv("data/oral-swabs/calcs.csv", check.names = FALSE) %>%
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

results <- read.csv("data/oral-swabs/summary.csv", check.names = FALSE) %>%
  na.omit() %>%
  mutate(
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC"))
  )



# Oral Swab Boxplot -------------------------------------------------------



df_sum %>%
  ggplot(aes(Months, mean_RAF, color = `Animal IDs`, fill = `Animal IDs`)) +
  # geom_boxplot(outliers = FALSE, color=ifelse(dark_bool, "darkgrey", "black"), linewidth=0.25) +
  # geom_col(position="identity") +
  geom_area(position="dodge", alpha=0.2) +
  facet_grid(cols=vars(Assay)) +
  scale_x_continuous(breaks=seq(0, 60, 3)) +
  coord_transform(
    y="log10",
    ylim=c(min(df_sum$mean_RAF), 1.1 * max(df_sum$mean_RAF)), expand=FALSE) +
  labs(
    y="Rate of Amyloid Formation (1/s)"
  ) +
  {if (dark_bool) theme_transparent() + dark_theme else main_theme} +
  theme(
    # panel.grid.major.y = element_line(color="black", linetype="dashed", linewidth = 0.2),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
    # legend.position = c(0.85, 0.85),
    legend.position = "none",
    legend.title = element_blank(),
    legend.background = element_blank()
  )
ggsave(
  ifelse(dark_bool, "dark_RAFs.png", "light_RAFs.png"), 
  path="figures/oral-swabs", width=16, height=8
)


df_ %>%
  arrange(desc(Months)) %>%
  ggplot(aes(MPR, fct_inorder(Months), fill=stat(x))) +
  ggridges::geom_density_ridges_gradient() +
  scale_fill_gradient(low="darkslateblue", high="darkorange") +
  scale_x_log10() +
  facet_grid(rows=vars(Assay))
ggsave("figures/oral-swabs/ridges.png", width=12, height=8)

library(dplyr)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(forcats)



# Themes ------------------------------------------------------------------



dark_bool = TRUE

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



df_ <- read.csv("data/necropsy/calcs.csv", check.names = FALSE) %>%
  na.omit() %>%
  mutate(
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC")),
  ) %>%
  mutate_at("Dilutions", as.factor)

df_sum <- df_ %>%
  group_by(`Sample IDs`, Dilutions, Assay, Tissue) %>%
  summarize(
    median_RAF = median(RAF)
  ) %>%
  mutate(
    Dilutions = factor(
      Dilutions, 
      levels=c(-2, -3, -4), 
      labels=c("10^{-2}", "10^{-3}", "10^{-4}"))
  )

results <- read.csv("data/necropsy/summary.csv", check.names = FALSE) %>%
  na.omit() %>%
  mutate_at("Dilutions", as.factor) %>%
  mutate(
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC"))
  )



# Tissue Boxplot ----------------------------------------------------------



df_ %>%
  mutate(
    Dilutions = factor(
      Dilutions, 
      levels=c(-2, -3, -4), 
      labels=c("10^{-2}", "10^{-3}", "10^{-4}"))
  ) %>%
  ggplot(aes(fct_inorder(`Sample IDs`), RAF, fill = Assay)) +
  geom_line(aes(y=median_RAF, color=Assay, group=Assay), data=df_sum, linewidth=0.6) +
  geom_boxplot(outliers = FALSE, color=ifelse(dark_bool, "darkgrey", "black"), linewidth=0.25) +
  facet_grid(vars(fct_rev(Dilutions)), vars(Tissue), space = "free", labeller=label_parsed) +
  scale_color_manual(values=c("darkslateblue", "darkorange")) +
  scale_fill_manual(values=c("darkslateblue", "darkorange")) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Dilution Factor", breaks = NULL)) +
  labs(
    y="Rate of Amyloid Formation (1/s)"
  ) +
  {if (dark_bool) theme_transparent() + dark_theme else main_theme} +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
    legend.position = c(0.85, 0.85),
    legend.title = element_blank(),
    legend.background = element_blank()
  )
ggsave(
  ifelse(dark_bool, "dark_RAFs.png", "light_RAFs.png"), 
  path="figures/necropsy", width=12, height=6
)



# Tissue Histograms -------------------------------------------------------



df_ %>%
  filter(TtT != 72) %>%
  ggplot(aes(RAF, fct_rev(Dilutions), fill = Assay)) +
  geom_density_ridges(
    scale=4, 
    rel_min_height=0.001, 
    panel_scaling=FALSE, 
    alpha=0.6, 
    color=ifelse(dark_bool, "darkgrey", "black")
  ) +
  facet_grid(vars(Tissue), scale = "free") +
  scale_color_manual(values=c("darkslateblue", "darkorange")) +
  scale_fill_manual(values=c("darkslateblue", "darkorange")) +
  scale_y_discrete(expand=c(0.2, 0)) +
  labs(
    x="Rate of Amyloid Formation (1/h)",
    y="Log Dilution Factors"
  ) +
  {if (dark_bool) dark_theme else main_theme} +
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
  ifelse(dark_bool, "dark_histograms.png", "light_histograms.png"), 
  path="figures/necropsy", width=16, height=8
)



# Logistic Curve ----------------------------------------------------------



# results %>%
#   mutate_at("thres_pos", as.integer) %>%
#   ggplot(aes(mean_MPR, thres_pos, color=Dilutions, linetype = Assay)) +
#   geom_smooth(
#     method="glm",
#     method.args = list(family = "binomial"), 
#     se = FALSE
#   )

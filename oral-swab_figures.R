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
    Months = as.factor(Months)
  )

df_sum <- df_ %>%
  group_by(`Sample IDs`, `Animal IDs`, Months, Dilutions, Assay) %>%
  summarize(
    median_RAF = median(RAF)
  )

results <- read.csv("data/oral-swabs/summary.csv", check.names = FALSE) %>%
  na.omit() %>%
  # mutate_at("Dilutions", as.factor) %>%
  mutate(
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC"))
  )



# Oral Swab Boxplot -------------------------------------------------------



df_ %>%
  ggplot(aes(`Animal IDs`, RAF, fill = Assay)) +
  geom_line(aes(y=median_RAF, color=Assay, group=Assay), data=df_sum, linewidth=0.6) +
  geom_boxplot(outliers = FALSE, color=ifelse(dark_bool, "darkgrey", "black"), linewidth=0.25) +
  facet_grid(rows=vars(Months), space = "free") +
  scale_color_manual(values=c("darkslateblue", "darkorange")) +
  scale_fill_manual(values=c("darkslateblue", "darkorange")) +
  # scale_y_continuous(sec.axis = sec_axis(~ ., name = "Dilution Factor", breaks = NULL)) +
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
  path="figures/oral-swabs", width=8, height=12
)


df_ %>%
  ggplot(aes(MPR, Months, fill=stat(x))) +
  ggridges::geom_density_ridges_gradient() +
  scale_fill_gradient(low="skyblue", high="darkred") +
  facet_grid(rows=vars(Assay))
ggsave("figures/oral-swabs/ridges.png", width=12, height=8)


df_ %>%
  ggplot(aes(Months, MPR)) +
  geom_boxplot() +
  facet_grid(rows=vars(Assay)) 











library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(forcats)



# Themes ------------------------------------------------------------------



dark_bool = T

main_theme <- theme(
  axis.title       = element_text(size=16),
  axis.text        = element_text(size=12, color="black"),
  strip.text       = element_text(size=16, face="bold"),
  legend.title     = element_text(size=12, color="black"),
  legend.text      = element_text(size=12),
  plot.title       = element_text(hjust=0.5, vjust=2, size=20, face="bold"),
)

dark_theme <- theme(
  axis.text.x      = element_text(size=12, face="bold", color="white"),
  plot.title       = element_text(hjust = 0.5, vjust=2, size = 20,
                                  face = "bold", color="white"),
  plot.background  = element_rect(fill="#1f1f1f", color="#1f1f1f"),
  panel.grid       = element_line(color="white"),
  axis.text.y      = element_text(size=12, color="white"),
  axis.title.y     = element_text(size=16, color="white"),
  axis.title.x     = element_text(size=14, color="white"),
  strip.background = element_rect(fill="transparent", color="white"),
  strip.text       = element_text(size=10, face="bold", color="white"),
  legend.text      = element_text(size=10, color="white")
)



# Load the data -----------------------------------------------------------



df_ <- read.csv("data/serum-plasma/calcs.csv", check.names = FALSE) %>%
  filter(!(Sample %in% c("P", "N"))) %>%
  na.omit() %>%
  mutate(
    Status = factor(Status, levels=c("P", "N")),
    Treatment = factor(Treatment, levels=c("sup", "pellet")),
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC")),
    Dilutions = factor(Dilutions, level=c(0, -1, -2, -3, -4))
  )

df_sum <- df_ %>%
  group_by(Sample, Status, Treatment, Dilutions, Assay) %>%
  summarize(
    median_RAF = median(RAF)
  )

results <- read.csv("data/serum-plasma/summary.csv", check.names = FALSE) %>%
  filter(!(Sample %in% c("P", "N"))) %>%
  na.omit() %>%
  mutate(
    Status = factor(Status, levels=c("P", "N")),
    Treatment = factor(Treatment, levels=c("sup", "pellet")),
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC")),
    Dilutions = factor(Dilutions, level=c(0, -1, -2, -3, -4))
  )

df_raw <- read.csv("data/serum-plasma/raw.csv", check.names = FALSE) %>%
  mutate_at(c("Sample", "Treatment", "Dilutions", "Assay"), as.factor)



# Blood Boxplot ----------------------------------------------------------



df_ %>%
  ggplot(aes(Sample, RAF, fill=Dilutions)) +
  geom_boxplot() +
  facet_grid(Treatment ~ Assay) +
  scale_y_log10() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text = element_text(size=12),
    strip.text = element_text(size=16, face="bold"),
    legend.title = element_text(size=12, face="bold", hjust=0.5),
    legend.text = element_text(size=12, hjust=0.5)
  )
ggsave("boxplot.png", path="figures/serum-plasma", width=12, height=6)



# Real-time graphs --------------------------------------------------------



df_raw %>%
  filter(Sample == "plasma", Treatment == "pellet", Dilutions == 0, Assay == "Nano-QuIC") %>%
  ggplot(aes(Time, Norm, group=Wells)) +
  geom_line() +
  scale_x_continuous(breaks=seq(0,96,12), expand=expansion()) +
  ggtitle("Plasma, Pellet, 10-1, Nano-QuIC")

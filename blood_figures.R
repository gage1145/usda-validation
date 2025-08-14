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



df_ <- read.csv("data/blood/calcs.csv", check.names = FALSE) %>%
  filter(!(`Sample IDs` %in% c("P", "N"))) %>%
  na.omit() %>%
  mutate_at("Dilutions", as.factor) %>%
  mutate(
    Assay = ifelse(Assay == "RT", "RT-QuIC", "Nano-QuIC"),
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC")),
    Dilutions = factor(Dilutions, level=c(0, -1, -2, -3, -4))
  )

df_sum <- df_ %>%
  group_by(`Sample IDs`, Dilutions, Assay) %>%
  summarize(
    median_RAF = median(RAF)
  )

results <- read.csv("data/blood/summary.csv", check.names = FALSE) %>%
  filter(!(`Sample IDs` %in% c("P", "N"))) %>%
  na.omit() %>%
  mutate_at("Dilutions", as.factor) %>%
  mutate(
    Assay = ifelse(Assay == "RT", "RT-QuIC", "Nano-QuIC"),
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC")),
    Dilutions = factor(Dilutions, level=c(0, -1, -2, -3, -4))
  )



# Tissue Boxplot ----------------------------------------------------------



df_ %>%
  rename("Preparation" = "Sample IDs") %>%
  ggplot(aes(Preparation, RAF, fill = Dilutions)) +
  geom_boxplot(outliers = FALSE, color="white", linewidth=0.25) +
  facet_grid(cols=vars(Assay), space = "free", scales="free") +
  scale_color_manual(
    values=c("cyan3", "darkcyan", "darkslateblue", "darkorange", "darkorange4")
  ) +
  scale_fill_manual(
    values=c("cyan3", "darkcyan", "darkslateblue", "darkorange", "darkorange4")
  ) +
  labs(
    x="Preparation",
    y="Rate of Amyloid Formation (1/s)"
  ) +
  {if (dark_bool) theme_transparent()} +
  {if (dark_bool) dark_theme else main_theme} +
  theme(
    axis.text.x = element_text(color="white"),
    axis.text.y = element_text(color="white"),
    legend.position = "right",
    legend.title = element_text(hjust=0.5, face="bold", color="white")
  )
ggsave("RAFs.png", path="figures/blood", width=16, height=8)

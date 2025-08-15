library(dplyr)
library(tidyr)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(forcats)



# Themes ------------------------------------------------------------------



dark_bool = F

main_theme <- theme(
  axis.title = element_text(size=20),
  axis.text = element_text(size=12, color="black"),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12, color="black"),
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

wells <- read.csv("data/blood/locs.csv", check.names = FALSE)

df_raw <- read.csv("data/blood/raw.csv", check.names = FALSE) %>%
  mutate(wells = wells$V1) %>%
  relocate(wells, .after="Assay") %>%
  pivot_longer(5:ncol(.), names_to = "time", values_to = "rfu") %>%
  mutate_at(c("time", "rfu"), as.numeric) %>%
  mutate_at(c("Sample IDs", "Dilutions", "Assay"), as.factor)



# Blood Boxplot ----------------------------------------------------------



highlights <- data.frame(
  xmin = c(1.95, 1.95, 0.95),
  xmax = c(2.25, 2.25, 1.25),
  ymin = c(0.0115, 0.013, 0.016),
  ymax = c(0.014, 0.022, 0.0315),
  Assay = factor(c("RT-QuIC", "Nano-QuIC", "Nano-QuIC"), levels=c("RT-QuIC", "Nano-QuIC"))
)

boxes <- df_ %>%
  rename("Preparation" = "Sample IDs") %>%
  ggplot(aes(Preparation, RAF, fill = Dilutions)) +
  geom_boxplot(outliers = FALSE, color=ifelse(dark_bool, "white", "black"), linewidth=0.25) +
  geom_rect(
    aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
    highlights,
    fill="transparent",
    color="red",
    linewidth=1,
    inherit.aes = FALSE
  ) +
  facet_grid(cols=vars(Assay), space = "free", scales="free") +
  scale_color_manual(
    values=c("cyan3", "darkcyan", "darkslateblue", "darkorange", "darkorange4")
  ) +
  scale_fill_manual(
    values=c("cyan3", "darkcyan", "darkslateblue", "darkorange", "darkorange4")
  ) +
  labs(
    x="Preparation",
    y="Rate of Amyloid Formation (1/s)",
    fill="Log Dilution"
  ) +
  {if (dark_bool) theme_transparent()} +
  {if (dark_bool) dark_theme else main_theme} +
  theme(
    legend.position = "right",
    legend.title = element_text(color=ifelse(dark_bool, "white", "black"), hjust=0.5, face="bold"),
  )
ggsave(ifelse(dark_bool, "dark_RAFs.png", "light_RAFs.png"), boxes, path="figures/blood", width=16, height=8)



# Real-time graphs --------------------------------------------------------



text_labs <- data.frame(
  Assay = c("Nano-QuIC", "Nano-QuIC", "RT-QuIC"),
  Dilutions = as.factor(c(-3, -2, -2)),
  Preparations = c("A", "B", "B"),
  color = c("darkorange", "darkslateblue", "darkslateblue"),
  x = c(12, 12, 12),
  y = c(6000, 5000, 6000)
)
rt <- df_raw %>%
  mutate(
    Assay = ifelse(Assay == "RT", "RT-QuIC", "Nano-QuIC"),
    assay_f = factor(Assay, levels=c("RT-QuIC", "Nano-QuIC"))
  ) %>%
  filter(
    (`Sample IDs` == "B" & Assay == "RT-QuIC" & Dilutions == -2) |
      (`Sample IDs` == "A" & Assay == "Nano-QuIC" & Dilutions == -3) |
      (`Sample IDs` == "B" & Assay == "Nano-QuIC" & Dilutions == -2) &
      !(`Sample IDs` %in% c("N", "P"))
  ) %>% 
  rename(Preparation = `Sample IDs`) %>%
  ggplot(aes(time, rfu, color=Preparation, group=wells)) +
  geom_line(linewidth=1) +
  # geom_text(
  #   aes(x=x, y=y, label=Dilutions, color=Dilutions),
  #   text_labs,
  #   inherit.aes = F
  # ) +
  facet_grid(cols=vars(assay_f)) +
  scale_color_manual(values=c("darkorange", "darkslateblue")) +
  # scale_color_manual(values=c("cyan3", "darkcyan", "darkslateblue", "darkorange")) +
  scale_x_continuous(limits=c(0,96), breaks=seq(12,96,12), expand=expansion()) +
  labs(
    y="RFU",
    x="Time (h)",
    color="Preparation"
  ) +
  {if (dark_bool) theme_transparent()} +
  {if (dark_bool) dark_theme else main_theme} +
  theme(
    legend.title = element_text(color=ifelse(dark_bool, "white", "black"), hjust=0.5, face="bold"),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1)
  )
ggsave(ifelse(dark_bool, "dark_rt.png", "light_rt.png"), rt, path="figures/blood", width=16, height=8)



# Combined ----------------------------------------------------------------



ggarrange(boxes, rt, ncol=1, labels=c("A", "B"), align="v", font.label = list(size=24))
ggsave("combined.png", path="figures/blood", width=16, height=10)

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



df_ <- read_parquet("data/blood/calcs.parquet") %>%
  filter(!(`Sample IDs` %in% c("P", "N"))) %>%
  na.omit() %>%
  mutate(
    Treatment = factor(Treatment, levels=c("A", "B", "C", "D", "E")),
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC")),
    Dilutions = factor(Dilutions, level=c(0, -1, -2, -3, -4))
  )

df_sum <- df_ %>%
  group_by(`Sample IDs`, Treatment, Dilutions, Assay) %>%
  summarize(
    median_RAF = median(RAF)
  )

results <- read_parquet("data/blood/summary.parquet") %>%
  filter(!(`Sample IDs` %in% c("P", "N"))) %>%
  na.omit() %>%
  mutate(
    Treatment = factor(Treatment, levels=c("A", "B", "C", "D", "E")),
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC")),
    Dilutions = factor(Dilutions, level=c(0, -1, -2, -3, -4))
  )

df_raw <- read_parquet("data/blood/raw.parquet") %>%
  mutate_at(c("Sample IDs", "Treatment", "Dilutions", "Assay"), as.factor)



# Blood Boxplot ----------------------------------------------------------



box_theme <- function(p) {
  p %>%
    filter(Dilutions %in% c(-2, -3)) %>%
    mutate(Treatment = paste("Treatment", Treatment)) %>%
    ggplot(aes(Dilutions, RAF, fill = Sample)) +
    geom_boxplot(
      outliers=FALSE, 
      color=ifelse(dark_bool, "white", "black"), 
      linewidth=0.25
    ) +
    facet_grid(
      rows=vars(Assay), 
      space = "free", 
      scales="fixed"
    ) +
    scale_fill_manual(values=c("darkslateblue", "red")) +
    scale_y_continuous(limits=c(min(p$RAF), 0.034)) +
    labs(
      x="Log Dilution Factor",
      y="Rate of Amyloid Formation (1/s)"
    )
}

boxes_A <- df_ %>%
  rename("Sample" = "Sample IDs") %>%
  filter(
    Treatment == "A",
    Dilutions == -3
  ) %>%
  box_theme() +
  ggtitle("Treatment A") +
  {if (dark_bool) theme_transparent()} +
  {if (dark_bool) dark_theme else main_theme} +
  theme(
    legend.position = "right",
    legend.title = element_text(
      color=ifelse(dark_bool, "white", "black"), hjust=0.5, face="bold"
    ),
    axis.title.x=element_blank(),
  )

boxes_B <- df_ %>%
  rename("Sample" = "Sample IDs") %>%
  filter(
    Treatment == "B",
    Dilutions == -2
  ) %>%
  box_theme() +
  ggtitle("Treatment B") +
  {if (dark_bool) theme_transparent()} +
  {if (dark_bool) dark_theme else main_theme} +
  theme(
    legend.position = "right",
    legend.title = element_text(
      color=ifelse(dark_bool, "white", "black"), hjust=0.5, face="bold"
    ),
  )

boxes_combined <- ggarrange(
  boxes_A, boxes_B,
  ncol=1,
  align="v",
  legend="none"
)
boxes_combined



# Real-time graphs --------------------------------------------------------



rt_theme <- function(p) {
  p %>%
    mutate(
      Assay = factor(Assay, levels=c("RT-QuIC", "Nano-QuIC")),
      group = paste(Wells, Reaction, sep="_")
    ) %>%
    ggplot(aes(Time, RFU, color=`Sample IDs`, group=group)) +
    geom_line(linewidth=1) +
    facet_grid(rows=vars(Assay)) +
    scale_color_manual(values=c("darkslateblue", "red")) +
    scale_x_continuous(limits=c(0,96), breaks=seq(0,96,12), expand=expansion()) +
    scale_y_continuous(limits=c(0,15000)) +
    labs(
      y="RFU",
      x="Time (h)",
      color="Sample"
    )
}

rt_A <- df_raw %>%
  filter(
    Treatment == "A",
    Dilutions == -3
  ) %>% 
  rt_theme() +
  ggtitle("Treatment A") +
  {if (dark_bool) theme_transparent()} +
  {if (dark_bool) dark_theme else main_theme} +
  theme(
    legend.position = "none",
    legend.title = element_text(
      color=ifelse(dark_bool, "white", "black"), hjust=0.5, face="bold"
    ),
    axis.title.x=element_blank(),
  )

rt_B <- df_raw %>%
  filter(
    Treatment == "B",
    Dilutions == -2
  ) %>% 
  rt_theme() +
  ggtitle("Treatment B") +
  {if (dark_bool) theme_transparent()} +
  {if (dark_bool) dark_theme else main_theme} +
  theme(
    legend.position = "none",
    legend.title = element_text(
      color=ifelse(dark_bool, "white", "black"), hjust=0.5, face="bold"
    ),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1)
  )

rt_combined <- ggarrange(
  rt_A, rt_B,
  ncol=1, 
  align="v",
  common.legend = TRUE,
  legend="right"
)



# Combined ----------------------------------------------------------------



ggarrange(
  boxes_combined, rt_combined,
  widths=c(1,2.5),
  ncol=2,
  align="v",
  common.legend = TRUE
) +
  theme(
    plot.background = element_rect(fill=ifelse(dark_bool, "#1f1f1f", "white"))
  )
ggsave(
  ifelse(dark_bool, "combined_dark.png", "combined_light.png"), 
  path="figures/blood", 
  width=16, 
  height=10
)

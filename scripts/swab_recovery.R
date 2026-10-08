library(tidyverse)
library(quicR)
library(modelr)
library(ggpubr)
library(zoo)
library(ggrepel)


main_theme <- theme(
  plot.title = element_text(size=24, hjust=0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=12),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)


files <- list.files("raw/swab-recovery", full.names = TRUE)

df_raw <- map_dfr(files, ~ as.data.frame(get_quic(.x))) %>%
  as.data.frame() %>%
  filter(sample != "N") %>%
  separate(sample, into = c("sample", "bio_rep"), sep = "_", fill="right") %>%
  mutate(dilution = -log10(as.numeric(dilution)))

df_ <- calculate_metrics(df_raw, threshold = 2)

df_standard <- df_ %>%
  filter(sample == "141234")

mod <- lm(raf ~ dilution, data = df_standard)
m <- coef(mod)[2]
b <- coef(mod)[1]

df_test <- df_ %>%
  filter(sample != "141234") %>%
  add_predictions(mod) 

df_test_sum <- df_test %>%
  summarize(
    across(c(raf, mpr, ms, auc, pred), median),
    .by = c(sample, dilution)
  ) %>%
  mutate(
    expected_dil = (raf - b) / m,
    recovery = raf / pred,
    perc_recovery = signif(recovery * 100, 2),
    recovery_label = paste0(sample, ": ", perc_recovery, "% Recovery"),
    x = -7,
    y = c(0.16, 0.15, 0.14)
  ) 


# Standard Curve ---------------------------------------------------------


df_standard %>%
  ggplot(aes(dilution, raf)) +
  stat_smooth(method = "lm", fullrange=TRUE, se=T, linetype = "dashed") +
  geom_point() +
  geom_segment(
    aes(y=pred, yend = raf, x=dilution, color=sample), data = df_test_sum, linewidth=1,
    show.legend = FALSE
  ) +
  geom_segment(
    aes(yend=raf, xend=expected_dil, x=dilution, color=sample), data = df_test_sum, linewidth=1, 
    arrow = arrow(length = unit(0.5, "cm"), type="closed"), 
    show.legend = FALSE
  ) +
  geom_point(aes(label = sample, color = sample), data = df_test_sum, size = 6) +
  geom_label(aes(x=x, y=y, label = recovery_label, color=sample), data = df_test_sum, hjust=0, show.legend=F, size=6) +
  scale_x_continuous(breaks=seq(-7, 0, 1)) +
  labs(
    x = "-log10(Dilution)",
    y = "Rate of Amyloid Formation (1/h)",
    title = "swab Recovery"
  ) +
  main_theme +
  theme(
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    legend.position = "none",
    legend.position.inside = c(0.1, 0.8),
    legend.background = element_blank()
  )

ggsave("figures/swab_recovery/swab_recovery.png", width=12, height=8)
  

# Real-time curves -------------------------------------------------------


df_raw %>%
  rename(swab = sample, replicate = bio_rep,) %>%
  filter(!is.na(replicate), time <= 24) %>%
  mutate(norm = rollmean(norm, 10, na.pad=T), .by = c(well)) %>%
  na.omit() %>%
  summarize(
    norm = median(norm),
    .by = c(time, swab, dilution, replicate)
  ) %>%
  ggplot(aes(time, norm, color = swab, fill = swab, linetype = replicate)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 72, 4), expand = expansion()) +
  labs(
    x = "time (h)",
    y = "normalized Fluorescence",
    title = "Real-time swab Recovery Curves"
  ) +
  main_theme +
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.position = "inside",
    legend.position.inside = c(0.1, 0.8),
    legend.background = element_blank()
  )

ggsave("figures/swab_recovery/real_time.png", width=12, height=8)

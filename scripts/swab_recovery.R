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

df_raw <- map_dfr(files, get_quic) %>%
  filter(`Sample IDs` != "N") %>%
  separate(`Sample IDs`, into = c("Sample IDs", "bio_rep"), sep = "_", fill="right") %>%
  mutate(Dilutions = -log10(as.numeric(Dilutions)))

df_ <- calculate_metrics(df_raw, threshold = 2)

df_standard <- df_ %>%
  filter(`Sample IDs` == "141234")

# mod <- lm(Dilutions ~ MPR + MS + AUC +RAF, data = df_standard)
mod <- lm(RAF ~ Dilutions, data = df_standard)
m <- coef(mod)[2]
b <- coef(mod)[1]
# modelr::rsquare(mod, df_standard)

df_test <- df_ %>%
  filter(`Sample IDs` != "141234") %>%
  add_predictions(mod) 

df_test_sum <- df_test %>%
  summarize(
    across(c(RAF, MPR, MS, AUC, pred), median),
    .by = c(`Sample IDs`, Dilutions)
  ) %>%
  mutate(
    expected_dil = (RAF - b) / m,
    recovery = RAF / pred,
    perc_recovery = signif(recovery * 100, 2),
    recovery_label = paste0(`Sample IDs`, ": ", perc_recovery, "% Recovery"),
    x = -7,
    y = c(0.16, 0.15, 0.14)
  ) 


# Standard Curve ---------------------------------------------------------


df_standard %>%
  ggplot(aes(Dilutions, RAF)) +
  stat_smooth(method = "lm", fullrange=TRUE, se=T, linetype = "dashed") +
  geom_point() +
  geom_segment(
    aes(y=pred, yend = RAF, x=Dilutions, color=`Sample IDs`), data = df_test_sum, linewidth=1,
    show.legend = FALSE
  ) +
  geom_segment(
    aes(yend=RAF, xend=expected_dil, x=Dilutions, color=`Sample IDs`), data = df_test_sum, linewidth=1, 
    arrow = arrow(length = unit(0.5, "cm"), type="closed"), 
    show.legend = FALSE
  ) +
  geom_point(aes(label = `Sample IDs`, color = `Sample IDs`), data = df_test_sum, size = 6) +
  geom_label(aes(x=x, y=y, label = recovery_label, color=`Sample IDs`), data = df_test_sum, hjust=0, show.legend=F, size=6) +
  scale_x_continuous(breaks=seq(-7, 0, 1)) +
  labs(
    x = "-log10(Dilution)",
    y = "Rate of Amyloid Formation (1/h)",
    title = "Swab Recovery"
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
  rename(
    Swab = `Sample IDs`,
    Replicate = bio_rep,
  ) %>%
  filter(
    !is.na(Replicate),
    Time <= 24
  ) %>%
  mutate(Norm = rollmean(Norm, 10, na.pad=T), 
    .by = c(Wells)) %>%
  na.omit() %>%
  summarize(
    Norm = median(Norm),
    .by = c(Time, Swab, Dilutions, Replicate)
  ) %>%
  ggplot(aes(Time, Norm, color = Swab, fill = Swab, linetype = Replicate)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 72, 4), expand = expansion()) +
  labs(
    x = "Time (h)",
    y = "Normalized Fluorescence",
    title = "Real-time Swab Recovery Curves"
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

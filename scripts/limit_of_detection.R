# This paper is very useful: https://pmc.ncbi.nlm.nih.gov/articles/PMC2556583/

library(tidyverse)
library(quicR)
library(lubridate)
library(arrow)
library(ggrepel)
library(scales)


main_theme <- theme(
  plot.title = element_text(size=24, hjust=0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=12),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)

threshold <- 5
norm_point <- 3
lod_cutoff <- 0.95
files <- list.files("raw/limit-of-detection", ".xlsx", full.names = TRUE, recursive = TRUE)
groups <- c("Sample IDs", "Well", "Dilutions", "Reaction", "Assay", "date", "reader", "tech")
lower_groups <- c("dilution", "group", "sample_type", "assay", "mpr", "auc", "ms")
metrics <- c("mpr", "auc", "ms")

extract_file_meta <- function(x, pattern, n) {
  str_split_i(x, pattern, n) %>%
    str_remove("\\.[[:alpha:]]+$")
}

norm_eq <- function(x, mu, sigma) {
  term_1 <- 1 / (sigma * sqrt(2 * pi))
  term_2 <- exp(-((x - mu) ^ 2 / (2 * sigma ^ 2)))
  term_1 * term_2
}

# One-sided Mahalanobis distance from the negative control distribution.
# Deviations below the negative mean are set to 0 so that only
# higher-than-negative values count toward the distance.
maha_dist <- function(x, mu, sigma) {
  x <- sweep(as.matrix(x), 2, mu)
  x <- pmax(x, 0)
  sqrt(mahalanobis(x, center = rep(0, length(mu)), cov = sigma))
}

get_raw <- function(file, np, w, zero) {
  rxn    <- extract_file_meta(file, "/", 3)
  date   <- extract_file_meta(rxn, "_", 1)
  date   <- parse_date_time(date, "%Y%m%d")
  reader <- extract_file_meta(rxn, "_", 2)
  tech   <- extract_file_meta(rxn, "_", 3)
  assay  <- extract_file_meta(rxn, "_", 5)

  file %>%
    get_quic(norm_point = np, window_size = w, zero = zero) %>%
    mutate(
      `Sample IDs` = str_remove(`Sample IDs`, "-P"),
      Dilutions = -log10(as.numeric(Dilutions)),
      Assay = assay,
      Reaction = rxn,
      date = date,
      reader = reader,
      tech = tech
    ) %>%
    suppressMessages() %>%
    suppressWarnings()
}

df_neg_wide <- read_parquet("data/data_dump.parquet") %>%
  # mutate(across(c(mpr, auc, ms), log)) %>%
  select(all_of(lower_groups)) %>%
  filter(group == "Negative Control" & sample_type == "PLN" & assay == "RT-QuIC")

# Negative control center and covariance for the combined metric
mu_neg <- colMeans(df_neg_wide[metrics])
sigma_neg <- cov(df_neg_wide[metrics])

df_neg <- df_neg_wide %>%
  mutate(combined = maha_dist(pick(all_of(metrics)), mu_neg, sigma_neg)) %>%
  pivot_longer(c(all_of(metrics), combined), names_to = "metric", values_to = "value")

df_neg_sum <- df_neg %>%
  summarize(
    total = n(),
    min = min(value),
    max = max(value),
    mean = mean(value),
    sd = sd(value),
    # The combined distance is not normal, so its LoB is the empirical 95th percentile
    lob = if (cur_group()$metric == "combined") {
      quantile(value, 0.95, names = FALSE)
    } else {
      mean(value) + 1.645 * sd(value)
    },
    distro = list(tibble(
      x = seq(0, max, length.out = 10000),
      y = norm_eq(x, mean, sd),
      cum_y = cumsum(y),
      alpha = x >= lob
    )), 
    .by = c(metric, group, dilution)
  )

df_raw <- map_dfr(files, get_raw, np = norm_point, w = 3, zero = F)

df_cal <- calculate_metrics(df_raw, groups, threshold = threshold)  %>%
  filter(`Sample IDs` == "141234") %>%
  rename_with(tolower) %>%
  rename(dilution = dilutions) %>%
  mutate(
    # across(c(mpr, auc, ms), log),
    group = "Positive Control"
  ) %>%
  select(all_of(lower_groups[-which(lower_groups == "sample_type")])) %>%
  mutate(combined = maha_dist(pick(all_of(metrics)), mu_neg, sigma_neg)) %>%
  pivot_longer(c(all_of(metrics), combined), names_to = "metric", values_to = "value") %>%
  full_join(select(df_neg_sum, metric, lob), by = "metric") 

df_lod <- df_cal %>%
  filter(dilution >= -9) %>%
  summarize(
    total = n(),
    min = min(value),
    max = max(value),
    mean = mean(value),
    sd = sd(value),
    n = sum(value > lob),
    perc = n / total,
    .by = c(dilution, metric, group, lob)
  ) %>%
  mutate(
    # Area of the fitted positive Gaussian above the negative control LoB
    p_detect = pnorm(lob, mean, sd, lower.tail = FALSE),
    detected = p_detect >= lod_cutoff
  ) %>%
  mutate(
    min = min(min),
    max = max(max),
    .by = c(metric)
  ) %>%
  mutate(
    distro = list(tibble(
      x = seq(0, max, length.out = 10000),
      y = norm_eq(x, mean, sd),
      cum_y = cumsum(y),
      alpha = x >= lob
    )),
    max_p = sapply(distro, function(x) max(x$y)),
    .by = c(dilution, metric, group, lob)
  ) %>%
  full_join(df_neg_sum)

# LoD: most dilute level where every more concentrated level also passes
df_lod_cut <- df_lod %>%
  filter(group == "Positive Control") %>%
  arrange(metric, desc(dilution)) %>%
  filter(cumall(detected), .by = metric) %>%
  slice_min(dilution, n = 1, by = metric) %>%
  select(metric, lod = dilution, p_detect, perc, max_p)

df_lod %>%
  filter(group == "Positive Control") %>%
  pivot_longer(c(perc, p_detect), names_to = "source", values_to = "rate") %>%
  ggplot(aes(dilution, rate, color = metric, linetype = source)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = lod_cutoff, linetype = "dashed") +
  scale_x_continuous(n.breaks = length(unique(df_lod$dilution))) +
  theme_bw() +
  theme(legend.position = "bottom")

color_palette <- c("-3" = "#000000", "-6" = "#005c3d", "-7" = "#118f5a", "-8" = "#38b48f", "-9" = "#5adfb3")

df_lod %>%
  unnest(distro) %>%
  mutate(
    dilution = as.factor(dilution)
  ) %>%
  filter(metric == "combined") %>%
  ggplot(aes(x, y, color = dilution, linetype = group)) +
  geom_ribbon(aes(ymin = 0, ymax = y, fill = dilution, alpha = alpha), show.legend = FALSE) +
  geom_vline(aes(xintercept = lob), data = df_neg_sum %>%filter(metric == "combined"), inherit.aes = FALSE) +
  geom_label_repel(
    aes(
      x = mean, y = max_p, fill = as.factor(dilution),
      label = sprintf("Dilution: %s\nOverlap: %s", dilution, signif(1 - p_detect, 3))
    ), 
    data = df_lod %>% filter(metric == "combined"), inherit.aes = FALSE, hjust = 0.5, alpha = 0.5, min.segment.length = 0,
    show.legend = FALSE, nudge_y = 0.005, ylim = c(0.005, NA) 
  ) +
  scale_fill_manual(values = color_palette) +
  scale_color_manual(values = color_palette) +
  scale_alpha_manual(values = c(0, 0.5)) +
  # scale_x_log10() +
  # facet_wrap(~ metric, scales = "free") +
  labs(x = "", y = "Probability Density") +
  main_theme +
  theme(
    axis.title.x = element_blank(),
  )
ggsave("limit_of_detection.png", path="figures/lod", width = 12, height = 8)

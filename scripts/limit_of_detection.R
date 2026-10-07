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
window_size <- 3
time_cutoff <- 72
lod_cutoff <- 0.95
lob_quantile <- 0.95
min_dilution <- -11
n_points <- 10000
pos_sample_id <- "141234"
neg_group <- "Negative Control"
pos_group <- "Positive Control"
plot_metric <- "combined"

files <- list.files("raw/limit-of-detection", ".xlsx", full.names = TRUE, recursive = TRUE)
neg_groups <- c("sample", "well", "dilution", "reaction", "assay", "group")
groups <- c(neg_groups, "date", "reader", "tech")
metrics <- c("mpr", "auc", "ms")
# lower_groups <- c("dilution", "group", "sample_type", "assay", "cutoff", metrics)
# Grouping shared by the negative and positive summaries so they stay aligned
summary_groups <- c("sample", "group", "metric", "dilution")

extract_file_meta <- function(x, pattern, n) {
  str_split_i(x, pattern, n) %>%
    str_remove("\\.[[:alpha:]]+$")
}

rename_cols <- function(df) {
  df %>%
  rename(
    sample = "Sample IDs",
    dilution = "Dilutions"
  ) %>%
  rename_with(tolower)
}

# One-sided Mahalanobis distance from the negative control distribution.
# Deviations below the negative mean are set to 0 so that only
# higher-than-negative values count toward the distance.
maha_dist <- function(x, mu, sigma) {
  x %>%
    as.matrix() %>%
    sweep(2, mu) %>%
    pmax(0) %>%
    mahalanobis(center = rep(0, length(mu)), cov = sigma) %>%
    sqrt()
}

# Adds the combined distance metric and pivots all metrics to long format
add_combined_long <- function(df, mu, sigma) {
  df %>%
    mutate(combined = maha_dist(pick(all_of(metrics)), mu, sigma)) %>%
    pivot_longer(c(all_of(metrics), combined), names_to = "metric", values_to = "value")
}

# The combined distance is not normal, so its LoB is the empirical quantile
calc_lob <- function(value, metric, lob_quant) {
  if (metric == "combined") {
    quantile(value, lob_quant, names = FALSE)
  } else {
    mean(value) + qnorm(lob_quant) * sd(value)
  }
}

# Common summary statistics used by both the negative and positive tables.
# Extra summaries can be passed through `...`.
summarize_values <- function(df, ..., .by = summary_groups) {
  df %>%
    summarize(
      total = n(),
      across(value, list(min = min, max = max, mean = mean, sd = sd), .names = "{.fn}"),
      ...,
      .by = all_of(.by)
    )
}

# Fitted Gaussian evaluated on a grid; `alpha` marks the area above the LoB
make_distro <- function(x_min, x_max, mean, sd, lob, shade) {
  tibble(
    x = seq(x_min, x_max, length.out = n_points),
    y = dnorm(x, mean, sd),
    cum_y = cumsum(y),
    alpha = shade & x >= lob
  )
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

df_neg_wide <- read_parquet("data/calcs.parquet") %>%
  unnest(where(is.list)) %>%
  rename_cols() %>%
  filter(cutoff == time_cutoff & sample == "N" & assay == "RT-QuIC") %>%
  mutate(group = neg_group) %>%
  select(all_of(c(neg_groups, metrics)))

# Negative control center and covariance for the combined metric
mu_neg <- colMeans(df_neg_wide[metrics])
sigma_neg <- cov(df_neg_wide[metrics])

df_neg_sum <- df_neg_wide %>%
  add_combined_long(mu_neg, sigma_neg) %>%
  summarize_values(lob = calc_lob(value, cur_group()$metric, lob_quantile), .by = summary_groups)

df_raw <- map_dfr(files, get_raw, np = norm_point, w = window_size, zero = FALSE) %>%
  rename_cols() %>%
  mutate(group = pos_group)

df_cal <- df_raw %>%
  calculate_metrics(
    groups, time_col = "time", ttt_values = "norm", auc_values = "norm", 
    norm_col = "norm", deriv_col = "deriv", threshold = threshold
  ) %>%
  rename_with(tolower) %>%
  filter(sample == pos_sample_id) %>%
  select(all_of(c(groups, metrics))) %>%
  add_combined_long(mu_neg, sigma_neg) %>%
  # Errors if the negative summary ever has more than one LoB per metric
  left_join(distinct(df_neg_sum, metric, lob), by = "metric", relationship = "many-to-one")

df_pos_sum <- df_cal %>%
  filter(dilution >= min_dilution) %>%
  summarize_values(
    n = sum(value > lob),
    perc = n / total,
    .by = c(summary_groups, "lob")
  ) %>%
  mutate(
    # Area of the fitted positive Gaussian above the negative control LoB
    p_detect = pnorm(lob, mean, sd, lower.tail = FALSE),
    detected = p_detect >= lod_cutoff
  )

# Both groups go through the same distribution step on a shared x grid per metric
df_lod <- bind_rows(df_neg_sum, df_pos_sum) %>%
  mutate(x_min = 0, x_max = max(max), .by = metric) %>%
  mutate(
    metric = factor(metric, levels = c("mpr", "auc", "ms", "combined"), labels = c("MPR", "AUC", "MS", "Combined")),
    distro = pmap(list(x_min,x_max, mean, sd, lob, group == pos_group), make_distro),
    max_p = map_dbl(distro, \(d) max(d$y))
  ) %>%
  select(-c(x_min, x_max))

df_lod_pos <- df_lod %>%
  filter(group == pos_group)

# LoD: most dilute level where every more concentrated level also passes
df_lod_cut <- df_lod_pos %>%
  arrange(metric, desc(dilution)) %>%
  # Next more dilute level, captured before filtering drops it
  mutate(
    next_dilution = lead(dilution),
    next_p_detect = lead(p_detect),
    slope = (next_p_detect - p_detect) / (next_dilution - dilution),
    intercept = p_detect - slope * dilution,
    lod_dil = (lod_cutoff - intercept) / slope,
    # low_conc_dil = 
    .by = metric
  ) %>%
  filter(cumall(detected), .by = metric) %>%
  slice_min(dilution, n = 1, by = metric) %>%
  select(metric, lod_dil, dilution, p_detect, next_dilution, next_p_detect, slope, intercept, perc, detected, max_p)

# df_intersect <- df_lod_cut %>%

df_lod_pos %>%
  pivot_longer(c(perc, p_detect), names_to = "source", values_to = "rate") %>%
  mutate(
    source = factor(source, levels = c("perc", "p_detect"), labels = c("Fraction of reps above LoB", "Area of fitted curve above LoB"))
  ) %>%
  ggplot(aes(dilution, rate, color = source)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = lod_cutoff, linetype = "dashed") +
  geom_label_repel(
    aes(x = lod_dil, y = 0, label = sprintf("Dilution: %s", signif(lod_dil, 3))),
    data = df_lod_cut, 
    inherit.aes = FALSE,
    alpha = 0.5,
    min.segment.length = 0, 
    nudge_x = -1,
    nudge_y = 0.3
  ) +
  geom_vline(aes(xintercept = lod_dil), data = df_lod_cut, linetype = "dashed") +
  scale_x_continuous(n.breaks = length(unique(df_lod$dilution))) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(~metric) +
  labs(x = "Log10(Dilution)", y = "Rate") +
  main_theme +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
ggsave("lod_plot.png", path="figures/lod", width = 12, height = 8)

df_lod %>%
  unnest(distro) %>%
  mutate(dilution = as.factor(dilution)) %>%
  ggplot(aes(x, y, linetype = group)) +
  geom_point(size = 8, alpha = 0) +
  geom_ribbon(aes(ymin = 0, ymax = y, fill = dilution, alpha = alpha), color = "black", show.legend = FALSE) +
  geom_vline(aes(xintercept = lob), data = df_lod, linetype = "solid", inherit.aes = FALSE) +
  geom_label_repel(
    aes(x = mean, y = max_p, fill = as.factor(dilution),
        label = sprintf("Dilution: %s\nOverlap: %s", dilution, signif(1 - p_detect, 3))),
    data = df_lod, 
    inherit.aes = FALSE,
    size = 3,
    alpha = 0.75,
    min.segment.length = 0, 
    max.iter = 10000,
    max.time = 2,
    force = 4,
    force_pull = 3, 
    xlim = c(NA, Inf),
    # ylim = c(NA, Inf),
    hjust = 1,
    vjust = 1,
    box.padding = 1,
    label.padding = 0.1,
    seed = 71957427,
    # nudge_y = 0.1, nudge_x = -0.1, 
    show.legend = FALSE
  ) +
  scale_fill_brewer(3) +
  scale_color_brewer(3) +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_alpha_manual(values = c(0, 0.5)) +
  scale_x_continuous(expand = expansion()) +
  # scale_x_log10(expand = expansion()) +
  facet_wrap(~metric, scales = "free") +
  labs(y = "Probability Density") +
  main_theme +
  theme(axis.title.x = element_blank())
ggsave("limit_of_detection.png", path="figures/lod", width = 12, height = 8)


# Example Graph ----------------------------------------------------------


# Example schematic of the LoB/LoD approach using simulated distributions.
# Parameters are illustrative only, not taken from the data.
ex_neg <- tibble(label = "Negative", mean = 10, sd = 2)
ex_lob <- qnorm(lob_quantile, ex_neg$mean, ex_neg$sd)

ex_sum <- tibble(
  dilution = c(-5, -6, -7, -8),
  mean = c(21, 20, 17, 14.5),
  sd = c(2.6, 4.2, 5, 2.7)
) %>%
  mutate(label = sprintf("Dilution: %s", dilution)) %>%
  bind_rows(ex_neg, .) %>%
  mutate(
    label = fct_inorder(label),
    p_detect = pnorm(ex_lob, mean, sd, lower.tail = FALSE),
    detected = p_detect >= lod_cutoff
  )

ex_curves <- ex_sum %>%
  mutate(curve = map2(mean, sd, \(m, s) tibble(x = seq(0, 45, length.out = n_points), y = dnorm(x, m, s)))) %>%
  unnest(curve) %>%
  # Negative: false positives above the LoB. Positives: missed detections below it.
  mutate(shade = if_else(is.na(dilution), x >= ex_lob, x < ex_lob))

ex_labels <- ex_sum %>%
  mutate(
    y = dnorm(mean, mean, sd),
    text = sprintf(
      "%s\nAbove LoB: %s", label, percent(p_detect, 0.1)
    )
  )

# Negative graph
ex_curves %>%
  filter(label == "Negative") %>%
  ggplot(aes(x, y), color = "blue") +
  geom_ribbon(aes(ymin = 0, ymax = y), data = filter(ex_curves, shade, label == "Negative"), fill = "blue", alpha = 0.4, color = NA) +
  geom_line(linewidth = 1, color = "blue") +
  geom_vline(xintercept = ex_lob, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 20),expand = expansion(c(0, 0.01))) +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  annotate(
    "label", x = ex_lob, y = max(ex_labels$y) * 0.9, hjust = 0.4,
    label = sprintf("LoB = %sth percentile of negatives", lob_quantile * 100)
  ) +
  labs(
    title = "Example: Limit of Blank",
    x = "Metric Value",
    y = "Probability Density",
  ) +
  main_theme 
ggsave("lob_example_neg.png", path = "figures/lod", width = 12, height = 8)


# Full graph
ex_curves %>%
  ggplot(aes(x, y, color = label)) +
  geom_ribbon(aes(ymin = 0, ymax = y, fill = label), data = filter(ex_curves, shade), alpha = 0.4, color = NA) +
  geom_line(linewidth = 1) +
  geom_vline(xintercept = ex_lob, linetype = "dashed") +
  annotate(
    "label", x = ex_lob, y = max(ex_labels$y) * 1.1, hjust = 0.5,
    label = sprintf("LoB = %sth percentile of negatives", lob_quantile * 100)
  ) +
  geom_label_repel(
    aes(x = mean, y = y, label = text, color = label),
    data = ex_labels,
    inherit.aes = FALSE,
    min.segment.length = 0,
    nudge_y = 0.05,
    xlim = c(30, Inf),
    direction = "y",
    seed = 71957427,
    alpha = 0.5,
    show.legend = FALSE
  ) +
  scale_color_manual(
    values = c("grey40", brewer_pal(palette = "Dark2")(nlevels(ex_sum$label) - 1)),
    aesthetics = c("color", "fill")
  ) +
  scale_x_continuous(limits = c(0, 40),expand = expansion(c(0, 0.01))) +
  scale_y_continuous(expand = expansion()) +
  labs(
    title = "Example: Determining LoB and LoD",
    x = "Metric Value",
    y = "Probability Density",
  ) +
  main_theme +
  theme(legend.position = "none")
ggsave("lod_example.png", path = "figures/lod", width = 12, height = 8)

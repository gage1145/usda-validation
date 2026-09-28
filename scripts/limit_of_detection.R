# This paper is very useful: https://pmc.ncbi.nlm.nih.gov/articles/PMC2556583/

library(tidyverse)
library(quicR)
library(lubridate)
library(arrow)


threshold <- 5
norm_point <- 3
files <- list.files("raw/limit-of-detection", ".xlsx", full.names = TRUE, recursive = TRUE)
groups <- c("Sample IDs", "Well", "Dilutions", "Reaction", "Assay", "date", "reader", "tech")
lower_groups <- c("dilution", "group", "sample_type", "assay", "mpr", "auc", "ms")

extract_file_meta <- function(x, pattern, n) {
  str_split_i(x, pattern, n) %>%
    str_remove("\\.[[:alpha:]]+$")
}

norm_eq <- function(x, mu, sigma) {
  term_1 <- 1 / (sigma * sqrt(2 * pi))
  term_2 <- exp(-((x - mu) ^ 2 / (2 * sigma ^ 2)))
  term_1 * term_2
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

df_neg <- read_parquet("data/data_dump.parquet") %>%
  select(all_of(lower_groups)) %>%
  filter(group == "Negative Control" & sample_type == "PLN" & assay == "RT-QuIC") %>%
  pivot_longer(c(mpr, auc, ms), names_to = "metric", values_to = "value") 

df_neg_sum <- df_neg %>%
  summarize(
    min = min(value),
    max = max(value),
    mean = mean(value),
    sd = sd(value),
    lob = mean(value) + 1.645 * sd(value),
    distro = list(tibble(
      x = seq(min, max, length.out = 100),
      y = norm_eq(x, mean, sd)
    )), 
    .by = c(metric, group)
  )

df_raw <- map_dfr(files, get_raw, np = norm_point, w = 3, zero = T)

df_cal <- calculate_metrics(df_raw, groups, threshold = threshold)  %>%
  filter(`Sample IDs` == "141234") %>%
  rename_with(tolower) %>%
  rename(dilution = dilutions) %>%
  mutate(group = "Positive Control") %>%
  select(all_of(lower_groups[-which(lower_groups == "sample_type")])) %>%
  pivot_longer(c(mpr, auc, ms), names_to = "metric", values_to = "value") %>%
  full_join(select(df_neg_sum, metric, lob), by = "metric") 

df_lod <- df_cal %>%
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
    min = min(min),
    max = max(max),
    .by = c(metric)
  ) %>%
  mutate(
    distro = list(tibble(
      x = seq(min, max, length.out = 100),
      y = norm_eq(x, mean, sd)
    )),
    .by = c(dilution, metric, group, lob)
  ) %>%
  full_join(df_neg_sum) 

df_lod %>%
  ggplot(aes(dilution, perc, color = metric)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = length(unique(df_lod$dilution))) +
  theme_bw() +
  theme(legend.position = "bottom")

low_conc_samp <- -6

df_lod %>%
  unnest(distro) %>%
  mutate(across(dilution, as.factor)) %>%
  ggplot(aes(x, y, color = dilution, linetype = group)) +
  geom_line() +
  facet_wrap(~ metric, scales = "free") +
  theme_bw() 


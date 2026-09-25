# This paper is very useful: https://pmc.ncbi.nlm.nih.gov/articles/PMC2556583/

library(tidyverse)
library(quicR)
library(lubridate)
library(arrow)


threshold <- 5
norm_point <- 2
files <- list.files("raw/limit-of-detection", ".xlsx", full.names = TRUE, recursive = TRUE)
groups <- c("Sample IDs", "Well", "Dilutions", "Reaction", "Assay", "date", "reader", "tech")

extract_file_meta <- function(x, pattern, n) {
  str_split_i(x, pattern, n) %>%
    str_remove("\\.[[:alpha:]]+$")
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
  filter(group == "Negative Control" & sample_type == "PLN")

lob_mpr <- mean(df_neg$mpr) + 1.645 * sd(df_neg$mpr)
lob_mpr
lob_auc <- mean(df_neg$auc) + 1.645 * sd(df_neg$auc)
lob_auc
lob_ms  <- mean(df_neg$ms)  + 1.645 * sd(df_neg$ms)
lob_ms

df_raw <- map_dfr(files, get_raw, np = norm_point, w = 3, zero = T)
df_cal <- calculate_metrics(df_raw, groups, threshold = threshold)  %>%
  filter(`Sample IDs` == "141234") %>%
  rename_with(tolower) %>%
  rename(dilution = dilutions) %>%
  mutate(group = "Positive Control")

df_lod <- df_cal %>%
  summarize(
    total = n(),
    n_mpr = sum(mpr > lob_mpr),
    perc_mpr = n_mpr / total,
    sd_mpr = sd(mpr),
    n_auc = sum(auc > lob_auc),
    perc_auc = n_auc / total,
    sd_auc = sd(auc),
    n_ms  = sum(ms  > lob_ms),
    perc_ms  = n_ms  / total,
    sd_ms  = sd(ms),
    .by = c(dilution)
  )

df_lod %>%
  pivot_longer(cols = c(perc_mpr, perc_auc, perc_ms), names_to = "metric", values_to = "value") %>%
  ggplot(aes(dilution, value, color = metric)) +
  geom_point() +
  geom_line() +
  scale_x_continuous(n.breaks = length(unique(df_lod$dilution))) +
  theme_bw() +
  theme(legend.position = "bottom")

low_conc_samp <- -6

df_combined <- df_neg %>%
  full_join(df_cal) %>%
  pivot_longer(cols = c(mpr, auc, ms), names_to = "metric", values_to = "value")

df_combined %>%
  mutate(across(dilution, as.factor)) %>%
  ggplot(aes(value, color = dilution, linetype = group)) +
  geom_density() +
  facet_wrap(~ metric, scales = "free") +
  theme_bw() 


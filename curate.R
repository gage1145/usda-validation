library(quicR)
library(tidyverse)
library(cli)
library(arrow)


threshold <- 5
norm_point <- 8

files <- list.files("raw/processedSamples", ".xlsx", full.names = TRUE, recursive = TRUE)

get_raw <- function(file) {
  extract_file_meta <- function(x, pattern) {
    pattern_count <- str_count(x, pattern)
    str_split_i(x, pattern, pattern_count + 1) %>%
      str_remove("\\.[[:alpha:]]+$") # Remove file extension.
  }
  rxn <- extract_file_meta(file, "/")
  assay <- extract_file_meta(rxn, "_")

  cli_alert_info(sprintf(" Reading file: %s", rxn))

  file %>%
    get_quic(norm_point = norm_point) %>%
    mutate(
      `Sample IDs` = str_remove(`Sample IDs`, "-P"),
      Dilutions = -log10(as.numeric(Dilutions)),
      Assay = assay,
      Reaction = rxn
    ) %>%
    suppressMessages() %>%
    suppressWarnings()
}

df_ <- map_dfr(files, get_raw)

calcs <- calculate_metrics(
  df_,
  "Sample IDs", "Dilutions", "Wells", "Assay", "Reaction",
  threshold = threshold
) %>%
  mutate(crossed = MPR > threshold)

df_sum <- calcs %>%
  summarize(
    across(
      c("MPR", "MS", "TtT", "RAF", "AUC"), 
      list(mean=mean, median=median, min=min, max=max, stdev=sd, var=var, iqr=IQR)
    ),
    reps = n(),
    thres_pos = sum(crossed) > reps / 2,
    .by = c(`Sample IDs`, Dilutions, Assay)
  )

write_parquet(df_, "data/processedSamples/raw.parquet")
write_parquet(calcs, "data/processedSamples/calcs.parquet")
write_parquet(df_sum, "data/processedSamples/summary.parquet")

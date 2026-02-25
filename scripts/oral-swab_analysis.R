library(quicR)
library(dplyr)
library(tidyr)
library(stringr)
library(cli)
library(arrow)



threshold <- 5
norm_point <- 8


get_raw <- function(file) {
  assay <- str_split_i(file, "_", 5) %>%
    str_remove(".xlsx")
  rxn <- str_split_i(file, "/", 3) %>%
    str_remove(".xlsx")
  
  cli_alert_info(sprintf(" Reading file: %s", rxn))
  
  file %>%
    get_quic(norm_point=norm_point) %>%
    mutate(
      dilution = -log10(as.numeric(Dilutions)),
      assay = assay,
      reaction = rxn
    ) %>%
    suppressMessages() %>%
    suppressWarnings()
}

files <- list.files("raw/oral-swabs", ".xlsx", full.names = TRUE)

df_ <- lapply(files, get_raw) %>%
  bind_rows() %>%
  filter(tolower(`Sample IDs`) != "empty")

calculate <- function (assay) {
  # I have to do it this way to apply a different threshold to each assay.
  df_ %>%
    filter(assay == assay) %>%
    calculate_metrics(
      "Sample IDs", "dilution", "Wells", "assay", "reaction",
      threshold=threshold
    ) %>%
    mutate(
      crossed = TtT != max(df_$Time)
    ) %>%
    assign(assay, .)
}

calcs <- lapply(c("Nano-QuIC", "RT-QuIC"), calculate) %>%
  bind_rows()

df_sum <- calcs %>%
  group_by(
    `Sample IDs`, dilution, assay, reaction
  ) %>%
  summarize(
    reps = n(),
    mean_MPR = mean(MPR),
    mean_MS  = mean(MS),
    mean_TtT = mean(TtT),
    mean_RAF = mean(RAF),
    thres_pos = sum(crossed) > reps / 2
  )

write_parquet(df_, "data/oral-swabs/raw.parquet")
write_parquet(calcs, "data/oral-swabs/calcs.parquet")
write_parquet(df_sum, "data/oral-swabs/summary.parquet")


library(quicR)
library(dplyr)
library(tidyr)
library(stringr)
library(cli)
library(arrow)

# readRenviron(".Renviron")
threshold <- 5
norm_point <- 8



files <- list.files("raw/necropsy", ".xlsx", full.names = TRUE)

get_raw <- function(file) {
  assay <- str_split_i(file, "_", 5) %>%
    str_remove(".xlsx")
  rxn <- str_split_i(file, "/", 3) %>%
    str_remove(".xlsx")
  
  cli_alert_info(sprintf(" Reading file: %s", rxn))
  
  file %>%
    get_quic(norm_point=norm_point) %>%
    mutate(
      Dilutions = -log10(as.numeric(Dilutions)),
      Assay = assay,
      Reaction = rxn
    ) %>%
    suppressMessages() %>%
    suppressWarnings()
}

df_ <- lapply(files, get_raw) %>%
    bind_rows()

calcs <- calculate_metrics(
  df_, 
  "Sample IDs", "Dilutions", "Wells", "Assay", "Reaction", 
  threshold=threshold
) %>%
  mutate(crossed = TtT != 72)
  # separate_wider_delim(
  #   "Sample IDs", 
  #   "_", 
  #   names = c("Sample IDs", "Tissue", "Side"), 
  #   too_few = "align_start"
  # )

df_sum <- calcs %>%
  group_by(`Sample IDs`, Dilutions, Assay) %>%
  summarize(
    reps = n(),
    mean_MPR = mean(MPR),
    mean_MS  = mean(MS),
    mean_TtT = mean(TtT),
    mean_RAF = mean(RAF),
    mean_AUC = mean(AUC),
    thres_pos = sum(crossed) > reps / 2
  )

write_parquet(df_, "data/necropsy/raw.parquet")
write_parquet(calcs, "data/necropsy/calcs.parquet")
write_parquet(df_sum, "data/necropsy/summary.parquet")


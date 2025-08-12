library(quicR)
library(dplyr)
library(tidyr)
library(stringr)
source("functions.R")



get_info <- function(file) {
  # assay <- str_split_i(file, "_", 5) %>%
  #   str_remove(".xlsx")
  
  organize_tables(file) %>%
    convert_tables() %>%
    mutate(
      Dilutions = -log10(as.numeric(Dilutions))
    ) %>%
    separate_wider_delim(
      `Sample IDs`, 
      "_", 
      names=c("Sample IDs", "Assay"),
      too_few="align_start"
    )
}

files <- list.files("raw/blood", ".xlsx", full.names = TRUE)

meta <- lapply(files, get_info) %>%
  bind_rows()

df_ <- lapply(files, get_raw) %>%
  bind_rows() %>%
  # rename("Preparation" = "Sample IDs") %>%
  mutate(`Sample IDs` = meta$`Sample IDs`) %>%
  mutate_at(2:ncol(.), as.numeric)

norm <- normalize_RFU(df_, transposed = TRUE)

# calcs <- calculate_metrics(norm, meta) %>%
#   mutate(
#     Assay = meta$Assay,
#     # Dilutions = meta$Dilutions,
#     crossed = TtT != 72
#   )

calcs <- data.frame(
  `Sample IDs` = meta$`Sample IDs`,
  Dilutions = meta$Dilutions,
  Assay = meta$Assay,
  check.names = FALSE
) %>%
  mutate(
    MPR = calculate_MPR(norm),
    MS = calculate_MS(norm),
    TtT = calculate_TtT(norm, 3),
    RAF = 1/TtT,
    crossed = TtT != 96
  )

df_sum <- calcs %>%
  group_by(`Sample IDs`, Dilutions, Assay) %>%
  summarize(
    reps = n(),
    mean_MPR = mean(MPR),
    mean_MS  = mean(MS),
    mean_TtT = mean(TtT),
    mean_RAF = mean(RAF),
    thres_pos = sum(crossed) > reps / 2
  )

df_ <- df_ %>%
  mutate(
    Dilutions = meta$Dilutions,
    Assay = meta$Assay
  ) %>%
  relocate(c(Dilutions, Assay), .after = `Sample IDs`)

norm <- norm %>%
  mutate(
    Dilutions = meta$Dilutions,
    Assay = meta$Assay
  ) %>%
  relocate(c(Dilutions, Assay), .after = `Sample IDs`)

write.csv(df_, "data/blood/raw.csv", row.names = FALSE)
write.csv(norm, "data/blood/norm.csv", row.names = FALSE)
write.csv(calcs, "data/blood/calcs.csv", row.names = FALSE)
write.csv(df_sum, "data/blood/summary.csv", row.names = FALSE)


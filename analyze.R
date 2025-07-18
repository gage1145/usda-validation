library(quicR)
library(dplyr)
library(tidyr)
library(stringr)



get_raw <- function(file) {
  get_real(file)[[1]] %>%
    transpose_real()
}

get_info <- function(file) {
  assay <- str_split_i(file, "_", 5) %>%
    str_remove(".xlsx")
  
  organize_tables(file) %>%
    convert_tables() %>%
    mutate(
      Dilutions = -log10(as.numeric(Dilutions)),
      Assay = assay
    )
}

files <- list.files("raw", ".xlsx", full.names = TRUE)

meta <- lapply(files, get_info) %>%
  bind_rows()

df_ <- lapply(files, get_raw) %>%
  bind_rows() %>%
  mutate(`Sample IDs` = meta$`Sample IDs`)

norm <- normalize_RFU(df_, transposed = TRUE)

calcs <- calculate_metrics(norm, meta) %>%
  mutate(
    Assay = meta$Assay,
    crossed = TtT != 72
  ) %>%
  separate_wider_delim(
    "Sample IDs", 
    "_", 
    names = c("Sample IDs", "Tissue"), 
    too_few = "align_start"
  )

df_sum <- calcs %>%
  group_by(`Sample IDs`, Dilutions, Assay, Tissue) %>%
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

write.csv(df_, "data/raw.csv", row.names = FALSE)
write.csv(norm, "data/norm.csv", row.names = FALSE)
write.csv(calcs, "data/calcs.csv", row.names = FALSE)
write.csv(df_sum, "data/summary.csv", row.names = FALSE)


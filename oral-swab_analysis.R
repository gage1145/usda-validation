library(quicR)
library(dplyr)
library(tidyr)
library(stringr)
source("functions.R")

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

files <- list.files("raw/oral-swabs", ".xlsx", full.names = TRUE)

key <- read.csv("data/oral-swabs/meta.csv", check.names = FALSE) %>%
  select("Sample IDs", "Animal IDs", "Months") %>%
  mutate(
    Months = Months %>%
      str_remove(" ") %>%
      str_remove("MPI") %>%
      as.integer()
  )

meta <- lapply(files, get_info) %>%
  bind_rows() %>%
  left_join(key, by="Sample IDs")

df_ <- lapply(files, get_raw) %>%
  bind_rows() %>%
  mutate(`Sample IDs` = meta$`Sample IDs`)

norm <- normalize_RFU(df_, transposed = TRUE)

calcs <- calculate_metrics(norm, meta) %>%
  mutate(
    `Animal IDs` = meta$`Animal IDs`,
    Months = meta$Months,
    Assay = meta$Assay,
    crossed = TtT != 72
  )
  # separate_wider_delim(
  #   "Sample IDs", 
  #   "_", 
  #   names = c("Sample IDs", "Tissue"), 
  #   too_few = "align_start"
  # )

df_sum <- calcs %>%
  group_by(`Sample IDs`, `Animal IDs`, Months, Dilutions, Assay) %>%
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
    `Animal IDs` = meta$`Animal IDs`,
    Months = meta$Months,
    Dilutions = meta$Dilutions,
    Assay = meta$Assay
  ) %>%
  relocate(c(`Animal IDs`, Months, Dilutions, Assay), .after = `Sample IDs`)

norm <- norm %>%
  mutate(
    `Animal IDs` = meta$`Animal IDs`,
    Months = meta$Months,
    Dilutions = meta$Dilutions,
    Assay = meta$Assay
  ) %>%
  relocate(c(`Animal IDs`, Months, Dilutions, Assay), .after = `Sample IDs`)

write.csv(df_, "data/oral-swabs/raw.csv", row.names = FALSE)
write.csv(norm, "data/oral-swabs/norm.csv", row.names = FALSE)
write.csv(calcs, "data/oral-swabs/calcs.csv", row.names = FALSE)
write.csv(df_sum, "data/oral-swabs/summary.csv", row.names = FALSE)


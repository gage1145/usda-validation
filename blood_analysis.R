library(quicR)
library(dplyr)
library(tidyr)
library(stringr)
source("functions.R")



get_raw <- function(file) {
  assay <- str_split_i(file, "_", 5) %>%
    str_remove(".xlsx")
  reaction <- str_split_i(file, "/", 3) %>%
    str_remove(".xlsx")
  
  get_quic(file) %>%
    mutate(
      Dilutions = -log10(as.numeric(Dilutions)),
      Assay = assay,
      Reaction = reaction
    ) %>%
    separate_wider_delim(
      `Sample IDs`, 
      "_", 
      names=c("Treatment", "Sample IDs"),
      too_few="align_end"
    )
}

files <- list.files("raw/blood", ".xlsx", full.names = TRUE)

df_ <- lapply(files, get_raw) %>%
  bind_rows()

calcs <- calculate_metrics(
  df_, 
  "Sample IDs", "Wells", "Treatment", "Dilutions", "Assay", "Reaction", 
  threshold=2.7
) %>%
  mutate(crossed = TtT != max(df_$Time))

df_sum <- calcs %>%
  group_by(`Sample IDs`, Treatment, Dilutions, Assay, Reaction) %>%
  summarize(
    reps = n(),
    mean_MPR = mean(MPR),
    mean_MS  = mean(MS),
    mean_TtT = mean(TtT),
    mean_RAF = mean(RAF),
    thres_pos = sum(crossed) > reps / 2
  )

write.csv(df_, "data/blood/raw.csv", row.names = FALSE)
write.csv(calcs, "data/blood/calcs.csv", row.names = FALSE)
write.csv(df_sum, "data/blood/summary.csv", row.names = FALSE)


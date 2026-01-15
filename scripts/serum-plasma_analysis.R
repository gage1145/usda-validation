library(quicR)
library(dplyr)
library(tidyr)
library(stringr)

readRenviron(".Renviron")
threshold <- as.numeric(Sys.getenv("THRESHOLD"))
norm_point <- as.integer(Sys.getenv("NORM_POINT"))



files <- list.files("raw/serum-plasma", ".xlsx", full.names = TRUE)

get_raw <- function(file) {
  assay <- str_split_i(file, "_", 5) %>%
    str_remove(".xlsx")
  reaction <- str_split_i(file, "/", 3) %>%
    str_remove(".xlsx")
  
  get_quic(file, norm_point=norm_point) %>%
    mutate(
      Dilutions = -log10(as.numeric(Dilutions)),
      Assay = assay,
      Reaction = reaction
    ) %>%
    separate_wider_delim(
      `Sample IDs`, 
      "_", 
      names=c("Status", "Sample", "Treatment"),
      too_few="align_start"
    )
}

df_ <- lapply(files, get_raw) %>%
  bind_rows()

calcs <- calculate_metrics(
  df_, 
  "Sample", "Treatment", "Status", "Wells", "Dilutions", "Assay", "Reaction", 
  threshold=threshold
) %>%
  mutate(crossed = TtT != max(df_$Time))

df_sum <- calcs %>%
  group_by(Sample, Status, Treatment, Dilutions, Assay, Reaction) %>%
  summarize(
    reps = n(),
    mean_MPR = mean(MPR),
    mean_MS  = mean(MS),
    mean_TtT = mean(TtT),
    mean_RAF = mean(RAF),
    thres_pos = sum(crossed) > reps / 2
  )

write_parquet(df_, "data/serum-plasma/raw.parquet")
write_parquet(calcs, "data/serum-plasma/calcs.parquet")
write_parquet(df_sum, "data/serum-plasma/summary.parquet")


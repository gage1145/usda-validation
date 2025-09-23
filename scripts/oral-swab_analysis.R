library(quicR)
library(dplyr)
library(tidyr)
library(stringr)

readRenviron(".Renviron")
threshold <- as.numeric(Sys.getenv("THRESHOLD"))
norm_point <- as.integer(Sys.getenv("NORM_POINT"))



# Metadata key for linking swabs to animals and timepoints.
key <- read.csv("data/oral-swabs/meta.csv", check.names = FALSE) %>%
  select("Sample IDs", "Animal IDs", "Months") %>%
  mutate(
    Months = Months %>%
      str_remove(" ") %>%
      str_remove("MPI") %>%
      as.integer()
  )

get_raw <- function(file) {
  assay <- str_split_i(file, "_", 5) %>%
    str_remove(".xlsx")
  rxn <- str_split_i(file, "/", 3) %>%
    str_remove(".xlsx")
  
  file %>%
    get_quic(norm_point=norm_point) %>%
    mutate(
      Dilutions = -log10(as.numeric(Dilutions)),
      Assay = assay,
      Reaction = rxn
    ) %>%
    left_join(key) %>%
    select("Sample IDs", "Dilutions", "Wells", "Animal IDs", "Months", "Assay", 
           "Reaction", "Time", "RFU", "Norm", "Deriv")
}

files <- list.files("raw/oral-swabs", ".xlsx", full.names = TRUE)

df_ <- lapply(files, get_raw) %>%
  bind_rows()

calculate <- function (assay) {
  # I have to do it this way to apply a different threshold to each assay.
  df_ %>%
    filter(Assay == assay) %>%
    calculate_metrics(
      "Sample IDs", "Dilutions", "Wells", "Animal IDs", "Months", "Assay", 
      "Reaction",
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
    `Sample IDs`, `Animal IDs`, Months, Dilutions, Assay, Reaction
  ) %>%
  summarize(
    reps = n(),
    mean_MPR = mean(MPR),
    mean_MS  = mean(MS),
    mean_TtT = mean(TtT),
    mean_RAF = mean(RAF),
    thres_pos = sum(crossed) > reps / 2
  )

write.csv(df_, "data/oral-swabs/raw.csv", row.names = FALSE)
write.csv(calcs, "data/oral-swabs/calcs.csv", row.names = FALSE)
write.csv(df_sum, "data/oral-swabs/summary.csv", row.names = FALSE)


library(quicR)
library(dplyr)
library(tidyr)
library(stringr)



files <- list.files("raw/necropsy", ".xlsx", full.names = TRUE)

get_raw <- function(file) {
  assay <- str_split_i(file, "_", 5) %>%
    str_remove(".xlsx")
  rxn <- str_split_i(file, "/", 3) %>%
    str_remove(".xlsx")
  
  file %>%
    get_quic(norm_point=4) %>%
    mutate(
      Dilutions = -log10(as.numeric(Dilutions)),
      Assay = assay,
      Reaction = rxn
    )
}

df_ <- lapply(files, get_raw) %>%
    bind_rows()

calcs <- calculate_metrics(
  df_, "Sample IDs", "Dilutions", "Wells", "Assay", "Reaction", threshold=2.7
) %>%
  mutate(
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

write.csv(df_, "data/necropsy/raw.csv", row.names = FALSE)
write.csv(calcs, "data/necropsy/calcs.csv", row.names = FALSE)
write.csv(df_sum, "data/necropsy/summary.csv", row.names = FALSE)


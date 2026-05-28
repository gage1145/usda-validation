library(quicR)
library(tidyverse)
library(cli)
library(arrow)


main <- function() {
  threshold <- 5
  norm_point <- 8
  only_new <- as.logical(Sys.getenv("ONLY_NEW"))


  user_input <- readline(sprintf("Environment variable 'ONLY_NEW' set to %s. Continue? [Y/n] ", only_new))
  user_happy <- tolower(user_input) == "y"
  if (!user_happy) stop("User stopped execution of curation script.")


  files <- list.files("raw/processedSamples", ".xlsx", full.names = TRUE, recursive = TRUE)

  extract_file_meta <- function(x, pattern) {
    pattern_count <- str_count(x, pattern)
    str_split_i(x, pattern, pattern_count + 1) %>%
      str_remove("\\.[[:alpha:]]+$") # Remove file extension.
  }

  if (only_new) {
    existing_raw_files  <- list.files("data/processedSamples", pattern = "raw.parquet$",     full.names = TRUE, recursive = TRUE)
    existing_data_files <- list.files("data/processedSamples", pattern = "calcs.parquet$",   full.names = TRUE, recursive = TRUE)
    existing_sum_files  <- list.files("data/processedSamples", pattern = "summary.parquet$", full.names = TRUE, recursive = TRUE)

    if (length(existing_data_files != 0)) {
      existing_raw_df  <- map_dfr(existing_raw_files,  read_parquet)
      existing_data_df <- map_dfr(existing_data_files, read_parquet)
      existing_sum_df  <- map_dfr(existing_sum_files,  read_parquet)

      existing_rxns <- existing_data_df$Reaction
      rxns <- sapply(files, function(x) extract_file_meta(x, "/"))
      files <- files[!(rxns %in% existing_rxns)]
    }
  }

  if (length(files) == 0) return(print("No new files to update"))

  get_raw <- function(file) {
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

  if (only_new) {
    df_    <- bind_rows(existing_raw_df, df_)
    calcs  <- bind_rows(existing_data_df, calcs)
    df_sum <- bind_rows(existing_sum_df, df_sum)
  }

  write_parquet(df_, "data/processedSamples/raw.parquet")
  write_parquet(calcs, "data/processedSamples/calcs.parquet")
  write_parquet(df_sum, "data/processedSamples/summary.parquet")
}

main()

main <- function() {
  require(quicR)
  require(tidyverse)
  require(cli)
  require(arrow)
  require(furrr)
  require(janitor)
  
  only_new <- TRUE
  threshold  <- 5
  norm_point <- 8
  print_progress <- FALSE
  raw_cols <- c(
    "Sample IDs", 
    "Dilutions", 
    "Well", 
    "Assay", 
    "Reaction", 
    "Time", 
    "RFU", 
    "Norm", 
    "Deriv"
  )
  
  plan(multisession, workers = parallel::detectCores() - 1)

  extract_file_meta <- function(x, pattern) {
    pattern_count <- str_count(x, pattern)
    str_split_i(x, pattern, pattern_count + 1) %>%
      str_remove("\\.[[:alpha:]]+$")
  }

  get_raw <- function(file, progress, cols) {
    rxn   <- extract_file_meta(file, "/")
    assay <- extract_file_meta(rxn, "_")

    if (progress) cli_alert_info(sprintf(" Reading file: %s", rxn))

    file %>%
      get_quic(norm_point = norm_point) %>%
      mutate(
        `Sample IDs` = str_remove(`Sample IDs`, "-P"),
        Dilutions = -log10(as.numeric(Dilutions)),
        Assay = assay,
        Reaction = rxn
      ) %>%
      select(all_of(cols)) %>%
      suppressMessages() %>%
      suppressWarnings()
  }

  user_input <- readline("Only new reactions will be updated. Continue [Y] or update all [n]? ")
  user_happy <- tolower(user_input) == "y"
  if (!user_happy) only_new <- FALSE

  files <- list.files("raw/processedSamples", ".xlsx", full.names = TRUE, recursive = TRUE)

  if (only_new) {
    existing_raw_files  <- list.files("data/processedSamples", pattern = "raw.parquet$",     full.names = TRUE, recursive = TRUE)
    existing_data_files <- list.files("data/processedSamples", pattern = "calcs.parquet$",   full.names = TRUE, recursive = TRUE)

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

  df_ <- future_map_dfr(files, get_raw, progress = print_progress, cols = raw_cols, .progress = TRUE)

  calcs <- calculate_metrics(
    df_,
    "Sample IDs", "Dilutions", "Well", "Assay", "Reaction",
    threshold = threshold
  ) %>%
    mutate(crossed = MPR > threshold)

  if (only_new) {
    df_    <- bind_rows(existing_raw_df, df_)
    calcs  <- bind_rows(existing_data_df, calcs)
  }

  df_ <- df_ %>%
    nest(.by=c(`Sample IDs`, Well, Dilutions, Assay, Reaction), .key = "data") %>%
    rename(sample = `Sample IDs`, well = Well, dilution = Dilutions, assay = Assay, rxn_name = Reaction)
  
  write_parquet(df_, "data/raw.parquet")
  write_parquet(calcs, "data/calcs.parquet")
}

main()
rm(main)

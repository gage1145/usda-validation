main <- function() {
  require(quicR)
  require(tidyverse)
  require(cli)
  require(arrow)
  require(furrr)
  require(janitor)
  source("globals.R")
  

  only_new <- TRUE
  print_progress <- FALSE
  cutoffs <- seq(12, 72, by = 12)
  grouping_cols <- c("Sample IDs", "Dilutions", "Well", "Assay", "Reaction")
  raw_cols <- c(grouping_cols, "Time",  "RFU",  "Norm",  "Deriv")
  n_cores <- parallel::detectCores(logical = FALSE) - 1

  cli_alert_info(sprintf(" Using %s cores", n_cores))
  plan(multisession, workers = n_cores)

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

  get_calcs <- function(cutoff, df, by, thresh, ...) {
    df_cutoff <- df %>%
      summarize(
        Time = max(Time),
        .by = all_of(by)
      ) %>%
      filter(Time >= cutoff) %>%
      select(-Time)

    df %>%
      inner_join(df_cutoff, by = by) %>%
      filter(Time <= cutoff) %>%
      calculate_metrics(by, threshold = thresh) %>%
      mutate(
        cutoff = cutoff,
        crossed = MPR > thresh
      )
  }

  user_input <- readline(" Only new reactions will be updated. Continue [Y] or update all [n]? ")
  user_happy <- tolower(user_input) == "y"
  if (!user_happy) only_new <- !only_new

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

  cli_alert_info(" Extracting Raw Data... ")
  df_ <- future_map_dfr(files, get_raw, progress = print_progress, cols = raw_cols, .progress = TRUE)

  cli_alert_info("\n Calculating Metrics... ")
  calcs <- map_dfr(cutoffs, get_calcs, df = df_, by = grouping_cols, thresh = threshold, .progress = TRUE) %>%
    nest(.by=grouping_cols, .key = "calcs")

  if (only_new) {
    df_    <- bind_rows(existing_raw_df, df_)
    calcs  <- bind_rows(existing_data_df, calcs)
  }

  df_ <- df_ %>%
    nest(.by=grouping_cols, .key = "data") %>%
    full_join(calcs, by = all_of(grouping_cols)) %>%
    rename(sample = `Sample IDs`, well = Well, dilution = Dilutions, assay = Assay, rxn_name = Reaction)
  
  write_parquet(df_, "data/raw.parquet")
  write_parquet(calcs, "data/calcs.parquet")
}

main()
rm(main)

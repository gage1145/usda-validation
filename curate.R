main <- function() {
  require(tidyverse)
  require(magrittr)
  require(parallel)
  require(janitor)
  require(arrow)
  require(furrr)
  require(quicR)
  require(cli)


  # Local variables --------------------------------------------------------


  progress <- FALSE # print files as they are read?
  output_file <- "data/data.parquet"
  raw_dir <- "raw/processedSamples"
  cutoffs <- seq(12, 72, by = 12)
  grouping_cols <- c("sample", "dilution", "well", "assay", "reaction")
  raw_cols <- c(grouping_cols, "time", "rfu", "norm", "deriv")
  n_cores <- detectCores(logical = FALSE) - 1

  # get_quic() options
  norm_point <- 8
  threshold <- 5
  window_size <- 3
  zero <- TRUE


  # Helper functions -------------------------------------------------------


  extract_file_meta <- function(x, pattern) {
    pattern_count <- str_count(x, pattern)
    str_remove(str_split_i(x, pattern, pattern_count + 1), "\\.[[:alpha:]]+$")
  }

  get_raw <- function(file, progress, ...) {
    if (progress) cli_alert_info(sprintf(" Reading file: %s", file))
    reaction <- extract_file_meta(file, "/")
    assay <- extract_file_meta(reaction, "_")
    data <- suppressWarnings(suppressMessages(get_quic(file, ...)))
    data$data %<>%
      mutate(
        reaction = reaction,
        assay = assay,
        sample = str_remove(sample, "-P"),
        dilution = -log10(as.numeric(dilution))
      )
    return(data)
  }

  get_calcs <- function(df, cutoffs, by, ...) {
    map_dfr(cutoffs, function(cutoff) {
      df %>%
        filter(time <= cutoff) %>%
        calculate_metrics(by, ...) %>%
        mutate(cutoff = cutoff)
    })
  }


  # Extract and analyze raw data -------------------------------------------


  # Set up parallel processing
  cli_alert_info(sprintf(" Using %s cores", n_cores))
  plan(multisession, workers = n_cores)

  # List of raw data Excel files
  files <- list.files(raw_dir, ".xlsx", full.names = TRUE)

  # Extract and format raw data
  cli_alert_info(" Extracting Raw Data... ")
  df_ <- future_map(
    files, get_raw,
    progress = progress, norm_point = norm_point,
    window_size = window_size, zero = zero, .progress = TRUE
  ) %>%
    map_dfr(as.data.frame) %>%
    select(all_of(raw_cols))

  # Calculate metrics (MPR, MS, AUC, etc.)
  cli_alert_info("\n Calculating Metrics... ")
  calcs <- get_calcs(df_, cutoffs, by = grouping_cols, threshold = threshold, zeroed = zero) %>%
    nest(.by = all_of(grouping_cols), .key = "calcs")

  # Write to parquet
  cli_alert_info("\n Writing to parquet... ")
  df_ %>%
    nest(.by = all_of(grouping_cols), .key = "data") %>%
    full_join(calcs, by = grouping_cols) %>%
    write_parquet(output_file)

  cli_alert_success(sprintf("\n Successfully wrote to %s", output_file))
}

main()
rm(main)

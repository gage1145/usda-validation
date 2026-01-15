# This script will run every analysis in the script sub-directory.
library(cli)

files <- list.files("scripts", full.names=TRUE)

run <- function(file) {
  cli_alert(sprintf(" Running file: %s", file))
  source(file)
}

lapply(files, run)

library(quicR)
library(tidyverse)
library(cli)

files <- list.files("raw", full.names = TRUE, recursive = TRUE)

get_pv <- function(file) {
  num_slash <- str_count(file, "/")
  name <- paste0(str_split_i(str_remove(file, ".xlsx"), "/", num_slash + 1))
  file_exists <- file.exists(paste0("figures/plate_views/", name, ".png"))
  
  if (file_exists) return(cli_alert(sprintf("Figure already exists for %s", name)))
  
  cli_alert_info(sprintf("Working on figure %s", name))
  get_quic(file) %>%
    suppressMessages() %>%
    as.data.frame() %>%
    mutate(dilution = -log10(as.numeric(dilution))) %>%
    plate_view(sep = " ", plot_deriv=FALSE) +
    ggtitle(name) +
    theme(
      axis.title = element_text(size=16),
      axis.text = element_text(size=10),
      strip.text = element_text(size=10),
      plot.title = element_text(hjust=0.5, size = 20)
    )
  ggsave(paste0(name, ".png"), path="figures/plate_views", width=12, height=8)
}

sapply(files, get_pv)

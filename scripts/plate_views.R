library(quicR)
library(stringr)
library(dplyr)
library(ggplot2)



files <- list.files("raw", full.names = TRUE, recursive = TRUE)

main_theme <- theme(
  axis.title = element_text(size=20),
  axis.text = element_text(size=12),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)

get_pv <- function(file) {
  name <- paste0(str_split_i(str_remove(file, ".xlsx"), "/", 3))
  if (!file.exists(paste0("figures/plate_views/", name, ".png"))) {
    df <- get_quic(file) %>%
      mutate(Dilutions = -log10(as.numeric(Dilutions)))
    plate_view(df, sep=" ", plot_deriv=FALSE) +
      main_theme +
      ggtitle(name) +
      theme(
        plot.title = element_text(hjust=0.5, face="bold"),
        strip.text = element_text(size=10)
      )
    ggsave(paste0(name, ".png"), path="figures/plate_views", width=12, height=8)
  }
}

sapply(files, get_pv)

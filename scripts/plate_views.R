library(quicR)
library(stringr)
library(dplyr)
library(ggplot2)
library(cli)
library(showtext)
library(tidyr)



font_add_google("Courier Prime", "courier_prime")
showtext.auto()

files <- list.files("raw", full.names = TRUE, recursive = TRUE)

# main_theme <- theme(
#   axis.title = element_text(size=20),
#   axis.text = element_text(size=12),
#   strip.text = element_text(size=16, face="bold"),
#   legend.title = element_text(size=12),
#   legend.text = element_text(size=12)
# )

dark_theme <- theme(
  plot.background = element_rect(fill="transparent", colour="#5cef88"),
  plot.title = element_text(family="courier_prime", colour="#5CEF88", size=24, hjust=0.5, face="bold"),
  axis.title = element_text(family="courier_prime", colour="#5CEF88", size=20),
  axis.text = element_text(family="courier_prime", colour="#5CEF88", size=12),
  strip.text = element_text(family="courier_prime", colour="#5CEF88", size=16, face="bold"),
  strip.background = element_blank(),
  legend.title = element_text(family="courier_prime", colour="#5CEF88", size=12),
  legend.text = element_text(family="courier_prime", colour="#5CEF88", size=12),
  panel.background = element_blank(),
  panel.border = element_rect(colour="#5cef88"),
  panel.grid = element_blank()
)

plate_view <- function(data, color="black", plate=96, sep="\n", plot_deriv=TRUE) {
  
  if (plate != 96 & plate != 384) {
    return("Invalid plate layout. Format should be either 96 or 384. ")
  }
  
  wells <- data %>%
    select("Sample IDs", "Dilutions", "Wells") %>%
    mutate_at("Dilutions", as.character) %>%
    group_by_at(1:3) %>%
    reframe() %>%
    full_join(
      expand.grid(
        {if (plate == 96) LETTERS[1:8] else LETTERS[1:16]},
        {if (plate == 96) sprintf("%02d", 1:12) else sprintf("%02d", 1:24)}
      ) %>%
        unite("Wells", 1,2, sep="")
    ) %>%
    mutate_all(function(x) replace_na(x, " ")) %>%
    suppressMessages()
  
  labels_lookup <- setNames(
    paste(wells$`Sample IDs`, wells$Dilutions, sep=sep),
    wells$Wells
  )
  
  p <- data %>%
    mutate_at("Dilutions", as.character) %>%
    full_join(wells) %>%
    suppressMessages() %>%
    ggplot(aes(.data$Time)) +
    geom_line(aes(y=.data$Norm), color=color) +
    {if (plot_deriv) geom_line(aes(y=.data$Deriv), color="blue")} +
    facet_wrap(
      vars(.data$Wells),
      nrow = ifelse(plate == 96, 8, 16),
      ncol = ifelse(plate == 96, 12, 24),
      labeller = as_labeller(labels_lookup)
    ) +
    labs(
      y = "RFU",
      x = "Time (h)"
    )
  
  print(p) %>%
    suppressWarnings() %>%
    suppressMessages()
}

get_pv <- function(file) {
  num_slash <- str_count(file, "/")
  name <- paste0(str_split_i(str_remove(file, ".xlsx"), "/", num_slash + 1))
  
  if (!file.exists(paste0("figures/plate_views/", name, ".png"))) {
    cli_alert_info(sprintf("Working on figure %s", name))
    df <- get_quic(file) %>%
      mutate(Dilutions = -log10(as.numeric(Dilutions)))
    plate_view(df, color="#5cef88", sep=" ", plot_deriv=FALSE) +
      # main_theme +
      ggtitle(name) +
      dark_theme
    ggsave(paste0(name, ".png"), path="figures/plate_views", width=6, height=4)
  } else {
    cli_alert(sprintf("Figure already exists for %s", name))
  }
}

sapply(files, get_pv)

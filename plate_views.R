library(quicR)



dark_bool = TRUE

main_theme <- theme(
  axis.title = element_text(size=20),
  axis.text = element_text(size=12),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)

dark_theme <- theme(
  # axis.text.x      = element_text(size=12, angle=45, face="bold",
  #                                 hjust=1, vjust=1, color="white"),
  plot.title       = element_text(hjust = 0.5, vjust=2, size = 16,
                                  face = "bold", color="white"),
  plot.background  = element_rect(fill="#1f1f1f", color="#1f1f1f"),
  panel.grid       = element_line(color="white"),
  # axis.text.y      = element_text(size = 12, color="white"),
  axis.title.y     = element_text(size = 14, color="white"),
  axis.title.x     = element_text(size = 14, color="white"),
  strip.background = element_rect(fill="transparent", color="white"),
  strip.text       = element_text(size=10, face="bold", color="white"),
  legend.text      = element_text(size=10, color="white")
)



# Plate View Function -----------------------------------------------------



plate_view <- function (df, meta, plate = 96) 
{
  if (plate != 96 & plate != 384) {
    return("Invalid plate layout. Format should be either 96 or 384. ")
  }
  df <- data.frame(df)
  colnames(df) <- c("Time", paste(meta[[1]], meta[[2]], sep = "."))
  template_columns <- expand.grid(if (plate == 96) {
    Var1 <- LETTERS[1:8]
  }
  else {
    Var1 <- LETTERS[1:16]
  }, if (plate == 96) {
    Var2 <- sprintf("%02d", 1:12)
  }
  else {
    Var2 <- sprintf("%02d", 1:24)
  })
  template_columns <- sort(paste0(template_columns$Var1, template_columns$Var2))
  rm(Var1, Var2)
  for (col in template_columns) {
    if (!(col %in% meta[[1]])) {
      df[col] <- NA
    }
  }
  template_columns <- as.data.frame(template_columns)
  colnames(template_columns) <- colnames(meta[1])
  full <- arrange_at(full_join(meta, as.data.frame(template_columns)), 
                     1) %>% suppressMessages()
  ID_labeller <- function(variable, value) {
    i <- full[, 2][full[, 1] == value]
    ifelse(is.na(i), " ", i)
  }
  ggplot(
    mutate(
      mutate(
        separate(
          reshape2::melt(
            df, id.vars = "Time"
          ), 
          "variable", c("Well", "ID"), "\\.", fill = "right"
        ), 
        Time = as.numeric(.data$Time), value = as.numeric(.data$value), 
        ID = as.character(.data$ID), Well = as.factor(.data$Well)
      ), 
      ID = replace_na(.data$ID, "none")
    ), 
    aes(x = .data$Time, y = .data$value)
  ) + 
    geom_line(color="white") + 
    labs(y = "RFU", x = "Time (h)") + 
    theme_classic() + 
    theme(
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5), 
      strip.background = element_blank(), 
      axis.text.y = element_blank()) + 
    suppressWarnings(facet_wrap(vars(.data$Well), nrow = ifelse(plate == 96, 8, 16), ncol = ifelse(plate == 96, 12, 24), 
                                labeller = ID_labeller))
}



# Plate Views -------------------------------------------------------------



files <- list.files("raw", full.names = TRUE, recursive = TRUE)
for (file in files) {
  name <- paste0(str_split_i(str_remove(file, ".xlsx"), "/", 3))
  if (!file.exists(paste0("figures/plate_views/", name, ".png"))) {
    locs <- get_sample_locations(file, dilution_bool = TRUE, dilution_fun = function(x) -log10(x), sep=" ")
    df <- get_real(file)[[1]]
    plate_view(df, locs) +
      {if (dark_bool) theme_transparent()} +
      {if (dark_bool) dark_theme else main_theme} +
      ggtitle(name) +
      theme(plot.title = element_text(hjust=0.5))
    ggsave(paste0(name, ".png"), path="figures/plate_views", width=12, height=8)
  }
}

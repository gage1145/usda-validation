library(quicR)



# Plate Views -------------------------------------------------------------



files <- list.files("raw", full.names = TRUE, recursive = TRUE)
for (file in files) {
  name <- paste0(str_split_i(str_remove(file, ".xlsx"), "/", 3))
  if (!file.exists(paste0("figures/plate_views/", name, ".png"))) {
    locs <- get_sample_locations(file, dilution_bool = TRUE, dilution_fun = function(x) -log10(x), sep=" ")
    df <- get_real(file)[[1]]
    plate_view(df, locs) +
      ggtitle(name) +
      theme(plot.title = element_text(hjust=0.5))
    ggsave(paste0(name, ".png"), path="figures/plate_views", width=12, height=8)
  }
}
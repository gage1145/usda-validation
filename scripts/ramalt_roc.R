library(tidyverse)
library(airtabler)
library(pROC)
library(janitor)

main_theme <- theme(
  plot.title = element_text(size=24, hjust=0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=16),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=16),
  legend.text = element_text(size=16)
)



# Load the data -----------------------------------------------------------
APP <- "app7KsgYl2jhOnYg7"

# Get the necessary tables
tables <- airtable(APP, c("animals", "results"))

results <- tables$results$select_all(
  filterByFormula = "{sample_type} = 'RAMALT'"
)

animals <- tables$animals$select_all()
animals <- animals %>%
  rename("animal" = "animal_id")



# Format the data ---------------------------------------------------------
df_ <- results %>%
  mutate(across(everything(), as.character)) %>%
  left_join(animals, by = "animal") %>%
  clean_names() %>%
  mutate(
    across(c(sample_id, animal), as.factor),
    assay = factor(assay, levels = c("RT-QuIC", "Nano-QuIC")),
    mpi   = as.integer(mpi),
    across(c(mpr, raf, ttt, ms, auc), as.numeric)
  )

# ROC dataset
# 0 MPI animals = confirmed negative, post-mortem = confirmed positive
# Excludes ante-mortem animals whose status is unknown
df_roc <- df_ %>%
  filter(mpi == 0 | mortem == "post-mortem") %>%
  mutate(
    positive = as.integer(mortem == "post-mortem")
  ) %>%
  pivot_longer(cols=c("mpr", "ms", "auc"))



# Prepare for ROC ---------------------------------------------------------
# Build all combinations of variables
combos <- expand.grid(
  m = unique(df_roc$name),
  a = unique(df_roc$assay),
  d = unique(df_roc$dilution),
  stringsAsFactors = FALSE
)

# Function to compute ROC + coords
compute_roc <- function(m, a, d) {
  sub_df <- df_roc %>%
    filter(name == m, assay == a, dilution == d)
  
  sub_roc <- roc(sub_df, response = "positive", predictor = "value")
  sub_roc$metric   <- m
  sub_roc$assay    <- a
  sub_roc$dilution <- d
  
  coord_df <- coords(sub_roc) %>%
    mutate(metric = m, assay = a, dilution = d)
  
  list(roc = sub_roc, coords = coord_df)
}

# Metric Labeller (capitalizes the metric acronyms)
relabel_metrics <- function(x) {
  lvls = sort(unique(x))
  factor(x, levels = lvls, labels = toupper(lvls))
}

roc_results  <- pmap(combos, compute_roc)
rocs     <- map(roc_results, "roc")
coord_df <- map_dfr(roc_results, "coords") %>% 
  mutate(
    youden = specificity + sensitivity - 1,
    metric = relabel_metrics(metric)
  )

# AUC summary table
auc_df <- combos %>%
  mutate(
    auc         = map_dbl(rocs, auc),
    specificity = 0.65,
    sensitivity = ifelse(a == "RT-QuIC", 0.15, 0.05),
    label       = paste0(a, " AUC = ", signif(auc, 3)),
    m = relabel_metrics(m)
  ) %>%
  rename(assay = a, dilution = d, metric = m) %>%
  arrange(desc(auc))

you_df <- coord_df %>%
  group_by(assay, dilution, metric) %>%
  filter(youden == max(youden, na.rm = TRUE))



# Generate the ROC figure -------------------------------------------------
coord_df %>%
  group_by(assay, dilution, metric) %>%
  arrange(sensitivity) %>%
  ggplot(aes(specificity, sensitivity, color = assay)) +
  geom_step(linewidth = 1) +
  geom_text(
    aes(label = label, color = assay),
    data = auc_df, hjust = 0, size = 6,
    show.legend = FALSE
  ) +
  scale_color_manual(values=c("darkcyan", "darkorange")) +
  facet_grid(vars(dilution), vars(metric)) +
  scale_x_reverse() +
  labs(y = "Sensitivity", x = "Specificity") +
  main_theme +
  theme(
    legend.position = "none",
    legend.title = element_blank()
  )

ggsave("ramalt_roc.png", path="figures/RAMALT", width=16, height=10)

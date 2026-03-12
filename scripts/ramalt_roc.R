library(tidyverse)
library(airtabler)
library(pROC)



main_theme <- theme(
  plot.title = element_text(size=24, hjust=0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=16),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
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

df_ <- results %>%
  mutate_all(as.character) %>%
  mutate(assay = factor(assay, level=c("RT-QuIC", "Nano-QuIC"))) %>%
  mutate_at(
    c("sample_id", "animal", "assay"),
    ~as.factor(.)
  ) %>%
  mutate_at("mpi", as.integer) %>%
  mutate_at(c("mpr", "raf", "ttt", "ms", "auc"), as.numeric) %>%
  left_join(animals, by="animal") %>%
  janitor::clean_names()


df_roc <- df_ %>%
  filter(mpi == 0 | mortem == "post-mortem") %>%
  mutate(
    positive = as.integer(mortem == "post-mortem")
  ) %>%
  pivot_longer(cols=c("mpr", "ms", "auc")) 
  # group_by(animal, assay, dilution, name, positive) %>%
  # get_summary_stats(value, type="common")

rocs <- list()
coords <- list()
for (m in unique(df_roc$name)) {
  for (a in unique(df_roc$assay)) {
    for (d in unique(df_roc$dilution)) {
      print(paste(metric, assay, dilution))
      sub_df <- df_roc %>%
        filter(name == m) %>%
        filter(assay == a) %>%
        filter(dilution == d)
      sub_roc <- sub_df %>%
        roc(response="positive", predictor=value)
      
      sub_roc$metric <- m
      sub_roc$assay <- a
      sub_roc$dilution <- d
      # names(sub_roc) <- roc_name
      rocs <- append(rocs, list(sub_roc))
      
      coord_df <- coords(sub_roc) %>%
        mutate(metric=m, assay=a, dilution=d)
      coords <- append(coords, list(coord_df))
    }
  }
}

coord_df <- bind_rows(coords)



assays <- sapply(rocs, function(x) x$assay)
dilutions <- sapply(rocs, function(x) x$dilution)
metrics <- sapply(rocs, function(x) x$metric)
aucs <- sapply(rocs, auc)

df_auc <- data.frame(
  assay = assays,
  dilution = dilutions,
  metric = metrics,
  auc = aucs
) %>%
  mutate(
    sensitivity = ifelse(assay=="RT-QuIC", 0.05, 0.15),
    specificity = 0.45,
    label = paste0("AUC = ", round(auc, 3))
  ) %>%
  arrange(desc(auc))

coord_df %>%
  group_by(assay, dilution, metric) %>%
  arrange(sensitivity) %>%
  ggplot(aes(1-specificity, sensitivity, color=assay)) +
  geom_step() +
  geom_text(aes(label=label, color=assay),
            data=df_auc, hjust=0, size=6) +
  facet_grid(vars(dilution), vars(metric)) +
  main_theme +
  theme(
    legend.title = element_blank()
  )
ggsave("ramalt_roc.png", path="figures/RAMALT", width=16, height=10)





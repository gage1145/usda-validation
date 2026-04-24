library(airtabler)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(lubridate)
library(pROC)
library(zoo)

APP <- "app7KsgYl2jhOnYg7"
KEY <- Sys.getenv("AIRTABLE_API_KEY")

tables  <- airtable(APP, c("animals", "sample-types", "samples", "reactions", "results"))

quick_clean <- function(x) {
  var_name <- deparse(substitute(x))
  var_name <- str_remove(var_name, "s$")
  print(var_name)
  x %>%
    clean_names() %>%
    rename("{var_name}.id" := id)
}


# Load and clean the data from Airtable ----------------------------------


animals <- tables$animals$select_all()
animals <- quick_clean(animals) %>%
  select(!where(is.list))

samples <- tables$samples$select_all()
samples <- quick_clean(samples) %>%
  unnest(animal) %>%
  rename(animal.id = animal)

sample_types <- tables$`sample-types`$select_all()
sample_types <- quick_clean(sample_types) %>%
  unnest(samples) %>%
  rename(sample.id = samples)

results <- tables$results$select_all()
results <- quick_clean(results) %>%
  unnest(cols = c(sample, reaction)) %>%
  rename(
    sample.id = sample,
    reaction.id = reaction
  )

reactions <- tables$reactions$select_all()
reactions <- quick_clean(reactions) %>%
  select(!where(is.list))


# Join the data into a single dataframe ----------------------------------


df_joined <- animals %>%
  left_join(samples, by = c("animal.id")) %>%
  left_join(sample_types, by = c("sample.id")) %>%
  left_join(results, by = c("sample.id")) %>%
  left_join(reactions, by = c("reaction.id")) %>%
  select(!where(is.list)) %>%
  select(!matches("\\.id$", ignore.case = FALSE)) %>%
  select(!starts_with("created_time")) %>%
  rename_all(~ str_remove(., "\\.\\D+$"))

write.csv(df_joined, "dashboard/data/results.csv", row.names = FALSE)
message("Wrote dashboard/data/results.csv (", nrow(results), " rows)")


# Clean and transform ----------------------------------------------------


df_ <- df_joined %>%
  select(-any_of(c("temperature", "result_id", "sample_type_id", "concentration", "inoculum"))) %>%
  mutate(
    sample_type = ifelse(str_detect(sample_type, "nasal"), "nasal swab", sample_type),
    sample_type = ifelse(str_detect(sample_type, "oral"), "oral swab", sample_type),
    across(any_of(c("animal_id", "room_number", "group", "sex", "genotype", "sample_id", "mortem", "well", "reader")), as.factor),
    across(matches("date|dob"), as_date),
    assay  = factor(assay, levels = c("RT-QuIC", "Nano-QuIC")),
    mpi    = as.integer(mpi),
    across(any_of(c("mpr", "raf", "ttt", "ms", "auc", "dilution")), as.numeric)
  ) %>%
  filter(
    !(str_detect(sample_type, "swab") & dilution < -1)
  )

df_lerp <- df_ %>%
  filter(!is.na(mpi)) %>%
  group_by(animal_id, sample_type, dilution, assay, group, room_number, sex, genotype, dob, dod_collection_date, species, mortem) %>%
  complete(mpi = min(mpi, na.rm = TRUE):max(mpi, na.rm = TRUE)) %>%
  mutate(
    interpolated_value = is.na(mpr),
    across(c(mpr, ms, auc, ttt, auc), na.spline)
  )

write.csv(df_lerp, "dashboard/data/results_clean.csv", row.names = FALSE)
message("Wrote dashboard/data/results_clean.csv")


# Compute ROC curves -----------------------------------------------------


df_roc <- df_ %>%
  filter(sample_type == "RAMALT") %>%
  filter(mpi == 0 | mortem == "post-mortem") %>%
  mutate(positive = as.integer(mortem == "post-mortem")) %>%
  pivot_longer(cols = any_of(c("mpr", "ms", "auc")), names_to = "name", values_to = "value")

relabel_metrics <- function(x) {
  lvls <- sort(unique(x))
  factor(x, levels = lvls, labels = toupper(lvls))
}

compute_roc <- function(m, a, d) {
  sub_df <- df_roc %>% filter(name == m, assay == a, dilution == d)
  if (nrow(sub_df) < 2 || length(unique(sub_df$positive)) < 2) return(NULL)
  sub_roc <- tryCatch(
    roc(sub_df, response = "positive", predictor = "value", quiet = TRUE),
    error = function(e) NULL
  )
  if (is.null(sub_roc)) return(NULL)
  coord_df <- coords(sub_roc) %>%
    mutate(metric = m, assay = a, dilution = d)
  list(roc = sub_roc, coords = coord_df)
}

combos <- expand.grid(
  m = unique(df_roc$name),
  a = levels(df_roc$assay),
  d = unique(df_roc$dilution),
  stringsAsFactors = FALSE
)

roc_list     <- Map(compute_roc, combos$m, combos$a, combos$d)
valid        <- !sapply(roc_list, is.null)
roc_list     <- roc_list[valid]
combos_valid <- combos[valid, ]

coord_df <- lapply(roc_list, `[[`, "coords") %>%
  bind_rows() %>%
  mutate(metric = relabel_metrics(metric))

auc_df <- combos_valid %>%
  mutate(
    auc         = sapply(roc_list, function(r) as.numeric(auc(r$roc))),
    specificity = 0.5,
    sensitivity = ifelse(a == "RT-QuIC", 0.15, 0.05),
    label       = paste0(a, " AUC = ", signif(auc, 3)),
    m           = relabel_metrics(m)
  ) %>%
  rename(assay = a, dilution = d, metric = m) %>%
  arrange(desc(auc))

write.csv(coord_df, "dashboard/data/roc_coords.csv", row.names = FALSE)
write.csv(auc_df,   "dashboard/data/roc_auc.csv",    row.names = FALSE)
message("Wrote dashboard/data/roc_coords.csv and roc_auc.csv")

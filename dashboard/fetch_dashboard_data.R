library(airtabler)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)

APP <- "app7KsgYl2jhOnYg7"

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


# Write the data to a CSV file -------------------------------------------


write.csv(df_joined, "dashboard/data/results.csv", row.names = FALSE)
message("Wrote dashboard/data/results.csv (", nrow(results), " rows)")

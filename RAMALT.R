# Load packages
library(readxl)
library(dplyr)
library(ggplot2)
library(lme4)
library(ggeffects)
library(emmeans)
library(broom)



# Read Excel file
attr_data <- read_excel("USDA_Attribute.xlsx")

names(summary)       # columns in your assay result data
names(attr_data)     # columns in your attribute data

# Rename Columns
attr_data <- attr_data %>%
  rename(
    Sample_ID = `Animal ID`,
    Room = `Room #`,
    exposure_group = Group,
    sex = Sex,
    genotype = GENOTYPE,
    inoculum = Inoculum,
    elisa_overall = `ELISA Overall Result`
  )


# Merge with Summary Data

summary <- read.csv("~/Documents/GitHub Projects/usda-validation/data/RAMALT/summary.csv")

summary_data <- summary %>%
  left_join(attr_data, by = c("Animal.IDs" = "Sample_ID"))

# Prepare for Modeling
summary_data <- summary_data %>%
  mutate(
    thres_pos = as.numeric(thres_pos),
    Assay = factor(Assay),
    exposure_group = factor(exposure_group)
  )

# Identify records with no attribite data
table(is.na(summary_data$exposure_group))

summary_data %>%
  filter(is.na(exposure_group)) %>%
  distinct(`Sample.IDs`)

# Remove control samples
summary_data <- summary_data[!summary_data$`Sample.IDs` %in% c("N", "P"), ]

# Double-check no more missing exposure_group
table(is.na(summary_data$exposure_group))

# Check sample distribution
table(summary_data$exposure_group)
table(summary_data$Dilutions)



summary_data <- summary_data %>%
  mutate(sample_positive = ifelse(thres_pos == 1, 1, 0))


oral_summary <- summary_data %>%
  # Keep only animals with known exposure
  filter(!is.na(exposure_group)) %>%
  filter(!is.na(Animal.IDs)) %>%
  # Collapse replicates per animal, assay, dilution, month
  group_by(Animal.IDs, exposure_group, Assay, Dilutions, Months) %>%
  summarise(
    sample_positive = as.integer(any(thres_pos == 1, na.rm = TRUE)),
    mean_MPR = mean(mean_MPR, na.rm = TRUE),
    n_reps = n(),
    .groups = "drop"
  )


oral_summary_counts <- oral_summary %>%
  group_by(Assay, Dilutions, Months, exposure_group) %>%
  summarise(
    n_animals = n_distinct(Animal.IDs),
    n_positive = sum(sample_positive, na.rm = TRUE),
    n_negative = n_animals - n_positive,
    .groups = "drop"
  ) %>%
  arrange(Assay, Dilutions, Months)


oral_summary_counts <- oral_summary_counts %>%
  mutate(pct_positive = n_positive / n_animals * 100)

#Filter Oral Swabs for dilution and two assays
oral_dil1 <- oral_summary %>%
  filter(Dilutions == -3, Assay %in% c("Nano-QuIC", "RT-QuIC"))

# Get list of necropsy animals
necropsy_animals <- summary_data_necropsy %>%
  pull(Sample.IDs) %>%
  unique()

#Filter oral swabs to only these animals
oral_dil1 <- oral_dil1 %>%
  filter(Animal.IDs %in% necropsy_animals)

#Summarize positivity oer assay
oral_summary_table <- oral_dil1 %>%
  group_by(Assay) %>%
  summarise(
    n_animals_tested = n_distinct(Animal.IDs),
    n_positive = sum(sample_positive, na.rm = TRUE),
    n_negative = n_animals_tested - n_positive,
    .groups = "drop"
  )

#Make table to see individual results
oral_summary_animals <- oral_dil1 %>%
  group_by(Animal.IDs, Assay) %>%
  summarise(
    sample_positive = as.integer(any(sample_positive == 1, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Assay,
    values_from = sample_positive
  ) %>%
  arrange(Animal.IDs)


#New
#Collapse oral swabs per animal/assay
oral_dil1_collapsed <- oral_summary %>%
  filter(Dilutions == -3, Assay %in% c("Nano-QuIC", "RT-QuIC")) %>%
  group_by(Animal.IDs, Assay) %>%
  summarise(
    sample_positive = as.integer(any(sample_positive == 1, na.rm = TRUE)),
    .groups = "drop"
  )

#Pivot to wide format
oral_wide <- oral_dil1_collapsed %>%
  pivot_wider(
    names_from = Assay,
    values_from = sample_positive
  )

#Add ELISA from necropsy
final_table <- oral_wide %>%
  left_join(
    summary_data_necropsy %>%
      select(Sample.IDs, elisa_overall, exposure_group) %>%
      distinct() %>%
      rename(Animal.IDs = Sample.IDs),
    by = "Animal.IDs"
  ) %>%
  arrange(Animal.IDs)


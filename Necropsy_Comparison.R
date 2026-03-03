# Load packages
library(dplyr)
library(ggplot2)
library(irr)
library(readxl)
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

summary <- read.csv("~/Documents/GitHub Projects/usda-validation/data/necropsy/summary.csv")

summary_data <- summary %>%
  left_join(attr_data, by = c("Sample.IDs" = "Sample_ID"))

# Prepare for Modeling
summary_data <- summary_data %>%
  mutate(
    thres_pos = as.numeric(thres_pos),
    Assay = factor(Assay),
    Tissue = factor(Tissue),
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
table(summary_data$Tissue)
table(summary_data$Dilutions)




#Not as useful due to small sample sizes
# Compare Agreement
# 1️⃣ Generate RT/Nano binary positivity
summary_data <- summary_data %>%
  mutate(sample_positive = ifelse(thres_pos == 1, 1, 0))

# 2️⃣ Collapse multiple RPLN samples per animal
animal_tissue_combined <- summary_data %>%
  group_by(Sample.IDs, Assay, Dilutions, Tissue) %>%
  summarise(
    rtquic_positive = as.integer(any(sample_positive == 1, na.rm = TRUE)),  # combine multiple samples
    elisa_positive = first(elisa_overall),  # one ELISA per animal
    .groups = "drop"
  ) %>%
  # Exclude animals without ELISA
  filter(!is.na(elisa_positive))

# 3️⃣ Summarize agreement counts
agreement_summary <- animal_tissue_combined %>%
  group_by(Assay, Dilutions, Tissue) %>%
  summarise(
    n_animals = n(),
    n_rtquic_pos = sum(rtquic_positive, na.rm = TRUE),
    n_elisa_pos = sum(elisa_positive == 1, na.rm = TRUE),
    n_both_pos = sum(rtquic_positive == 1 & elisa_positive == 1, na.rm = TRUE),
    n_rtquic_only = sum(rtquic_positive == 1 & elisa_positive == 0, na.rm = TRUE),
    n_elisa_only = sum(rtquic_positive == 0 & elisa_positive == 1, na.rm = TRUE),
    n_both_neg = sum(rtquic_positive == 0 & elisa_positive == 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Assay, Tissue, Dilutions)

# 4️⃣ View the summary
agreement_summary


agreement_summary <- agreement_summary %>%
  mutate(
    pct_agree = (n_both_pos + n_both_neg) / n_animals * 100,
    pct_both_pos = n_both_pos / n_animals * 100,
    pct_rtquic_only = n_rtquic_only / n_animals * 100,
    pct_elisa_only = n_elisa_only / n_animals * 100,
    pct_both_neg = n_both_neg / n_animals * 100
  )

agreement_summary %>%
  select(Assay, Dilutions, Tissue, n_animals, pct_agree, pct_both_pos, pct_rtquic_only, pct_elisa_only, pct_both_neg)


library(ggplot2)
library(tidyr)

# Reshape for plotting
plot_data <- agreement_summary %>%
  select(Assay, Dilutions, Tissue, n_both_pos, n_rtquic_only, n_elisa_only, n_both_neg) %>%
  pivot_longer(cols = c(n_both_pos, n_rtquic_only, n_elisa_only, n_both_neg),
               names_to = "agreement_type", values_to = "n_animals")

ggplot(plot_data, aes(x = factor(Dilutions), y = n_animals, fill = agreement_type)) +
  geom_bar(stat = "identity") +
  facet_grid(Assay ~ Tissue) +
  labs(x = "Dilution", y = "Number of animals", fill = "Agreement type",
       title = "Agreement between RT/Nano-QuIC and ELISA by tissue and dilution") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")











#1  KAPPA analysis of Necropsy Samples (Animal Level all tissues)

# Generate a RT-QUIC/NANO Binary Column
summary_data <- summary_data %>%
  mutate(sample_positive = ifelse(thres_pos == 1, 1, 0))

# Collapse to animal-level positivity by dilution and assay
animal_pos_by_dil <- summary_data %>%
  group_by(Sample.IDs, exposure_group, Assay, Dilutions) %>%
  summarise(
    rtquic_positive = as.integer(any(sample_positive == 1, na.rm = TRUE)),
    elisa_positive = first(elisa_overall),  # ELISA is per animal
    .groups = "drop"
  ) %>%
  # 3️⃣ Exclude animals without ELISA data (still alive)
  filter(!is.na(elisa_positive))

# Merge with ELISA positivity
animal_combined <- animal_pos_by_dil %>%
  left_join(
    summary_data %>%
      select(Sample.IDs, elisa_overall) %>%
      distinct(),
    by = "Sample.IDs"
  )

# Compute Kappa by dilution and assay


kappa_results <- animal_combined %>%
  group_by(Assay, Dilutions) %>%
  summarise(
    kappa_val = kappa2(cbind(rtquic_positive, elisa_overall), weight = "unweighted")$value,
    .groups = "drop"
  )

kappa_results


# Plot Results

ggplot(kappa_results, aes(x = as.numeric(Dilutions), y = kappa_val, color = Assay)) +
  geom_point(size = 3) +
  geom_line() +
  scale_x_reverse() +  # usually dilutions are negative exponents, -2, -3, -4
  labs(x = "Dilution", y = "Cohen's Kappa", title = "Agreement between RT-QuIC/Nano-QuIC and ELISA") +
  theme_bw()


# Kappa Analysis of Necropsy samples by tissue type
summary_data <- summary_data %>%
  mutate(sample_positive = ifelse(thres_pos == 1, 1, 0))


#combine multiple RPLN per animal
animal_tissue_combined <- summary_data %>%
  group_by(Sample.IDs, exposure_group, Assay, Dilutions, Tissue) %>%
  summarise(
    rtquic_positive = as.integer(any(sample_positive == 1, na.rm = TRUE)),  # combines multiple RPLN
    elisa_positive = first(elisa_overall),  # ELISA is one per animal
    .groups = "drop"
  ) %>%
  # Exclude animals without ELISA yet
  filter(!is.na(elisa_positive))

tissue_kappa <- animal_tissue_combined %>%
  group_by(Assay, Dilutions, Tissue) %>%
  summarise(
    kappa_val = kappa2(cbind(rtquic_positive, elisa_positive), weight = "unweighted")$value,
    n_animals = n(),
    .groups = "drop"
  )

tissue_kappa




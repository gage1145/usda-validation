library(tidyverse)
library(ggridges)
library(ggpubr)
library(forcats)
library(ggridges)
library(airtabler)
library(janitor)
library(psych)
library(ggbiplot)
library(car)
library(lme4)
library(lmerTest)
library(emmeans)
library(survival)
library(arrow)
library(jsonlite)


# Themes ------------------------------------------------------------------


main_theme <- theme(
  plot.title = element_text(size=24, hjust=0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=12),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)


# Load the data -----------------------------------------------------------


factors <- c("animal", "assay", "room_number", "group", "sex", "genotype", "inoculum", "mpi")

df_ <- read_parquet("data/data_dump.parquet") %>%
  select(-data) %>%
  mutate(calcs = map(calcs, fromJSON)) %>%
  unnest(calcs) %>%
  mutate(
    across(where(is.character), as.factor),
    assay = factor(assay, level=c("RT-QuIC", "Nano-QuIC")),
    mpi = as.integer(mpi),
    positive = case_when(
      mpi == 0                          ~ 0,  # confirmed negative
      group == "Inoculated" & mpi > 0   ~ 1,  # confirmed positive
      group == "Contact"    & mpi > 0   ~ NA  # unknown — model separately
    )
  )


# ANOVA -------------------------------------------------------------------


mpr_formula <- mpr ~ assay + dilution + group + sex + genotype + mpi + (1 | room/animal_id)
ms_formula  <- ms  ~ assay + dilution + group + sex + genotype + mpi + (1 | room/animal_id)
auc_formula <- auc ~ assay + dilution + group + sex + genotype + mpi + (1 | room/animal_id)

mpr_model <- lmer(mpr_formula, data = df_)
ms_model  <- lmer(ms_formula,  data = df_)
auc_model <- lmer(auc_formula, data = df_)

# Post-hoc pairwise comparisons for primary predictors
mpr_comps <- emmeans(mpr_model, pairwise ~ assay | group | dilution)
plot(mpr_comps)
emmeans(mpr_model, pairwise ~ dilution)
emmeans(mpr_model, pairwise ~ mpi)

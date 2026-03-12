library(tidyverse)
library(airtabler)
library(janitor)
library(lme4)
library(lmerTest)
library(emmeans)



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
  filter(dilution == "-3") %>%
  mutate(across(everything(), as.character)) %>%
  left_join(animals, by = "animal") %>%
  clean_names() %>%
  mutate(
    across(c(sample_id, animal), as.factor),
    assay = factor(assay, levels = c("RT-QuIC", "Nano-QuIC")),
    group = as.factor(group),
    room_number = as.factor(room_number),
    mpi   = as.integer(mpi),
    across(c(mpr, raf, ttt, ms, auc), as.numeric)
  )

df_norm <- df_ %>%
  mutate(across(c("mpr", "raf", "ttt", "ms", "auc"), scale))



# Models ------------------------------------------------------------------
auc_model <- lmer(
  auc ~ mpi * group + mpi * assay + sex + genotype +
    (1 + mpi | animal) +
    (1 | room_number),
  data = df_
)
summary(auc_model)
anova(auc_model)
plot(auc_model)

# How do the two assays differ in trajectory slope?
emtrends(auc_model, pairwise ~ assay, var = "mpi")

# How do the two groups differ in trajectory slope?
emtrends(auc_model, pairwise ~ group, var = "mpi")

# Predicted marginal means at each timepoint by assay and group
emmeans(auc_model, ~ mpi * assay * group,
        at = list(mpi = c(0, 6, 12, 24, 36, 48))) %>%
  as.data.frame() %>%
  ggplot(aes(mpi, emmean, color = assay, linetype = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = lower.CL, ymax = upper.CL, fill = assay),
              alpha = 0.15, color = NA) +
  scale_color_manual(values = c("darkslateblue", "darkorange")) +
  scale_fill_manual(values  = c("darkslateblue", "darkorange")) +
  labs(
    title    = "Predicted AUC Trajectories by Assay and Group",
    x        = "MPI",
    y        = "Predicted AUC",
    color    = "Assay",
    fill     = "Assay",
    linetype = "Group"
  ) 

# Quick visual check — is the AUC-MPI relationship roughly linear?
df_ %>%
  group_by(mpi, assay, group) %>%
  summarise(mean_auc = mean(auc, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(mpi, mean_auc, color = assay)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  facet_grid(rows=vars(group))

df_ %>%
  filter(group == "Inoculated", dilution == -3) %>%
  ggplot(aes(mpi, auc, group = animal, color = animal)) +
  geom_line(alpha = 0.6, linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(animal~assay) +
  theme(legend.position = "none") +
  labs(title = "Individual Inoculated Animal Trajectories",
       x = "MPI", y = "AUC")
#

library(JM)         # joint models for longitudinal and survival data
library(survival)

# Step 1 — build the survival component
# You need a per-animal endpoint dataset
df_survival <- df_norm %>%
  group_by(animal, group, sex, genotype, room_number) %>%
  summarise(
    last_mpi   = max(mpi, na.rm=TRUE),
    euthanized = any(!is.na(dod_collection_date)),  # adjust to match your column name
    .groups    = "drop"
  )

# Kaplan-Meier by group first — just to visualize
library(survminer)
km_fit <- survfit(Surv(last_mpi, euthanized) ~ group, data = df_survival)
# png("figures/RAMALT/survival_plot.png", width = 12, height = 8, units = "in", res = 150)
ggsurvplot(
  km_fit,
  data     = df_survival,
  pval     = TRUE,
  conf.int = TRUE,
  palette  = c("darkslateblue", "darkorange"),
  xlab     = "MPI",
  ylab     = "Survival Probability",
  title    = "Time to Endpoint by Group"
)
# dev.off()

#


























# Trajectory clustering ---------------------------------------------------
# Within Nano-QuIC only, do trajectory shapes cluster meaningfully?
run_normalized_clustering <- function(assay_name, k_range = 2:4) {
  traj_data <- df_ %>%
    filter(dilution == -3, assay == assay_name) %>%
    group_by(animal, group, room_number) %>%
    arrange(mpi) %>%
    mutate(auc_scaled = scale(auc)[,1]) %>%
    summarise(
      traj  = list(auc_scaled),
      group = first(group),
      n_obs = n(),
      .groups = "drop"
    ) %>%
    filter(n_obs >= 3)
  
  clust <- tsclust(
    traj_data$traj,
    type     = "partitional",
    k        = k_range,
    distance = "dtw_basic",
    seed     = 42
  )
  
  list(data = traj_data, clust = clust)
}

nano_clust <- run_normalized_clustering("Nano-QuIC")
rt_clust   <- run_normalized_clustering("RT-QuIC")

# Compare silhouette scores across k values
sapply(nano_clust$clust, cvi, type = "internal")["Sil",]
sapply(rt_clust$clust,   cvi, type = "internal")["Sil",]


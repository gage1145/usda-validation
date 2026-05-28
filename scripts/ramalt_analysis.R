library(tidyverse)
library(airtabler)
library(janitor)
library(lme4)
library(lmerTest)
library(emmeans)
library(scales)


main_theme <- theme(
  plot.title = element_text(size=24, hjust=0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=12),
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

animals <- tables$animals$select_all() %>%
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
clrs <- c("darkcyan", "darkorange")

unique_group_months <- df_ %>%
  filter(!is.na(mpi)) %>%
  summarize(.by = c(group, mpi)) %>%
  filter(duplicated(mpi))

dup_group_months <- unique_group_months[duplicated(unique_group_months$mpi), "mpi"]

df_group_cum <- df_ %>%
  filter(mpi %in% unique_group_months$mpi) %>%
  mutate(auc = rescale(auc, c(0, 1)), .by=assay) %>% 
  summarize(across(auc, list(mean = mean, stdev = ~ sd(.) / sqrt(length(.)))), .by = c(assay, group, mpi)) %>%
  mutate(upper = auc_mean + auc_stdev, lower = auc_mean - auc_stdev) %>%
  group_by(group, assay) %>%
  arrange(mpi, .by_group = TRUE) %>%
  mutate(across(c(auc_mean, upper, lower), cumsum)) %>%
  ungroup() 
  # mutate(alpha_val = rescale(auc_mean, c(0, 0.2)))


df_group_cum %>%
  ggplot(aes(mpi, auc_mean, color = group, linetype = assay, fill = group)) +
  geom_point(size = 2) +
  geom_line(linewidth=1.5) +
  geom_ribbon(aes(ymin=lower, ymax=upper, alpha = upper), color = NA, show.legend = FALSE) +
  scale_alpha_continuous(range = c(0, 0.3)) +
  scale_fill_manual(values = clrs) +
  scale_color_manual(values = clrs) +
  scale_x_continuous(breaks=seq(0, 63, 3)) +
  # geom_smooth(se=F) 
  coord_cartesian(xlim = c(0, 48.5), expand=FALSE) +
  main_theme +
  labs(
    y = "Normalized Cumulative Area Under the Curve",
    x = "Months Post-Inoculation"
  ) +
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.position = c(0.1, 0.8)
  )
  # facet_grid(rows=vars(assay))
ggsave("cum_auc.png", path="figures/RAMALT", width=12, height=8)


# Animal level accumulation ----------------------------------------------

unique_assay_months <- df_ %>%
  filter(!is.na(mpi)) %>%
  summarize(.by = c(assay, mpi))

dup_assay_months <- unique_assay_months[duplicated(unique_assay_months$mpi), "mpi"]

df_animal_cum <- df_ %>%
  ungroup() %>%
  filter(mpi %in% dup_assay_months) %>%
  mutate(auc = rescale(auc, c(0, 1)), .by = assay) %>% 
  summarize(across(auc, list(mean = mean, stdev = ~ sd(.) / sqrt(n()))), .by = c(assay, animal, mpi)) %>%
  mutate(upper = auc_mean + auc_stdev, lower = auc_mean - auc_stdev) %>%
  group_by(animal, assay) %>%
  arrange(mpi, .by_group = TRUE) %>%
  mutate(across(c(auc_mean, upper, lower), cumsum)) %>%
  ungroup() %>%
  mutate(across(c(auc_mean, upper, lower), ~ rescale(., c(0, 1), c(min(.data$auc_mean), max(.data$auc_mean)))), .by = assay)

df_animal_cum %>%
  ggplot(aes(mpi, auc_mean, group = assay, color = assay, fill = assay)) +
  geom_point(size = 1.5) +
  geom_line(linewidth=1) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha = 0.4, color = NA, show.legend = FALSE) +
  scale_alpha_continuous(range = c(0, 0.3)) +
  scale_fill_manual(values = clrs) +
  scale_color_manual(values = clrs) +
  scale_x_continuous(breaks=seq(0, 63, 12)) +
  scale_y_continuous(breaks=seq(0, 1, 0.25)) +
  # coord_cartesian(xlim = c(0, 48.5), expand=FALSE) +
  labs(
    y = "Normalized Cumulative Area Under the Curve",
    x = "Months Post-Inoculation"
  ) +
  facet_wrap(vars(animal)) +
  main_theme +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )
ggsave("animal_cumulative.png", path="figures/RAMALT", width=10, height=8)

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

# Trajectory clustering ---------------------------------------------------
# Within Nano-QuIC only, do trajectory shapes cluster meaningfully?
# run_normalized_clustering <- function(assay_name, k_range = 2:4) {
#   traj_data <- df_ %>%
#     filter(dilution == -3, assay == assay_name) %>%
#     group_by(animal, group, room_number) %>%
#     arrange(mpi) %>%
#     mutate(auc_scaled = scale(auc)[,1]) %>%
#     summarise(
#       traj  = list(auc_scaled),
#       group = first(group),
#       n_obs = n(),
#       .groups = "drop"
#     ) %>%
#     filter(n_obs >= 3)
  
#   clust <- tsclust(
#     traj_data$traj,
#     type     = "partitional",
#     k        = k_range,
#     distance = "dtw_basic",
#     seed     = 42
#   )
  
#   list(data = traj_data, clust = clust)
# }

# nano_clust <- run_normalized_clustering("Nano-QuIC")
# rt_clust   <- run_normalized_clustering("RT-QuIC")

# # Compare silhouette scores across k values
# sapply(nano_clust$clust, cvi, type = "internal")["Sil",]
# sapply(rt_clust$clust,   cvi, type = "internal")["Sil",]


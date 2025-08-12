library(dplyr)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(forcats)



main_theme <- theme(
  axis.title = element_text(size=20),
  axis.text = element_text(size=12),
  strip.text = element_text(size=16, face="bold"),
  legend.title = element_text(size=12),
  legend.text = element_text(size=12)
)

df_ <- read.csv("data/blood/calcs.csv", check.names = FALSE) %>%
  filter(!(`Sample IDs` %in% c("P", "N"))) %>%
  na.omit() %>%
  mutate_at("Dilutions", as.factor) %>%
  mutate(
    Assay = ifelse(Assay == "RT", "RT-QuIC", "Nano-QuIC"),
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC")),
    Dilutions = factor(Dilutions, level=c(0, -1, -2, -3, -4))
  )

df_sum <- df_ %>%
  group_by(`Sample IDs`, Dilutions, Assay) %>%
  summarize(
    median_RAF = median(RAF)
  )

results <- read.csv("data/blood/summary.csv", check.names = FALSE) %>%
  filter(!(`Sample IDs` %in% c("P", "N"))) %>%
  na.omit() %>%
  mutate_at("Dilutions", as.factor) %>%
  mutate(
    Assay = ifelse(Assay == "RT", "RT-QuIC", "Nano-QuIC"),
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC")),
    Dilutions = factor(Dilutions, level=c(0, -1, -2, -3, -4))
  )

# Tissue Boxplot
df_ %>%
  rename("Preparation" = "Sample IDs") %>%
  # arrange(desc(RAF)) %>%
  ggplot(aes(`Dilutions`, RAF, fill = Preparation)) +
  # geom_line(aes(y=median_RAF, color=Assay, group=Assay), data=df_sum, linewidth=1) +
  geom_boxplot(outliers = FALSE) +
  facet_grid(cols=vars(Assay), space = "free", scales="free") +
  # scale_color_manual(values=c("cyan3", "orangered1")) +
  # scale_fill_manual(values=c("cyan3", "orangered1")) +
  labs(
    x="Log Dilution Factor",
    y="Rate of Amyloid Formation (1/s)"
  ) +
  main_theme +
  theme(
    # axis.title.x = element_blank(),
    # axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
    legend.position = "right",
    legend.title = element_text(hjust=0.5, face="bold")
  )
ggsave("RAFs.png", path="figures/blood", width=16, height=8)
  
# Tissue Histograms
# df_ %>%
#   # filter(TtT != 96) %>%
#   ggplot(aes(MPR, `Sample IDs`, fill = Assay)) +
#   geom_density_ridges(scale=4, rel_min_height=0.001, panel_scaling=FALSE, alpha=0.6) +
#   facet_grid(vars(Dilutions), scale = "free") +
#   # scale_color_manual(values=c("cyan3", "orangered1")) +
#   # scale_fill_manual(values=c("cyan3", "orangered1")) +
#   # scale_x_continuous(breaks=seq(0,0.2,0.02), expand=c(0,0)) +
#   scale_y_discrete(expand=c(0.2, 0)) +
#   labs(
#     # x="Rate of Amyloid Formation (1/h)",
#     # y="Log Dilution Factors"
#   ) +
#   main_theme +
#   theme(
#     axis.text.y = element_text(size=16),
#     legend.position.inside = TRUE,
#     legend.position = c(0.8, 0.93),
#     legend.direction = "horizontal",
#     legend.title = element_blank(),
#     legend.background = element_blank()
#   )
# ggsave("histograms.png", path="figures/blood", width=12, height=8)



# # Logistic Curve
# results %>%
#   mutate_at("thres_pos", as.integer) %>%
#   ggplot(aes(mean_MPR, thres_pos, color=Dilutions, linetype = Assay)) +
#   geom_smooth(
#     method="glm",
#     method.args = list(family = "binomial"), 
#     se = FALSE
#   )





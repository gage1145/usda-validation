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

df_ <- read.csv("data/calcs.csv", check.names = FALSE) %>%
  na.omit() %>%
  mutate_at("Dilutions", as.factor) %>%
  mutate(Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC")))

df_sum <- df_ %>%
  group_by(`Sample IDs`, Dilutions, Assay, Tissue) %>%
  summarize(
    median_RAF = median(RAF)
  )

df_ %>%
  arrange(desc(RAF)) %>%
  ggplot(aes(fct_inorder(`Sample IDs`), RAF, fill = Assay)) +
  geom_line(aes(y=median_RAF, color=Assay, group=Assay), data=df_sum, linewidth=1) +
  geom_boxplot(outliers = FALSE) +
  facet_grid(vars(Dilutions), vars(Tissue), space = "free") +
  scale_color_manual(values=c("cyan3", "orangered1")) +
  scale_fill_manual(values=c("cyan3", "orangered1")) +
  labs(
    y="Rate of Amyloid Formation (1/s)"
  ) +
  main_theme +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle=45, hjust=1, vjust=1),
    legend.position = "bottom",
    legend.title = element_blank()
  )
ggsave("RAFs.png", path="figures", width=12, height=8)
  
df_ %>%
  filter(TtT != 72) %>%
  ggplot(aes(RAF, fct_rev(Dilutions), fill = Assay)) +
  geom_density_ridges(scale=4, rel_min_height=0.001, panel_scaling=FALSE, alpha=0.6) +
  facet_grid(vars(Tissue)) +
  scale_color_manual(values=c("cyan3", "orangered1")) +
  scale_fill_manual(values=c("cyan3", "orangered1")) +
  scale_x_continuous(breaks=seq(0,0.2,0.02), expand=c(0,0)) +
  scale_y_discrete(expand=c(0.2, 0)) +
  labs(
    x="Rate of Amyloid Formation (1/h)",
    y="Log Dilution Factors"
  ) +
  main_theme +
  theme(
    axis.text.y = element_text(size=16),
    legend.position.inside = TRUE,
    legend.position = c(0.8, 0.93),
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.background = element_blank()
  )
ggsave("histograms.png", path="figures", width=12, height=8)

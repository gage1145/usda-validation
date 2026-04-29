library(tidyverse)
library(pROC)
library(arrow)
library(cli)


threshold <- 5
norm_point <- 8

main_theme <- theme(
  axis.title       = element_text(size=16),
  axis.text        = element_text(size=12, color="black"),
  strip.text       = element_text(size=16, face="bold"),
  legend.title     = element_text(size=12, color="black"),
  legend.text      = element_text(size=12),
  plot.title       = element_text(hjust=0.5, vjust=2, size=20, face="bold"),
)



# Load the data -----------------------------------------------------------



df_ <- read_parquet("data/blood/calcs.parquet") %>%
  mutate(`Sample IDs` = (`Sample IDs` == "pos")) %>%
  filter(!(`Sample IDs` %in% c("P", "N"))) %>%
  rename(response=`Sample IDs`) %>%
  na.omit()

treatments = unique(df_$Treatment)
assays = unique(df_$Assay)
dilutions = unique(df_$Dilutions)
sub_concs = unique(df_$Substrate_conc)

roc_list <- list()
names_list <- c()

for (sub_conc in sub_concs) {
  for (treatment in treatments) {
    for (dilution in dilutions) {
      for(assay in assays) {
        print(
          sprintf(
            "Analyzing: %s, %s, %s, %s", treatment, assay, dilution, sub_conc 
          )
        )
        
        roc_name <- paste(treatment, assay, dilution, sub_conc, sep="_")
        
        sub_df <- df_ %>%
          filter(
            Treatment == treatment, 
            Dilutions == dilution, 
            Assay == assay, 
            Substrate_conc == sub_conc
          )
        
        if (nrow(sub_df) == 0) { 
          cli_alert_danger(sprintf("Subset %s had no data.", roc_name))
          next
        }
        
        if (length(unique(sub_df$response)) != 2) {
          cli_alert_danger(sprintf("Subset %s didn't have matching responses.", roc_name))
          next
        }
        
        # print(sub_df)
        
        sub_roc <- roc(sub_df, response, MPR, direction="<", ci=TRUE)
        roc_list <- append(roc_list, list(sub_roc))
        names_list <- c(names_list, roc_name)
      }
    }
  }
}

names(roc_list) <- names_list
aucs <- stack(sapply(roc_list, function(x) x$auc)) %>%
  separate(ind, c("treatment", "assay", "dilution", "sub_conc"), "_", remove=FALSE)

thresholds <- roc_list %>%
  sapply(function(x) x$thresholds) %>%
  stack() %>%
  filter(
    values >= threshold,
    !is.infinite(values)
  )

cis <- roc_list %>%
  sapply(function(x) x$ci)

good_rocs <- unique(thresholds$ind)

# ggroc(roc_list)

auc_plot <- aucs %>%
  filter(ind %in% good_rocs) %>%
  arrange(desc(values)) %>%
  ggplot(aes(fct_inorder(ind), values, fill=assay)) +
  geom_col() +
  geom_text(aes(label=ind, y=values+0.01), angle=90, hjust=0, vjust=0.5) +
  scale_y_continuous(limits=c(0, 1.1)) +
  scale_fill_manual(values=c("red", "darkcyan")) +
  labs(
    y = "Area Under ROC Curve",
    x = " "
  ) +
  main_theme +
  theme(
    axis.text.x = element_blank(),
    # axis.ticks.x = element_blank(),
    # axis.text.x = element_text(hjust=1, vjust=1, angle=45),
    # axis.title.x = element_blank(),
    legend.position = c(0.9, 0.9),
    legend.background = element_blank(),
    legend.title = element_blank()
  )
auc_plot
ggsave("blood_auc.png", path="figures/blood", width=12, height=8)



# Other Plots -------------------------------------------------------------



# df_ %>%
#   filter(Treatment %in% c("A", "B")) %>%
#   mutate(
#     Dilutions = as.factor(Dilutions),
#     response = ifelse(response, "Pos", "Neg")  
#   ) %>%
#   ggplot(aes(Dilutions, MPR, fill=response)) + 
#   geom_boxplot() + 
#   facet_grid(vars(Assay), vars(Treatment, Substrate_conc)) +
#   # scale_y_log10() +
#   scale_fill_manual(values=c("darkcyan", "darkorange")) +
#   main_theme +
#   theme(
#     legend.title = element_blank(),
#     legend.position = "bottom"
#   )
# ggsave("mpr_boxplot.png", path="figures/blood", width=12, height=8)

raf_box_plot <- df_ %>%
  filter(Treatment %in% c("A", "B")) %>%
  mutate(
    Treatment = factor(Treatment, levels=c("A", "B"), labels = c("Treatment A", "Treatment B")),
    Substrate_conc = factor(Substrate_conc, levels=c("1X", "2X"), labels = c("Substrate 1X", "Substrate 2X")),
    Dilutions = as.factor(Dilutions),
    response = ifelse(response, "Pos", "Neg")  
  ) %>%
  ggplot(aes(Dilutions, RAF, fill=response)) + 
  geom_boxplot() + 
  facet_grid(vars(Assay), vars(Treatment, Substrate_conc), scales="free_x", space="free") +
  # scale_y_log10() +
  scale_fill_manual(values=c("darkcyan", "red")) +
  labs(
    y="Rate of Amyloid Formation (1/h)"
    # title="Rates of Amyloid Formation"
  ) +
  main_theme +
  theme(
    legend.title = element_blank(),
    legend.direction = "vertical"
  )
raf_box_plot
ggsave("raf_boxplot.png", path="figures/blood", width=10, height=12)

ggarrange(auc_plot, raf_box_plot, ncol=2, legend="none", align = "h")
ggsave("raf_auc_combo.png", path="figures/blood", width=16, height=11)

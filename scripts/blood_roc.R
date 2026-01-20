library(tidyverse)
library(pROC)


readRenviron(".Renviron")
threshold <- as.numeric(Sys.getenv("THRESHOLD"))

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
          # cli_alert_danger(sprintf("Subset %s had no data.", roc_name))
          next
        }
        
        if (length(unique(sub_df$response)) != 2) {
          # cli_alert_danger(sprintf("Subset %s didn't have matching responses.", roc_name))
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
aucs <- stack(sapply(roc_list, function(x) x$auc)) 

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

aucs %>%
  filter(ind %in% good_rocs) %>%
  arrange(desc(values)) %>%
  ggplot(aes(fct_inorder(ind), values)) +
  geom_col() +
  labs(
    title = "Areas Under the Curve of Blood ROC Analysis",
    y = "AUC"
  ) +
  main_theme +
  theme(
    axis.text.x = element_text(hjust=1, vjust=1, angle=45),
    axis.title.x = element_blank()
  )
ggsave("blood_auc.png", path="figures/blood", width=12, height=8)



# Other Plots -------------------------------------------------------------



df_ %>%
  filter(Treatment %in% c("A", "B")) %>%
  mutate(
    Dilutions = as.factor(Dilutions),
    response = ifelse(response, "Pos", "Neg")  
  ) %>%
  ggplot(aes(Dilutions, MPR, fill=response)) + 
  geom_boxplot() + 
  facet_grid(vars(Assay), vars(Treatment, Substrate_conc)) +
  scale_y_log10() +
  scale_fill_manual(values=c("darkcyan", "red")) +
  main_theme +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  )
ggsave("boxplot.png", path="figures/blood", width=12, height=8)


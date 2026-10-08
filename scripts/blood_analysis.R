library(quicR)
library(tidyverse)
library(cli)
library(ggridges)
library(ggpubr)
library(forcats)
library(pROC)
library(arrow)


main_theme <- theme(
  axis.title       = element_text(size=16),
  axis.text        = element_text(size=12, color="black"),
  strip.text       = element_text(size=16, face="bold"),
  legend.title     = element_text(size=12, color="black"),
  legend.text      = element_text(size=12),
  plot.title       = element_text(hjust=0.5, vjust=2, size=20, face="bold"),
)

threshold <- 5
norm_point <- 8


files <- list.files("raw/blood", ".xlsx", full.names = TRUE, recursive = TRUE)

get_raw <- function(file) {
  file_list <- str_split(file, "/")
  substrate_conc <- if ("2X" %in% file_list[[1]]) "2X" else "1X"
  split_count <- length(str_split(file, "_")[[1]])
  assay <- str_split_i(file, "_", split_count) %>%
    str_remove(".xlsx")
  reaction <- str_split_i(file, "/", length(file_list[[1]])) %>%
    str_remove(".xlsx")
  
  cli_alert_info(sprintf(" Reading file: %s", reaction))
  get_quic(file, norm_point=norm_point) %>%
    as.data.frame() %>%
    mutate(
      dilution = -log10(as.numeric(dilution)),
      assay = assay,
      reaction = reaction,
      substrate_conc = substrate_conc
    ) %>%
    separate_wider_delim(
      sample, 
      "_", 
      names=c("treatment", "sample"),
      too_few="align_end"
    ) %>%
    suppressMessages() %>%
    suppressWarnings()
}

df_ <- map_dfr(files, get_raw) 

calcs <- calculate_metrics(
  df_,
  "sample", "well", "treatment", "dilution", "assay", "reaction", "substrate_conc", 
  threshold=threshold
)

write_parquet(df_, "data/blood/raw.parquet")
write_parquet(calcs, "data/blood/calcs.parquet")


# Figures ----------------------------------------------------------------


df_fig <- calcs %>%
  filter(!(sample %in% c("P", "N"))) %>%
  # na.omit() %>%
  mutate(
    treatment = factor(treatment, levels=sort(unique(treatment))),
    assay = factor(assay, levels=unique(assay)),
    dilution = factor(dilution, sort(unique(dilution)))
  )


# Blood Boxplot ----------------------------------------------------------



box_theme <- function(p) {
  p %>%
    filter(dilution %in% c(-2, -3)) %>%
    mutate(treatment = paste("treatment", treatment)) %>%
    ggplot(aes(dilution, raf, fill = sample)) +
    geom_boxplot(outliers=FALSE, linewidth=0.25) +
    facet_grid(rows=vars(assay), space = "free", scales="fixed") +
    scale_fill_manual(values=c("darkslateblue", "red")) +
    scale_y_continuous(limits=c(min(p$raf), 0.034)) +
    labs(
      x="Log Dilution Factor",
      y="Rate of Amyloid Formation (1/s)"
    )
}

boxes_A <- df_fig %>%
  filter(treatment == "A", dilution == -3) %>%
  box_theme() +
  ggtitle("treatment A") +
  main_theme +
  theme(
    legend.position = "right",
    legend.title = element_text(hjust=0.5, face="bold"),
    axis.title.x=element_blank(),
  )

boxes_B <- df_fig %>%
  filter(treatment == "B", dilution == -2) %>%
  box_theme() +
  ggtitle("treatment B") +
  main_theme +
  theme(
    legend.position = "right",
    legend.title = element_text(hjust=0.5, face="bold"),
  )

boxes_combined <- ggarrange(
  boxes_A, boxes_B,
  ncol=1,
  align="v",
  legend="none"
)
boxes_combined



# Real-time graphs --------------------------------------------------------



rt_theme <- function(p) {
  p %>%
    mutate(
      assay = factor(assay, levels=c("RT-QuIC", "Nano-QuIC")),
      group = paste(well, reaction, sep="_")
    ) %>%
    ggplot(aes(time, rfu, color=sample, group=group)) +
    geom_line(linewidth=1) +
    facet_grid(rows=vars(assay)) +
    scale_color_manual(values=c("darkslateblue", "red")) +
    scale_x_continuous(limits=c(0,96), breaks=seq(0,96,12), expand=expansion()) +
    scale_y_continuous(limits=c(0,15000)) +
    labs(
      y="RFU",
      x="Time (h)",
      color="Sample"
    )
}

rt_A <- df_ %>%
  filter(
    treatment == "A",
    dilution == -3
  ) %>% 
  rt_theme() +
  ggtitle("treatment A") +
  main_theme +
  theme(
    legend.position = "none",
    legend.title = element_text(hjust=0.5, face="bold"),
    axis.title.x=element_blank(),
  )

rt_B <- df_ %>%
  filter(
    treatment == "B",
    dilution == -2
  ) %>% 
  rt_theme() +
  ggtitle("treatment B") +
  main_theme +
  theme(
    legend.position = "none",
    legend.title = element_text(hjust=0.5, face="bold"),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1)
  )

rt_combined <- ggarrange(
  rt_A, rt_B,
  ncol=1, 
  align="v",
  common.legend = TRUE,
  legend="right"
)
rt_combined



# Combined ----------------------------------------------------------------



ggarrange(
  boxes_combined, rt_combined,
  widths=c(1,2.5),
  ncol=2,
  align="v",
  common.legend = TRUE
) +
  theme(
    plot.background = element_rect(fill="white")
  )
ggsave(
  "combined.png", 
  path="figures/blood", 
  width=16, 
  height=10
)


# ROC Analysis -----------------------------------------------------------


df_roc <- calcs %>%
  filter(!(sample %in% c("P", "N"))) %>%
  mutate(response = sample == "pos") %>%
  na.omit()

treatments = unique(df_roc$treatment)
assays = unique(df_roc$assay)
dilutions = unique(df_roc$dilution)
sub_concs = unique(df_roc$substrate_conc)

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
        
        sub_df <- df_roc %>%
          filter(
            treatment      == .env$treatment, 
            dilution       == .env$dilution, 
            assay          == .env$assay, 
            substrate_conc == .env$sub_conc
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
        
        sub_roc <- roc(sub_df, response, mpr, direction="<", ci=TRUE)
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

raf_box_plot <- df_roc %>%
  filter(treatment %in% c("A", "B")) %>%
  mutate(
    treatment = factor(treatment, levels=c("A", "B"), labels = c("treatment A", "treatment B")),
    substrate_conc = factor(substrate_conc, levels=c("1X", "2X"), labels = c("Substrate 1X", "Substrate 2X")),
    dilution = as.factor(dilution),
    response = ifelse(response, "Pos", "Neg")  
  ) %>%
  ggplot(aes(dilution, raf, fill=response)) + 
  geom_boxplot() + 
  facet_grid(vars(assay), vars(treatment, substrate_conc), scales="free_x", space="free") +
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

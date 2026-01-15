library(tidyverse)
library(pROC)



# Load the data -----------------------------------------------------------



df_ <- read_parquet("data/blood/calcs.parquet") %>%
  filter(!(`Sample IDs` %in% c("P", "N"))) %>%
  na.omit() %>%
  mutate(
    Treatment = factor(Treatment, levels=c("A", "B", "C", "D", "E")),
    Assay = factor(Assay, level=c("RT-QuIC", "Nano-QuIC")),
    Dilutions = factor(Dilutions, level=c(0, -1, -2, -3, -4))
  )



# Treatment A -------------------------------------------------------------



# Treatment A, -3, RT-QuIC
a_rt <- df_ %>%
  mutate(`Sample IDs` = as.factor(ifelse(`Sample IDs` == "pos", 1, 0))) %>%
  rename(response=`Sample IDs`) %>%
  filter(Treatment == "A", Dilutions == -3, Assay == "RT-QuIC") %>%
  roc(response, MS)

# Treatment A, -3, Nano-QuIC
a_nano <- df_ %>%
  mutate(`Sample IDs` = as.factor(ifelse(`Sample IDs` == "pos", 1, 0))) %>%
  rename(response=`Sample IDs`) %>%
  filter(Treatment == "A", Dilutions == -3, Assay == "Nano-QuIC") %>%
  roc(response, MS)

# Treatment A, -3, RT-QuIC
b_rt <- df_ %>%
  mutate(`Sample IDs` = as.factor(ifelse(`Sample IDs` == "pos", 1, 0))) %>%
  rename(response=`Sample IDs`) %>%
  filter(Treatment == "B", Dilutions == -2, Assay == "RT-QuIC") %>%
  roc(response, MS)

# Treatment A, -3, Nano-QuIC
b_nano <- df_ %>%
  mutate(`Sample IDs` = as.factor(ifelse(`Sample IDs` == "pos", 1, 0))) %>%
  rename(response=`Sample IDs`) %>%
  filter(Treatment == "B", Dilutions == -2, Assay == "Nano-QuIC") %>%
  roc(response, MS)

ggroc(
  list(a_rt=a_rt, a_nano=a_nano, b_rt=b_rt, b_nano=b_nano),
  linewidth=1
) +
  geom_abline(intercept = 1, slope = 1, linetype = "dashed", color = "red") + # Add reference line
  labs(title = "ROC Curve", x = "Specificity", y = "Sensitivity") +
  theme_minimal()

df_ %>%
  mutate(`Sample IDs` = ifelse(`Sample IDs` == "pos", 1, 0)) %>%
  rename(response=`Sample IDs`) %>%
  filter(Treatment %in% c("A", "B")) %>%
  filter(Dilutions %in% c(-2, -3)) %>%
  ggplot(aes(MPR, response, color=Treatment)) +
  geom_point() +
  geom_smooth(method="glm", method.args = list(family = "binomial"), se=FALSE) +
  facet_grid(cols=vars(Dilutions), rows=vars(Assay)) +
  scale_x_continuous(limits=c(1,15))






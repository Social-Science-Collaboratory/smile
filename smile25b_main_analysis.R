# Preliminary data analysis

# Load libraries
library(tidyverse)
library(lmerTest)
library(emmeans)
library(BayesFactor)

# Source functions used in the Bayesian analyses and figure generation
source("smile25b_functions.R")

# Read processed data set
df <- read_csv("data/smile25b_processed_data.csv")

# Descriptive statistics

## Calculate age mean and standard deviation
print(paste0("Mean age: ", round(mean(df$Age, na.rm=TRUE),2), "; sd: ", round(sd(df$Age, na.rm=TRUE),2)))

## Calculate percentage of female participants
print(paste0("Percentage female: ",
  round(
    nrow(df %>% filter(Gender == "Female")) / nrow(df) * 100,
     2),
  "%"
  )
)

## Ethinicity distribution
table(df$Ethnicity)

# Prepare dataframe for data analysis
df_long <- df %>%
  ## create a unique participant id
  mutate(id = row_number()) %>%
  select(
    id, threat, context, repetition,
    SP_DEQ_happy_total, SP_DEQ_fear_total, SP_DEQ_anger_total,
    SP_SWL_total, SP_Burnout_total,
    NP_DEQ_happy_total, NP_DEQ_fear_total, NP_DEQ_anger_total,
    NP_SWL_total, NP_Burnout_total ) %>%
  ## Pivot smile and natural pose columns into long format
  pivot_longer(
    cols = c(starts_with("SP_"), starts_with("NP_")),
    names_to = c("pose", ".value"),
    names_pattern = "^(SP|NP)_(.*)$"
  ) %>%
  ## recode pose variable into readable labels
  mutate(pose = case_when(
    pose == "SP" ~ "smile",
    pose == "NP" ~ "natural",
    TRUE ~ pose)
  ) %>%
  relocate(pose, .after = id)

# Main analysis: Outcome = happiness, dataset = full

# Full factorial general linear mixed model of happiness predicted by pose, context, threat, and repetition
happy_model <- lmer(
  DEQ_happy_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(happy_model))

# Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
happy_emm <- emmeans(happy_model, ~ pose | context + threat + repetition)

summary(happy_emm)

# Compute Bayes Factor anova for the DEQ Happiness scores with the custom function 'compute_bf_anova'
happy_bf_anova <- compute_bf_anova(
  df_long = df_long, 
  outcome = "DEQ_happy_total")

# Save and optionally reload Bayesian ANOVA results
saveRDS(happy_bf_anova, "data/main_analysis/smile25b_happy_bf_anova.Rds")
happy_bf_anova <- readRDS("data/main_analysis/smile25b_happy_bf_anova.Rds")

# Extract Bayes Factor ANOVA estimates with the custom function 'extract_bf_anova'
happy_bf_anova_table <- extract_bf_anova(
  outcome_bf_anova = happy_bf_anova)

print(happy_bf_anova_table)

# Highlight the pre-registered two-way interactions
happy_bf_anova_table %>%
  filter(term_dropped %in% c("pose:context", "pose:repetition", "pose:threat")) %>%
  print()

# Compute the Bayes Factor simple effects for the dumbbell plot figure with the custom function 'compute_bf_simple_effects'
happy_simple_effects <- compute_bf_simple_effects(
  df_wide = df, 
  outcome = "DEQ_happy_total")

print(happy_simple_effects)

# Prepare the dataframe for the dumbbell plot with the custom function 'prepare_plot_data'
happy_plot_data <- prepare_plot_data(
  outcome_emm = happy_emm,
  outcome_simple_effects = happy_simple_effects
)

# Draw the dumbbell plot using the happiness score plot data with the custom function 'draw_dumbbell_plot'
happy_dumbbell_plot <- draw_dumbbell_plot(
  plot_data = happy_plot_data, 
  outcome_label = "Happiness",
  color_natural = "#316ac6",
  color_smile = "#f5ad06"
)
  
ggsave(
  "figures/smile25b_happy_dumbbell_plot.png",
  plot = happy_dumbbell_plot,
  width = 15, height = 8, dpi = 300
  )

# Sensitivity analysis: Outcome = happiness, dataset = compliant responses only

# Compliance check based on AU12 activation difference (delta = 1.5 out of 5) in smile vs natual condition via OpenFace
table(df$face_compliance_scalar)

# Check the distribution of math errors
table(df$math_errors)

# Check participants who were aware of the true purpose of the study
table(df$joint_awareness_check)

# Prepare data for sensitivity analysis
df_sens_wide <- df %>%
  filter(
    # Filter participants who complied with facial expression instructions
    face_compliance_scalar == TRUE,
    # With no math errors
    math_errors == 0,
    # Who were not aware of the study's true purpose
    joint_awareness_check == FALSE
  )

# Convert to long format
df_sens_long <- df_sens_wide %>%
  ## create a unique participant id
  mutate(id = row_number()) %>%
  select(
    id, threat, context, repetition,
    SP_DEQ_happy_total, SP_DEQ_fear_total) %>%
  ## Pivot smile and natural pose columns into long format
  pivot_longer(
    cols = c(starts_with("SP_"), starts_with("NP_")),
    names_to = c("pose", ".value"),
    names_pattern = "^(SP|NP)_(.*)$"
  ) %>%
  ## recode pose variable into readable labels
  mutate(pose = case_when(
    pose == "SP" ~ "smile",
    pose == "NP" ~ "natural",
    TRUE ~ pose)
  ) %>%
  relocate(pose, .after = id)

# Full factorial general linear mixed model of happiness predicted by pose, context, threat, and repetition
happy_sens_model <- lmer(
  DEQ_happy_total ~ pose * context * threat * repetition + (1 | id),
  data = df_sens_long
)

# Save plot to figures folder
print(summary(happy_sens_model))

# Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
happy_sens_emm <- emmeans(happy_sens_model, ~ pose | context + threat + repetition)

summary(happy_sens_emm)

# Compute Bayes Factor anova for the DEQ Happiness scores with the custom function 'compute_bf_anova'
happy_sens_bf_anova <- compute_bf_anova(df_sens_long, "DEQ_happy_total")

# Save and optionally reload Bayesian ANOVA results
saveRDS(happy_sens_bf_anova, "data/main_analysis/smile25b_happy_sens_bf_anova.Rds")
happy_sens_bf_anova <- readRDS("data/main_analysis/smile25b_happy_sens_bf_anova.Rds")

# Extract Bayes Factor ANOVA estimates with the custom function 'extract_bf_anova'
happy_sens_bf_anova_table <- extract_bf_anova(happy_sens_bf_anova)

print(happy_sens_bf_anova_table)

# Highlight the pre-registered two-way interactions
happy_sens_bf_anova_table %>%
  filter(term_dropped %in% c("pose:context", "pose:repetition", "pose:threat")) %>%
  print()

# Compute the Bayes Factor simple effects for the dumbbell plot figure with the custom function 'compute_bf_simple_effects'
happy_sens_simple_effects <- compute_bf_simple_effects(df, "DEQ_happy_total")

print(happy_sens_simple_effects)

# Prepare the dataframe for the dumbbell plot with the custom function 'prepare_plot_data'
happy_sens_plot_data <- prepare_plot_data(
  outcome_emm = happy_sens_emm,
  outcome_simple_effects = happy_sens_simple_effects
)

# Draw the dumbbell plot with the custom function 'draw_dumbell_plot'
happy_sens_dumbbell_plot <- draw_dumbbell_plot(
  plot_data = happy_sens_plot_data, 
  outcome_label = "Happiness",
  color_natural = "#316ac6",
  color_smile = "#f5ad06"
)

# Save plot to figures folder
ggsave(
  "figures/smile25b_happy_sens_dumbbell_plot.png",
  plot = happy_dumbbell_plot,
  width = 15, height = 8, dpi = 300
  )


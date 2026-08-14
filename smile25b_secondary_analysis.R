# Load libraries
library(tidyverse)
library(lmerTest)
library(emmeans)
library(BayesFactor)
library(ggh4x)
library(see) #see:geom_violinhalf()
library(cowplot)

# Source functions used in the Bayesian analyses and figure generation
source("smile25b_functions.R")

# Create secondary analysis folder (if it doesn't exist)
if (!dir.exists("data/secondary_analysis")) {
  dir.create("data/secondary_analysis")
}

# read processed data set
df <- read_csv("data/smile25b_processed_data.csv")

# Prepare dataframe for data analysis
df_long <- df %>%
  # create a unique participant id
  mutate(id = row_number()) %>%   
  select(
    id, threat, context, repetition,
    SP_DEQ_happy_total, SP_DEQ_fear_total, SP_DEQ_anger_total,
    SP_SWL_total, SP_Burnout_total,
    NP_DEQ_happy_total, NP_DEQ_fear_total, NP_DEQ_anger_total,
    NP_SWL_total, NP_Burnout_total,
    face_compliance_scalar, face_compliance_binary, math_errors
  ) %>%
  # Pivot smile and natural pose columns into long format
  pivot_longer(
    cols = c(starts_with("SP_"), starts_with("NP_")),
    names_to = c("pose", ".value"),       
    names_pattern = "^(SP|NP)_(.*)$"
  ) %>%
  # recode pose variable into readable labels
  mutate(pose = case_when(                
    pose == "SP" ~ "smile",
    pose == "NP" ~ "natural",
    TRUE ~ pose)
  ) %>%
  relocate(pose, .after = id)

# Secondary analyses

# Satisfaction with life predicted by pose, context, threat, and repetition
SWL_model <- lmer(
  SWL_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(SWL_model))

# Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
SWL_emm <- emmeans(SWL_model, ~ pose | context + threat + repetition)

summary(SWL_emm)

# Draw the SWL half-violin plot with the custom function 'draw_violin_plot'
SWL_violin_plot <- draw_violin_plot(
  df_wide = df, 
  outcome = "SWL_total",
  outcome_label = "satisfaction with life",
  legend_position = "none",
  x_axis = FALSE,
  y_axis = FALSE
)

print(SWL_violin_plot)

# SWL Bayesian analysis

print(SWL_start_time <- Sys.time())

# Compute Bayes Factor anova for the SWL scores using the custom function 'compute_bf_anova'
SWL_bf_anova <- compute_bf_anova(df_long, "SWL_total")

print(SWL_execution_time <- Sys.time() - SWL_start_time)

# Save and optionally reload Bayesian ANOVA results
saveRDS(SWL_bf_anova, "data/secondary_analysis/smile25b_SWL_bf_anova.Rds")
SWL_bf_anova <- readRDS("data/secondary_analysis/smile25b_SWL_bf_anova.Rds")

# Extract Bayes Factor ANOVA estimates with the custom function 'extract_bf_anova'
SWL_bf_anova_table <- extract_bf_anova(SWL_bf_anova)

print(SWL_bf_anova_table)

# Highlight the pre-registered two-way interactions
SWL_bf_anova_table %>%
  filter(term_dropped %in% c("pose:context", "pose:repetition", "pose:threat")) %>%
  print()

# Compute the Bayes Factor simple effects for the dumbbell plot figure with the custom function 'compute_bf_simple_effects'
SWL_simple_effects <- compute_bf_simple_effects(df, "SWL_total")

print(SWL_simple_effects)

# Burnout predicted by pose, context, threat, and repetition
Burnout_model <- lmer(
  Burnout_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(Burnout_model)) 

# Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
Burnout_emm <- emmeans(Burnout_model, ~ pose | context + threat + repetition)

summary(Burnout_emm)

# Draw the Burnout half-violin plot with the custom function 'draw_violin_plot'
Burnout_violin_plot <- draw_violin_plot(
  df_wide = df, 
  outcome = "Burnout_total",
  outcome_label = "burnout",
  legend_position = "top_right",
  x_axis = FALSE,
  y_axis = FALSE
)

print(Burnout_violin_plot)

# Burnout Bayesian analysis

print(Burnout_start_time <- Sys.time())

# Compute Bayes Factor anova for the Burnout scores using the custom function 'compute_bf_anova'
Burnout_bf_anova <- compute_bf_anova(df_long, "Burnout_total")

print(Burnout_execution_time <- Sys.time() - Burnout_start_time)

# Save and optionally reload Bayesian ANOVA results
saveRDS(Burnout_bf_anova, "data/secondary_analysis/smile25b_Burnout_bf_anova.Rds")
Burnout_bf_anova <- readRDS("data/secondary_analysis/smile25b_Burnout_bf_anova.Rds")

# Extract Bayes Factor ANOVA estimates with the custom function 'extract_bf_anova'
Burnout_bf_anova_table <- extract_bf_anova(Burnout_bf_anova)

print(Burnout_bf_anova_table)

# Highlight the pre-registered two-way interactions
Burnout_bf_anova_table %>%
  filter(term_dropped %in% c("pose:context", "pose:repetition", "pose:threat")) %>%
  print()

# Compute the Bayes Factor simple effects for the dumbbell plot figure with the custom function 'compute_bf_simple_effects'
Burnout_simple_effects <- compute_bf_simple_effects(df, "Burnout_total")

print(Burnout_simple_effects)

# Exploratory analysis: Outcome = fear, dataset = full

# Full factorial general linear mixed model of fear predicted by pose, context, threat, and repetition
fear_model <- lmer(
  DEQ_fear_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(fear_model))

# Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
fear_emm <- emmeans(fear_model, ~ pose | context + threat + repetition)

summary(fear_emm)

# Draw the Fear half-violin plot with the custom function 'draw_violin_plot'
fear_violin_plot <- draw_violin_plot(
  df_wide = df, 
  outcome = "DEQ_fear_total",
  outcome_label = "fear",
  legend_position = "none",
  x_axis = TRUE,
  y_axis = FALSE
)

print(fear_violin_plot)

# Fear Bayesian analysis

print(fear_start_time <- Sys.time())

# Compute Bayes Factor anova for the Fear scores using the custom function 'compute_bf_anova'
fear_bf_anova <- compute_bf_anova(df_long, "DEQ_fear_total")

print(fear_execution_time <- Sys.time() - fear_start_time)

# Save and optionally reload Bayesian ANOVA results
saveRDS(fear_bf_anova, "data/secondary_analysis/smile25b_fear_bf_anova.Rds")
fear_bf_anova <- readRDS("data/secondary_analysis/smile25b_fear_bf_anova.Rds")

# Extract Bayes Factor ANOVA estimates with the custom function 'extract_bf_anova'
fear_bf_anova_table <- extract_bf_anova(fear_bf_anova)

print(fear_bf_anova_table)

# Compute the Bayes Factor simple effects for the dumbbell plot figure with the custom function 'compute_bf_simple_effects'
fear_simple_effects <- compute_bf_simple_effects(
  df_wide = df, 
  outcome = "DEQ_fear_total")

print(fear_simple_effects)

# Exploratory analysis: Outcome = anger, dataset = full

# Full factorial general linear mixed model of anger predicted by pose, context, threat, and repetition
anger_model <- lmer(
  DEQ_anger_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(anger_model))

# Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
anger_emm <- emmeans(anger_model, ~ pose | context + threat + repetition)

summary(anger_emm)

# Draw the Anger half-violin plot with the custom function 'draw_violin_plot'
anger_violin_plot <- draw_violin_plot(
  df_wide = df, 
  outcome = "DEQ_anger_total",
  outcome_label = "anger",
  legend_position = "none",
  x_axis = TRUE,
  y_axis = FALSE
)

print(anger_violin_plot)

# Anger Bayesian analysis

print(anger_start_time <- Sys.time())

# Compute Bayes Factor anova for the Fear scores using the custom function 'compute_bf_anova'
anger_bf_anova <- compute_bf_anova(df_long, "DEQ_anger_total")

print(anger_execution_time <- Sys.time() - anger_start_time)

# Save and optionally reload Bayesian ANOVA results
saveRDS(anger_bf_anova, "data/secondary_analysis/smile25b_anger_bf_anova.Rds")
anger_bf_anova <- readRDS("data/secondary_analysis/smile25b_anger_bf_anova.Rds")

# Extract Bayes Factor ANOVA estimates with the custom function 'extract_bf_anova'
anger_bf_anova_table <- extract_bf_anova(anger_bf_anova)

print(anger_bf_anova_table)

# Compute the Bayes Factor simple effects for the dumbbell plot figure with the custom function 'compute_bf_simple_effects'
anger_simple_effects <- compute_bf_simple_effects(
  df_wide = df, 
  outcome = "DEQ_anger_total")

print(anger_simple_effects)


combined_plot <- plot_grid(
  SWL_violin_plot, Burnout_violin_plot,
  fear_violin_plot, anger_violin_plot,
  labels = c('a)', 'b)', 'c)', 'd)'),
  ncol = 2
)

print(combined_plot)

ggsave(
  "figures/smile25b_combined_secondary_plot.jpg",
  plot = combined_plot,
  width = 14, height = 10, dpi = 300
  )

# Load libraries
library(tidyverse)
library(lmerTest)
library(emmeans)
library(BayesFactor)

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

# Compute Bayes Factor anova for the SWL scores using the custom function 'compute_bf_anova'
SWL_bf_anova <- compute_bf_anova(df_long, "SWL_total")

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

# Prepare the dataframe for the dumbbell plot with the custom function 'prepare_plot_data'
SWL_plot_data <- prepare_plot_data(
  outcome_emm = SWL_emm,
  outcome_simple_effects = SWL_simple_effects
)

# Draw the dumbbell plot with the custom function 'draw_dumbell_plot'
SWL_dumbbell_plot <- draw_dumbbell_plot(
  plot_data = SWL_plot_data, 
  outcome_label = "Satisfaction with Life",
  color_natural = "#316ac6",
  color_smile = "#009900"
)

# Save plot to figures folder
ggsave(
  "figures/smile25b_SWL_dumbbell_plot.png",
  plot = SWL_dumbbell_plot,
  width = 15, height = 8, dpi = 300
  )


# Burnout predicted by pose, context, threat, and repetition
Burnout_model <- lmer(
  Burnout_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(Burnout_model)) 

# Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
Burnout_emm <- emmeans(Burnout_model, ~ pose | context + threat + repetition)

summary(Burnout_emm)

# Compute Bayes Factor anova for the Burnout scores using the custom function 'compute_bf_anova'
Burnout_bf_anova <- compute_bf_anova(df_long, "Burnout_total")

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

# Prepare the dataframe for the dumbbell plot with the custom function 'prepare_plot_data'
Burnout_plot_data <- prepare_plot_data(
  outcome_emm = Burnout_emm,
  outcome_simple_effects = Burnout_simple_effects
)

# Draw the dumbbell plot with the custom function 'draw_dumbell_plot'
Burnout_dumbbell_plot <- draw_dumbbell_plot(
  plot_data = Burnout_plot_data, 
  outcome_label = "Burnout",
  color_natural = "#316ac6",
  color_smile = "#FF0000"
)

# Save plot to figures folder
ggsave(
  "figures/smile25b_Burnout_dumbbell_plot.png",
  plot = Burnout_dumbbell_plot,
  width = 15, height = 8, dpi = 300
  )

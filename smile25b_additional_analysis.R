# Load libraries
library(tidyverse)
library(lmerTest)
library(emmeans)

# read processed data set
df <- read_csv("data/smile25b_processed_data.csv")

# Complaince of successful AU12 activation via OpenFace 
table(df$face_compliance_binary)

# Compliance check based on AU12 activation difference (delta = 1.5 out of 5) in smile vs natual condition via OpenFace
table(df$face_compliance_scalar)

# Compliance check based on AU12 activation difference (delta = 0.5 out of 5) in smile vs natual condition via OpenFace
table(df$face_compliance_scalar_soft)

# Check the distribution of math errors
table(df$math_errors)

# Combined distribution of facial expression compliance and math errors
table(df$face_compliance_scalar, df$math_errors)

## Check potential issue with image display on Gorilla
print(
  paste0(
    "Percentage of correct image display reports: ",
    round(
      nrow(df %>% filter(Display_check == "2 times"))/
      sum(!is.na(df$Display_check)),
      2
    )
  )
)

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
    face_compliance_scalar, face_compliance_scalar_soft, face_compliance_binary, math_errors
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

# Sensitivity analysis: remove failed OpenFace compliance checks
df_sensitivity <- df_long %>% filter(face_compliance_scalar == TRUE, math_errors == 0)

# rerun model with the filtered sample
happy_model_sensitivity <- lmer(  # re-fit the same model on the compliance-filtered subset
  DEQ_happy_total ~ pose * context * threat * repetition + (1 | id),
  data = df_sensitivity
)

print(summary(happy_model_sensitivity))

# Sensitivity analysis: remove failed OpenFace compliance checks
df_sensitivity_soft <- df_long %>% filter(face_compliance_scalar_soft == TRUE, math_errors == 0)

# rerun model with the filtered sample
happy_model_sensitivity_soft <- lmer(  # re-fit the same model on the compliance-filtered subset
  DEQ_happy_total ~ pose * context * threat * repetition + (1 | id),
  data = df_sensitivity_soft
)

print(summary(happy_model_sensitivity_soft))

# Exploratory analyses

# Satisfaction with life predicted by pose, context, threat, and repetition
SWL_model <- lmer(
  SWL_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(SWL_model)) 

# Burnout predicted by pose, context, threat, and repetition
Burnout_model <- lmer(
  Burnout_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(Burnout_model)) 

# Anger predicted by pose, context, threat, and repetition
Anger_model <- lmer(
  DEQ_anger_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(Anger_model)) 

# Fear predicted by pose, context, threat, and repetition
Fear_model <- lmer(
  DEQ_fear_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(Fear_model)) 
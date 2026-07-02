# Preliminary data analysis

# Load libraries
library(tidyverse)
library(lmerTest)
library(emmeans)

# read processed data set
df <- read_csv("data/smile25b_processed_data.csv")

# Descriptive statistics

# Complaince of successful AU12 activation via OpenFace 
table(df$face_compliance_binary)

# Compliance check of smiles above the AU12 1.5 threshold via OpenFace
table(df$face_compliance_scalar)  

# Check the distribution of math errors
table(df$math_errors)

# Combined distribution of facial expression compliance and math errors
table(df$face_compliance_binary, df$math_errors)

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

# Calculate age mean and standard deviation
print(paste0("Mean age: ", round(mean(df$Age, na.rm=TRUE),2), "; sd: ", round(sd(df$Age, na.rm=TRUE),2)))

# Calculate percentage of female participants
print(paste0("Percentage female: ",  
  round(
    nrow(df %>% filter(Gender == "Female")) / nrow(df) * 100,
     2),
  "%"
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

# Full factorial general linear mixed model of happiness predicted by pose, context, threat, and repetition
happy_model <- lmer(
  DEQ_happy_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(happy_model)) 

# Plot happiness difference in smile vs natural poses across context x threat x repetition

## Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
happy_emm <- emmeans(happy_model, ~ pose | context + threat + repetition) 
summary(happy_emm)

## Extract point estimates of smile and natural pose happiness scores across conditions
happy_plot_df <- as.data.frame(happy_emm) %>%
  select(context, threat, repetition, pose, emmean) %>%
  pivot_wider(names_from = pose, values_from = emmean, names_glue = "{pose}_mean")

## Extract pairwise comparison estimates and p values
happy_contrasts <- pairs(happy_emm, reverse = TRUE) %>% 
  as.data.frame() %>%
  select(context, threat, repetition, estimate, p.value)

## Join the point and pairwise estimates 
happy_plot_df <- happy_plot_df %>%
  left_join(happy_contrasts, by = c("context", "threat", "repetition")) %>% 
  # Adjust labels
  rename(diff_mean = estimate) %>%
  mutate(
    context_label = str_to_title(context),  
    threat_label = threat,                   
    repetition_label = if_else(repetition == "one", "One time", "Ten times"), 
    condition = paste(context_label, threat_label, repetition_label, sep = " | "),
    condition = fct_reorder(condition, diff_mean),
    sig_label = case_when(
      p.value < .05 & diff_mean > 0 ~ "Smile > Natural (p < .05)",
      p.value < .05 & diff_mean < 0 ~ "Natural > Smile (p < .05)",
      TRUE ~ "No significant difference"
    )
  )

# Extract point estimates and labels for plot generation
happy_plot_points <- happy_plot_df %>%
  select(condition, smile_mean, natural_mean) %>%
  pivot_longer(c(smile_mean, natural_mean), names_to = "pose", values_to = "mean_happy") %>%  # long format for geom_point
  mutate(pose = recode(pose, smile_mean = "Smile", natural_mean = "Natural"))  # readable pose labels

# Create a dumbbell plot comparing the smile vs pose happiness scores across all conditions
happy_dumbbell_plot <- ggplot() +
  # Create the line that connects the natural and smile means for each condition
  geom_segment(  
    data = happy_plot_df,
    aes(x = natural_mean, xend = smile_mean, y = condition, yend = condition, color = sig_label),
    linewidth = 1.5
  ) +
  # smile/natural pose happiness means
  geom_point(  
    data = happy_plot_points,
    aes(x = mean_happy, y = condition, shape = pose, fill = pose),
    size = 4.5, stroke = 0
  ) +
  # determine line color based on the direction/significance of the score difference
  scale_color_manual(values = c(  
    "Smile > Natural (p < .05)" = "#f5ad06",
    "Natural > Smile (p < .05)" = "#316ac6",
    "No significant difference" = "grey70"
  )) +
  # fill the points matching the pose color
  scale_fill_manual(values = c(  
    "Natural" = "#316ac6",
    "Smile" = "#f5ad06"
  )) +
  # Set the shape of the points based on pose
  scale_shape_manual(values = c( 
    "Natural" = 21,
    "Smile" = 23
  )) +
  # Configure labels
  labs(
    x = "Happiness",
    y = "Context | Threat | Repetition",
    color = "Paired t-test",
    shape = "Pose",
    fill = "Pose",
    title = "Happiness by Posed Expression across Conditions"
  ) +
  # Set theme and font size
  theme_minimal(base_size = 16)

print(happy_dumbbell_plot)

ggsave(
  "smile25b_happy_dumbbell_plot.png",
  plot = happy_dumbbell_plot,
  width = 10, height = 8, dpi = 300
)

# Sensitivity analysis: remove failed OpenFace compliance checks
df_sensitivity <- df_long %>% filter(face_compliance_binary == TRUE, math_errors == 0)

# rerun model with the filtered sample
happy_model_sensitivity <- lmer(  # re-fit the same model on the compliance-filtered subset
  DEQ_happy_total ~ pose * context * threat * repetition + (1 | id),
  data = df_sensitivity
)

print(summary(happy_model_sensitivity))

# Preliminary data analysis

# Load libraries
library(tidyverse)
library(lmerTest)
library(emmeans)
library(BayesFactor)

# read processed data set
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
    NP_SWL_total, NP_Burnout_total,
    face_compliance_scalar, face_compliance_scalar_soft, face_compliance_binary, math_errors
  ) %>%
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
happy_model <- lmer(
  DEQ_happy_total ~ pose * context * threat * repetition + (1 | id),
  data = df_long
)

print(summary(happy_model))

# Plot happiness difference in smile vs natural poses across context x threat x repetition

## Calculate estimated marginal of pose scores conditioned by context, threat, and repetition
happy_emm <- emmeans(happy_model, ~ pose | context + threat + repetition)
summary(happy_emm)

# Bayesian analysis of facial feedback effect

# BayesFactor requires complete cases and factor-typed predictors
df_bf <- df_long %>%
  select(id, pose, context, threat, repetition, DEQ_happy_total) %>%
  drop_na(id, pose, context, threat, repetition, DEQ_happy_total) %>%
  mutate(
    id = factor(id),
    pose = factor(pose),
    context = factor(context),
    threat = factor(threat),
    repetition = factor(repetition)
  ) %>%
  as.data.frame()

# Confirmatory Bayesian ANOVA-style tests of main effects and interactions
# using default (medium Cauchy, r = 1/2) priors and default MCMC settings.

happy_bf_top <- anovaBF(
  # Full model of happiness by pose, context, threat, and repetition
  DEQ_happy_total ~ pose * context * threat * repetition + id,
  data = df_bf,
  whichRandom = "id",
  whichModels = "top"
)

print(happy_bf_top)

# Save and optionally reload Bayesian ANOVA results

saveRDS(happy_bf_top, "data/smile25b_bayes_data.Rds")

happy_bf_top <- readRDS("data/smile25b_bayes_data.Rds")

# List all main and interaction effects in the full factorial ANOVA model
full_model_terms <- attr(
  terms(DEQ_happy_total ~ pose * context * threat * repetition),
  "term.labels"
)

# Classify BF10 strength of evidence per Lee & Wagenmakers (2013)
bf_label <- function(bf) {
  bf_ref <- ifelse(bf >= 1, bf, 1 / bf)
  direction <- ifelse(bf >= 1, "alternative", "null")
  strength <- case_when(
    bf_ref < 3 ~ "anecdotal",
    bf_ref < 10 ~ "moderate",
    bf_ref < 30 ~ "strong",
    bf_ref < 100 ~ "very strong",
    TRUE ~ "extreme"
  )
  paste(strength, "evidence for", direction)
}

# Format BF10 for compact display on the dumbbell plot: 2 decimals, with
# extreme values cropped symmetrically to keep labels short and readable.
bf_value_label <- function(bf) {
  case_when(
    bf > 100  ~ "> 100",
    bf < 0.01 ~ "< 0.01",
    TRUE      ~ formatC(bf, format = "f", digits = 2)
  )
}

# Extract numeric values from the bayesian ANOVA 
# and rename the terms to highlight the term being tested (dropped) in each reduced model
happy_bf_top_table <- happy_bf_top %>%
  extractBF() %>%
  as.data.frame() %>%
  rownames_to_column("model_included") %>%
  select(model_included, bf, error) %>%
  # anovaBF(whichModels = "top") reports the reduced  model against the full model, so the bf value is inverted.
  mutate(BF10 = 1 / bf, error_pct = error) %>%
  rowwise() %>%
  mutate(
    included_terms = list(str_trim(str_split(model_included, "\\+")[[1]])),
    term_dropped = paste(setdiff(full_model_terms, included_terms), collapse = ", ")
  ) %>%
  ungroup() %>%
  select(-included_terms, -model_included, -bf, -error)  %>%
  mutate(evidence = bf_label(BF10)) %>%
  relocate(term_dropped, .before = 1) %>%
  arrange(desc(row_number()))

print(happy_bf_top_table)

## Highlight the pre-registered two-way interactions (H1, H2, H3)
happy_bf_two_way_hypotheses <- happy_bf_top_table %>%
  filter(term_dropped %in% c("pose:context", "pose:repetition", "pose:threat")) %>%
  mutate(hypothesis = case_when(
    term_dropped == "pose:context" ~ "H1: pose x context",
    term_dropped == "pose:repetition" ~ "H2: pose x repetition",
    term_dropped == "pose:threat" ~ "H3: pose x threat"
  ))

print(happy_bf_two_way_hypotheses)

# Simple-effect Bayes factors: smile vs. natural pose happiness, -----
# within each combination of context x threat x repetition.
# Pose is within-subject (SP_/NP_ columns for the same participant), so each
# cell is tested with a paired-samples Bayes factor t-test on the wide data.

condition_grid <- df %>%
  distinct(context, threat, repetition) %>%
  drop_na(context, threat, repetition)

happy_simple_effects <- condition_grid %>%
  mutate(
    result = pmap(list(context, threat, repetition), function(ctx, thr, rep) {
      cell_data <- df %>%
        filter(context == ctx, threat == thr, repetition == rep) %>%
        drop_na(SP_DEQ_happy_total, NP_DEQ_happy_total)

      bf <- ttestBF(
        x = cell_data$SP_DEQ_happy_total,
        y = cell_data$NP_DEQ_happy_total,
        paired = TRUE
      )
      bf_summary <- extractBF(bf)

      tibble(
        n = nrow(cell_data),
        BF10 = bf_summary$bf,
        error_pct = bf_summary$error
      )
    })
  ) %>%
  unnest(result)  %>%
  mutate(evidence = bf_label(BF10))

print(happy_simple_effects)

# Create figure with frequentist and bayesian simple effects

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

## Join per-condition Bayesian simple-effect results and build a two-line
## "BF10 = x.xx / strength evidence for direction" label for each condition
happy_plot_df <- happy_plot_df %>%
  left_join(
    happy_simple_effects %>% select(context, threat, repetition, BF10, evidence),
    by = c("context", "threat", "repetition")
    ) %>%
  mutate(
    bf_display = bf_value_label(BF10),
    bf_label_text = paste0(
     "BF10 ", ifelse(bf_display %in% c("> 100", "< 0.01"), bf_display, paste0("= ", bf_display)),
     "\n", evidence
      ),
    # place each label just past that condition's own right-most point estimate
    label_x = pmax(smile_mean, natural_mean, na.rm = TRUE) +
    0.05 * diff(range(c(smile_mean, natural_mean), na.rm = TRUE))
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
  # Bayesian BF10 and level-of-evidence label for each condition, placed just
  # past that condition's own right-most point estimate
  geom_text(
    data = happy_plot_df,
    aes(x = label_x, y = condition, label = bf_label_text),
    hjust = 0, size = 4.5, lineheight = 0.9, color = "grey30"
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
  # Fix the x-axis range to leave room for the BF labels past each
  # condition's right-most point estimate
  scale_x_continuous(limits = c(1, 5.5)) +
  # Set theme and font size
  theme_minimal(base_size = 16) +
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),
    plot.background = element_rect(fill = "white", colour = "white"),
    plot.margin = margin(t = 5, r = 15, b = 5, l = 5)
    )

ggsave(
  "smile25b_happy_dumbbell_plot.png",
  plot = happy_dumbbell_plot,
  width = 14, height = 8, dpi = 300
)

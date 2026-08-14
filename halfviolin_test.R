# Load libraries
library(tidyverse)
library(lmerTest)
library(emmeans)
library(BayesFactor)
library(ggh4x)
library(see)


# Read processed data set
df_wide <- read_csv("data/smile25b_processed_data.csv")

# Prepare dataframe for data analysis
df_long <- df_wide %>%
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
  relocate(pose, .after = id) %>%
  mutate(
    # x-axis grouping: Context (ordered, drives color)
    context_label = factor(str_to_title(context), levels = c("Positive", "Negative")),
    # facet-column keys: Repetition x Threat (ordered)
    repetition_label = factor(
      if_else(repetition == "one", "Pose once", "Pose ten times"),
      levels = c("Pose once", "Pose ten times")
    ),
    threat_label = factor(threat, levels = c("No threat", "Threat"))
  )

outcome <- "DEQ_happy_total"

SP_outcome <- paste0("SP_", outcome)
NP_outcome <- paste0("NP_", outcome)

violin_data <- df_wide %>%
    select(context, threat, repetition, all_of(SP_outcome), all_of(NP_outcome)) %>%
    drop_na(context, threat, repetition, all_of(SP_outcome), all_of(NP_outcome)) %>%
    mutate(diff = .data[[SP_outcome]] - .data[[NP_outcome]]) %>%
    mutate(
      # x-axis grouping: Context (ordered, drives color)
      context_label = factor(str_to_title(context), levels = c("Positive", "Negative")),
      # facet-column keys: Repetition x Threat (ordered)
      repetition_label = factor(
        if_else(repetition == "one", "Pose once", "Pose ten times"),
        levels = c("Pose once", "Pose ten times")
      ),
      threat_label = factor(threat, levels = c("No threat", "Threat"))
    ) %>%
    mutate(
      condition = interaction(
        repetition_label,
        threat_label,
        sep = " × "
      )
    )

plot_data <- violin_data %>%
    group_by(context, threat, repetition, context_label, repetition_label, threat_label) %>%
    summarise(
      diff_mean = mean(diff),
      SE = sd(diff) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      # +/- 1 SE error-bar bounds
      ymin = diff_mean - SE,
      ymax = diff_mean + SE,
      # Nudge the summary marker inside its corresponding violin half instead of
      # sitting at the shared center between the two halves
      summary_x = if_else(context_label == "Positive", 0.85, 1.15)
    )

diff_plot <- ggplot() +
    # Zero line = no smile-vs-natural difference
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    # Split half-violin: Positive context's distribution on the left half, Negative
    # context's on the right half, mirrored around the shared "" x position. `flip = 1`
    # mirrors ggplot's internal group 1, which is "Positive" since context_label's factor
    # levels are ordered c("Positive", "Negative"); Negative (group 2) stays at
    # geom_violinhalf()'s un-flipped default, which already bulges rightward.
    see::geom_violinhalf(
      data = violin_data,
      aes(
        x = interaction(repetition_label, threat_label), 
        y = diff, fill = context_label,
        group = interaction(repetition_label, threat_label, context_label)
      ),
      flip = 1, position = "identity", colour = NA, trim = TRUE
    )

print(diff_plot)



ggplot(
  df_long,
  aes(x = DEQ_happy_total)
) +
  geom_density() +
  geom_rug(alpha = 0.2) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey60"
  ) +
  facet_grid(
    pose ~ repetition_label + threat_label + context_label
  ) +
  theme_classic()

ggplot(violin_data) +

  # Positive: left-facing
  see::geom_violinhalf(
    data = subset(violin_data, context_label == "Positive"),
    aes(
      x = interaction(repetition_label, threat_label),
      y = diff,
      fill = context_label,
      color = context_label
    ),
    position = "identity",
    alpha = .5,
    flip = TRUE
  ) +

  # Negative: right-facing
  see::geom_violinhalf(
    data = subset(violin_data, context_label == "Negative"),
    aes(
      x = interaction(repetition_label, threat_label),
      y = diff,
      fill = context_label,
      color = context_label
    ),
    position = "identity",
    alpha = .5,
    flip = FALSE
  ) +

  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  geom_errorbar(
      data = plot_data,
      aes(
        x = summary_x, 
        ymin = ymin, 
        ymax = ymax, 
        colour = context_label),
      width = 0.1, linewidth = 0.9, show.legend = FALSE
    ) +
    geom_point(
      data = plot_data,
      aes(x = summary_x, y = diff_mean, colour = context_label),
      size = 3, show.legend = FALSE
    ) +

  coord_cartesian(ylim = c(-7, 7))



ggplot(
  violin_data,
  aes(
    x = interaction(repetition_label, threat_label),
    y = diff,
    color = context_label
  )
) +
  geom_jitter(
    size = 1,
    width = 0.2,
    height = 0.05,
    alpha = 0.5
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  coord_cartesian(ylim = c(-7, 7))

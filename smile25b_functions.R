
# Declare the functions used in the Bayesian analyses and in the dumbbell plot generation

# Function: classify BF10 strength of evidence per Lee & Wagenmakers (2013)
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

# Function: Compute the Bayes Factor ANOVA for a generic outcome variable
compute_bf_anova <- function(df_long, outcome) {

  # Remove incomplete cases and convert predictors to factors for the BayesFactor analysis
  df_bf <- df_long %>%
    select(id, pose, context, threat, repetition, {{outcome}}) %>%
    drop_na(id, pose, context, threat, repetition, {{outcome}}) %>%
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
  
  # Prepare the anovaBF formula using the outcome variable input
  bf_formula <- as.formula(
    paste0(outcome, " ~ pose * context * threat * repetition + id")
  )

  # Compute the full anovaBF model of outcome score by pose, context, threat, and repetition
  outcome_bf_anova <- anovaBF(
    bf_formula,
    data = df_bf,
    whichRandom = "id",
    whichModels = "top"
  )
  
  return(outcome_bf_anova)
}

# Function: Extract main and interaction effect estimates from the Bayesian ANOVA object
extract_bf_anova <- function(outcome_bf_anova) {

  # Extract all main and interaction effects from the full factorial model
  full_model_terms <- attr(
    terms(~ pose * context * threat * repetition), 
    "term.labels")

  outcome_bf_anova_table <- outcome_bf_anova %>%
    # Extract Bayes Factor estimates and convert to dataframe
    extractBF() %>%
    as.data.frame() %>%
    # Convert rownames into columns containing the model specification details
    rownames_to_column("model_included") %>%
    # Select variables of interest
    select(model_included, bf, error) %>%
    # Invert the bf value, which is computed using the reduced model (without the term of interest) as the alternative hypothesis
    mutate(BF10 = 1 / bf, error_pct = error) %>%
    # RCreate a new column containing the term being tested in each reduced model
    rowwise() %>%
    mutate(
      included_terms = list(str_trim(str_split(model_included, "\\+")[[1]])),
      term_dropped = paste(setdiff(full_model_terms, included_terms), collapse = ", ")
    ) %>%
    ungroup() %>%
    # Remove unused variables
    select(-included_terms, -model_included, -bf, -error)  %>%
    # Assign labels reflecting the strength of the evidence of each BF value
    mutate(evidence = bf_label(BF10)) %>%
    # Reorder rows and columns
    relocate(term_dropped, .before = 1) %>%
    arrange(desc(row_number()))

  return(outcome_bf_anova_table)
}

# Function: compute the simple-effect Bayes factor for the smile vs. natural pose outcome scores
# within each combination of the context x threat x repetition conditions.

compute_bf_simple_effects <- function(df_wide, outcome) {

  # Set Smile pose and Natural pose outcome variable names
  SP_outcome <- paste0("SP_", outcome)
  NP_outcome <- paste0("NP_", outcome)

  outcome_simple_effects <- df_wide %>%
    # list all possible condition combinations
    distinct(context, threat, repetition) %>%
    # remove any combinations that contain NAs
    drop_na(context, threat, repetition) %>%
    arrange(context, threat, repetition) %>%
    mutate(
      # For each condition combination, extract cases with complete outcome scores
      result = pmap(list(context, threat, repetition), function(ctx, thr, rep) {
        cell_data <- df_wide  %>%
          filter(context == ctx, threat == thr, repetition == rep) %>%
          drop_na(all_of(SP_outcome), all_of(NP_outcome))

        # For each condition combination, calculate the simple-effect Bayes Factor for the smile vs natural pose conditions
        bf <- ttestBF(
          x = cell_data[[SP_outcome]],
          y = cell_data[[NP_outcome]],
          paired = TRUE
        )

        # Extract the simple-effect Bayes Factor estimates
        bf_summary <- extractBF(bf)

        # store the number of observations, Bayes Factor estimate, and error in a tible in the result column
        tibble(
          n = nrow(cell_data),
          BF10 = bf_summary$bf,
          error_pct = bf_summary$error
        )
      })
    ) %>%
    # Extract the n, BF10, and error_pct columns nested in the result column
    unnest(result) %>%
    # Assign labels representing the strength of evidence for each Bayes Factor estimate
    mutate(evidence = bf_label(BF10))
  
  return(outcome_simple_effects)
}

# Function: Prepare plot data for the dumbbell plot with frequentist and bayesian simple effects

prepare_plot_data <- function(outcome_emm, outcome_simple_effects) {

  ## Extract point estimates of smile and natural pose happiness scores across conditions
  plot_data <- as.data.frame(outcome_emm) %>%
    select(context, threat, repetition, pose, emmean) %>%
    pivot_wider(names_from = pose, values_from = emmean, names_glue = "{pose}_mean")

  ## Extract pairwise comparison estimates and p values
  outcome_contrasts <- pairs(outcome_emm, reverse = TRUE) %>%
    as.data.frame() %>%
    select(context, threat, repetition, estimate, p.value)

  ## Join the point and pairwise estimates
  plot_data <- plot_data %>%
    left_join(outcome_contrasts, by = c("context", "threat", "repetition")) %>%
    # Adjust labels
    rename(diff_mean = estimate) %>%
    mutate(
      context_label = str_to_title(context),
      threat_label = threat,
      repetition_label = if_else(repetition == "one", "One time", "Ten times"),
      condition = paste(context_label,  repetition_label, threat_label, sep = " | "),
      condition = factor(condition, levels = rev(sort(unique(condition)))),
      sig_label = case_when(
      p.value < .05 & diff_mean > 0 ~ "Smile > Natural (p < .05)",
      p.value < .05 & diff_mean < 0 ~ "Natural > Smile (p < .05)",
      TRUE ~ "No significant difference"
      )
    )

  # Join frequentist and Bayesian analyses results and build the BF10 label for each condition
  plot_data <- plot_data %>%
    left_join(
      outcome_simple_effects %>% select(context, threat, repetition, BF10, evidence),
      by = c("context", "threat", "repetition")
      ) %>%
    mutate(
      # Crop BF10 values over 100 or under .01 and round digits to 2 decimal cases 
      bf_display = case_when(
        BF10 > 100  ~ "> 100",
        BF10 < 0.01 ~ "< 0.01",
        TRUE      ~ formatC(BF10, format = "f", digits = 2)),
      # Create label using the BF10 value and the evidence strength categories
      bf_label_text = paste0(
        "BF10 ", ifelse(bf_display %in% c("> 100", "< 0.01"), bf_display, paste0("= ", bf_display)),
        "\n", evidence),
      # place each label to the side of the right-most point estimate
      label_x = pmax(smile_mean, natural_mean, na.rm = TRUE) +
      0.05 * diff(range(c(smile_mean, natural_mean), na.rm = TRUE))
      )

  return(plot_data)
}

# Function: Use plot data to generate the dumbbell plot
draw_dumbbell_plot <- function(plot_data, outcome_label, color_natural, color_smile) {
  
  # Extract point estimates for plot generation
    plot_points <- plot_data %>%
      select(condition, smile_mean, natural_mean) %>%
      pivot_longer(c(smile_mean, natural_mean), names_to = "pose", values_to = "mean_outcome") %>%  # long format for geom_point
      mutate(pose = recode(pose, smile_mean = "Smile", natural_mean = "Natural"))  # readable pose labels

  # Create a dumbbell plot comparing the smile vs pose happiness scores across all conditions
  dumbbell_plot <- ggplot() +
  # Draw the line that connects the natural and smile means for each condition
    geom_segment(
      data = plot_data,
      aes(x = natural_mean, xend = smile_mean, y = condition, yend = condition, color = sig_label),
      linewidth = 1.5
    ) +
    # Draw the point estimates of outcome mean scores per condition
    geom_point(
      data = plot_points,
      aes(x = mean_outcome, y = condition, shape = pose, fill = pose),
      size = 4.5, stroke = 0
      ) +
    # Bayes Factor label for each condition, placed to the side of the right-most point estimate
    geom_text(
      data = plot_data,
      aes(x = label_x, y = condition, label = bf_label_text),
      hjust = 0, size = 4.5, lineheight = 0.9, color = "grey30"
      ) +
    # determine line color based on the direction/significance of the outcome score difference
    scale_color_manual(values = c(
      "Smile > Natural (p < .05)" = color_smile,
      "Natural > Smile (p < .05)" = color_natural,
      "No significant difference" = "grey70"
      )) +
    # fill the points matching the pose color
      scale_fill_manual(values = c(
      "Natural" = color_natural,
      "Smile" = color_smile
      )) +
    # Set the shape of the points based on pose
    scale_shape_manual(values = c(
      "Natural" = 21,
      "Smile" = 23
      )) +
    # Configure axis and legend labels
    labs(
      x = outcome_label,
      y = "Context | Repetition | Threat",
      color = "Smile vs. Natural contrast",
      shape = "Pose",
      fill = "Pose",
      title = paste0(outcome_label, " by Posed Expression across Conditions")
      ) +
    # Fix the x-axis range to leave enough room for the BF labels
    scale_x_continuous(expand = expansion(add = c(0.5, 2.5))) +
    # Set theme and font size
    theme_minimal(base_size = 16) +
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background = element_rect(fill = "white", colour = "white"),
      plot.margin = margin(t = 5, r = 15, b = 5, l = 5)
      )
}
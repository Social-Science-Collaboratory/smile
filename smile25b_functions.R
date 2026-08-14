

# Function: Prepare the per-participant smile - natural difference scores, used to
# draw the point estimate + error bar plot (positive = smiling increased the outcome)

draw_plot <- function(
  df_wide, 
  outcome_label, 
  outcome,
  legend_position = c("top_right", "bottom_right", "none"),
  x_axis = TRUE,
  y_axis = TRUE,
  y_text
) {

  legend_position <- match.arg(legend_position)

  # Set Smile pose and Natural pose outcome variable names
  SP_outcome <- paste0("SP_", outcome)
  NP_outcome <- paste0("NP_", outcome)

  # Set plot colors: blue = positive, reddish-orange = negative
  color_positive <- "#0055c4"
  color_negative <- "#e53b03c4"


  plot_data <- df_wide %>%
    select(context, threat, repetition, all_of(SP_outcome), all_of(NP_outcome)) %>%
    drop_na(context, threat, repetition, all_of(SP_outcome), all_of(NP_outcome)) %>%
    mutate(diff = .data[[SP_outcome]] - .data[[NP_outcome]]) %>%
    mutate(
      # x-axis grouping: Context (ordered, drives color)
      context_label = factor(str_to_title(context), levels = c("Negative", "Positive")),
      # facet-column keys: Repetition x Threat (ordered)
      repetition_label = factor(
        if_else(repetition == "one", "Pose once", "Pose ten times"),
        levels = c("Pose once", "Pose ten times")
      ),
      threat_label = factor(threat, levels = c("No threat", "Threat"))
    ) %>%
    # Group by the condition combinations
    group_by(context_label, repetition_label, threat_label) %>%
    # Calculate mean and standard error
    summarise(
      diff_mean = mean(diff),
      SE = sd(diff) / sqrt(n()),
      .groups = "drop"
    ) %>%
    mutate(
      # calculate error bar limits on the y-axis
      error_min = diff_mean - SE,
      error_max = diff_mean + SE
    )

  # Prepare pose effect direction labels that indicate the direction of the pose effect 
  direction_label_data <- data.frame(
    # Set x-axis positioning
    x = 0.5,
    # Set y-axis positining
    y = c(.5, -.5),
    # define label text
    label = c(
      paste0("smiling increases ", outcome_label),
      paste0("smiling decreases ", outcome_label)
    ),
    # Identify the facet where the label will appear ('Pose once', 'No threat')
    repetition_label = levels(plot_data$repetition_label)[1],
    threat_label = levels(plot_data$threat_label)[1]
  )

  # Legend placement: draw legend at the top right, bottom right, or hide
  legend_theme <- switch(
    legend_position,
    top_right = theme(
      legend.position = "inside",
      legend.position.inside = c(.95, .95),
      legend.justification = c("right", "top"),
      legend.background = element_rect(colour = "black", fill = "white")
    ),
    bottom_right = theme(
      legend.position = "inside",
      legend.position.inside = c(.95, .05),
      legend.justification = c("right", "bottom"),
      legend.background = element_rect(colour = "black", fill = "white")
    ),
    none = theme(legend.position = "none")
  )

  # Optionally show direction labels, placed inside the panel to the left of the violins
  direction_labels <- if (y_axis) {
    geom_text(
      data = direction_label_data,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE, angle = 90, size = 5, color = "grey30"
    )
  } else {
    NULL
  }
  
  # Optionally show facet strip labels and nesting line (acts as the x-axis grouping)
  nest_line_element <- if (x_axis) {
    element_line(colour = "black", linewidth = 1)
  } else {
    element_blank()
  }

  strip_theme <- if (x_axis) {
    theme(
      strip.placement = "outside",
      strip.text.x = element_text(size = 15),
      ggh4x.facet.nestline = nest_line_element
    )
  } else {
    theme(
      strip.text.x = element_blank(),
      ggh4x.facet.nestline = nest_line_element
    )
  }

  # Begin plotting
  diff_plot <- ggplot() +
    # Plot the y = 0 intercept line
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    # Draw the mean point estimates using the point_data information
    geom_point(
      data = plot_data,
      aes(x = "", y = diff_mean, color = context_label),
      size = 2
    ) +
    # Draw the error bar using the point_data information
    geom_errorbar(
      data = plot_data,
      aes(x = "", ymin = error_min, ymax = error_max, color = context_label),
      width = 0.075, linewidth = 0.7, show.legend = FALSE
    ) +
    # Optionally show direction labels, placed inside the panel to the left of the violins
    direction_labels +
    # Create nested facet labels using the ggh4x package
    ggh4x::facet_nested(
      # Nest the threat factor within the repetition factor 
      . ~ repetition_label + threat_label,
      # Position facet labels on the bottom
      switch = "x",
      # Create a line above the facet labels indicating nestedness
      nest_line = nest_line_element,
      # Remove the frame and background of the facet strips
      strip = ggh4x::strip_nested(
        background_x = list(
          element_rect(linewidth = 0, fill = "white"),
          element_blank()
        ) 
      )
    ) +
    scale_colour_manual(
      values = c("Positive" = color_positive, "Negative" = color_negative)
    ) +
    # Set the y-axis range
    coord_cartesian(ylim = c(-1, 1)) +
    # Remove x-axis and create legend box
    labs(
      x = NULL,
      y = y_text,
      color = "Context"
    ) +
    # Define classic theme (no grid) and theme settings (background, text size, facet strip settings)
    theme_classic(base_size = 16) +
    theme(
      panel.background = element_rect(fill = "white", colour = "white"),
      plot.background  = element_rect(fill = "white", colour = "white"),
      plot.margin      = margin(t = 25, r = 25, b = 15, l = 15),
      axis.text.x      = element_blank(),
      axis.ticks.x     = element_blank(),
      legend.key.width = unit(4, "lines"),
      legend.key.height = unit(1.5, "lines")
    ) +
    # Optionally show facet strip labels and nesting line (acts as the x-axis)
    strip_theme +
    legend_theme

  diff_plot
}


# Declare the functions used in the Bayesian analyses and in the plot generation

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

  # Compute the full anovaBF model of outcome score by pose, context, threat, and
  # repetition
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
    # Create a new column containing the term being tested in each reduced model
    rowwise() %>%
    mutate(
      included_terms = list(str_trim(str_split(model_included, "\\+")[[1]])),
      term_dropped = paste(setdiff(full_model_terms, included_terms), collapse = ", ")
    ) %>%
    ungroup() %>%
    # Remove unused variables
    select(-included_terms, -model_included, -bf, -error)  %>%
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
    unnest(result)
  
  return(outcome_simple_effects)
}

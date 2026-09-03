message("Generating review-level effect displays without meta-meta-analysis...")

anchor_effects <- readRDS(file.path(cfg$paths$results, "data", "anchor_effects.rds"))
supporting_effects <- readRDS(file.path(cfg$paths$results, "data", "supporting_effects.rds"))

display_data <- bind_rows(
  anchor_effects |> mutate(display_role = "Anchor estimate"),
  supporting_effects |> mutate(display_role = "Supporting / concordance only")
) |>
  filter(!is.na(display_estimate)) |>
  mutate(
    display_role = factor(
      display_role,
      levels = c("Anchor estimate", "Supporting / concordance only")
    ),
    display_ci_low = if_else(effect_family == "RATIO" & display_ci_low <= 0, NA_real_, display_ci_low),
    display_ci_high = if_else(effect_family == "RATIO" & display_ci_high <= 0, NA_real_, display_ci_high),
    threshold_label = if_else(is_missing_token(threshold), "", paste0("; ", threshold)),
    row_label = paste0(
      master_id, " · ", predictor, " → ", outcome,
      threshold_label, " [", effect_id, "]"
    ),
    row_label = stringr::str_trunc(row_label, 115)
  )

effect_profile <- display_data |>
  group_by(analysis_cell, effect_measure, effect_family) |>
  summarise(
    displayed_estimates = n(),
    anchor_estimates = sum(display_role == "Anchor estimate"),
    supporting_estimates = sum(display_role == "Supporting / concordance only"),
    reviews = n_distinct(master_id),
    estimates_with_ci = sum(!is.na(display_ci_low) & !is.na(display_ci_high)),
    .groups = "drop"
  ) |>
  arrange(analysis_cell, effect_family, effect_measure)

write_table(effect_profile, "effect_display_profile.csv", cfg)
write_table(
  display_data |>
    select(
      effect_id, master_id, analysis_cell, predictor, outcome, time,
      effect_measure, estimate, ci_low, ci_high, threshold, display_role,
      source_type, page_or_table, dependency_note
    ),
  "effect_display_rows.csv",
  cfg
)

make_effect_plot <- function(df, cell, measure, family) {
  df <- df |>
    arrange(display_role, master_id, predictor, outcome, threshold) |>
    mutate(row_order = row_number(), row_label = factor(row_label, levels = rev(unique(row_label))))

  p <- ggplot(df, aes(x = display_estimate, y = row_label, colour = display_role)) +
    geom_segment(
      data = df |> filter(!is.na(display_ci_low), !is.na(display_ci_high)),
      aes(x = display_ci_low, xend = display_ci_high, yend = row_label),
      linewidth = 0.7,
      alpha = 0.85
    ) +
    geom_point(aes(shape = display_role), size = 2.7, stroke = 0.8) +
    scale_colour_manual(values = c(
      "Anchor estimate" = "#17365D",
      "Supporting / concordance only" = "#8A9BA8"
    )) +
    scale_shape_manual(values = c(
      "Anchor estimate" = 18,
      "Supporting / concordance only" = 16
    )) +
    labs(
      title = paste0(gsub("_", " ", cell), " — ", measure),
      subtitle = "Review-level estimates are displayed for triangulation; no pooled diamond is calculated",
      x = measure,
      y = NULL,
      colour = NULL,
      shape = NULL
    ) +
    theme_publication(base_size = ifelse(nrow(df) > 25, 7.5, 9)) +
    theme(panel.grid.major.y = element_line(colour = "#EEF2F5", linewidth = 0.3))

  null <- unique(na.omit(df$null_value))
  if (length(null) == 1L) {
    p <- p + geom_vline(xintercept = null, linetype = "dashed", colour = "#6B7280", linewidth = 0.5)
  }

  if (family == "RATIO") {
    positive_values <- c(df$display_estimate, df$display_ci_low, df$display_ci_high)
    positive_values <- positive_values[is.finite(positive_values) & positive_values > 0]
    if (length(positive_values) > 0L) {
      p <- p + scale_x_log10(labels = scales::label_number(accuracy = 0.01))
    }
  } else if (family == "PROBABILITY") {
    p <- p + scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      breaks = seq(0, 1, 0.2),
      limits = c(0, 1)
    )
  }

  p
}

plot_groups <- display_data |>
  distinct(analysis_cell, effect_measure, effect_family) |>
  arrange(analysis_cell, effect_measure)

plot_index <- vector("list", nrow(plot_groups))

for (i in seq_len(nrow(plot_groups))) {
  cell <- plot_groups$analysis_cell[i]
  measure <- plot_groups$effect_measure[i]
  family <- plot_groups$effect_family[i]
  one <- display_data |>
    filter(analysis_cell == cell, effect_measure == measure, effect_family == family)

  if (nrow(one) < 2L) {
    plot_index[[i]] <- tibble(
      analysis_cell = cell,
      effect_measure = measure,
      effect_family = family,
      rows = nrow(one),
      status = "NOT_PLOTTED_SINGLE_ESTIMATE",
      exclusion_reason = "A one-row forest plot adds no comparative information; the estimate remains in the tabular output.",
      figure_tier = "TABLE_ONLY",
      file_stem = NA_character_
    )
    next
  }

  incompatible_cognitive_units <- cell == "COGNITIVE_CHANGE" &&
    family == "DIFFERENCE" &&
    n_distinct(paste(one$predictor, one$outcome, sep = " -> ")) > 1L
  if (incompatible_cognitive_units) {
    plot_index[[i]] <- tibble(
      analysis_cell = cell,
      effect_measure = measure,
      effect_family = family,
      rows = nrow(one),
      status = "NOT_PLOTTED_HETEROGENEOUS_INSTRUMENT_UNITS",
      exclusion_reason = "Unstandardized differences from incompatible cognitive instruments cannot share a meaningful numeric axis.",
      figure_tier = "TABLE_ONLY",
      file_stem = NA_character_
    )
    next
  }

  # Probability values outside [0,1] indicate an extraction or scale issue and are not silently clipped.
  if (family == "PROBABILITY" && any(
    c(one$display_estimate, one$display_ci_low, one$display_ci_high) < 0 |
      c(one$display_estimate, one$display_ci_low, one$display_ci_high) > 1,
    na.rm = TRUE
  )) {
    plot_index[[i]] <- tibble(
      analysis_cell = cell,
      effect_measure = measure,
      effect_family = family,
      rows = nrow(one),
      status = "NOT_PLOTTED_PROBABILITY_OUT_OF_RANGE",
      exclusion_reason = "At least one extracted probability or confidence limit lies outside [0,1].",
      figure_tier = "TABLE_ONLY",
      file_stem = NA_character_
    )
    next
  }

  stem <- paste0(
    "forest_",
    sanitize_filename(cell),
    "__",
    sanitize_filename(measure)
  )
  height <- min(14, max(4.5, 2.5 + 0.28 * nrow(one)))
  p <- make_effect_plot(one, cell, measure, family)
  save_plot_dual(p, stem, cfg, width = 11.5, height = height)

  plot_index[[i]] <- tibble(
    analysis_cell = cell,
    effect_measure = measure,
    effect_family = family,
    rows = nrow(one),
    status = "PLOTTED_NO_POOLING",
    exclusion_reason = NA_character_,
    figure_tier = if_else(
      cell %in% c("TAP_TEST_DTA", "LIT_DTA", "ROUT_THRESHOLDS", "ELD_DTA"),
      "CURATED_CLINICAL_SUPPLEMENT",
      "EXPLORATORY_SUPPLEMENT"
    ),
    file_stem = stem
  )
}

plot_index <- bind_rows(plot_index)
write_table(plot_index, "effect_display_figure_index.csv", cfg)

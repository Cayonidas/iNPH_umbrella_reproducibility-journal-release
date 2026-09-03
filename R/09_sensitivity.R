message("Running umbrella-review sensitivity and fragility analyses...")

anchor_effects <- readRDS(file.path(cfg$paths$results, "data", "anchor_effects.rds"))

baseline_microcells <- sort(unique(anchor_effects$microcell_id))
baseline_cells <- sort(unique(anchor_effects$analysis_cell))

scenario_filters <- list(
  main = rep(TRUE, nrow(anchor_effects)),
  exclude_ft012_single_anchor_robustness = anchor_effects$master_id != "FT-012",
  exclude_ft003_author_conflict = anchor_effects$master_id != "FT-003",
  exclude_all_current_team_authored_reviews = !anchor_effects$master_id %in% c("FT-003", "FT-011"),
  robis_low_final_consensus = if (run_state$quality_consensus_complete) {
    anchor_effects$robis_final_low %in% TRUE
  } else {
    rep(FALSE, nrow(anchor_effects))
  },
  robis_not_high_final_consensus = if (run_state$quality_consensus_complete) {
    !(anchor_effects$robis_final_high %in% TRUE)
  } else {
    rep(FALSE, nrow(anchor_effects))
  },
  robis_low_by_both_reviewers = anchor_effects$robis_low_both %in% TRUE,
  robis_not_high_by_either_reviewer = anchor_effects$robis_not_high_both %in% TRUE,
  explicitly_validated_inph_shunt_response_threshold = anchor_effects$any_validated_threshold %in% TRUE
)

scenario_results <- imap_dfr(scenario_filters, function(keep, scenario) {
  x <- anchor_effects[keep %in% TRUE, , drop = FALSE]
  retained_microcells <- sort(unique(x$microcell_id))
  retained_cells <- sort(unique(x$analysis_cell))
  tibble(
    scenario = scenario,
    estimates_retained = nrow(x),
    reviews_retained = n_distinct(x$master_id),
    microcells_retained = length(retained_microcells),
    high_level_cells_retained = length(retained_cells),
    microcells_lost_n = length(setdiff(baseline_microcells, retained_microcells)),
    high_level_cells_lost_n = length(setdiff(baseline_cells, retained_cells)),
    microcells_lost = paste(setdiff(baseline_microcells, retained_microcells), collapse = " || "),
    high_level_cells_lost = paste(setdiff(baseline_cells, retained_cells), collapse = "; "),
    interpretation = case_when(
      scenario == "main" ~ "Reference umbrella analysis; no review-level pooling.",
      length(retained_microcells) == 0L ~ "All anchor microcells lost; no matched estimate substitution is allowed.",
      length(setdiff(baseline_microcells, retained_microcells)) > 0L ~
        "Some anchor microcells are lost. This indicates dependence on excluded reviews, not a contradictory pooled effect.",
      TRUE ~ "All anchor microcells retained."
    )
  )
})

scenario_microcell_status <- imap_dfr(scenario_filters, function(keep, scenario) {
  retained <- unique(anchor_effects$microcell_id[keep %in% TRUE])
  anchor_effects |>
    distinct(microcell_id, analysis_cell, master_id, predictor, outcome, time, effect_measure, threshold) |>
    transmute(
      scenario,
      microcell_id,
      analysis_cell,
      anchor_review = master_id,
      retained = microcell_id %in% retained,
      status = if_else(retained, "ANCHOR_RETAINED", "ANCHOR_LOST"),
      rule = "No alternative review is substituted unless it matches the same predictor, outcome, time, measure, and threshold."
    )
})

leave_one_review_out <- sort(unique(anchor_effects$master_id)) |>
  map_dfr(function(excluded_review) {
    retained <- anchor_effects |>
      filter(master_id != excluded_review)
    lost_microcells <- setdiff(baseline_microcells, unique(retained$microcell_id))
    lost_cells <- setdiff(baseline_cells, unique(retained$analysis_cell))
    tibble(
      excluded_review,
      anchor_estimates_removed = sum(anchor_effects$master_id == excluded_review),
      microcells_lost_n = length(lost_microcells),
      high_level_cells_lost_n = length(lost_cells),
      high_level_cells_lost = paste(lost_cells, collapse = "; "),
      microcells_lost = paste(lost_microcells, collapse = " || ")
    )
  }) |>
  arrange(desc(microcells_lost_n), excluded_review)

single_review_dependency <- anchor_effects |>
  group_by(analysis_cell) |>
  summarise(
    anchor_review_n = n_distinct(master_id),
    anchor_reviews = paste(sort(unique(master_id)), collapse = "; "),
    microcells_n = n_distinct(microcell_id),
    wholly_dependent_on_one_review = anchor_review_n == 1,
    depends_on_ft012 = "FT-012" %in% master_id,
    depends_on_ft003 = "FT-003" %in% master_id,
    depends_on_any_current_team_authored_review = any(master_id %in% c("FT-003", "FT-011")),
    .groups = "drop"
  )

write_table(scenario_results, "sensitivity_scenario_summary.csv", cfg)
write_table(scenario_microcell_status, "sensitivity_microcell_status.csv", cfg)
write_table(leave_one_review_out, "sensitivity_leave_one_anchor_review_out.csv", cfg)
write_table(single_review_dependency, "sensitivity_single_review_dependency.csv", cfg)
saveRDS(
  list(
    summary = scenario_results,
    microcells = scenario_microcell_status,
    leave_one_out = leave_one_review_out,
    dependency = single_review_dependency
  ),
  file.path(cfg$paths$results, "data", "sensitivity_results.rds")
)

p_sensitivity <- scenario_results |>
  mutate(
    scenario_label = stringr::str_to_sentence(gsub("_", " ", scenario)),
    scenario_label = factor(scenario_label, levels = rev(scenario_label))
  ) |>
  ggplot(aes(x = microcells_retained, y = scenario_label, fill = microcells_lost_n > 0)) +
  geom_col(width = 0.68) +
  geom_text(
    aes(label = paste0(microcells_retained, "/", length(baseline_microcells))),
    hjust = -0.08,
    size = 3.2
  ) +
  scale_fill_manual(values = c(`FALSE` = "#0F6B78", `TRUE` = "#ED7D31"), guide = "none") +
  scale_x_continuous(
    limits = c(0, length(baseline_microcells) * 1.12),
    breaks = scales::pretty_breaks()
  ) +
  labs(
    title = "Anchor-microcell retention under sensitivity analyses",
    subtitle = "Loss of an anchor is reported as fragility; unmatched estimates are not substituted",
    x = "Quantitative microcells retained",
    y = NULL
  ) +
  theme_publication()

save_plot_dual(p_sensitivity, "figure_09_sensitivity_anchor_retention", cfg, width = 10.5, height = 6.5)

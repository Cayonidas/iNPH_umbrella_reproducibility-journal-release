message("Building the clinical translation and analysis-readiness matrices...")

dat <- readRDS(file.path(cfg$paths$results, "data", "analysis_data.rds"))
run_state <- readRDS(file.path(cfg$paths$results, "data", "run_state.rds"))
quality_flags <- readRDS(file.path(cfg$paths$results, "data", "quality_flags.rds"))
definition_profile <- readRDS(file.path(cfg$paths$results, "data", "definition_profile.rds"))
anchor_cell_audit <- readRDS(file.path(cfg$paths$results, "data", "anchor_cell_audit.rds"))
anchor_effects <- readRDS(file.path(cfg$paths$results, "data", "anchor_effects.rds"))
overlap_summary <- readRDS(file.path(cfg$paths$results, "data", "overlap_summary.rds"))

anchor_ids_long <- dat$meta_cells |>
  transmute(analysis_cell = cell, master_id = map(anchor_review, extract_ft_ids)) |>
  unnest(master_id)

anchor_limitations <- anchor_ids_long |>
  left_join(quality_flags, by = "master_id") |>
  left_join(definition_profile, by = "master_id") |>
  group_by(analysis_cell) |>
  summarise(
    anchor_reviews = paste(sort(unique(master_id)), collapse = "; "),
    anchor_review_n = n_distinct(master_id),
    robis_high_by_reviewer_1_n = sum(robis_overall_a == "HIGH", na.rm = TRUE),
    robis_high_by_reviewer_2_n = sum(robis_overall_b == "HIGH", na.rm = TRUE),
    robis_high_either_n = sum(robis_high_either, na.rm = TRUE),
    all_anchors_low_both = all(robis_low_both, na.rm = TRUE),
    robis_high_consensus_n = sum(robis_final_high %in% TRUE, na.rm = TRUE),
    all_anchors_low_consensus = all(robis_final_low %in% TRUE, na.rm = TRUE),
    any_validated_threshold_proxy = any(any_validated_threshold, na.rm = TRUE),
    any_patient_reported_proxy = any(any_patient_reported, na.rm = TRUE),
    .groups = "drop"
  )

effect_counts <- anchor_effects |>
  group_by(analysis_cell) |>
  summarise(
    anchor_estimates_n = n(),
    numeric_anchor_estimates_n = sum(!is.na(estimate_num)),
    anchor_microcells_n = n_distinct(microcell_id),
    .groups = "drop"
  )

cell_overlap_proxy <- overlap_summary$cell_proxy |>
  transmute(
    analysis_cell = cell,
    overlap_coverage = coverage,
    overlap_coverage_fraction = coverage_fraction,
    cell_proxy_cca = cca_num,
    cell_proxy_cca_interpretation = interpretation,
    cell_proxy_status = status
  )

cell_overlap_targeted_ai <- overlap_summary$targeted_ai |>
  filter(scenario == "ALL_RESPONSE_REFERENCE_STANDARDS") |>
  transmute(
    analysis_cell,
    overlap_coverage = paste0(c, "/", count_ft_ids(requested_reviews), " verified reviews"),
    overlap_coverage_fraction = coverage_fraction,
    cell_proxy_cca = cca,
    cell_proxy_cca_interpretation = interpretation,
    cell_proxy_status = "EXACT_CELL_SPECIFIC_RECOVERY"
  )

cell_overlap <- bind_rows(
  cell_overlap_proxy |> filter(analysis_cell != "AI_MODELS"),
  cell_overlap_targeted_ai
)

translation_lookup <- tribble(
  ~analysis_cell, ~clinical_translation,
  "TAP_TEST_DTA", "PROBABILITY_MODIFIER_NOT_NEGATIVE_GATEKEEPER",
  "LIT_DTA", "PROBABILITY_MODIFIER_NOT_NEGATIVE_GATEKEEPER",
  "ROUT_THRESHOLDS", "THRESHOLD_SPECIFIC_COUNSELING_NOT_NEGATIVE_GATEKEEPER",
  "ELD_DTA", "SELECTED_CASES_AT_EXPERIENCED_CENTERS",
  "COMPARATIVE_PROGNOSTIC_TESTS", "NO_INDIRECT_RANKING",
  "STRUCTURAL_IMAGING", "DIAGNOSTIC_SUPPORT_NOT_SELECTION_GATEKEEPER",
  "BIOMARKERS", "RESEARCH_ONLY",
  "COMPLEX_PHENOTYPE", "PROGNOSTIC_COUNSELING_NOT_CAUSAL_EFFECT_MODIFICATION",
  "COGNITIVE_CHANGE", "MEASUREMENT_AND_EXPECTATION_SETTING",
  "GAIT_CHANGE", "MEASUREMENT_STANDARDIZATION",
  "SURVIVAL", "PROGNOSTIC_COUNSELING",
  "AI_MODELS", "RESEARCH_ONLY_UNTIL_EXTERNAL_VALIDATION_AND_CALIBRATION",
  "BENEFIT_HARM_DURABILITY", "SHARED_DECISION_MAKING_AND_LONGITUDINAL_FOLLOW_UP"
)

clinical_cell_matrix <- dat$meta_cells |>
  transmute(
    analysis_cell = cell,
    clinical_question,
    prespecified_anchor_text = anchor_review,
    supporting_reviews,
    current_data,
    current_action,
    de_novo_feasibility,
    key_risk
  ) |>
  left_join(effect_counts, by = "analysis_cell") |>
  left_join(anchor_limitations, by = "analysis_cell") |>
  left_join(cell_overlap, by = "analysis_cell") |>
  left_join(translation_lookup, by = "analysis_cell") |>
  mutate(
    across(
      c(anchor_estimates_n, numeric_anchor_estimates_n, anchor_microcells_n, anchor_review_n),
      ~ coalesce(.x, 0L)
    ),
    quantitative_anchor_status = if_else(
      numeric_anchor_estimates_n > 0,
      "AVAILABLE",
      "NO_NUMERIC_ANCHOR"
    ),
    robis_status = case_when(
      run_state$quality_consensus_complete & robis_high_consensus_n > 0 ~
        "HIGH_RISK_IN_FINAL_CONSENSUS",
      run_state$quality_consensus_complete & all_anchors_low_consensus ~
        "LOW_RISK_IN_FINAL_CONSENSUS",
      run_state$quality_consensus_complete ~ "MIXED_OR_UNCLEAR_FINAL_CONSENSUS",
      robis_high_either_n > 0 ~ "HIGH_BY_AT_LEAST_ONE_REVIEWER",
      all_anchors_low_both ~ "LOW_BY_BOTH_REVIEWERS",
      TRUE ~ "DUAL_REVIEWER_MIXED_OR_UNCLEAR"
    ),
    response_definition_status = case_when(
      any_validated_threshold_proxy ~ "ANCHOR_REVIEW_CONTAINS_VALIDATED_THRESHOLD_PROXY",
      TRUE ~ "NO_VALIDATED_THRESHOLD_IDENTIFIED_IN_ANCHOR_REVIEW"
    ),
    overlap_status = case_when(
      is.na(cell_proxy_cca) ~ "NOT_ESTIMABLE_WITH_CURRENT_COVERAGE",
      cell_proxy_cca_interpretation %in% c("HIGH", "VERY_HIGH") ~ "LOCALLY_HIGH_OR_VERY_HIGH",
      TRUE ~ paste0(cell_proxy_cca_interpretation, "_WITH_PARTIAL_COVERAGE")
    ),
    analysis_readiness = case_when(
      quantitative_anchor_status == "NO_NUMERIC_ANCHOR" ~ "STRUCTURED_NARRATIVE_ONLY",
      !run_state$quality_consensus_complete ~ "READY_FOR_ANALYSIS_QUALITY_CONSENSUS_PENDING",
      TRUE ~ "READY_FOR_FINAL_ANALYSIS"
    )
  )

write_table(clinical_cell_matrix, "clinical_cell_analysis_matrix.csv", cfg)
write_table(dat$clinical_synthesis, "clinical_translation_claims.csv", cfg)

readiness_long <- clinical_cell_matrix |>
  transmute(
    analysis_cell,
    `Quantitative anchor` = quantitative_anchor_status,
    `ROBIS` = robis_status,
    `Response definition` = response_definition_status,
    `Overlap` = overlap_status
  ) |>
  pivot_longer(-analysis_cell, names_to = "dimension", values_to = "status") |>
  mutate(
    severity = case_when(
      str_detect(status, "AVAILABLE$|LOW_BY_BOTH|LOW_RISK_IN_FINAL|VALIDATED_THRESHOLD_PROXY|SLIGHT") ~
        "Favorable / available",
      str_detect(status, "NO_NUMERIC|HIGH_OR_VERY_HIGH|NO_VALIDATED|HIGH_BY_AT_LEAST|HIGH_RISK_IN_FINAL") ~
        "Major limitation",
      TRUE ~ "Caution / incomplete"
    ),
    analysis_cell = factor(analysis_cell, levels = rev(clinical_cell_matrix$analysis_cell)),
    dimension = factor(
      dimension,
      levels = c("Quantitative anchor", "ROBIS", "Response definition", "Overlap")
    )
  )

write_table(readiness_long, "clinical_cell_limitations_long.csv", cfg)

p_readiness <- ggplot(readiness_long, aes(x = dimension, y = analysis_cell, fill = severity)) +
  geom_tile(colour = "white", linewidth = 0.6) +
  scale_fill_manual(values = c(
    "Favorable / available" = "#70AD47",
    "Caution / incomplete" = "#FFC000",
    "Major limitation" = "#C00000"
  )) +
  labs(
    title = "Analysis readiness and major limitations by clinical evidence cell",
    subtitle = "Colours are categorical flags, not a numerical certainty score",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_publication(base_size = 9) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_plot_dual(p_readiness, "figure_08_clinical_cell_readiness", cfg, width = 10.5, height = 7.2)

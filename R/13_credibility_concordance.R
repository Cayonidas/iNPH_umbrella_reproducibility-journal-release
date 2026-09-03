message("Building non-scored credibility, concordance, completeness, and timeline analyses...")

dat <- readRDS(file.path(cfg$paths$results, "data", "analysis_data.rds"))
run_state <- readRDS(file.path(cfg$paths$results, "data", "run_state.rds"))
quality_flags <- readRDS(file.path(cfg$paths$results, "data", "quality_flags.rds"))
anchor_effects <- readRDS(file.path(cfg$paths$results, "data", "anchor_effects.rds"))
definition_profile <- readRDS(file.path(cfg$paths$results, "data", "definition_profile.rds"))
overlap_summary <- readRDS(file.path(cfg$paths$results, "data", "overlap_summary.rds"))

normalize_direction <- function(x) {
  z <- norm_token(x)
  case_when(
    is.na(z) ~ "UNCERTAIN_OR_NOT_DIRECTIONAL",
    str_detect(z, "INCONSISTENT|MIXED|HETEROGEN|VARIABLE|CONFLICT") ~ "MIXED_OR_INCONSISTENT",
    str_detect(z, "NO_CLEAR|NO_CONSISTENT|NO_ASSOCIATION|NEAR_NULL|NULL|NOT_PREDICT|NO_DIFFERENCE") ~
      "NO_ASSOCIATION",
    str_detect(z, "WORSE|LOWER_RESPONSE|NEGATIVE|REDUCED_RESPONSE|POORER") ~
      "NEGATIVE_ASSOCIATION",
    str_detect(z, "POSITIVE|IMPROV|HIGHER_RESPONSE|PREDICT|ASSOCIAT|PROMISING") ~
      "POSITIVE_ASSOCIATION",
    TRUE ~ "UNCERTAIN_OR_NOT_DIRECTIONAL"
  )
}

finding_directions <- dat$findings |>
  transmute(
    finding_id,
    master_id,
    predictor_category,
    predictor,
    outcome_domain,
    source_direction = association_direction,
    direction_class = normalize_direction(association_direction),
    adjusted_status = norm_token(adjusted_or_unadjusted),
    studies_k_num = parse_num(studies_k),
    participants_n_num = parse_num(participants_n),
    interpretation_guardrail = "Directional concordance across reviews; not a causal treatment-effect-modification analysis."
  )

review_category_direction <- finding_directions |>
  group_by(master_id, predictor_category) |>
  summarise(
    contributing_findings = n(),
    observed_direction_classes = paste(sort(unique(direction_class)), collapse = "; "),
    review_level_direction = case_when(
      n_distinct(direction_class[direction_class != "UNCERTAIN_OR_NOT_DIRECTIONAL"]) > 1 ~
        "MIXED_OR_INCONSISTENT",
      any(direction_class == "MIXED_OR_INCONSISTENT") ~ "MIXED_OR_INCONSISTENT",
      any(direction_class == "POSITIVE_ASSOCIATION") ~ "POSITIVE_ASSOCIATION",
      any(direction_class == "NEGATIVE_ASSOCIATION") ~ "NEGATIVE_ASSOCIATION",
      any(direction_class == "NO_ASSOCIATION") ~ "NO_ASSOCIATION",
      TRUE ~ "UNCERTAIN_OR_NOT_DIRECTIONAL"
    ),
    .groups = "drop"
  )

direction_concordance <- review_category_direction |>
  count(predictor_category, review_level_direction, name = "review_count") |>
  group_by(predictor_category) |>
  mutate(
    reviews_in_category = sum(review_count),
    proportion_within_category = review_count / reviews_in_category
  ) |>
  ungroup()

method_completeness <- dat$method_facts |>
  transmute(
    master_id,
    protocol_before_review_source = protocol_before_review,
    comprehensive_search_source = comprehensive_search,
    duplicate_screening_source = duplicate_screening,
    duplicate_extraction_source = duplicate_extraction,
    primary_rob_assessed_source = primary_rob_assessed,
    rob_used_in_synthesis_source = rob_used_in_synthesis,
    protocol_before_review = clearly_yes_flag(protocol_before_review_source),
    comprehensive_search = clearly_yes_flag(comprehensive_search_source),
    duplicate_screening = clearly_yes_flag(duplicate_screening_source),
    duplicate_extraction = clearly_yes_flag(duplicate_extraction_source),
    primary_rob_assessed = clearly_yes_flag(primary_rob_assessed_source),
    rob_used_in_synthesis = clearly_yes_flag(rob_used_in_synthesis_source),
    comprehensive_search_partial_or_limited = str_detect(
      norm_token(comprehensive_search_source),
      "PARTIAL|LIMITED|UNCLEAR"
    ),
    rob_used_in_synthesis_partial_or_unclear = str_detect(
      norm_token(rob_used_in_synthesis_source),
      "PARTIAL|UNCLEAR|NO/NR|NA/NR"
    ),
    method_note = "Binary completeness flags indicate clearly reported conduct; partial/unclear reporting is not upgraded to yes."
  )

quantitative_completeness <- dat$effects |>
  summarise(
    quantitative_rows = n(),
    rows_with_point_estimate = sum(!is.na(estimate_num)),
    rows_with_complete_ci = sum(!is.na(ci_low_num) & !is.na(ci_high_num)),
    rows_with_numeric_study_count = sum(!is.na(studies_k_num)),
    rows_with_numeric_participant_count = sum(!is.na(participants_n_num)),
    rows_explicitly_adjusted = sum(str_detect(norm_token(adjusted), "^ADJUSTED$|^YES$"), na.rm = TRUE),
    rows_unadjusted_or_unclear = sum(!str_detect(norm_token(adjusted), "^ADJUSTED$|^YES$") | is.na(norm_token(adjusted)))
  ) |>
  mutate(
    causal_guardrail = "Association or classification estimates are not causal treatment-effect-modification estimates."
  )

evidence_construct_audit <- dat$effects |>
  transmute(
    effect_id,
    master_id,
    analysis_cell,
    predictor,
    outcome,
    effect_measure,
    adjusted_status = norm_token(adjusted),
    evidence_construct = case_when(
      str_detect(norm_token(effect_measure), "CALIBR|BRIER|OBSERVED/EXPECTED") ~ "CALIBRATION",
      str_detect(norm_token(effect_measure), "AUC|C_STATISTIC|DISCRIMINATION") ~ "DISCRIMINATION",
      str_detect(norm_token(effect_measure), "SENSITIVITY|SPECIFICITY|PPV|NPV|ACCURACY|DOR") ~
        "RESPONSE_CLASSIFICATION",
      str_detect(
        norm_token(paste(effect_measure, predictor, outcome)),
        "INTERACTION|TREATMENT_EFFECT_MODIFICATION"
      ) & str_detect(norm_token(adjusted), "^ADJUSTED$|^YES$") ~
        "CAUSAL_TREATMENT_EFFECT_MODIFICATION_CANDIDATE",
      str_detect(norm_token(effect_measure), "CHANGE|DIFFERENCE|MD|SMD|SMCR") ~ "OUTCOME_CHANGE",
      TRUE ~ "PROGNOSTIC_ASSOCIATION_OR_OTHER"
    ),
    interpretation_rule = case_when(
      evidence_construct == "CAUSAL_TREATMENT_EFFECT_MODIFICATION_CANDIDATE" ~
        "Requires a credible treatment-by-predictor interaction design before causal interpretation.",
      TRUE ~ "Must not be interpreted as proof that the predictor changes the causal effect of shunting."
    )
  )

evidence_construct_summary <- evidence_construct_audit |>
  count(evidence_construct, name = "estimate_count") |>
  mutate(proportion = estimate_count / sum(estimate_count))

anchor_ids <- dat$meta_cells |>
  transmute(analysis_cell = cell, master_id = map(anchor_review, extract_ft_ids)) |>
  unnest(master_id)

anchor_method <- anchor_ids |>
  left_join(method_completeness, by = "master_id") |>
  left_join(quality_flags, by = "master_id") |>
  left_join(definition_profile, by = "master_id") |>
  group_by(analysis_cell) |>
  summarise(
    anchor_reviews = paste(sort(unique(master_id)), collapse = "; "),
    anchor_review_n = n_distinct(master_id),
    any_robis_high = if (run_state$quality_consensus_complete) {
      any(robis_final_high %in% TRUE)
    } else {
      any(robis_high_either %in% TRUE)
    },
    all_robis_low = if (run_state$quality_consensus_complete) {
      all(robis_final_low %in% TRUE)
    } else {
      all(robis_low_both %in% TRUE)
    },
    robis_basis = if (run_state$quality_consensus_complete) {
      "FINAL_AUTHOR_CONSENSUS"
    } else {
      "DUAL_REVIEWER_PRECONSENSUS"
    },
    all_primary_rob_assessed = all(primary_rob_assessed %in% TRUE),
    all_rob_used_in_synthesis = all(rob_used_in_synthesis %in% TRUE),
    any_validated_response_threshold = any(any_validated_threshold %in% TRUE),
    .groups = "drop"
  )

anchor_precision <- anchor_effects |>
  group_by(analysis_cell) |>
  summarise(
    anchor_estimates_n = n(),
    complete_ci_n = sum(!is.na(display_ci_low) & !is.na(display_ci_high)),
    adjusted_estimates_n = sum(str_detect(norm_token(adjusted), "^ADJUSTED$|^YES$"), na.rm = TRUE),
    .groups = "drop"
  )

cell_overlap_proxy <- overlap_summary$cell_proxy |>
  transmute(
    analysis_cell = cell,
    overlap_coverage_fraction = coverage_fraction,
    overlap_cca = cca_num,
    overlap_interpretation = interpretation
  )

cell_overlap_targeted_ai <- overlap_summary$targeted_ai |>
  filter(scenario == "ALL_RESPONSE_REFERENCE_STANDARDS") |>
  transmute(
    analysis_cell,
    overlap_coverage_fraction = coverage_fraction,
    overlap_cca = cca,
    overlap_interpretation = interpretation
  )

cell_overlap <- bind_rows(
  cell_overlap_proxy |> filter(analysis_cell != "AI_MODELS"),
  cell_overlap_targeted_ai
)

credibility_profile <- dat$meta_cells |>
  transmute(analysis_cell = cell, clinical_question, key_risk) |>
  left_join(anchor_method, by = "analysis_cell") |>
  left_join(anchor_precision, by = "analysis_cell") |>
  left_join(cell_overlap, by = "analysis_cell") |>
  mutate(
    robis_concern = case_when(
      any_robis_high ~ "HIGH_CONCERN",
      all_robis_low ~ "LOW_CONCERN",
      TRUE ~ "SOME_CONCERN"
    ),
    primary_rob_integration_concern = case_when(
      all_primary_rob_assessed & all_rob_used_in_synthesis ~ "LOW_CONCERN",
      all_primary_rob_assessed ~ "SOME_CONCERN",
      TRUE ~ "HIGH_CONCERN"
    ),
    precision_concern = case_when(
      is.na(anchor_estimates_n) | anchor_estimates_n == 0 ~ "NOT_ASSESSABLE",
      complete_ci_n == anchor_estimates_n ~ "LOW_CONCERN",
      complete_ci_n > 0 ~ "SOME_CONCERN",
      TRUE ~ "HIGH_CONCERN"
    ),
    directness_concern = case_when(
      analysis_cell %in% c("BIOMARKERS", "AI_MODELS", "COMPLEX_PHENOTYPE") ~ "HIGH_CONCERN",
      analysis_cell %in% c("COMPARATIVE_PROGNOSTIC_TESTS", "STRUCTURAL_IMAGING") ~ "SOME_CONCERN",
      TRUE ~ "LOW_CONCERN"
    ),
    response_definition_concern = if_else(
      any_validated_response_threshold,
      "LOW_CONCERN",
      "HIGH_CONCERN"
    ),
    overlap_concern = case_when(
      is.na(overlap_coverage_fraction) | overlap_coverage_fraction < 0.50 ~ "HIGH_CONCERN",
      overlap_coverage_fraction < 1 | overlap_interpretation %in% c("HIGH", "VERY_HIGH") ~ "SOME_CONCERN",
      TRUE ~ "LOW_CONCERN"
    ),
    single_anchor_dependence_concern = case_when(
      is.na(anchor_review_n) | anchor_review_n == 0 ~ "NOT_ASSESSABLE",
      anchor_review_n == 1 ~ "HIGH_CONCERN",
      TRUE ~ "SOME_CONCERN"
    ),
    causal_interpretation_concern = case_when(
      analysis_cell %in% c("COMPLEX_PHENOTYPE", "BIOMARKERS", "STRUCTURAL_IMAGING", "AI_MODELS") &
        coalesce(adjusted_estimates_n, 0L) == 0L ~ "HIGH_CONCERN",
      coalesce(adjusted_estimates_n, 0L) == 0L ~ "SOME_CONCERN",
      TRUE ~ "LOW_CONCERN"
    )
  ) |>
  rowwise() |>
  mutate(
    decision_support = case_when(
      sum(c_across(ends_with("_concern")) == "HIGH_CONCERN", na.rm = TRUE) >= 3 ~ "VERY_LIMITED",
      any(c_across(ends_with("_concern")) == "HIGH_CONCERN", na.rm = TRUE) ~ "LIMITED",
      TRUE ~ "CAUTIOUSLY_INTERPRETABLE"
    ),
    framework_note = paste(
      "Transparent categorical profile developed for this umbrella review; not GRADE,",
      "not a numerical score, and not a substitute for domain-specific judgment."
    )
  ) |>
  ungroup()

credibility_long <- credibility_profile |>
  select(analysis_cell, ends_with("_concern")) |>
  pivot_longer(-analysis_cell, names_to = "dimension", values_to = "concern") |>
  mutate(dimension = str_to_sentence(gsub("_concern$|_", " ", dimension)))

review_timeline <- dat$reviews |>
  transmute(
    master_id,
    year = parse_num(year),
    layer = layer_group(final_layer),
    primary_domain
  ) |>
  filter(!is.na(year)) |>
  count(year, layer, name = "review_count") |>
  group_by(layer) |>
  arrange(year, .by_group = TRUE) |>
  mutate(cumulative_reviews_within_layer = cumsum(review_count)) |>
  ungroup()

timeline_overall <- dat$reviews |>
  transmute(year = parse_num(year)) |>
  filter(!is.na(year)) |>
  count(year, name = "review_count") |>
  arrange(year) |>
  mutate(cumulative_reviews = cumsum(review_count))

recent_review_n <- dat$reviews |>
  summarise(n = sum(parse_num(year) >= 2024 & parse_num(year) <= 2026, na.rm = TRUE)) |>
  pull(n)

write_table(finding_directions, "concordance_finding_directions.csv", cfg)
write_table(review_category_direction, "concordance_review_predictor_category.csv", cfg)
write_table(direction_concordance, "concordance_summary_by_predictor_category.csv", cfg)
write_table(method_completeness, "methods_completeness_by_review.csv", cfg)
write_table(quantitative_completeness, "quantitative_reporting_and_causal_audit.csv", cfg)
write_table(evidence_construct_audit, "evidence_construct_audit.csv", cfg)
write_table(evidence_construct_summary, "evidence_construct_summary.csv", cfg)
write_table(credibility_profile, "claim_level_credibility_profile_not_grade.csv", cfg)
write_table(credibility_long, "claim_level_credibility_profile_long.csv", cfg)
write_table(review_timeline, "review_timeline_by_layer.csv", cfg)
write_table(timeline_overall, "review_timeline_overall.csv", cfg)

p_concordance <- direction_concordance |>
  mutate(
    predictor_category = factor(
      predictor_category,
      levels = rev(unique(predictor_category[order(reviews_in_category)]))
    )
  ) |>
  ggplot(aes(x = review_level_direction, y = predictor_category, fill = proportion_within_category)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = review_count), size = 2.7) +
  scale_fill_gradient(low = "#EEF5F9", high = "#17365D", labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Directional concordance across reviews",
    subtitle = "Counts are review × predictor-category summaries; direction does not establish causality",
    x = NULL, y = NULL, fill = "Within category"
  ) +
  theme_publication(base_size = 7.5) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

save_plot_dual(p_concordance, "figure_12_directional_concordance", cfg, width = 11, height = 10)

p_credibility <- credibility_long |>
  mutate(
    analysis_cell = factor(analysis_cell, levels = rev(unique(analysis_cell))),
    concern = factor(
      concern,
      levels = c("LOW_CONCERN", "SOME_CONCERN", "HIGH_CONCERN", "NOT_ASSESSABLE")
    )
  ) |>
  ggplot(aes(x = dimension, y = analysis_cell, fill = concern)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  scale_fill_manual(values = c(
    LOW_CONCERN = "#70AD47", SOME_CONCERN = "#FFC000",
    HIGH_CONCERN = "#C00000", NOT_ASSESSABLE = "#B8C2CC"
  )) +
  labs(
    title = "Claim-level credibility profile",
    subtitle = "Categorical framework developed for this review; not GRADE and not a numerical score",
    x = NULL, y = NULL, fill = NULL
  ) +
  theme_publication(base_size = 8) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot_dual(p_credibility, "figure_13_claim_credibility_profile", cfg, width = 12, height = 7.5)

p_timeline <- timeline_overall |>
  ggplot(aes(x = year)) +
  geom_col(aes(y = review_count), fill = "#9ECAE1", width = 0.8) +
  geom_line(aes(y = cumulative_reviews), colour = "#17365D", linewidth = 1) +
  geom_point(aes(y = cumulative_reviews), colour = "#17365D", size = 1.8) +
  labs(
    title = "Rapid growth of the iNPH review literature",
    subtitle = paste0(
      "Bars show annual reviews; line shows cumulative reviews; ",
      recent_review_n, "/", nrow(dat$reviews),
      " were published in 2024–2026; 2026 is incomplete (data freeze 11 August 2026)"
    ),
    x = "Publication year", y = "Review count"
  ) +
  theme_publication()

save_plot_dual(p_timeline, "figure_14_review_timeline", cfg, width = 10, height = 5.8)

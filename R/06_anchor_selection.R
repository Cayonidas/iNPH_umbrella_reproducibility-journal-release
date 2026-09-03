message("Freezing anchor-review microcells and dependency rules...")

dat <- readRDS(file.path(cfg$paths$results, "data", "analysis_data.rds"))
quality_flags <- readRDS(file.path(cfg$paths$results, "data", "quality_flags.rds"))
definition_profile <- readRDS(file.path(cfg$paths$results, "data", "definition_profile.rds"))
overlap_summary <- readRDS(file.path(cfg$paths$results, "data", "overlap_summary.rds"))

included_ids <- dat$eligibility |>
  filter(norm_token(decision) == "INCLUDE") |>
  pull(master_id)

effects <- dat$effects |>
  filter(master_id %in% included_ids) |>
  mutate(
    analysis_cell = coalesce(analysis_cell, "UNCLASSIFIED"),
    microcell_id = paste(
      analysis_cell,
      coalesce(predictor, "NR"),
      coalesce(outcome, "NR"),
      coalesce(time, "NR"),
      coalesce(effect_measure, "NR"),
      coalesce(threshold, "NR"),
      sep = " | "
    ),
    role_norm = norm_token(umbrella_role),
    source_type_norm = norm_token(source_type)
  ) |>
  left_join(quality_flags, by = "master_id") |>
  left_join(definition_profile, by = "master_id") |>
  left_join(dat$report_status, by = "master_id")

anchor_effects <- effects |>
  filter(
    role_norm == "ANCHOR_ESTIMATE_FOR_CELL",
    source_type_norm != "OWN_COHORT",
    norm_token(pool_across_reviews) == "NO"
  )

supporting_effects <- effects |>
  filter(
    role_norm == "SUPPORTING_CONCORDANCE_ONLY",
    source_type_norm != "OWN_COHORT",
    norm_token(pool_across_reviews) == "NO"
  )

excluded_hybrid_effects <- effects |>
  filter(
    role_norm == "EXCLUDE_FROM_UMBRELLA_POOL" |
      source_type_norm == "OWN_COHORT"
  )

microcell_audit <- anchor_effects |>
  group_by(microcell_id, analysis_cell, predictor, outcome, time, effect_measure, threshold) |>
  summarise(
    anchor_review_count = n_distinct(master_id),
    anchor_review = paste(sort(unique(master_id)), collapse = "; "),
    anchor_effect_count = n(),
    estimate_available = any(!is.na(estimate_num)),
    ci_available = any(!is.na(ci_low_num) & !is.na(ci_high_num)),
    selection_status = case_when(
      anchor_review_count == 1 ~ "ONE_ANCHOR_REVIEW",
      anchor_review_count == 0 ~ "NO_ANCHOR_REVIEW",
      TRUE ~ "CONFLICT_MULTIPLE_ANCHOR_REVIEWS"
    ),
    .groups = "drop"
  )

anchor_conflicts <- microcell_audit |>
  filter(anchor_review_count != 1)

meta_cell_ids <- dat$meta_cells |>
  transmute(
    analysis_cell = cell,
    clinical_question,
    anchor_review_text = anchor_review,
    supporting_reviews_text = supporting_reviews,
    prespecified_anchor_ids = map(anchor_review, extract_ft_ids),
    prespecified_support_ids = map(supporting_reviews, extract_ft_ids),
    current_data,
    current_action,
    de_novo_feasibility,
    key_risk
  )

observed_anchor_by_cell <- anchor_effects |>
  group_by(analysis_cell) |>
  summarise(
    observed_anchor_ids = list(sort(unique(master_id))),
    anchor_estimate_count = n(),
    numeric_anchor_estimate_count = sum(!is.na(estimate_num)),
    microcell_count = n_distinct(microcell_id),
    .groups = "drop"
  )

support_by_cell <- supporting_effects |>
  group_by(analysis_cell) |>
  summarise(
    observed_support_ids = list(sort(unique(master_id))),
    supporting_estimate_count = n(),
    .groups = "drop"
  )

normalize_id_list <- function(x) {
  if (is.null(x) || length(x) == 0L || all(is.na(x))) character() else as.character(x)
}

anchor_cell_audit <- meta_cell_ids |>
  left_join(observed_anchor_by_cell, by = "analysis_cell") |>
  left_join(support_by_cell, by = "analysis_cell") |>
  mutate(
    observed_anchor_ids = map(observed_anchor_ids, normalize_id_list),
    observed_support_ids = map(observed_support_ids, normalize_id_list),
    anchor_estimate_count = coalesce(anchor_estimate_count, 0L),
    numeric_anchor_estimate_count = coalesce(numeric_anchor_estimate_count, 0L),
    microcell_count = coalesce(microcell_count, 0L),
    supporting_estimate_count = coalesce(supporting_estimate_count, 0L),
    prespecified_anchor_present = map2_lgl(
      prespecified_anchor_ids,
      observed_anchor_ids,
      ~ length(intersect(.x, .y)) > 0L
    ),
    extra_observed_anchor_ids = map2(
      observed_anchor_ids,
      prespecified_anchor_ids,
      setdiff
    ),
    missing_prespecified_anchor_ids = map2(
      prespecified_anchor_ids,
      observed_anchor_ids,
      setdiff
    ),
    depends_on_ft012 = map_lgl(prespecified_anchor_ids, ~ "FT-012" %in% .x),
    depends_on_ft003 = map_lgl(prespecified_anchor_ids, ~ "FT-003" %in% .x),
    anchor_status = case_when(
      numeric_anchor_estimate_count == 0 ~ "NO_NUMERIC_ANCHOR_ESTIMATE",
      !prespecified_anchor_present ~ "ANCHOR_ROLE_REQUIRES_REVIEW",
      TRUE ~ "ANCHOR_AVAILABLE"
    )
  )

anchor_cell_audit_flat <- anchor_cell_audit |>
  mutate(
    prespecified_anchor_ids = map_chr(prespecified_anchor_ids, ~ paste(.x, collapse = "; ")),
    prespecified_support_ids = map_chr(prespecified_support_ids, ~ paste(.x, collapse = "; ")),
    observed_anchor_ids = map_chr(observed_anchor_ids, ~ paste(.x, collapse = "; ")),
    observed_support_ids = map_chr(observed_support_ids, ~ paste(.x, collapse = "; ")),
    extra_observed_anchor_ids = map_chr(extra_observed_anchor_ids, ~ paste(.x, collapse = "; ")),
    missing_prespecified_anchor_ids = map_chr(missing_prespecified_anchor_ids, ~ paste(.x, collapse = "; "))
  )

anchor_quality <- anchor_effects |>
  distinct(
    master_id,
    robis_overall_a,
    robis_overall_b,
    robis_consensus_overall,
    amstar_consensus_confidence,
    quality_mode
  ) |>
  arrange(master_id)

# The extraction workbook freezes the adjudicated role of each quantitative
# estimate. This audit makes the operational hierarchy explicit and exposes
# every candidate row, so the role assignment is reproducible and reviewable.
# Magnitude and direction are deliberately absent from the hierarchy.
anchor_candidate_audit <- effects |>
  mutate(
    own_cohort_excluded = source_type_norm == "OWN_COHORT" |
      role_norm == "EXCLUDE_FROM_UMBRELLA_POOL",
    exact_microcell_match = !is.na(analysis_cell) &
      !is.na(predictor) & !is.na(outcome) & !is.na(time) &
      !is.na(effect_measure),
    paired_or_interval_reporting =
      (!is.na(estimate_num) & !is.na(ci_low_num) & !is.na(ci_high_num)) |
      effect_family %in% c("SENSITIVITY", "SPECIFICITY"),
    selected_as_anchor = role_norm == "ANCHOR_ESTIMATE_FOR_CELL" &
      !own_cohort_excluded & norm_token(pool_across_reviews) == "NO",
    selection_or_exclusion_reason = case_when(
      own_cohort_excluded ~ "Excluded before ranking: authors' own cohort or hybrid primary-study estimate.",
      role_norm == "ANCHOR_ESTIMATE_FOR_CELL" ~
        "Selected after exact microcell matching and adjudication using reporting completeness, ROBIS, primary-study-list/overlap usability, evidence-base size and search recency; magnitude and direction were not selection criteria.",
      role_norm == "SUPPORTING_CONCORDANCE_ONLY" ~
        "Eligible supporting estimate; not selected as anchor after matched-cell adjudication and retained only for direction/concordance.",
      role_norm == "EXPLORATORY_NO_POOL" ~
        "Exploratory or non-comparable estimate; retained descriptively and not eligible for anchor synthesis.",
      TRUE ~ "Not eligible for anchor synthesis under the frozen role rule."
    ),
    hierarchy = paste(
      "1 exclude own-cohort/hybrid estimates;",
      "2 require exact predictor-outcome-time-measure-threshold microcell;",
      "3 prefer complete paired/interval reporting;",
      "4 prefer lower ROBIS and usable primary-study lists/overlap data;",
      "5 prefer larger evidence base then more recent search;",
      "6 resolve ties by two-reviewer adjudication; magnitude/direction never used"
    )
  ) |>
  select(
    effect_id, master_id, analysis_cell, microcell_id, predictor, outcome, time,
    effect_measure, threshold, source_type, umbrella_role, selected_as_anchor,
    own_cohort_excluded, exact_microcell_match, paired_or_interval_reporting,
    robis_consensus_overall, studies_k, participants_n,
    selection_or_exclusion_reason, hierarchy
  ) |>
  arrange(analysis_cell, microcell_id, desc(selected_as_anchor), master_id, effect_id)

write_table(microcell_audit, "anchor_microcell_audit.csv", cfg)
write_table(anchor_conflicts, "anchor_microcell_conflicts.csv", cfg)
write_table(anchor_cell_audit_flat, "anchor_high_level_cell_audit.csv", cfg)
write_table(anchor_quality, "anchor_review_quality.csv", cfg)
write_table(anchor_candidate_audit, "anchor_selection_candidate_audit.csv", cfg)
write_table(excluded_hybrid_effects, "quantitative_own_cohort_rows_excluded.csv", cfg)

saveRDS(anchor_effects, file.path(cfg$paths$results, "data", "anchor_effects.rds"))
saveRDS(supporting_effects, file.path(cfg$paths$results, "data", "supporting_effects.rds"))
saveRDS(anchor_cell_audit, file.path(cfg$paths$results, "data", "anchor_cell_audit.rds"))

if (nrow(anchor_conflicts) > 0L) {
  stop(
    "Anchor selection contains ", nrow(anchor_conflicts),
    " microcells without exactly one anchor review. Inspect anchor_microcell_conflicts.csv.",
    call. = FALSE
  )
}

message(
  "Anchor hierarchy frozen: ", nrow(microcell_audit),
  " quantitative microcells, each with exactly one anchor review."
)

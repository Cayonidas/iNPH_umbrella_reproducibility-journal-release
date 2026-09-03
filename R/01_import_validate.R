message("Importing source workbooks as immutable text tables...")

source_paths <- c(
  extraction = cfg$paths$extraction_workbook,
  overlap = cfg$paths$overlap_workbook,
  quality = cfg$paths$quality_workbook,
  dta_2x2 = cfg$paths$dta_2x2,
  report_status = cfg$paths$report_status,
  targeted_ai_overlap = cfg$paths$targeted_ai_overlap
)

missing_sources <- source_paths[!file.exists(source_paths)]
if (length(missing_sources) > 0L) {
  stop("Missing source files: ", paste(missing_sources, collapse = ", "), call. = FALSE)
}

manifest <- tibble(
  source = names(source_paths),
  path = unname(source_paths),
  file_name = basename(unname(source_paths)),
  size_bytes = unname(file.info(source_paths)$size),
  modified_at = format(unname(file.info(source_paths)$mtime), tz = "UTC", usetz = TRUE),
  sha256 = vapply(
    source_paths,
    digest::digest,
    character(1),
    algo = "sha256",
    file = TRUE
  )
)
write_data(manifest, "source_manifest.csv", cfg)

extraction_file <- source_paths[["extraction"]]
overlap_file <- source_paths[["overlap"]]
quality_file <- source_paths[["quality"]]

dat <- list(
  eligibility = read_sheet_text(extraction_file, "Final_Eligibility"),
  reviews = read_sheet_text(extraction_file, "Review_Characteristics"),
  findings = read_sheet_text(extraction_file, "Predictor_Findings"),
  effects = read_sheet_text(extraction_file, "Quantitative_Estimates"),
  definitions = read_sheet_text(extraction_file, "Outcome_Definitions"),
  primary_studies = read_sheet_text(extraction_file, "Primary_Studies"),
  cohort_overlap = read_sheet_text(extraction_file, "Cohort_Overlap"),
  method_facts = read_sheet_text(extraction_file, "Method_Facts"),
  clinical_synthesis = read_sheet_text(extraction_file, "Clinical_Synthesis"),
  review_domains = read_sheet_text(extraction_file, "Review_Domains"),
  domain_evidence_map = read_sheet_text(extraction_file, "Domain_Evidence_Map"),
  statistical_plan = read_sheet_text(extraction_file, "Statistical_Plan"),
  meta_cells = read_sheet_text(extraction_file, "Meta_Analysis_Cells"),
  pending_verification = read_sheet_text(extraction_file, "Pending_Verification"),
  cca_summary_source = read_sheet_text(overlap_file, "CCA_Summary", skip = 3),
  cca_layer_source = read_sheet_text(overlap_file, "CCA_By_Layer", skip = 3),
  cca_domain_source = read_sheet_text(overlap_file, "CCA_By_Domain", skip = 3),
  cell_proxy_cca = read_sheet_text(overlap_file, "Cell_Proxy_CCA", skip = 3),
  review_audit = read_sheet_text(overlap_file, "Review_Audit", skip = 3),
  occurrence_long = read_sheet_text(overlap_file, "Occurrence_Long", skip = 3),
  shared_publications = read_sheet_text(overlap_file, "Shared_Publications", skip = 3),
  cohort_families = read_sheet_text(overlap_file, "Cohort_Families", skip = 3),
  pairwise_overlap_source = read_sheet_text(overlap_file, "Pairwise_Overlap", skip = 3),
  candidate_queue = read_sheet_text(overlap_file, "Candidate_Queue", skip = 3),
  quality_reviews = read_sheet_text(quality_file, "Review_Consensus"),
  robis_domain_queue = read_sheet_text(quality_file, "ROBIS_Domain_Queue"),
  robis_signalling = read_sheet_text(quality_file, "ROBIS_Signalling"),
  amstar_summary = read_sheet_text(quality_file, "AMSTAR2_Summary"),
  amstar_items = read_sheet_text(quality_file, "AMSTAR2_Items"),
  agreement_source = read_sheet_text(quality_file, "Agreement_Metrics"),
  analysis_readiness_source = read_sheet_text(quality_file, "Analysis_Readiness")
)

require_columns(
  dat$definitions,
  c(
    "master_id", "primary_study_or_review_level", "domain",
    "definition_class", "patient_reported", "validated_for_inph", "mcid_source"
  ),
  "Outcome_Definitions"
)

require_columns(
  dat$method_facts,
  c(
    "master_id", "protocol_before_review", "comprehensive_search",
    "duplicate_screening", "duplicate_extraction", "primary_rob_assessed",
    "rob_used_in_synthesis"
  ),
  "Method_Facts"
)

require_columns(
  dat$quality_reviews,
  c(
    "master_id",
    "d1_a", "d1_b", "d1_consensus",
    "d2_a", "d2_b", "d2_consensus",
    "d3_a", "d3_b", "d3_consensus",
    "d4_a", "d4_b", "d4_consensus",
    "overall_a", "overall_b", "overall_consensus"
  ),
  "Review_Consensus"
)

require_columns(
  dat$amstar_summary,
  c(
    "master_id", "applicability_a", "applicability_b",
    "confidence_a", "confidence_b", "final_applicability", "final_confidence"
  ),
  "AMSTAR2_Summary"
)

require_columns(
  dat$amstar_items,
  c("master_id", "item", "answer_a", "answer_b", "final_consensus"),
  "AMSTAR2_Items"
)

dat$eligibility <- dat$eligibility |> filter(str_detect(master_id, "^FT-[0-9]{3}$"))
dat$reviews <- dat$reviews |> filter(str_detect(master_id, "^FT-[0-9]{3}$"))
dat$findings <- dat$findings |>
  filter(!is.na(finding_id), trimws(finding_id) != "")
dat$effects <- dat$effects |>
  filter(str_detect(effect_id, "^FE-[0-9]{4}$")) |>
  mutate(
    estimate_num = parse_num(estimate),
    ci_low_num = parse_num(ci_low),
    ci_high_num = parse_num(ci_high),
    studies_k_num = parse_num(studies_k),
    participants_n_num = parse_num(participants_n),
    effect_family = effect_family(effect_measure),
    display_estimate = normalize_probability_scale(estimate_num, effect_family),
    display_ci_low = normalize_probability_scale(ci_low_num, effect_family),
    display_ci_high = normalize_probability_scale(ci_high_num, effect_family),
    null_value = effect_null(effect_family)
  )
dat$definitions <- dat$definitions |> filter(str_detect(master_id, "^FT-[0-9]{3}$"))
dat$primary_studies <- dat$primary_studies |> filter(str_detect(master_id, "^FT-[0-9]{3}$"))
dat$method_facts <- dat$method_facts |> filter(str_detect(master_id, "^FT-[0-9]{3}$"))
dat$review_domains <- dat$review_domains |> filter(str_detect(master_id, "^FT-[0-9]{3}$"))
dat$pending_verification <- dat$pending_verification |> filter(str_detect(master_id, "^FT-[0-9]{3}$"))
dat$review_audit <- dat$review_audit |> filter(str_detect(review_id, "^FT-[0-9]{3}$"))
dat$occurrence_long <- dat$occurrence_long |> filter(str_detect(review_id, "^FT-[0-9]{3}$"))
dat$candidate_queue <- dat$candidate_queue |> filter(str_detect(review_id, "^FT-[0-9]{3}$"))
dat$quality_reviews <- dat$quality_reviews |> filter(str_detect(master_id, "^FT-[0-9]{3}$"))
dat$robis_domain_queue <- dat$robis_domain_queue |> filter(str_detect(master_id, "^FT-[0-9]{3}$"))
dat$robis_signalling <- dat$robis_signalling |> filter(str_detect(master_id, "^FT-[0-9]{3}$"))
dat$amstar_summary <- dat$amstar_summary |> filter(str_detect(master_id, "^FT-[0-9]{3}$"))
dat$amstar_items <- dat$amstar_items |> filter(str_detect(master_id, "^FT-[0-9]{3}$"))

dat$report_status <- readr::read_csv(
  cfg$paths$report_status,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
)

included_ids <- dat$eligibility |>
  filter(norm_token(decision) == "INCLUDE") |>
  pull(master_id)

publication_reviews <- dat$review_audit |>
  filter(norm_token(publication_cca_status) == "INCLUDED") |>
  pull(review_id) |>
  unique()

publication_occurrences <- dat$occurrence_long |>
  filter(
    review_id %in% publication_reviews,
    yes_flag(umbrella_eligible),
    !is.na(publication_key_cca),
    publication_key_cca != ""
  ) |>
  distinct(review_id, publication_key_cca)

publication_recomputed <- cca_stat(
  c = n_distinct(publication_occurrences$review_id),
  N = nrow(publication_occurrences),
  r = n_distinct(publication_occurrences$publication_key_cca)
)

cohort_reviews <- dat$review_audit |>
  filter(norm_token(cohort_cca_status) == "INCLUDED") |>
  pull(review_id) |>
  unique()

cohort_occurrences <- dat$occurrence_long |>
  filter(
    review_id %in% cohort_reviews,
    yes_flag(umbrella_eligible),
    !is.na(cohort_family),
    cohort_family != ""
  ) |>
  distinct(review_id, cohort_family)

cohort_recomputed <- cca_stat(
  c = n_distinct(cohort_occurrences$review_id),
  N = nrow(cohort_occurrences),
  r = n_distinct(cohort_occurrences$cohort_family)
)

source_cohort_summary <- dat$cca_summary_source |>
  filter(norm_token(analysis) == "CONSERVATIVE_COHORT_SENSITIVITY")
source_cohort_N <- if (nrow(source_cohort_summary) > 0L) {
  parse_num(source_cohort_summary$n[[1]])
} else {
  NA_real_
}
source_cohort_cca <- if (nrow(source_cohort_summary) > 0L) {
  parse_num(source_cohort_summary$cca[[1]])
} else {
  NA_real_
}

checks <- list()
add_check <- function(id, severity, observed, expected, passed, message) {
  checks[[length(checks) + 1L]] <<- tibble(
    check_id = id,
    severity = severity,
    passed = isTRUE(passed),
    observed = as.character(observed),
    expected = as.character(expected),
    message = message
  )
}

add_check(
  "included_review_count", "ERROR", length(included_ids), cfg$expected$included_reviews,
  length(included_ids) == cfg$expected$included_reviews,
  "Final eligibility must contain the frozen included-review count."
)
add_check(
  "review_characteristics_count", "ERROR", nrow(dat$reviews), cfg$expected$included_reviews,
  nrow(dat$reviews) == cfg$expected$included_reviews,
  "Review characteristics must cover every included review."
)
add_check(
  "predictor_finding_count", "ERROR", nrow(dat$findings), cfg$expected$predictor_findings,
  nrow(dat$findings) == cfg$expected$predictor_findings,
  "Predictor findings changed from the frozen extraction."
)
add_check(
  "predictor_finding_id_format", "ERROR",
  paste(dat$findings$finding_id[!valid_finding_id(dat$findings$finding_id)], collapse = ";"),
  "All IDs match F-0000 or documented semantic F-000-LABEL schema",
  all(valid_finding_id(dat$findings$finding_id)),
  paste(
    "Finding IDs must use the sequential schema (for example, F-0001) or the",
    "documented semantic schema (for example, F-019-SURV)."
  )
)
add_check(
  "unique_predictor_finding_ids", "ERROR",
  n_distinct(dat$findings$finding_id), nrow(dat$findings),
  n_distinct(dat$findings$finding_id) == nrow(dat$findings),
  "Every predictor finding must have a unique identifier."
)
add_check(
  "predictor_finding_review_id_coverage", "ERROR",
  sum(dat$findings$master_id %in% included_ids), nrow(dat$findings),
  all(dat$findings$master_id %in% included_ids),
  "Every predictor finding must link to an included review."
)
add_check(
  "quantitative_estimate_count", "ERROR", nrow(dat$effects), cfg$expected$quantitative_estimates,
  nrow(dat$effects) == cfg$expected$quantitative_estimates,
  "Quantitative estimates changed from the frozen extraction."
)
add_check(
  "outcome_definition_count", "ERROR", nrow(dat$definitions), cfg$expected$outcome_definitions,
  nrow(dat$definitions) == cfg$expected$outcome_definitions,
  "Outcome definitions changed from the frozen extraction."
)
add_check(
  "unique_review_ids", "ERROR", n_distinct(dat$reviews$master_id), nrow(dat$reviews),
  n_distinct(dat$reviews$master_id) == nrow(dat$reviews),
  "Review IDs must be unique."
)
add_check(
  "review_id_coverage", "ERROR",
  length(intersect(included_ids, dat$reviews$master_id)), length(included_ids),
  setequal(included_ids, dat$reviews$master_id),
  "Eligibility and review-characteristics IDs must match."
)
add_check(
  "never_pool_across_reviews", "ERROR",
  paste(sort(unique(norm_token(dat$effects$pool_across_reviews))), collapse = ";"), "NO",
  all(norm_token(dat$effects$pool_across_reviews) == "NO"),
  "Every quantitative row must prohibit pooling across reviews."
)
add_check(
  "ft012_accepted_source_status_explicit", "ERROR",
  paste(dat$report_status$status[dat$report_status$master_id == "FT-012"], collapse = ";"),
  "ACCEPTED_PEER_REVIEWED_FINAL_FORMAT_PENDING",
  "FT-012" %in% included_ids &&
    any(dat$report_status$master_id == "FT-012" & dat$report_status$status == "ACCEPTED_PEER_REVIEWED_FINAL_FORMAT_PENDING"),
  "FT-012 policy must remain explicit and auditable."
)

add_check(
  "publication_cca_c", "ERROR", publication_recomputed$c, cfg$expected$publication_cca_reviews,
  publication_recomputed$c == cfg$expected$publication_cca_reviews,
  "Publication-level CCA review denominator must reproduce."
)
add_check(
  "publication_cca_N", "ERROR", publication_recomputed$N, cfg$expected$publication_cca_occurrences,
  publication_recomputed$N == cfg$expected$publication_cca_occurrences,
  "Publication-level occurrence count must reproduce."
)
add_check(
  "publication_cca_r", "ERROR", publication_recomputed$r, cfg$expected$publication_cca_unique,
  publication_recomputed$r == cfg$expected$publication_cca_unique,
  "Publication-level unique-publication count must reproduce."
)
add_check(
  "publication_cca_value", "ERROR",
  signif(publication_recomputed$cca, 15), signif(cfg$expected$publication_cca, 15),
  isTRUE(all.equal(publication_recomputed$cca, cfg$expected$publication_cca, tolerance = 1e-12)),
  "Publication-level CCA must reproduce exactly within numerical tolerance."
)
add_check(
  "cohort_cca_c", "ERROR", cohort_recomputed$c, cfg$expected$cohort_cca_reviews,
  cohort_recomputed$c == cfg$expected$cohort_cca_reviews,
  "Cohort-level CCA review denominator must reproduce."
)
add_check(
  "cohort_cca_N", "ERROR", cohort_recomputed$N, cfg$expected$cohort_cca_occurrences,
  cohort_recomputed$N == cfg$expected$cohort_cca_occurrences,
  "Cohort-level occurrence count must reproduce."
)
add_check(
  "cohort_cca_r", "ERROR", cohort_recomputed$r, cfg$expected$cohort_cca_unique,
  cohort_recomputed$r == cfg$expected$cohort_cca_unique,
  "Cohort-level unique-family count must reproduce."
)
add_check(
  "cohort_cca_value", "ERROR",
  signif(cohort_recomputed$cca, 15), signif(cfg$expected$cohort_cca, 15),
  isTRUE(all.equal(cohort_recomputed$cca, cfg$expected$cohort_cca, tolerance = 1e-12)),
  "Cohort-level CCA must reproduce exactly within numerical tolerance."
)
add_check(
  "cohort_source_summary_duplicate_occurrence", "WARNING",
  source_cohort_N, cohort_recomputed$N,
  isTRUE(all.equal(source_cohort_N, cohort_recomputed$N)),
  paste(
    "The source CCA summary counts both SINPHONI-2 companion publications within FT-002",
    "as separate occurrences. The binary review-by-cohort matrix correctly counts one",
    "FT-002 × SINPHONI_2 cell, so the recomputed N=579 and CCA are authoritative."
  )
)

add_check(
  "quality_reviewer_coverage", "ERROR", nrow(dat$quality_reviews), cfg$expected$included_reviews,
  setequal(dat$quality_reviews$master_id, included_ids),
  "Both independent quality assessments must cover all included reviews."
)

pending_robis_queue <- dat$robis_domain_queue |>
  filter(is.na(final_consensus) | trimws(final_consensus) == "" | norm_token(status) == "PENDING") |>
  nrow()
pending_robis_final_columns <- dat$quality_reviews |>
  select(d1_consensus, d2_consensus, d3_consensus, d4_consensus, overall_consensus) |>
  summarise(across(everything(), ~ sum(is.na(norm_token(.x))))) |>
  unlist(use.names = FALSE) |>
  sum()
pending_robis <- pending_robis_queue + pending_robis_final_columns
pending_amstar_scope <- dat$amstar_summary |>
  filter(
    (
      norm_token(applicability_agreement) == "DISAGREE" &
        (is.na(final_applicability) | trimws(final_applicability) == "")
    ) |
      (
        norm_token(confidence_agreement) == "DISAGREE" &
          (is.na(final_confidence) | trimws(final_confidence) == "")
      )
  ) |>
  nrow()
pending_amstar_applicability <- dat$amstar_summary |>
  filter(
    norm_token(applicability_agreement) == "DISAGREE",
    is.na(final_applicability) | trimws(final_applicability) == ""
  ) |>
  nrow()
pending_amstar_confidence <- dat$amstar_summary |>
  filter(
    norm_token(confidence_agreement) == "DISAGREE",
    is.na(final_confidence) | trimws(final_confidence) == ""
  ) |>
  nrow()
pending_amstar_items <- dat$amstar_items |>
  filter(norm_token(agreement) == "DISAGREE") |>
  filter(is.na(final_consensus) | trimws(final_consensus) == "") |>
  nrow()

add_check(
  "robis_final_consensus", "WARNING", pending_robis, 0, pending_robis == 0,
  paste(
    "Final ROBIS columns and every queued disagreement must be resolved before",
    "quality outputs can be labelled final consensus."
  )
)
add_check(
  "amstar_scope_confidence_consensus", "WARNING", pending_amstar_scope, 0,
  pending_amstar_scope == 0,
  "Applicable AMSTAR 2 reviews still require final scope/confidence consensus."
)
add_check(
  "amstar_item_consensus", "WARNING", pending_amstar_items, 0,
  pending_amstar_items == 0,
  "Applicable AMSTAR 2 item disagreements still require final consensus."
)

unverified_statuses <- unique(norm_token(dat$candidate_queue$status))
add_check(
  "candidate_queue_excluded", "ERROR",
  paste(unverified_statuses, collapse = ";"), "UNVERIFIED_DO_NOT_USE_IN_CCA",
  all(unverified_statuses == "UNVERIFIED_DO_NOT_USE_IN_CCA"),
  "The candidate queue must remain outside every CCA calculation."
)

validation <- bind_rows(checks)
write_data(validation, "validation_checks.csv", cfg)

quality_consensus_complete <- all(validation$passed[validation$check_id %in% c(
  "robis_final_consensus", "amstar_scope_confidence_consensus", "amstar_item_consensus"
)])

run_state <- list(
  included_ids = included_ids,
  publication_recomputed = publication_recomputed,
  cohort_recomputed = cohort_recomputed,
  source_cohort_summary = list(N = source_cohort_N, cca = source_cohort_cca),
  quality_consensus_complete = quality_consensus_complete,
  pending_robis = pending_robis,
  pending_amstar_scope = pending_amstar_scope,
  pending_amstar_applicability = pending_amstar_applicability,
  pending_amstar_confidence = pending_amstar_confidence,
  pending_amstar_items = pending_amstar_items,
  source_manifest = manifest
)

saveRDS(dat, file.path(cfg$paths$results, "data", "analysis_data.rds"))
saveRDS(run_state, file.path(cfg$paths$results, "data", "run_state.rds"))

write_data(dat$reviews, "reviews_analysis_ready.csv", cfg)
write_data(dat$findings, "predictor_findings_analysis_ready.csv", cfg)
write_data(dat$effects, "quantitative_estimates_analysis_ready.csv", cfg)
write_data(dat$definitions, "outcome_definitions_analysis_ready.csv", cfg)
write_data(dat$method_facts, "method_facts_analysis_ready.csv", cfg)
write_data(dat$occurrence_long, "overlap_occurrences_analysis_ready.csv", cfg)

critical_failures <- validation |>
  filter(severity == "ERROR", !passed)

if (nrow(critical_failures) > 0L && isTRUE(cfg$analysis$strict_input_validation)) {
  stop(
    "Critical input validation failed: ",
    paste(critical_failures$check_id, collapse = ", "),
    ". Inspect results/data/validation_checks.csv.",
    call. = FALSE
  )
}

message(
  "Imported ", length(included_ids), " reviews, ", nrow(dat$findings),
  " findings, ", nrow(dat$effects), " quantitative estimates, and ",
  nrow(dat$definitions), " response definitions."
)

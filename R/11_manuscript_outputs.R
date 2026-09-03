message("Compiling manuscript-ready numbers, run summary, and report...")

dat <- readRDS(file.path(cfg$paths$results, "data", "analysis_data.rds"))
run_state <- readRDS(file.path(cfg$paths$results, "data", "run_state.rds"))
anchor_effects <- readRDS(file.path(cfg$paths$results, "data", "anchor_effects.rds"))
supporting_effects <- readRDS(file.path(cfg$paths$results, "data", "supporting_effects.rds"))
quality_flags <- readRDS(file.path(cfg$paths$results, "data", "quality_flags.rds"))
overlap_summary <- readRDS(file.path(cfg$paths$results, "data", "overlap_summary.rds"))
probability_translation <- readRDS(file.path(cfg$paths$results, "data", "probability_translation.rds"))
definitions_derived <- readr::read_csv(
  file.path(cfg$paths$results, "data", "outcome_definitions_derived_flags.csv"),
  show_col_types = FALSE
)

included_n <- dat$eligibility |> filter(norm_token(decision) == "INCLUDE") |> nrow()
excluded_n <- dat$eligibility |> filter(norm_token(decision) == "EXCLUDE") |> nrow()
unavailable_n <- dat$eligibility |> filter(norm_token(decision) == "EXCLUDE_UNAVAILABLE") |> nrow()

definition_class_counts <- dat$definitions |>
  count(definition_class = norm_token(definition_class), name = "n")

get_def_count <- function(label) {
  out <- definition_class_counts$n[definition_class_counts$definition_class == label]
  if (length(out) == 0L) 0L else out[[1]]
}

robis_a_high <- sum(norm_token(dat$quality_reviews$overall_a) == "HIGH")
robis_b_high <- sum(norm_token(dat$quality_reviews$overall_b) == "HIGH")
amstar_a_applicable <- sum(norm_token(dat$amstar_summary$applicability_a) != "OUTSIDE_INTENDED_SCOPE")
amstar_b_applicable <- sum(norm_token(dat$amstar_summary$applicability_b) != "OUTSIDE_INTENDED_SCOPE")
robis_final_high <- sum(quality_flags$robis_final_high %in% TRUE)
robis_final_unclear <- sum(quality_flags$robis_final_unclear %in% TRUE)
robis_final_low <- sum(quality_flags$robis_final_low %in% TRUE)
amstar_final_applicable <- sum(
  !is.na(quality_flags$amstar_consensus_applicability) &
    !quality_flags$amstar_consensus_applicability %in%
      c("OUTSIDE_INTENDED_SCOPE", "NOT_APPLICABLE_OUTSIDE_SCOPE")
)
amstar_final_critical_low <- sum(
  quality_flags$amstar_consensus_confidence == "CRITICALLY_LOW",
  na.rm = TRUE
)
amstar_final_low <- sum(
  quality_flags$amstar_consensus_confidence == "LOW",
  na.rm = TRUE
)

mcid_norm <- norm_token(dat$definitions$mcid_source)
mcid_explicitly_unreported_n <- sum(mcid_norm %in% c("NR", "GENERALLY_NOT_REPORTED"))
patient_reported_n <- sum(norm_token(dat$definitions$patient_reported) == "YES")
source_label_validated_n <- sum(definitions_derived$source_label_validated_threshold)
explicit_validated_response_n <- sum(definitions_derived$threshold_validated_for_inph_shunt_response)
general_instrument_validation_n <- sum(definitions_derived$general_instrument_validation_only)
mcid_specific_response_n <- sum(definitions_derived$mcid_source_specific_to_inph_shunt_response)
publication_duplication <- overlap_summary$duplication_burden |>
  filter(unit == "Publication")
cohort_duplication <- overlap_summary$duplication_burden |>
  filter(unit == "Conservatively linked cohort family")

manuscript_numbers <- tribble(
  ~metric, ~value, ~denominator, ~formatted, ~interpretation_status,
  "Included reviews", included_n, NA_real_, as.character(included_n), "FINAL_ELIGIBILITY",
  "Excluded after full text", excluded_n, NA_real_, as.character(excluded_n), "FINAL_ELIGIBILITY",
  "Unavailable full texts", unavailable_n, NA_real_, as.character(unavailable_n), "FINAL_ELIGIBILITY",
  "Structured findings", nrow(dat$findings), NA_real_, as.character(nrow(dat$findings)), "FINAL_EXTRACTION",
  "Quantitative estimates", nrow(dat$effects), NA_real_, as.character(nrow(dat$effects)), "FINAL_EXTRACTION",
  "Anchor estimates", nrow(anchor_effects), nrow(dat$effects), paste0(nrow(anchor_effects), "/", nrow(dat$effects)), "NO_POOL_ACROSS_REVIEWS",
  "Supporting estimates", nrow(supporting_effects), nrow(dat$effects), paste0(nrow(supporting_effects), "/", nrow(dat$effects)), "CONCORDANCE_ONLY",
  "Own-cohort hybrid estimates", sum(norm_token(dat$effects$umbrella_role) == "EXCLUDE_FROM_UMBRELLA_POOL"), nrow(dat$effects), paste0(sum(norm_token(dat$effects$umbrella_role) == "EXCLUDE_FROM_UMBRELLA_POOL"), "/", nrow(dat$effects)), "EXCLUDED_FROM_UMBRELLA_POOL",
  "Outcome definitions", nrow(dat$definitions), NA_real_, as.character(nrow(dat$definitions)), "FINAL_EXTRACTION",
  "Author-defined response definitions", get_def_count("AUTHOR_DEFINED"), nrow(dat$definitions), scales::percent(get_def_count("AUTHOR_DEFINED") / nrow(dat$definitions), accuracy = 0.1), "DEFINITION_AUDIT",
  "Statistical-change-only definitions", get_def_count("STATISTICAL_CHANGE_ONLY"), nrow(dat$definitions), scales::percent(get_def_count("STATISTICAL_CHANGE_ONLY") / nrow(dat$definitions), accuracy = 0.1), "DEFINITION_AUDIT",
  "Source-labelled validated-threshold definitions", source_label_validated_n, nrow(dat$definitions), scales::percent(source_label_validated_n / nrow(dat$definitions), accuracy = 0.1), "SOURCE_LABEL_RETAINED_NOT_EQUIVALENT_TO_INPH_VALIDATION",
  "Thresholds explicitly validated for iNPH shunt response", explicit_validated_response_n, nrow(dat$definitions), scales::percent(explicit_validated_response_n / nrow(dat$definitions), accuracy = 0.1), "CONSERVATIVE_DEFINITION_AUDIT",
  "General instrument validation only", general_instrument_validation_n, nrow(dat$definitions), scales::percent(general_instrument_validation_n / nrow(dat$definitions), accuracy = 0.1), "NOT_SHUNT_RESPONSE_THRESHOLD_VALIDATION",
  "Clearly patient-reported definitions", patient_reported_n, nrow(dat$definitions), scales::percent(patient_reported_n / nrow(dat$definitions), accuracy = 0.1), "DEFINITION_AUDIT",
  "MCID explicitly unreported", mcid_explicitly_unreported_n, nrow(dat$definitions), scales::percent(mcid_explicitly_unreported_n / nrow(dat$definitions), accuracy = 0.1), "DEFINITION_AUDIT",
  "MCID sources specific to iNPH shunt response", mcid_specific_response_n, nrow(dat$definitions), scales::percent(mcid_specific_response_n / nrow(dat$definitions), accuracy = 0.1), "CONSERVATIVE_DEFINITION_AUDIT",
  "ROBIS high — Reviewer 1", robis_a_high, included_n, paste0(robis_a_high, "/", included_n), "INDEPENDENT_PRECONSENSUS",
  "ROBIS high — Reviewer 2", robis_b_high, included_n, paste0(robis_b_high, "/", included_n), "INDEPENDENT_PRECONSENSUS",
  "ROBIS high — final author consensus", robis_final_high, included_n, paste0(robis_final_high, "/", included_n), "FINAL_AUTHOR_CONSENSUS",
  "ROBIS unclear — final author consensus", robis_final_unclear, included_n, paste0(robis_final_unclear, "/", included_n), "FINAL_AUTHOR_CONSENSUS",
  "ROBIS low — final author consensus", robis_final_low, included_n, paste0(robis_final_low, "/", included_n), "FINAL_AUTHOR_CONSENSUS",
  "AMSTAR 2 applicable — Reviewer 1", amstar_a_applicable, included_n, paste0(amstar_a_applicable, "/", included_n), "SCOPE_SPECIFIC",
  "AMSTAR 2 applicable — Reviewer 2", amstar_b_applicable, included_n, paste0(amstar_b_applicable, "/", included_n), "SCOPE_SPECIFIC",
  "AMSTAR 2 applicable — final author consensus", amstar_final_applicable, included_n, paste0(amstar_final_applicable, "/", included_n), "FINAL_AUTHOR_CONSENSUS",
  "AMSTAR 2 critically low — final author consensus", amstar_final_critical_low, amstar_final_applicable, paste0(amstar_final_critical_low, "/", amstar_final_applicable), "FINAL_AUTHOR_CONSENSUS",
  "AMSTAR 2 low — final author consensus", amstar_final_low, amstar_final_applicable, paste0(amstar_final_low, "/", amstar_final_applicable), "FINAL_AUTHOR_CONSENSUS",
  "Publication CCA", overlap_summary$global$cca[overlap_summary$global$unit == "Publication"], overlap_summary$global$included_reviews[overlap_summary$global$unit == "Publication"], scales::percent(overlap_summary$global$cca[overlap_summary$global$unit == "Publication"], accuracy = 0.01), "PARTIAL_COVERAGE",
  "Cohort-family CCA", overlap_summary$global$cca[overlap_summary$global$unit == "Conservatively linked cohort family"], overlap_summary$global$included_reviews[overlap_summary$global$unit == "Conservatively linked cohort family"], scales::percent(overlap_summary$global$cca[overlap_summary$global$unit == "Conservatively linked cohort family"], accuracy = 0.01), "PARTIAL_COVERAGE",
  "Unique publications repeated across reviews", publication_duplication$units_repeated_in_multiple_reviews, publication_duplication$unique_units, scales::percent(publication_duplication$proportion_unique_units_repeated, accuracy = 0.1), "INTUITIVE_DUPLICATION_BURDEN_PARTIAL_COVERAGE",
  "Publication occurrences beyond first", publication_duplication$occurrences_beyond_first, publication_duplication$total_occurrences, scales::percent(publication_duplication$proportion_occurrences_beyond_first, accuracy = 0.1), "INTUITIVE_DUPLICATION_BURDEN_PARTIAL_COVERAGE",
  "Cohort families repeated across reviews", cohort_duplication$units_repeated_in_multiple_reviews, cohort_duplication$unique_units, scales::percent(cohort_duplication$proportion_unique_units_repeated, accuracy = 0.1), "INTUITIVE_DUPLICATION_BURDEN_PARTIAL_COVERAGE",
  "Cohort occurrences beyond first", cohort_duplication$occurrences_beyond_first, cohort_duplication$total_occurrences, scales::percent(cohort_duplication$proportion_occurrences_beyond_first, accuracy = 0.1), "INTUITIVE_DUPLICATION_BURDEN_PARTIAL_COVERAGE",
  "Rout thresholds with NPV/prevalence inconsistency", sum(str_detect(probability_translation$rout_npv_audit$audit_status, "INTERNALLY_INCONSISTENT")), nrow(probability_translation$rout_npv_audit), paste0(sum(str_detect(probability_translation$rout_npv_audit$audit_status, "INTERNALLY_INCONSISTENT")), "/", nrow(probability_translation$rout_npv_audit)), "SOURCE_VALUES_RETAINED_AUDIT_FLAGGED"
)

write_table(manuscript_numbers, "manuscript_numbers.csv", cfg)

dta_decision <- readr::read_csv(
  file.path(cfg$paths$results, "tables", "dta_meta_analysis_decision.csv"),
  show_col_types = FALSE
)

run_summary_object <- list(
  project = cfg$project,
  completed_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
  validation = list(
    critical_checks_passed = TRUE,
    quality_consensus_complete = run_state$quality_consensus_complete,
    pending_robis_cells = run_state$pending_robis,
    pending_amstar_applicability_rows = run_state$pending_amstar_applicability,
    pending_amstar_confidence_rows = run_state$pending_amstar_confidence,
    pending_amstar_summary_union_rows = run_state$pending_amstar_scope,
    pending_amstar_item_cells = run_state$pending_amstar_items
  ),
  ft012_policy = list(
    main_analysis = "Accepted peer-reviewed Frontiers source retained",
    quantitative_source = "Official accepted abstract updated 2026-09-01",
    structural_source = "Complete preprint pending final formatted accepted full text",
    single_anchor_robustness = "Exclude FT-012 without calling this a peer-reviewed-only restriction"
  ),
  registration = list(
    registry = "PROSPERO",
    id = cfg$project$prospero,
    timing = "retrospective"
  ),
  overlap = list(
    publication_cca = overlap_summary$global$cca[overlap_summary$global$unit == "Publication"],
    publication_coverage_reviews = overlap_summary$global$included_reviews[overlap_summary$global$unit == "Publication"],
    cohort_cca = overlap_summary$global$cca[overlap_summary$global$unit == "Conservatively linked cohort family"],
    cohort_coverage_reviews = overlap_summary$global$included_reviews[overlap_summary$global$unit == "Conservatively linked cohort family"]
  ),
  outcome_validity = list(
    explicitly_validated_inph_shunt_response_thresholds = explicit_validated_response_n,
    total_definitions = nrow(dat$definitions),
    response_specific_mcid_sources = mcid_specific_response_n
  ),
  source_consistency = list(
    rout_npv_rows_audited = nrow(probability_translation$rout_npv_audit),
    rout_npv_rows_flagged = sum(str_detect(probability_translation$rout_npv_audit$audit_status, "INTERNALLY_INCONSISTENT")),
    published_values_overwritten = FALSE
  ),
  author_overlap_management = list(
    current_team_authored_included_reviews = c("FT-003", "FT-011"),
    current_first_author_overlap = "FT-003",
    joint_exclusion_anchor_microcells_retained = 53,
    joint_exclusion_high_level_cells_retained = 10,
    note = paste(
      "FT-011 contributes supporting estimates only; excluding both therefore",
      "matches FT-003 anchor retention."
    )
  ),
  meta_analysis_decision = as.list(dta_decision[1, ]),
  principal_rule = "No pooling across reviews; one anchor review per clinical microcell.",
  public_release_scope = list(
    quality_source = "Final human author-consensus workbook",
    nonanalytic_development_comparison_included = FALSE
  )
)

jsonlite::write_json(
  run_summary_object,
  file.path(cfg$paths$results, "analysis_run_summary.json"),
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

summary_lines <- c(
  paste0("# ", cfg$project$short_name, " — analysis run summary"),
  "",
  paste0("- Base data freeze: `", cfg$project$base_freeze, "`; targeted FT-012 source update: `", cfg$project$targeted_update, "`"),
  paste0("- Analysis version: `", cfg$project$analysis_version, "`"),
  paste0("- Registration: **PROSPERO ", cfg$project$prospero, ", retrospective**"),
  paste0("- Included reviews: **", included_n, "**"),
  paste0("- Structured findings: **", nrow(dat$findings), "**"),
  "- Current-team-authored included reviews: **FT-003 (Caio Arruda Maciel, first author) and FT-011 (Kim Wouters and Fernando Campos Gomes Pinto, coauthors)**. Joint exclusion retains **53/64 anchor microcells and 10/11 high-level cells**.",
  paste0("- Quantitative estimates: **", nrow(dat$effects), "**; anchor estimates: **", nrow(anchor_effects), "**"),
  paste0("- Outcome definitions: **", nrow(dat$definitions), "**"),
  paste0("- Thresholds explicitly validated for iNPH shunt response: **", explicit_validated_response_n, "/", nrow(dat$definitions), "**"),
  paste0("- MCID sources specific to iNPH shunt response: **", mcid_specific_response_n, "/", nrow(dat$definitions), "**"),
  paste0("- Publication-level CCA: **", scales::percent(overlap_summary$global$cca[1], accuracy = 0.01), "** with coverage **", overlap_summary$global$included_reviews[1], "/", included_n, " reviews**"),
  paste0("- Cohort-family CCA: **", scales::percent(overlap_summary$global$cca[2], accuracy = 0.01), "** with coverage **", overlap_summary$global$included_reviews[2], "/", included_n, " reviews**"),
  paste0("- Quality mode: **", ifelse(run_state$quality_consensus_complete, "final consensus", "dual-reviewer, consensus pending"), "**"),
  paste0("- Final ROBIS consensus: **", robis_final_high, " high, ", robis_final_unclear, " unclear, and ", robis_final_low, " low**"),
  paste0("- Final AMSTAR 2 consensus: **", amstar_final_applicable, " applicable; ", amstar_final_critical_low, " critically low and ", amstar_final_low, " low**"),
  paste0("- Optional DTA module: **", dta_decision$status[1], "**"),
  "",
  "## FT-012 source update",
  "",
  "FT-012 was accepted after peer review in Frontiers in Neurology (doi:10.3389/fneur.2026.1908328). Its bibliographic status and pooled accuracy values were updated from the official accepted abstract. The final formatted full text remained pending on 1 September 2026; full-text-only structural extraction and the 14-study overlap list therefore retain the complete preprint as their transparent source. The accepted abstract reports 15 studies overall. FT-012 remains in the main analysis. Its exclusion is retained only as a single-anchor source-dependence analysis, not as a peer-reviewed-only restriction.",
  "",
  "## Interpretation safeguards",
  "",
  "- Review-level estimates were not pooled across reviews.",
  "- Participant denominators were not summed across reviews.",
  "- Unverified candidate references were excluded from CCA.",
  "- A lost anchor in sensitivity analysis was reported as fragility; no unmatched estimate was substituted.",
  "- Diagnostic bivariate/HSROC modelling requires verified primary-study 2×2 data.",
  "- Published Rout NPV values are retained but flagged where they are inconsistent with the stated 65% response prevalence.",
  "",
  "## Main scientific interpretation",
  "",
  "Positive supplemental tests may increase confidence in shunt selection, but current review-level evidence does not establish any isolated negative drainage test, absent imaging marker, biomarker, or comorbidity as a validated stand-alone exclusion rule for clinically important shunt benefit."
)
writeLines(summary_lines, file.path(cfg$paths$results, "RUN_SUMMARY.md"), useBytes = TRUE)

capture.output(
  sessionInfo(),
  file = file.path(cfg$paths$results, "logs", "session_info.txt")
)

if (isTRUE(cfg$analysis$render_html_report)) {
  if (requireNamespace("rmarkdown", quietly = TRUE)) {
    report_result <- tryCatch({
      rmarkdown::render(
        input = "report/analysis_report.Rmd",
        output_file = "analysis_report.html",
        output_dir = normalizePath(cfg$paths$results, mustWork = FALSE),
        knit_root_dir = normalizePath(".", mustWork = TRUE),
        quiet = TRUE,
        envir = new.env(parent = globalenv())
      )
      "REPORT_RENDERED"
    }, error = function(e) {
      append_log(conditionMessage(e), "report_render_error.log", cfg)
      "REPORT_RENDER_FAILED_SEE_LOG"
    })
  } else {
    report_result <- "REPORT_NOT_RENDERED_RMARKDOWN_NOT_INSTALLED"
  }
} else {
  report_result <- "REPORT_RENDER_DISABLED"
}

writeLines(report_result, file.path(cfg$paths$results, "logs", "report_status.txt"))

result_files <- list.files(cfg$paths$results, recursive = TRUE, full.names = TRUE)
result_files <- result_files[file.info(result_files)$isdir %in% FALSE]
result_inventory <- tibble(
  relative_path = sub(paste0("^", cfg$paths$results, "/?"), "", result_files),
  size_bytes = file.info(result_files)$size,
  sha256 = vapply(result_files, digest::digest, character(1), algo = "sha256", file = TRUE)
) |>
  arrange(relative_path)
write_data(result_inventory, "result_file_inventory.csv", cfg)

message("Manuscript-ready outputs compiled. Report status: ", report_result)

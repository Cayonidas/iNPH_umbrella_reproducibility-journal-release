options(stringsAsFactors = FALSE)

if (!file.exists("config.yml") || !dir.exists("results")) {
  stop("Run this script from the repository root after source('run_all.R').", call. = FALSE)
}

checks <- list()
add_check <- function(name, observed, expected, tolerance = NULL) {
  passed <- if (is.null(tolerance)) {
    identical(as.character(observed), as.character(expected))
  } else {
    isTRUE(all.equal(as.numeric(observed), as.numeric(expected), tolerance = tolerance))
  }
  checks[[length(checks) + 1L]] <<- data.frame(
    check = name,
    observed = paste(observed, collapse = ";"),
    expected = paste(expected, collapse = ";"),
    passed = passed,
    stringsAsFactors = FALSE
  )
}

read_chr <- function(path) {
  readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = readr::cols(.default = readr::col_character()),
    na = character()
  )
}

required_files <- c(
  "results/analysis_run_summary.json",
  "results/RUN_SUMMARY.md",
  "results/data/reviews_analysis_ready.csv",
  "results/data/predictor_findings_analysis_ready.csv",
  "results/data/quantitative_estimates_analysis_ready.csv",
  "results/data/outcome_definitions_analysis_ready.csv",
  "results/tables/anchor_selection_candidate_audit.csv",
  "results/tables/quality_flags_by_review.csv",
  "results/tables/overlap_global_cca_with_coverage.csv",
  "results/tables/manuscript_numbers.csv",
  "results/tables/sensitivity_scenario_summary.csv",
  "results/tables/sensitivity_single_review_dependency.csv",
  "results/tables/dta_meta_analysis_decision.csv"
)

missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0L) {
  stop("Missing required outputs: ", paste(missing_files, collapse = ", "), call. = FALSE)
}

reviews <- read_chr("results/data/reviews_analysis_ready.csv")
findings <- read_chr("results/data/predictor_findings_analysis_ready.csv")
effects <- read_chr("results/data/quantitative_estimates_analysis_ready.csv")
definitions <- read_chr("results/data/outcome_definitions_analysis_ready.csv")
anchors <- read_chr("results/tables/anchor_selection_candidate_audit.csv")
quality <- read_chr("results/tables/quality_flags_by_review.csv")
overlap <- read_chr("results/tables/overlap_global_cca_with_coverage.csv")
numbers <- read_chr("results/tables/manuscript_numbers.csv")
sensitivity <- read_chr("results/tables/sensitivity_scenario_summary.csv")
dependency <- read_chr("results/tables/sensitivity_single_review_dependency.csv")
dta <- read_chr("results/tables/dta_meta_analysis_decision.csv")

add_check("included reviews", nrow(reviews), 41)
add_check("structured findings", nrow(findings), 192)
add_check("quantitative estimates", nrow(effects), 155)
add_check("response definitions", nrow(definitions), 139)
add_check("selected anchors", sum(toupper(anchors$selected_as_anchor) == "TRUE"), 64)
add_check("ROBIS high", sum(toupper(quality$robis_final_high) == "TRUE"), 32)
add_check("ROBIS unclear", sum(toupper(quality$robis_final_unclear) == "TRUE"), 4)
add_check("ROBIS low", sum(toupper(quality$robis_final_low) == "TRUE"), 5)
add_check("AMSTAR 2 critically low", sum(quality$amstar_consensus_confidence == "CRITICALLY_LOW"), 14)
add_check("AMSTAR 2 low", sum(quality$amstar_consensus_confidence == "LOW"), 1)

publication_row <- overlap[overlap$unit == "Publication", ]
cohort_row <- overlap[overlap$unit == "Conservatively linked cohort family", ]
add_check("publication CCA reviews", publication_row$included_reviews, 19)
add_check("publication CCA N", publication_row$N, 545)
add_check("publication CCA r", publication_row$r, 429)
add_check("publication CCA", publication_row$cca, 0.015022015022015, tolerance = 1e-12)
add_check("cohort CCA reviews", cohort_row$included_reviews, 20)
add_check("cohort CCA N", cohort_row$N, 579)
add_check("cohort CCA r", cohort_row$r, 452)
add_check("cohort CCA", cohort_row$cca, 0.014788076385654, tolerance = 1e-12)

joint <- sensitivity[sensitivity$scenario == "exclude_all_current_team_authored_reviews", ]
add_check("joint author-overlap retained microcells", joint$microcells_retained, 53)
add_check("joint author-overlap retained high-level cells", joint$high_level_cells_retained, 10)
add_check(
  "single-anchor-dependent high-level cells",
  sum(toupper(dependency$wholly_dependent_on_one_review) == "TRUE"),
  10
)
add_check("optional DTA status", dta$status[[1]], "NOT_READY_NO_PRIMARY_2X2_DATA")

summary_json <- jsonlite::read_json("results/analysis_run_summary.json", simplifyVector = TRUE)
add_check("critical runtime checks", summary_json$validation$critical_checks_passed, TRUE)
add_check("quality consensus complete", summary_json$validation$quality_consensus_complete, TRUE)
add_check("PROSPERO identifier", summary_json$registration$id, "CRD420261494316")

results <- do.call(rbind, checks)
dir.create("results/logs", recursive = TRUE, showWarnings = FALSE)
write.csv(results, "results/logs/release_validation.csv", row.names = FALSE, na = "")

for (i in seq_len(nrow(results))) {
  message(if (results$passed[[i]]) "PASS  " else "FAIL  ", results$check[[i]],
          " | observed=", results$observed[[i]], " | expected=", results$expected[[i]])
}

if (!all(results$passed)) {
  stop("Release validation failed. Inspect results/logs/release_validation.csv.", call. = FALSE)
}

message("OVERALL: PASS")


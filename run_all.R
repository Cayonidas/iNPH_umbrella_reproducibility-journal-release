options(stringsAsFactors = FALSE, warn = 1)

if (!file.exists("config.yml")) {
  stop(
    "Run this script from the project root (the folder containing config.yml). ",
    "In RStudio, open iNPH_umbrella_analysis.Rproj first.",
    call. = FALSE
  )
}

existing_results <- if (dir.exists("results")) {
  list.files("results", all.files = TRUE, no.. = TRUE, recursive = TRUE)
} else {
  character()
}
if (length(existing_results) > 0L) {
  stop(
    "The results/ directory is not empty. Move or remove it before a clean reproducibility run; ",
    "the pipeline will not mix outputs from different runs.",
    call. = FALSE
  )
}

scripts <- c(
  "R/00_utils.R",
  "R/01_import_validate.R",
  "R/02_evidence_map.R",
  "R/03_quality_appraisal.R",
  "R/04_response_definitions.R",
  "R/05_overlap.R",
  "R/06_anchor_selection.R",
  "R/07_effect_displays.R",
  "R/08_clinical_matrix.R",
  "R/09_sensitivity.R",
  "R/10_dta_module.R",
  "R/12_probability_translation.R",
  "R/13_credibility_concordance.R",
  "R/11_manuscript_outputs.R"
)

started_at <- Sys.time()

for (script in scripts) {
  message("\n==> ", script)
  source(script, local = globalenv(), encoding = "UTF-8")
}

elapsed <- difftime(Sys.time(), started_at, units = "secs")
message("\nPipeline completed in ", round(as.numeric(elapsed), 1), " seconds.")
message("Open results/analysis_report.html or results/RUN_SUMMARY.md.")

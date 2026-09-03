test_that("CCA reproduces the adjudicated publication-level value", {
  out <- cca_stat(c = 19, N = 545, r = 429)
  expect_equal(out$cca, 0.015022015022015, tolerance = 1e-12)
})

test_that("CCA is undefined for a single review", {
  out <- cca_stat(c = 1, N = 10, r = 10)
  expect_true(is.na(out$cca))
})

test_that("Kappa is one under exact agreement", {
  x <- c("LOW", "UNCLEAR", "HIGH", "LOW")
  expect_equal(safe_kappa(x, x, levels = c("LOW", "UNCLEAR", "HIGH")), 1)
})

test_that("FT identifiers are recovered from free text", {
  expect_equal(extract_ft_ids("FT-012; FT-031 and FT-043"), c("FT-012", "FT-031", "FT-043"))
  expect_equal(count_ft_ids(c(NA, "", "FT-021; FT-027")), c(0L, 0L, 2L))
})

test_that("probability translation follows Bayes odds", {
  expect_equal(post_test_probability(0.50, 2), 2 / 3)
  expect_equal(post_test_probability(0.65, 1), 0.65)
})

test_that("predictive-value audit recovers implied prevalence", {
  implied <- implied_prevalence_from_npv(0.769, 0.340, 0.262)
  expect_equal(implied, 0.80, tolerance = 0.015)
  recalculated <- predictive_values(0.769, 0.340, 0.65)
  expect_gt(recalculated$npv, 0.40)
})

test_that("targeted AI overlap is exact and very high", {
  out <- cca_stat(c = 2, N = 10, r = 8)
  expect_equal(out$cca, 0.25)
})

test_that("documented predictor-finding ID schemas are accepted", {
  expect_true(valid_finding_id("F-0001"))
  expect_true(valid_finding_id("F-019-SURV"))
  expect_false(valid_finding_id("F-019 SURV"))
  expect_false(valid_finding_id("finding-1"))
})

test_that("iNPH acronym variants map to one stable column name", {
  expect_equal(clean_names_base("Validated_for_iNPH"), "validated_for_inph")
  expect_equal(clean_names_base("Validated_for_i_NPH"), "validated_for_inph")
  expect_equal(clean_names_base("Validated_for_inph"), "validated_for_inph")
})

test_that("RoB acronym variants map to one stable column name", {
  expect_equal(clean_names_base("Primary_RoB_assessed"), "primary_rob_assessed")
  expect_equal(clean_names_base("Primary_Ro_B_assessed"), "primary_rob_assessed")
  expect_equal(clean_names_base("Primary_rob_assessed"), "primary_rob_assessed")
  expect_equal(clean_names_base("RoB_used_in_synthesis"), "rob_used_in_synthesis")
  expect_equal(clean_names_base("Primary_study_RoB_tool"), "primary_study_rob_tool")
})

test_that("method completeness does not promote partial reporting", {
  expect_true(clearly_yes_flag("YES"))
  expect_true(clearly_yes_flag("YES; PROSPERO 2024, identifier not reported"))
  expect_false(clearly_yes_flag("YES/PARTIAL"))
  expect_false(clearly_yes_flag("PARTIAL/YES"))
  expect_false(clearly_yes_flag("LIMITED (single database)"))
  expect_false(clearly_yes_flag("NR"))
})

test_that("required input columns fail early with an informative error", {
  input <- tibble::tibble(master_id = "FT-001")
  expect_invisible(require_columns(input, "master_id", "Example"))
  expect_error(
    require_columns(input, c("master_id", "validated_for_inph"), "Outcome_Definitions"),
    "missing required column\\(s\\): validated_for_inph"
  )
})

test_that("Method_Facts schema is canonicalized before analysis", {
  raw_headers <- c(
    "Master_ID", "Protocol_before_review", "Comprehensive_search",
    "Duplicate_screening", "Duplicate_extraction", "Primary_RoB_assessed",
    "RoB_used_in_synthesis"
  )
  cleaned_headers <- clean_names_base(raw_headers)
  input <- as_tibble(setNames(
    replicate(length(cleaned_headers), character(), simplify = FALSE),
    cleaned_headers
  ))
  expect_invisible(require_columns(
    input,
    c(
      "master_id", "protocol_before_review", "comprehensive_search",
      "duplicate_screening", "duplicate_extraction", "primary_rob_assessed",
      "rob_used_in_synthesis"
    ),
    "Method_Facts"
  ))
})

test_that("explicit author consensus supersedes pair reconstruction", {
  expect_equal(
    resolve_quality_consensus(
      reviewer_a = c("UNCLEAR", "HIGH", "LOW"),
      reviewer_b = c("UNCLEAR", "LOW", "LOW"),
      adjudicated = c(NA, "HIGH", NA),
      explicit_final = c("LOW", "HIGH", "LOW"),
      prefer_explicit = TRUE
    ),
    c("LOW", "HIGH", "LOW")
  )
  expect_equal(
    resolve_quality_consensus(
      reviewer_a = "UNCLEAR",
      reviewer_b = "UNCLEAR",
      explicit_final = "LOW",
      prefer_explicit = FALSE
    ),
    "UNCLEAR"
  )
})

test_that("public release uses only the final human quality workbook", {
  config <- yaml::read_yaml("config.yml")
  expect_true(file.exists(config$paths$quality_workbook))
  expect_false(any(grepl("codex.*(robis|amstar)", names(config$paths), ignore.case = TRUE)))
  expect_setequal(
    intersect(
      readxl::excel_sheets(config$paths$quality_workbook),
      c(
        "Review_Consensus", "ROBIS_Domain_Queue", "ROBIS_Signalling",
        "AMSTAR2_Summary", "AMSTAR2_Items", "Agreement_Metrics",
        "Analysis_Readiness"
      )
    ),
    c(
      "Review_Consensus", "ROBIS_Domain_Queue", "ROBIS_Signalling",
      "AMSTAR2_Summary", "AMSTAR2_Items", "Agreement_Metrics",
      "Analysis_Readiness"
    )
  )
})

test_that("scientific AI-overlap input remains present", {
  config <- yaml::read_yaml("config.yml")
  expect_true(file.exists(config$paths$targeted_ai_overlap))
  expect_match(basename(config$paths$targeted_ai_overlap), "targeted_ai_overlap_verified")
})

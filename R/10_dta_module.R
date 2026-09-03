message("Evaluating the optional de novo diagnostic-accuracy component...")

dta_path <- cfg$paths$dta_2x2
if (!file.exists(dta_path)) {
  stop("Missing DTA input template: ", dta_path, call. = FALSE)
}

dta_raw <- readr::read_csv(
  dta_path,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character()),
  na = c("", "NA", "NR")
)
names(dta_raw) <- clean_names_base(names(dta_raw))

required_dta_columns <- c(
  "study_id", "cohort_id", "index_test", "threshold_group",
  "tp", "fp", "fn", "tn", "reference_standard", "follow_up",
  "independent_cohort", "reference_standard_compatible", "include_in_primary_dta"
)

missing_dta_columns <- setdiff(required_dta_columns, names(dta_raw))
if (length(missing_dta_columns) > 0L) {
  stop(
    "DTA template is missing required columns: ",
    paste(missing_dta_columns, collapse = ", "),
    call. = FALSE
  )
}

if (nrow(dta_raw) == 0L) {
  dta_individual <- tibble()
  dta_feasibility <- tibble(
    index_test = c("TAP_TEST", "LIT_ROUT", "ELD"),
    threshold_group = "NO_PRIMARY_DATA",
    eligible_rows = 0L,
    independent_cohorts = 0L,
    minimum_required = cfg$analysis$dta_min_independent_cohorts,
    model_enabled = cfg$analysis$dta_meta_analysis_enabled,
    status = "NOT_READY_NO_PRIMARY_2X2_DATA",
    reason = "The umbrella extraction contains review-level estimates, not verified primary-study TP/FP/FN/TN tables."
  )
  dta_global_decision <- tibble(
    component = "De novo diagnostic-accuracy meta-analysis",
    decision = "DO_NOT_RUN_YET",
    status = "NOT_READY_NO_PRIMARY_2X2_DATA",
    clinical_reason = paste(
      "A valid bivariate/HSROC model requires verified independent primary cohorts,",
      "compatible post-shunt reference standards, and complete 2x2 data."
    ),
    consequence_for_umbrella = "None. The umbrella-review analyses remain valid and complete without this optional expansion."
  )
} else {
  dta <- dta_raw |>
    mutate(
      across(c(tp, fp, fn, tn), ~ as.integer(parse_num(.x))),
      index_test = norm_token(index_test),
      threshold_group = coalesce(norm_token(threshold_group), "UNSPECIFIED_THRESHOLD"),
      include_flag = yes_flag(include_in_primary_dta),
      independence_flag = yes_flag(independent_cohort),
      reference_compatible_flag = yes_flag(reference_standard_compatible),
      integer_counts = !is.na(tp) & tp >= 0 &
        !is.na(fp) & fp >= 0 &
        !is.na(fn) & fn >= 0 &
        !is.na(tn) & tn >= 0,
      complete_2x2 = !is.na(tp) & !is.na(fp) & !is.na(fn) & !is.na(tn),
      eligible_row = include_flag & independence_flag & reference_compatible_flag &
        integer_counts & complete_2x2
    )

  duplicate_cohorts <- dta |>
    filter(eligible_row) |>
    count(index_test, threshold_group, cohort_id, name = "rows_per_cohort") |>
    filter(rows_per_cohort > 1L)

  if (nrow(duplicate_cohorts) > 0L) {
    write_table(duplicate_cohorts, "dta_duplicate_cohort_check.csv", cfg)
    stop(
      "Duplicate cohort contributions were found within an index-test/threshold group. ",
      "Resolve them before modelling; see dta_duplicate_cohort_check.csv.",
      call. = FALSE
    )
  }

  dta_individual <- dta |>
    filter(eligible_row) |>
    rowwise() |>
    mutate(
      sensitivity = unname(binom_exact_ci(tp, tp + fn)["estimate"]),
      sensitivity_low = unname(binom_exact_ci(tp, tp + fn)["low"]),
      sensitivity_high = unname(binom_exact_ci(tp, tp + fn)["high"]),
      specificity = unname(binom_exact_ci(tn, tn + fp)["estimate"]),
      specificity_low = unname(binom_exact_ci(tn, tn + fp)["low"]),
      specificity_high = unname(binom_exact_ci(tn, tn + fp)["high"])
    ) |>
    ungroup()

  dta_feasibility <- dta |>
    group_by(index_test, threshold_group) |>
    summarise(
      total_rows = n(),
      eligible_rows = sum(eligible_row),
      independent_cohorts = n_distinct(cohort_id[eligible_row]),
      all_reference_standards_compatible = all(reference_compatible_flag[include_flag], na.rm = TRUE),
      all_included_rows_complete = all(complete_2x2[include_flag], na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      minimum_required = cfg$analysis$dta_min_independent_cohorts,
      model_enabled = cfg$analysis$dta_meta_analysis_enabled,
      eligible_for_bivariate_model =
        independent_cohorts >= minimum_required &
        all_reference_standards_compatible &
        all_included_rows_complete,
      status = case_when(
        !eligible_for_bivariate_model & independent_cohorts < minimum_required ~
          "DESCRIPTIVE_ONLY_FEWER_THAN_4_INDEPENDENT_COHORTS",
        !eligible_for_bivariate_model ~ "NOT_READY_FAILED_COMPATIBILITY_OR_COMPLETENESS_GATE",
        eligible_for_bivariate_model & !model_enabled ~ "READY_BUT_MODEL_DISABLED",
        TRUE ~ "READY_TO_MODEL"
      ),
      reason = case_when(
        status == "DESCRIPTIVE_ONLY_FEWER_THAN_4_INDEPENDENT_COHORTS" ~
          "Fewer than the prespecified minimum of four independent cohorts.",
        status == "NOT_READY_FAILED_COMPATIBILITY_OR_COMPLETENESS_GATE" ~
          "Reference-standard compatibility or 2x2 completeness gate failed.",
        status == "READY_BUT_MODEL_DISABLED" ~
          "Primary data pass the gate; enable dta_meta_analysis_enabled in config.yml after methodological review.",
        TRUE ~ "All prespecified feasibility gates passed."
      )
    )

  model_log <- vector("list", nrow(dta_feasibility))
  if (isTRUE(cfg$analysis$dta_meta_analysis_enabled)) {
    if (!requireNamespace("mada", quietly = TRUE)) {
      dta_feasibility <- dta_feasibility |>
        mutate(
          status = if_else(status == "READY_TO_MODEL", "READY_BUT_MADA_NOT_INSTALLED", status),
          reason = if_else(
            status == "READY_BUT_MADA_NOT_INSTALLED",
            "Install the optional mada package and rerun.",
            reason
          )
        )
    } else {
      for (i in seq_len(nrow(dta_feasibility))) {
        group_row <- dta_feasibility[i, ]
        if (group_row$status != "READY_TO_MODEL") next
        one <- dta_individual |>
          filter(
            index_test == group_row$index_test,
            threshold_group == group_row$threshold_group
          )
        stem <- paste0(
          "dta_",
          sanitize_filename(group_row$index_test),
          "__",
          sanitize_filename(group_row$threshold_group)
        )

        fit_result <- tryCatch({
          mada_data <- mada::mada(
            TP = one$tp,
            FN = one$fn,
            FP = one$fp,
            TN = one$tn,
            names = one$study_id
          )
          fit <- mada::reitsma(mada_data)
          saveRDS(fit, file.path(cfg$paths$results, "models", paste0(stem, "_reitsma.rds")))
          capture.output(
            summary(fit),
            file = file.path(cfg$paths$results, "models", paste0(stem, "_summary.txt"))
          )

          grDevices::png(
            file.path(cfg$paths$results, "figures", paste0(stem, "_sroc.png")),
            width = 2400, height = 2000, res = 300
          )
          plot(fit, main = paste(group_row$index_test, group_row$threshold_group))
          grDevices::dev.off()

          grDevices::pdf(
            file.path(cfg$paths$results, "figures", paste0(stem, "_sroc.pdf")),
            width = 8, height = 7
          )
          plot(fit, main = paste(group_row$index_test, group_row$threshold_group))
          grDevices::dev.off()

          list(ok = TRUE, message = "Bivariate Reitsma model fitted.")
        }, error = function(e) {
          while (grDevices::dev.cur() > 1L) grDevices::dev.off()
          list(ok = FALSE, message = conditionMessage(e))
        })

        model_log[[i]] <- tibble(
          index_test = group_row$index_test,
          threshold_group = group_row$threshold_group,
          model_success = fit_result$ok,
          model_message = fit_result$message
        )
        dta_feasibility$status[i] <- if (fit_result$ok) "MODEL_FITTED" else "MODEL_ERROR_REVIEW_LOG"
        dta_feasibility$reason[i] <- fit_result$message
      }
    }
  }

  model_log <- bind_rows(model_log)
  if (nrow(model_log) > 0L) write_table(model_log, "dta_model_log.csv", cfg)

  dta_global_decision <- tibble(
    component = "De novo diagnostic-accuracy meta-analysis",
    decision = if_else(
      any(dta_feasibility$status == "MODEL_FITTED"),
      "MODEL_FITTED_FOR_AT_LEAST_ONE_ELIGIBLE_GROUP",
      "NO_MODEL_FITTED"
    ),
    status = paste(sort(unique(dta_feasibility$status)), collapse = "; "),
    clinical_reason = paste(
      "Pooling is permitted only within compatible index-test/threshold groups",
      "using independent primary cohorts."
    ),
    consequence_for_umbrella = "The umbrella review remains separate; DTA results, if any, form an explicitly updated primary-study component."
  )
}

write_table(dta_feasibility, "dta_meta_analysis_feasibility.csv", cfg)
write_table(dta_global_decision, "dta_meta_analysis_decision.csv", cfg)
if (nrow(dta_individual) > 0L) {
  write_table(dta_individual, "dta_primary_study_accuracy.csv", cfg)

  dta_plot_data <- dta_individual |>
    select(study_id, index_test, threshold_group, sensitivity, specificity) |>
    pivot_longer(c(sensitivity, specificity), names_to = "metric", values_to = "estimate") |>
    mutate(study_id = factor(study_id, levels = rev(unique(study_id))))

  p_dta <- ggplot(dta_plot_data, aes(x = estimate, y = study_id, colour = metric)) +
    geom_point(size = 2.5) +
    facet_grid(index_test + threshold_group ~ ., scales = "free_y", space = "free_y") +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
    scale_colour_manual(values = c(sensitivity = "#17365D", specificity = "#C55A11")) +
    labs(
      title = "Primary-study diagnostic performance",
      subtitle = "Exact confidence intervals are available in the accompanying table",
      x = NULL,
      y = NULL,
      colour = NULL
    ) +
    theme_publication()
  save_plot_dual(p_dta, "optional_dta_primary_study_performance", cfg, width = 9, height = 7)
}

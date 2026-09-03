message("Translating paired sensitivity and specificity into clinically interpretable probabilities...")

anchor_effects <- readRDS(file.path(cfg$paths$results, "data", "anchor_effects.rds"))

dta_components <- anchor_effects |>
  mutate(measure_norm = norm_token(effect_measure)) |>
  filter(measure_norm %in% c("SENSITIVITY", "SPECIFICITY")) |>
  group_by(master_id, analysis_cell, predictor, time, threshold, measure_norm) |>
  summarise(
    estimate = first(display_estimate[!is.na(display_estimate)], default = NA_real_),
    ci_low = first(display_ci_low[!is.na(display_ci_low)], default = NA_real_),
    ci_high = first(display_ci_high[!is.na(display_ci_high)], default = NA_real_),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = measure_norm,
    values_from = c(estimate, ci_low, ci_high),
    names_sep = "_"
  ) |>
  filter(!is.na(estimate_SENSITIVITY), !is.na(estimate_SPECIFICITY)) |>
  mutate(
    test_id = paste(master_id, predictor, threshold, sep = " | "),
    likelihood_ratio_positive = estimate_SENSITIVITY / (1 - estimate_SPECIFICITY),
    likelihood_ratio_negative = (1 - estimate_SENSITIVITY) / estimate_SPECIFICITY,
    lr_positive_plausible_low = ci_low_SENSITIVITY / (1 - ci_low_SPECIFICITY),
    lr_positive_plausible_high = ci_high_SENSITIVITY / (1 - ci_high_SPECIFICITY),
    lr_negative_plausible_low = (1 - ci_high_SENSITIVITY) / ci_high_SPECIFICITY,
    lr_negative_plausible_high = (1 - ci_low_SENSITIVITY) / ci_low_SPECIFICITY,
    uncertainty_note = paste(
      "Likelihood-ratio bounds combine marginal confidence limits and are plausible ranges,",
      "not formal joint 95% confidence intervals."
    ),
    causal_note = "Diagnostic/prognostic classification; not evidence of causal treatment-effect modification."
  )

pretest_scenarios <- tibble(pretest_probability = c(0.50, 0.65, 0.80))

probability_translation <- tidyr::crossing(dta_components, pretest_scenarios) |>
  mutate(
    post_test_probability_positive = post_test_probability(
      pretest_probability,
      likelihood_ratio_positive
    ),
    post_test_probability_negative = post_test_probability(
      pretest_probability,
      likelihood_ratio_negative
    ),
    positive_plausible_low = post_test_probability(
      pretest_probability,
      lr_positive_plausible_low
    ),
    positive_plausible_high = post_test_probability(
      pretest_probability,
      lr_positive_plausible_high
    ),
    negative_plausible_low = post_test_probability(
      pretest_probability,
      lr_negative_plausible_low
    ),
    negative_plausible_high = post_test_probability(
      pretest_probability,
      lr_negative_plausible_high
    )
  )

negative_gatekeeper <- probability_translation |>
  mutate(
    cohort_size = 100,
    modeled_responders = cohort_size * pretest_probability,
    modeled_nonresponders = cohort_size * (1 - pretest_probability),
    true_positive_tests = modeled_responders * estimate_SENSITIVITY,
    false_negative_tests = modeled_responders * (1 - estimate_SENSITIVITY),
    true_negative_tests = modeled_nonresponders * estimate_SPECIFICITY,
    false_positive_tests = modeled_nonresponders * (1 - estimate_SPECIFICITY),
    all_negative_tests = false_negative_tests + true_negative_tests,
    residual_response_probability_among_test_negatives = if_else(
      all_negative_tests > 0,
      false_negative_tests / all_negative_tests,
      NA_real_
    ),
    scenario_note = paste(
      "Modeled per 100 tested from review-level sensitivity/specificity; not observed causal harm.",
      "Selection and verification bias may affect transportability."
    )
  )

reported_rout_npv <- anchor_effects |>
  filter(
    master_id == "FT-010",
    norm_token(effect_measure) == "NPV_AT_65_RESPONSE_PREVALENCE"
  ) |>
  transmute(
    threshold,
    reported_npv = display_estimate,
    reported_npv_ci_low = display_ci_low,
    reported_npv_ci_high = display_ci_high,
    reported_effect_id = effect_id,
    declared_response_prevalence = 0.65
  )

rout_npv_audit <- dta_components |>
  filter(master_id == "FT-010", analysis_cell == "ROUT_THRESHOLDS") |>
  left_join(reported_rout_npv, by = "threshold") |>
  mutate(
    implied_response_prevalence = implied_prevalence_from_npv(
      estimate_SENSITIVITY,
      estimate_SPECIFICITY,
      reported_npv
    ),
    recalculated_npv_at_50 = predictive_values(
      estimate_SENSITIVITY,
      estimate_SPECIFICITY,
      0.50
    )$npv,
    recalculated_npv_at_65 = predictive_values(
      estimate_SENSITIVITY,
      estimate_SPECIFICITY,
      0.65
    )$npv,
    recalculated_npv_at_80 = predictive_values(
      estimate_SENSITIVITY,
      estimate_SPECIFICITY,
      0.80
    )$npv,
    absolute_discrepancy_at_declared_prevalence = abs(reported_npv - recalculated_npv_at_65),
    audit_status = case_when(
      is.na(reported_npv) ~ "REPORTED_NPV_NOT_AVAILABLE",
      absolute_discrepancy_at_declared_prevalence <= 0.01 ~ "CONSISTENT_WITH_DECLARED_PREVALENCE",
      abs(implied_response_prevalence - 0.80) <= 0.03 ~
        "INTERNALLY_INCONSISTENT_IMPLIED_PREVALENCE_APPROX_80_PERCENT",
      TRUE ~ "INTERNALLY_INCONSISTENT_OTHER"
    ),
    preservation_rule = "Published NPV is retained unchanged; recalculation is an audit flag, not a replacement."
  )

if (nrow(rout_npv_audit) != 3L ||
    !all(str_detect(rout_npv_audit$audit_status, "INTERNALLY_INCONSISTENT"))) {
  stop("Expected three audit-flagged Rout NPV/prevalence inconsistencies.", call. = FALSE)
}

probability_curves <- tidyr::crossing(
  dta_components,
  pretest_probability = seq(0.20, 0.90, by = 0.01),
  test_result = c("Positive", "Negative")
) |>
  mutate(
    likelihood_ratio = if_else(
      test_result == "Positive",
      likelihood_ratio_positive,
      likelihood_ratio_negative
    ),
    post_test_probability = post_test_probability(pretest_probability, likelihood_ratio),
    test_label = stringr::str_trunc(paste0(predictor, "; ", threshold), 55)
  )

write_table(dta_components, "probability_translation_paired_accuracy.csv", cfg)
write_table(probability_translation, "probability_translation_pretest_scenarios.csv", cfg)
write_table(negative_gatekeeper, "probability_translation_negative_gatekeeper_per_100.csv", cfg)
write_table(rout_npv_audit, "rout_npv_internal_consistency_audit.csv", cfg)
write_table(probability_curves, "probability_translation_curves.csv", cfg)

saveRDS(
  list(
    paired_accuracy = dta_components,
    scenarios = probability_translation,
    negative_gatekeeper = negative_gatekeeper,
    rout_npv_audit = rout_npv_audit
  ),
  file.path(cfg$paths$results, "data", "probability_translation.rds")
)

p_probability <- probability_curves |>
  filter(analysis_cell %in% c(
    "TAP_TEST_DTA", "LIT_DTA", "ROUT_THRESHOLDS", "ELD_DTA", "STRUCTURAL_IMAGING"
  )) |>
  ggplot(aes(x = pretest_probability, y = post_test_probability, colour = test_result)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", colour = "#8A9BA8") +
  geom_line(linewidth = 0.8) +
  facet_wrap(~test_label, ncol = 2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_colour_manual(values = c(Positive = "#0F6B78", Negative = "#C00000")) +
  labs(
    title = "How test results change modeled shunt-response probability",
    subtitle = "Selected CSF tests, Rout thresholds, ELD, and DESH; review-level accuracy without primary-study re-pooling",
    x = "Pretest probability of response",
    y = "Post-test probability of response",
    colour = "Test result"
  ) +
  theme_publication(base_size = 8.5)

save_plot_dual(p_probability, "figure_10_probability_translation", cfg, width = 11, height = 11)

gatekeeper_plot_data <- negative_gatekeeper |>
  filter(pretest_probability == 0.65) |>
  mutate(test_label = stringr::str_trunc(paste0(predictor, "; ", threshold), 65)) |>
  arrange(residual_response_probability_among_test_negatives) |>
  mutate(test_label = factor(test_label, levels = unique(test_label)))

p_gatekeeper <- gatekeeper_plot_data |>
  ggplot(aes(x = residual_response_probability_among_test_negatives, y = test_label)) +
  geom_segment(
    aes(
      x = negative_plausible_low,
      xend = negative_plausible_high,
      y = test_label,
      yend = test_label
    ),
    colour = "#8A9BA8",
    linewidth = 1.1
  ) +
  geom_point(colour = "#C00000", size = 2.8) +
  geom_text(
    aes(label = scales::percent(residual_response_probability_among_test_negatives, accuracy = 1)),
    hjust = -0.25,
    size = 3
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.06)
  ) +
  labs(
    title = "A negative test can leave substantial modeled response probability",
    subtitle = "Scenario assumes 65% pretest probability; values are model-based and not causal harm estimates",
    x = "Residual response probability among test-negative patients",
    y = NULL,
    caption = paste(
      "Horizontal ranges combine marginal sensitivity and specificity confidence limits;",
      "they are plausible ranges, not formal joint 95% confidence intervals."
    )
  ) +
  theme_publication(base_size = 9)

save_plot_dual(p_gatekeeper, "figure_11_negative_gatekeeper_scenario", cfg, width = 10.5, height = 6.5)

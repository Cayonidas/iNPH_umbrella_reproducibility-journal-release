message("Auditing what the literature calls a shunt response...")

dat <- readRDS(file.path(cfg$paths$results, "data", "analysis_data.rds"))

definitions <- dat$definitions |>
  mutate(
    definition_class_norm = norm_token(definition_class),
    patient_reported_norm = norm_token(patient_reported),
    validated_norm = norm_token(validated_for_inph),
    mcid_norm = norm_token(mcid_source),
    outcome_group = outcome_group(domain),
    source_label_validated_threshold = definition_class_norm == "VALIDATED_THRESHOLD",
    general_instrument_validation_only = (master_id == "FT-007" & source_label_validated_threshold) |
      validated_norm == "NOT_SPECIFICALLY_FOR_INPH",
    threshold_validated_for_inph_shunt_response = FALSE,
    threshold_validation_status = case_when(
      threshold_validated_for_inph_shunt_response ~
        "EXPLICITLY_VALIDATED_FOR_INPH_SHUNT_RESPONSE",
      general_instrument_validation_only ~ "GENERAL_INSTRUMENT_VALIDATION_ONLY",
      source_label_validated_threshold ~ "UNCLEAR_STUDY_SPECIFIC_THRESHOLD",
      TRUE ~ "NOT_VALIDATED_OR_NOT_REPORTED"
    ),
    patient_reported_clear = patient_reported_norm == "YES",
    mcid_explicitly_unreported = mcid_norm %in% c("NR", "GENERALLY_NOT_REPORTED"),
    mcid_not_applicable = toupper(trimws(as.character(mcid_source))) %in% "NA",
    instrument_validation_source_described = !is.na(mcid_norm) &
      !mcid_explicitly_unreported &
      !mcid_not_applicable,
    mcid_source_specific_to_inph_shunt_response = FALSE,
    statistical_change_only = definition_class_norm == "STATISTICAL_CHANGE_ONLY",
    author_defined = definition_class_norm == "AUTHOR_DEFINED"
  )

definition_class_table <- definitions |>
  count(definition_class_norm, name = "definition_count") |>
  mutate(
    proportion = definition_count / sum(definition_count),
    label = stringr::str_to_sentence(gsub("_", " ", definition_class_norm))
  ) |>
  arrange(desc(definition_count))

definition_indicator_table <- tibble(
  indicator = c(
    "Author-defined response",
    "Statistical change only",
    "Source-labelled validated threshold",
    "Explicitly validated for iNPH shunt response",
    "General instrument validation only",
    "Clearly patient-reported",
    "MCID explicitly unreported",
    "Instrument-validation source described",
    "MCID source specific to iNPH shunt response",
    "MCID not applicable"
  ),
  definition_count = c(
    sum(definitions$author_defined),
    sum(definitions$statistical_change_only),
    sum(definitions$source_label_validated_threshold),
    sum(definitions$threshold_validated_for_inph_shunt_response),
    sum(definitions$general_instrument_validation_only),
    sum(definitions$patient_reported_clear),
    sum(definitions$mcid_explicitly_unreported),
    sum(definitions$instrument_validation_source_described),
    sum(definitions$mcid_source_specific_to_inph_shunt_response),
    sum(definitions$mcid_not_applicable)
  )
) |>
  mutate(proportion = definition_count / nrow(definitions))

definition_by_level <- definitions |>
  count(primary_study_or_review_level, definition_class_norm, name = "definition_count") |>
  group_by(primary_study_or_review_level) |>
  mutate(proportion_within_level = definition_count / sum(definition_count)) |>
  ungroup()

definition_by_domain <- definitions |>
  count(outcome_group, definition_class_norm, name = "definition_count") |>
  group_by(outcome_group) |>
  mutate(proportion_within_domain = definition_count / sum(definition_count)) |>
  ungroup()

review_definition_profile <- definitions |>
  group_by(master_id) |>
  summarise(
    definitions_n = n(),
    author_defined_n = sum(author_defined),
    statistical_change_only_n = sum(statistical_change_only),
    source_label_validated_threshold_n = sum(source_label_validated_threshold),
    validated_threshold_n = sum(threshold_validated_for_inph_shunt_response),
    general_instrument_validation_only_n = sum(general_instrument_validation_only),
    patient_reported_clear_n = sum(patient_reported_clear),
    instrument_validation_source_described_n = sum(instrument_validation_source_described),
    mcid_source_described_n = sum(mcid_source_specific_to_inph_shunt_response),
    any_validated_threshold = any(threshold_validated_for_inph_shunt_response),
    any_patient_reported = any(patient_reported_clear),
    .groups = "drop"
  )

write_table(definition_class_table, "response_definition_classes.csv", cfg)
write_table(definition_indicator_table, "response_definition_clinical_validity_indicators.csv", cfg)
write_table(definition_by_level, "response_definitions_by_evidence_level.csv", cfg)
write_table(definition_by_domain, "response_definitions_by_clinical_domain.csv", cfg)
write_table(review_definition_profile, "response_definition_profile_by_review.csv", cfg)
write_data(definitions, "outcome_definitions_derived_flags.csv", cfg)
saveRDS(review_definition_profile, file.path(cfg$paths$results, "data", "definition_profile.rds"))

class_order <- definition_class_table |>
  arrange(definition_count) |>
  pull(label)

p_class <- definition_class_table |>
  mutate(label = factor(label, levels = class_order)) |>
  ggplot(aes(x = definition_count, y = label)) +
  geom_col(fill = "#17365D", width = 0.7) +
  geom_text(aes(label = paste0(definition_count, " (", scales::percent(proportion, accuracy = 0.1), ")")), hjust = -0.08, size = 3.2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.23))) +
  labs(
    title = "Most definitions of shunt response were not clinically anchored",
    subtitle = "Each row is one extracted response definition (n = 139)",
    x = "Definitions",
    y = NULL
  ) +
  theme_publication()

p_indicators <- definition_indicator_table |>
  arrange(proportion) |>
  mutate(indicator = factor(indicator, levels = indicator)) |>
  ggplot(aes(x = proportion, y = indicator)) +
  geom_col(fill = "#0F6B78", width = 0.7) +
  geom_text(aes(label = paste0(definition_count, "/", nrow(definitions))), hjust = -0.08, size = 3.1) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1.12),
    breaks = seq(0, 1, 0.25)
  ) +
  labs(
    title = "Clinical-importance indicators",
    subtitle = "Instrument validation is separated from validation of a shunt-response threshold; no composite score",
    x = "Proportion of definitions",
    y = NULL
  ) +
  theme_publication()

p_response <- p_class / p_indicators + patchwork::plot_annotation(tag_levels = "A")
save_plot_dual(p_response, "figure_05_response_definition_audit", cfg, width = 11, height = 9.2)

domain_plot_data <- definition_by_domain |>
  filter(definition_class_norm %in% c(
    "AUTHOR_DEFINED", "STATISTICAL_CHANGE_ONLY", "VALIDATED_THRESHOLD",
    "PATIENT_CAREGIVER_REPORT", "CLINICAL_JUDGMENT", "TIME_TO_EVENT"
  )) |>
  mutate(
    definition_class = stringr::str_to_sentence(gsub("_", " ", definition_class_norm)),
    outcome_group = factor(
      outcome_group,
      levels = rev(unique(outcome_group[order(definition_count)]))
    )
  )

p_domain <- ggplot(domain_plot_data, aes(x = definition_class, y = outcome_group, fill = definition_count)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(definition_count == 0, "", definition_count)), size = 3) +
  scale_fill_gradient(low = "#EEF5F9", high = "#17365D") +
  labs(
    title = "Response-definition classes by clinical domain",
    x = NULL,
    y = NULL,
    fill = "Definitions"
  ) +
  theme_publication(base_size = 9) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot_dual(p_domain, "supplement_response_definitions_by_domain", cfg, width = 11, height = 6.5)

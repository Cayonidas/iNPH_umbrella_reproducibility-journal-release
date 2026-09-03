message("Building the descriptive evidence map...")

dat <- readRDS(file.path(cfg$paths$results, "data", "analysis_data.rds"))

review_quantitative <- dat$effects |>
  distinct(master_id) |>
  mutate(has_quantitative_estimate = TRUE)

reviews_map <- dat$reviews |>
  mutate(
    layer_group = layer_group(final_layer),
    year_num = as.integer(parse_num(year)),
    meta_analysis_group = case_when(
      norm_token(meta_analysis) == "YES" ~ "Meta-analysis",
      str_detect(norm_token(meta_analysis), "PARTIAL") ~ "Partial / descriptive",
      TRUE ~ "No meta-analysis"
    )
  ) |>
  left_join(review_quantitative, by = "master_id") |>
  mutate(has_quantitative_estimate = coalesce(has_quantitative_estimate, FALSE))

review_map <- reviews_map |>
  count(primary_domain, layer_group, name = "review_count") |>
  tidyr::complete(
    primary_domain,
    layer_group = c("Prediction", "Response measurement", "Both"),
    fill = list(review_count = 0L)
  )

domain_order <- review_map |>
  group_by(primary_domain) |>
  summarise(total = sum(review_count), .groups = "drop") |>
  arrange(total) |>
  pull(primary_domain)

review_map <- review_map |>
  mutate(primary_domain = factor(primary_domain, levels = domain_order))

findings_map <- dat$findings |>
  left_join(
    dat$reviews |> select(master_id, primary_domain, final_layer),
    by = "master_id"
  ) |>
  mutate(
    outcome_group = outcome_group(outcome_domain),
    layer_group = layer_group(final_layer)
  ) |>
  count(primary_domain, outcome_group, name = "finding_count")

review_summary <- reviews_map |>
  summarise(
    reviews = n(),
    years_min = min(year_num, na.rm = TRUE),
    years_max = max(year_num, na.rm = TRUE),
    reviews_with_meta_analysis = sum(meta_analysis_group == "Meta-analysis"),
    reviews_with_quantitative_estimates = sum(has_quantitative_estimate),
    primary_domains = n_distinct(primary_domain)
  )

review_type_table <- reviews_map |>
  count(review_type, meta_analysis_group, name = "review_count") |>
  arrange(desc(review_count), review_type)

year_table <- reviews_map |>
  count(year_num, layer_group, name = "review_count") |>
  filter(!is.na(year_num))

write_table(review_map |> mutate(primary_domain = as.character(primary_domain)), "evidence_map_reviews_by_domain_layer.csv", cfg)
write_table(findings_map, "evidence_map_findings_by_domain_outcome.csv", cfg)
write_table(review_summary, "evidence_map_summary.csv", cfg)
write_table(review_type_table, "review_types.csv", cfg)
write_table(year_table, "reviews_by_year_layer.csv", cfg)

p_review_map <- ggplot(review_map, aes(x = layer_group, y = primary_domain, fill = review_count)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(review_count == 0, "", review_count)), size = 3.4) +
  scale_fill_gradient(low = "#EEF5F9", high = "#0F6B78", breaks = scales::pretty_breaks()) +
  labs(
    title = "Evidence architecture across 41 included reviews",
    subtitle = "Counts represent reviews, not independent patients or primary studies",
    x = NULL,
    y = NULL,
    fill = "Reviews"
  ) +
  theme_publication(base_size = 10) +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_plot_dual(p_review_map, "figure_01_review_evidence_map", cfg, width = 10.5, height = 6.8)

finding_domain_order <- findings_map |>
  group_by(primary_domain) |>
  summarise(total = sum(finding_count), .groups = "drop") |>
  arrange(total) |>
  pull(primary_domain)

outcome_order <- findings_map |>
  group_by(outcome_group) |>
  summarise(total = sum(finding_count), .groups = "drop") |>
  arrange(desc(total)) |>
  pull(outcome_group)

findings_plot_data <- findings_map |>
  mutate(
    primary_domain = factor(primary_domain, levels = finding_domain_order),
    outcome_group = factor(outcome_group, levels = outcome_order)
  )

p_findings <- ggplot(findings_plot_data, aes(x = outcome_group, y = primary_domain, fill = finding_count)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = ifelse(finding_count == 0, "", finding_count)), size = 2.8) +
  scale_fill_gradient(low = "#F3F7FA", high = "#17365D") +
  labs(
    title = paste0(
      "Clinical domains represented by ", nrow(dat$findings),
      " structured findings"
    ),
    subtitle = "A finding is an extracted review-level claim; counts do not imply certainty",
    x = NULL,
    y = NULL,
    fill = "Findings"
  ) +
  theme_publication(base_size = 9) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))

save_plot_dual(p_findings, "figure_02_finding_evidence_map", cfg, width = 12, height = 7.5)

p_year <- ggplot(year_table, aes(x = year_num, y = review_count, fill = layer_group)) +
  geom_col(position = "stack", width = 0.8) +
  scale_fill_manual(values = c(
    "Prediction" = "#17365D",
    "Response measurement" = "#0F6B78",
    "Both" = "#8FB8C8"
  )) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  labs(
    title = "Publication year and analytic layer",
    x = "Publication year",
    y = "Reviews",
    fill = "Layer"
  ) +
  theme_publication()

save_plot_dual(p_year, "supplement_review_year_distribution", cfg, width = 9, height = 5.5)

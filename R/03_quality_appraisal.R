message("Analysing duplicate ROBIS and AMSTAR 2 appraisals...")

dat <- readRDS(file.path(cfg$paths$results, "data", "analysis_data.rds"))
run_state <- readRDS(file.path(cfg$paths$results, "data", "run_state.rds"))
q <- dat$quality_reviews

robis_long <- q |>
  select(
    master_id,
    d1_a, d1_b, d2_a, d2_b, d3_a, d3_b, d4_a, d4_b,
    overall_a, overall_b
  ) |>
  pivot_longer(
    -master_id,
    names_to = c("domain", "reviewer_code"),
    names_pattern = "(d[1-4]|overall)_([ab])$",
    values_to = "judgment"
  ) |>
  mutate(
    domain = recode(
      domain,
      d1 = "Domain 1",
      d2 = "Domain 2",
      d3 = "Domain 3",
      d4 = "Domain 4",
      overall = "Overall"
    ),
    reviewer = recode(
      reviewer_code,
      a = cfg$labels$reviewer_a,
      b = cfg$labels$reviewer_b
    ),
    judgment = norm_token(judgment)
  )

robis_distribution <- robis_long |>
  count(domain, reviewer, judgment, name = "review_count") |>
  group_by(domain, reviewer) |>
  mutate(proportion = review_count / sum(review_count)) |>
  ungroup()

domain_pairs <- q |>
  transmute(
    master_id,
    `Domain 1_A` = norm_token(d1_a), `Domain 1_B` = norm_token(d1_b),
    `Domain 2_A` = norm_token(d2_a), `Domain 2_B` = norm_token(d2_b),
    `Domain 3_A` = norm_token(d3_a), `Domain 3_B` = norm_token(d3_b),
    `Domain 4_A` = norm_token(d4_a), `Domain 4_B` = norm_token(d4_b),
    Overall_A = norm_token(overall_a), Overall_B = norm_token(overall_b)
  )

robis_agreement <- map_dfr(
  c("Domain 1", "Domain 2", "Domain 3", "Domain 4", "Overall"),
  function(domain) {
    a <- domain_pairs[[paste0(domain, "_A")]]
    b <- domain_pairs[[paste0(domain, "_B")]]
    kappa_summary(
      a, b,
      levels = c("LOW", "UNCLEAR", "HIGH"),
      weights = "quadratic"
    ) |>
      mutate(instrument_level = paste("ROBIS", domain), .before = 1)
  }
)

signalling_agreement <- bind_rows(
  kappa_summary(
    norm_token(dat$robis_signalling$answer_a),
    norm_token(dat$robis_signalling$answer_b),
    levels = c("YES", "PROBABLY_YES", "NO_INFORMATION", "PROBABLY_NO", "NO"),
    weights = "unweighted"
  ) |>
    mutate(instrument_level = "ROBIS signalling — five categories", .before = 1),
  kappa_summary(
    norm_token(dat$robis_signalling$three_level_a),
    norm_token(dat$robis_signalling$three_level_b),
    levels = c("FAVORABLE", "NO_INFORMATION", "UNFAVORABLE"),
    weights = "unweighted"
  ) |>
    mutate(instrument_level = "ROBIS signalling — collapsed three categories", .before = 1)
)

amstar_applicability_agreement <- kappa_summary(
  norm_token(dat$amstar_summary$applicability_a),
  norm_token(dat$amstar_summary$applicability_b),
  levels = c("OUTSIDE_INTENDED_SCOPE", "PARTIAL_SEPARABLE", "IN_SCOPE"),
  weights = "unweighted"
) |>
  mutate(instrument_level = "AMSTAR 2 applicability", .before = 1)

amstar_confidence_agreement <- kappa_summary(
  norm_token(dat$amstar_summary$confidence_a),
  norm_token(dat$amstar_summary$confidence_b),
  levels = c(
    "NOT_APPLICABLE_OUTSIDE_SCOPE", "CRITICALLY_LOW", "LOW", "MODERATE", "HIGH"
  ),
  weights = "unweighted"
) |>
  mutate(instrument_level = "AMSTAR 2 confidence", .before = 1)

amstar_item_agreement <- kappa_summary(
  norm_token(dat$amstar_items$answer_a),
  norm_token(dat$amstar_items$answer_b),
  levels = c(
    "NOT_APPLICABLE_OUTSIDE_SCOPE", "NO", "PARTIAL_YES", "YES", "NOT_APPLICABLE"
  ),
  weights = "unweighted"
) |>
  mutate(instrument_level = "AMSTAR 2 items", .before = 1)

agreement_table <- bind_rows(
  signalling_agreement,
  robis_agreement,
  amstar_applicability_agreement,
  amstar_confidence_agreement,
  amstar_item_agreement
)

# Build row-level consensus only from exact agreements or explicitly adjudicated cells.
queue_lookup <- dat$robis_domain_queue |>
  transmute(
    master_id,
    domain = recode(
      norm_token(domain_or_overall),
      D1 = "Domain 1", D2 = "Domain 2", D3 = "Domain 3", D4 = "Domain 4",
      OVERALL = "Overall"
    ),
    adjudicated = norm_token(final_consensus),
    queue_status = norm_token(status)
  )

final_consensus_lookup <- q |>
  transmute(
    master_id,
    `Domain 1` = norm_token(d1_consensus),
    `Domain 2` = norm_token(d2_consensus),
    `Domain 3` = norm_token(d3_consensus),
    `Domain 4` = norm_token(d4_consensus),
    Overall = norm_token(overall_consensus)
  ) |>
  pivot_longer(
    -master_id,
    names_to = "domain",
    values_to = "explicit_final_consensus"
  )

robis_pairs_long <- q |>
  select(
    master_id,
    d1_a, d1_b, d2_a, d2_b, d3_a, d3_b, d4_a, d4_b,
    overall_a, overall_b
  ) |>
  pivot_longer(
    -master_id,
    names_to = c("domain_code", "reviewer_code"),
    names_pattern = "(d[1-4]|overall)_([ab])$",
    values_to = "judgment"
  ) |>
  mutate(
    domain = recode(
      domain_code,
      d1 = "Domain 1", d2 = "Domain 2", d3 = "Domain 3", d4 = "Domain 4",
      overall = "Overall"
    ),
    reviewer_code = toupper(reviewer_code),
    judgment = norm_token(judgment)
  ) |>
  select(master_id, domain, reviewer_code, judgment) |>
  pivot_wider(names_from = reviewer_code, values_from = judgment) |>
  left_join(queue_lookup, by = c("master_id", "domain")) |>
  left_join(final_consensus_lookup, by = c("master_id", "domain")) |>
  mutate(
    reconstructed_consensus = resolve_quality_consensus(A, B, adjudicated),
    consensus_candidate = resolve_quality_consensus(
      A,
      B,
      adjudicated = adjudicated,
      explicit_final = explicit_final_consensus,
      prefer_explicit = run_state$quality_consensus_complete
    ),
    consensus_source = case_when(
      run_state$quality_consensus_complete &
        !is.na(explicit_final_consensus) &
        !is.na(reconstructed_consensus) &
        explicit_final_consensus != reconstructed_consensus ~
        "EXPLICIT_AUTHOR_CONSENSUS_OVERRIDE_OF_INITIAL_AGREEMENT",
      run_state$quality_consensus_complete & !is.na(explicit_final_consensus) ~
        "EXPLICIT_AUTHOR_CONSENSUS_FINAL_COLUMN",
      A == B ~ "EXACT_REVIEWER_AGREEMENT",
      !is.na(adjudicated) ~ "ADJUDICATED_DISAGREEMENT",
      TRUE ~ "PENDING"
    ),
    overrides_reconstructed_pair = !is.na(explicit_final_consensus) &
      !is.na(reconstructed_consensus) &
      explicit_final_consensus != reconstructed_consensus,
    consensus_cell_complete = !is.na(consensus_candidate),
    consensus_judgment = if (run_state$quality_consensus_complete) consensus_candidate else NA_character_
  )

if (run_state$quality_consensus_complete) {
  invalid_final_robis <- robis_pairs_long |>
    filter(
      is.na(explicit_final_consensus) |
        !explicit_final_consensus %in% c("LOW", "UNCLEAR", "HIGH")
    )

  if (nrow(invalid_final_robis) > 0L) {
    stop(
      "Final ROBIS consensus contains missing or invalid values: ",
      paste(
        paste(invalid_final_robis$master_id, invalid_final_robis$domain, sep = " / "),
        collapse = "; "
      ),
      call. = FALSE
    )
  }

  queue_conflicts <- robis_pairs_long |>
    filter(
      A != B,
      !is.na(adjudicated),
      !is.na(explicit_final_consensus),
      adjudicated != explicit_final_consensus
    )

  if (nrow(queue_conflicts) > 0L) {
    stop(
      "Explicit ROBIS final columns conflict with resolved queue decisions: ",
      paste(
        paste(queue_conflicts$master_id, queue_conflicts$domain, sep = " / "),
        collapse = "; "
      ),
      call. = FALSE
    )
  }
}

robis_consensus_overrides <- robis_pairs_long |>
  filter(overrides_reconstructed_pair %in% TRUE) |>
  transmute(
    master_id,
    domain,
    reviewer_a = A,
    reviewer_b = B,
    reconstructed_consensus,
    explicit_final_consensus,
    consensus_source,
    audit_note = paste(
      "The explicit final author-consensus column is authoritative; this row",
      "records a post-discussion consensus that supersedes the initial pair."
    )
  )

robis_consensus_wide <- robis_pairs_long |>
  select(master_id, domain, consensus_judgment) |>
  pivot_wider(
    names_from = domain,
    values_from = consensus_judgment,
    names_prefix = "robis_consensus_"
  )
names(robis_consensus_wide) <- clean_names_base(names(robis_consensus_wide))

amstar_consensus <- dat$amstar_summary |>
  transmute(
    master_id,
    applicability_a = norm_token(applicability_a),
    applicability_b = norm_token(applicability_b),
    confidence_a = norm_token(confidence_a),
    confidence_b = norm_token(confidence_b),
    final_applicability_raw = norm_token(final_applicability),
    final_confidence_raw = norm_token(final_confidence),
    reconstructed_applicability = resolve_quality_consensus(
      applicability_a,
      applicability_b,
      adjudicated = final_applicability_raw
    ),
    reconstructed_confidence = resolve_quality_consensus(
      confidence_a,
      confidence_b,
      adjudicated = final_confidence_raw
    ),
    consensus_applicability_candidate = resolve_quality_consensus(
      applicability_a,
      applicability_b,
      adjudicated = final_applicability_raw,
      explicit_final = final_applicability_raw,
      prefer_explicit = run_state$quality_consensus_complete
    ),
    consensus_confidence_candidate = resolve_quality_consensus(
      confidence_a,
      confidence_b,
      adjudicated = final_confidence_raw,
      explicit_final = final_confidence_raw,
      prefer_explicit = run_state$quality_consensus_complete
    ),
    amstar_consensus_applicability = if (run_state$quality_consensus_complete) {
      consensus_applicability_candidate
    } else {
      NA_character_
    },
    amstar_consensus_confidence = if (run_state$quality_consensus_complete) {
      consensus_confidence_candidate
    } else {
      NA_character_
    }
  )

if (run_state$quality_consensus_complete) {
  valid_amstar_applicability <- c(
    "OUTSIDE_INTENDED_SCOPE", "PARTIAL_SEPARABLE", "IN_SCOPE"
  )
  valid_amstar_confidence <- c(
    "NOT_APPLICABLE_OUTSIDE_SCOPE", "CRITICALLY_LOW", "LOW", "MODERATE", "HIGH"
  )
  invalid_amstar_summary <- amstar_consensus |>
    filter(
      is.na(amstar_consensus_applicability) |
        !amstar_consensus_applicability %in% valid_amstar_applicability |
        is.na(amstar_consensus_confidence) |
        !amstar_consensus_confidence %in% valid_amstar_confidence
    )

  if (nrow(invalid_amstar_summary) > 0L) {
    stop(
      "Final AMSTAR 2 summary consensus contains missing or invalid values: ",
      paste(invalid_amstar_summary$master_id, collapse = "; "),
      call. = FALSE
    )
  }
}

quality_flags <- q |>
  transmute(
    master_id,
    robis_overall_a = norm_token(overall_a),
    robis_overall_b = norm_token(overall_b),
    robis_low_both = robis_overall_a == "LOW" & robis_overall_b == "LOW",
    robis_not_high_both = robis_overall_a != "HIGH" & robis_overall_b != "HIGH",
    robis_high_either = robis_overall_a == "HIGH" | robis_overall_b == "HIGH"
  ) |>
  left_join(robis_consensus_wide, by = "master_id") |>
  left_join(amstar_consensus, by = "master_id") |>
  mutate(
    robis_final_low = robis_consensus_overall == "LOW",
    robis_final_unclear = robis_consensus_overall == "UNCLEAR",
    robis_final_high = robis_consensus_overall == "HIGH",
    quality_mode = if_else(
      run_state$quality_consensus_complete,
      "FINAL_CONSENSUS",
      "DUAL_REVIEWER_PRECONSENSUS"
    )
  )

if (run_state$quality_consensus_complete) {
  observed_robis_final <- c(
    HIGH = sum(quality_flags$robis_final_high %in% TRUE),
    UNCLEAR = sum(quality_flags$robis_final_unclear %in% TRUE),
    LOW = sum(quality_flags$robis_final_low %in% TRUE)
  )
  expected_robis_final <- c(HIGH = 32L, UNCLEAR = 4L, LOW = 5L)

  observed_amstar_final <- c(
    CRITICALLY_LOW = sum(
      quality_flags$amstar_consensus_confidence == "CRITICALLY_LOW",
      na.rm = TRUE
    ),
    LOW = sum(
      quality_flags$amstar_consensus_confidence == "LOW",
      na.rm = TRUE
    )
  )
  expected_amstar_final <- c(CRITICALLY_LOW = 14L, LOW = 1L)

  if (!isTRUE(all(observed_robis_final == expected_robis_final))) {
    stop(
      "Final ROBIS consensus distribution differs from the frozen 2026-08-11 source. ",
      "Observed: ",
      paste(names(observed_robis_final), observed_robis_final, sep = "=", collapse = ", "),
      ". Expected: ",
      paste(names(expected_robis_final), expected_robis_final, sep = "=", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (!isTRUE(all(observed_amstar_final == expected_amstar_final))) {
    stop(
      "Final AMSTAR 2 consensus distribution differs from the frozen 2026-08-11 source. ",
      "Observed: ",
      paste(names(observed_amstar_final), observed_amstar_final, sep = "=", collapse = ", "),
      ". Expected: ",
      paste(names(expected_amstar_final), expected_amstar_final, sep = "=", collapse = ", "),
      ".",
      call. = FALSE
    )
  }
}

saveRDS(quality_flags, file.path(cfg$paths$results, "data", "quality_flags.rds"))
write_table(robis_distribution, "robis_distribution_by_reviewer.csv", cfg)
write_table(agreement_table, "quality_appraisal_agreement.csv", cfg)
write_table(robis_pairs_long, "robis_cell_consensus_audit.csv", cfg)
write_table(robis_consensus_overrides, "robis_consensus_overrides.csv", cfg)
write_table(quality_flags, "quality_flags_by_review.csv", cfg)

amstar_distribution <- dat$amstar_summary |>
  select(master_id, confidence_a, confidence_b) |>
  pivot_longer(
    c(confidence_a, confidence_b),
    names_to = "reviewer_code",
    values_to = "confidence"
  ) |>
  mutate(
    reviewer = if_else(
      reviewer_code == "confidence_a",
      cfg$labels$reviewer_a,
      cfg$labels$reviewer_b
    ),
    confidence = norm_token(confidence)
  ) |>
  filter(confidence != "NOT_APPLICABLE_OUTSIDE_SCOPE") |>
  count(reviewer, confidence, name = "review_count") |>
  group_by(reviewer) |>
  mutate(proportion = review_count / sum(review_count)) |>
  ungroup()

amstar_item_by_item <- dat$amstar_items |>
  select(item, answer_a, answer_b) |>
  pivot_longer(c(answer_a, answer_b), names_to = "reviewer_code", values_to = "answer") |>
  mutate(
    reviewer = if_else(reviewer_code == "answer_a", cfg$labels$reviewer_a, cfg$labels$reviewer_b),
    answer = norm_token(answer)
  ) |>
  filter(!answer %in% c("NOT_APPLICABLE_OUTSIDE_SCOPE", "NOT_APPLICABLE"), !is.na(answer)) |>
  count(item, reviewer, answer, name = "review_count") |>
  group_by(item, reviewer) |>
  mutate(proportion = review_count / sum(review_count)) |>
  ungroup()

write_table(amstar_distribution, "amstar2_confidence_by_reviewer.csv", cfg)
write_table(amstar_item_by_item, "amstar2_item_responses_by_reviewer.csv", cfg)

robis_colors <- c(LOW = "#70AD47", UNCLEAR = "#FFC000", HIGH = "#C00000")
robis_domain_levels <- c("Domain 1", "Domain 2", "Domain 3", "Domain 4", "Overall")

p_robis_dual <- robis_distribution |>
  mutate(
    domain = factor(domain, levels = robis_domain_levels),
    judgment = factor(judgment, levels = c("LOW", "UNCLEAR", "HIGH"))
  ) |>
  ggplot(aes(x = domain, y = proportion, fill = judgment)) +
  geom_col(width = 0.75) +
  facet_wrap(~reviewer, ncol = 1) +
  scale_fill_manual(values = robis_colors, drop = FALSE) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "ROBIS judgments from two independent assessments",
    subtitle = "Assessment-specific judgments retained as a transparency analysis",
    x = NULL,
    y = "Proportion of 41 reviews",
    fill = "Judgment"
  ) +
  theme_publication() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

save_plot_dual(
  p_robis_dual,
  "supplement_robis_dual_reviewer_distribution",
  cfg,
  width = 9,
  height = 7.5
)

if (run_state$quality_consensus_complete) {
  robis_consensus_distribution <- robis_pairs_long |>
    filter(!is.na(consensus_judgment)) |>
    count(domain, judgment = consensus_judgment, name = "review_count") |>
    group_by(domain) |>
    mutate(proportion = review_count / sum(review_count)) |>
    ungroup()

  p_robis_main <- robis_consensus_distribution |>
    mutate(
      domain = factor(domain, levels = robis_domain_levels),
      judgment = factor(judgment, levels = c("LOW", "UNCLEAR", "HIGH"))
    ) |>
    ggplot(aes(x = domain, y = proportion, fill = judgment)) +
    geom_col(width = 0.72) +
    geom_text(
      aes(label = review_count),
      position = position_stack(vjust = 0.5),
      colour = "white",
      fontface = "bold"
    ) +
    scale_fill_manual(values = robis_colors, drop = FALSE) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0.04))
    ) +
    labs(
      title = "Final ROBIS consensus across 41 included reviews",
      subtitle = "Author consensus; reviewer-specific distributions are retained in the supplement",
      x = NULL,
      y = "Proportion of reviews",
      fill = "Judgment"
    ) +
    theme_publication() +
    theme(axis.text.x = element_text(angle = 20, hjust = 1))
} else {
  p_robis_main <- p_robis_dual
}

save_plot_dual(p_robis_main, "figure_03_robis_quality_distribution", cfg, width = 9, height = 7.5)

traffic_data <- robis_long |>
  mutate(
    domain = factor(domain, levels = rev(robis_domain_levels)),
    master_id = factor(master_id, levels = rev(sort(unique(master_id)))),
    judgment = factor(judgment, levels = c("LOW", "UNCLEAR", "HIGH"))
  )

p_traffic <- ggplot(traffic_data, aes(x = domain, y = master_id, fill = judgment)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  facet_wrap(~reviewer, nrow = 1) +
  scale_fill_manual(values = robis_colors, drop = FALSE) +
  labs(
    title = "Review-level ROBIS traffic-light matrix",
    subtitle = "High risk predominates, especially in synthesis and interpretation",
    x = NULL,
    y = NULL,
    fill = "Judgment"
  ) +
  theme_publication(base_size = 8) +
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1),
    panel.spacing.x = grid::unit(1.2, "lines")
  )

save_plot_dual(p_traffic, "supplement_robis_traffic_light", cfg, width = 11.5, height = 10)

amstar_colors <- c(CRITICALLY_LOW = "#C00000", LOW = "#ED7D31", MODERATE = "#FFC000", HIGH = "#70AD47")

p_amstar_dual <- amstar_distribution |>
  mutate(confidence = factor(confidence, levels = names(amstar_colors))) |>
  ggplot(aes(x = reviewer, y = review_count, fill = confidence)) +
  geom_col(width = 0.62) +
  geom_text(
    aes(label = review_count),
    position = position_stack(vjust = 0.5),
    colour = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(values = amstar_colors, drop = FALSE) +
  labs(
    title = "AMSTAR 2 confidence among reviews within intended scope",
    subtitle = "No numerical score is calculated; outside-scope reviews are omitted",
    x = NULL,
    y = "Applicable reviews/components",
    fill = "Confidence"
  ) +
  theme_publication()

save_plot_dual(
  p_amstar_dual,
  "supplement_amstar2_dual_reviewer_confidence",
  cfg,
  width = 8.5,
  height = 5.5
)

if (run_state$quality_consensus_complete) {
  amstar_consensus_distribution <- amstar_consensus |>
    filter(
      !is.na(amstar_consensus_confidence),
      amstar_consensus_confidence != "NOT_APPLICABLE_OUTSIDE_SCOPE"
    ) |>
    count(confidence = amstar_consensus_confidence, name = "review_count")

  p_amstar_main <- amstar_consensus_distribution |>
    mutate(
      confidence = factor(confidence, levels = names(amstar_colors)),
      display_source = "Final author consensus"
    ) |>
    ggplot(aes(x = display_source, y = review_count, fill = confidence)) +
    geom_col(width = 0.62) +
    geom_text(
      aes(label = review_count),
      position = position_stack(vjust = 0.5),
      colour = "white",
      fontface = "bold"
    ) +
    scale_fill_manual(values = amstar_colors, drop = FALSE) +
    labs(
      title = "Final AMSTAR 2 confidence among applicable reviews",
      subtitle = "Outside-scope reviews are omitted; no numerical score is calculated",
      x = NULL,
      y = "Applicable reviews/components",
      fill = "Confidence"
    ) +
    theme_publication()
} else {
  p_amstar_main <- p_amstar_dual
}

save_plot_dual(p_amstar_main, "figure_04_amstar2_confidence", cfg, width = 8.5, height = 5.5)

critical_amstar_items <- c(2, 4, 7, 9, 11, 13, 15)
amstar_item_display <- dat$amstar_items |>
  mutate(
    item_number = parse_num(item),
    answer_a_norm = norm_token(answer_a),
    answer_b_norm = norm_token(answer_b),
    final_norm = norm_token(final_consensus),
    reconstructed_consensus = resolve_quality_consensus(
      answer_a_norm,
      answer_b_norm,
      adjudicated = final_norm
    ),
    consensus_candidate = resolve_quality_consensus(
      answer_a_norm,
      answer_b_norm,
      adjudicated = final_norm,
      explicit_final = final_norm,
      prefer_explicit = run_state$quality_consensus_complete
    )
  ) |>
  filter(
    item_number %in% critical_amstar_items,
    !answer_a_norm %in% c("NOT_APPLICABLE_OUTSIDE_SCOPE") |
      !answer_b_norm %in% c("NOT_APPLICABLE_OUTSIDE_SCOPE")
  )

if (run_state$quality_consensus_complete) {
  amstar_item_display <- amstar_item_display |>
    transmute(
      master_id,
      item_number,
      display_source = "Final author consensus",
      answer = consensus_candidate,
      display_status = "PRIMARY_HUMAN_CONSENSUS"
    )
} else {
  amstar_item_display <- amstar_item_display |>
    select(master_id, item_number, answer_a_norm, answer_b_norm) |>
    pivot_longer(
      c(answer_a_norm, answer_b_norm),
      names_to = "reviewer_code",
      values_to = "answer"
    ) |>
    mutate(
      display_source = if_else(
        reviewer_code == "answer_a_norm",
        cfg$labels$reviewer_a,
        cfg$labels$reviewer_b
      ),
      display_status = "PRIMARY_HUMAN_DUAL_REVIEWER_CONSENSUS_PENDING"
    ) |>
    select(master_id, item_number, display_source, answer, display_status)
}

write_table(amstar_item_display, "amstar2_critical_domains_author_display.csv", cfg)

p_amstar_critical <- amstar_item_display |>
  mutate(
    master_id = factor(master_id, levels = rev(sort(unique(master_id)))),
    item_number = factor(item_number, levels = critical_amstar_items),
    answer_group = case_when(
      answer == "YES" ~ "Yes",
      answer == "PARTIAL_YES" ~ "Partial yes",
      answer == "NO" ~ "No",
      TRUE ~ "Not applicable / missing"
    )
  ) |>
  ggplot(aes(x = item_number, y = master_id, fill = answer_group)) +
  geom_tile(colour = "white", linewidth = 0.25) +
  facet_wrap(~display_source, nrow = 1) +
  scale_fill_manual(values = c(
    "Yes" = "#70AD47", "Partial yes" = "#FFC000", "No" = "#C00000",
    "Not applicable / missing" = "#B8C2CC"
  )) +
  labs(
    title = "AMSTAR 2 critical-domain profile",
    subtitle = if (run_state$quality_consensus_complete) {
      "Final author consensus; no numerical score"
    } else {
      "Independent assessments shown separately while author consensus fields remain incomplete"
    },
    x = "Critical item", y = NULL, fill = NULL
  ) +
  theme_publication(base_size = 8)

save_plot_dual(
  p_amstar_critical,
  "supplement_amstar2_critical_domain_heatmap",
  cfg,
  width = 11,
  height = 7
)

quality_readiness <- tibble(
  component = c(
    "ROBIS final consensus",
    "AMSTAR 2 applicability disagreements",
    "AMSTAR 2 confidence disagreements",
    "AMSTAR 2 summary rows with any pending decision",
    "AMSTAR 2 item consensus",
    "Analysis mode"
  ),
  status = c(
    ifelse(run_state$pending_robis == 0, "READY", "PENDING"),
    ifelse(run_state$pending_amstar_applicability == 0, "READY", "PENDING"),
    ifelse(run_state$pending_amstar_confidence == 0, "READY", "PENDING"),
    ifelse(run_state$pending_amstar_scope == 0, "READY", "PENDING"),
    ifelse(run_state$pending_amstar_items == 0, "READY", "PENDING"),
    ifelse(run_state$quality_consensus_complete, "FINAL_CONSENSUS", "DUAL_REVIEWER_PRECONSENSUS")
  ),
  pending_cells = c(
    run_state$pending_robis,
    run_state$pending_amstar_applicability,
    run_state$pending_amstar_confidence,
    run_state$pending_amstar_scope,
    run_state$pending_amstar_items,
    NA_integer_
  )
)
write_table(quality_readiness, "quality_consensus_readiness.csv", cfg)

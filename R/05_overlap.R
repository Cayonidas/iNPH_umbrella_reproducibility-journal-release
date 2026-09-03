message("Recomputing publication- and cohort-level overlap with explicit coverage...")

dat <- readRDS(file.path(cfg$paths$results, "data", "analysis_data.rds"))
run_state <- readRDS(file.path(cfg$paths$results, "data", "run_state.rds"))

eligible_publication_reviews <- dat$review_audit |>
  filter(norm_token(publication_cca_status) == "INCLUDED") |>
  pull(review_id) |>
  unique()

pub_occ <- dat$occurrence_long |>
  filter(
    review_id %in% eligible_publication_reviews,
    yes_flag(umbrella_eligible),
    !is.na(publication_key_cca),
    publication_key_cca != ""
  ) |>
  distinct(review_id, publication_key_cca, .keep_all = TRUE)

eligible_cohort_reviews <- dat$review_audit |>
  filter(norm_token(cohort_cca_status) == "INCLUDED") |>
  pull(review_id) |>
  unique()

cohort_occ <- dat$occurrence_long |>
  filter(
    review_id %in% eligible_cohort_reviews,
    yes_flag(umbrella_eligible),
    !is.na(cohort_family),
    cohort_family != ""
  ) |>
  distinct(review_id, cohort_family, .keep_all = TRUE)

pub_global <- cca_stat(
  c = n_distinct(pub_occ$review_id),
  N = nrow(pub_occ),
  r = n_distinct(pub_occ$publication_key_cca)
) |>
  mutate(
    unit = "Publication",
    included_reviews = c,
    total_included_reviews = cfg$expected$included_reviews,
    coverage = included_reviews / total_included_reviews,
    interpretation = vapply(cca, cca_label, character(1), cfg = cfg),
    coverage_note = paste0(
      included_reviews, "/", total_included_reviews,
      " reviews with closed or table-bounded lists"
    )
  )

cohort_global <- cca_stat(
  c = n_distinct(cohort_occ$review_id),
  N = nrow(cohort_occ),
  r = n_distinct(cohort_occ$cohort_family)
) |>
  mutate(
    unit = "Conservatively linked cohort family",
    included_reviews = c,
    total_included_reviews = cfg$expected$included_reviews,
    coverage = included_reviews / total_included_reviews,
    interpretation = vapply(cca, cca_label, character(1), cfg = cfg),
    coverage_note = paste0(
      included_reviews, "/", total_included_reviews,
      " reviews with usable cohort-family mapping"
    )
  )

global_overlap <- bind_rows(pub_global, cohort_global) |>
  mutate(
    source_workbook_N = case_when(
      unit == "Publication" ~ cfg$expected$publication_cca_occurrences,
      unit == "Conservatively linked cohort family" ~ run_state$source_cohort_summary$N,
      TRUE ~ NA_real_
    ),
    source_workbook_cca = case_when(
      unit == "Publication" ~ cfg$expected$publication_cca,
      unit == "Conservatively linked cohort family" ~ run_state$source_cohort_summary$cca,
      TRUE ~ NA_real_
    ),
    audit_note = case_when(
      unit == "Conservatively linked cohort family" & source_workbook_N != N ~
        "Corrected: two SINPHONI-2 companion publications in FT-002 form one review-by-cohort cell.",
      TRUE ~ "Recomputed value agrees with the source summary."
    )
  ) |>
  select(
    unit, included_reviews, total_included_reviews, coverage,
    N, r, denominator, cca, interpretation, coverage_note,
    source_workbook_N, source_workbook_cca, audit_note
  )

duplication_burden <- tibble(
  unit = c("Publication", "Conservatively linked cohort family"),
  total_occurrences = c(nrow(pub_occ), nrow(cohort_occ)),
  unique_units = c(n_distinct(pub_occ$publication_key_cca), n_distinct(cohort_occ$cohort_family)),
  units_repeated_in_multiple_reviews = c(
    pub_occ |> count(publication_key_cca) |> filter(n > 1) |> nrow(),
    cohort_occ |> count(cohort_family) |> filter(n > 1) |> nrow()
  )
) |>
  mutate(
    occurrences_beyond_first = total_occurrences - unique_units,
    proportion_unique_units_repeated = units_repeated_in_multiple_reviews / unique_units,
    proportion_occurrences_beyond_first = occurrences_beyond_first / total_occurrences,
    interpretation = "Intuitive duplication burden among reviews with reconstructable lists; reported with CCA and coverage."
  )

calc_stratified_cca <- function(occ, key_col, stratum_col, review_meta) {
  key_col <- rlang::ensym(key_col)
  stratum_col <- rlang::ensym(stratum_col)
  occ |>
    select(review_id, !!key_col) |>
    left_join(review_meta, by = c("review_id" = "master_id")) |>
    filter(!is.na(!!stratum_col), !!stratum_col != "") |>
    group_by(!!stratum_col) |>
    summarise(
      c = n_distinct(review_id),
      N = n(),
      r = n_distinct(!!key_col),
      .groups = "drop"
    ) |>
    mutate(
      denominator = r * c - r,
      cca = if_else(c > 1 & denominator > 0, (N - r) / denominator, NA_real_),
      interpretation = vapply(cca, cca_label, character(1), cfg = cfg)
    )
}

review_meta <- dat$reviews |>
  transmute(
    master_id,
    layer_group = layer_group(final_layer),
    primary_domain
  )

cca_by_layer <- calc_stratified_cca(
  pub_occ, publication_key_cca, layer_group, review_meta
) |>
  rename(stratum = layer_group) |>
  mutate(stratum_type = "Layer", .before = 1)

cca_by_domain <- calc_stratified_cca(
  pub_occ, publication_key_cca, primary_domain, review_meta
) |>
  rename(stratum = primary_domain) |>
  mutate(stratum_type = "Primary domain", .before = 1)

review_sets <- split(pub_occ$publication_key_cca, pub_occ$review_id)
review_ids <- sort(names(review_sets))

pairwise_upper <- if (length(review_ids) >= 2L) {
  combn(review_ids, 2, simplify = FALSE) |>
    map_dfr(function(pair) {
      a <- unique(review_sets[[pair[1]]])
      b <- unique(review_sets[[pair[2]]])
      intersection <- length(intersect(a, b))
      union_n <- length(union(a, b))
      tibble(
        review_1 = pair[1],
        review_2 = pair[2],
        n_1 = length(a),
        n_2 = length(b),
        shared_publications = intersection,
        union_publications = union_n,
        jaccard = ifelse(union_n > 0, intersection / union_n, NA_real_),
        overlap_coefficient = ifelse(
          min(length(a), length(b)) > 0,
          intersection / min(length(a), length(b)),
          NA_real_
        )
      )
    })
} else {
  tibble()
}

pairwise_plot <- bind_rows(
  pairwise_upper |>
    select(review_1, review_2, jaccard, shared_publications),
  pairwise_upper |>
    transmute(
      review_1 = review_2,
      review_2 = review_1,
      jaccard,
      shared_publications
    ),
  tibble(
    review_1 = review_ids,
    review_2 = review_ids,
    jaccard = 1,
    shared_publications = vapply(review_sets[review_ids], length, integer(1))
  )
)

shared_publication_table <- pub_occ |>
  group_by(publication_key_cca) |>
  summarise(
    review_count = n_distinct(review_id),
    review_ids = paste(sort(unique(review_id)), collapse = "; "),
    first_author = first(na.omit(first_author), default = NA_character_),
    year = first(na.omit(year), default = NA_character_),
    representative_title = first(na.omit(title), default = NA_character_),
    .groups = "drop"
  ) |>
  arrange(desc(review_count), publication_key_cca)

coverage_table <- dat$review_audit |>
  transmute(
    review_id,
    layer,
    primary_domain,
    publication_cca_status = norm_token(publication_cca_status),
    cohort_cca_status = norm_token(cohort_cca_status),
    coverage_status,
    reason_or_next_action
  ) |>
  mutate(
    publication_included = publication_cca_status == "INCLUDED",
    cohort_included = cohort_cca_status == "INCLUDED"
  )

cell_proxy <- dat$cell_proxy_cca |>
  filter(!is.na(cell), cell != "") |>
  mutate(
    c_num = parse_num(c),
    N_num = parse_num(n),
    r_num = parse_num(r),
    cca_num = parse_num(cca),
    requested_n = count_ft_ids(requested_reviews),
    eligible_n = count_ft_ids(cca_eligible_reviews),
    coverage_fraction = if_else(requested_n > 0, eligible_n / requested_n, NA_real_),
    interpretation = vapply(cca_num, cca_label, character(1), cfg = cfg),
    limitation = "Review-level proxy; it does not identify which primary studies contributed to each effect estimate."
  )

ai_overlap_long <- readr::read_csv(
  cfg$paths$targeted_ai_overlap,
  show_col_types = FALSE,
  col_types = readr::cols(.default = readr::col_character())
) |>
  mutate(direct_post_shunt_reference = yes_flag(direct_post_shunt_reference))

calculate_targeted_ai_overlap <- function(x, scenario) {
  stats <- cca_stat(
    c = n_distinct(x$review_id),
    N = nrow(x),
    r = n_distinct(x$study_key)
  )
  stats |>
    mutate(
      analysis_cell = "AI_MODELS",
      scenario = scenario,
      requested_reviews = "FT-021; FT-027",
      eligible_reviews = paste(sort(unique(x$review_id)), collapse = "; "),
      coverage_fraction = n_distinct(x$review_id) / 2,
      interpretation = vapply(cca, cca_label, character(1), cfg = cfg),
      status = "EXACT_CELL_SPECIFIC_RECOVERY",
      scope_note = "Verified from explicit included-study tables; does not replace global CCA."
    )
}

targeted_ai_overlap <- bind_rows(
  calculate_targeted_ai_overlap(ai_overlap_long, "ALL_RESPONSE_REFERENCE_STANDARDS"),
  calculate_targeted_ai_overlap(
    ai_overlap_long |> filter(direct_post_shunt_reference),
    "DIRECT_POST_SHUNT_REFERENCE_ONLY"
  )
)

if (nrow(targeted_ai_overlap) != 2L ||
    any(targeted_ai_overlap$c != 2) ||
    any(abs(targeted_ai_overlap$cca - 0.25) > 1e-12) ||
    any(targeted_ai_overlap$coverage_fraction != 1)) {
  stop("Targeted AI overlap invariants failed; inspect the verified study matrix.", call. = FALSE)
}

overlap_recovery_queue <- cell_proxy |>
  transmute(
    analysis_cell = cell,
    requested_n,
    eligible_n,
    coverage_fraction,
    current_proxy_cca = cca_num,
    priority = case_when(
      cell == "AI_MODELS" ~ "COMPLETED_TARGETED_RECOVERY",
      cell %in% c("TAP_TEST_DTA", "LIT_DTA", "ELD_DTA", "ROUT_THRESHOLDS") ~ "HIGH",
      cell %in% c("STRUCTURAL_IMAGING", "BENEFIT_HARM_DURABILITY") ~ "HIGH",
      requested_n > eligible_n ~ "MEDIUM",
      TRUE ~ "LOW"
    ),
    next_action = case_when(
      cell == "AI_MODELS" ~ "Use exact targeted table; retain review-level proxy only as a historical coverage audit.",
      requested_n > eligible_n ~ "Recover explicit cell-contributing primary-study lists and verify cohort identity before exact CCA.",
      TRUE ~ "No additional recovery currently prioritized."
    ),
    guardrail = "Queue status is not evidence; unrecovered cells remain coverage-limited."
  ) |>
  arrange(
    factor(priority, levels = c("COMPLETED_TARGETED_RECOVERY", "HIGH", "MEDIUM", "LOW")),
    coverage_fraction,
    analysis_cell
  )

write_table(global_overlap, "overlap_global_cca_with_coverage.csv", cfg)
write_table(duplication_burden, "overlap_intuitive_duplication_burden.csv", cfg)
write_table(cca_by_layer, "overlap_cca_by_layer.csv", cfg)
write_table(cca_by_domain, "overlap_cca_by_primary_domain.csv", cfg)
write_table(pairwise_upper, "overlap_pairwise_publications.csv", cfg)
write_table(shared_publication_table, "overlap_shared_publications.csv", cfg)
write_table(coverage_table, "overlap_review_coverage.csv", cfg)
write_table(cell_proxy, "overlap_cell_proxy_cca.csv", cfg)
write_table(ai_overlap_long, "overlap_ai_verified_study_matrix.csv", cfg)
write_table(targeted_ai_overlap, "overlap_ai_targeted_exact_cca.csv", cfg)
write_table(overlap_recovery_queue, "overlap_cell_specific_recovery_queue.csv", cfg)

overlap_summary <- list(
  global = global_overlap,
  by_layer = cca_by_layer,
  by_domain = cca_by_domain,
  pairwise = pairwise_upper,
  cell_proxy = cell_proxy,
  targeted_ai = targeted_ai_overlap,
  duplication_burden = duplication_burden,
  publication_occurrences = pub_occ,
  cohort_occurrences = cohort_occ
)
saveRDS(overlap_summary, file.path(cfg$paths$results, "data", "overlap_summary.rds"))

p_overlap <- pairwise_plot |>
  mutate(
    review_1 = factor(review_1, levels = review_ids),
    review_2 = factor(review_2, levels = rev(review_ids))
  ) |>
  ggplot(aes(x = review_1, y = review_2, fill = jaccard)) +
  geom_tile(colour = "white", linewidth = 0.2) +
  scale_fill_gradientn(
    colours = c("#F7FBFF", "#9ECAE1", "#3182BD", "#17365D"),
    values = scales::rescale(c(0, 0.05, 0.20, 1)),
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Pairwise publication overlap among reconstructable reviews",
    subtitle = paste0(
      "Jaccard similarity; coverage ", length(review_ids), "/",
      cfg$expected$included_reviews, " included reviews"
    ),
    x = NULL,
    y = NULL,
    fill = "Jaccard"
  ) +
  theme_publication(base_size = 8) +
  theme(
    axis.text.x = element_text(angle = 55, hjust = 1),
    panel.grid = element_blank()
  )

save_plot_dual(p_overlap, "figure_06_pairwise_overlap_heatmap", cfg, width = 10.5, height = 9.5)

cell_plot_data <- cell_proxy |>
  filter(!is.na(cca_num)) |>
  arrange(cca_num) |>
  mutate(cell = factor(cell, levels = cell))

p_cell_cca <- ggplot(cell_plot_data, aes(x = cca_num, y = cell, colour = coverage_fraction)) +
  geom_segment(aes(x = 0, xend = cca_num, yend = cell), colour = "#D9E5EC", linewidth = 0.8) +
  geom_point(size = 3.2) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), expand = expansion(mult = c(0, 0.08))) +
  scale_colour_gradient(low = "#C00000", high = "#0F6B78", labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Local overlap can be high despite low overall CCA",
    subtitle = "Cell estimates are review-level proxies and must be interpreted with their coverage",
    x = "Corrected covered area",
    y = NULL,
    colour = "Review-list coverage"
  ) +
  theme_publication()

save_plot_dual(p_cell_cca, "figure_07_cell_proxy_cca", cfg, width = 10.5, height = 7)

required_packages <- c(
  "digest", "dplyr", "ggplot2", "jsonlite", "patchwork", "purrr",
  "readr", "readxl", "rlang", "scales", "stringr", "tibble", "tidyr", "yaml"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  stop(
    "Missing required R packages: ", paste(missing_packages, collapse = ", "),
    ". Run source(\"install_packages.R\") first.",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(patchwork)
  library(purrr)
  library(readr)
  library(stringr)
  library(tibble)
  library(tidyr)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

clean_names_base <- function(x) {
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x, perl = TRUE)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  # Preserve RoB (risk of bias) as one canonical acronym. The generic
  # camel-case rule above turns headers such as `Primary_RoB_assessed` into
  # `primary_ro_b_assessed`. Downstream code uses the stable name `rob`.
  x <- gsub("(^|_)ro_b($|_)", "\\1rob\\2", x, perl = TRUE)
  # Preserve iNPH as one canonical acronym. The generic camel-case rule above
  # turns headers such as `Validated_for_iNPH` into `validated_for_i_nph`.
  # Downstream code uses the stable, spelling-independent name `inph`.
  x <- gsub("(^|_)i_nph($|_)", "\\1inph\\2", x, perl = TRUE)
  make.unique(x, sep = "_")
}

require_columns <- function(data, required, dataset_name) {
  missing <- setdiff(required, names(data))
  if (length(missing) > 0L) {
    stop(
      "Input schema mismatch in ", dataset_name, ": missing required column(s): ",
      paste(missing, collapse = ", "),
      ". Available columns: ", paste(names(data), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(data)
}

drop_empty_rows <- function(x) {
  if (nrow(x) == 0L) return(x)
  keep <- rowSums(!is.na(x) & trimws(as.matrix(x)) != "") > 0L
  x[keep, , drop = FALSE]
}

read_sheet_text <- function(path, sheet, skip = 0L) {
  out <- readxl::read_excel(
    path = path,
    sheet = sheet,
    skip = skip,
    col_types = "text",
    .name_repair = "minimal"
  )
  names(out) <- clean_names_base(names(out))
  out <- drop_empty_rows(as.data.frame(out, stringsAsFactors = FALSE))
  tibble::as_tibble(out)
}

norm_token <- function(x) {
  x <- as.character(x)
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- toupper(trimws(x))
  x <- gsub("[[:space:]]+", "_", x)
  x <- gsub("[^A-Z0-9_+/-]+", "", x)
  x[x %in% c("", "NA", "N/A", "NULL", "NONE")] <- NA_character_
  x
}

resolve_quality_consensus <- function(
  reviewer_a,
  reviewer_b,
  adjudicated = NA_character_,
  explicit_final = NA_character_,
  prefer_explicit = FALSE
) {
  reviewer_a <- norm_token(reviewer_a)
  reviewer_b <- norm_token(reviewer_b)
  adjudicated <- norm_token(adjudicated)
  explicit_final <- norm_token(explicit_final)

  exact_agreement <- !is.na(reviewer_a) & !is.na(reviewer_b) & reviewer_a == reviewer_b
  reconstructed <- ifelse(exact_agreement, reviewer_a, adjudicated)

  if (isTRUE(prefer_explicit)) {
    ifelse(!is.na(explicit_final), explicit_final, reconstructed)
  } else {
    reconstructed
  }
}

is_missing_token <- function(x) {
  z <- norm_token(x)
  is.na(z) | z %in% c(
    "NR", "NOT_REPORTED", "NOT_REPORTED/UNCLEAR", "NAO_EXTRAIVEL",
    "INDETERMINADO", "UNKNOWN", "NOT_APPLICABLE", "GENERALLY_NOT_REPORTED"
  )
}

parse_num <- function(x) {
  x <- as.character(x)
  x[is_missing_token(x)] <- NA_character_
  suppressWarnings(readr::parse_number(x, locale = readr::locale(decimal_mark = ".")))
}

yes_flag <- function(x) {
  norm_token(x) %in% c("YES", "SIM", "TRUE", "1", "INCLUDE", "INCLUDED")
}

clearly_yes_flag <- function(x) {
  # For methodological completeness, count only an unqualified affirmative.
  # Narrative details after YES are allowed, but partial or unclear conduct is
  # deliberately not upgraded to complete reporting.
  z <- norm_token(x)
  !is.na(z) &
    str_detect(z, "^(YES|SIM)($|_)") &
    !str_detect(z, "PARTIAL|UNCLEAR|LIMITED|NO/|/NO")
}

extract_ft_ids <- function(x) {
  if (length(x) == 0L || all(is.na(x))) return(character())
  ids <- unlist(stringr::str_extract_all(paste(x, collapse = ";"), "FT-[0-9]{3}"))
  unique(ids)
}

count_ft_ids <- function(x) {
  vapply(
    x,
    function(one) length(extract_ft_ids(one)),
    integer(1)
  )
}

probability_to_odds <- function(p) {
  p <- as.numeric(p)
  ifelse(is.na(p) | p < 0 | p > 1, NA_real_, p / (1 - p))
}

odds_to_probability <- function(o) {
  o <- as.numeric(o)
  ifelse(is.na(o) | o < 0, NA_real_, o / (1 + o))
}

post_test_probability <- function(pretest_probability, likelihood_ratio) {
  odds_to_probability(probability_to_odds(pretest_probability) * likelihood_ratio)
}

predictive_values <- function(sensitivity, specificity, prevalence) {
  sensitivity <- as.numeric(sensitivity)
  specificity <- as.numeric(specificity)
  prevalence <- as.numeric(prevalence)
  ppv_denominator <- sensitivity * prevalence + (1 - specificity) * (1 - prevalence)
  npv_denominator <- specificity * (1 - prevalence) + (1 - sensitivity) * prevalence
  tibble(
    ppv = ifelse(ppv_denominator > 0, sensitivity * prevalence / ppv_denominator, NA_real_),
    npv = ifelse(npv_denominator > 0, specificity * (1 - prevalence) / npv_denominator, NA_real_)
  )
}

implied_prevalence_from_npv <- function(sensitivity, specificity, npv) {
  numerator <- specificity * (1 - npv)
  denominator <- npv * (1 - sensitivity) + numerator
  ifelse(denominator > 0, numerator / denominator, NA_real_)
}

valid_finding_id <- function(x) {
  # Most findings use sequential IDs (for example, F-0001). The frozen
  # extraction also contains a deliberately semantic survival ID,
  # F-019-SURV. Accept both documented schemas, but do not silently accept
  # arbitrary labels.
  x <- as.character(x)
  stringr::str_detect(x, "^F-[0-9]{4}$") |
    stringr::str_detect(x, "^F-[0-9]{3}-[A-Z0-9][A-Z0-9_-]*$")
}

split_semicolon <- function(x) {
  if (length(x) == 0L || is.na(x) || trimws(x) == "") return(character())
  out <- unlist(strsplit(as.character(x), "\\s*;\\s*"))
  out[nzchar(out)]
}

cca_stat <- function(c, N, r) {
  c <- as.numeric(c)
  N <- as.numeric(N)
  r <- as.numeric(r)
  denominator <- r * c - r
  cca <- ifelse(c <= 1 | r <= 0 | denominator <= 0, NA_real_, (N - r) / denominator)
  tibble(c = c, N = N, r = r, denominator = denominator, cca = cca)
}

cca_label <- function(cca, cfg = NULL) {
  if (is.na(cca)) return("NOT_ESTIMABLE")
  limits <- if (!is.null(cfg)) cfg$cca else list(
    slight_upper_percent = 5,
    moderate_upper_percent = 10,
    high_upper_percent = 15
  )
  pct <- 100 * cca
  dplyr::case_when(
    pct <= limits$slight_upper_percent ~ "SLIGHT",
    pct <= limits$moderate_upper_percent ~ "MODERATE",
    pct <= limits$high_upper_percent ~ "HIGH",
    TRUE ~ "VERY_HIGH"
  )
}

safe_kappa <- function(x, y, levels = NULL, weights = c("unweighted", "quadratic")) {
  weights <- match.arg(weights)
  keep <- !is.na(x) & !is.na(y)
  x <- as.character(x[keep])
  y <- as.character(y[keep])
  if (length(x) == 0L) return(NA_real_)
  if (is.null(levels)) levels <- sort(unique(c(x, y)))
  x <- factor(x, levels = levels)
  y <- factor(y, levels = levels)
  tab <- table(x, y)
  n <- sum(tab)
  k <- length(levels)
  if (n == 0L || k < 2L) return(NA_real_)
  if (weights == "unweighted") {
    w <- diag(k)
  } else {
    idx <- seq_len(k)
    w <- 1 - (outer(idx, idx, "-") / (k - 1))^2
  }
  p_obs <- sum(w * tab) / n
  p_exp_matrix <- outer(rowSums(tab), colSums(tab)) / n^2
  p_exp <- sum(w * p_exp_matrix)
  if (isTRUE(all.equal(p_exp, 1))) {
    return(ifelse(isTRUE(all.equal(p_obs, 1)), 1, NA_real_))
  }
  (p_obs - p_exp) / (1 - p_exp)
}

kappa_summary <- function(x, y, levels = NULL, weights = "unweighted") {
  keep <- !is.na(x) & !is.na(y)
  n <- sum(keep)
  exact <- if (n == 0L) NA_real_ else mean(as.character(x[keep]) == as.character(y[keep]))
  tibble(
    n = n,
    exact_agreement = exact,
    kappa = safe_kappa(x, y, levels = levels, weights = weights),
    weighting = weights
  )
}

effect_family <- function(measure) {
  z <- norm_token(measure)
  dplyr::case_when(
    grepl("RISK_DIFFERENCE|DIFFERENCE_IN|MD$|SMD$|SMCR|HEDGES|BETA", z) ~ "DIFFERENCE",
    grepl("(^|_)OR$|(^|_)RR$|(^|_)HR$|DOR|ODDS_RATIO|RISK_RATIO|HAZARD_RATIO", z) ~ "RATIO",
    grepl("SENSITIVITY|SPECIFICITY|PPV|NPV|AUC|ACCURACY|PROPORTION|PREVALENCE|PROBABILITY|RISK_%", z) ~ "PROBABILITY",
    TRUE ~ "OTHER"
  )
}

effect_null <- function(family) {
  dplyr::case_when(
    family == "RATIO" ~ 1,
    family == "DIFFERENCE" ~ 0,
    TRUE ~ NA_real_
  )
}

normalize_probability_scale <- function(value, family) {
  value <- as.numeric(value)
  ifelse(family == "PROBABILITY" & !is.na(value) & abs(value) > 1, value / 100, value)
}

outcome_group <- function(x) {
  z <- norm_token(x)
  dplyr::case_when(
    grepl("GAIT|MOBILITY|BALANCE|WALK", z) ~ "Gait / mobility",
    grepl("COGNIT|NEUROPSYCH", z) ~ "Cognition",
    grepl("URIN|INCONTIN", z) ~ "Urinary",
    grepl("QUALITY_OF_LIFE|QOL", z) ~ "Quality of life",
    grepl("SURVIVAL|MORTAL", z) ~ "Survival",
    grepl("COMPLICATION|HARM|REOPERATION", z) ~ "Harm / durability",
    grepl("NEUROPSYCHIATR", z) ~ "Neuropsychiatric",
    TRUE ~ "Global / mixed"
  )
}

layer_group <- function(x) {
  z <- norm_token(x)
  dplyr::case_when(
    z %in% c("A", "A-PARCIAL", "A_PARTIAL", "A_PREDICTION") ~ "Prediction",
    z %in% c("B", "B_RESPONSE_MEASUREMENT") ~ "Response measurement",
    z %in% c("A+B", "A_B", "BOTH") ~ "Both",
    TRUE ~ "Other"
  )
}

sanitize_filename <- function(x) {
  x <- iconv(as.character(x), from = "", to = "ASCII//TRANSLIT")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  substr(x, 1, 120)
}

theme_publication <- function(base_size = 10) {
  ggplot2::theme_minimal(base_size = base_size, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold", colour = "#17365D"),
      plot.subtitle = ggplot2::element_text(colour = "#4B5563"),
      axis.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(face = "bold", colour = "#17365D")
    )
}

save_plot_dual <- function(plot, stem, cfg, width = NULL, height = NULL) {
  width <- width %||% cfg$analysis$figure_width
  height <- height %||% cfg$analysis$figure_height
  figure_dir <- file.path(cfg$paths$results, "figures")
  dir.create(figure_dir, recursive = TRUE, showWarnings = FALSE)
  png_path <- file.path(figure_dir, paste0(stem, ".png"))
  pdf_path <- file.path(figure_dir, paste0(stem, ".pdf"))
  ggplot2::ggsave(
    png_path, plot = plot, width = width, height = height,
    units = "in", dpi = cfg$analysis$figure_dpi, bg = "white"
  )
  ggplot2::ggsave(
    pdf_path, plot = plot, width = width, height = height,
    units = "in",
    device = if (isTRUE(capabilities("cairo"))) grDevices::cairo_pdf else grDevices::pdf,
    bg = "white"
  )
  invisible(c(png_path, pdf_path))
}

write_table <- function(x, filename, cfg) {
  path <- file.path(cfg$paths$results, "tables", filename)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path, na = "")
  invisible(path)
}

write_data <- function(x, filename, cfg) {
  path <- file.path(cfg$paths$results, "data", filename)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(x, path, na = "")
  invisible(path)
}

append_log <- function(text, filename, cfg) {
  path <- file.path(cfg$paths$results, "logs", filename)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  cat(text, "\n", file = path, append = TRUE)
  invisible(path)
}

binom_exact_ci <- function(events, total, conf.level = 0.95) {
  if (is.na(events) || is.na(total) || total <= 0 || events < 0 || events > total) {
    return(c(estimate = NA_real_, low = NA_real_, high = NA_real_))
  }
  test <- stats::binom.test(events, total, conf.level = conf.level)
  c(estimate = events / total, low = test$conf.int[1], high = test$conf.int[2])
}

if (file.exists("config.yml")) {
  cfg <- yaml::read_yaml("config.yml")
  result_dirs <- file.path(
    cfg$paths$results,
    c("data", "tables", "figures", "models", "logs")
  )
  invisible(lapply(result_dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
  ggplot2::theme_set(theme_publication())
}

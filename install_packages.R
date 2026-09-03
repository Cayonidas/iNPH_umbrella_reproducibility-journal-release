required <- c(
  "digest", "dplyr", "ggplot2", "jsonlite", "patchwork", "purrr",
  "readr", "readxl", "rlang", "scales", "stringr", "tibble", "tidyr", "yaml"
)

recommended <- c("knitr", "rmarkdown", "testthat", "renv")
optional_dta <- c("mada", "metafor")

installed <- rownames(installed.packages())
missing_required <- setdiff(required, installed)
missing_recommended <- setdiff(recommended, installed)

if (length(missing_required) > 0L) {
  install.packages(missing_required, repos = "https://cloud.r-project.org")
}

if (length(missing_recommended) > 0L) {
  message(
    "Recommended packages not installed: ", paste(missing_recommended, collapse = ", "),
    ". Installing them for the HTML report, tests, and environment capture."
  )
  install.packages(missing_recommended, repos = "https://cloud.r-project.org")
}

if (identical(tolower(Sys.getenv("INSTALL_OPTIONAL_DTA")), "true")) {
  missing_dta <- setdiff(optional_dta, rownames(installed.packages()))
  if (length(missing_dta) > 0L) {
    message("Installing optional diagnostic-meta-analysis packages: ", paste(missing_dta, collapse = ", "))
    install.packages(missing_dta, repos = "https://cloud.r-project.org")
  }
} else {
  message("Optional DTA packages not requested; set INSTALL_OPTIONAL_DTA=true to install them.")
}

message("Package check complete.")

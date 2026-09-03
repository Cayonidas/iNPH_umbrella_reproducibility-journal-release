options(stringsAsFactors = FALSE)

if (!file.exists("DESCRIPTION") || !file.exists("config.yml")) {
  stop("Run this script from the repository root.", call. = FALSE)
}

if (!file.exists("results/logs/release_validation.csv")) {
  stop("Run the full pipeline and scripts/validate_outputs.R before capturing the release environment.", call. = FALSE)
}

validation <- read.csv("results/logs/release_validation.csv", stringsAsFactors = FALSE)
if (!all(validation$passed)) {
  stop("The release validation contains failures; the environment will not be frozen.", call. = FALSE)
}

dir.create("environment", recursive = TRUE, showWarnings = FALSE)
capture.output(sessionInfo(), file = "environment/session_info.txt")

desc <- read.dcf("DESCRIPTION")
dependency_fields <- intersect(c("Depends", "Imports", "Suggests"), colnames(desc))
dependency_text <- paste(desc[1, dependency_fields], collapse = ",")
dependencies <- trimws(unlist(strsplit(dependency_text, ",", fixed = TRUE)))
dependencies <- gsub("\\s*\\([^)]*\\)", "", dependencies)
dependencies <- unique(dependencies[nzchar(dependencies) & dependencies != "R"])

installed <- rownames(installed.packages())
package_versions <- data.frame(
  package = c("R", dependencies),
  version = c(as.character(getRversion()), vapply(
    dependencies,
    function(pkg) if (pkg %in% installed) as.character(packageVersion(pkg)) else NA_character_,
    character(1)
  )),
  stringsAsFactors = FALSE
)
write.csv(package_versions, "environment/package_versions.csv", row.names = FALSE, na = "NOT_INSTALLED")

if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv", repos = "https://cloud.r-project.org")
}

renv::snapshot(
  project = ".",
  lockfile = "renv.lock",
  type = "explicit",
  prompt = FALSE,
  force = TRUE
)

required <- c("environment/session_info.txt", "environment/package_versions.csv", "renv.lock")
if (any(!file.exists(required)) || any(file.info(required)$size <= 0)) {
  stop("Environment capture did not create every required non-empty file.", call. = FALSE)
}

message("Environment capture complete: session_info.txt, package_versions.csv, and renv.lock.")


# Clean reproduction in Google Colab

Use this route if a separate local R installation is unavailable. The Colab
runtime is independent of the machine used to prepare the repository, which
makes it suitable for the final clean-run gate.

## 1. Start from the public repository

Create a new Colab notebook, change the runtime to **R**, and run:

```r
system("git clone https://github.com/<OWNER>/inph-shunt-ruleout-umbrella.git")
setwd("inph-shunt-ruleout-umbrella")
system("git status --short")
```

Replace `<OWNER>` after the repository has been created. The status output
should be empty.

## 2. Install, reproduce, and validate

```r
source("install_packages.R")
source("tests/testthat.R")
source("run_all.R")
source("scripts/validate_outputs.R")
source("scripts/capture_environment.R")
```

Then run the release-mode structural audit:

```r
status <- system("python scripts/static_release_check.py --release")
stopifnot(status == 0)
```

Do not continue if any step fails or if any frozen invariant differs.

## 3. Retrieve the authentic environment files

Download these files from the Colab file browser:

- `renv.lock`
- `environment/session_info.txt`
- `environment/package_versions.csv`
- `results/logs/release_validation.csv`

Commit the first three files to the repository. Retain the validation log with
the release records; the generated `results/` directory remains excluded from
version control.

## 4. Confirm a clean release tree

After committing the environment capture, make a fresh clone and repeat:

```r
renv::restore(prompt = FALSE)
source("tests/testthat.R")
source("run_all.R")
source("scripts/validate_outputs.R")
status <- system("python scripts/static_release_check.py --release")
stopifnot(status == 0)
```

Only tag `v1.2.3-journal-release` after this second run passes.

# Environment capture

This directory is populated by `scripts/capture_environment.R` **after** a successful clean run.

The script creates:

- `session_info.txt` — R and platform session information;
- `package_versions.csv` — exact versions of declared dependencies found in the successful environment;
- root-level `renv.lock` — a dependency lockfile produced from that environment.

These files are not prefilled because an environment record must describe the runtime that actually reproduced the analysis. Do not replace them with guessed package versions.


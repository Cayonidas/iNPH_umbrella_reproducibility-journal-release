# Reproducibility archive

This repository contains the analytic datasets and R code supporting:

> **Can negative tests rule out shunt benefit in idiopathic normal-pressure hydrocephalus? An umbrella review of decision validity**

- PROSPERO: **CRD420261494316** (retrospective registration)
- Scientific analysis version: **1.2.3**
- Public release tag: **v1.2.3-journal-release**
- Base data freeze: **11 August 2026**
- Targeted FT-012 source update: **1 September 2026**

## What this release reproduces

The pipeline imports the final adjudicated extraction, overlap, and author-consensus quality workbooks; validates the frozen study counts and identifiers; and reconstructs the evidence map, response-definition audit, overlap analyses, anchor hierarchy, sensitivity analyses, probability translations, clinical matrix, manuscript-facing tables, and figures.

The analytic unit is the systematic review. Review-level estimates are not treated as independent primary studies and are never pooled across reviews. One outcome-blind anchor review is selected per exact clinical microcell; other matched estimates inform concordance and robustness only.

The `expected_outputs/` directory contains the manuscript-facing snapshot used to audit a new run. It is a reference snapshot, not an alternative input to the analysis.

## Repository structure

```text
.
├── README.md
├── LICENSE
├── DATA_LICENSE.md
├── CITATION.cff
├── CHANGELOG.md
├── DESCRIPTION
├── config.yml
├── install_packages.R
├── run_all.R
├── R/
├── data/
│   ├── raw/
│   └── manual/
├── docs/
├── environment/
├── expected_outputs/
├── report/
├── scripts/
├── tests/
└── included_reviews_41.ris
```

Development-only, superseded, paywalled, and non-analytic comparison files are deliberately excluded from this public release. See `docs/REPRODUCIBILITY_SCOPE.md`.

## Requirements

- R **4.2 or later**
- Internet access during first-time package installation
- A system capable of opening `.xlsx` workbooks

Required R packages are declared in `DESCRIPTION` and checked by the pipeline. The optional HTML report and diagnostic-accuracy module use packages listed under `Suggests`.

## Quick start

From a fresh clone or extraction:

```bash
Rscript install_packages.R
Rscript run_all.R
Rscript scripts/validate_outputs.R
```

In RStudio, open `iNPH_umbrella_analysis.Rproj` and run the same scripts with `source()`.

For a browser-based clean run, see `docs/COLAB_CLEAN_RUN.md`.

The pipeline writes a new `results/` directory. Do not copy `results/` from an older release into a clean run.

## Expected validation invariants

A successful reproduction must confirm:

| Invariant | Expected value |
| --- | ---: |
| Included reviews | 41 |
| Structured findings | 192 |
| Quantitative estimates | 155 |
| Response definitions | 139 |
| Anchor microcells | 64 |
| Final ROBIS high risk | 32/41 |
| AMSTAR 2 critically low | 14/15 applicable |
| Publication-level CCA | 1.50% |
| Cohort-family CCA | 1.48% |
| High-level cells dependent on one anchor review | 10/11 |

The diagnostic-accuracy module is expected to report `NOT_READY_NO_PRIMARY_2X2_DATA`; the supplied 2×2 template is intentionally empty.

## FT-012 provenance

FT-012 is retained as an accepted, peer-reviewed Frontiers in Neurology source. Four quantitative accuracy values were updated from the official accepted abstract on 1 September 2026. The final formatted full text was not yet available; full-text-only structural fields and the identifiable 14-study overlap list therefore retain complete-preprint provenance. The accepted abstract reports 15 studies overall; the unidentified fifteenth study was not imputed. FT-012 exclusion is presented only as a single-anchor source-dependence analysis.

## Quality-appraisal provenance

The primary analysis reads only `data/raw/iNPH_ROBIS_AMSTAR2_final_author_consensus_2026-08-11.xlsx`. This workbook contains the final human author consensus used for every ROBIS- and AMSTAR 2-dependent result. Reviewer-specific fields remain available for transparency.

An additional AI-assisted ROBIS/AMSTAR 2 comparison was conducted as an internal quality-control exercise after the human workflow. It was technically segregated and did not contribute to final judgments, anchor selection, sensitivity analyses, or reported conclusions. Those non-analytic development artifacts are not required to reproduce the manuscript-facing analyses and are not distributed in this public release.

The filename `targeted_ai_overlap_verified.csv` uses **AI** to mean *artificial-intelligence prediction studies*. It is a scientific overlap input for FT-021 and FT-027 and is unrelated to generative-AI quality appraisal.

## Environment capture and final release gate

After the pipeline and validation pass in the environment that will support the public release, run:

```bash
Rscript scripts/capture_environment.R
```

This writes `environment/session_info.txt`, `environment/package_versions.csv`, and a real `renv.lock`. These files must be generated from the successful clean run; they are intentionally not fabricated by repository-cleaning tools. Commit them before creating the GitHub release and Zenodo snapshot.

Then run the final release-mode structural gate:

```bash
python scripts/static_release_check.py --release
```

The release-mode gate is intentionally stricter than the ordinary structural
audit: it fails until the environment files have been generated by a successful
clean run.

## Outputs

- `results/analysis_report.html`: navigable report when `rmarkdown` is available
- `results/RUN_SUMMARY.md`: human-readable run summary
- `results/analysis_run_summary.json`: machine-readable run summary
- `results/data/`: validated analytic datasets and input manifest
- `results/tables/`: manuscript and supplementary tables
- `results/figures/`: 400-dpi PNG and vector PDF figures
- `results/logs/`: validation, report status, and R session information

## Data and copyright

Author-generated code is licensed under MIT. Derived analytic datasets and documentation are licensed under CC BY 4.0; see `DATA_LICENSE.md`. Full-text source publications are not redistributed and remain subject to their respective rights holders.

## Citation

Use the metadata in `CITATION.cff`. After Zenodo archives the tagged release, cite the version-specific Zenodo DOI and update the manuscript Data Availability Statement with that DOI.

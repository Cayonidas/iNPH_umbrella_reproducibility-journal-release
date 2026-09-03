# Build validation record

Public release preparation date: **2 September 2026**  
Scientific analysis version: **1.2.3**  
Release tag: **v1.2.3-journal-release**

## Source-workbook integrity

The quality workbook included in an earlier submission archive was only 25,217 bytes and lacked a valid ZIP central directory. It could not be opened as an `.xlsx` file and would cause a clean run to fail.

The public release restores the intact 318,636-byte author-consensus workbook from the validated pre-submission package:

`data/raw/iNPH_ROBIS_AMSTAR2_final_author_consensus_2026-08-11.xlsx`

The restored workbook passes ZIP integrity checking and contains all ten expected worksheets, including `Review_Consensus`, `ROBIS_Signalling`, `AMSTAR2_Summary`, and `AMSTAR2_Items`.

## Frozen scientific invariants

The manuscript-facing expected-output snapshot records:

- 41 included reviews;
- 192 structured findings;
- 155 quantitative estimates;
- 139 response definitions;
- 64 selected anchor microcells;
- final ROBIS consensus of 32 high, 4 unclear, and 5 low risk;
- 15 AMSTAR 2-applicable reviews/components, of which 14 are critically low and one low confidence;
- publication-level CCA 0.0150220150 (1.50%);
- cohort-family CCA 0.0147880764 (1.48%);
- 10 of 11 high-level cells dependent on a single anchor review.

## Public-release separation

The public pipeline contains no path, import, join, table, or figure dependent on the internal AI-assisted ROBIS/AMSTAR 2 comparison files. The final human author-consensus workbook remains the sole quality source used analytically. Superseded workbooks and development archives are excluded.

`targeted_ai_overlap_verified.csv` remains in the release because it is an analytic input about artificial-intelligence prediction studies in FT-021 and FT-027.

## Static checks completed during repository cleaning

- Repository structure and required files checked.
- All three source workbooks passed container-integrity tests.
- Required sheets and core worksheet schemas were inspected.
- Expected CSV row counts and key numerical invariants were independently recomputed.
- R dependency declarations were reconciled with namespace use; `rlang` is now explicit.
- R source references to excluded comparison files were removed.
- No article PDF, superseded workbook, development archive, `.Rhistory`, or generated `results/` directory is distributed.
- `CITATION.cff`, licensing, and YAML files passed parser-based checks.
- The release ZIP passed CRC testing and contains one repository root.

## Runtime validation boundary

The repository-cleaning environment did not contain an R runtime. Consequently, the full plotting pipeline was not rerun here and no `renv.lock` or session record was fabricated. The pipeline includes runtime gates in `R/01_import_validate.R`, regression tests, `scripts/validate_outputs.R`, and `scripts/capture_environment.R`.

Before publishing the GitHub release or Zenodo DOI, perform the clean-run procedure in `docs/FINAL_RUN_CHECKLIST.md`. A successful clean run must generate and commit the authentic environment capture.


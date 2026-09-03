# Final clean-run and release checklist

Scientific analysis version: **1.2.3**  
Base freeze: **11 August 2026**  
Targeted FT-012 update: **1 September 2026**  
Release tag: **v1.2.3-journal-release**

## A. Fresh-clone run

1. Clone or extract the release into a new directory.
2. Confirm that no pre-existing `results/` directory is present.
3. Run:

```bash
Rscript install_packages.R
Rscript run_all.R
Rscript scripts/validate_outputs.R
```

4. Confirm that all three commands exit successfully.

## B. Required scientific gates

The run is eligible for release only if all of the following hold:

- `critical_checks_passed` is `true`;
- `quality_consensus_complete` is `true`;
- pending ROBIS cells are 0;
- pending AMSTAR 2 applicability, confidence, summary-union, and item cells are 0;
- 41 reviews, 192 findings, 155 quantitative estimates, 139 response definitions, and 64 anchors are reproduced;
- final ROBIS is 32 high, 4 unclear, and 5 low;
- AMSTAR 2 is 14 critically low and 1 low among 15 applicable reviews/components;
- publication CCA is 1.50% and cohort-family CCA is 1.48%;
- joint exclusion of FT-003 and FT-011 retains 53/64 microcells and 10/11 high-level cells;
- FT-012 exclusion is labelled single-anchor source-dependence robustness, not a publication-status restriction;
- the optional diagnostic meta-analysis status is `NOT_READY_NO_PRIMARY_2X2_DATA`.

Inspect first:

- `results/RUN_SUMMARY.md`;
- `results/analysis_run_summary.json`;
- `results/data/validation_checks.csv`;
- `results/tables/quality_consensus_readiness.csv`;
- `results/tables/manuscript_numbers.csv`.

## C. Environment capture

After the successful run:

```bash
Rscript scripts/capture_environment.R
```

Confirm that these files now exist and are non-empty:

- `environment/session_info.txt`;
- `environment/package_versions.csv`;
- `renv.lock`.

Do not create or edit these files manually.

## D. Static release check

Run:

```bash
python scripts/static_release_check.py --release
```

The script must finish with `OVERALL: PASS`.

## E. Repository and archive identity

- Commit the successful environment capture.
- Confirm there are no article PDFs, superseded workbooks, development archives, or `.Rhistory` files.
- Confirm `README.md` is at repository root.
- Create tag `v1.2.3-journal-release` without manufacturing prior history.
- Archive the tagged release in Zenodo.
- Use the version-specific Zenodo DOI in the manuscript.
- Provide JNNP with a ZIP containing the same scientific content as the tagged release.

## Freeze rule

After all gates pass, later changes are limited to a critical reproducibility error or a documented editor/reviewer request. Other editorial revisions should not change the scientific analysis version.

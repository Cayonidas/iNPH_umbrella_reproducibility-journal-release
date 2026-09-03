# Manual analytic inputs

## Optional primary-study diagnostic-accuracy data

`dta_primary_2x2.csv` is intentionally empty. One row represents one independent study/cohort contribution for one index test and threshold group.

Required fields for quantitative modelling are:

- `study_id`, `cohort_id`, `index_test`, and `threshold_group`;
- integer `tp`, `fp`, `fn`, and `tn`;
- `reference_standard` and `follow_up`;
- `independent_cohort = YES`;
- `reference_standard_compatible = YES`;
- `include_in_primary_dta = YES`.

Use stable cohort IDs across companion publications. Reports with overlapping participants must not be entered as independent studies in the same group. Counts must not be reconstructed from rounded sensitivity or specificity unless all four cells and the denominator can be verified exactly.

The diagnostic module is disabled in `config.yml`; its expected status is `NOT_READY_NO_PRIMARY_2X2_DATA`.

## Report-status provenance

`report_status.csv` records source availability and the accepted-abstract/full-text provenance for FT-012.

## Targeted artificial-intelligence-study overlap

`targeted_ai_overlap_verified.csv` contains only artificial-intelligence prediction studies visible in explicit included-study tables for FT-021 and FT-027. It supports a cell-specific CCA and a direct post-shunt sensitivity; neither estimate replaces global CCA. In this filename, `ai` means artificial-intelligence prediction studies, not a generative-AI tool.


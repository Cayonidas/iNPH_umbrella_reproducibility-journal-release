# Output catalog

The frozen manuscript-facing snapshot is protected by
`expected_outputs/MANIFEST.sha256`. The structural audit verifies every listed
checksum before evaluating numerical invariants.

## Data and audit

- `source_manifest.csv`: file names, sizes, modification times, and SHA-256 hashes.
- `validation_checks.csv`: critical and warning-level checks.
- `analysis_data.rds`: imported analysis objects.
- `method_facts_analysis_ready.csv`: canonicalized review-method fields used in
  the completeness and credibility analyses, with source values unchanged.
- `quality_flags.rds`, `overlap_summary.rds`, `anchor_effects.rds`: reusable intermediate objects.
- `probability_translation.rds`: paired accuracy, post-test scenarios, gatekeeper scenarios, and Rout consistency audit.

## Tables

- evidence map by review/domain/layer;
- evidence map by clinical outcome;
- final-consensus ROBIS and AMSTAR 2 distributions, plus reviewer-specific
  transparency analyses and agreement;
- `robis_consensus_overrides.csv`, a dedicated audit of any explicit final
  author-consensus decision that supersedes mechanical reviewer-pair
  reconstruction;
- outcome-definition taxonomy and clinical-validity indicators;
- CCA at publication and cohort level, coverage, and pairwise overlap;
- anchor microcell audit and review-level quantitative displays;
- clinical translation matrix and analysis-readiness matrix;
- sensitivity/fragility matrix;
- diagnostic meta-analysis feasibility and, if eligible, primary-study estimates.
- probability translation and negative-gatekeeper scenarios;
- Rout NPV/prevalence consistency audit;
- exact targeted AI overlap and intuitive duplication burden;
- directional concordance, completeness/causal audit, and review timeline;
- categorical claim-level credibility profile (explicitly not GRADE);
- figure eligibility index with table-only exclusion reasons.

## Figures

Figures are exported as 400-dpi PNG and vector PDF. They include the evidence
map, final-consensus quality appraisal, supplementary dual-reviewer displays,
response-definition audit, overlap heatmap, probability curves including DESH,
negative-test uncertainty ranges, directional concordance, categorical
credibility profile, partial-year-labelled timeline, and eligible cell-specific
forest-style displays without pooled diamonds.

# Scope of the public reproducibility release

## Included

This release contains the materials required to reproduce the manuscript-facing analysis:

- three final analytic source workbooks;
- three manual analytic inputs, including the intentionally empty diagnostic 2×2 template;
- R analysis modules and tests;
- statistical and provenance documentation;
- a manuscript-facing expected-output snapshot;
- the RIS bibliography of 41 included reviews.

## Deliberately excluded

The following are not part of the public release:

- article PDFs or other publisher-owned full texts;
- superseded 10 August workbooks and development archives;
- `.Rhistory`, `.RData`, temporary, operating-system, or editor files;
- generated `results/` from the repository build;
- an internal AI-assisted ROBIS/AMSTAR 2 comparison exercise.

The internal comparison was performed after the human review workflow as a quality-control exercise. It was technically segregated from the final human consensus and did not enter quality flags, anchor selection, sensitivity analyses, credibility profiles, or reported conclusions. Its omission therefore does not alter or limit reproduction of any manuscript-facing result.

The public analytic source for ROBIS and AMSTAR 2 is exclusively:

`data/raw/iNPH_ROBIS_AMSTAR2_final_author_consensus_2026-08-11.xlsx`

## Files whose names require care

`data/manual/targeted_ai_overlap_verified.csv` is required. Here, **AI** means artificial-intelligence prediction studies included in FT-021 and FT-027. It is unrelated to the excluded generative-AI quality-control comparison.

## Version identity

The scientific analysis remains version **1.2.3**. Repository cleaning does not constitute a new scientific analysis and is labelled with release tag **v1.2.3-journal-release**.


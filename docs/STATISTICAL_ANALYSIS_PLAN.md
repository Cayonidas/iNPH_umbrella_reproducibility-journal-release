# Statistical Analysis Plan

## 1. Objective and unit of analysis

The primary unit is the systematic review. Primary publications and cohort families are used only to characterise dependence and overlap. Published pooled estimates from different reviews are not independent effect estimates and will not be meta-meta-analysed.

## 2. Descriptive evidence map

Review characteristics will be summarised as counts, proportions, medians, and ranges. Participant denominators will not be summed across reviews. Evidence will be mapped by review layer, primary domain, outcome domain, review type, quantitative synthesis, and calendar year.

## 3. Methodological quality

ROBIS is applied to all included reviews. AMSTAR 2 is used only for reviews or separable components within its intended healthcare-intervention scope. No numerical AMSTAR 2 score is calculated.

Before final consensus, Reviewer 1 and Reviewer 2 are displayed separately, with exact and weighted agreement. Provisional judgments are not treated as final. Once every required final field is complete, the pipeline automatically uses consensus judgments.

## 4. Outcome-definition audit

Definitions are classified by level, clinical domain, instrument, threshold, definition class, assessor, patient-reported status, timing, composite rule, iNPH validation, and MCID reporting. A source label of `VALIDATED_THRESHOLD` is retained but is not accepted as validation for iNPH shunt response unless the source explicitly supports that population, construct, threshold, and reference standard. General instrument validation and shunt-response MCID sources are separate indicators; they are not collapsed into a composite score.

## 5. Overlap

Corrected covered area is calculated as:

`CCA = (N - r) / (r*c - r)`

where `c` is the number of reviews, `N` the total number of publication occurrences, and `r` the number of unique publications. CCA is calculated only among reviews with a closed or table-bounded primary-study list. The same procedure is repeated for conservatively linked cohort families. Every estimate reports coverage.

Pairwise shared-publication counts, Jaccard similarity, and overlap coefficients are calculated. Candidate references not verified as included primary studies remain excluded.

The number and proportion of unique units repeated across reviews and occurrences beyond the first are reported with CCA. Exact cell-specific CCA may be recovered only from explicit included-study tables; such estimates remain separate from global CCA and are labelled with their reference-standard scope.

For cohort-family sensitivity, `N` is the number of unique review-by-cohort cells in the binary matrix. Two companion publications from `SINPHONI_2` occurring within `FT-002` therefore contribute one occurrence, not two. This yields 579 review-by-cohort occurrences; the source workbook summary value of 580 is retained only in the audit trail as a pre-correction value.

## 6. Anchor-review synthesis

The analytic microcell is:

`analysis cell × predictor × outcome × time × effect measure × threshold`.

Exactly one anchor review is permitted per microcell. The adjudicated `Umbrella_role` field is authoritative. Supporting reviews are used only for direction, consistency, and sensitivity. Forest-style displays contain no pooled diamond across reviews and never place incompatible measures on the same numerical axis. A group with one estimate is table-only. Unstandardized differences from incompatible cognitive instruments are also table-only.

## 7. Sensitivity analyses

Prespecified analyses include:

1. single-anchor exclusion of `FT-012` as source-dependence robustness; this is not a peer-review-status restriction;
2. exclusion of `FT-003` because members of the umbrella-review team authored that review;
3. restriction to reviews rated low risk by both independent ROBIS assessors before consensus;
4. restriction to reviews not rated high risk by either assessor;
5. effect of requiring at least one threshold explicitly validated for iNPH shunt response;
6. exclusion of own-cohort estimates from hybrid reports;
7. identification of cells wholly dependent on one anchor review.

Because there is no review-level pooling, a sensitivity analysis may remove the only anchor and yield `ANCHOR_LOST`; this is a meaningful fragility result, not a reason to substitute an unmatched estimate.

## 8. Optional de novo diagnostic-accuracy meta-analysis

The diagnostic module is a separate component and uses primary-study 2×2 data only. It is eligible when:

- at least four independent cohorts contribute to the same index-test/threshold group;
- TP, FP, FN, and TN are complete non-negative integers;
- the post-shunt reference standard and follow-up are clinically compatible;
- cohort independence is verified;
- duplicate reports are resolved to a single analytical unit.

When eligible, a bivariate random-effects model/HSROC is fitted with `mada::reitsma`. Individual study sensitivity and specificity with exact confidence intervals are always shown. Groups below the threshold receive descriptive study estimates only. Formal small-study-effect tests are not performed below ten independent studies.

## 9. Interpretation

Association, discrimination, calibration, response classification, and causal treatment-effect modification are reported as distinct constructs. No negative temporary-drainage test, imaging marker, biomarker, or comorbidity is recommended as a stand-alone gatekeeper unless evidence directly supports safe exclusion of clinically important benefit.

Paired sensitivity and specificity are translated to likelihood ratios and post-test probabilities at 50%, 65%, and 80% pretest probability. Ranges formed from marginal confidence limits are labelled plausible ranges rather than joint 95% confidence intervals. Modeled counts per 100 tested are scenario analyses and not observed causal harms.

A categorical claim-level profile records ROBIS, primary-study risk-of-bias integration, precision, directness, response-definition validity, overlap, anchor dependence, and causal interpretation. It is explicitly not GRADE and has no numerical score.

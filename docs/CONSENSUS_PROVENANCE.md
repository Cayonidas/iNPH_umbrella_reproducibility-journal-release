# Quality-consensus provenance

Date recorded: **11 August 2026**.

The author team completed ROBIS and AMSTAR 2 consensus outside the analytic pipeline. Final judgments were entered into the dedicated author-consensus workbook, which is the sole quality-appraisal source used by the public analysis:

`data/raw/iNPH_ROBIS_AMSTAR2_final_author_consensus_2026-08-11.xlsx`

The workbook retains the two reviewers’ signalling and item-level judgments, disagreement queues, final consensus fields, agreement metrics, operational rules, and analysis-readiness checks. Final consensus contains 32 high-risk, 4 unclear, and 5 low-risk ROBIS ratings. AMSTAR 2 applies to 15 reviews/components; 14 are critically low and one is low confidence.

The explicit final author-consensus columns are authoritative. This is relevant to one documented post-discussion decision: FT-004 was initially `UNCLEAR`/`UNCLEAR` overall, whereas final author consensus is `LOW`. The pipeline preserves the final decision and exports the override in a dedicated audit table rather than silently reconstructing the initial reviewer pair.

An additional AI-assisted ROBIS/AMSTAR 2 comparison was conducted later as an internal quality-control exercise. It did not contribute to the human consensus and was never eligible for quality flags, anchor selection, sensitivity analyses, credibility profiles, or conclusions. The comparison inputs and outputs are non-analytic development artifacts and are not included in the public reproducibility release.

For source reviews overlapping the present author team, non-overlapping reviewers adjudicated risk of bias and anchor roles. FT-003 was led by Caio Arruda Maciel and also included Kauã Gabriel Oliveira da Silva, Vinicius Galbim de Paula, and Fernando Campos Gomes Pinto. FT-011 included Kim Wouters and Fernando Campos Gomes Pinto. Joint exclusion of FT-003 and FT-011 is retained as a post hoc author-overlap sensitivity analysis.

FT-012 full-text structural fields retain the complete preprint available at the base freeze. Official accepted-abstract quantitative values were substituted on 1 September 2026; the final formatted full text remained pending.


#!/usr/bin/env python3
"""Standard-library structural and invariant audit for the public release."""

from __future__ import annotations

import argparse
import csv
import hashlib
import json
import math
import re
import sys
import zipfile
from pathlib import Path
from xml.etree import ElementTree as ET


ROOT = Path(__file__).resolve().parents[1]
FAILURES: list[str] = []
WARNINGS: list[str] = []


def check(condition: bool, label: str, detail: str = "") -> None:
    status = "PASS" if condition else "FAIL"
    print(f"{status:4}  {label}" + (f" | {detail}" if detail else ""))
    if not condition:
        FAILURES.append(label)


def warn(label: str, detail: str = "") -> None:
    print(f"WARN  {label}" + (f" | {detail}" if detail else ""))
    WARNINGS.append(label)


def read_csv(rel: str) -> list[dict[str, str]]:
    with (ROOT / rel).open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def workbook_sheet_names(path: Path) -> list[str]:
    with zipfile.ZipFile(path) as archive:
        bad = archive.testzip()
        if bad is not None:
            raise ValueError(f"CRC failure in {bad}")
        xml = archive.read("xl/workbook.xml")
    root = ET.fromstring(xml)
    return [node.attrib["name"] for node in root.iter() if node.tag.endswith("}sheet")]


def verify_checksum_manifest(path: Path) -> tuple[bool, str]:
    checked = 0
    for line in path.read_text(encoding="utf-8").splitlines():
        if not line.strip():
            continue
        expected, rel = line.split(maxsplit=1)
        target = ROOT / rel.strip()
        if not target.is_file():
            return False, f"missing {rel.strip()}"
        observed = hashlib.sha256(target.read_bytes()).hexdigest()
        if observed != expected:
            return False, f"checksum mismatch: {rel.strip()}"
        checked += 1
    return checked > 0, f"{checked} files"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--release",
        action="store_true",
        help="also require session_info.txt, package_versions.csv, and renv.lock",
    )
    args = parser.parse_args()

    required = [
        "README.md", "LICENSE", "DATA_LICENSE.md", "CITATION.cff", "CHANGELOG.md",
        "DESCRIPTION", "config.yml", "run_all.R", "install_packages.R",
        "included_reviews_41.ris", "docs/STATISTICAL_ANALYSIS_PLAN.md",
        "docs/REPRODUCIBILITY_SCOPE.md", "scripts/validate_outputs.R",
        "scripts/capture_environment.R", "expected_outputs/analysis_run_summary.json",
        "expected_outputs/MANIFEST.sha256",
    ]
    check(all((ROOT / rel).is_file() for rel in required), "required release files present")

    all_files = [p for p in ROOT.rglob("*") if p.is_file()]
    relative = [p.relative_to(ROOT).as_posix() for p in all_files]
    forbidden = [
        rel for rel in relative
        if "/archive/" in f"/{rel}/"
        or Path(rel).name in {".Rhistory", ".RData", ".DS_Store", "Thumbs.db"}
        or re.search(r"codex_independent_(robis|amstar2)_comparison", rel, re.I)
        or rel.lower().endswith(".pdf")
        or rel.startswith("results/")
    ]
    check(not forbidden, "forbidden public-release files absent", "; ".join(forbidden[:5]))

    checksum_ok, checksum_detail = verify_checksum_manifest(ROOT / "expected_outputs/MANIFEST.sha256")
    check(checksum_ok, "expected-output checksum manifest", checksum_detail)

    quality = ROOT / "data/raw/iNPH_ROBIS_AMSTAR2_final_author_consensus_2026-08-11.xlsx"
    extraction = ROOT / "data/raw/iNPH_umbrella_final_adjudicated_extraction_v120_2026-08-11.xlsx"
    overlap_wb = ROOT / "data/raw/iNPH_CCA_publication_cohort_review_2026-08-10.xlsx"
    for path in (quality, extraction, overlap_wb):
        try:
            with zipfile.ZipFile(path) as archive:
                check(archive.testzip() is None, f"workbook integrity: {path.name}")
        except Exception as exc:
            check(False, f"workbook integrity: {path.name}", str(exc))

    expected_quality_sheets = {
        "Review_Consensus", "ROBIS_Domain_Queue", "ROBIS_Signalling",
        "AMSTAR2_Summary", "AMSTAR2_Items", "Agreement_Metrics", "Analysis_Readiness",
    }
    try:
        sheets = set(workbook_sheet_names(quality))
        check(expected_quality_sheets <= sheets, "quality workbook sheets", ", ".join(sorted(sheets)))
    except Exception as exc:
        check(False, "quality workbook sheets", str(exc))

    counts = {
        "expected_outputs/data/reviews_analysis_ready.csv": 41,
        "expected_outputs/data/predictor_findings_analysis_ready.csv": 192,
        "expected_outputs/data/quantitative_estimates_analysis_ready.csv": 155,
        "expected_outputs/data/outcome_definitions_analysis_ready.csv": 139,
        "expected_outputs/tables/quality_flags_by_review.csv": 41,
        "expected_outputs/tables/anchor_selection_candidate_audit.csv": 155,
    }
    for rel, expected in counts.items():
        observed = len(read_csv(rel))
        check(observed == expected, f"row count: {Path(rel).name}", f"{observed} vs {expected}")

    anchors = read_csv("expected_outputs/tables/anchor_selection_candidate_audit.csv")
    anchor_n = sum(row["selected_as_anchor"].upper() == "TRUE" for row in anchors)
    check(anchor_n == 64, "selected anchor microcells", str(anchor_n))

    quality_rows = read_csv("expected_outputs/tables/quality_flags_by_review.csv")
    robis = {
        "high": sum(row["robis_final_high"].upper() == "TRUE" for row in quality_rows),
        "unclear": sum(row["robis_final_unclear"].upper() == "TRUE" for row in quality_rows),
        "low": sum(row["robis_final_low"].upper() == "TRUE" for row in quality_rows),
    }
    check(robis == {"high": 32, "unclear": 4, "low": 5}, "final ROBIS distribution", json.dumps(robis))
    amstar_critical = sum(row["amstar_consensus_confidence"] == "CRITICALLY_LOW" for row in quality_rows)
    amstar_low = sum(row["amstar_consensus_confidence"] == "LOW" for row in quality_rows)
    check((amstar_critical, amstar_low) == (14, 1), "final AMSTAR 2 distribution", f"{amstar_critical} critically low; {amstar_low} low")

    overlap = {row["unit"]: row for row in read_csv("expected_outputs/tables/overlap_global_cca_with_coverage.csv")}
    pub = overlap["Publication"]
    coh = overlap["Conservatively linked cohort family"]
    check((pub["included_reviews"], pub["N"], pub["r"]) == ("19", "545", "429"), "publication CCA dimensions")
    check(math.isclose(float(pub["cca"]), 0.015022015022015, rel_tol=0, abs_tol=1e-12), "publication CCA value", pub["cca"])
    check((coh["included_reviews"], coh["N"], coh["r"]) == ("20", "579", "452"), "cohort CCA dimensions")
    check(math.isclose(float(coh["cca"]), 0.014788076385654, rel_tol=0, abs_tol=1e-12), "cohort CCA value", coh["cca"])

    sensitivity = {row["scenario"]: row for row in read_csv("expected_outputs/tables/sensitivity_scenario_summary.csv")}
    joint = sensitivity["exclude_all_current_team_authored_reviews"]
    check((joint["microcells_retained"], joint["high_level_cells_retained"]) == ("53", "10"), "joint author-overlap sensitivity", "53/64; 10/11")

    summary = json.loads((ROOT / "expected_outputs/analysis_run_summary.json").read_text(encoding="utf-8"))
    check(summary["registration"]["id"] == "CRD420261494316", "PROSPERO identifier", summary["registration"]["id"])

    source_text = "\n".join(
        p.read_text(encoding="utf-8", errors="replace")
        for p in [ROOT / "config.yml", ROOT / "R/01_import_validate.R", ROOT / "R/03_quality_appraisal.R"]
    )
    stale_tokens = [
        "codex_robis_comparison", "codex_amstar2_comparison",
        "codex_independent_robis_comparison.csv", "codex_independent_amstar2_comparison.csv",
    ]
    check(not any(token in source_text for token in stale_tokens), "no excluded comparison dependency in public pipeline")

    if args.release:
        env_files = [ROOT / "environment/session_info.txt", ROOT / "environment/package_versions.csv", ROOT / "renv.lock"]
        check(all(p.is_file() and p.stat().st_size > 0 for p in env_files), "authentic environment capture present")
    else:
        env_files = [ROOT / "environment/session_info.txt", ROOT / "environment/package_versions.csv", ROOT / "renv.lock"]
        if not all(p.is_file() and p.stat().st_size > 0 for p in env_files):
            warn("environment capture pending", "run capture_environment.R after a successful clean run")

    print(f"\nOVERALL: {'FAIL' if FAILURES else 'PASS'}")
    if WARNINGS:
        print("Warnings: " + "; ".join(WARNINGS))
    return 1 if FAILURES else 0


if __name__ == "__main__":
    sys.exit(main())

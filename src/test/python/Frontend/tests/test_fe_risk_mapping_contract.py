"""Contracts for the FE-RISK portion of the canonical coverage closure.

This module deliberately does not promote any row or artifact.  It checks that
the active registry is represented exactly once in the canonical test-point
table and that a row marked ``HIT`` carries enough evidence to be audited as a
real DUT result.  Legacy rows which do not meet that bar should fail this test
until they are re-run/back-annotated; changing their status here would hide the
gap the contract is intended to expose.
"""

from __future__ import annotations

import csv
import json
import re
from pathlib import Path

import pytest

from tools.backannotate_funcov import evaluate_artifact


_REFERENCE_RE = re.compile(
    r"^covergroup ([^,;]+), coverpoint ([^,;]+), bins ([^ (;]+) \((BIN-\d+)\)$"
)
_DUT_ENTRY_RE = re.compile(r"(?:^|; )(DUT:[^;]+)")
_DUT_TAG_RE = re.compile(r"^DUT:([^:;,]+)")
_RUN_ID_TOKEN_RE = re.compile(r"(?:^|,)run_id=([^,;]+)")
_WAVEFORM_TOKEN_RE = re.compile(r"(?:waveform|(?:^|[\s=/])[^;\s]+\.(?:fst|vcd|fsdb))", re.I)


def _frontend_root() -> Path:
    return Path(__file__).resolve().parents[1]


def _canonical_paths() -> tuple[Path, Path, Path]:
    root = _frontend_root()
    return (
        root / "docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv",
        root / "docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv",
        root / "data",
    )


def _read_rows(path: Path) -> list[dict[str, str]]:
    with path.open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def _active_bin_ids() -> set[str]:
    return {
        *(f"BIN-{index:03d}" for index in range(401, 424)),
        *(f"BIN-{index:03d}" for index in range(501, 542)),
    }


def _mapped_rows(testpoint_rows: list[dict[str, str]], active: set[str]) -> dict[str, tuple[int, dict[str, str]]]:
    mapped: dict[str, tuple[int, dict[str, str]]] = {}
    for line, row in enumerate(testpoint_rows, start=2):
        match = _REFERENCE_RE.fullmatch(str(row.get("coverage") or "").strip())
        if match is None:
            continue
        bin_id = match.group(4)
        if bin_id not in active:
            continue
        if bin_id in mapped:
            first_line = mapped[bin_id][0]
            raise AssertionError(f"{bin_id} is mapped more than once (lines {first_line} and {line})")
        mapped[bin_id] = (line, row)
    return mapped


def test_fe_risk_active_bins_have_one_actionable_canonical_mapping():
    pilot_path, testpoint_path, _artifact_dir = _canonical_paths()
    active = _active_bin_ids()
    pilot_rows = {
        str(row["Bin_ID"]).strip(): row
        for row in _read_rows(pilot_path)
        if str(row.get("Bin_ID") or "").strip() in active
    }
    assert set(pilot_rows) == active

    mapped = _mapped_rows(_read_rows(testpoint_path), active)
    assert set(mapped) == active
    for bin_id, (_line, row) in mapped.items():
        assert all(str(row.get(column) or "").strip() for column in ("Condition", "Checkpoint", "Object")), bin_id
        match = _REFERENCE_RE.fullmatch(str(row.get("coverage") or "").strip())
        assert match is not None
        pilot = pilot_rows[bin_id]
        assert match.group(1) == str(pilot["Coverage_Group"]).strip()
        assert match.group(2) == str(pilot["Coverpoint"]).strip()
        assert match.group(3) == str(pilot["Bin_Name"]).strip()


@pytest.mark.parametrize(
    ("label", "tokens"),
    (
        ("cross-page/PBMT/RVI", ("跨页", "PBMT", "RVI")),
        ("second-block ownership", ("第一块", "第二块", "ftqIdx")),
        ("exceptionMask/enqEnable", ("exceptionMask", "enqEnable")),
        ("WayLookup empty/write", ("WayLookup", "empty", "write")),
        ("redirect/trainCache", ("redirect", "trainCache")),
        ("BPU history/replacer", ("history", "replacer")),
    ),
)
def test_fe_risk_terms_are_present_in_canonical_testpoints(label: str, tokens: tuple[str, ...]):
    _pilot_path, testpoint_path, _artifact_dir = _canonical_paths()
    rows = _read_rows(testpoint_path)
    haystack = "\n".join(
        " ".join(str(value or "") for value in row.values()).lower() for row in rows
    )
    missing = [token for token in tokens if token.lower() not in haystack]
    assert not missing, f"{label}: missing canonical test-point token(s): {missing}"


def _artifact_candidates(data_dir: Path, tag: str, run_id: str = "") -> list[Path]:
    if not run_id:
        return []
    run_component = Path(run_id).name
    exact = data_dir / "runs" / run_component / "funcov" / f"{tag}.funcov.json"
    if exact.is_file():
        return [exact]
    return sorted((data_dir / "runs" / run_component / "funcov").glob(f"{tag}*.funcov.json"))


def test_artifact_candidates_use_the_evidence_run_id(tmp_path):
    tag = "case_a_test_bin_trace"
    stale = tmp_path / "runs" / "stale-run" / "funcov" / f"{tag}.funcov.json"
    current = tmp_path / "runs" / "current-run" / "funcov" / f"{tag}.funcov.json"
    stale.parent.mkdir(parents=True)
    current.parent.mkdir(parents=True)
    stale.write_text("{}", encoding="utf-8")
    current.write_text("{}", encoding="utf-8")

    assert _artifact_candidates(tmp_path, tag, "current-run") == [current]
    assert _artifact_candidates(tmp_path, tag) == []


def _hit_evidence_gaps(
    testpoint_rows: list[dict[str, str]], data_dir: Path, active: set[str]
) -> list[str]:
    gaps: list[str] = []
    for line, row in enumerate(testpoint_rows, start=2):
        if str(row.get("status") or "").strip() != "HIT":
            continue
        match = _REFERENCE_RE.fullmatch(str(row.get("coverage") or "").strip())
        if match is None or match.group(4) not in active:
            continue
        bin_id = match.group(4)
        evidence = str(row.get("evidence") or "").strip()
        dut_entries = _DUT_ENTRY_RE.findall(evidence)
        if not dut_entries:
            gaps.append(f"{bin_id} line {line}: no DUT:<artifact> evidence")
            continue
        if "monitor_errors=0" not in evidence:
            gaps.append(f"{bin_id} line {line}: evidence lacks monitor_errors=0")
        if _WAVEFORM_TOKEN_RE.search(evidence) is None:
            gaps.append(f"{bin_id} line {line}: evidence lacks waveform/path")

        for entry in dut_entries:
            tag_match = _DUT_TAG_RE.search(entry)
            if tag_match is None:
                gaps.append(f"{bin_id} line {line}: malformed DUT evidence {entry!r}")
                continue
            tag = tag_match.group(1)
            run_match = _RUN_ID_TOKEN_RE.search(entry)
            run_id = run_match.group(1).strip() if run_match is not None else ""
            if not run_id:
                gaps.append(f"{bin_id} line {line}: {tag} evidence lacks run_id")
                continue
            candidates = _artifact_candidates(data_dir, tag, run_id)
            if not candidates:
                gaps.append(f"{bin_id} line {line}: DUT artifact {tag!r} not found")
                continue
            # Any artifact named in the evidence must be a clean, cycle-bearing
            # run.  A failed run with a positive bin hit is never DUT evidence.
            for artifact_path in candidates[:1]:
                try:
                    artifact = json.loads(artifact_path.read_text(encoding="utf-8"))
                except (OSError, json.JSONDecodeError) as exc:
                    gaps.append(f"{bin_id} line {line}: cannot read {artifact_path.name}: {exc}")
                    continue
                monitor = ((artifact.get("stats") or {}).get("monitor") or {})
                errors = artifact.get("errors") or []
                gate = evaluate_artifact(artifact)
                if not gate["eligible"]:
                    gaps.append(
                        f"{bin_id} line {line}: {tag} rejected by DUT gate: "
                        + ",".join(gate["reasons"])
                    )
                if int(monitor.get("cycles_total", 0) or 0) <= 0:
                    gaps.append(f"{bin_id} line {line}: {tag} has no DUT cycles")
                if errors or int(monitor.get("error_count", 0) or 0) != 0:
                    gaps.append(f"{bin_id} line {line}: {tag} contains monitor errors")
    return gaps


def test_fe_risk_hit_evidence_is_auditable_dut():
    """Reject positive back-annotations that cannot be independently audited."""

    _pilot_path, testpoint_path, data_dir = _canonical_paths()
    gaps = _hit_evidence_gaps(_read_rows(testpoint_path), data_dir, _active_bin_ids())
    assert not gaps, "FE-RISK HIT evidence gaps:\n" + "\n".join(gaps)

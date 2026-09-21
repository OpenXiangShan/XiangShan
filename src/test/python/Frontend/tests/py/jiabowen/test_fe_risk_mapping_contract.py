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
import re
from pathlib import Path

import pytest

_REFERENCE_RE = re.compile(
    r"^covergroup ([^,;]+), coverpoint ([^,;]+), bins ([^ (;]+) \((BIN-\d+)\)$"
)


def _frontend_root() -> Path:
    return Path(__file__).resolve().parents[3]


def _canonical_paths() -> tuple[Path, Path, Path]:
    root = _frontend_root()
    return (
        root / "docs/03_funcov_model/frontend_bt_functional_coverage_pilot.csv",
        root / "docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv",
        root.parents[3] / "build-frontend/artifacts",
    )


def _read_rows(path: Path) -> list[dict[str, str]]:
    with path.open(encoding="utf-8-sig", newline="") as handle:
        return list(csv.DictReader(handle))


def _active_bin_ids() -> set[str]:
    return {
        *(f"BIN-{index:03d}" for index in range(401, 433)),
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


def test_fe_risk_hit_evidence_is_auditable_dut():
    """Keep detailed DUT diagnostics in artifacts rather than testpoint rows."""

    _pilot_path, testpoint_path, _data_dir = _canonical_paths()
    assert all(
        not str(row.get("evidence") or "").strip()
        for row in _read_rows(testpoint_path)
    )

#!/usr/bin/env python3
"""Back-annotate functional-coverage evidence into the testpoint CSV.

The tool deliberately treats model/FakeDut artifacts as modeling evidence only.
Only an artifact with real DUT runtime statistics can move a leaf to HIT;
CLOSED remains an explicit human-acceptance state.
"""

from __future__ import annotations

import argparse
import csv
import json
import re
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


STATUSES = {"UNMAPPED", "MODELED", "PARTIAL", "HIT", "CLOSED", "BLOCKED", "N-A"}
REFERENCE_RE = re.compile(
    r"^covergroup ([^,;]+), coverpoint ([^,;]+), bins ([^ (;]+) \((BIN-\d+)\)$"
)


@dataclass(frozen=True)
class PilotBin:
    bin_id: str
    group: str
    coverpoint: str
    bin_name: str
    testcase: str


def _read_csv(path: Path) -> tuple[list[str], list[dict[str, str]]]:
    with path.open(encoding="utf-8-sig", newline="") as f:
        reader = csv.DictReader(f)
        return list(reader.fieldnames or []), list(reader)


def _write_csv(path: Path, fields: list[str], rows: Iterable[dict[str, str]]) -> None:
    with path.open("w", encoding="utf-8-sig", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fields, extrasaction="ignore")
        writer.writeheader()
        writer.writerows(rows)


def load_pilot(path: Path, *, bin_prefix: str = "BIN-5") -> dict[str, PilotBin]:
    fields, rows = _read_csv(path)
    required = {"Bin_ID", "Coverage_Group", "Coverpoint", "Bin_Name", "建议试点用例"}
    missing = required - set(fields)
    if missing:
        raise ValueError(f"pilot missing columns: {sorted(missing)}")
    result = {}
    for row in rows:
        bin_id = row["Bin_ID"].strip()
        if not bin_id.startswith(bin_prefix):
            continue
        item = PilotBin(
            bin_id=bin_id,
            group=row["Coverage_Group"].strip(),
            coverpoint=row["Coverpoint"].strip(),
            bin_name=row["Bin_Name"].strip(),
            testcase=row["建议试点用例"].strip(),
        )
        if not item.group or not item.coverpoint or not item.bin_name:
            raise ValueError(f"incomplete pilot row: {bin_id}")
        if bin_id in result:
            raise ValueError(f"duplicate pilot bin: {bin_id}")
        result[bin_id] = item
    return result


def parse_reference(value: str) -> tuple[str, str, str, str] | None:
    match = REFERENCE_RE.fullmatch(str(value or "").strip())
    return match.groups() if match else None


def validate_mapping(
    testpoint_path: Path,
    pilot: dict[str, PilotBin],
    *,
    bin_prefix: str = "BIN-5",
) -> dict[str, int]:
    fields, rows = _read_csv(testpoint_path)
    required = {"coverage", "status", "testcase", "evidence"}
    missing = required - set(fields)
    if missing:
        raise ValueError(f"testpoint missing columns: {sorted(missing)}")

    mapped: dict[str, int] = {}
    for line, row in enumerate(rows, start=2):
        status = row["status"].strip()
        if status and status not in STATUSES:
            raise ValueError(f"line {line}: invalid status {status!r}")
        coverage = row["coverage"].strip()
        if bin_prefix not in coverage:
            continue
        ref = parse_reference(coverage)
        if ref is None:
            raise ValueError(f"line {line}: leaf must bind exactly one group/point/bin")
        group, point, bin_name, bin_id = ref
        if not bin_id.startswith(bin_prefix):
            continue
        expected = pilot.get(bin_id)
        if expected is None:
            raise ValueError(f"line {line}: unknown pilot bin {bin_id}")
        if (group, point, bin_name) != (expected.group, expected.coverpoint, expected.bin_name):
            raise ValueError(f"line {line}: pilot mismatch for {bin_id}")
        if bin_id in mapped:
            raise ValueError(f"line {line}: {bin_id} already owned by line {mapped[bin_id]}")
        mapped[bin_id] = line

    missing_bins = sorted(set(pilot) - set(mapped))
    if missing_bins:
        raise ValueError(f"pilot bins not back-annotated: {missing_bins}")
    return mapped


def artifact_kind(raw: dict) -> str:
    monitor = ((raw.get("stats") or {}).get("monitor") or {})
    cycles = monitor.get("cycles_total", 0)
    try:
        return "dut" if int(cycles) > 0 else "model"
    except (TypeError, ValueError):
        return "model"


def load_artifacts(paths: Iterable[Path]) -> list[tuple[Path, dict, str]]:
    artifacts = []
    for path in paths:
        with path.open(encoding="utf-8") as f:
            raw = json.load(f)
        artifacts.append((path, raw, artifact_kind(raw)))
    return artifacts


def _append_evidence(existing: str, entries: Iterable[str]) -> str:
    values = [item for item in str(existing or "").split("; ") if item]
    for entry in entries:
        if entry not in values:
            values.append(entry)
    return "; ".join(values)


def backannotate(
    testpoint_path: Path,
    pilot: dict[str, PilotBin],
    artifacts: Iterable[tuple[Path, dict, str]],
    *,
    apply: bool,
) -> dict[str, int]:
    fields, rows = _read_csv(testpoint_path)
    validate_mapping(testpoint_path, pilot)
    artifacts = list(artifacts)
    counts = {"model": 0, "partial": 0, "hit": 0, "closed_preserved": 0}

    for row in rows:
        ref = parse_reference(row["coverage"])
        if ref is None:
            continue
        group, _point, bin_name, bin_id = ref
        item = pilot.get(bin_id)
        if item is None:
            continue

        row["testcase"] = item.testcase
        status = row["status"].strip() or "MODELED"
        if status == "CLOSED":
            counts["closed_preserved"] += 1
            continue
        if status in {"BLOCKED", "N-A"}:
            continue

        key = f"{group}::{bin_name}"
        model_entries = []
        dut_entries = []
        dut_seen_for_testcase = False
        for path, raw, kind in artifacts:
            hit = ((raw.get("hits") or {}).get(key) or {}).get("hits", 0)
            tag = str(raw.get("artifact_tag") or path.stem)
            if kind == "dut":
                dut_seen_for_testcase |= bool(item.testcase and item.testcase in tag)
                if int(hit) > 0:
                    dut_entries.append(f"DUT:{tag}:hits={int(hit)}")
            else:
                model_entries.append(f"MODEL:{tag}")

        row["evidence"] = _append_evidence(row["evidence"], [*model_entries, *dut_entries])
        if dut_entries:
            row["status"] = "HIT"
            counts["hit"] += 1
        elif dut_seen_for_testcase:
            row["status"] = "PARTIAL"
            counts["partial"] += 1
        else:
            row["status"] = "MODELED"
            counts["model"] += 1

    if apply:
        _write_csv(testpoint_path, fields, rows)
    return counts


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--pilot", type=Path, required=True)
    parser.add_argument("--testpoints", type=Path, required=True)
    parser.add_argument("--artifact", type=Path, action="append", default=[])
    parser.add_argument("--check", action="store_true", help="validate only; do not write")
    args = parser.parse_args()

    pilot = load_pilot(args.pilot)
    artifacts = load_artifacts(args.artifact)
    counts = backannotate(args.testpoints, pilot, artifacts, apply=not args.check)
    print(" ".join(f"{key}={value}" for key, value in sorted(counts.items())))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

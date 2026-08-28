#!/usr/bin/env python3
"""Verify full checkpoint coverage and Constantin updates for a perf run."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
import re
import sys
from typing import Any


FILE_LOAD_RE = re.compile(r"^\[INFO\] file for constantin: loaded from .+\.$", re.MULTILINE)
UPDATE_RE = re.compile(r"^\[INFO\] constant updated: (?P<name>.+) = (?P<value>\d+)$", re.MULTILINE)
COVERAGE_RE = re.compile(r"^Minimal Coverage\s*:\s*(?P<coverage>\d+\.\d+)/1\.00\s*$", re.MULTILINE)
CHECKPOINT_RE = re.compile(
    r"^Checkpoints Number\s*:\s*(?P<success>\d+)/(?P<total>\d+)\s*$", re.MULTILINE
)
FAILED_MARKER = "=============== Failed Checkpoints ==============="


def parse_cst(path: Path) -> dict[str, int]:
    records: dict[str, int] = {}
    for line_number, line in enumerate(path.read_text(encoding="ascii").splitlines(), start=1):
        if not line.strip():
            continue
        fields = line.split()
        if len(fields) != 2:
            raise ValueError(f"{path}:{line_number}: expected '<name> <unsigned decimal>'")
        name, value = fields
        if name in records:
            raise ValueError(f"{path}:{line_number}: duplicate Constantin key {name}")
        if not value.isdecimal():
            raise ValueError(f"{path}:{line_number}: expected an unsigned decimal value")
        records[name] = int(value, 10)
    if not records:
        raise ValueError(f"{path}: Constantin file is empty")
    return records


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as source:
        for chunk in iter(lambda: source.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def parse_score(path: Path) -> dict[str, Any]:
    text = path.read_text(encoding="utf-8", errors="replace")
    coverage_match = COVERAGE_RE.search(text)
    checkpoint_match = CHECKPOINT_RE.search(text)
    if coverage_match is None:
        raise ValueError(f"{path}: missing Minimal Coverage line")
    if checkpoint_match is None:
        raise ValueError(f"{path}: missing Checkpoints Number line")
    if FAILED_MARKER not in text:
        raise ValueError(f"{path}: missing failed-checkpoint section")

    failed_text = text.split(FAILED_MARKER, maxsplit=1)[1].lstrip()
    try:
        failed, _ = json.JSONDecoder().raw_decode(failed_text)
    except json.JSONDecodeError as error:
        raise ValueError(f"{path}: invalid failed-checkpoint JSON: {error}") from error
    if not isinstance(failed, list) or not all(isinstance(item, str) for item in failed):
        raise ValueError(f"{path}: failed-checkpoint JSON must be a list of strings")

    return {
        "coverage": coverage_match.group("coverage"),
        "success": int(checkpoint_match.group("success"), 10),
        "total": int(checkpoint_match.group("total"), 10),
        "failed_checkpoints": failed,
    }


def verify_log(path: Path, result_dir: Path, expected: dict[str, int]) -> dict[str, Any]:
    text = path.read_text(encoding="utf-8", errors="replace")
    updates: dict[str, list[int]] = {}
    for match in UPDATE_RE.finditer(text):
        updates.setdefault(match.group("name"), []).append(int(match.group("value"), 10))

    missing = sorted(set(expected) - set(updates))
    unexpected = sorted(set(updates) - set(expected))
    duplicates = sorted(name for name, values in updates.items() if len(values) != 1)
    mismatched = {
        name: {"expected": expected[name], "observed": updates[name]}
        for name in sorted(set(expected) & set(updates))
        if updates[name] != [expected[name]]
    }
    loaded_file = FILE_LOAD_RE.search(text) is not None
    verified = loaded_file and not missing and not unexpected and not duplicates and not mismatched
    return {
        "checkpoint": str(path.parent.relative_to(result_dir)),
        "log": str(path.relative_to(result_dir)),
        "loaded_file": loaded_file,
        "verified": verified,
        "missing": missing,
        "unexpected": unexpected,
        "duplicate_updates": duplicates,
        "mismatched": mismatched,
    }


def unreadable_log(
    path: Path, result_dir: Path, expected: dict[str, int], error: OSError
) -> dict[str, Any]:
    return {
        "checkpoint": str(path.parent.relative_to(result_dir)),
        "log": str(path.relative_to(result_dir)),
        "loaded_file": False,
        "verified": False,
        "missing": sorted(expected),
        "unexpected": [],
        "duplicate_updates": [],
        "mismatched": {},
        "read_error": str(error),
    }


def write_json(path: Path, value: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(json.dumps(value, indent=2, sort_keys=True) + "\n", encoding="utf-8")


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--result-dir", required=True, type=Path)
    parser.add_argument("--cst-file", required=True, type=Path)
    parser.add_argument("--score-file", required=True, type=Path)
    parser.add_argument("--expected-checkpoints", default=0, type=int)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    if args.expected_checkpoints < 0:
        parser.error("--expected-checkpoints must be non-negative")
    return args


def main() -> int:
    args = parse_args()
    errors: list[str] = []
    expected: dict[str, int] = {}
    cst_sha256: str | None = None
    score: dict[str, Any] | None = None

    try:
        expected = parse_cst(args.cst_file)
        cst_sha256 = sha256(args.cst_file)
    except (OSError, ValueError) as error:
        errors.append(str(error))

    try:
        score = parse_score(args.score_file)
    except (OSError, ValueError) as error:
        errors.append(str(error))

    logs = sorted(args.result_dir.rglob("simulator_out.txt")) if args.result_dir.is_dir() else []
    if not args.result_dir.is_dir():
        errors.append(f"{args.result_dir}: result directory does not exist")

    verifications = []
    if expected:
        for log in logs:
            try:
                verifications.append(verify_log(log, args.result_dir, expected))
            except OSError as error:
                verifications.append(unreadable_log(log, args.result_dir, expected, error))
    failed_logs = [record["checkpoint"] for record in verifications if not record["verified"]]

    if score is not None:
        if score["coverage"] != "1.00":
            errors.append(f"Minimal Coverage is {score['coverage']}/1.00, expected 1.00/1.00")
        if score["success"] != score["total"]:
            errors.append(f"Checkpoints Number is {score['success']}/{score['total']}, expected all successful")
        if score["failed_checkpoints"]:
            errors.append(f"score reports {len(score['failed_checkpoints'])} failed checkpoints")
        if args.expected_checkpoints and score["total"] != args.expected_checkpoints:
            errors.append(
                f"score reports {score['total']} checkpoints, expected {args.expected_checkpoints}"
            )
        if len(logs) != score["total"]:
            errors.append(f"found {len(logs)} simulator_out.txt files, expected {score['total']}")
    elif args.expected_checkpoints and len(logs) != args.expected_checkpoints:
        errors.append(f"found {len(logs)} simulator_out.txt files, expected {args.expected_checkpoints}")

    if failed_logs:
        errors.append(f"{len(failed_logs)} checkpoint logs did not exactly load the copied Constantin file")

    manifest = {
        "cst_file": str(args.cst_file),
        "cst_sha256": cst_sha256,
        "expected_checkpoints": args.expected_checkpoints,
        "expected_records": expected,
        "log_count": len(logs),
        "score": score,
        "verified_log_count": len(verifications) - len(failed_logs),
        "verifications": verifications,
        "errors": errors,
        "ok": not errors,
    }
    write_json(args.output, manifest)

    if errors:
        for error in errors:
            print(f"ERROR: {error}", file=sys.stderr)
        return 1

    print(f"Verified {len(verifications)} checkpoints and {len(expected)} Constantin records per log.")
    return 0


if __name__ == "__main__":
    sys.exit(main())

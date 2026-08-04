from __future__ import annotations

import argparse
import csv
import subprocess
from io import StringIO
from pathlib import Path
from typing import TextIO


def _canonical_testpoint_path() -> Path:
    root = Path(__file__).resolve().parents[3]
    return root / "docs/02_testpoint/Frontend_testpoint_0525_coverage_backannotated.csv"


def _check_one_physical_line_per_record(handle: TextIO) -> None:
    reader = csv.DictReader(handle)
    prev_end = 1
    for line, row in enumerate(reader, start=2):
        assert reader.line_num == prev_end + 1, f"row {line} spans multiple physical lines"
        assert all(
            "\n" not in str(value or "") and "\r" not in str(value or "")
            for value in row.values()
        )
        prev_end = reader.line_num


def _staged_testpoint_contents(testpoint_path: Path) -> StringIO:
    root = Path(__file__).resolve().parents[7]
    result = subprocess.run(
        ["git", "show", f":{testpoint_path.relative_to(root)}"],
        cwd=root,
        check=True,
        stdout=subprocess.PIPE,
    )
    return StringIO(result.stdout.decode("utf-8-sig"), newline="")


def test_canonical_testpoint_csv_uses_one_physical_line_per_record() -> None:
    with _canonical_testpoint_path().open(encoding="utf-8-sig", newline="") as handle:
        _check_one_physical_line_per_record(handle)


def test_csv_contract_rejects_a_multiline_record() -> None:
    try:
        _check_one_physical_line_per_record(StringIO('column_a,column_b\n"first\nsecond",value\n'))
    except AssertionError:
        return
    raise AssertionError("multiline record was accepted")


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--staged", action="store_true")
    args = parser.parse_args()
    testpoint_path = _canonical_testpoint_path()

    if args.staged:
        with _staged_testpoint_contents(testpoint_path) as handle:
            _check_one_physical_line_per_record(handle)
    else:
        with testpoint_path.open(encoding="utf-8-sig", newline="") as handle:
            _check_one_physical_line_per_record(handle)


if __name__ == "__main__":
    main()

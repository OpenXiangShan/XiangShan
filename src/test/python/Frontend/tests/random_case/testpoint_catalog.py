#!/usr/bin/env python3
from __future__ import annotations

import argparse
import csv
import hashlib
import json
import re
from collections import Counter
from pathlib import Path
from typing import Iterable


CATALOG_SCHEMA_VERSION = 1
HIERARCHY_COLUMNS = (
    "一级测试点",
    "二级测试点",
    "三级测试点",
    "四级测试点",
    "五级测试点",
)
DETAIL_COLUMNS = (
    "Condition",
    "Checkpoint",
    "Object",
    "coverage",
    "status",
    "testcase",
    "evidence",
)
COVERAGE_REF_RE = re.compile(r"\b[A-Z][A-Z0-9_]*-\d+\b")
FRONTEND_ROOT = Path(__file__).resolve().parents[2]
def default_testpoint_source(frontend_root: Path) -> Path:
    """Prefer the current canonical filename; retain old checkout compatibility.

    The selected CSV is still read and hashed in full by build_catalog. No
    historical CSV is copied into a current checkout or treated as current.
    """
    directory = frontend_root / "docs" / "02_testpoint"
    current = directory / "Frontend_testpoint_v1.0.csv"
    historical = directory / "Frontend_testpoint_0525_coverage_backannotated.csv"
    return current if current.is_file() or not historical.is_file() else historical


DEFAULT_SOURCE = default_testpoint_source(FRONTEND_ROOT)


def _clean(value: object) -> str:
    return str(value or "").strip()


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _coverage_refs(value: str) -> list[str]:
    return sorted(set(COVERAGE_REF_RE.findall(value)))


def _testpoint_fingerprint(record: dict) -> str:
    payload = json.dumps(
        {
            "path": record["path"],
            "condition": record["condition"],
            "checkpoint": record["checkpoint"],
            "object": record["object"],
            "coverage": record["coverage"],
        },
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
    ).encode("utf-8")
    return hashlib.sha256(payload).hexdigest()[:12].upper()


def load_testpoints(source: Path) -> list[dict]:
    source = source.resolve()
    with source.open(encoding="utf-8-sig", newline="") as stream:
        reader = csv.DictReader(stream)
        fieldnames = tuple(reader.fieldnames or ())
        missing = [name for name in (*HIERARCHY_COLUMNS, *DETAIL_COLUMNS) if name not in fieldnames]
        if missing:
            raise ValueError(f"testpoint CSV is missing columns: {', '.join(missing)}")

        hierarchy = [""] * len(HIERARCHY_COLUMNS)
        records: list[dict] = []
        fingerprint_counts: Counter[str] = Counter()
        for source_row, row in enumerate(reader, start=2):
            explicit_levels: list[int] = []
            for index, column in enumerate(HIERARCHY_COLUMNS):
                value = _clean(row.get(column))
                if not value:
                    continue
                hierarchy[index] = value
                hierarchy[index + 1 :] = [""] * (len(hierarchy) - index - 1)
                explicit_levels.append(index + 1)

            details = {column: _clean(row.get(column)) for column in DETAIL_COLUMNS}
            has_details = any(details.values())
            has_explicit_leaf_name = bool(_clean(row.get(HIERARCHY_COLUMNS[-1])))
            if not has_details and not has_explicit_leaf_name:
                continue

            path = [value for value in hierarchy if value]
            if has_details and has_explicit_leaf_name:
                record_kind = "documented_leaf"
            elif has_explicit_leaf_name:
                record_kind = "undocumented_leaf"
            else:
                record_kind = "unnamed_leaf"
            record = {
                "source_row": source_row,
                "record_kind": record_kind,
                "path": path,
                "path_text": "/".join(path),
                "explicit_levels": explicit_levels,
                "condition": details["Condition"],
                "checkpoint": details["Checkpoint"],
                "object": details["Object"],
                "coverage": details["coverage"],
                "coverage_refs": _coverage_refs(details["coverage"]),
                "status": details["status"] or "UNSPECIFIED",
                "testcase": details["testcase"],
                "evidence": details["evidence"],
            }
            fingerprint = _testpoint_fingerprint(record)
            fingerprint_counts[fingerprint] += 1
            suffix = fingerprint_counts[fingerprint]
            record["testpoint_id"] = (
                f"TP-{fingerprint}" if suffix == 1 else f"TP-{fingerprint}-{suffix:02d}"
            )
            records.append(record)
    return records


def _counter(items: Iterable[str]) -> dict[str, int]:
    return dict(sorted(Counter(items).items()))


def build_catalog(source: Path) -> dict:
    source = source.resolve()
    testpoints = load_testpoints(source)
    all_refs = sorted({ref for item in testpoints for ref in item["coverage_refs"]})
    summary = {
        "testpoint_count": len(testpoints),
        "record_kind_counts": _counter(item["record_kind"] for item in testpoints),
        "status_counts": _counter(item["status"] for item in testpoints),
        "top_level_counts": _counter(
            item["path"][0] if item["path"] else "<missing>" for item in testpoints
        ),
        "with_coverage_text": sum(bool(item["coverage"]) for item in testpoints),
        "without_coverage_text": sum(not item["coverage"] for item in testpoints),
        "unique_coverage_ref_count": len(all_refs),
    }
    return {
        "schema_version": CATALOG_SCHEMA_VERSION,
        "source_path": str(source),
        "source_sha256": _sha256(source),
        "summary": summary,
        "coverage_refs": all_refs,
        "testpoints": testpoints,
    }


def write_catalog(catalog: dict, output: Path) -> None:
    output = output.resolve()
    output.parent.mkdir(parents=True, exist_ok=True)
    temporary = output.with_name(f".{output.name}.tmp")
    temporary.write_text(
        json.dumps(catalog, ensure_ascii=False, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    temporary.replace(output)


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Build a lossless random-regression catalog from the Frontend testpoint CSV"
    )
    parser.add_argument("--source", type=Path, default=DEFAULT_SOURCE)
    parser.add_argument("--output", type=Path, required=True)
    return parser.parse_args()


def main() -> int:
    args = _parse_args()
    try:
        catalog = build_catalog(args.source)
        write_catalog(catalog, args.output)
    except (OSError, ValueError, csv.Error) as exc:
        raise SystemExit(f"testpoint catalog generation failed: {exc}") from exc
    summary = catalog["summary"]
    print(
        f"testpoints={summary['testpoint_count']} "
        f"with_coverage={summary['with_coverage_text']} "
        f"unique_coverage_refs={summary['unique_coverage_ref_count']} "
        f"catalog={args.output.resolve()}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

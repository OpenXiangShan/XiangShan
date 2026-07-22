#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path


FRONTEND_ROOT = Path(__file__).resolve().parents[1]
if str(FRONTEND_ROOT) not in sys.path:
    sys.path.insert(0, str(FRONTEND_ROOT))

from env.artifact_provenance import file_sha256
from env.functional_coverage import FunctionalCoverageRecorder


def _target_union(paths: list[Path]) -> dict[str, list[str]]:
    result = {"bin_ids": [], "hit_keys": [], "tp_ids": [], "testcases": []}
    seen = {key: set() for key in result}
    for path in paths:
        raw = json.loads(path.read_text(encoding="utf-8"))
        targets = raw.get("coverage_targets")
        if not isinstance(targets, dict):
            continue
        for key in result:
            values = targets.get(key)
            if not isinstance(values, list):
                continue
            for value in values:
                text = str(value).strip()
                if text and text not in seen[key]:
                    seen[key].add(text)
                    result[key].append(text)
    return result


def main() -> int:
    parser = argparse.ArgumentParser(
        description="Merge compatible Frontend functional-coverage artifacts into a diagnostic report"
    )
    parser.add_argument("--artifact", type=Path, action="append", required=True)
    parser.add_argument("--output-dir", type=Path, required=True)
    parser.add_argument("--artifact-tag", required=True)
    parser.add_argument("--run-id", default="")
    args = parser.parse_args()

    artifact_tag = str(args.artifact_tag).strip()
    if re.fullmatch(r"[A-Za-z0-9_.=-]+", artifact_tag) is None:
        parser.error("--artifact-tag must contain only A-Z, a-z, 0-9, _, ., =, or -")
    raw_paths = [path.resolve() for path in args.artifact]
    missing = [str(path) for path in raw_paths if not path.is_file()]
    if missing:
        parser.error(f"functional coverage artifact not found: {missing}")

    output_dir = args.output_dir.resolve()
    expected_outputs = [
        output_dir / f"{artifact_tag}.funcov.json",
        output_dir / f"{artifact_tag}.funcov.summary.csv",
        output_dir / f"{artifact_tag}.funcov.unhit.csv",
        output_dir / f"{artifact_tag}.funcov.merge.json",
    ]
    existing = [str(path) for path in expected_outputs if path.exists()]
    if existing:
        parser.error(f"refusing to overwrite existing functional coverage report: {existing}")

    merged = FunctionalCoverageRecorder.merge_raw_files(
        raw_paths,
        artifact_tag=artifact_tag,
        output_dir=output_dir,
    )
    merged.coverage_targets = _target_union(raw_paths)
    merged.set_run_metadata(
        outcome="aggregate",
        run_id=str(args.run_id).strip() or artifact_tag,
        checker={"status": "aggregate", "error_count": 0, "errors": []},
        extra={
            "aggregate": True,
            "input_artifacts": [str(path) for path in raw_paths],
            "input_sha256": [file_sha256(path) for path in raw_paths],
        },
    )
    outputs = merged.write_artifacts()
    manifest_path = output_dir / f"{artifact_tag}.funcov.merge.json"
    manifest = {
        "artifact_type": "diagnostic_observed_aggregate",
        "run_id": str(args.run_id).strip() or artifact_tag,
        "compatibility_signature": merged.provenance["compatibility_signature"],
        "inputs": [
            {"path": str(path), "sha256": file_sha256(path)} for path in raw_paths
        ],
        "outputs": outputs,
    }
    manifest_path.write_text(json.dumps(manifest, indent=2) + "\n", encoding="utf-8")
    print(
        f"merged={len(raw_paths)} artifact_tag={artifact_tag} "
        f"summary={outputs['summary_path']} audit_only=true"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

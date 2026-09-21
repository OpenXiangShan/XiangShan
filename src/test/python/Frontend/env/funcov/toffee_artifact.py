from __future__ import annotations

import json
from copy import deepcopy
from pathlib import Path
from typing import Any, Iterable


_PASS_OUTCOMES = {"pass", "passed", "ok", "success", "successful"}


def _coverage_index(groups: list[dict[str, Any]]) -> dict[tuple[str, str, str], dict[str, Any]]:
    result = {}
    for group in groups:
        group_name = str(group["name"])
        for point in group["points"]:
            point_name = str(point["name"])
            for item in point["bins"]:
                key = (group_name, point_name, str(item["name"]))
                if key in result:
                    raise ValueError(f"duplicate Toffee coverage key: {key}")
                result[key] = item
    return result


def _summary(groups: list[dict[str, Any]]) -> dict[str, int | float]:
    points = [point for group in groups for point in group["points"]]
    bins = [item for point in points for item in point["bins"]]
    hinted_bins = sum(int(item["hints"] > 0) for item in bins)
    hinted_points = sum(
        int(all(item["hints"] > 0 for item in point["bins"])) for point in points
    )
    hinted_groups = sum(
        int(
            all(
                all(item["hints"] > 0 for item in point["bins"])
                for point in group["points"]
            )
        )
        for group in groups
    )
    return {
        "group_num_total": len(groups),
        "group_num_hints": hinted_groups,
        "point_num_total": len(points),
        "point_num_hints": hinted_points,
        "bin_num_total": len(bins),
        "bin_num_hints": hinted_bins,
        "bin_rate": (100.0 * hinted_bins / len(bins)) if bins else 0.0,
    }


def merge_toffee_artifacts(paths: Iterable[Path], output: Path) -> Path:
    inputs = [Path(path) for path in paths]
    if not inputs:
        raise ValueError("at least one Toffee artifact is required")

    payloads = [json.loads(path.read_text(encoding="utf-8")) for path in inputs]
    from tools.backannotate_funcov import evaluate_artifact, normalize_artifact

    for path, payload in zip(inputs, payloads):
        gate = evaluate_artifact(
            normalize_artifact(payload, artifact_path=path), require_targets=False
        )
        if not gate["eligible"]:
            raise ValueError(
                f"Toffee artifact failed signoff gate: {path}: "
                + ",".join(gate["reasons"])
            )
    first = payloads[0]
    if first.get("schema_version") != 1:
        raise ValueError("unsupported Toffee artifact schema")
    expected_bin_ids = first.get("bin_ids")
    expected_collector = first.get("collector")
    expected_signature = first.get("metadata", {}).get("provenance", {}).get(
        "compatibility_signature"
    )
    groups = deepcopy(first.get("coverage", {}).get("groups", []))
    merged_index = _coverage_index(groups)

    for path, payload in zip(inputs, payloads):
        run = payload.get("metadata", {}).get("run", {})
        if str(run.get("outcome") or "").strip().lower() not in _PASS_OUTCOMES:
            raise ValueError(f"Toffee artifact run did not pass: {path}")
        if int(run.get("exit_code", 1)) != 0:
            raise ValueError(f"Toffee artifact exit code is not zero: {path}")
        checker = run.get("checker") or {}
        if str(checker.get("status") or "").strip().lower() != "pass":
            raise ValueError(f"Toffee artifact checker did not pass: {path}")
        if payload.get("schema_version") != 1:
            raise ValueError(f"Toffee artifact schema mismatch: {path}")
        if payload.get("bin_ids") != expected_bin_ids:
            raise ValueError(f"Toffee artifact bin identity mismatch: {path}")
        if payload.get("collector") != expected_collector:
            raise ValueError(f"Toffee artifact collector version mismatch: {path}")
        signature = payload.get("metadata", {}).get("provenance", {}).get(
            "compatibility_signature"
        )
        if not expected_signature or signature != expected_signature:
            raise ValueError(f"Toffee artifact compatibility mismatch: {path}")

        source_index = _coverage_index(payload.get("coverage", {}).get("groups", []))
        if set(source_index) != set(merged_index):
            raise ValueError(f"Toffee artifact coverage definition mismatch: {path}")
        if payload is first:
            continue
        for key, item in source_index.items():
            merged_index[key]["hints"] = int(merged_index[key]["hints"]) + int(
                item["hints"]
            )

    result = {
        "schema_version": 1,
        "collector": expected_collector,
        "metadata": {
            "mode": "merged",
            "input_artifacts": [str(path.resolve()) for path in inputs],
            "provenance": first["metadata"]["provenance"],
        },
        "summary": _summary(groups),
        "bin_ids": expected_bin_ids,
        "coverage": {"groups": groups},
    }
    output = Path(output)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(
        json.dumps(result, ensure_ascii=False, indent=2, sort_keys=True),
        encoding="utf-8",
    )
    return output

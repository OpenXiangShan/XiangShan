#!/usr/bin/env python3
"""Compare historical page-tail repetition variants against the same original seeds.

The report ranks offline execution cost only.  It deliberately does not claim
that fewer probe repetitions preserve DUT timing behavior or coverage HITs.
"""

from __future__ import annotations

import argparse
import hashlib
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

try:
    from .candidate_offline_compare import build_report as build_candidate_report
except ImportError:
    from candidate_offline_compare import build_report as build_candidate_report


EXPECTED_PATTERN = "page_tail_pair_sparse"
EXPECTED_ATTRIBUTE_MODE = "rvi_nc_pair"
PREDECLARED_TARGET_BIN_IDS = ["BIN-417", "BIN-1120", "BIN-1121"]


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _load_json(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"JSON root is not an object: {path}")
    return payload


def _cases_by_ordinal(campaign: dict[str, Any]) -> dict[int, dict[str, Any]]:
    result: dict[int, dict[str, Any]] = {}
    for record in campaign.get("cases") or []:
        spec = record.get("spec") or {}
        ordinal = spec.get("ordinal")
        if type(ordinal) is not int or ordinal < 0 or ordinal in result:
            raise ValueError(f"invalid or duplicate ordinal: {ordinal}")
        result[ordinal] = record
    return result


def _expected_attribute_plan(ordinal: int) -> tuple[str, list[int]]:
    if ordinal % 2 == 0:
        return "page_tail_rvc_nc_to_cacheable_rvi_nc_pair", [1, 0, 1, 0, 0]
    return "page_tail_rvc_cacheable_to_nc_rvi_nc_pair", [0, 1, 0, 0, 0]


def _metric_delta(metrics: dict[str, Any], name: str) -> int:
    return int((metrics.get(name) or {}).get("delta") or 0)


def _event_candidate(metrics: dict[str, Any], name: str) -> int:
    return int(
        ((metrics.get("dynamic_event_counts") or {}).get(name) or {}).get("candidate")
        or 0
    )


def _case_summary(report: dict[str, Any], repetitions: int, ordinal: int) -> dict[str, Any]:
    metrics = report.get("offline_metrics") or {}
    repetition = metrics.get("dynamic_pc_repetition") or {}
    offline_check = report["offline_check"]
    return {
        "ordinal": ordinal,
        "probe_repetitions": repetitions,
        "offline_check": {
            "pass": offline_check["pass"] is True,
            "reasons": list(offline_check["reasons"]),
        },
        "dynamic_instruction_count": int(
            (metrics.get("dynamic_instruction_count") or {}).get("candidate") or 0
        ),
        "delta_from_original": {
            "static_program_map_instructions": _metric_delta(
                metrics, "program_map_static_instruction_count"
            ),
            "text_span_bytes": _metric_delta(metrics, "text_size_bytes"),
            "dynamic_instructions": _metric_delta(metrics, "dynamic_instruction_count"),
            "unique_pcs": _metric_delta(metrics, "unique_executed_pc_count"),
            "unique_cfg_edges": _metric_delta(
                metrics, "unique_control_flow_edge_count"
            ),
            "repeated_dynamic_instruction_proxy": int(
                repetition.get("incremental_repeated_dynamic_instruction_count") or 0
            ),
        },
        "realized_program_events": {
            "page_tail_rvc_fallthrough": _event_candidate(
                metrics, "trace.page_tail_rvc_fallthrough"
            ),
            "cross_page_instruction": _event_candidate(
                metrics, "trace.cross_page_instruction"
            ),
        },
        "comparison_provenance": report.get("provenance") or {},
    }


def _uniform_value(cases: list[dict[str, Any]], path: tuple[str, ...]) -> int | None:
    values: set[int] = set()
    for case in cases:
        value: Any = case
        for key in path:
            value = value[key]
        values.add(int(value))
    return next(iter(values)) if len(values) == 1 else None


def _variant_summary(repetitions: int, cases: list[dict[str, Any]]) -> dict[str, Any]:
    dynamic_counts = [int(case["dynamic_instruction_count"]) for case in cases]
    uniform_deltas = {
        key: _uniform_value(cases, ("delta_from_original", key))
        for key in (
            "static_program_map_instructions",
            "text_span_bytes",
            "dynamic_instructions",
            "unique_pcs",
            "unique_cfg_edges",
            "repeated_dynamic_instruction_proxy",
        )
    }
    added_dynamic = uniform_deltas["dynamic_instructions"]
    added_unique = uniform_deltas["unique_pcs"]
    added_repeated = uniform_deltas["repeated_dynamic_instruction_proxy"]
    return {
        "probe_repetitions": repetitions,
        "case_count": len(cases),
        "all_cases_comparable": bool(cases) and all(case["offline_check"]["pass"] for case in cases),
        "minimum_dynamic_instruction_count": min(dynamic_counts) if dynamic_counts else 0,
        "maximum_dynamic_instruction_count": max(dynamic_counts) if dynamic_counts else 0,
        "uniform_deltas_from_original": uniform_deltas,
        "incremental_density": {
            "unique_pc_fraction": (
                added_unique / added_dynamic
                if added_dynamic is not None and added_unique is not None and added_dynamic > 0
                else None
            ),
            "repeated_pc_proxy_fraction": (
                added_repeated / added_dynamic
                if added_dynamic is not None and added_repeated is not None and added_dynamic > 0
                else None
            ),
            "scope": (
                "Committed-PC identity is a repetition proxy, not proof that an execution "
                "is coverage-redundant."
            ),
        },
        "cases": cases,
    }


def build_report(
    original_path: Path,
    candidate_paths: dict[int, Path],
) -> dict[str, Any]:
    original_path = original_path.resolve()
    original = _load_json(original_path)
    original_cases = _cases_by_ordinal(original)
    reasons: list[str] = []
    variants: list[dict[str, Any]] = []
    input_candidates: list[dict[str, Any]] = []

    if (original.get("status") != "pass" or not original_cases
            or (original.get("execution") or {}).get("complete") is not True):
        reasons.append("original_campaign_not_complete_pass")

    if any(type(repetitions) is not int or repetitions <= 0 for repetitions in candidate_paths):
        raise ValueError("candidate repetitions must be positive integers")
    for repetitions, raw_path in sorted(candidate_paths.items()):
        candidate_path = raw_path.resolve()
        candidate = _load_json(candidate_path)
        parameters = candidate.get("parameters") or {}
        candidate_cases = _cases_by_ordinal(candidate)
        variant_reasons: list[str] = []
        if candidate.get("status") != "pass":
            variant_reasons.append("campaign_not_pass")
        if (candidate.get("execution") or {}).get("complete") is not True:
            variant_reasons.append("campaign_incomplete")
        if parameters.get("directed_pattern") != EXPECTED_PATTERN:
            variant_reasons.append("directed_pattern_mismatch")
        if parameters.get("page_tail_attribute_mode") != EXPECTED_ATTRIBUTE_MODE:
            variant_reasons.append("attribute_mode_mismatch")
        if (type(parameters.get("page_tail_probe_repetitions")) is not int
                or parameters["page_tail_probe_repetitions"] != repetitions):
            variant_reasons.append("probe_repetitions_mismatch")
        for field in (
            "master_seed", "timing_master_seed", "families", "family_case_counts",
            "planned_case_count", "repo_head",
        ):
            if (field not in candidate or field not in original
                    or type(candidate[field]) is not type(original[field])
                    or candidate[field] != original[field]):
                variant_reasons.append(f"campaign_{field}_mismatch")
        for label, manifest, cases in (("original", original, original_cases),
                                       ("candidate", candidate, candidate_cases)):
            count = manifest.get("planned_case_count")
            if type(count) is not int or count != len(cases):
                variant_reasons.append(f"{label}_planned_case_count_mismatch")
        if set(candidate_cases) != set(original_cases):
            variant_reasons.append("case_ordinal_set_mismatch")

        case_summaries: list[dict[str, Any]] = []
        for ordinal in sorted(set(candidate_cases) & set(original_cases)):
            original_record = original_cases[ordinal]
            candidate_record = candidate_cases[ordinal]
            comparison = build_candidate_report(
                original_campaign_path=original_path,
                candidate_campaign_path=candidate_path,
                original_name=str(original_record.get("name") or ""),
                candidate_name=str(candidate_record.get("name") or ""),
            )
            case_summary = _case_summary(comparison, repetitions, ordinal)
            candidate_spec = candidate_record.get("spec") or {}
            if (type(candidate_spec.get("page_tail_probe_repetitions")) is not int
                    or candidate_spec["page_tail_probe_repetitions"] != repetitions):
                case_summary["offline_check"]["pass"] = False
                case_summary["offline_check"]["reasons"].append(
                    "case_probe_repetitions_mismatch"
                )
            scenario_path = Path(comparison["candidate"]["inputs"]["scenario_spec"]["path"])
            if _sha256(scenario_path) != candidate_record["scenario_spec_sha256"]:
                raise ValueError("candidate scenario spec changed during sweep")
            scenario = _load_json(scenario_path)
            scenario_parameters = scenario.get("parameters") or {}
            reuse = scenario_parameters.get("pattern_reuse") or {}
            attribute_plan = (
                (scenario_parameters.get("translation") or {}).get("attribute_plan")
                or {}
            )
            expected_profile, expected_pma_pattern = _expected_attribute_plan(ordinal)
            for passed, reason in (
                (reuse.get("attribute_mode") == EXPECTED_ATTRIBUTE_MODE,
                 "case_attribute_mode_mismatch"),
                (type(reuse.get("probe_repetitions")) is int and reuse["probe_repetitions"] == repetitions,
                 "case_scenario_probe_repetitions_mismatch"),
                (reuse.get("probe_repetitions_decoupled_from_program_iterations") is True,
                 "case_probe_repetitions_not_decoupled"),
                (attribute_plan.get("profile") == expected_profile,
                 "case_attribute_profile_mismatch"),
                (attribute_plan.get("pma_pattern") == expected_pma_pattern,
                 "case_pma_pattern_mismatch"),
            ):
                if not passed:
                    case_summary["offline_check"]["pass"] = False
                    case_summary["offline_check"]["reasons"].append(reason)
            if case_summary["dynamic_instruction_count"] < 5000:
                case_summary["offline_check"]["pass"] = False
                case_summary["offline_check"]["reasons"].append(
                    "minimum_dynamic_instruction_count_not_met"
                )
            case_summaries.append(case_summary)
            variant_reasons.extend(
                f"case_{ordinal}:{reason}"
                for reason in case_summary["offline_check"]["reasons"]
            )

        variant = _variant_summary(repetitions, case_summaries)
        variant["comparison_reasons"] = variant_reasons
        if variant_reasons:
            variant["all_cases_comparable"] = False
            reasons.extend(f"pr{repetitions:02d}:{reason}" for reason in variant_reasons)
        variants.append(variant)
        input_candidates.append(
            {
                "probe_repetitions": repetitions,
                "path": str(candidate_path),
                "sha256": _sha256(candidate_path),
            }
        )

    if not variants:
        reasons.append("no_candidate_variants")
    comparable = bool(variants) and not reasons and all(
        variant["all_cases_comparable"] for variant in variants
    )
    lowest_repetition = min(variant["probe_repetitions"] for variant in variants) if comparable else None
    return {
        "schema_version": 2,
        "evidence_scope": "historical_diagnostic_only",
        "canonical_hit_eligible": False,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "comparison_kind": "page_tail_probe_repetition_sweep",
        "policy": (
            "Historical comparison of retained program inputs and trace costs."
        ),
        "inputs": {
            "original_campaign": {
                "path": str(original_path),
                "sha256": _sha256(original_path),
            },
            "candidate_campaigns": input_candidates,
        },
        "hypothesis": {
            "change": (
                "Keep the ten-iteration random body and rvi_nc_pair attributes fixed while "
                "reducing only the in-loop page-tail pair executions."
            ),
            "predeclared_target_bin_ids": PREDECLARED_TARGET_BIN_IDS,
            "proved_offline": False,
        },
        "variants": variants,
        "selection": {
            "lowest_comparable_repetition_count": lowest_repetition,
            "coverage_equivalence_claimed": False,
        },
        "offline_check": {
            "pass": comparable,
            "reasons": reasons,
            "status": "comparable" if comparable else "not_comparable",
        },
        "qualification": {
            "pass": False,
            "status": "historical_diagnostic_only",
            "DUT_status": "not_run",
            "coverage_hit": False,
        },
    }


def _parse_candidate(value: str) -> tuple[int, Path]:
    raw_repetitions, separator, raw_path = value.partition("=")
    if not separator or not raw_path:
        raise argparse.ArgumentTypeError("candidate must use REPETITIONS=CAMPAIGN_JSON")
    try:
        repetitions = int(raw_repetitions, 0)
    except ValueError as exc:
        raise argparse.ArgumentTypeError("candidate repetitions must be an integer") from exc
    if repetitions <= 0:
        raise argparse.ArgumentTypeError("candidate repetitions must be > 0")
    return repetitions, Path(raw_path)


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--original", type=Path, required=True)
    parser.add_argument(
        "--candidate",
        action="append",
        type=_parse_candidate,
        required=True,
        metavar="REPETITIONS=CAMPAIGN_JSON",
    )
    parser.add_argument("--output", type=Path, required=True)
    return parser.parse_args()


def main() -> int:
    args = _parse_args()
    if args.output.exists():
        raise SystemExit(f"refusing to overwrite output: {args.output}")
    candidate_paths = dict(args.candidate)
    if len(candidate_paths) != len(args.candidate):
        raise SystemExit("duplicate repetition count in --candidate")
    report = build_report(args.original, candidate_paths)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("x", encoding="utf-8") as stream:
        json.dump(report, stream, indent=2, sort_keys=True)
        stream.write("\n")
    print(
        f"page-tail-repetition-sweep status={report['offline_check']['status']} "
        f"variants={len(report['variants'])} output={args.output.resolve()}"
    )
    return 0 if report["offline_check"]["pass"] else 1


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Compare two page-tail campaigns that differ only in translation attributes."""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


EXPECTED_PATTERN = "page_tail_pair_sparse"
CONTROL_MODE = "alternating"
CANDIDATE_MODE = "rvi_nc_pair"
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


def _input_binding(
    record: dict[str, Any],
    *,
    path_key: str,
    hash_key: str | None,
) -> tuple[dict[str, Any], dict[str, Any] | None]:
    path = Path(str(record.get(path_key) or ""))
    exists = path.is_file()
    actual = _sha256(path) if exists else None
    expected = str(record.get(hash_key) or "") if hash_key else None
    passed = exists and (hash_key is None or actual == expected)
    binding = {
        "path": str(path),
        "actual_sha256": actual,
        "recorded_sha256": expected,
        "pass": passed,
    }
    return binding, _load_json(path) if passed and path.suffix == ".json" else None


def _normalized_program_map(payload: dict[str, Any]) -> dict[str, Any]:
    normalized = copy.deepcopy(payload)
    normalized.pop("binary_path", None)
    normalized.pop("source_path", None)
    return normalized


def _normalized_realization(payload: dict[str, Any]) -> dict[str, Any]:
    normalized = copy.deepcopy(payload)
    normalized.pop("artifact_paths", None)
    return normalized


def _normalized_scenario(payload: dict[str, Any]) -> dict[str, Any]:
    normalized = copy.deepcopy(payload)
    parameters = normalized.get("parameters")
    if isinstance(parameters, dict):
        reuse = parameters.get("pattern_reuse")
        if isinstance(reuse, dict):
            reuse.pop("attribute_mode", None)
        translation = parameters.get("translation")
        if isinstance(translation, dict):
            translation.pop("attribute_plan", None)
    return normalized


def _case_identity(record: dict[str, Any]) -> tuple[Any, ...]:
    spec = record.get("spec") or {}
    return (
        str(spec.get("family") or ""),
        int(spec.get("ordinal", -1)),
        int(spec.get("seed", -1)),
        int(record.get("timing_seed", -1)),
    )


def _expected_candidate_plan(ordinal: int) -> tuple[str, list[int]]:
    if int(ordinal) % 2 == 0:
        return "page_tail_rvc_nc_to_cacheable_rvi_nc_pair", [1, 0, 1, 0, 0]
    return "page_tail_rvc_cacheable_to_nc_rvi_nc_pair", [0, 1, 0, 0, 0]


def _case_inputs(record: dict[str, Any]) -> tuple[dict[str, Any], dict[str, Any]]:
    bindings: dict[str, Any] = {}
    payloads: dict[str, Any] = {}
    fields = (
        ("source", "source_path", "source_sha256"),
        ("binary", "bin_path", "bin_sha256"),
        ("trace", "trace_path", "trace_sha256"),
        ("program_map", "program_map_path", "program_map_sha256"),
        ("scenario_spec", "scenario_spec_path", "scenario_spec_sha256"),
        ("scenario_realization", "scenario_realization_path", None),
        ("configuration_preflight", "configuration_preflight_path", None),
    )
    for label, path_key, hash_key in fields:
        binding, payload = _input_binding(
            record, path_key=path_key, hash_key=hash_key
        )
        bindings[label] = binding
        if payload is not None:
            payloads[label] = payload
    return bindings, payloads


def build_report(control_path: Path, candidate_path: Path) -> dict[str, Any]:
    control = _load_json(control_path)
    candidate = _load_json(candidate_path)
    reasons: list[str] = []

    for label, campaign, expected_mode in (
        ("control", control, CONTROL_MODE),
        ("candidate", candidate, CANDIDATE_MODE),
    ):
        parameters = campaign.get("parameters") or {}
        if campaign.get("status") != "pass":
            reasons.append(f"{label}_campaign_not_pass")
        if not (campaign.get("execution") or {}).get("complete"):
            reasons.append(f"{label}_campaign_incomplete")
        if parameters.get("directed_pattern") != EXPECTED_PATTERN:
            reasons.append(f"{label}_directed_pattern_mismatch")
        if parameters.get("page_tail_attribute_mode") != expected_mode:
            reasons.append(f"{label}_attribute_mode_mismatch")

    common_campaign_fields = (
        "master_seed",
        "timing_master_seed",
        "families",
        "family_case_counts",
        "planned_case_count",
        "repo_head",
    )
    for field in common_campaign_fields:
        if control.get(field) != candidate.get(field):
            reasons.append(f"campaign_{field}_mismatch")
    control_parameters = dict(control.get("parameters") or {})
    candidate_parameters = dict(candidate.get("parameters") or {})
    control_parameters.pop("page_tail_attribute_mode", None)
    candidate_parameters.pop("page_tail_attribute_mode", None)
    if control_parameters != candidate_parameters:
        reasons.append("campaign_non_attribute_parameters_mismatch")

    control_cases = {
        _case_identity(record): record for record in control.get("cases") or []
    }
    candidate_cases = {
        _case_identity(record): record for record in candidate.get("cases") or []
    }
    if not control_cases or set(control_cases) != set(candidate_cases):
        reasons.append("case_identity_set_mismatch")

    case_reports: list[dict[str, Any]] = []
    for identity in sorted(set(control_cases) & set(candidate_cases)):
        control_record = control_cases[identity]
        candidate_record = candidate_cases[identity]
        control_bindings, control_payloads = _case_inputs(control_record)
        candidate_bindings, candidate_payloads = _case_inputs(candidate_record)
        case_reasons: list[str] = []
        if not bool(control_record.get("qualification_pass")):
            case_reasons.append("control_not_qualified")
        if not bool(candidate_record.get("qualification_pass")):
            case_reasons.append("candidate_not_qualified")
        for side, bindings in (
            ("control", control_bindings),
            ("candidate", candidate_bindings),
        ):
            for label, binding in bindings.items():
                if not binding["pass"]:
                    case_reasons.append(f"{side}_{label}_integrity_failed")

        program_hash_fields = (
            "source_sha256",
            "bin_sha256",
            "trace_sha256",
            "generator_sha256",
            "logical_static_instructions",
            "dynamic_trace_instructions",
        )
        for field in program_hash_fields:
            if control_record.get(field) != candidate_record.get(field):
                case_reasons.append(f"program_{field}_mismatch")

        maps_equal = (
            _normalized_program_map(control_payloads.get("program_map") or {})
            == _normalized_program_map(candidate_payloads.get("program_map") or {})
        )
        if not maps_equal:
            case_reasons.append("normalized_program_map_mismatch")
        realizations_equal = (
            _normalized_realization(
                control_payloads.get("scenario_realization") or {}
            )
            == _normalized_realization(
                candidate_payloads.get("scenario_realization") or {}
            )
        )
        if not realizations_equal:
            case_reasons.append("normalized_scenario_realization_mismatch")

        control_scenario = control_payloads.get("scenario_spec") or {}
        candidate_scenario = candidate_payloads.get("scenario_spec") or {}
        scenarios_equal_except_attributes = (
            _normalized_scenario(control_scenario)
            == _normalized_scenario(candidate_scenario)
        )
        if not scenarios_equal_except_attributes:
            case_reasons.append("scenario_non_attribute_fields_mismatch")
        control_reuse = (
            (control_scenario.get("parameters") or {}).get("pattern_reuse") or {}
        )
        candidate_reuse = (
            (candidate_scenario.get("parameters") or {}).get("pattern_reuse") or {}
        )
        control_plan = (
            ((control_scenario.get("parameters") or {}).get("translation") or {}).get(
                "attribute_plan"
            )
            or {}
        )
        candidate_plan = (
            ((candidate_scenario.get("parameters") or {}).get("translation") or {}).get(
                "attribute_plan"
            )
            or {}
        )
        if control_reuse.get("attribute_mode") != CONTROL_MODE:
            case_reasons.append("control_scenario_attribute_mode_mismatch")
        if candidate_reuse.get("attribute_mode") != CANDIDATE_MODE:
            case_reasons.append("candidate_scenario_attribute_mode_mismatch")
        expected_profile, expected_pma_pattern = _expected_candidate_plan(identity[1])
        if candidate_plan.get("profile") != expected_profile:
            case_reasons.append("candidate_attribute_profile_mismatch")
        if candidate_plan.get("pma_pattern") != expected_pma_pattern:
            case_reasons.append("candidate_pma_pattern_mismatch")

        for side, payloads in (
            ("control", control_payloads),
            ("candidate", candidate_payloads),
        ):
            configuration = payloads.get("configuration_preflight") or {}
            realization = payloads.get("scenario_realization") or {}
            if configuration.get("status") != "valid" or configuration.get(
                "reasons"
            ):
                case_reasons.append(f"{side}_configuration_invalid")
            if realization.get("status") != "REALIZED" or realization.get(
                "missing_events"
            ):
                case_reasons.append(f"{side}_scenario_not_realized")

        case_reports.append(
            {
                "identity": {
                    "family": identity[0],
                    "ordinal": identity[1],
                    "program_seed": identity[2],
                    "timing_seed": identity[3],
                },
                "name": str(candidate_record.get("name") or ""),
                "qualification": {
                    "pass": not case_reasons,
                    "reasons": case_reasons,
                },
                "program_identity": {
                    "source_binary_trace_hashes_equal": all(
                        control_record.get(field) == candidate_record.get(field)
                        for field in ("source_sha256", "bin_sha256", "trace_sha256")
                    ),
                    "normalized_program_map_equal": maps_equal,
                    "normalized_scenario_realization_equal": realizations_equal,
                    "dynamic_instruction_count": int(
                        candidate_record.get("dynamic_trace_instructions") or 0
                    ),
                    "logical_static_instruction_count": int(
                        candidate_record.get("logical_static_instructions") or 0
                    ),
                },
                "scenario_delta": {
                    "only_attribute_fields_changed": scenarios_equal_except_attributes,
                    "control_attribute_mode": control_reuse.get("attribute_mode"),
                    "control_attribute_plan": control_plan,
                    "candidate_attribute_mode": candidate_reuse.get("attribute_mode"),
                    "candidate_attribute_plan": candidate_plan,
                },
                "inputs": {
                    "control": control_bindings,
                    "candidate": candidate_bindings,
                },
            }
        )
        reasons.extend(
            f"case_{identity[1]}:{reason}" for reason in case_reasons
        )

    passed = not reasons
    return {
        "schema_version": 1,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "comparison_kind": "page_tail_translation_attribute_only",
        "policy": (
            "This report proves input integrity, same-program identity, and "
            "configuration-only qualification. It does not prove DUT behavior or "
            "functional-coverage HIT."
        ),
        "inputs": {
            "control_campaign": {
                "path": str(control_path.resolve()),
                "sha256": _sha256(control_path),
            },
            "candidate_campaign": {
                "path": str(candidate_path.resolve()),
                "sha256": _sha256(candidate_path),
            },
        },
        "hypothesis": {
            "root_cause": (
                "The alternating seed0 RVI probe crosses from a non-cacheable "
                "page into a cacheable page, so its second half returns through "
                "ICache rather than an InstrUncache recovery request."
            ),
            "candidate_change": (
                "Keep virtual pages 3 and 4 non-cacheable while preserving the "
                "sparse page-tail program and independent timing seed."
            ),
            "predeclared_target_bin_ids": PREDECLARED_TARGET_BIN_IDS,
            "proved_offline": False,
        },
        "case_count": len(case_reports),
        "cases": case_reports,
        "qualification": {
            "pass": passed,
            "reasons": reasons,
            "status": (
                "qualified_for_serialized_DUT_replay" if passed else "not_qualified"
            ),
            "DUT_status": "not_run",
            "coverage_hit": False,
        },
    }


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--control", type=Path, required=True)
    parser.add_argument("--candidate", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    return parser.parse_args()


def main() -> int:
    args = _parse_args()
    if args.output.exists():
        raise SystemExit(f"refusing to overwrite output: {args.output}")
    report = build_report(args.control, args.candidate)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(
        json.dumps(report, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )
    print(
        f"page-tail-attribute-compare status={report['qualification']['status']} "
        f"cases={report['case_count']} output={args.output}"
    )
    return 0 if report["qualification"]["pass"] else 1


if __name__ == "__main__":
    raise SystemExit(main())

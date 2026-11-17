#!/usr/bin/env python3
"""Qualify a timing-only candidate against NEMU-qualified program inputs."""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _load(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"expected a JSON object: {path}")
    return payload


def _binding(path: Path, expected: str) -> dict[str, Any]:
    exists = path.is_file()
    actual = _sha256(path) if exists else "missing"
    return {
        "path": str(path.resolve()) if exists else str(path),
        "recorded_sha256": str(expected),
        "actual_sha256": actual,
        "pass": bool(expected) and actual == str(expected),
    }


def _normalized_candidate_scenario(
    payload: dict[str, Any], *, candidate_kind: str
) -> dict[str, Any]:
    normalized = copy.deepcopy(payload)
    parameters = normalized.get("parameters") or {}
    parameters.pop("uncache_a_stall_reuse", None)
    timing = parameters.get("timing_plan") or {}
    timing.pop("uncache_a_stall_cycles", None)
    policy = timing.get("policy") or {}
    policy.pop("uncache_a_stall", None)
    if candidate_kind == "pbmt_nc_stall":
        reuse = parameters.get("pattern_reuse") or {}
        reuse.pop("attribute_mode", None)
        translation = parameters.get("translation") or {}
        translation.pop("attribute_plan", None)
    normalized["required_events"] = [
        item
        for item in normalized.get("required_events") or []
        if item.get("event")
        not in {"dut.uncache_a_stall_cycle", "dut.uncache_a_stall_release"}
    ]
    return normalized


def _expected_pbmt_nc_plan(ordinal: int) -> dict[str, Any]:
    if int(ordinal) % 2 == 0:
        return {
            "profile": "page_tail_rvc_pma_nc_rvi_pbmt_nc_pair",
            "pbmt": 0,
            "pbmte": 1,
            "pma_pattern": [1, 0, 1, 1, 1],
            "pbmt_pattern": [0, 0, 0, 1, 1],
        }
    return {
        "profile": "page_tail_rvc_cacheable_rvi_pbmt_nc_pair",
        "pbmt": 0,
        "pbmte": 1,
        "pma_pattern": [0, 1, 0, 1, 1],
        "pbmt_pattern": [0, 0, 0, 1, 1],
    }


def _case_index(campaign: dict[str, Any]) -> dict[str, dict[str, Any]]:
    return {
        str(record.get("name") or ""): record
        for record in campaign.get("cases") or []
        if isinstance(record, dict) and record.get("name")
    }


def build_report(
    control_campaign_path: Path,
    candidate_campaign_path: Path,
    *,
    candidate_kind: str = "timing_only",
) -> dict[str, Any]:
    if candidate_kind not in {"timing_only", "pbmt_nc_stall"}:
        raise ValueError(f"unsupported candidate kind: {candidate_kind}")
    control = _load(control_campaign_path)
    candidate = _load(candidate_campaign_path)
    reasons: list[str] = []
    if control.get("status") != "pass":
        reasons.append("control_campaign_not_pass")
    if not (control.get("execution") or {}).get("complete"):
        reasons.append("control_campaign_incomplete")
    if candidate.get("status") != "generated" or candidate.get("mode") != "generate":
        reasons.append("candidate_campaign_not_generate_only")
    for field in (
        "master_seed",
        "timing_master_seed",
        "families",
        "family_case_counts",
        "planned_case_count",
        "repo_head",
    ):
        if control.get(field) != candidate.get(field):
            reasons.append(f"campaign_{field}_mismatch")
    control_parameters = dict(control.get("parameters") or {})
    candidate_parameters = dict(candidate.get("parameters") or {})
    stall_cycles = candidate_parameters.pop("uncache_a_stall_cycles", None)
    if candidate_kind == "pbmt_nc_stall":
        control_mode = control_parameters.pop("page_tail_attribute_mode", None)
        candidate_mode = candidate_parameters.pop("page_tail_attribute_mode", None)
        if control_mode != "rvi_nc_pair":
            reasons.append("control_attribute_mode_mismatch")
        if candidate_mode != "rvi_pbmt_nc_pair":
            reasons.append("candidate_attribute_mode_mismatch")
    if control_parameters != candidate_parameters:
        reasons.append("campaign_non_candidate_parameters_mismatch")
    if not isinstance(stall_cycles, int) or not 1 <= stall_cycles <= 1024:
        reasons.append("candidate_stall_cycles_invalid")

    control_cases = _case_index(control)
    candidate_cases = _case_index(candidate)
    if not control_cases or set(control_cases) != set(candidate_cases):
        reasons.append("case_identity_set_mismatch")

    case_reports: list[dict[str, Any]] = []
    for name in sorted(set(control_cases) & set(candidate_cases)):
        original = control_cases[name]
        timing_candidate = candidate_cases[name]
        case_reasons: list[str] = []
        if original.get("qualification_pass") is not True:
            case_reasons.append("control_case_not_nemu_qualified")
        for field in (
            "spec",
            "source_sha256",
            "generator_sha256",
            "logical_static_instructions",
            "timing_seed",
        ):
            if original.get(field) != timing_candidate.get(field):
                case_reasons.append(f"case_{field}_mismatch")

        source_control = _binding(
            Path(str(original.get("source_path") or "")),
            str(original.get("source_sha256") or ""),
        )
        source_candidate = _binding(
            Path(str(timing_candidate.get("source_path") or "")),
            str(timing_candidate.get("source_sha256") or ""),
        )
        generator_control_path = Path(str(original.get("generator_manifest_path") or ""))
        generator_candidate_path = Path(
            str(timing_candidate.get("generator_manifest_path") or "")
        )
        generator_control_sha = (
            _sha256(generator_control_path) if generator_control_path.is_file() else "missing"
        )
        generator_candidate_sha = (
            _sha256(generator_candidate_path)
            if generator_candidate_path.is_file()
            else "missing"
        )
        inherited = {
            label: _binding(
                Path(str(original.get(path_key) or "")),
                str(original.get(hash_key) or ""),
            )
            for label, path_key, hash_key in (
                ("binary", "bin_path", "bin_sha256"),
                ("trace", "trace_path", "trace_sha256"),
                ("program_map", "program_map_path", "program_map_sha256"),
            )
        }
        control_scenario_binding = _binding(
            Path(str(original.get("scenario_spec_path") or "")),
            str(original.get("scenario_spec_sha256") or ""),
        )
        candidate_scenario_binding = _binding(
            Path(str(timing_candidate.get("scenario_spec_path") or "")),
            str(timing_candidate.get("scenario_spec_sha256") or ""),
        )
        if not all(
            binding["pass"]
            for binding in (
                source_control,
                source_candidate,
                control_scenario_binding,
                candidate_scenario_binding,
                *inherited.values(),
            )
        ):
            case_reasons.append("input_hash_binding_failed")
        if source_control["actual_sha256"] != source_candidate["actual_sha256"]:
            case_reasons.append("source_bytes_changed")
        if (
            generator_control_sha == "missing"
            or generator_control_sha != generator_candidate_sha
        ):
            case_reasons.append("generator_manifest_bytes_changed")

        control_scenario = _load(Path(control_scenario_binding["path"]))
        candidate_scenario = _load(Path(candidate_scenario_binding["path"]))
        if _normalized_candidate_scenario(
            control_scenario, candidate_kind=candidate_kind
        ) != _normalized_candidate_scenario(
            candidate_scenario, candidate_kind=candidate_kind
        ):
            case_reasons.append("scenario_changed_outside_approved_delta")
        required = {
            str(item.get("event") or ""): int(item.get("min_count") or 0)
            for item in candidate_scenario.get("required_events") or []
        }
        if required.get("dut.uncache_a_stall_cycle") != stall_cycles:
            case_reasons.append("stall_cycle_requirement_mismatch")
        if required.get("dut.uncache_a_stall_release") != 1:
            case_reasons.append("stall_release_requirement_missing")
        candidate_parameters_payload = candidate_scenario.get("parameters") or {}
        if candidate_kind == "pbmt_nc_stall":
            candidate_reuse = candidate_parameters_payload.get("pattern_reuse") or {}
            candidate_plan = candidate_parameters_payload.get("translation") or {}
            candidate_plan = candidate_plan.get("attribute_plan") or {}
            if candidate_reuse.get("attribute_mode") != "rvi_pbmt_nc_pair":
                case_reasons.append("candidate_scenario_attribute_mode_mismatch")
            expected_plan = _expected_pbmt_nc_plan(
                int((timing_candidate.get("spec") or {}).get("ordinal", -1))
            )
            if any(candidate_plan.get(key) != value for key, value in expected_plan.items()):
                case_reasons.append("candidate_pbmt_nc_attribute_plan_mismatch")
        reuse = (candidate_scenario.get("parameters") or {}).get(
            "uncache_a_stall_reuse"
        ) or {}
        source_path = Path(__file__).resolve().parents[2] / str(
            reuse.get("source_path") or ""
        )
        source_provenance = _binding(
            source_path,
            str(reuse.get("source_sha256") or ""),
        )
        if not source_provenance["pass"]:
            case_reasons.append("directed_source_provenance_failed")

        case_reports.append(
            {
                "name": name,
                "pass": not case_reasons,
                "reasons": case_reasons,
                "program_identity": {
                    "source_control": source_control,
                    "source_candidate": source_candidate,
                    "generator_manifest_control_sha256": generator_control_sha,
                    "generator_manifest_candidate_sha256": generator_candidate_sha,
                    "source_and_generator_unchanged": (
                        source_control["actual_sha256"]
                        == source_candidate["actual_sha256"]
                        and generator_control_sha == generator_candidate_sha
                    ),
                },
                "inherited_nemu_inputs": inherited,
                "scenario": {
                    "control": control_scenario_binding,
                    "candidate": candidate_scenario_binding,
                    "only_timing_action_changed": (
                        candidate_kind == "timing_only"
                        and control_scenario
                        == _normalized_candidate_scenario(
                            candidate_scenario, candidate_kind=candidate_kind
                        )
                    ),
                    "only_pbmt_nc_attribute_and_timing_changed": (
                        candidate_kind == "pbmt_nc_stall"
                        and _normalized_candidate_scenario(
                            control_scenario, candidate_kind=candidate_kind
                        )
                        == _normalized_candidate_scenario(
                            candidate_scenario, candidate_kind=candidate_kind
                        )
                    ),
                },
                "directed_source_provenance": source_provenance,
                "metrics": {
                    "static_instruction_delta": 0,
                    "dynamic_instruction_delta": 0,
                    "cfg_edge_delta": 0,
                    "repeated_instruction_delta": 0,
                    "scope": "identical inherited program and NEMU trace",
                },
            }
        )
    failed_cases = [item["name"] for item in case_reports if not item["pass"]]
    reasons.extend(f"case_failed:{name}" for name in failed_cases)
    passed = not reasons
    return {
        "schema_version": 1,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "inputs": {
            "control_campaign": {
                "path": str(control_campaign_path.resolve()),
                "sha256": _sha256(control_campaign_path),
            },
            "candidate_campaign": {
                "path": str(candidate_campaign_path.resolve()),
                "sha256": _sha256(candidate_campaign_path),
            },
        },
        "stall_cycles": stall_cycles,
        "candidate_kind": candidate_kind,
        "case_count": len(case_reports),
        "cases": case_reports,
        "qualification": {
            "pass": passed,
            "status": "qualified" if passed else "not_qualified",
            "reasons": reasons,
            "input_qualification": passed,
            "nemu_replay_required": False if passed else None,
            "dut_replay_completed": False,
            "coverage_hit_claimed": False,
            "scope": (
                "Program bytes and NEMU trace are inherited unchanged; only the "
                + (
                    "scenario timing contract"
                    if candidate_kind == "timing_only"
                    else "reviewed PBMT.NC page-tail attribute and scenario timing contracts"
                )
                + " are qualified. DUT behavior and coverage remain unproved until "
                "serialized replay and independent audit."
            ),
        },
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--control-campaign", type=Path, required=True)
    parser.add_argument("--candidate-campaign", type=Path, required=True)
    parser.add_argument(
        "--candidate-kind",
        choices=("timing_only", "pbmt_nc_stall"),
        default="timing_only",
    )
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    report = build_report(
        args.control_campaign,
        args.candidate_campaign,
        candidate_kind=args.candidate_kind,
    )
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("x", encoding="utf-8") as stream:
        json.dump(report, stream, indent=2, sort_keys=True)
        stream.write("\n")
    print(
        f"qualification={report['qualification']['status']} "
        f"cases={report['case_count']} output={args.output}"
    )
    return 0 if report["qualification"]["pass"] else 1


if __name__ == "__main__":
    raise SystemExit(main())

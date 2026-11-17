#!/usr/bin/env python3
"""Compare two qualified same-program DUT replays across one reviewed delta."""
from __future__ import annotations

import argparse
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
    value = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(value, dict):
        raise ValueError(f"expected JSON object: {path}")
    return value


def _qualified_artifact(path: Path, expected_case: str) -> tuple[dict, dict, set[str], list[str]]:
    qualification = _load(path)
    failures: list[str] = []
    if qualification.get("scenario_id") != expected_case:
        failures.append("scenario_id_mismatch")
    if not qualification.get("replay_artifact_qualification", {}).get("pass"):
        failures.append("replay_not_qualified")
    if not qualification.get("dut_pass", {}).get("pass"):
        failures.append("DUT_not_passed")
    artifact_input = (qualification.get("inputs") or {}).get("artifact") or {}
    artifact_path = Path(str(artifact_input.get("path") or "/nonexistent")).resolve()
    expected_sha = str(artifact_input.get("sha256") or "")
    actual_sha = _sha256(artifact_path) if artifact_path.is_file() else "missing"
    if not expected_sha or actual_sha != expected_sha:
        failures.append("artifact_hash_mismatch")
        artifact: dict[str, Any] = {}
    else:
        artifact = _load(artifact_path)
    provenance = artifact.get("provenance") or {}
    signature = str(provenance.get("compatibility_signature") or "")
    if signature != str(qualification.get("recorded_compatibility_signature") or ""):
        failures.append("qualification_artifact_contract_mismatch")
    definitions = sorted(
        str(item.get("bin_id") or "")
        for item in artifact.get("definitions") or []
        if isinstance(item, dict) and item.get("bin_id")
    )
    hits = {
        str(item.get("bin_id") or "")
        for item in (artifact.get("hits") or {}).values()
        if isinstance(item, dict)
        and item.get("bin_id")
        and int(item.get("hits") or 0) > 0
    }
    summary = {
        "qualification_path": str(path.resolve()),
        "qualification_sha256": _sha256(path),
        "artifact_path": str(artifact_path),
        "artifact_expected_sha256": expected_sha,
        "artifact_actual_sha256": actual_sha,
        "compatibility_signature": signature,
        "definitions_sha256": str(provenance.get("definitions_sha256") or ""),
        "definition_bin_ids": definitions,
        "raw_hit_count": len(hits),
        "declared_target_bin_ids": sorted(
            str(value)
            for value in (artifact.get("coverage_targets") or {}).get("bin_ids") or []
        ),
        "pass": not failures,
        "failures": failures,
    }
    return qualification, summary, hits, failures


def build_report(
    *,
    original_qualification_path: Path,
    candidate_qualification_path: Path,
    timing_candidate_audit_path: Path,
    case_name: str,
) -> dict[str, Any]:
    original_qualification_path = original_qualification_path.resolve()
    candidate_qualification_path = candidate_qualification_path.resolve()
    timing_candidate_audit_path = timing_candidate_audit_path.resolve()
    timing = _load(timing_candidate_audit_path)
    timing_failures: list[str] = []
    qualification = timing.get("qualification") or {}
    candidate_kind = str(timing.get("candidate_kind") or "timing_only")
    if candidate_kind not in {"timing_only", "pbmt_nc_stall"}:
        timing_failures.append("unsupported_candidate_kind")
    if not (
        qualification.get("pass") is True
        and qualification.get("status") == "qualified"
        and qualification.get("input_qualification") is True
        and qualification.get("nemu_replay_required") is False
        and qualification.get("dut_replay_completed") is False
        and qualification.get("coverage_hit_claimed") is False
    ):
        timing_failures.append("timing_candidate_audit_not_qualified")
    matches = [
        record for record in timing.get("cases") or [] if record.get("name") == case_name
    ]
    if len(matches) != 1:
        timing_failures.append(f"timing_candidate_case_matches={len(matches)}")
        timing_case: dict[str, Any] = {}
    else:
        timing_case = matches[0]
        zero_metrics = {
            "cfg_edge_delta": 0,
            "dynamic_instruction_delta": 0,
            "repeated_instruction_delta": 0,
            "scope": "identical inherited program and NEMU trace",
            "static_instruction_delta": 0,
        }
        if timing_case.get("pass") is not True or timing_case.get("reasons"):
            timing_failures.append("timing_candidate_case_not_qualified")
        if timing_case.get("metrics") != zero_metrics:
            timing_failures.append("timing_candidate_program_delta_not_zero")
        scenario = timing_case.get("scenario", {})
        if candidate_kind == "timing_only":
            if scenario.get("only_timing_action_changed") is not True:
                timing_failures.append("scenario_delta_not_timing_only")
        elif candidate_kind == "pbmt_nc_stall":
            if scenario.get("only_pbmt_nc_attribute_and_timing_changed") is not True:
                timing_failures.append("scenario_delta_not_pbmt_nc_stall_only")
        if (
            timing_case.get("program_identity", {}).get("source_and_generator_unchanged")
            is not True
        ):
            timing_failures.append("program_identity_changed")

    original_q, original, original_hits, original_failures = _qualified_artifact(
        original_qualification_path, case_name
    )
    candidate_q, candidate, candidate_hits, candidate_failures = _qualified_artifact(
        candidate_qualification_path, case_name
    )
    contract_match = bool(
        original.get("compatibility_signature")
        and original.get("compatibility_signature") == candidate.get("compatibility_signature")
    )
    definitions_match = bool(
        original.get("definitions_sha256")
        and original.get("definitions_sha256") == candidate.get("definitions_sha256")
        and original.get("definition_bin_ids") == candidate.get("definition_bin_ids")
    )
    if not contract_match:
        timing_failures.append("compatibility_signature_mismatch")
    if not definitions_match:
        timing_failures.append("functional_coverage_definitions_mismatch")

    original_integrity = original_q.get("input_integrity_review") or {}
    candidate_integrity = candidate_q.get("input_integrity_review") or {}
    if original_integrity.get("status") != "proved":
        timing_failures.append("original_input_integrity_not_proved")
    if candidate_integrity.get("status") != "proved":
        timing_failures.append("candidate_input_integrity_not_proved")
    inherited_binding = candidate_integrity.get("inherited_input_binding") or {}
    if inherited_binding.get("status") != "proved":
        timing_failures.append("candidate_inherited_input_binding_not_proved")
    if inherited_binding.get("sha256") != _sha256(timing_candidate_audit_path):
        timing_failures.append("candidate_timing_audit_hash_mismatch")

    failures = (
        timing_failures
        + [f"original_{reason}" for reason in original_failures]
        + [f"candidate_{reason}" for reason in candidate_failures]
    )
    return {
        "schema_version": 1,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "policy": (
            "This report compares diagnostic raw functional coverage only after both DUT "
            "artifacts independently qualify under one compatibility contract and the "
            "reviewed scenario sidecar proves zero program/trace delta. It never grants "
            "trusted HITs."
        ),
        "case_name": case_name,
        "candidate_kind": candidate_kind,
        "timing_candidate_audit": {
            "path": str(timing_candidate_audit_path),
            "sha256": _sha256(timing_candidate_audit_path),
            "pass": not timing_failures,
            "failures": timing_failures,
        },
        "original": original,
        "candidate": candidate,
        "coverage_contract": {
            "same_compatibility_signature": contract_match,
            "same_definitions": definitions_match,
            "compatibility_signature": (
                original.get("compatibility_signature") if contract_match else None
            ),
            "definitions_sha256": (
                original.get("definitions_sha256") if definitions_match else None
            ),
        },
        "raw_coverage_comparison": {
            "original_raw_hit_count": len(original_hits),
            "candidate_raw_hit_count": len(candidate_hits),
            "raw_hit_count_delta": len(candidate_hits) - len(original_hits),
            "candidate_only_raw_bin_ids": sorted(candidate_hits - original_hits),
            "original_only_raw_bin_ids": sorted(original_hits - candidate_hits),
            "candidate_is_raw_superset": original_hits <= candidate_hits,
            "trusted_coverage_hit_claimed": False,
        },
        "qualification": {
            "pass": not failures,
            "status": "qualified" if not failures else "not_qualified",
            "reasons": failures,
            "trusted_coverage_hit_claimed": False,
        },
    }


def write_json_exclusive(path: Path, payload: dict[str, Any]) -> None:
    path = path.resolve()
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("x", encoding="utf-8") as stream:
        json.dump(payload, stream, indent=2, sort_keys=True)
        stream.write("\n")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--original-qualification", type=Path, required=True)
    parser.add_argument("--candidate-qualification", type=Path, required=True)
    parser.add_argument("--timing-candidate-audit", type=Path, required=True)
    parser.add_argument("--case-name", required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    report = build_report(
        original_qualification_path=args.original_qualification,
        candidate_qualification_path=args.candidate_qualification,
        timing_candidate_audit_path=args.timing_candidate_audit,
        case_name=args.case_name,
    )
    write_json_exclusive(args.output, report)
    comparison = report["raw_coverage_comparison"]
    print(
        f"qualified={int(report['qualification']['pass'])} "
        f"raw={comparison['original_raw_hit_count']}->{comparison['candidate_raw_hit_count']} "
        f"output={args.output.resolve()}"
    )
    return 0 if report["qualification"]["pass"] else 1


if __name__ == "__main__":
    raise SystemExit(main())

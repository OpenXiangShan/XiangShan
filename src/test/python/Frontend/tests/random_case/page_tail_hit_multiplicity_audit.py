#!/usr/bin/env python3
"""Bind full-repeat target multiplicity to full/reduced program event counts.

Equal counts are useful candidate-selection evidence, but are not exact
per-occurrence attribution and do not prove reduced-candidate DUT coverage.
"""

from __future__ import annotations

import argparse
import hashlib
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

try:
    from .program_map import analyze_trace
except ImportError:
    from program_map import analyze_trace


TARGET_BIN_IDS = ("BIN-417", "BIN-1120", "BIN-1121")
COMMON_GENERATOR_FIELDS = (
    "family",
    "seed",
    "ordinal",
    "blocks",
    "iterations",
    "max_call_depth",
    "indirect_target_mode",
    "directed_pattern",
)


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


def _case(campaign: dict[str, Any], name: str) -> dict[str, Any]:
    matches = [record for record in campaign.get("cases") or [] if record.get("name") == name]
    if len(matches) != 1:
        raise ValueError(f"expected exactly one case named {name!r}, found {len(matches)}")
    return matches[0]


def _verified_file(record: dict[str, Any], path_key: str, hash_key: str) -> Path:
    path = Path(str(record.get(path_key) or "")).resolve()
    expected = str(record.get(hash_key) or "")
    if not path.is_file() or not expected or _sha256(path) != expected:
        raise ValueError(f"input binding failed: {path_key}")
    return path


def _event_count(analysis: dict[str, Any], event: str) -> int:
    matches = [int(item["count"]) for item in analysis.get("events") or [] if item.get("event") == event]
    if len(matches) != 1:
        raise ValueError(f"expected one event count for {event}, found {len(matches)}")
    return matches[0]


def _target_hits(artifact: dict[str, Any]) -> dict[str, dict[str, Any]]:
    result: dict[str, dict[str, Any]] = {}
    for key, value in (artifact.get("hits") or {}).items():
        if not isinstance(value, dict):
            continue
        bin_id = str(value.get("bin_id") or "")
        if bin_id in TARGET_BIN_IDS:
            evidence = list(value.get("evidence") or [])
            result[bin_id] = {
                "hit_key": str(key),
                "hit_count": int(value.get("hits") or 0),
                "first_cycle": value.get("first_cycle"),
                "last_cycle": value.get("last_cycle"),
                "retained_evidence_sample_count": len(evidence),
                "retained_evidence_cycles": sorted(
                    {
                        int(cycle)
                        for item in evidence
                        if isinstance(item, dict)
                        for cycle in (item.get("request_cycle"), item.get("cycle"))
                        if cycle is not None
                    }
                ),
            }
    return result


def _program_summary(record: dict[str, Any]) -> dict[str, Any]:
    source_path = _verified_file(record, "source_path", "source_sha256")
    binary_path = _verified_file(record, "bin_path", "bin_sha256")
    trace_path = _verified_file(record, "trace_path", "trace_sha256")
    map_path = _verified_file(record, "program_map_path", "program_map_sha256")
    analysis = analyze_trace(trace_path, _load_json(map_path))
    return {
        "name": str(record.get("name") or ""),
        "qualification_pass": bool(record.get("qualification_pass")),
        "execution_status": record.get("execution_status"),
        "program_seed": int((record.get("spec") or {}).get("seed", -1)),
        "timing_seed": int(record.get("timing_seed", -1)),
        "generator_parameters": dict(record.get("spec") or {}),
        "dynamic_instruction_count": _event_count(analysis, "trace.committed_instruction"),
        "cross_page_instruction_count": _event_count(
            analysis, "trace.cross_page_instruction"
        ),
        "page_tail_rvc_fallthrough_count": _event_count(
            analysis, "trace.page_tail_rvc_fallthrough"
        ),
        "source": {"path": str(source_path), "sha256": _sha256(source_path)},
        "binary": {"path": str(binary_path), "sha256": _sha256(binary_path)},
        "trace": {"path": str(trace_path), "sha256": _sha256(trace_path)},
        "program_map": {"path": str(map_path), "sha256": _sha256(map_path)},
    }


def _replay_binding_reasons(
    *,
    full: dict[str, Any],
    full_campaign_sha256: str,
    artifact: dict[str, Any],
    artifact_sha256: str,
    qualification: dict[str, Any],
    target_audit: dict[str, Any],
) -> list[str]:
    """Bind the counted full-repeat program to the qualified DUT artifact.

    The target audit proves trusted target HITs, while this additional binding
    proves that those HITs came from the exact source/bin/trace whose NEMU
    events are counted by this audit.
    """

    reasons: list[str] = []
    run = artifact.get("run") or {}
    run_seeds = run.get("seeds") or {}
    expected_run_fields = {
        "source_sha256": (
            str(run.get("asm_sha256") or ""),
            str((full.get("source") or {}).get("sha256") or ""),
        ),
        "binary_sha256": (
            str(run.get("bin_sha256") or ""),
            str((full.get("binary") or {}).get("sha256") or ""),
        ),
        "trace_sha256": (
            str(run.get("trace_sha256") or ""),
            str((full.get("trace") or {}).get("sha256") or ""),
        ),
        "program_seed": (
            run.get("seed"),
            full.get("program_seed"),
        ),
        "timing_seed": (
            run_seeds.get("backend"),
            full.get("timing_seed"),
        ),
    }
    for field, (actual, expected) in expected_run_fields.items():
        if actual in (None, "") or actual != expected:
            reasons.append(f"full_DUT_{field}_mismatch")

    qualification_inputs = qualification.get("inputs") or {}
    qualification_artifact = qualification_inputs.get("artifact") or {}
    if str(qualification_artifact.get("sha256") or "") != artifact_sha256:
        reasons.append("qualification_artifact_hash_mismatch")
    if not bool((qualification.get("replay_artifact_qualification") or {}).get("pass")):
        reasons.append("qualification_replay_artifact_not_qualified")
    if not bool((qualification.get("dut_pass") or {}).get("pass")):
        reasons.append("qualification_DUT_not_passed")

    integrity = qualification.get("input_integrity_review") or {}
    campaign = integrity.get("campaign") or {}
    if str(campaign.get("sha256") or "") != full_campaign_sha256:
        reasons.append("qualification_campaign_hash_mismatch")
    if str(campaign.get("case_name") or "") != str(full.get("name") or ""):
        reasons.append("qualification_case_name_mismatch")
    if integrity.get("status") != "proved" or not isinstance(
        integrity.get("failures"), list
    ):
        reasons.append("qualification_input_integrity_not_proved")
    elif integrity.get("failures"):
        reasons.append("qualification_input_integrity_failures")

    integrity_files = integrity.get("files") or {}
    for key, full_key in (
        ("source", "source"),
        ("binary", "binary"),
        ("trace", "trace"),
        ("program_map", "program_map"),
    ):
        file_review = integrity_files.get(key) or {}
        expected_sha = str((full.get(full_key) or {}).get("sha256") or "")
        reviewed_hashes = {
            str(file_review.get("expected_sha256") or ""),
            str(file_review.get("actual_sha256") or ""),
            str(file_review.get("recorded_sha256") or ""),
        }
        if (
            not bool(file_review.get("pass"))
            or not expected_sha
            or reviewed_hashes != {expected_sha}
        ):
            reasons.append(f"qualification_{key}_hash_mismatch")

    artifact_signature = str(
        ((artifact.get("provenance") or {}).get("compatibility_signature") or "")
    )
    if (
        not artifact_signature
        or str(qualification.get("recorded_compatibility_signature") or "")
        != artifact_signature
    ):
        reasons.append("qualification_compatibility_signature_mismatch")
    if (
        str(
            ((target_audit.get("coverage_contract") or {}).get(
                "compatibility_signature"
            ) or "")
        )
        != artifact_signature
    ):
        reasons.append("target_audit_compatibility_signature_mismatch")
    return reasons


def build_report(
    *,
    full_campaign_path: Path,
    full_name: str,
    reduced_campaign_path: Path,
    reduced_name: str,
    target_audit_path: Path,
) -> dict[str, Any]:
    full_campaign_path = full_campaign_path.resolve()
    reduced_campaign_path = reduced_campaign_path.resolve()
    target_audit_path = target_audit_path.resolve()
    full_campaign = _load_json(full_campaign_path)
    reduced_campaign = _load_json(reduced_campaign_path)
    target_audit = _load_json(target_audit_path)
    full = _program_summary(_case(full_campaign, full_name))
    reduced = _program_summary(_case(reduced_campaign, reduced_name))
    reasons: list[str] = []
    input_binding_reasons: list[str] = []

    artifact_input = (target_audit.get("inputs") or {}).get("artifact") or {}
    artifact_path = Path(str(artifact_input.get("path") or "")).resolve()
    expected_artifact_sha = str(artifact_input.get("expected_sha256") or "")
    actual_artifact_sha = _sha256(artifact_path) if artifact_path.is_file() else "missing"
    if not expected_artifact_sha or actual_artifact_sha != expected_artifact_sha:
        reasons.append("target_artifact_hash_mismatch")
        input_binding_reasons.append("target_artifact_hash_mismatch")
        artifact: dict[str, Any] = {}
    else:
        artifact = _load_json(artifact_path)
        if not artifact:
            reasons.append("target_artifact_payload_empty")
            input_binding_reasons.append("target_artifact_payload_empty")

    qualification_input = (target_audit.get("inputs") or {}).get("qualification") or {}
    qualification_path = Path(str(qualification_input.get("path") or "")).resolve()
    expected_qualification_sha = str(qualification_input.get("sha256") or "")
    actual_qualification_sha = (
        _sha256(qualification_path) if qualification_path.is_file() else "missing"
    )
    qualification_binding_valid = bool(
        expected_qualification_sha
        and actual_qualification_sha == expected_qualification_sha
    )
    if not qualification_binding_valid:
        reasons.append("target_qualification_hash_mismatch")
        input_binding_reasons.append("target_qualification_hash_mismatch")
        qualification: dict[str, Any] = {}
    else:
        qualification = _load_json(qualification_path)
        if not qualification:
            reasons.append("target_qualification_payload_empty")
            input_binding_reasons.append("target_qualification_payload_empty")

    replay_binding_reasons: list[str] = []
    if artifact and qualification:
        replay_binding_reasons = _replay_binding_reasons(
            full=full,
            full_campaign_sha256=_sha256(full_campaign_path),
            artifact=artifact,
            artifact_sha256=actual_artifact_sha,
            qualification=qualification,
            target_audit=target_audit,
        )
        reasons.extend(replay_binding_reasons)
        input_binding_reasons.extend(replay_binding_reasons)

    target_result = target_audit.get("target_result") or {}
    if not bool((target_audit.get("qualification") or {}).get("pass")):
        reasons.append("target_audit_not_qualified")
    if sorted(target_result.get("declared_target_bin_ids") or []) != sorted(TARGET_BIN_IDS):
        reasons.append("declared_target_set_mismatch")
    if sorted(target_result.get("trusted_hit_bin_ids") or []) != sorted(TARGET_BIN_IDS):
        reasons.append("trusted_target_set_mismatch")
    if not bool(target_result.get("all_declared_targets_hit")):
        reasons.append("not_all_declared_targets_hit")

    hits = _target_hits(artifact)
    if set(hits) != set(TARGET_BIN_IDS):
        reasons.append("artifact_target_hit_set_mismatch")
    full_cross_page = int(full["cross_page_instruction_count"])
    hit_counts = {bin_id: int(item["hit_count"]) for bin_id, item in hits.items()}
    if any(count != full_cross_page for count in hit_counts.values()):
        reasons.append("target_hit_counts_do_not_match_full_cross_page_count")

    for side, summary in (("full", full), ("reduced", reduced)):
        if not summary["qualification_pass"] or summary["execution_status"] != "completed":
            reasons.append(f"{side}_offline_case_not_qualified")
    if full["program_seed"] != reduced["program_seed"]:
        reasons.append("program_seed_mismatch")
    if full["timing_seed"] != reduced["timing_seed"]:
        reasons.append("timing_seed_mismatch")
    full_parameters = full["generator_parameters"]
    reduced_parameters = reduced["generator_parameters"]
    for field in COMMON_GENERATOR_FIELDS:
        if full_parameters.get(field) != reduced_parameters.get(field):
            reasons.append(f"generator_parameter_mismatch:{field}")
    if full_parameters.get("page_tail_probe_repetitions") not in (None, 10):
        reasons.append("full_repetition_contract_mismatch")
    if int(reduced_parameters.get("page_tail_probe_repetitions") or -1) != 1:
        reasons.append("reduced_repetition_contract_mismatch")

    compatibility_signature = str(
        ((artifact.get("provenance") or {}).get("compatibility_signature") or "")
    )
    passed = not reasons
    return {
        "schema_version": 1,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "audit_kind": "page_tail_target_hit_multiplicity",
        "policy": (
            "Equal full-run program-event and target-hit counts are selection evidence only. "
            "This audit does not claim exact per-occurrence attribution or reduced-candidate "
            "DUT coverage equivalence."
        ),
        "inputs": {
            "audit_tool": {
                "path": str(Path(__file__).resolve()),
                "sha256": _sha256(Path(__file__).resolve()),
            },
            "program_map_tool": {
                "path": str(Path(analyze_trace.__code__.co_filename).resolve()),
                "sha256": _sha256(
                    Path(analyze_trace.__code__.co_filename).resolve()
                ),
            },
            "full_campaign": {
                "path": str(full_campaign_path),
                "sha256": _sha256(full_campaign_path),
            },
            "reduced_campaign": {
                "path": str(reduced_campaign_path),
                "sha256": _sha256(reduced_campaign_path),
            },
            "target_audit": {
                "path": str(target_audit_path),
                "sha256": _sha256(target_audit_path),
            },
            "replay_qualification": {
                "path": str(qualification_path),
                "recorded_sha256": expected_qualification_sha,
                "actual_sha256": actual_qualification_sha,
                "pass": qualification_binding_valid,
            },
            "functional_coverage_artifact": {
                "path": str(artifact_path),
                "recorded_sha256": expected_artifact_sha,
                "actual_sha256": actual_artifact_sha,
                "pass": actual_artifact_sha == expected_artifact_sha,
            },
        },
        "compatibility_signature": compatibility_signature,
        "full_DUT_input_binding": {
            "pass": not input_binding_reasons,
            "reasons": input_binding_reasons,
            "policy": (
                "The qualified DUT artifact must bind to the exact full-repeat "
                "source, binary, trace, program map, program seed, timing seed, "
                "campaign, and compatibility signature counted by this audit."
            ),
        },
        "full_repeat": full,
        "reduced_candidate": reduced,
        "target_hit_multiplicity": hits,
        "correlation": {
            "full_cross_page_instruction_count": full_cross_page,
            "target_hit_counts": hit_counts,
            "all_target_hit_counts_equal_full_cross_page_count": bool(hit_counts)
            and all(count == full_cross_page for count in hit_counts.values()),
            "reduced_cross_page_instruction_count": int(
                reduced["cross_page_instruction_count"]
            ),
            "exact_occurrence_attribution_claimed": False,
            "coverage_equivalence_claimed": False,
        },
        "qualification": {
            "pass": passed,
            "reasons": reasons,
            "status": "supports_first_serialized_reduced_replay" if passed else "not_qualified",
            "reduced_DUT_status": "not_run",
            "reduced_coverage_hit": False,
        },
    }


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--full-campaign", type=Path, required=True)
    parser.add_argument("--full-name", required=True)
    parser.add_argument("--reduced-campaign", type=Path, required=True)
    parser.add_argument("--reduced-name", required=True)
    parser.add_argument("--target-audit", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    return parser.parse_args()


def main() -> int:
    args = _parse_args()
    if args.output.exists():
        raise SystemExit(f"refusing to overwrite output: {args.output}")
    report = build_report(
        full_campaign_path=args.full_campaign,
        full_name=args.full_name,
        reduced_campaign_path=args.reduced_campaign,
        reduced_name=args.reduced_name,
        target_audit_path=args.target_audit,
    )
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("x", encoding="utf-8") as stream:
        json.dump(report, stream, indent=2, sort_keys=True)
        stream.write("\n")
    print(
        f"page-tail-hit-multiplicity status={report['qualification']['status']} "
        f"output={args.output.resolve()}"
    )
    return 0 if report["qualification"]["pass"] else 1


if __name__ == "__main__":
    raise SystemExit(main())

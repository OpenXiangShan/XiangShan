#!/usr/bin/env python3
"""Audit seeded taken-C.J offset sequences without granting DUT or HIT status."""
from __future__ import annotations

import argparse
import hashlib
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


def _load(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"expected JSON object: {path}")
    return payload


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _case_by_ordinal(campaign: dict[str, Any]) -> dict[int, dict[str, Any]]:
    result: dict[int, dict[str, Any]] = {}
    for case in campaign.get("cases") or []:
        ordinal = int((case.get("spec") or {})["ordinal"])
        if ordinal in result:
            raise ValueError(f"duplicate campaign ordinal: {ordinal}")
        result[ordinal] = case
    return result


def _event_count(analysis: dict[str, Any], event_name: str) -> int:
    matches = [
        int(event["count"])
        for event in analysis.get("events") or []
        if event.get("event") == event_name
    ]
    return matches[0] if len(matches) == 1 else -1


def _declared_sequence(generator: dict[str, Any]) -> list[int]:
    ring = [
        episode
        for episode in generator.get("episodes") or []
        if episode.get("kind") == "taken_cj_offset_ring"
    ]
    if len(ring) != 16:
        return []
    try:
        ordered = sorted(
            ring,
            key=lambda episode: int(episode["parameters"]["execution_position"]),
        )
        return [int(episode["parameters"]["halfword_offset"]) for episode in ordered]
    except (KeyError, TypeError, ValueError):
        return []


def build_report(candidate_path: Path, control_path: Path | None = None) -> dict[str, Any]:
    candidate = _load(candidate_path)
    control = _load(control_path) if control_path is not None else {}
    candidate_cases = _case_by_ordinal(candidate)
    control_cases = _case_by_ordinal(control) if control_path is not None else {}
    failures: list[str] = []
    records: list[dict[str, Any]] = []
    sequences: list[tuple[int, ...]] = []
    if len(candidate_cases) < 2:
        failures.append("candidate_batch_requires_at_least_two_cases")
    if control_path is not None and set(candidate_cases) != set(control_cases):
        failures.append("candidate_control_ordinal_set_mismatch")

    for ordinal, case in sorted(candidate_cases.items()):
        case_failures: list[str] = []
        spec = case.get("spec") or {}
        if spec.get("directed_pattern") != "taken_cj_offset_ring_permuted":
            case_failures.append("candidate_pattern_mismatch")
        if not (
            case.get("execution_status") == "completed"
            and int(case.get("exit_code", -1)) == 0
            and case.get("qualification_pass") is True
            and case.get("program_map_status") == "valid"
            and case.get("scenario_realization_status") == "REALIZED"
        ):
            case_failures.append("candidate_offline_qualification_failed")

        input_specs = {
            label: (Path(str(case.get(f"{label}_path") or "")), str(case.get(f"{label}_sha256") or ""))
            for label in ("source", "bin", "trace", "program_map", "scenario_spec")
        }
        input_records: dict[str, dict[str, Any]] = {}
        for label, (path, expected_hash) in input_specs.items():
            exists = path.is_file()
            actual_hash = _sha256(path) if exists else ""
            passed = bool(exists and expected_hash and actual_hash == expected_hash)
            if not passed:
                case_failures.append(f"{label}_hash_mismatch")
            input_records[label] = {
                "path": str(path.resolve()) if exists else str(path),
                "expected_sha256": expected_hash,
                "actual_sha256": actual_hash,
                "pass": passed,
            }

        analysis_path = Path(str(case.get("trace_analysis_path") or ""))
        generator_path = Path(str(case.get("generator_manifest_path") or ""))
        configuration_path = Path(str(case.get("configuration_preflight_path") or ""))
        realization_path = Path(str(case.get("scenario_realization_path") or ""))
        auxiliary = {
            label: {"path": str(path.resolve()), "sha256": _sha256(path)}
            for label, path in {
                "trace_analysis": analysis_path,
                "generator_manifest": generator_path,
                "configuration_preflight": configuration_path,
                "scenario_realization": realization_path,
            }.items()
            if path.is_file()
        }
        if len(auxiliary) != 4:
            case_failures.append("auxiliary_evidence_missing")
            analysis = {}
            generator = {}
            configuration = {}
            realization = {}
        else:
            analysis = _load(analysis_path)
            generator = _load(generator_path)
            configuration = _load(configuration_path)
            realization = _load(realization_path)

        actual_sequence = list(
            (analysis.get("program_diversity") or {}).get("taken_cj_offset_ring_sequence") or []
        )
        actual_offsets = list(
            (analysis.get("program_diversity") or {}).get("taken_cj_offset_ring_offsets") or []
        )
        declared_sequence = _declared_sequence(generator)
        sequence_valid = bool(
            len(actual_sequence) == 16
            and sorted(actual_sequence) == list(range(16))
            and actual_offsets == list(range(16))
            and actual_sequence == declared_sequence
            and _event_count(analysis, "trace.taken_cj_offset_ring") == 16
            and _event_count(analysis, "trace.taken_cj_offset_transition") == 15
        )
        if not sequence_valid:
            case_failures.append("executed_offset_sequence_invalid")
        if sequence_valid:
            sequences.append(tuple(actual_sequence))

        binary_sha = str(case.get("bin_sha256") or "")
        if not (
            analysis.get("trace_sha256") == case.get("trace_sha256")
            and analysis.get("program_map_binary_sha256") == binary_sha
        ):
            case_failures.append("trace_analysis_hash_binding_invalid")
        if not (
            configuration.get("status") == "valid"
            and not configuration.get("reasons")
            and configuration.get("binary_sha256") == binary_sha
        ):
            case_failures.append("translation_configuration_invalid")
        if not (
            realization.get("status") == "REALIZED"
            and not realization.get("missing_events")
            and realization.get("scenario_id") == case.get("name")
        ):
            case_failures.append("scenario_realization_invalid")

        control_record: dict[str, Any] | None = None
        if ordinal in control_cases:
            control_case = control_cases[ordinal]
            control_spec = control_case.get("spec") or {}
            seed_match = (
                int(control_spec.get("seed", -1)) == int(spec.get("seed", -2))
                and int(control_case.get("timing_seed", -1)) == int(case.get("timing_seed", -2))
            )
            control_valid = bool(
                control_case.get("execution_status") == "completed"
                and int(control_case.get("exit_code", -1)) == 0
                and control_case.get("qualification_pass") is True
                and seed_match
            )
            if not control_valid:
                case_failures.append("same_seed_control_invalid")
            control_record = {
                "name": control_case.get("name"),
                "program_and_timing_seed_match": seed_match,
                "offline_qualified": control_valid,
            }

        failures.extend(f"ordinal_{ordinal}:{reason}" for reason in case_failures)
        records.append(
            {
                "ordinal": ordinal,
                "name": case.get("name"),
                "program_seed": spec.get("seed"),
                "timing_seed": case.get("timing_seed"),
                "input_integrity": input_records,
                "auxiliary_evidence": auxiliary,
                "declared_offset_sequence": declared_sequence,
                "executed_offset_sequence": actual_sequence,
                "distinct_offsets": actual_offsets,
                "distinct_transition_count": _event_count(
                    analysis, "trace.taken_cj_offset_transition"
                ),
                "same_seed_control": control_record,
                "pass": not case_failures,
                "reasons": case_failures,
            }
        )

    unique_sequence_count = len(set(sequences))
    if unique_sequence_count != len(candidate_cases):
        failures.append(
            f"executed_sequences_not_unique:{unique_sequence_count}/{len(candidate_cases)}"
        )
    return {
        "schema_version": 1,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "policy": (
            "This is NEMU/program/configuration evidence only. It proves neither DUT PASS "
            "nor functional-coverage HIT and does not promote or replace a corpus slot."
        ),
        "inputs": {
            "candidate_campaign": {
                "path": str(candidate_path.resolve()),
                "sha256": _sha256(candidate_path),
            },
            "control_campaign": (
                None
                if control_path is None
                else {"path": str(control_path.resolve()), "sha256": _sha256(control_path)}
            ),
        },
        "case_count": len(candidate_cases),
        "unique_executed_sequence_count": unique_sequence_count,
        "records": records,
        "qualification": {
            "pass": not failures,
            "status": "qualified_for_serialized_DUT_replay" if not failures else "not_qualified",
            "reasons": failures,
            "DUT_status": "not_run",
            "coverage_hit": False,
            "corpus_replacement": False,
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
    parser.add_argument("--candidate-campaign", type=Path, required=True)
    parser.add_argument("--control-campaign", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    report = build_report(
        args.candidate_campaign.resolve(),
        args.control_campaign.resolve() if args.control_campaign else None,
    )
    write_json_exclusive(args.output, report)
    print(
        f"qualified={int(report['qualification']['pass'])} "
        f"sequences={report['unique_executed_sequence_count']}/{report['case_count']} "
        f"DUT_status={report['qualification']['DUT_status']} "
        f"coverage_hit={int(report['qualification']['coverage_hit'])} "
        f"output={args.output.resolve()}"
    )
    return 0 if report["qualification"]["pass"] else 1


if __name__ == "__main__":
    raise SystemExit(main())

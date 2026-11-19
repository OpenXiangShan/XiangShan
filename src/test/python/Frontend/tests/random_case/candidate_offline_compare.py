#!/usr/bin/env python3
"""Immutable comparison of an original random case and a candidate.

Compare retained program traces and optionally summarize historical replay
sidecars. Report status describes this offline check only.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import subprocess
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

try:
    from .program_map import analyze_trace
except ImportError:
    from program_map import analyze_trace


INPUT_FIELDS = {
    "source": ("source_path", "source_sha256"),
    "binary": ("bin_path", "bin_sha256"),
    "trace": ("trace_path", "trace_sha256"),
    "program_map": ("program_map_path", "program_map_sha256"),
    "scenario_spec": ("scenario_spec_path", "scenario_spec_sha256"),
}


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _load_json(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"expected JSON object: {path}")
    return payload


def _historical_funcov_observation(
    qualification: dict[str, Any],
) -> tuple[dict[str, Any], set[str], list[str]]:
    """Load only the funcov artifact cryptographically bound by an audit sidecar."""
    failures: list[str] = []
    artifact_input = (qualification.get("inputs") or {}).get("artifact") or {}
    raw_path = str(artifact_input.get("path") or "")
    expected_sha256 = str(artifact_input.get("sha256") or "")
    path = Path(raw_path).resolve() if raw_path else Path("/nonexistent")
    actual_sha256 = _sha256(path) if path.is_file() else "missing"
    if not expected_sha256 or actual_sha256 != expected_sha256:
        failures.append("qualification_artifact_hash_mismatch")
        return {
            "path": raw_path or None,
            "recorded_sha256": expected_sha256 or None,
            "actual_sha256": actual_sha256,
            "pass": False,
        }, set(), failures

    artifact = _load_json(path)
    provenance = artifact.get("provenance") or {}
    qualification_signature = str(
        qualification.get("recorded_compatibility_signature") or ""
    )
    artifact_signature = str(provenance.get("compatibility_signature") or "")
    if not qualification_signature or artifact_signature != qualification_signature:
        failures.append("qualification_artifact_compatibility_signature_mismatch")
    definitions = artifact.get("definitions") or []
    definition_bin_ids = sorted(
        {
            str(item.get("bin_id") or "")
            for item in definitions
            if isinstance(item, dict) and str(item.get("bin_id") or "")
        }
    )
    raw_hit_bin_ids = {
        str(item.get("bin_id") or "")
        for item in (artifact.get("hits") or {}).values()
        if isinstance(item, dict)
        and int(item.get("hits") or 0) > 0
        and str(item.get("bin_id") or "")
    }
    targets = artifact.get("coverage_targets") or {}
    summary = {
        "path": str(path),
        "recorded_sha256": expected_sha256,
        "actual_sha256": actual_sha256,
        "pass": not failures,
        "compatibility_signature": artifact_signature,
        "definitions_sha256": str(provenance.get("definitions_sha256") or ""),
        "definition_bin_ids": definition_bin_ids,
        "raw_observed_hit_bin_count": len(raw_hit_bin_ids),
        "declared_target_bin_ids": sorted(
            str(value) for value in (targets.get("bin_ids") or [])
        ),
    }
    return summary, raw_hit_bin_ids, failures


def _case(campaign: dict[str, Any], name: str) -> dict[str, Any]:
    matches = [record for record in campaign.get("cases") or [] if record.get("name") == name]
    if len(matches) != 1:
        raise ValueError(f"expected exactly one case named {name!r}, found {len(matches)}")
    return matches[0]


def _verified_inputs(record: dict[str, Any]) -> tuple[dict[str, Any], list[str]]:
    files: dict[str, Any] = {}
    failures: list[str] = []
    for label, (path_key, hash_key) in INPUT_FIELDS.items():
        path = Path(str(record.get(path_key) or "")).resolve()
        expected = str(record.get(hash_key) or "")
        if not path.is_file():
            actual = "missing"
            passed = False
        else:
            actual = _sha256(path)
            passed = bool(expected) and actual == expected
        files[label] = {
            "path": str(path),
            "recorded_sha256": expected,
            "actual_sha256": actual,
            "pass": passed,
        }
        if not passed:
            failures.append(f"{label}_hash_mismatch")
    return files, failures


def _event_counts(analysis: dict[str, Any]) -> dict[str, int]:
    return {str(item["event"]): int(item["count"]) for item in analysis["events"]}


def _delta(original: int | float, candidate: int | float) -> dict[str, int | float | None]:
    return {
        "original": original,
        "candidate": candidate,
        "delta": candidate - original,
        "delta_percent": ((candidate - original) * 100.0 / original) if original else None,
    }


def _comparison_metrics(
    original_map: dict[str, Any],
    candidate_map: dict[str, Any],
    original_analysis: dict[str, Any],
    candidate_analysis: dict[str, Any],
) -> dict[str, Any]:
    original_events = _event_counts(original_analysis)
    candidate_events = _event_counts(candidate_analysis)
    original_static = int(original_map["summary"]["static_instruction_count"])
    candidate_static = int(candidate_map["summary"]["static_instruction_count"])
    original_dynamic = original_events.get("trace.committed_instruction", 0)
    candidate_dynamic = candidate_events.get("trace.committed_instruction", 0)
    original_unique = original_events["trace.unique_pc"]
    candidate_unique = candidate_events["trace.unique_pc"]
    if original_static <= 0 or candidate_static <= 0:
        raise ValueError("comparison requires nonempty ProgramMaps")
    added_static = candidate_static - original_static
    added_dynamic = candidate_dynamic - original_dynamic
    added_unique = candidate_unique - original_unique
    event_names = sorted(set(original_events) | set(candidate_events))
    original_diversity = original_analysis["program_diversity"]
    candidate_diversity = candidate_analysis["program_diversity"]
    return {
        "program_map_static_instruction_count": _delta(original_static, candidate_static),
        "text_size_bytes": _delta(
            int(original_map["text_size_bytes"]), int(candidate_map["text_size_bytes"])
        ),
        "dynamic_instruction_count": _delta(original_dynamic, candidate_dynamic),
        "unique_executed_pc_count": _delta(original_unique, candidate_unique),
        "unique_control_flow_edge_count": _delta(
            int(original_diversity["unique_control_flow_edges"]),
            int(candidate_diversity["unique_control_flow_edges"]),
        ),
        "dynamic_control_flow_edge_count": _delta(
            int(original_diversity["dynamic_control_flow_edges"]),
            int(candidate_diversity["dynamic_control_flow_edges"]),
        ),
        "instruction_start_boundary_transitions": {
            label: _delta(
                int(original_diversity["instruction_start_boundary_transitions"][label]),
                int(candidate_diversity["instruction_start_boundary_transitions"][label]),
            )
            for label in ("fetch_block", "cacheline", "page")
        },
        "dynamic_event_counts": {
            name: _delta(original_events.get(name, 0), candidate_events.get(name, 0))
            for name in event_names
        },
        "execution_efficiency": {
            "unique_pc_per_static_instruction": _delta(
                original_unique / original_static, candidate_unique / candidate_static
            ),
            "dynamic_instruction_per_static_instruction": _delta(
                original_dynamic / original_static, candidate_dynamic / candidate_static
            ),
            "static_instructions_not_observed_as_unique_pcs": _delta(
                original_static - original_unique, candidate_static - candidate_unique
            ),
            "incremental_unique_pc_per_added_static_instruction": (
                (candidate_unique - original_unique) / added_static if added_static > 0 else None
            ),
            "incremental_dynamic_instruction_per_added_static_instruction": (
                (candidate_dynamic - original_dynamic) / added_static if added_static > 0 else None
            ),
            "interpretation": (
                "ProgramMap growth is not itself useful stress; promotion requires the separately "
                "observed dynamic events and a later compatible serialized DUT comparison."
            ),
        },
        "dynamic_pc_repetition": {
            "repeated_dynamic_instruction_count": _delta(
                original_dynamic - original_unique,
                candidate_dynamic - candidate_unique,
            ),
            "repeated_dynamic_instruction_fraction": _delta(
                (original_dynamic - original_unique) / original_dynamic if original_dynamic else 0.0,
                (candidate_dynamic - candidate_unique) / candidate_dynamic if candidate_dynamic else 0.0,
            ),
            "unique_pc_fraction": _delta(
                original_unique / original_dynamic if original_dynamic else 0.0,
                candidate_unique / candidate_dynamic if candidate_dynamic else 0.0,
            ),
            "incremental_unique_pc_fraction": (
                added_unique / added_dynamic if added_dynamic > 0 else None
            ),
            "incremental_repeated_dynamic_instruction_count": (
                added_dynamic - added_unique if added_dynamic > 0 else None
            ),
            "scope": (
                "This is a committed-PC repetition proxy, not proof that repeated executions are "
                "coverage-redundant; intentional target-pressure repetitions remain visible."
            ),
        },
    }


def _git_head(repo: Path) -> str | None:
    if not (repo / ".git").exists():
        return None
    try:
        result = subprocess.run(
            ["git", "-C", str(repo), "rev-parse", "--verify", "HEAD"],
            check=False,
            capture_output=True,
            text=True,
        )
    except FileNotFoundError:
        return None
    return result.stdout.strip() if result.returncode == 0 else None


def build_report(
    *,
    original_campaign_path: Path,
    candidate_campaign_path: Path,
    original_name: str,
    candidate_name: str,
    seed0_qualification_path: Path | None = None,
    candidate_qualification_path: Path | None = None,
) -> dict[str, Any]:
    original_campaign = _load_json(original_campaign_path)
    candidate_campaign = _load_json(candidate_campaign_path)
    original = _case(original_campaign, original_name)
    candidate = _case(candidate_campaign, candidate_name)
    seed0_qualification = (
        _load_json(seed0_qualification_path) if seed0_qualification_path is not None else {}
    )
    candidate_qualification = (
        _load_json(candidate_qualification_path)
        if candidate_qualification_path is not None
        else {}
    )
    if seed0_qualification_path is not None:
        qualified_case = str(seed0_qualification.get("scenario_id") or "")
        if qualified_case != original_name:
            raise ValueError(
                "original qualification sidecar case mismatch: "
                f"{qualified_case!r} != {original_name!r}"
            )
    if candidate_qualification_path is not None:
        qualified_case = str(candidate_qualification.get("scenario_id") or "")
        if qualified_case != candidate_name:
            raise ValueError(
                "candidate qualification sidecar case mismatch: "
                f"{qualified_case!r} != {candidate_name!r}"
            )

    original_inputs, original_failures = _verified_inputs(original)
    candidate_inputs, candidate_failures = _verified_inputs(candidate)
    if original_failures or candidate_failures:
        raise ValueError(
            "offline input integrity failed: "
            + ", ".join([f"original_{reason}" for reason in original_failures]
                        + [f"candidate_{reason}" for reason in candidate_failures])
        )
    original_map = _load_json(Path(original_inputs["program_map"]["path"]))
    candidate_map = _load_json(Path(candidate_inputs["program_map"]["path"]))
    original_analysis = analyze_trace(
        Path(original_inputs["trace"]["path"]), original_map
    )
    candidate_analysis = analyze_trace(
        Path(candidate_inputs["trace"]["path"]), candidate_map
    )
    original_spec = _load_json(Path(original_inputs["scenario_spec"]["path"]))
    candidate_spec = _load_json(Path(candidate_inputs["scenario_spec"]["path"]))
    candidate_realization_path = Path(str(candidate["scenario_realization_path"])).resolve()
    realization_sha = _sha256(candidate_realization_path)
    if candidate.get("scenario_realization_sha256") != realization_sha:
        raise ValueError("candidate scenario realization hash mismatch")
    candidate_realization = _load_json(candidate_realization_path)
    candidate_family = str(candidate_spec.get("family") or "")
    configuration_required = candidate_family == "translation_timing_cross"
    configuration_path: Path | None = None
    configuration: dict[str, Any] = {}
    if configuration_required:
        raw_configuration_path = str(candidate.get("configuration_preflight_path") or "")
        possible_configuration_path = Path(raw_configuration_path).resolve()
        if raw_configuration_path and possible_configuration_path.is_file():
            configuration_path = possible_configuration_path
            configuration = _load_json(configuration_path)

    for spec in (original_spec, candidate_spec):
        for key in ("program_seed", "timing_seed"):
            if type(spec.get(key)) is not int or not 0 <= spec[key] < 2**64:
                raise ValueError(f"invalid scenario {key}")
    program_seed_equal = original_spec["program_seed"] == candidate_spec["program_seed"]
    timing_seed_equal = original_spec["timing_seed"] == candidate_spec["timing_seed"]
    common_parameters = ("family", "blocks", "iterations", "max_call_depth", "ordinal", "seed")
    original_parameters = dict(original_spec["parameters"]["generator_parameters"])
    candidate_parameters = dict(candidate_spec["parameters"]["generator_parameters"])
    common_parameter_equality = {
        key: (
            key in original_parameters and key in candidate_parameters
            and type(original_parameters[key]) is type(candidate_parameters[key])
            and original_parameters[key] == candidate_parameters[key]
        )
        for key in common_parameters
    }

    candidate_pattern = candidate_parameters.get("directed_pattern", "none")
    serialized_pattern_reuse = candidate_spec["parameters"].get("pattern_reuse")
    pattern_reuse_warning = bool(candidate_pattern == "page_tail_pair" and not serialized_pattern_reuse)

    original_nemu_pass = (
        original.get("execution_status") == "completed" and type(original.get("exit_code")) is int and original["exit_code"] == 0
    )
    candidate_nemu_pass = (
        candidate.get("execution_status") == "completed" and type(candidate.get("exit_code")) is int and candidate["exit_code"] == 0
    )
    def valid_map(record, inputs, program_map, analysis):
        return (
            record.get("program_map_status") == "valid"
            and program_map.get("binary_text_verified") is True
            and program_map.get("binary_sha256") == inputs["binary"]["actual_sha256"]
            and program_map.get("text_sha256") == inputs["binary"]["actual_sha256"]
            and analysis["missing_program_map_pc_count"] == 0
            and _event_counts(analysis).get("trace.committed_instruction", 0) > 0
        )

    original_program_map_valid = valid_map(original, original_inputs, original_map, original_analysis)
    candidate_program_map_valid = valid_map(candidate, candidate_inputs, candidate_map, candidate_analysis)
    candidate_binary_hash = candidate_inputs["binary"]["actual_sha256"]
    candidate_map_hash_bindings_valid = (
        candidate_map.get("binary_sha256") == candidate_binary_hash
        and candidate_map.get("text_sha256") == candidate_binary_hash
    )
    candidate_configuration_valid = (
        not configuration_required or configuration.get("status") == "valid"
    )
    candidate_configuration_hash_binding_valid = (
        not configuration_required
        or (
            str(configuration.get("binary_sha256") or "") == candidate_binary_hash
            and candidate.get("configuration_preflight") == configuration
        )
    )
    candidate_realization_identity_valid = (
        str(candidate_realization.get("scenario_id") or "") == candidate_name
    )
    candidate_realized = (
        candidate.get("scenario_realization_status") == "REALIZED"
        and candidate_realization.get("status") == "REALIZED"
        and candidate_realization.get("missing_events") == []
    )
    original_reported_replay_qualification = (
        seed0_qualification.get("replay_artifact_qualification", {}).get("pass") is True
    )
    original_dut_pass = seed0_qualification.get("dut_pass", {}).get("pass") is True
    candidate_reported_replay_qualification = (
        candidate_qualification.get("replay_artifact_qualification", {}).get("pass") is True
    )
    candidate_dut_pass = candidate_qualification.get("dut_pass", {}).get("pass") is True
    original_observation: dict[str, Any] | None = None
    candidate_observation: dict[str, Any] | None = None
    original_raw_hits: set[str] = set()
    candidate_raw_hits: set[str] = set()
    observation_failures: list[str] = []
    compatible_dut_comparison = False
    if candidate_qualification_path is not None:
        if seed0_qualification_path is None:
            observation_failures.append("original_qualification_required_for_candidate_comparison")
        else:
            (
                original_observation,
                original_raw_hits,
                original_observation_failures,
            ) = _historical_funcov_observation(seed0_qualification)
            (
                candidate_observation,
                candidate_raw_hits,
                candidate_observation_failures,
            ) = _historical_funcov_observation(candidate_qualification)
            observation_failures.extend(
                f"original_{reason}" for reason in original_observation_failures
            )
            observation_failures.extend(
                f"candidate_{reason}" for reason in candidate_observation_failures
            )
            signatures_match = bool(
                original_observation.get("compatibility_signature")
                and original_observation.get("compatibility_signature")
                == candidate_observation.get("compatibility_signature")
            )
            definitions_match = bool(
                original_observation.get("definitions_sha256")
                and original_observation.get("definitions_sha256")
                == candidate_observation.get("definitions_sha256")
                and original_observation.get("definition_bin_ids")
                == candidate_observation.get("definition_bin_ids")
            )
            if not signatures_match:
                observation_failures.append("DUT_compatibility_signature_mismatch")
            if not definitions_match:
                observation_failures.append("functional_coverage_definitions_mismatch")
            compatible_dut_comparison = signatures_match and definitions_match

    reasons = []
    if not program_seed_equal:
        reasons.append("program_seed_mismatch")
    if not timing_seed_equal:
        reasons.append("timing_seed_mismatch")
    reasons.extend(
        f"generator_parameter_mismatch:{key}"
        for key, equal in common_parameter_equality.items()
        if not equal
    )
    for passed, reason in (
        (original_nemu_pass, "original_nemu_not_passed"),
        (candidate_nemu_pass, "candidate_nemu_not_passed"),
        (original_program_map_valid, "original_program_map_invalid"),
        (candidate_program_map_valid, "candidate_program_map_invalid"),
        (candidate_map_hash_bindings_valid, "candidate_program_map_binary_binding_invalid"),
        (candidate_configuration_valid, "candidate_configuration_invalid"),
        (
            candidate_configuration_hash_binding_valid,
            "candidate_configuration_binary_binding_invalid",
        ),
        (candidate_realized, "candidate_program_scenario_not_realized"),
        (candidate_realization_identity_valid, "candidate_realization_identity_invalid"),
    ):
        if not passed:
            reasons.append(reason)
    if candidate_qualification_path is not None:
        for passed, reason in (
            (original_reported_replay_qualification, "original_sidecar_did_not_report_qualification"),
            (original_dut_pass, "original_sidecar_did_not_report_DUT_pass"),
            (candidate_reported_replay_qualification, "candidate_sidecar_did_not_report_qualification"),
            (candidate_dut_pass, "candidate_sidecar_did_not_report_DUT_pass"),
        ):
            if not passed:
                observation_failures.append(reason)
    comparable_observations = compatible_dut_comparison and not observation_failures

    repo = Path(__file__).resolve().parents[6]
    metrics = _comparison_metrics(
        original_map, candidate_map, original_analysis, candidate_analysis
    )
    translation = candidate_spec.get("parameters", {}).get("translation")
    pma_pattern = (
        translation.get("attribute_plan", {}).get("pma_pattern")
        if isinstance(translation, dict)
        else None
    )
    seed0_hit = seed0_qualification.get("coverage_hit_eligibility") or {}
    candidate_hit = candidate_qualification.get("coverage_hit_eligibility") or {}
    diagnostic_raw_observation = (
        {
            "available": True,
            "policy": (
                "Raw observations are compared using matching historical artifact metadata."
                " Sidecar flags describe what was recorded historically."
            ),
            "same_compatibility_signature": compatible_dut_comparison,
            "compatibility_signature": (
                original_observation.get("compatibility_signature")
                if original_observation is not None
                else None
            ),
            "definitions_sha256": (
                original_observation.get("definitions_sha256")
                if original_observation is not None
                else None
            ),
            "original_raw_hit_bin_count": len(original_raw_hits),
            "candidate_raw_hit_bin_count": len(candidate_raw_hits),
            "raw_hit_bin_count_delta": len(candidate_raw_hits) - len(original_raw_hits),
            "candidate_only_raw_bin_ids": sorted(candidate_raw_hits - original_raw_hits),
            "original_only_raw_bin_ids": sorted(original_raw_hits - candidate_raw_hits),
            "candidate_is_raw_superset": original_raw_hits <= candidate_raw_hits,
            "trusted_coverage_hit_claimed": False,
        }
        if comparable_observations
        else {
            "available": False,
            "reasons": observation_failures or ["paired_historical_sidecars_not_supplied"],
            "trusted_coverage_hit_claimed": False,
        }
    )
    return {
        "schema_version": 2,
        "evidence_scope": "historical_diagnostic_only",
        "canonical_hit_eligible": False,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "comparison_kind": "offline_original_vs_candidate",
        "policy": (
            "Historical diagnostic comparison of campaign records and recorded traces."
        ),
        "original": {
            "name": original_name,
            "inputs": original_inputs,
            "recorded_NEMU_status": "pass" if original_nemu_pass else "not_pass",
            "historical_replay_sidecar": {
                "path": str(seed0_qualification_path.resolve()) if seed0_qualification_path else None,
                "sha256": _sha256(seed0_qualification_path) if seed0_qualification_path else None,
                "reported_replay_qualification": original_reported_replay_qualification,
                "reported_DUT_pass": original_dut_pass,
            },
            "DUT_status": "unverified",
            "coverage_hit": False,
            "coverage_note": {
                "reason": seed0_hit.get("reason"),
                "raw_observed_hit_count": seed0_hit.get("raw_observed_hit_count"),
                "historically_reported_eligible": seed0_hit.get("eligible"),
            },
        },
        "candidate": {
            "name": candidate_name,
            "inputs": candidate_inputs,
            "recorded_NEMU_status": "pass" if candidate_nemu_pass else "not_pass",
            "configuration_preflight": (
                {
                    "applicable": True,
                    "path": str(configuration_path) if configuration_path is not None else None,
                    "sha256": _sha256(configuration_path) if configuration_path is not None else None,
                    "status": configuration.get("status", "missing"),
                    **{key: value for key, value in configuration.items()
                       if key not in {"applicable", "path", "sha256", "status"}},
                }
                if configuration_required
                else {
                    "applicable": False,
                    "path": None,
                    "sha256": None,
                    "status": "not_applicable",
                    "reason": "bare_program_family",
                }
            ),
            "program_map_status": "valid" if candidate_program_map_valid else "invalid",
            "program_map_binary_binding": {
                "binary_sha256": candidate_map.get("binary_sha256"),
                "text_sha256": candidate_map.get("text_sha256"),
                "actual_binary_sha256": candidate_binary_hash,
                "pass": candidate_map_hash_bindings_valid,
            },
            "scenario_realization": {
                "path": str(candidate_realization_path),
                "sha256": _sha256(candidate_realization_path),
                "status": candidate_realization.get("status"),
                "scenario_id": candidate_realization.get("scenario_id"),
                "identity_pass": candidate_realization_identity_valid,
                "missing_events": candidate_realization.get("missing_events"),
            },
            "translation_attributes": (
                {
                    "applicable": True,
                    "pma_pattern": pma_pattern,
                    "page_count": configuration.get("page_count"),
                    "pmp_entry_count": configuration.get("pmp_entry_count"),
                    "pma_entry_count": configuration.get("pma_entry_count"),
                }
                if configuration_required
                else {"applicable": False, "reason": "bare_program_family"}
            ),
            "historical_replay_sidecar": {
                "path": str(candidate_qualification_path.resolve()) if candidate_qualification_path else None,
                "sha256": _sha256(candidate_qualification_path) if candidate_qualification_path else None,
                "reported_replay_qualification": candidate_reported_replay_qualification,
                "reported_DUT_pass": candidate_dut_pass,
            },
            "DUT_status": "unverified",
            "coverage_hit": False,
            "coverage_note": (
                {
                    "reason": candidate_hit.get("reason"),
                    "raw_observed_hit_count": candidate_hit.get("raw_observed_hit_count"),
                    "historically_reported_eligible": candidate_hit.get("eligible"),
                }
                if candidate_qualification_path is not None
                else "no_historical_sidecar_supplied"
            ),
            "pattern_reuse_provenance": {
                "directed_pattern": candidate_pattern,
                "serialized_pattern_reuse_present": serialized_pattern_reuse is not None,
                "retained_scenario_spec_is_unchanged": True,
                "warning": (
                    "retained scenario_spec omits pattern_reuse; source provenance is incomplete."
                    if pattern_reuse_warning
                    else None
                ),
            },
        },
        "seed_and_parameter_consistency": {
            "program_seed": _delta(int(original_spec["program_seed"]), int(candidate_spec["program_seed"])),
            "program_seed_equal": program_seed_equal,
            "timing_seed": _delta(int(original_spec["timing_seed"]), int(candidate_spec["timing_seed"])),
            "timing_seed_equal": timing_seed_equal,
            "common_generator_parameters_equal": common_parameter_equality,
            "candidate_only_parameters": {
                key: value for key, value in candidate_parameters.items() if key not in original_parameters
            },
        },
        "offline_metrics": metrics,
        "diagnostic_raw_funcov_comparison": diagnostic_raw_observation,
        "qualification": {"pass": False, "status": "historical_diagnostic_only", "coverage_hit": False},
        "offline_check": {
            "pass": not reasons,
            "status": "comparable" if not reasons else "not_comparable",
            "reasons": reasons,
            "warnings": (
                ["candidate_scenario_spec_missing_pattern_reuse_annotation"]
                if pattern_reuse_warning
                else []
            ),
        },
        "provenance": {
            "original_campaign": {
                "path": str(original_campaign_path.resolve()),
                "sha256": _sha256(original_campaign_path),
            },
            "candidate_campaign": {
                "path": str(candidate_campaign_path.resolve()),
                "sha256": _sha256(candidate_campaign_path),
            },
            "comparison_tool": {
                "path": str(Path(__file__).resolve()),
                "sha256": _sha256(Path(__file__).resolve()),
            },
            "trace_analyzer": {
                "path": str(Path(analyze_trace.__code__.co_filename).resolve()),
                "sha256": _sha256(Path(analyze_trace.__code__.co_filename).resolve()),
                "analysis_mode": "both traces recomputed in memory with the same current analyzer",
            },
            "repo_head": _git_head(repo),
            "source_artifacts_rewritten": False,
        },
    }


def write_json_exclusive(path: Path, payload: dict[str, Any]) -> None:
    path = path.resolve()
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("x", encoding="utf-8") as stream:
        json.dump(payload, stream, indent=2, sort_keys=True)
        stream.write("\n")


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--original-campaign", type=Path, required=True)
    parser.add_argument("--candidate-campaign", type=Path, required=True)
    parser.add_argument("--original-name", required=True)
    parser.add_argument("--candidate-name", required=True)
    parser.add_argument("--seed0-qualification", type=Path, default=None)
    parser.add_argument("--candidate-qualification", type=Path, default=None)
    parser.add_argument("--output", type=Path, required=True)
    return parser.parse_args()


def main() -> int:
    args = _parse_args()
    report = build_report(
        original_campaign_path=args.original_campaign.resolve(),
        candidate_campaign_path=args.candidate_campaign.resolve(),
        original_name=args.original_name,
        candidate_name=args.candidate_name,
        seed0_qualification_path=(
            args.seed0_qualification.resolve() if args.seed0_qualification else None
        ),
        candidate_qualification_path=(
            args.candidate_qualification.resolve()
            if args.candidate_qualification
            else None
        ),
    )
    write_json_exclusive(args.output, report)
    print(
        f"offline_comparable={int(report['offline_check']['pass'])} "
        f"DUT_status={report['candidate']['DUT_status']} "
        f"coverage_hit={int(report['candidate']['coverage_hit'])} "
        f"output={args.output.resolve()}"
    )
    return 0 if report["offline_check"]["pass"] else 1


if __name__ == "__main__":
    raise SystemExit(main())

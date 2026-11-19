import hashlib
import json
from pathlib import Path

import pytest

from . import candidate_offline_compare as compare
from .candidate_offline_compare import _comparison_metrics, write_json_exclusive


def _analysis(dynamic, unique, unique_edges, dynamic_edges, events=None):
    counts = {
        "trace.committed_instruction": dynamic,
        "trace.unique_pc": unique,
        **(events or {}),
    }
    return {
        "missing_program_map_pc_count": 0,
        "events": [
            {"event": event, "count": count, "evidence_source": "fixture"}
            for event, count in counts.items()
        ],
        "program_diversity": {
            "unique_control_flow_edges": unique_edges,
            "dynamic_control_flow_edges": dynamic_edges,
            "instruction_start_boundary_transitions": {
                "fetch_block": 3,
                "cacheline": 2,
                "page": 1,
            },
        },
    }


def _program_map(static, text_bytes):
    return {"summary": {"static_instruction_count": static}, "text_size_bytes": text_bytes}


def _sha256(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _write_json(path, payload):
    path.write_text(json.dumps(payload, sort_keys=True) + "\n", encoding="ascii")
    return path


def _input_record(root, name, *, directed_pattern, candidate=False):
    source = root / f"{name}.S"
    binary = root / f"{name}.bin"
    trace = root / f"{name}.trace.jsonl"
    program_map = root / f"{name}.program_map.json"
    scenario_spec = root / f"{name}.scenario_spec.json"
    source.write_text(".text\n", encoding="ascii")
    binary.write_bytes(b"\x13\x00\x00\x00")
    trace.write_text('{"index":0,"pc":"0x80000000","size":4}\n', encoding="ascii")
    binary_sha256 = _sha256(binary)
    _write_json(
        program_map,
        {
            "binary_sha256": binary_sha256,
            "text_sha256": binary_sha256,
            "binary_text_verified": True,
            "summary": {"static_instruction_count": 1},
            "text_size_bytes": 4,
            "instructions": [{"pc": 0x80000000, "size": 4}],
        },
    )
    parameters = {
        "family": "cfg_layout_mix",
        "blocks": 48,
        "iterations": 10,
        "max_call_depth": 6,
        "ordinal": 0,
        "seed": 17,
        "directed_pattern": directed_pattern,
    }
    scenario_parameters = {"generator_parameters": parameters}
    if candidate:
        scenario_parameters["pattern_reuse"] = {"pattern_id": directed_pattern}
    _write_json(
        scenario_spec,
        {
            "family": "cfg_layout_mix",
            "program_seed": 17,
            "timing_seed": 29,
            "parameters": scenario_parameters,
        },
    )
    record = {
        "name": name,
        "execution_status": "completed",
        "exit_code": 0,
        "program_map_status": "valid",
    }
    for label, path in (
        ("source", source),
        ("bin", binary),
        ("trace", trace),
        ("program_map", program_map),
        ("scenario_spec", scenario_spec),
    ):
        record[f"{label}_path"] = str(path)
        record[f"{label}_sha256"] = _sha256(path)
    if candidate:
        realization = _write_json(
            root / f"{name}.scenario_realization.json",
            {"scenario_id": name, "status": "REALIZED", "missing_events": []},
        )
        record.update(
            {
                "scenario_realization_path": str(realization),
                "scenario_realization_status": "REALIZED",
                "scenario_realization_sha256": _sha256(realization),
            }
        )
    return record


def test_metrics_separate_static_inflation_from_executed_gain():
    original = _analysis(100, 20, 7, 30, {"trace.cross_page_instruction": 1})
    candidate = _analysis(110, 22, 9, 35, {"trace.cross_page_instruction": 11})
    report = _comparison_metrics(
        _program_map(40, 160), _program_map(80, 320), original, candidate
    )
    assert report["program_map_static_instruction_count"]["delta"] == 40
    assert report["dynamic_instruction_count"]["delta"] == 10
    assert report["unique_executed_pc_count"]["delta"] == 2
    assert report["unique_control_flow_edge_count"]["delta"] == 2
    assert report["dynamic_event_counts"]["trace.cross_page_instruction"]["delta"] == 10
    assert report["execution_efficiency"]["incremental_unique_pc_per_added_static_instruction"] == 0.05
    assert report["execution_efficiency"]["unique_pc_per_static_instruction"]["delta"] == pytest.approx(-0.225)
    repetition = report["dynamic_pc_repetition"]
    assert repetition["repeated_dynamic_instruction_count"] == {
        "original": 80,
        "candidate": 88,
        "delta": 8,
        "delta_percent": 10.0,
    }
    assert repetition["incremental_unique_pc_fraction"] == pytest.approx(0.2)
    assert repetition["incremental_repeated_dynamic_instruction_count"] == 8
    assert repetition["unique_pc_fraction"]["original"] == pytest.approx(0.2)
    assert repetition["unique_pc_fraction"]["candidate"] == pytest.approx(0.2)


def test_exclusive_sidecar_writer_never_overwrites(tmp_path):
    output = tmp_path / "comparison.json"
    write_json_exclusive(output, {"version": 1})
    with pytest.raises(FileExistsError):
        write_json_exclusive(output, {"version": 2})
    assert json.loads(output.read_text()) == {"version": 1}


def test_bare_comparison_uses_real_trace_analyzer(tmp_path):
    original_name = "original"
    candidate_name = "candidate_cjor"
    original = _input_record(
        tmp_path, original_name, directed_pattern="none"
    )
    candidate = _input_record(
        tmp_path,
        candidate_name,
        directed_pattern="taken_cj_offset_ring",
        candidate=True,
    )
    original_campaign = _write_json(
        tmp_path / "original-campaign.json", {"cases": [original]}
    )
    candidate_campaign = _write_json(
        tmp_path / "candidate-campaign.json", {"cases": [candidate]}
    )

    report = compare.build_report(
        original_campaign_path=original_campaign,
        candidate_campaign_path=candidate_campaign,
        original_name=original_name,
        candidate_name=candidate_name,
    )

    assert report["offline_check"]["pass"] is True
    assert report["qualification"]["pass"] is False
    assert report["canonical_hit_eligible"] is False
    assert report["offline_metrics"]["dynamic_instruction_count"]["candidate"] == 1
    assert report["candidate"]["configuration_preflight"] == {
        "applicable": False,
        "path": None,
        "sha256": None,
        "status": "not_applicable",
        "reason": "bare_program_family",
    }
    assert report["candidate"]["translation_attributes"] == {
        "applicable": False,
        "reason": "bare_program_family",
    }
    assert report["candidate"]["DUT_status"] == "unverified"
    assert report["candidate"]["coverage_hit"] is False


@pytest.mark.parametrize("tamper_artifact", [False, True])
def test_historical_sidecars_only_supply_diagnostics(tmp_path, tamper_artifact):
    original_name = "original"
    candidate_name = "candidate_cjor"
    original = _input_record(tmp_path, original_name, directed_pattern="none")
    candidate = _input_record(
        tmp_path,
        candidate_name,
        directed_pattern="taken_cj_offset_ring",
        candidate=True,
    )
    original_campaign = _write_json(
        tmp_path / "original-campaign.json", {"cases": [original]}
    )
    candidate_campaign = _write_json(
        tmp_path / "candidate-campaign.json", {"cases": [candidate]}
    )
    definitions = [
        {"bin_id": "BIN-1"},
        {"bin_id": "BIN-2"},
    ]

    def artifact(name, hits):
        path = tmp_path / f"{name}.funcov.json"
        return _write_json(
            path,
            {
                "provenance": {
                    "compatibility_signature": "same-contract",
                    "definitions_sha256": "same-definitions",
                },
                "definitions": definitions,
                "hits": {
                    bin_id: {"bin_id": bin_id, "hits": 1} for bin_id in hits
                },
                "coverage_targets": {"bin_ids": []},
            },
        )

    def qualification(name, artifact_path, raw_hit_count):
        return _write_json(
            tmp_path / f"{name}.qualification.json",
            {
                "scenario_id": name,
                "replay_artifact_qualification": {"pass": True},
                "dut_pass": {"pass": True},
                "coverage_hit_eligibility": {
                    "eligible": False,
                    "reason": "diagnostic_run_has_no_declared_target_bins",
                    "raw_observed_hit_count": raw_hit_count,
                },
                "recorded_compatibility_signature": "same-contract",
                "inputs": {
                    "artifact": {
                        "path": str(artifact_path),
                        "sha256": _sha256(artifact_path),
                    }
                },
            },
        )

    original_artifact = artifact(original_name, ["BIN-1"])
    candidate_artifact = artifact(candidate_name, ["BIN-1", "BIN-2"])
    original_qualification = qualification(original_name, original_artifact, 1)
    candidate_qualification = qualification(candidate_name, candidate_artifact, 2)

    if tamper_artifact:
        candidate_artifact.write_text("{}")
    report = compare.build_report(
        original_campaign_path=original_campaign,
        candidate_campaign_path=candidate_campaign,
        original_name=original_name,
        candidate_name=candidate_name,
        seed0_qualification_path=original_qualification,
        candidate_qualification_path=candidate_qualification,
    )

    assert report["offline_check"]["pass"] is True
    assert report["qualification"]["pass"] is False
    assert report["candidate"]["DUT_status"] == "unverified"
    assert report["candidate"]["historical_replay_sidecar"]["reported_DUT_pass"] is True
    assert report["candidate"]["coverage_hit"] is False
    assert report["canonical_hit_eligible"] is False
    comparison = report["diagnostic_raw_funcov_comparison"]
    assert comparison["trusted_coverage_hit_claimed"] is False
    assert comparison["available"] is not tamper_artifact
    if tamper_artifact:
        assert "candidate_qualification_artifact_hash_mismatch" in comparison["reasons"]
    else:
        assert comparison["candidate_only_raw_bin_ids"] == ["BIN-2"]
        assert comparison["candidate_is_raw_superset"] is True


@pytest.mark.parametrize("field", ["trace", "program_map", "scenario_realization"])
def test_changed_input_under_frozen_manifest_is_rejected(tmp_path, field):
    original = _input_record(tmp_path, "original", directed_pattern="none")
    candidate = _input_record(tmp_path, "candidate", directed_pattern="none", candidate=True)
    manifest = _write_json(tmp_path / "campaign.json", {"cases": [original, candidate]})
    Path(candidate[f"{field}_path"]).write_text("{}")
    with pytest.raises(ValueError, match="hash mismatch|hash_mismatch"):
        compare.build_report(original_campaign_path=manifest, candidate_campaign_path=manifest,
                             original_name="original", candidate_name="candidate")


def test_source_archive_does_not_inherit_an_enclosing_git_head(monkeypatch, tmp_path):
    def unexpected(*args, **kwargs):
        pytest.fail("archive attempted to consult the enclosing repository")
    monkeypatch.setattr(compare.subprocess, "run", unexpected)
    assert compare._git_head(tmp_path) is None

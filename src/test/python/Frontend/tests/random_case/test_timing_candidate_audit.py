import hashlib
import json
from pathlib import Path

from .timing_candidate_audit import build_report


def _write(path: Path, payload) -> str:
    path.parent.mkdir(parents=True, exist_ok=True)
    if isinstance(payload, bytes):
        path.write_bytes(payload)
    else:
        path.write_text(json.dumps(payload, sort_keys=True) + "\n", encoding="utf-8")
    return hashlib.sha256(path.read_bytes()).hexdigest()


def test_timing_candidate_audit_qualifies_only_scenario_delta(tmp_path: Path) -> None:
    control_root = tmp_path / "control"
    candidate_root = tmp_path / "candidate"
    source_sha = _write(control_root / "case.S", b"addi zero, zero, 0\n")
    _write(candidate_root / "case.S", b"addi zero, zero, 0\n")
    _write(control_root / "case.generator.json", {"seed": 7})
    _write(candidate_root / "case.generator.json", {"seed": 7})
    binary_sha = _write(control_root / "case.bin", b"\x13\x00\x00\x00")
    trace_sha = _write(control_root / "case.trace.jsonl", b'{"pc":2147483648}\n')
    map_sha = _write(
        control_root / "case.program_map.json",
        {"binary_text_verified": True, "binary_sha256": binary_sha},
    )
    source_test = (
        Path(__file__).resolve().parents[2]
        / "tests/py/jiabowen/test_ifu_instr_uncache_attribute_stability_v3_dut.py"
    )
    source_test_sha = hashlib.sha256(source_test.read_bytes()).hexdigest()
    base_timing = {
        "maximum_backpressure_windows": 1,
        "policy": {"event_triggered": True},
    }
    control_scenario = {
        "scenario_id": "case",
        "required_events": [{"event": "trace.committed_instruction", "min_count": 5000}],
        "parameters": {"timing_plan": base_timing},
    }
    candidate_scenario = json.loads(json.dumps(control_scenario))
    candidate_scenario["required_events"].extend(
        [
            {"event": "dut.uncache_a_stall_cycle", "min_count": 8},
            {"event": "dut.uncache_a_stall_release", "min_count": 1},
        ]
    )
    candidate_scenario["parameters"]["timing_plan"] = {
        **base_timing,
        "uncache_a_stall_cycles": 8,
        "policy": {
            **base_timing["policy"],
            "uncache_a_stall": {"release_requires_handshake": True},
        },
    }
    candidate_scenario["parameters"]["uncache_a_stall_reuse"] = {
        "source_path": "tests/py/jiabowen/test_ifu_instr_uncache_attribute_stability_v3_dut.py",
        "source_sha256": source_test_sha,
    }
    control_scenario_sha = _write(control_root / "case.scenario.json", control_scenario)
    candidate_scenario_sha = _write(
        candidate_root / "case.scenario.json", candidate_scenario
    )
    common_record = {
        "name": "case",
        "spec": {"family": "translation_timing_cross", "seed": 7, "ordinal": 0},
        "source_sha256": source_sha,
        "generator_sha256": "generator-source",
        "logical_static_instructions": 1,
        "timing_seed": 11,
    }
    control_record = {
        **common_record,
        "qualification_pass": True,
        "source_path": str(control_root / "case.S"),
        "generator_manifest_path": str(control_root / "case.generator.json"),
        "bin_path": str(control_root / "case.bin"),
        "bin_sha256": binary_sha,
        "trace_path": str(control_root / "case.trace.jsonl"),
        "trace_sha256": trace_sha,
        "program_map_path": str(control_root / "case.program_map.json"),
        "program_map_sha256": map_sha,
        "scenario_spec_path": str(control_root / "case.scenario.json"),
        "scenario_spec_sha256": control_scenario_sha,
    }
    candidate_record = {
        **common_record,
        "source_path": str(candidate_root / "case.S"),
        "generator_manifest_path": str(candidate_root / "case.generator.json"),
        "scenario_spec_path": str(candidate_root / "case.scenario.json"),
        "scenario_spec_sha256": candidate_scenario_sha,
    }
    common_campaign = {
        "master_seed": 1,
        "timing_master_seed": 2,
        "families": ["translation_timing_cross"],
        "family_case_counts": {"translation_timing_cross": 1},
        "planned_case_count": 1,
        "repo_head": "fixed",
        "parameters": {"directed_pattern": "page_tail_pair_sparse"},
    }
    control_campaign = {
        **common_campaign,
        "status": "pass",
        "mode": "nemu",
        "execution": {"complete": True},
        "cases": [control_record],
    }
    candidate_campaign = {
        **common_campaign,
        "status": "generated",
        "mode": "generate",
        "parameters": {
            **common_campaign["parameters"],
            "uncache_a_stall_cycles": 8,
        },
        "cases": [candidate_record],
    }
    control_campaign_path = control_root / "campaign.json"
    candidate_campaign_path = candidate_root / "campaign.json"
    _write(control_campaign_path, control_campaign)
    _write(candidate_campaign_path, candidate_campaign)

    report = build_report(control_campaign_path, candidate_campaign_path)

    assert report["qualification"]["pass"] is True
    assert report["qualification"]["nemu_replay_required"] is False
    assert report["qualification"]["dut_replay_completed"] is False
    assert report["qualification"]["coverage_hit_claimed"] is False
    assert report["cases"][0]["scenario"]["only_timing_action_changed"] is True
    assert report["cases"][0]["metrics"] == {
        "static_instruction_delta": 0,
        "dynamic_instruction_delta": 0,
        "cfg_edge_delta": 0,
        "repeated_instruction_delta": 0,
        "scope": "identical inherited program and NEMU trace",
    }

    control_scenario["parameters"].update(
        {
            "pattern_reuse": {"attribute_mode": "rvi_nc_pair"},
            "translation": {"attribute_plan": {"profile": "old", "pbmt": 0}},
        }
    )
    candidate_scenario["parameters"].update(
        {
            "pattern_reuse": {"attribute_mode": "rvi_pbmt_nc_pair"},
            "translation": {
                "attribute_plan": {
                    "profile": "page_tail_rvc_pma_nc_rvi_pbmt_nc_pair",
                    "pbmt": 0,
                    "pbmte": 1,
                    "pma_pattern": [1, 0, 1, 1, 1],
                    "pbmt_pattern": [0, 0, 0, 1, 1],
                }
            },
        }
    )
    control_record["scenario_spec_sha256"] = _write(
        control_root / "case.scenario.json", control_scenario
    )
    candidate_record["scenario_spec_sha256"] = _write(
        candidate_root / "case.scenario.json", candidate_scenario
    )
    control_campaign["parameters"] = {
        **common_campaign["parameters"],
        "page_tail_attribute_mode": "rvi_nc_pair",
    }
    candidate_campaign["parameters"] = {
        **common_campaign["parameters"],
        "page_tail_attribute_mode": "rvi_pbmt_nc_pair",
        "uncache_a_stall_cycles": 8,
    }
    _write(control_campaign_path, control_campaign)
    _write(candidate_campaign_path, candidate_campaign)

    pbmt_report = build_report(
        control_campaign_path,
        candidate_campaign_path,
        candidate_kind="pbmt_nc_stall",
    )

    assert pbmt_report["qualification"]["pass"] is True
    assert pbmt_report["candidate_kind"] == "pbmt_nc_stall"
    assert pbmt_report["cases"][0]["scenario"][
        "only_pbmt_nc_attribute_and_timing_changed"
    ] is True

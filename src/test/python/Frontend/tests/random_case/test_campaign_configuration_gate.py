import json

import pytest

from . import run_random_campaign as runner
from .scenario_contract import EventRequirement, ScenarioSpec


@pytest.mark.parametrize("layout,qualified", [("per_page_v1", False), ("exact_napot_v1", True)])
def test_nemu_pass_cannot_qualify_invalid_translation_configuration(tmp_path, monkeypatch, layout, qualified):
    name = "configuration-gate"
    frontend = tmp_path / "Frontend"
    binary = frontend / "tests/asm_cases/generated" / f"{name}.bin"
    binary.parent.mkdir(parents=True)
    binary.write_bytes(bytes(36 * 4096))
    case_root = tmp_path / "run"
    inputs = case_root / "inputs"
    inputs.mkdir(parents=True)
    (inputs / f"{name}.trace.jsonl").write_text('{"pc":"0x80000000"}\n' * 5000)
    scenario_path = tmp_path / "scenario.json"
    spec = ScenarioSpec(
        scenario_id=name, family="translation_timing_cross", ordinal=0,
        program_seed=1, timing_seed=2, risk_tags=("config_gate",),
        required_events=(EventRequirement("trace.committed_instruction", evidence_stage="program"),),
        parameters={"translation": {
            "schema_version": 1, "randomizer_seed": 123, "ordinal": 0,
            "physical_base": 0x80400000, "guest_physical_base": 0x80600000,
            "permission_layout": layout,
        }},
    )
    scenario_path.write_text(json.dumps(spec.to_dict()))
    monkeypatch.setattr(runner, "FRONTEND_ROOT", frontend)
    # Isolate the campaign's conjunction of gates; exercise the REAL scenario
    # builder and CSR validation with a 36-page payload, no fake validator.
    monkeypatch.setattr(runner, "build_program_map", lambda *a, **kw: {"binary_sha256": runner._sha256(binary)})
    monkeypatch.setattr(runner, "analyze_trace", lambda *a, **kw: {
        "events": [{"event": "trace.committed_instruction", "count": 5000,
                    "evidence_source": "nemu_committed_trace"}],
        "realized_episode_counts": {}, "missing_program_map_pc_count": 0,
    })
    result = runner._case_record_after_run(
        {"name": name, "source_path": str(tmp_path / "source.S"), "scenario_spec_path": str(scenario_path)},
        spec, case_root, "completed", 0, 5000, "nemu", 0x80000000, 0x80000000,
    )
    assert result["exit_code"] == 0
    assert result["dynamic_instruction_requirement_met"]
    assert result["scenario_realization_status"] == "REALIZED"  # program evidence preserved
    assert result["qualification_pass"] is qualified
    config = result["configuration_preflight"]
    assert config["page_count"] == 36
    assert config["status"] == ("valid" if qualified else "invalid")
    if not qualified:
        assert "entry index out of range: 32" in config["reasons"][0]
    assert json.loads((inputs / f"{name}.configuration_preflight.json").read_text()) == config

    # Old manifests can claim true without containing the new gate. Resume
    # must recompute it, even when every recorded input hash is intact.
    retained = {"name": name, "qualification_pass": True}
    for kind in ("source", "bin", "trace", "program_map", "scenario_spec"):
        path = binary if kind == "bin" else scenario_path
        retained[kind + "_path"] = str(path)
        retained[kind + "_sha256"] = runner._sha256(path)
    snapshot = json.dumps(retained, sort_keys=True)
    if qualified:
        runner._validate_retained_translation_records([retained], {name: spec}, 0x80000000)
        retained["trace_sha256"] = "wrong"
        with pytest.raises(ValueError, match="trace hash missing or changed"):
            runner._validate_retained_translation_records([retained], {name: spec}, 0x80000000)
    else:
        with pytest.raises(ValueError, match="configuration preflight failed"):
            runner._validate_retained_translation_records([retained], {name: spec}, 0x80000000)
        assert json.dumps(retained, sort_keys=True) == snapshot


def test_missing_binary_fails_configuration_and_nontranslation_is_not_applicable(tmp_path):
    spec = ScenarioSpec(scenario_id="missing", family="translation_timing_cross", ordinal=0,
                        program_seed=1, timing_seed=2, parameters={"translation": {}})
    assert runner._translation_configuration_preflight(spec, tmp_path / "missing.bin", 0x80000000)["status"] == "invalid"
    spec = ScenarioSpec(scenario_id="cfg", family="cfg_layout_mix", ordinal=0, program_seed=1, timing_seed=2)
    assert runner._translation_configuration_preflight(spec, tmp_path / "missing.bin", 0x80000000)["status"] == "not_applicable"

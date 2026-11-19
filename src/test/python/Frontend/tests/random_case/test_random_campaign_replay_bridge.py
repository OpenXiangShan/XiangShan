from __future__ import annotations

import json
from pathlib import Path

import pytest

from . import run_random_campaign as campaign_runner
from .scenario_contract import ScenarioSpec
from .test_random_bin_trace_dut import _timeline_event_limit


def test_timeline_event_limit_scales_with_trace_and_allows_override(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    monkeypatch.delenv("TB_RANDOM_MAX_TIMELINE_EVENTS", raising=False)
    assert _timeline_event_limit(1000) == 20_000
    assert _timeline_event_limit(6781) == 54_248

    monkeypatch.setenv("TB_RANDOM_MAX_TIMELINE_EVENTS", "75000")
    assert _timeline_event_limit(6781) == 75_000

    monkeypatch.setenv("TB_RANDOM_MAX_TIMELINE_EVENTS", "-1")
    with pytest.raises(AssertionError, match="must be >= 0"):
        _timeline_event_limit(6781)


def test_failed_dut_run_still_preserves_verified_program_map(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    frontend_root = tmp_path / "Frontend"
    case_name = "failed-random-case"
    binary_path = frontend_root / "tests" / "asm_cases" / "generated" / f"{case_name}.bin"
    binary_path.parent.mkdir(parents=True)
    binary_path.write_bytes(b"\x13\x00\x00\x00")
    source_path = tmp_path / f"{case_name}.S"
    source_path.write_text("addi zero, zero, 0\n", encoding="ascii")
    scenario_path = tmp_path / f"{case_name}.scenario.json"
    scenario_path.write_text("{}\n", encoding="ascii")
    case_root = tmp_path / "case"
    trace_path = case_root / "inputs" / f"{case_name}.trace.jsonl"
    trace_path.parent.mkdir(parents=True)
    trace_path.write_text('{"pc": 2147483648}\n', encoding="ascii")
    calls: list[str] = []

    def fake_program_map(*_args, **_kwargs) -> dict:
        calls.append("build")
        return {
            "binary_sha256": "binary-hash",
            "binary_text_verified": True,
        }

    def fake_trace_analysis(_trace_path: Path, _program_map: dict) -> dict:
        return {
            "events": [],
            "realized_episode_counts": {},
            "missing_program_map_pc_count": 0,
            "missing_program_map_pcs": [],
        }

    monkeypatch.setattr(campaign_runner, "FRONTEND_ROOT", frontend_root)
    monkeypatch.setattr(campaign_runner, "build_program_map", fake_program_map)
    monkeypatch.setattr(campaign_runner, "analyze_trace", fake_trace_analysis)
    scenario = ScenarioSpec(
        scenario_id=case_name,
        family="test",
        ordinal=0,
        program_seed=1,
        timing_seed=2,
        risk_tags=("failure_evidence",),
    )

    result = campaign_runner._case_record_after_run(
        {
            "name": case_name,
            "source_path": str(source_path),
            "scenario_spec_path": str(scenario_path),
        },
        scenario,
        case_root,
        "completed",
        1,
        5000,
        "nemu",
        0x80000000,
        0x80000000,
    )

    assert calls == ["build"]
    assert result["program_map_status"] == "valid"
    assert result["scenario_realization_status"] == "FAIL"
    assert result["qualification_pass"] is False
    realization_path = Path(result["scenario_realization_path"])
    assert result["scenario_realization_sha256"] == campaign_runner._sha256(realization_path)
    assert json.loads(realization_path.read_text())["status"] == "FAIL"

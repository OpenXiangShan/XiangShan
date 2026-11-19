from __future__ import annotations

import csv
import copy
import hashlib
import json
import shutil
import subprocess
from pathlib import Path

import pytest

from .funcov_compare import build_report, load_artifact, write_report
from . import run_random_campaign as campaign_runner
from .generate_random_asm import CaseSpec, generate_case, write_cases
from .program_map import build_program_map, parse_disassembly
from .run_random_campaign import _load_dut_timeline, _load_funcov_evidence
from .scenario_contract import (
    EventRequirement,
    RealizedEvent,
    ScenarioSpec,
    evaluate_realization,
)
from .scenario_coordinator import ScenarioCoordinator, TimingPlan
from .scenario_coordinator import (
    BIN951_IFU_SIGNAL_NAMES,
    BIN951_FTQ_SIGNAL_NAMES,
    BIN951_REDIRECT_SIGNAL_NAMES,
    validate_bin951_event,
)
from env.support.pc_utils import fold_pc
from .scenario_profiles import build_scenario_spec
from .testpoint_catalog import DEFAULT_SOURCE, build_catalog, write_catalog
from .test_random_bin_trace_dut import _timeline_event_limit


FRONTEND_ROOT = Path(__file__).resolve().parents[2]


def test_campaign_aggregate_separates_case_failures_from_coverage_gates() -> None:
    campaign: dict = {"post_run_gates": []}
    executed = [
        {"name": "qualified", "qualification_pass": True},
        {"name": "checker-failed", "qualification_pass": False},
    ]

    case_failures = campaign_runner._update_case_execution_summary(
        campaign,
        planned_case_count=2,
        executed=executed,
    )
    campaign_runner._record_post_run_gate(
        campaign,
        gate_id="trusted_funcov",
        exit_code=1,
    )

    assert case_failures == 1
    assert campaign["execution"] == {
        "planned_case_count": 2,
        "executed_case_count": 2,
        "qualified_case_count": 1,
        "case_failure_count": 1,
        "complete": True,
    }
    assert campaign_runner._finalize_campaign_status(campaign) == 2
    assert campaign["case_failure_count"] == 1
    assert campaign["post_run_gate_failure_count"] == 1
    assert campaign["status"] == "fail"

    all_qualified = [
        {"name": "qualified-a", "qualification_pass": True},
        {"name": "qualified-b", "qualification_pass": True},
    ]
    campaign = {"post_run_gates": []}
    campaign_runner._update_case_execution_summary(
        campaign,
        planned_case_count=2,
        executed=all_qualified,
    )
    campaign_runner._record_post_run_gate(
        campaign,
        gate_id="trusted_funcov",
        exit_code=1,
    )

    assert campaign_runner._finalize_campaign_status(campaign) == 1
    assert campaign["case_failure_count"] == 0
    assert campaign["post_run_gate_failure_count"] == 1
    assert campaign["status"] == "fail"


def test_family_count_overrides_make_the_generation_matrix_explicit() -> None:
    overrides = campaign_runner._parse_family_count_overrides(
        ["cfg_layout_mix=33", "translation_timing_cross=3"]
    )
    families, counts = campaign_runner._resolve_family_case_counts([], overrides, 34)

    assert families == ["cfg_layout_mix", "translation_timing_cross"]
    assert counts == {"cfg_layout_mix": 33, "translation_timing_cross": 3}

    with pytest.raises(ValueError, match="duplicate"):
        campaign_runner._parse_family_count_overrides(
            ["cfg_layout_mix=1", "cfg_layout_mix=2"]
        )
    with pytest.raises(ValueError, match="not selected"):
        campaign_runner._resolve_family_case_counts(
            ["cfg_layout_mix"],
            {"translation_timing_cross": 1},
            1,
        )


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

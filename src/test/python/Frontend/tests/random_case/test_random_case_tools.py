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


def test_generator_is_deterministic_and_seed_sensitive() -> None:
    spec = CaseSpec(
        family="cfg_layout_mix",
        seed=0x1234,
        blocks=24,
        iterations=3,
    )
    source_a, metadata_a = generate_case(spec)
    source_b, metadata_b = generate_case(spec)
    source_c, metadata_c = generate_case(
        CaseSpec(family="cfg_layout_mix", seed=0x1235, blocks=24, iterations=3)
    )

    assert source_a == source_b
    assert metadata_a == metadata_b
    assert source_a != source_c
    assert metadata_a["source_sha256"] != metadata_c["source_sha256"]
    assert all(phase in source_a for phase in ("warmup", "train", "perturb", "stress", "drain"))
    assert ".fill 2047, 2, 0x0001" in source_a
    trap_index = source_a.index(".word 0x0005006b")
    drain_padding_index = source_a.index(".rept 128", trap_index)
    assert drain_padding_index > trap_index
    assert "c.nop" in source_a[drain_padding_index:]
    assert set(metadata_a["kind_counts"]) == {
        "branch_rvi",
        "branch_rvc",
        "jal",
        "c_j",
        "jalr",
        "call_ret",
    }


def test_history4_indirect_targets_are_opt_in_and_have_distinct_artifact_names() -> None:
    fixed = CaseSpec(family="cfg_layout_mix", seed=0x1234, blocks=48)
    varied = CaseSpec(family="cfg_layout_mix", seed=0x1234, blocks=48, indirect_target_mode="history4")
    old_source, _ = generate_case(fixed)
    source, metadata = generate_case(varied)
    replay, _ = generate_case(varied)
    assert source == replay
    assert source != old_source
    assert varied.name == fixed.name + "_ih4"
    assert "_target_0:" not in old_source
    assert "srli t1, s3, 7" in source
    assert "slli t1, t1, 5" in source
    episodes = [e for e in metadata["episodes"] if e["kind"] == "jalr"]
    assert episodes
    assert all(e["parameters"]["target_count"] == 4 for e in episodes)
    assert source.count("_target_3:") == len(episodes)
    assert metadata["spec"]["indirect_target_mode"] == "history4"


def test_history4_rejects_families_that_do_not_implement_it() -> None:
    with pytest.raises(ValueError, match="require a CFG or translation family"):
        generate_case(CaseSpec(family="ras_indirect_history", seed=1, indirect_target_mode="history4"))


def test_history4_scenario_requires_observed_four_target_sites() -> None:
    catalog = build_catalog(campaign_runner.DEFAULT_TESTPOINT_SOURCE)
    _, record = generate_case(CaseSpec(family="cfg_layout_mix", seed=0x1234, blocks=48,
                                       indirect_target_mode="history4"))
    scenario = build_scenario_spec(record, timing_seed=11, testpoint_catalog=catalog)
    requirement = next(e for e in scenario.required_events if e.event == "trace.indirect_four_target_site")
    assert requirement.min_count == sum(e["kind"] == "jalr" for e in record["episodes"])
    assert requirement.evidence_stage == "program"


def test_history4_page_uses_distinct_name_stride_and_observed_page_gate() -> None:
    base = CaseSpec(family="translation_timing_cross", seed=0x1234, blocks=48)
    page = CaseSpec(family=base.family, seed=base.seed, blocks=48, indirect_target_mode="history4_page")
    source, record = generate_case(page)
    assert page.name == base.name + "_ih4p"
    assert source == generate_case(page)[0]
    assert "slli t1, t1, 12" in source
    episodes = [e for e in record["episodes"] if e["kind"] == "jalr"]
    assert episodes
    for episode in episodes:
        assert episode["parameters"]["target_stride_bytes"] == 4096
        label = episode["entry_labels"][0]
        for target in range(4):
            assert f".balign 4096\n{label}_target_{target}:" in source
    scenario = build_scenario_spec(record, timing_seed=11,
                                   testpoint_catalog=build_catalog(campaign_runner.DEFAULT_TESTPOINT_SOURCE))
    requirement = next(e for e in scenario.required_events if e.event == "trace.indirect_four_target_page_site")
    assert requirement.min_count == len(episodes)
    assert requirement.evidence_stage == "program"
    assert scenario.parameters["translation"]["permission_layout"] == "exact_napot_v1"
    _, old_record = generate_case(base)
    old_scenario = build_scenario_spec(old_record, timing_seed=11,
                                       testpoint_catalog=build_catalog(campaign_runner.DEFAULT_TESTPOINT_SOURCE))
    assert "permission_layout" not in old_scenario.parameters["translation"]


def test_ras_family_contains_nested_and_indirect_control_flow() -> None:
    source, metadata = generate_case(
        CaseSpec(
            family="ras_indirect_history",
            seed=0x5678,
            blocks=16,
            iterations=3,
            max_call_depth=5,
        )
    )

    assert "ras_chain_000_d00" in source
    assert "jalr zero, 0(ra)" in source
    assert "jal t0," in source
    assert "jalr zero, 0(t0)" in source
    assert metadata["max_generated_call_depth"] <= 5
    assert metadata["max_generated_call_depth"] >= 2
    assert metadata["logical_static_instructions"] > 500


def test_ibuffer_pressure_family_contains_long_boundary_bursts() -> None:
    source, metadata = generate_case(
        CaseSpec(
            family="ibuffer_pressure_cross",
            seed=0x9876,
            blocks=16,
            iterations=3,
        )
    )

    assert "ibp_main_loop" in source
    assert "jalr zero, 0(t1)" in source
    assert "jalr zero, 0(ra)" in source
    assert {14, 28, 30, 60, 62} & set(metadata["boundary_offsets"])
    assert min(metadata["pressure_burst_lengths"]) >= 32
    assert metadata["maximum_pressure_burst"] >= 64
    assert metadata["logical_static_instructions"] > 800


def test_write_cases_records_replay_metadata(tmp_path: Path) -> None:
    records = write_cases(
        [CaseSpec(family="cfg_layout_mix", seed=7, blocks=16, iterations=2)],
        tmp_path,
    )

    assert len(records) == 1
    record = records[0]
    source_path = Path(record["source_path"])
    sidecar_path = Path(record["generator_manifest_path"])
    assert source_path.is_file()
    assert sidecar_path.is_file()
    assert json.loads(sidecar_path.read_text(encoding="ascii"))["source_sha256"] == record[
        "source_sha256"
    ]
    assert record["episodes"]
    assert record["episodes"][0]["entry_labels"]


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


@pytest.mark.skipif(
    shutil.which("riscv64-unknown-elf-gcc") is None,
    reason="RISC-V assembler is not installed",
)
@pytest.mark.parametrize(
    "family",
    (
        "cfg_layout_mix",
        "ras_indirect_history",
        "ibuffer_pressure_cross",
        "translation_timing_cross",
    ),
)
def test_generated_source_assembles(family: str, tmp_path: Path) -> None:
    source, _ = generate_case(
        CaseSpec(
            family=family,
            seed=0xA55A,
            blocks=16,
            iterations=2,
            max_call_depth=4,
        )
    )
    asm_path = tmp_path / f"{family}.S"
    elf_path = tmp_path / f"{family}.elf"
    asm_path.write_text(source, encoding="ascii")
    subprocess.run(
        [
            "riscv64-unknown-elf-gcc",
            "-march=rv64gc_zicsr_zifencei",
            "-mabi=lp64",
            "-static",
            "-no-pie",
            "-nostdlib",
            "-nostartfiles",
            "-Wl,--build-id=none",
            "-Wl,-Ttext=0x80000000",
            "-Wl,-e,_start",
            "-o",
            str(elf_path),
            str(asm_path),
        ],
        check=True,
    )
    assert elf_path.is_file()

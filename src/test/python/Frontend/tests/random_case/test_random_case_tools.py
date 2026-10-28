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
    realization_path = Path(result["scenario_realization_path"])
    assert result["scenario_realization_sha256"] == campaign_runner._sha256(realization_path)
    assert json.loads(realization_path.read_text())["status"] == "FAIL"


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


def test_objdump_parser_preserves_instruction_width_and_labels() -> None:
    instructions, labels = parse_disassembly(
        """
0000000080000000 <_start>:
    80000000:  0001                 c.addi  x0,0
    80000002:  008000ef             jal     x1,8000000a <callee>
000000008000000a <callee>:
    8000000a:  00008067             jalr    x0,0(x1)
"""
    )

    assert labels == {"_start": 0x80000000, "callee": 0x8000000A}
    assert [item["size"] for item in instructions] == [2, 4, 4]
    assert instructions[0]["labels"] == ["_start"]
    assert instructions[-1]["labels"] == ["callee"]


@pytest.mark.parametrize(
    "second_pc",
    [0x80000000, 0x80000002],
)
def test_objdump_parser_rejects_duplicate_or_overlapping_instructions(second_pc) -> None:
    with pytest.raises(ValueError, match="out-of-order, overlapping, or duplicate"):
        parse_disassembly(
            f"""
    80000000:  00000013             addi    x0,x0,0
    {second_pc:x}:  0001                 c.addi  x0,0
"""
        )


def test_objdump_parser_rejects_out_of_order_instructions() -> None:
    with pytest.raises(ValueError, match="out-of-order"):
        parse_disassembly(
            """
    80000004:  0001                 c.addi  x0,0
    80000000:  0001                 c.addi  x0,0
"""
        )


def test_objdump_parser_rejects_conflicting_label_addresses() -> None:
    with pytest.raises(ValueError, match="conflicting disassembly label"):
        parse_disassembly(
            """
0000000080000000 <same>:
    80000000:  0001                 c.addi  x0,0
0000000080000002 <same>:
    80000002:  0001                 c.addi  x0,0
"""
        )


@pytest.mark.skipif(
    any(
        shutil.which(tool) is None
        for tool in (
            "riscv64-unknown-elf-gcc",
            "riscv64-unknown-elf-objcopy",
            "riscv64-unknown-elf-objdump",
        )
    ),
    reason="RISC-V ELF tools are not installed",
)
def test_program_map_is_verified_against_actual_binary(tmp_path: Path) -> None:
    source, metadata = generate_case(
        CaseSpec(family="cfg_layout_mix", seed=0xC0FFEE, blocks=16, iterations=2)
    )
    source_path = tmp_path / "case.S"
    elf_path = tmp_path / "case.elf"
    binary_path = tmp_path / "case.bin"
    source_path.write_text(source, encoding="ascii")
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
            str(source_path),
        ],
        check=True,
    )
    subprocess.run(
        [
            "riscv64-unknown-elf-objcopy",
            "-O",
            "binary",
            "-j",
            ".text",
            str(elf_path),
            str(binary_path),
        ],
        check=True,
    )

    program_map = build_program_map(
        source_path,
        binary_path,
        metadata,
        link_address=0x80000000,
        binary_base=0x80000000,
    )

    assert program_map["binary_text_verified"] is True
    assert program_map["summary"]["episode_region_count"] >= 17
    assert program_map["summary"]["feature_counts"]["crosses_page"] == 1
    assert any(item["episode_id"] == "cfg-page-tail-probe" for item in program_map["instructions"])


def _write_funcov(
    path: Path,
    *,
    signature: str,
    run_id: str,
    hit_bin_ids: tuple[str, ...],
    program_seed: int | None = None,
    timing_seed: int | None = None,
) -> None:
    definitions = [
        {
            "bin_id": bin_id,
            "coverage_group": "group",
            "coverpoint": "point",
            "bin_name": f"name_{bin_id}",
            "suggested_testcase": "case",
        }
        for bin_id in ("BIN-001", "BIN-002", "BIN-003", "BIN-004")
    ]
    path.write_text(
        json.dumps(
            {
                "artifact_schema_version": 2,
                "artifact_tag": run_id,
                "provenance": {
                    "compatibility_signature": signature,
                    "definitions_sha256": "definitions",
                    "build_manifest_status": "valid",
                },
                "definitions": definitions,
                "hits": {
                    f"group::point::name_{bin_id}": {"bin_id": bin_id, "hits": 1}
                    for bin_id in hit_bin_ids
                },
                "run": {
                    "run_id": run_id,
                    "pytest_outcome": "pass",
                    "checker": {"status": "pass"},
                    "run_command": f"replay {run_id}",
                    **(
                        {
                            "seed": program_seed,
                            "seeds": {
                                "test": program_seed,
                                "backend": timing_seed,
                            },
                        }
                        if program_seed is not None and timing_seed is not None
                        else {}
                    ),
                },
                "outcome": {"status": "pass", "exit_code": 0},
                "checker": {"status": "pass", "error_count": 0, "errors": []},
            }
        ),
        encoding="utf-8",
    )


def test_funcov_report_uses_only_compatible_baseline_and_tracks_marginal_gain(
    tmp_path: Path,
) -> None:
    random_a = tmp_path / "random_a.funcov.json"
    random_b = tmp_path / "random_b.funcov.json"
    baseline = tmp_path / "baseline.funcov.json"
    incompatible = tmp_path / "incompatible.funcov.json"
    _write_funcov(
        random_a,
        signature="same",
        run_id="random-a",
        hit_bin_ids=("BIN-001",),
        program_seed=1,
        timing_seed=11,
    )
    _write_funcov(
        random_b,
        signature="same",
        run_id="random-b",
        hit_bin_ids=("BIN-001", "BIN-003"),
        program_seed=2,
        timing_seed=22,
    )
    _write_funcov(baseline, signature="same", run_id="directed", hit_bin_ids=("BIN-001", "BIN-002"))
    _write_funcov(incompatible, signature="old", run_id="old", hit_bin_ids=("BIN-004",))

    report = build_report(
        [load_artifact(random_a), load_artifact(random_b)],
        [load_artifact(baseline), load_artifact(incompatible)],
    )
    report_path = tmp_path / "comparison.json"
    write_report(report, report_path)

    assert report["random"]["union_hit_bins"] == 2
    assert report["random"]["total_definition_bins"] == 4
    assert report["random"]["hit_rate"] == 0.5
    assert report["random"]["unhit_bin_ids"] == ["BIN-002", "BIN-004"]
    assert report["qualification"]["pass"] is False
    assert report["directed_baseline"]["union_hit_bins"] == 2
    assert report["comparison"]["random_only_bin_ids"] == ["BIN-003"]
    assert report["comparison"]["baseline_only_bin_ids"] == ["BIN-002"]
    assert [item["new_random_union_bins"] for item in report["per_random_artifact_marginal_gain"]] == [1, 1]
    assert [item["seed_pair"] for item in report["per_random_artifact_marginal_gain"]] == [
        [1, 11],
        [2, 22],
    ]
    corpus = report["coverage_guided_corpus"]
    assert corpus["status"] == "available"
    assert corpus["union_preserved"] is True
    assert [item["run_id"] for item in corpus["retained_runs"]] == ["random-b"]
    assert [item["run_id"] for item in corpus["discard_candidates"]] == ["random-a"]
    assert corpus["bin_repeatability"]["distinct_seed_pairs"] == 2
    assert corpus["bin_repeatability"]["bins_hit_by_multiple_seed_pairs"] == 1
    assert corpus["bin_repeatability"]["single_seed_bin_ids"] == ["BIN-003"]
    assert [item["run_id"] for item in report["directed_baseline"]["comparable_artifacts"]] == [
        "directed"
    ]
    assert report["skipped_baseline_artifacts"][0]["reason"] == "incompatible_signature"
    assert report_path.is_file()
    assert report_path.with_suffix(".csv").is_file()


def test_funcov_report_does_not_union_incompatible_random_contracts(tmp_path: Path) -> None:
    random_a = tmp_path / "random_a.funcov.json"
    random_b = tmp_path / "random_b.funcov.json"
    _write_funcov(random_a, signature="contract-a", run_id="random-a", hit_bin_ids=("BIN-001",))
    _write_funcov(random_b, signature="contract-b", run_id="random-b", hit_bin_ids=("BIN-002",))

    report = build_report(
        [load_artifact(random_a), load_artifact(random_b)],
        [],
    )

    assert report["strictly_comparable"] is False
    assert report["random"]["union_hit_bins"] is None
    assert report["random"]["hit_rate"] is None
    assert len(report["random"]["by_contract"]) == 2
    assert "multiple_random_compatibility_contracts" in report["qualification"]["reasons"]
    assert report["coverage_guided_corpus"]["status"] == "unavailable"
    assert report["coverage_guided_corpus"]["retained_runs"] == []


def test_random_sources_are_not_discovered_by_directed_asm_suite() -> None:
    suite_script = FRONTEND_ROOT / "scripts" / "run_baremode_asm_suite.sh"
    listed = subprocess.run(
        ["bash", str(suite_script), "--list"],
        check=True,
        capture_output=True,
        text=True,
    ).stdout.splitlines()

    assert listed
    assert all("/tests/asm_cases/" in path for path in listed)
    assert all("/tests/random_case/" not in path for path in listed)


def test_testpoint_catalog_preserves_many_to_many_traceability(tmp_path: Path) -> None:
    source = tmp_path / "testpoints.csv"
    columns = (
        "一级测试点",
        "二级测试点",
        "三级测试点",
        "四级测试点",
        "五级测试点",
        "Condition",
        "Checkpoint",
        "Object",
        "coverage",
        "status",
        "testcase",
        "evidence",
    )
    rows = [
        {"一级测试点": "Frontend"},
        {"二级测试点": "IFU"},
        {
            "五级测试点": "cross boundary redirect",
            "Condition": "redirect at page tail",
            "coverage": "bins (BIN-001) and (ATP-002)",
            "status": "MODELED",
        },
        {"四级测试点": "unnamed group"},
        {"Condition": "metadata without a leaf name", "status": "UNMAPPED"},
        {"五级测试点": "document title without metadata"},
    ]
    with source.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=columns)
        writer.writeheader()
        for row in rows:
            writer.writerow(row)

    catalog = build_catalog(source)
    output = tmp_path / "catalog.json"
    write_catalog(catalog, output)

    assert catalog["summary"]["testpoint_count"] == 3
    assert catalog["summary"]["record_kind_counts"] == {
        "documented_leaf": 1,
        "undocumented_leaf": 1,
        "unnamed_leaf": 1,
    }
    assert catalog["testpoints"][0]["coverage_refs"] == ["ATP-002", "BIN-001"]
    assert catalog["testpoints"][0]["path_text"] == "Frontend/IFU/cross boundary redirect"
    assert catalog["testpoints"][1]["path_text"] == "Frontend/IFU/unnamed group"
    assert json.loads(output.read_text(encoding="utf-8"))["source_sha256"]


def test_scenario_contract_uses_dynamic_many_to_many_coverage() -> None:
    spec = ScenarioSpec(
        scenario_id="cfg-000",
        family="cfg_layout_mix",
        ordinal=0,
        program_seed=1,
        timing_seed=2,
        testpoint_ids=("TP-A", "TP-B"),
        risk_tags=("redirect", "page_boundary"),
        required_events=(
            EventRequirement(event="backend.redirect", relation="T+1", anchor="uncache.d"),
        ),
    )

    payload = spec.to_dict()

    assert payload["minimum_dynamic_instructions"] == 5000
    assert payload["coverage_attribution"]["policy"] == "dynamic_many_to_many"
    assert payload["coverage_attribution"]["single_target_bin"] is False
    assert payload["coverage_attribution"]["planned_testpoint_ids"] == ["TP-A", "TP-B"]


def test_scenario_profile_links_each_case_to_multiple_canonical_testpoints(tmp_path: Path) -> None:
    catalog = build_catalog(DEFAULT_SOURCE)
    record = write_cases(
        [CaseSpec(family="cfg_layout_mix", seed=7, blocks=16, iterations=2)],
        tmp_path,
    )[0]

    scenario = build_scenario_spec(record, timing_seed=11, testpoint_catalog=catalog)

    assert scenario.program_seed == 7
    assert scenario.timing_seed == 11
    assert len(scenario.testpoint_ids) > 1
    assert scenario.parameters["testpoint_intent"]
    assert all(item["path_text"] for item in scenario.parameters["testpoint_intent"])
    assert scenario.to_dict()["coverage_attribution"]["single_target_bin"] is False


def test_realization_requires_actual_event_counts() -> None:
    spec = ScenarioSpec(
        scenario_id="case",
        family="cfg_layout_mix",
        ordinal=0,
        program_seed=1,
        timing_seed=2,
        risk_tags=("boundary",),
        required_events=(
            EventRequirement(event="trace.committed_instruction", min_count=5000),
            EventRequirement(event="trace.cross_page_instruction"),
        ),
    )
    events = (
        RealizedEvent(
            event="trace.committed_instruction",
            count=6000,
            evidence_source="nemu_committed_trace",
        ),
    )

    missing = evaluate_realization(
        spec,
        events,
        evidence_stage="nemu_program",
        execution_status="completed",
        exit_code=0,
    )
    realized = evaluate_realization(
        spec,
        (
            *events,
            RealizedEvent(
                event="trace.cross_page_instruction",
                count=1,
                evidence_source="nemu_committed_trace",
            ),
        ),
        evidence_stage="nemu_program",
        execution_status="completed",
        exit_code=0,
    )

    assert missing.status == "NOT_REALIZED"
    assert [item.event for item in missing.missing_events] == ["trace.cross_page_instruction"]
    assert realized.status == "REALIZED"


class _FakeSignal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


class _FakeICacheAgent:
    def __init__(self) -> None:
        self.configuration: dict = {}

    def configure(self, **configuration) -> None:
        self.configuration = dict(configuration)


class _FakeUncacheAgent:
    def __init__(self, interface) -> None:
        self.interface = interface
        self.a_ready_override = None
        self.event_sink = None
        self.overrides: list[int | None] = []

    def set_a_ready(self, value) -> None:
        self.a_ready_override = None if value is None else (1 if int(value) else 0)
        self.overrides.append(self.a_ready_override)

    def on_clock_edge(self, cycle: int) -> None:
        ready = 1 if self.a_ready_override is None else int(self.a_ready_override)
        self.interface.a_ready.value = ready
        if int(self.interface.a_valid.value) != 1 or ready != 1:
            return
        if self.event_sink is not None:
            self.event_sink(
                {
                    "type": "handshake.uncache_a",
                    "source": "uncache_agent",
                    "cycle": int(cycle),
                    "payload": {
                        "address": int(self.interface.a_bits_address.value),
                        "mem_back_type_mm": int(
                            self.interface.a_bits_user_mem_back_type_mm.value
                        ),
                        "mem_page_type_nc": int(
                            self.interface.a_bits_user_mem_page_type_nc.value
                        ),
                    },
                }
            )


class _FakeBackendModel:
    def __init__(self, can_accept_signal: _FakeSignal) -> None:
        self.can_accept_signal = can_accept_signal
        self.values: list[int] = []

    def set_can_accept(self, value: int) -> None:
        normalized = 1 if int(value) else 0
        self.values.append(normalized)
        self.can_accept_signal.value = normalized


class _FakeCoordinatorEnv:
    def __init__(self, previous_event_sink) -> None:
        self.event_sink = previous_event_sink
        self.backend_ctrl_if = type(
            "BackendControl",
            (),
            {"can_accept": _FakeSignal(1), "redirect_valid": _FakeSignal(0)},
        )()
        self.frontend_info_if = type(
            "FrontendInfo",
            (),
            {"io_frontendInfo_ibufFull": _FakeSignal(0)},
        )()
        self.clock_reset = type("ClockReset", (), {"reset": _FakeSignal(0)})()
        self.uncache_if = type(
            "UncacheInterface",
            (),
            {
                "a_valid": _FakeSignal(0),
                "a_ready": _FakeSignal(1),
                "a_bits_address": _FakeSignal(0),
                "a_bits_user_mem_back_type_mm": _FakeSignal(0),
                "a_bits_user_mem_page_type_nc": _FakeSignal(0),
            },
        )()
        self.icache_agent = _FakeICacheAgent()
        self.uncache_agent = _FakeUncacheAgent(self.uncache_if)
        self.backend_model = _FakeBackendModel(self.backend_ctrl_if.can_accept)
        self.dut = None
        self.cycle_observers: list = []
        self.pre_drive_cycle_observers: list = []

    def set_event_sink(self, sink) -> None:
        self.event_sink = sink
        self.uncache_agent.event_sink = sink

    def register_cycle_observer(self, observer) -> None:
        self.cycle_observers.append(observer)

    def register_pre_drive_cycle_observer(self, observer) -> None:
        self.pre_drive_cycle_observers.append(observer)

    def drive_uncache_edge(self, cycle: int) -> None:
        for observer in self.pre_drive_cycle_observers:
            if getattr(observer, "__name__", "") == "observe_uncache_a_edge":
                observer(cycle, self)
        self.uncache_agent.on_clock_edge(cycle)


def _coordinator_program_map() -> dict:
    return {
        "binary_text_verified": True,
        "binary_sha256": "program-binary-sha256",
        "instructions": [
            {
                "pc": 0x8000003E,
                "size": 4,
                "episode_id": "boundary-episode",
                "crosses_fetch_block": True,
                "crosses_cacheline": True,
                "crosses_page": False,
                "conditional_branch": False,
                "call": False,
                "return": False,
            }
        ],
    }


class _FakeDutSignal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


def _fake_bin951_dut():
    dut = type("FakeBin951Dut", (), {})()
    for name in BIN951_REDIRECT_SIGNAL_NAMES.values():
        setattr(dut, name, _FakeDutSignal())
    for name in BIN951_IFU_SIGNAL_NAMES.values():
        setattr(dut, name, _FakeDutSignal())
    for name in BIN951_FTQ_SIGNAL_NAMES.values():
        setattr(dut, name, _FakeDutSignal())
    return dut


def _completed_bin951_event():
    env = _FakeCoordinatorEnv(None)
    env.dut = _fake_bin951_dut()
    plan = TimingPlan.from_seed(19, "ibuffer_pressure_cross")
    coordinator = ScenarioCoordinator(
        scenario_id="bin951-contract",
        timing_plan=plan,
        program_map=_coordinator_program_map(),
    )
    coordinator._bin951_runtime_probe_status = "RAW_FST_CONFIRMED"
    coordinator.install(env)

    def drive(group: dict, values: dict[str, int]) -> None:
        for key, value in values.items():
            getattr(env.dut, group[key]).value = int(value)

    drive(BIN951_IFU_SIGNAL_NAMES, {
        "s0_prev_end_is_half_rvi": 1,
        "s1_half_valid": 1,
        "s1_half_data": 0x0013,
        "s1_half_pc": 0x8000003E >> 1,
        "s1_enqueue_ptr": 7,
        "s1_valid": 1,
        "s2_valid": 1,
    })
    coordinator.observe_cycle(10, env)
    drive(BIN951_REDIRECT_SIGNAL_NAMES, {
        "valid": 1,
        "pc": 0x8000003E,
        "target": 0x80000400,
        "ftq_flag": 0,
        "ftq_value": 5,
        "ftq_offset": 3,
    })
    drive(BIN951_IFU_SIGNAL_NAMES, {
        "ifu_redirect_valid": 1,
        "s0_flush": 1,
        "s1_flush": 1,
        "s2_flush": 1,
    })
    drive(BIN951_FTQ_SIGNAL_NAMES, {
        "ftq_redirect_valid": 1,
        "ftq_redirect_flag": 0,
        "ftq_redirect_value": 5,
        "ftq_redirect_offset": 3,
        "ftq_redirect_target": 0x80000400,
    })
    coordinator.observe_bin951_redirect_edge(11, env)
    coordinator.observe_cycle(11, env)
    drive(BIN951_REDIRECT_SIGNAL_NAMES, {"valid": 0})
    drive(BIN951_IFU_SIGNAL_NAMES, {
        "ifu_redirect_valid": 0,
        "s0_prev_end_is_half_rvi": 0,
        "s1_half_valid": 0,
        "s1_valid": 0,
        "s2_valid": 0,
        "to_ibuffer_valid": 0,
        "s0_fire": 1,
    })
    coordinator.observe_cycle(12, env)
    drive(BIN951_IFU_SIGNAL_NAMES, {
        "s0_fire": 0,
        "s1_valid": 1,
        "s1_fetch_block0_start_pc": 0x80000400 >> 1,
    })
    coordinator.observe_cycle(13, env)
    drive(BIN951_IFU_SIGNAL_NAMES, {
        "s2_aligned_pc0": 0x80000400 >> 1,
        "to_ibuffer_valid": 1,
        "to_ibuffer_enq0": 1,
        "to_ibuffer_instr0": 0x00000013,
        "to_ibuffer_foldpc0": fold_pc(0x80000400),
    })
    coordinator.observe_cycle(14, env)
    return coordinator, coordinator.build_timeline(execution_status="REALIZED")


def test_bin951_event_contract_correlates_redirect_half_cleanup_and_recovery():
    coordinator, timeline = _completed_bin951_event()
    contract = timeline["bin951_event_contract"]
    assert contract["schema_version"] == 2
    event = contract["sampled_events"][0]
    assert event["redirect"] == {
        "valid": 1,
        "pc": 0x8000003E,
        "target": 0x80000400,
        "ftq_flag": 0,
        "ftq_value": 5,
        "ftq_offset": 3,
    }
    assert event["summary"]["pre_redirect_half_present"] is True
    assert event["summary"]["old_payload_suppressed_until_recovery"] is True
    assert event["summary"]["recovery_target_match"] is True
    assert event["summary"]["source_program_map_matches"] is True
    assert event["summary"]["half_program_map_matches"] is True
    assert event["status"] == "REALIZED"
    assert event["validation_failures"] == []
    assert not event["missing_fields"]
    realized = {item.event: item.count for item in coordinator.realized_events()}
    assert realized["dut.bin951_redirect_half_cleanup"] == 1


@pytest.mark.parametrize("path, reason", (
    (("redirect", "ftq_value"), "redirect.ftq_value_missing"),
    (("pre_redirect_half_state", 0, "s1_half_data"), "pre_redirect_half.s1_half_data_missing"),
    (("redirect_state", "s1_flush"), "redirect_state.s1_flush_missing_or_low"),
    (("redirect_state", "ftq_redirect_value"), "ftq_redirect_value_missing_or_mismatched"),
    (("post_redirect_samples", 0, "s2_valid"), "post_redirect.s2_valid_missing_or_active"),
    (("post_redirect_samples", 1, "s1_fetch_block0_start_pc"), "recovery_target_missing_or_mismatched"),
    (("post_redirect_samples", 2, "to_ibuffer_foldpc0"), "recovery_ibuffer_identity_missing_or_mismatched"),
))
def test_bin951_missing_or_wrong_evidence_is_unconfirmed(path, reason):
    _, timeline = _completed_bin951_event()
    event = copy.deepcopy(timeline["bin951_event_contract"]["sampled_events"][0])
    value = event
    for key in path[:-1]:
        value = value[key]
    value[path[-1]] = None
    assert reason in validate_bin951_event(event)

    coordinator = ScenarioCoordinator(
        scenario_id="bin951-invalid",
        timing_plan=TimingPlan.from_seed(19, "ibuffer_pressure_cross"),
        program_map=_coordinator_program_map(),
    )
    coordinator._bin951_active = event
    coordinator._finish_bin951_event()
    invalid = coordinator.build_timeline(execution_status="REALIZED")
    assert invalid["bin951_event_contract"]["sampled_events"][0]["status"] == "UNCONFIRMED"
    assert not any(item.event == "dut.bin951_redirect_half_cleanup" for item in coordinator.realized_events())


def test_bin951_requires_pre_drive_redirect_identity():
    env = _FakeCoordinatorEnv(None)
    env.dut = _fake_bin951_dut()
    coordinator = ScenarioCoordinator(
        scenario_id="bin951-no-edge",
        timing_plan=TimingPlan.from_seed(19, "ibuffer_pressure_cross"),
        program_map=_coordinator_program_map(),
    )
    coordinator.install(env)
    getattr(env.dut, BIN951_REDIRECT_SIGNAL_NAMES["valid"]).value = 1
    coordinator.observe_cycle(11, env)
    assert coordinator.build_timeline(execution_status="REALIZED")["bin951_event_contract"]["sampled_events"] == []
    assert not any(item.event == "dut.bin951_redirect_half_cleanup" for item in coordinator.realized_events())


def test_bin951_event_contract_preserves_missing_probes_as_null():
    env = _FakeCoordinatorEnv(None)
    env.dut = _fake_bin951_dut()
    delattr(env.dut, BIN951_IFU_SIGNAL_NAMES["s1_half_data"])
    coordinator = ScenarioCoordinator(
        scenario_id="bin951-missing-probe",
        timing_plan=TimingPlan.from_seed(23, "ibuffer_pressure_cross"),
        program_map=_coordinator_program_map(),
    )
    coordinator.install(env)
    getattr(env.dut, BIN951_REDIRECT_SIGNAL_NAMES["valid"]).value = 1
    getattr(env.dut, BIN951_REDIRECT_SIGNAL_NAMES["pc"]).value = 0x8000003E
    getattr(env.dut, BIN951_IFU_SIGNAL_NAMES["s1_half_valid"]).value = 1
    coordinator.observe_bin951_redirect_edge(4, env)
    coordinator.observe_cycle(4, env)

    timeline = coordinator.build_timeline(execution_status="REALIZED")
    active = timeline["bin951_event_contract"]["active_event_at_end"]
    assert active["redirect"]["pc"] == 0x8000003E
    assert active["redirect_state"]["s1_half_data"] is None
    assert "redirect_state.s1_half_data" in active["missing_fields"]
    assert active["status"] == "PENDING"
    assert not any(item.event == "dut.bin951_redirect_half_cleanup" for item in coordinator.realized_events())


def _uncache_stall_coordinator(stall_cycles: int = 8):
    env = _FakeCoordinatorEnv(None)
    plan = TimingPlan.from_dict(
        {
            **TimingPlan.from_seed(7, "translation_timing_cross").to_dict(),
            "uncache_a_stall_cycles": stall_cycles,
        }
    )
    coordinator = ScenarioCoordinator(
        scenario_id="uncache-stall",
        timing_plan=plan,
        program_map=_coordinator_program_map(),
    )
    coordinator.install(env)
    env.uncache_if.a_valid.value = 1
    env.uncache_if.a_bits_address.value = 0x80403FF8
    env.uncache_if.a_bits_user_mem_back_type_mm.value = 1
    env.uncache_if.a_bits_user_mem_page_type_nc.value = 1
    return coordinator, env


def test_timing_plan_default_serialization_and_hash_remain_compatible() -> None:
    payload = TimingPlan.from_seed(7).to_dict()

    assert "uncache_a_stall_cycles" not in payload
    assert hashlib.sha256(
        json.dumps(payload, sort_keys=True, separators=(",", ":")).encode()
    ).hexdigest() == "972f876591d1eb40347a871b13b090a29afd64596035a0d27d6bea7582b36b9e"
    assert TimingPlan.from_dict(payload).uncache_a_stall_cycles == 0


def test_uncache_a_stall_holds_payload_for_eight_cycles_then_requires_fire() -> None:
    coordinator, env = _uncache_stall_coordinator()

    env.drive_uncache_edge(10)
    for cycle in range(11, 19):
        env.drive_uncache_edge(cycle)

    action = coordinator.build_timeline(execution_status="REALIZED")["actions"][0]
    realized = {event.event: event.count for event in coordinator.realized_events()}
    assert action["status"] == "REALIZED"
    assert action["confirmed_low_cycles"] == 8
    assert action["release_cycle"] == 18
    assert action["completion_cycle"] == 18
    assert realized["dut.uncache_a_stall_cycle"] == 8
    assert realized["dut.uncache_a_stall_release"] == 1
    assert env.uncache_agent.overrides == [0, None, None]


def test_uncache_a_stall_is_not_realized_until_release_handshake() -> None:
    coordinator, env = _uncache_stall_coordinator(stall_cycles=2)

    coordinator.observe_uncache_a_edge(10, env)
    env.uncache_if.a_ready.value = 0
    coordinator.observe_uncache_a_edge(11, env)
    coordinator.observe_uncache_a_edge(12, env)

    assert coordinator.has_active_action is True
    assert coordinator._uncache_stall_action["status"] == "RELEASE_PENDING"
    assert "dut.uncache_a_stall_release" not in {
        event.event for event in coordinator.realized_events()
    }
    env.uncache_agent.on_clock_edge(12)
    assert coordinator.has_active_action is False
    assert coordinator._uncache_stall_action["status"] == "REALIZED"


@pytest.mark.parametrize(
    ("field", "value"),
    (
        ("a_bits_address", 0x80404000),
        ("a_bits_user_mem_back_type_mm", 0),
        ("a_bits_user_mem_page_type_nc", 0),
    ),
)
def test_uncache_a_stall_fails_closed_when_payload_changes(field: str, value: int) -> None:
    coordinator, env = _uncache_stall_coordinator()
    env.drive_uncache_edge(10)
    getattr(env.uncache_if, field).value = value
    env.drive_uncache_edge(11)

    action = coordinator.build_timeline(execution_status="FAIL")["actions"][0]
    assert action["status"] == "FAIL"
    assert action["failure_reason"] == "uncache_a_payload_changed_while_stalled"
    assert coordinator.unrealized_action_ids == ("uncache-a-stall-000",)


def test_uncache_a_stall_fails_closed_when_valid_withdraws() -> None:
    coordinator, env = _uncache_stall_coordinator()
    env.drive_uncache_edge(10)
    env.uncache_if.a_valid.value = 0
    env.drive_uncache_edge(11)

    action = coordinator.build_timeline(execution_status="FAIL")["actions"][0]
    assert action["status"] == "FAIL"
    assert action["failure_reason"] == "uncache_a_valid_deasserted_before_release"
    assert "dut.uncache_a_stall_release" not in {
        event.event for event in coordinator.realized_events()
    }


def test_scenario_coordinator_records_realized_top_level_timing_actions(tmp_path: Path) -> None:
    forwarded_events: list[dict] = []
    env = _FakeCoordinatorEnv(forwarded_events.append)
    plan = TimingPlan(
        timing_seed=0x1234,
        icache_seed=0x5678,
        icache_hit_latency=2,
        icache_miss_latency=12,
        icache_miss_rate=0.1,
        warmup_response_lines=1,
        periodic_response_stride=19,
        maximum_backpressure_windows=1,
        backpressure_durations=(2,),
    )
    program_map = _coordinator_program_map()
    coordinator = ScenarioCoordinator(
        scenario_id="cfg-000",
        timing_plan=plan,
        program_map=program_map,
    )
    coordinator.install(env)

    request = {
        "type": "handshake.icache_a",
        "source": "icache_agent",
        "cycle": 9,
        "payload": {"address": 0x80000000},
    }
    pbmte_control = {
        "type": "control.translation_pbmte",
        "source": "frontend_env",
        "cycle": 9,
        "payload": {
            "requested": {"machine": 1, "hypervisor": None},
            "driven": {"machine": 1},
            "unsupported": [],
            "supported": True,
        },
    }
    response = {
        "type": "handshake.icache_d",
        "source": "icache_agent",
        "cycle": 10,
        "payload": {"address": 0x80000000, "beat_idx": 0},
    }
    env.event_sink(request)
    env.event_sink(pbmte_control)
    env.event_sink(response)
    coordinator.observe_cycle(10, env)
    coordinator.observe_cycle(11, env)

    assert forwarded_events == [request, pbmte_control, response]
    assert env.icache_agent.configuration == {
        "hit_latency": 2,
        "miss_latency": 12,
        "miss_rate": 0.1,
        "seed": 0x5678,
    }
    assert env.backend_model.values == [0, 1]
    assert coordinator.action_count == 1
    assert coordinator.has_active_action is False
    assert coordinator.unrealized_action_ids == ()
    realized = {event.event: event.count for event in coordinator.realized_events()}
    assert realized["dut.icache_request"] == 1
    assert realized["dut.icache_response"] == 1
    assert realized["dut.backend_backpressure_cycle"] == 2
    assert realized["dut.backend_backpressure_window"] == 1
    assert realized["dut.icache_response_x_backend_backpressure"] == 1

    timeline_path = tmp_path / "timeline.json"
    timeline = coordinator.write_timeline(
        timeline_path,
        execution_status="REALIZED",
        details={"monitor_errors": 0},
    )
    scenario = ScenarioSpec(
        scenario_id="cfg-000",
        family="cfg_layout_mix",
        ordinal=0,
        program_seed=1,
        timing_seed=plan.timing_seed,
        risk_tags=("boundary",),
        parameters={"timing_plan": plan.to_dict()},
    )
    loaded, loaded_events, status, reasons = _load_dut_timeline(
        timeline_path,
        scenario,
        program_map,
    )
    assert timeline["actions"][0]["status"] == "REALIZED"
    assert timeline["event_counts"]["control.translation_pbmte"] == 1
    assert any(
        event["type"] == "control.translation_pbmte"
        and event["payload"]["driven"] == {"machine": 1}
        for event in timeline["timeline"]
    )
    assert loaded["scenario_id"] == "cfg-000"
    assert loaded_events
    assert status == "valid"
    assert reasons == []


def test_scenario_coordinator_marks_unfinished_action_as_failed() -> None:
    env = _FakeCoordinatorEnv(None)
    plan = TimingPlan(
        timing_seed=1,
        icache_seed=2,
        icache_hit_latency=1,
        icache_miss_latency=12,
        icache_miss_rate=0.0,
        warmup_response_lines=1,
        periodic_response_stride=13,
        maximum_backpressure_windows=1,
        backpressure_durations=(4,),
    )
    coordinator = ScenarioCoordinator(
        scenario_id="unfinished",
        timing_plan=plan,
        program_map=_coordinator_program_map(),
    )
    coordinator.install(env)
    env.event_sink(
        {
            "type": "handshake.icache_d",
            "source": "icache_agent",
            "cycle": 20,
            "payload": {"address": 0x80000000, "beat_idx": 0},
        }
    )

    assert coordinator.has_active_action is True
    timeline = coordinator.build_timeline(execution_status="FAIL")

    assert timeline["maximum_timeline_events"] == 20_000
    assert timeline["actions"][0]["status"] == "FAIL"
    assert timeline["actions"][0]["failure_reason"] == "run_ended_before_window_completed"
    assert coordinator.has_active_action is False
    assert coordinator.unrealized_action_ids == ("backend-backpressure-000",)


def test_timeline_loader_rejects_malformed_action_without_raising(tmp_path: Path) -> None:
    program_map = _coordinator_program_map()
    plan = TimingPlan.from_seed(7)
    scenario = ScenarioSpec(
        scenario_id="malformed-action",
        family="cfg_layout_mix",
        ordinal=0,
        program_seed=1,
        timing_seed=plan.timing_seed,
        risk_tags=("boundary",),
        parameters={"timing_plan": plan.to_dict()},
    )
    timeline_path = tmp_path / "timeline.json"
    timeline_path.write_text(
        json.dumps(
            {
                "schema_version": 1,
                "scenario_id": scenario.scenario_id,
                "execution_status": "REALIZED",
                "timing_plan": plan.to_dict(),
                "program_map_binary_sha256": program_map["binary_sha256"],
                "actions": ["truncated-action"],
                "details": {"monitor_errors": 0},
                "realized_events": [],
            }
        ),
        encoding="utf-8",
    )

    _, events, status, reasons = _load_dut_timeline(
        timeline_path,
        scenario,
        program_map,
    )

    assert events == ()
    assert status == "invalid"
    assert reasons == [
        "scenario_timeline_incomplete_actions=index-0-not-an-object"
    ]


def test_timeline_loader_rejects_dropped_events_and_oracle_errors(tmp_path: Path) -> None:
    program_map = _coordinator_program_map()
    plan = TimingPlan.from_seed(7)
    scenario = ScenarioSpec(
        scenario_id="invalid-evidence",
        family="translation_timing_cross",
        ordinal=0,
        program_seed=1,
        timing_seed=plan.timing_seed,
        risk_tags=("translation",),
        parameters={"timing_plan": plan.to_dict()},
    )
    timeline_path = tmp_path / "timeline.json"
    timeline_path.write_text(
        json.dumps(
            {
                "schema_version": 1,
                "scenario_id": scenario.scenario_id,
                "execution_status": "REALIZED",
                "timing_plan": plan.to_dict(),
                "program_map_binary_sha256": program_map["binary_sha256"],
                "actions": [],
                "dropped_timeline_events": 2,
                "details": {
                    "monitor_errors": 0,
                    "translation_oracle_error_count": 1,
                },
                "realized_events": [],
            }
        ),
        encoding="utf-8",
    )

    _, events, status, reasons = _load_dut_timeline(
        timeline_path, scenario, program_map
    )

    assert events == ()
    assert status == "invalid"
    assert "scenario_timeline_translation_oracle_errors=1" in reasons
    assert "scenario_timeline_dropped_events=2" in reasons


def test_funcov_case_evidence_records_dynamic_many_to_many_hits(tmp_path: Path) -> None:
    artifact_path = tmp_path / "case.funcov.json"
    _write_funcov(
        artifact_path,
        signature="fixed-contract",
        run_id="random-case",
        hit_bin_ids=("BIN-001", "BIN-003"),
    )

    evidence = _load_funcov_evidence([artifact_path])

    assert evidence["status"] == "eligible"
    assert evidence["observed_bin_ids"] == ["BIN-001", "BIN-003"]
    assert evidence["compatibility_contract"] == {
        "compatibility_signature": "fixed-contract",
        "definitions_sha256": "definitions",
        "definition_bin_count": 4,
    }
    assert evidence["eligible_artifact_paths"] == [str(artifact_path.resolve())]


def test_funcov_evidence_rejects_translation_oracle_errors(tmp_path: Path) -> None:
    artifact_path = tmp_path / "translation.funcov.json"
    _write_funcov(
        artifact_path,
        signature="fixed-contract",
        run_id="translation-case",
        hit_bin_ids=("BIN-001",),
    )
    payload = json.loads(artifact_path.read_text(encoding="utf-8"))
    payload["stats"] = {"translation_oracle": {"error_count": 1, "errors": [{"reason": "mismatch"}]}}
    artifact_path.write_text(json.dumps(payload), encoding="utf-8")

    evidence = _load_funcov_evidence([artifact_path])

    assert evidence["status"] == "ineligible"
    assert any(
        "translation_oracle_error_count=1" in reason
        for reason in evidence["reasons"]
    )


def test_funcov_evidence_rejects_unproved_speculation_despite_checker_pass(tmp_path: Path) -> None:
    artifact_path = tmp_path / "translation.funcov.json"
    _write_funcov(artifact_path, signature="fixed-contract", run_id="translation-case",
                  hit_bin_ids=("BIN-001",))
    payload = json.loads(artifact_path.read_text())
    payload["stats"] = {"translation_oracle": {
        "error_count": 0, "errors": [], "records": [
            {"kind": "cfvec_normal", "pc": 0x80000000},
            {"kind": "speculative_cfvec_exception", "pc": 0, "cycle": 10},
        ],
    }}
    artifact_path.write_text(json.dumps(payload))
    assert not load_artifact(artifact_path).eligible
    evidence = _load_funcov_evidence([artifact_path])
    assert evidence["status"] == "ineligible"
    assert evidence["eligible_artifact_paths"] == []
    assert evidence["observed_bin_ids"] == []
    assert any("unproved_speculative_cfvec_exceptions=1" in r for r in evidence["reasons"])


def test_case_realization_keeps_observed_funcov_bin_ids(tmp_path: Path) -> None:
    artifact_path = tmp_path / "case.funcov.json"
    _write_funcov(
        artifact_path,
        signature="fixed-contract",
        run_id="random-case",
        hit_bin_ids=("BIN-001", "BIN-003"),
    )
    evidence = _load_funcov_evidence([artifact_path])
    spec = ScenarioSpec(
        scenario_id="case",
        family="cfg_layout_mix",
        ordinal=0,
        program_seed=1,
        timing_seed=2,
        risk_tags=("control_flow",),
    )
    realized = evaluate_realization(
        spec,
        tuple(
            RealizedEvent(
                event="funcov.bin_hit",
                relation="bin_id",
                anchor=bin_id,
                count=1,
                evidence_source="compatible_functional_coverage_artifact",
            )
            for bin_id in evidence["observed_bin_ids"]
        ),
        evidence_stage="dut_program_plus_funcov",
        execution_status="completed",
        exit_code=0,
        observed_bin_ids=tuple(evidence["observed_bin_ids"]),
    )

    assert realized.observed_bin_ids == ("BIN-001", "BIN-003")
    assert [event.anchor for event in realized.realized_events] == ["BIN-001", "BIN-003"]

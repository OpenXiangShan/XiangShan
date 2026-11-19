from __future__ import annotations

import copy
import csv
import hashlib
import json
from pathlib import Path

import pytest

from env.support.pc_utils import fold_pc

from .generate_random_asm import CaseSpec, generate_case, write_cases
from .random_case_test_support import _FakeCoordinatorEnv, _coordinator_program_map, _fake_bin951_dut
from .scenario_contract import EventRequirement, RealizedEvent, ScenarioSpec, evaluate_realization
from .scenario_coordinator import (
    BIN951_FTQ_SIGNAL_NAMES, BIN951_IFU_SIGNAL_NAMES, BIN951_REDIRECT_SIGNAL_NAMES,
    ScenarioCoordinator, TimingPlan, validate_bin951_event,
)
from .scenario_profiles import build_scenario_spec
from .testpoint_catalog import DEFAULT_SOURCE, build_catalog, write_catalog


def test_history4_scenario_requires_observed_four_target_sites() -> None:
    catalog = build_catalog(DEFAULT_SOURCE)
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
                                   testpoint_catalog=build_catalog(DEFAULT_SOURCE))
    requirement = next(e for e in scenario.required_events if e.event == "trace.indirect_four_target_page_site")
    assert requirement.min_count == len(episodes)
    assert requirement.evidence_stage == "program"
    assert scenario.parameters["translation"]["permission_layout"] == "exact_napot_v1"
    _, old_record = generate_case(base)
    old_scenario = build_scenario_spec(old_record, timing_seed=11,
                                       testpoint_catalog=build_catalog(DEFAULT_SOURCE))
    assert "permission_layout" not in old_scenario.parameters["translation"]


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

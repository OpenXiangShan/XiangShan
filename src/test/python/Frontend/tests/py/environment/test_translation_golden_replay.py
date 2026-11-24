from __future__ import annotations

from copy import copy
from types import SimpleNamespace

import pytest

from env.core.frontend_env import FrontendEnv
from env.model.golden_trace import GoldenTrace, TraceEntry
from env.runtime.dut_factory import FakeDUTFrontend
from env.sequences import TranslationPte, TranslationScenario, TranslationScenarioBuilder


def _replay(*, pbmt=0):
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    state = TranslationScenarioBuilder(env).build(TranslationScenario(
        scenario_id="speculative-translation-replay", va=0x80000000, pa=0x80400000,
        payload=b"\x13\x00\x00\x00" * 33, s1_pte=TranslationPte(pbmt=pbmt),
    ))
    env.arm_translation_scenario(state)
    trace = GoldenTrace([
        TraceEntry(index=0, pc=0x80000000, instr=0x13, size=4),
        TraceEntry(index=1, pc=0x80000080, instr=0x13, size=4),
    ])
    env.backend_model.set_golden_trace(trace)
    env.translation_checker.bind_golden_replay(trace)
    request = {key: state.expected_ptw_request[key] for key in ("vpn", "s2xlate", "get_gpa")}
    checker = env.translation_checker
    checker.observe_ptw_request(10, **request)
    checker.observe_ptw_response(11, **request, response=env.page_table.build_ptw_resp(**request))
    for pa in (0x80400000, 0x80400080):
        checker.observe_fetch_request(12, path="icache" if pbmt == 0 else "uncache", pa=pa)
    env._last_run_until_golden_result = SimpleNamespace(
        status="completed", completed=True, cursor=2, total_entries=2,
        pending_work=0, monitor_error_count=0,
    )
    return env, checker, request


def _fault(env, *, pc=0, cycle=20):
    model = env.backend_model
    model.current_cycle = cycle
    index = model._append_cfvec_queue_instr(
        slot=0, pc=pc, instr=0, is_rvc=False, pred_taken=False,
        ftq_flag=0, ftq_value=7, ftq_offset=1, is_last=True,
        exception_bits=1 << 12,
    )
    return model._cfvec_queue[index]


def test_replay_requires_executed_blocks_but_not_skipped_binary_padding():
    env, checker, _ = _replay()
    assert len(checker.active["expected_fetches"]) == 3
    assert len(checker.active["required_fetches"]) == 2
    assert env.assert_translation_scenario()["error_count"] == 0
    checker.active["observed_fetch_pas"].remove(0x80400080)
    with pytest.raises(AssertionError, match="fetch_block"):
        env.assert_translation_scenario()


@pytest.mark.parametrize("vpn", [0, 0x12345])
def test_background_ptw_after_target_response_is_not_an_architectural_mismatch(vpn):
    env, checker, request = _replay()
    background = {**request, "vpn": vpn}
    checker.observe_ptw_request(20, **background)
    checker.observe_ptw_response(24, **background, response=env.page_table.build_ptw_resp(**background))
    assert env.assert_translation_scenario()["error_count"] == 0
    assert any(r["kind"] == "background_ptw_request" and r["actual"]["vpn"] == vpn for r in checker.records)
    assert checker.records[-1]["kind"] == "background_ptw_response"
    assert checker.active["requested_ptw_vpns"] == [request["vpn"]]
    assert checker._ptw_requests == []


@pytest.mark.parametrize("field", ["vpn", "s2xlate", "get_gpa"])
def test_background_response_must_match_a_real_request(field):
    env, checker, request = _replay()
    background = {**request, "vpn": 0}
    checker.observe_ptw_request(20, **background)
    wrong = {**background, field: background[field] + 1}
    checker.observe_ptw_response(24, **wrong, response={})
    with pytest.raises(AssertionError, match="ptw_response_context_mismatch"):
        env.assert_translation_scenario()


@pytest.mark.parametrize("field", ["s2xlate", "get_gpa"])
def test_same_page_with_wrong_translation_context_still_fails(field):
    env, checker, request = _replay()
    checker.observe_ptw_request(20, **{**request, field: request[field] + 1})
    with pytest.raises(AssertionError, match="ptw_request_mismatch"):
        env.assert_translation_scenario()


def test_actual_control_flow_flush_resolves_the_exact_fault_queue_instance():
    env, checker, _ = _replay()
    model = env.backend_model
    fault = _fault(env)
    model._set_active_wrong_path_episode(
        origin_index=0, target_pc=0x80000080,
        redirect_context={"branch_type": 3, "pc": 0x80000000,
                          "ftq_flag": 0, "ftq_value": 6, "ftq_offset": 1},
        redirect_driven=True, redirect_driven_cycle=22,
    )
    model.current_cycle = 23
    model._cfvec_queue_flush_wrong_path()
    assert env.assert_translation_scenario()["error_count"] == 0
    proof = next(r for r in checker.records if r["kind"] == "squashed_cfvec_exception")
    assert proof["observed_cycle"] == fault.cycle
    assert proof["ftq_value"] == fault.ftq_value


@pytest.mark.parametrize("proof", ["none", "other_instance", "fault_retry", "satp_flush", "memory_violation"])
def test_fault_requires_instance_specific_control_flow_squash(proof):
    env, checker, _ = _replay()
    fault = _fault(env)
    redirect = {"branch_type": 3, "pc": 0x80000000,
                "ftq_flag": 0, "ftq_value": 6, "ftq_offset": 1}
    if proof == "other_instance":
        checker.observe_backend_squash(25, [copy(fault)], redirect=redirect)
    elif proof == "fault_retry":
        checker.observe_backend_squash(25, [fault], redirect={**redirect, "backend_ipf": 1})
    elif proof == "memory_violation":
        checker.observe_backend_squash(25, [fault], redirect={**redirect, "redirect_class": "memory_violation"})
    elif proof == "satp_flush":
        checker.observe_backend_squash(25, [fault], redirect={**redirect, "satp_flush": 1})
    with pytest.raises(AssertionError, match="unresolved_cfvec_exception"):
        env.assert_translation_scenario()


def test_fault_on_a_golden_page_does_not_pass_with_only_completed_pc_replay():
    env, _, _ = _replay()
    _fault(env, pc=0x80000000)
    with pytest.raises(AssertionError, match="unresolved_cfvec_exception"):
        env.assert_translation_scenario()


@pytest.mark.parametrize("field,value", [("completed", False), ("cursor", 1),
    ("pending_work", 1), ("monitor_error_count", 1)])
def test_background_policy_cannot_pass_an_incomplete_or_failed_replay(field, value):
    env, _, _ = _replay()
    setattr(env._last_run_until_golden_result, field, value)
    with pytest.raises(AssertionError, match="golden_replay_completion"):
        env.assert_translation_scenario()


@pytest.mark.parametrize("pbmt", [1, 2])
def test_replay_tracks_each_executed_uncache_beat(pbmt):
    env, checker, _ = _replay(pbmt=pbmt)
    assert {item["pa"] for item in checker.active["required_fetches"]} == {0x80400000, 0x80400080}
    assert env.assert_translation_scenario()["error_count"] == 0
    checker.active["observed_fetch_pas"].remove(0x80400080)
    with pytest.raises(AssertionError, match="fetch_block"):
        env.assert_translation_scenario()


def test_background_traffic_cannot_replace_required_scenario_ptw_evidence():
    env, checker, request = _replay()
    checker.active["requested_ptw_request_keys"].clear()
    background = {**request, "vpn": 0}
    checker.observe_ptw_request(20, **background)
    checker.observe_ptw_response(24, **background, response={})
    with pytest.raises(AssertionError, match="ptw_request"):
        env.assert_translation_scenario()


def test_directed_scenario_still_rejects_extra_ptw_requests():
    env, checker, request = _replay()
    state = TranslationScenarioBuilder(env).build(TranslationScenario(
        scenario_id="strict-directed", va=0x80000000, pa=0x80400000,
        payload=b"\x13\x00\x00\x00",
    ))
    env.arm_translation_scenario(state)
    checker.observe_ptw_request(10, **request)
    checker.observe_ptw_response(11, **request, response=env.page_table.build_ptw_resp(**request))
    checker.observe_ptw_request(20, **{**request, "vpn": 0})
    with pytest.raises(AssertionError, match="ptw_request_mismatch"):
        env.assert_translation_scenario()


def test_cross_page_instruction_requires_both_translated_fetch_lines():
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    state = TranslationScenarioBuilder(env).build(TranslationScenario(
        scenario_id="golden-cross-page", va=0x80000FFE, pa=0x80400FFE,
        payload=b"\x13\x00\x00\x00", page_count=2,
    ))
    env.arm_translation_scenario(state)
    trace = GoldenTrace([TraceEntry(index=0, pc=0x80000FFE, instr=0x13, size=4)])
    env.translation_checker.bind_golden_replay(trace)
    assert {item["pa"] for item in env.translation_checker.active["required_fetches"]} == {
        0x80400FC0, 0x80401000,
    }
    assert len(env.translation_checker.active["required_ptw_request_keys"]) == 2


def test_replay_timeline_preserves_request_and_exact_squash_evidence():
    from tests.random_case.scenario_coordinator import ScenarioCoordinator, TimingPlan

    env, checker, request = _replay()
    coordinator = ScenarioCoordinator(
        scenario_id="speculative-translation-replay", timing_plan=TimingPlan.from_seed(1),
        program_map={"binary_text_verified": True, "instructions": []},
    )
    checker.set_event_sink(coordinator.handle_event)
    background = {**request, "vpn": 0}
    checker.observe_ptw_request(20, **background)
    checker.observe_ptw_response(24, **background, response={})
    fault = _fault(env, cycle=25)
    checker.observe_backend_squash(30, [fault], redirect={
        "branch_type": 3, "pc": 0x80000000,
        "ftq_flag": 0, "ftq_value": 6, "ftq_offset": 1,
    })
    rows = coordinator.build_timeline(execution_status="REALIZED")["timeline"]
    assert {row["type"] for row in rows} >= {
        "translation.background_ptw_request", "translation.background_ptw_response",
        "translation.pending_cfvec_exception", "translation.squashed_cfvec_exception",
    }
    proof = next(row for row in rows if row["type"] == "translation.squashed_cfvec_exception")
    assert proof["payload"]["observed_cycle"] == 25
    assert proof["payload"]["ftq_value"] == 7
    assert proof["payload"]["redirect"]["ftq_value"] == 6

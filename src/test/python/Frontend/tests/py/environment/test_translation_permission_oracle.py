from __future__ import annotations

import pytest

from env.core.frontend_env import FrontendEnv
from env.runtime.dut_factory import FakeDUTFrontend, FakeSignal
from env.sequences import (
    TranslationPermissionProbe,
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationPtwResponseOverride,
    TranslationScenario,
    TranslationScenarioBuilder,
    TranslationScenarioSequence,
    TranslationSectorLane,
)
from env.support import PmpPmaConfig, fold_pc


_ITLB_PTW_REQ_GET_GPA = "Frontend_top.Frontend.inner_itlb.io_ptw_req_0_bits_getGpa"


class _PmpProbeDut(FakeDUTFrontend):
    _is_fake_frontend_dut = False

    def __init__(self) -> None:
        super().__init__()
        self._frontend_is_fake_dut = False
        self._internal_signals = {}

    def GetInternalSignal(self, name: str):
        return self._internal_signals.get(name)


def _state(*, scenario_id: str, va: int, pa: int, pte: TranslationPte):
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    state = TranslationScenarioBuilder(env).build(
        TranslationScenario(
            scenario_id=scenario_id,
            va=va,
            pa=pa,
            payload=b"\x13\x00\x00\x00",
            s1_pte=pte,
            expected_path="fault" if not pte.x else "cacheable",
            expected_result="page_fault" if not pte.x else "normal",
        )
    )
    env.arm_translation_scenario(state)
    return env, state


def _observe_matching_ptw(env: FrontendEnv, state, cycle: int = 10) -> None:
    request = state.expected_ptw_request
    response = env.page_table.build_ptw_resp(
        request["vpn"], s2xlate=request["s2xlate"], get_gpa=request["get_gpa"]
    )
    env.translation_oracle.observe_ptw_request(cycle, **{key: request[key] for key in ("vpn", "s2xlate", "get_gpa")})
    env.translation_oracle.observe_ptw_response(cycle + 1, **{key: request[key] for key in ("vpn", "s2xlate", "get_gpa")}, response=response)


def test_oracle_accepts_cacheable_pa_from_icache_request() -> None:
    env, state = _state(
        scenario_id="oracle-cacheable",
        va=0x8020_0004,
        pa=0x8040_0004,
        pte=TranslationPte(v=1, r=1, x=1, a=1),
    )

    _observe_matching_ptw(env, state)
    env.translation_oracle.observe_fetch_request(12, path="icache", pa=state.expected_outcome["pa"] & ~0x3F)

    stats = env.assert_translation_scenario()
    assert stats["error_count"] == 0
    assert [record["kind"] for record in stats["records"]] == ["armed", "ptw_request", "ptw_response", "fetch_request"]
    assert all(record["scenario_id"] == "oracle-cacheable" for record in stats["records"])


def test_oracle_uses_first_normal_fetch_as_translation_evidence() -> None:
    env, state = _state(
        scenario_id="oracle-cacheable-stream",
        va=0x8020_0004,
        pa=0x8040_0004,
        pte=TranslationPte(v=1, r=1, x=1, a=1),
    )

    _observe_matching_ptw(env, state)
    first_pa = state.expected_outcome["pa"] & ~0x3F
    env.translation_oracle.observe_fetch_request(12, path="icache", pa=first_pa)
    env.translation_oracle.observe_fetch_request(13, path="icache", pa=first_pa + 0x40)

    stats = env.assert_translation_scenario()
    assert stats["error_count"] == 0
    assert [record["kind"] for record in stats["records"]] == ["armed", "ptw_request", "ptw_response", "fetch_request"]


def test_oracle_aligns_uncache_requests_to_the_eight_byte_beat() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    state = TranslationScenarioBuilder(env).build(
        TranslationScenario(
            scenario_id="oracle-uncache-beat-alignment",
            va=0x8020_0FFC,
            pa=0x8040_0FFC,
            payload=b"\x13\x00\x00\x00",
            s1_pte=TranslationPte(pbmt=1),
            expected_path="uncache",
        )
    )
    active = env.arm_translation_scenario(state)

    _observe_matching_ptw(env, state)
    assert active["expected_fetches"] == [
        {"page": 0, "vpn": 0x8020_0000 >> 12, "path": "uncache", "pa": 0x8040_0FF8}
    ]
    env.translation_oracle.observe_fetch_request(12, path="uncache", pa=0x8040_0FF8)

    assert env.assert_translation_scenario()["error_count"] == 0


def test_oracle_requires_each_cacheable_fetch_block_covered_by_the_payload() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    state = TranslationScenarioBuilder(env).build(
        TranslationScenario(
            scenario_id="oracle-two-fetch-blocks",
            va=0x8020_0040,
            pa=0x8040_0040,
            payload=b"\x13\x00\x00\x00" * 32,
        )
    )
    active = env.arm_translation_scenario(state)

    _observe_matching_ptw(env, state)
    assert len(active["expected_fetches"]) == 2
    env.translation_oracle.observe_fetch_request(12, path="icache", pa=active["expected_fetches"][0]["pa"])
    with pytest.raises(AssertionError, match="fetch_block"):
        env.assert_translation_scenario()

    env.translation_oracle.errors.clear()
    env.translation_oracle.observe_fetch_request(13, path="icache", pa=active["expected_fetches"][1]["pa"])
    assert env.assert_translation_scenario()["error_count"] == 0


def test_phase_completion_requires_normal_cfvec_from_each_selected_page() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    state = TranslationScenarioBuilder(env).build(
        TranslationScenario(
            scenario_id="oracle-phase-complete-cross-page-cfvec",
            va=0x8020_0F00,
            pa=0x8040_0F00,
            payload=b"\x13\x00\x00\x00" * 512,
            page_count=2,
        )
    )
    active = env.arm_translation_scenario(state)

    for cycle, request in enumerate(active["expected_ptw_requests"], start=10):
        response = env.page_table.build_ptw_resp(
            request["vpn"], s2xlate=request["s2xlate"], get_gpa=request["get_gpa"]
        )
        env.translation_oracle.observe_ptw_request(cycle * 2, **{key: request[key] for key in ("vpn", "s2xlate", "get_gpa")})
        env.translation_oracle.observe_ptw_response(
            cycle * 2 + 1,
            **{key: request[key] for key in ("vpn", "s2xlate", "get_gpa")},
            response=response,
        )
    for cycle, fetch in enumerate(active["expected_fetches"], start=100):
        env.translation_oracle.observe_fetch_request(cycle, path=fetch["path"], pa=fetch["pa"])

    first_page_va = int(state.expected_page_outcomes[0]["va"])
    second_page_va = int(state.expected_page_outcomes[1]["va"])
    env.translation_oracle.observe_cfvec(200, pc=first_page_va, exception_bits={})
    env.translation_oracle.observe_cfvec(201, pc=first_page_va + 4, exception_bits={})

    assert not TranslationScenarioSequence._phase_complete(env)
    assert env.translation_oracle.get_active()["observed_normal_cfvec_pages"] == [0]

    env.translation_oracle.observe_cfvec(202, pc=second_page_va, exception_bits={})

    assert TranslationScenarioSequence._phase_complete(env)
    assert env.translation_oracle.get_active()["observed_normal_cfvec_pages"] == [0, 1]


def test_oracle_expects_one_ptw_for_a_superpage_covering_two_payload_pages() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    state = TranslationScenarioBuilder(env).build(
        TranslationScenario(
            scenario_id="oracle-superpage-ptw-coverage",
            va=0x8020_0F00,
            pa=0x8040_0F00,
            payload=b"\x13\x00\x00\x00" * 512,
            page_count=2,
            s1_pte=TranslationPte(level=1),
        )
    )

    active = env.arm_translation_scenario(state)

    assert [request["vpn"] for request in active["expected_ptw_requests"]] == [state.expected_ptw_request["vpn"]]


def test_oracle_expects_one_ptw_for_sector_lanes_covering_two_payload_pages() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    state = TranslationScenarioBuilder(env).build(
        TranslationScenario(
            scenario_id="oracle-sector-ptw-coverage",
            va=0x8020_0F00,
            pa=0x8040_0F00,
            payload=b"\x13\x00\x00\x00" * 512,
            page_count=2,
            s1_sector_lanes=(TranslationSectorLane(lane=1, ppn=(0x8040_0000 >> 12) + 1),),
        )
    )

    active = env.arm_translation_scenario(state)

    assert [request["vpn"] for request in active["expected_ptw_requests"]] == [state.expected_ptw_request["vpn"]]


def test_oracle_disarm_keeps_completed_phase_history() -> None:
    env, state = _state(
        scenario_id="oracle-disarm",
        va=0x8020_0004,
        pa=0x8040_0004,
        pte=TranslationPte(v=1, r=1, x=1, a=1),
    )

    _observe_matching_ptw(env, state)
    env.translation_oracle.observe_fetch_request(12, path="icache", pa=state.expected_outcome["pa"] & ~0x3F)
    env.assert_translation_scenario()
    env.translation_oracle.disarm()

    stats = env.translation_oracle.get_stats()
    assert stats["active"] is None
    assert stats["records"][-1]["kind"] == "disarmed"
    assert stats["error_count"] == 0


def test_oracle_does_not_attribute_transactions_before_redirect_recovery() -> None:
    env, state = _state(
        scenario_id="oracle-pre-redirect",
        va=0x8020_1000,
        pa=0x8040_1000,
        pte=TranslationPte(v=1, r=1, x=0, a=1),
    )
    env.translation_oracle.set_fetch_observation_ready(ready=False)
    request = state.expected_ptw_request

    env.translation_oracle.observe_ptw_request(
        12,
        vpn=request["vpn"] + 1,
        s2xlate=request["s2xlate"],
        get_gpa=request["get_gpa"],
    )
    env.translation_oracle.observe_cfvec(12, pc=0, exception_bits={1: 1})

    stats = env.translation_oracle.get_stats()
    assert stats["error_count"] == 0
    assert [record["kind"] for record in stats["records"][-2:]] == [
        "pre_redirect_ptw_request",
        "pre_redirect_cfvec_exception",
    ]


def test_oracle_does_not_attribute_old_vpn_before_the_target_request() -> None:
    env, state = _state(
        scenario_id="oracle-pre-target",
        va=0x8020_1000,
        pa=0x8040_1000,
        pte=TranslationPte(v=1, r=1, x=0, a=1),
    )
    request = state.expected_ptw_request

    env.translation_oracle.observe_ptw_request(
        12,
        vpn=request["vpn"] + 1,
        s2xlate=request["s2xlate"],
        get_gpa=request["get_gpa"],
    )
    env.translation_oracle.observe_cfvec(12, pc=state.scenario.va + 0x1000, exception_bits={1: 1})

    stats = env.translation_oracle.get_stats()
    assert stats["error_count"] == 0
    assert [record["kind"] for record in stats["records"][-2:]] == [
        "pre_target_ptw_request",
        "pre_target_cfvec_exception",
    ]


def test_oracle_maps_generated_cfvec_page_fault_bit() -> None:
    env, state = _state(
        scenario_id="oracle-page-fault",
        va=0x8020_1000,
        pa=0x8040_1000,
        pte=TranslationPte(v=1, r=1, x=0, a=1),
    )

    _observe_matching_ptw(env, state)
    env.translation_oracle.observe_cfvec(12, pc=state.scenario.va, exception_bits={12: 1}, cross_page=False)

    assert env.assert_translation_scenario()["error_count"] == 0


def test_oracle_accepts_zero_exception_pc_with_matching_foldpc() -> None:
    env, state = _state(
        scenario_id="oracle-foldpc-fallback",
        va=0x8020_1FFC,
        pa=0x8040_1FFC,
        pte=TranslationPte(v=1, r=1, x=0, a=1),
    )

    _observe_matching_ptw(env, state)
    env.translation_oracle.observe_cfvec(
        12,
        pc=0,
        folded_pc=fold_pc(state.scenario.va),
        exception_bits={12: 1},
    )

    stats = env.assert_translation_scenario()
    assert stats["error_count"] == 0
    assert stats["records"][-1]["kind"] == "cfvec_exception_foldpc_match"


def test_oracle_rejects_zero_exception_pc_with_wrong_foldpc() -> None:
    env, state = _state(
        scenario_id="oracle-foldpc-mismatch",
        va=0x8020_1FFC,
        pa=0x8040_1FFC,
        pte=TranslationPte(v=1, r=1, x=0, a=1),
    )

    _observe_matching_ptw(env, state)
    env.translation_oracle.observe_cfvec(
        12,
        pc=0,
        folded_pc=fold_pc(state.scenario.va) ^ 1,
        exception_bits={12: 1},
    )

    with pytest.raises(AssertionError, match="cfvec_exception_pc_mismatch"):
        env.assert_translation_scenario()


def test_oracle_uses_first_expected_fault_as_exception_evidence() -> None:
    env, state = _state(
        scenario_id="oracle-page-fault-stream",
        va=0x8020_1000,
        pa=0x8040_1000,
        pte=TranslationPte(v=1, r=1, x=0, a=1),
    )

    _observe_matching_ptw(env, state)
    env.translation_oracle.observe_cfvec(12, pc=state.scenario.va, exception_bits={12: 1})
    env.translation_oracle.observe_cfvec(13, pc=state.scenario.va + 0x40, exception_bits={12: 1})

    stats = env.assert_translation_scenario()
    assert stats["error_count"] == 0
    assert [record["kind"] for record in stats["records"]] == ["armed", "ptw_request", "ptw_response", "cfvec_exception"]


def test_oracle_allows_only_declared_first_page_fetch_before_cross_page_fault() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    scenario = TranslationScenario(
        scenario_id="oracle-cross-page-second-fault",
        va=0x8020_0FFE,
        pa=0x8040_0FFE,
        payload=b"\x13\x00\x00\x00",
        page_count=2,
        s1_pte=TranslationPte(pbmt=1),
        expected_path="fault",
        expected_result="access_fault",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(
                    match="napot", read=True, write=True, execute=True
                ),
                addr=0x8040_0000,
                size=0x1000,
            ),
            TranslationPmpPmaEntry(
                kind="pmp",
                index=1,
                config=PmpPmaConfig(
                    match="napot", read=True, write=True, execute=False
                ),
                addr=0x8040_1000,
                size=0x1000,
            ),
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    active = env.arm_translation_scenario(state)
    for cycle, request in enumerate(active["expected_ptw_requests"], start=10):
        response = env.page_table.build_ptw_resp(
            request["vpn"],
            s2xlate=request["s2xlate"],
            get_gpa=request["get_gpa"],
        )
        request_fields = {
            key: request[key] for key in ("vpn", "s2xlate", "get_gpa")
        }
        env.translation_oracle.observe_ptw_request(cycle * 2, **request_fields)
        env.translation_oracle.observe_ptw_response(
            cycle * 2 + 1, **request_fields, response=response
        )

    assert active["expected_fetches"] == [
        {
            "page": 0,
            "vpn": 0x8020_0000 >> 12,
            "path": "uncache",
            "pa": 0x8040_0FF8,
        }
    ]
    env.translation_oracle.observe_fetch_request(
        30, path="uncache", pa=0x8040_0FF8
    )
    env.translation_oracle.observe_cfvec(
        31,
        pc=scenario.va,
        exception_bits={1: 1},
        cross_page=True,
    )
    assert env.assert_translation_scenario()["error_count"] == 0

    env.translation_oracle.observe_fetch_request(
        32, path="uncache", pa=0x8040_1000
    )
    with pytest.raises(AssertionError, match="unexpected_fetch_after_fault"):
        env.assert_translation_scenario()


def test_oracle_rejects_wrong_cfvec_fault_type_with_scenario_context() -> None:
    env, state = _state(
        scenario_id="oracle-fault-type",
        va=0x8020_2000,
        pa=0x8040_2000,
        pte=TranslationPte(v=1, r=1, x=0, a=1),
    )

    _observe_matching_ptw(env, state)
    env.translation_oracle.observe_cfvec(12, pc=state.scenario.va, exception_bits={1: 1})

    with pytest.raises(AssertionError, match="cfvec_exception_type_mismatch"):
        env.assert_translation_scenario()
    error = env.translation_oracle.get_stats()["errors"][0]
    assert error["scenario_id"] == "oracle-fault-type"
    assert error["vpn"] == state.scenario.va >> 12


def test_oracle_rejects_followup_ptw_request_after_fault() -> None:
    env, state = _state(
        scenario_id="oracle-no-followup",
        va=0x8020_2800,
        pa=0x8040_2800,
        pte=TranslationPte(v=1, r=1, x=0, a=1),
    )

    _observe_matching_ptw(env, state)
    env.translation_oracle.observe_cfvec(12, pc=state.scenario.va, exception_bits={12: 1})
    request = state.expected_ptw_request
    env.translation_oracle.observe_ptw_request(13, **{key: request[key] for key in ("vpn", "s2xlate", "get_gpa")})

    with pytest.raises(AssertionError, match="unexpected_followup_ptw_request"):
        env.assert_translation_scenario()


def test_oracle_marks_pre_fence_response_stale_in_new_epoch() -> None:
    env, old_state = _state(
        scenario_id="oracle-old-context",
        va=0x8020_3000,
        pa=0x8040_3000,
        pte=TranslationPte(v=1, r=1, x=1, a=1),
    )
    old_request = old_state.expected_ptw_request
    env.translation_oracle.observe_ptw_request(10, **{key: old_request[key] for key in ("vpn", "s2xlate", "get_gpa")})
    env.pulse_sfence()

    new_state = TranslationScenarioBuilder(env).build(
        TranslationScenario(
            scenario_id="oracle-new-context",
            va=0x8020_4000,
            pa=0x8040_4000,
            payload=b"\x13\x00\x00\x00",
        )
    )
    env.arm_translation_scenario(new_state)
    old_response = env.page_table.build_ptw_resp(old_request["vpn"])
    env.translation_oracle.observe_ptw_response(20, **{key: old_request[key] for key in ("vpn", "s2xlate", "get_gpa")}, response=old_response)
    new_request = new_state.expected_ptw_request
    new_response = env.page_table.build_ptw_resp(
        new_request["vpn"], s2xlate=new_request["s2xlate"], get_gpa=new_request["get_gpa"]
    )
    env.translation_oracle.observe_ptw_request(21, **{key: new_request[key] for key in ("vpn", "s2xlate", "get_gpa")})
    env.translation_oracle.observe_ptw_response(22, **{key: new_request[key] for key in ("vpn", "s2xlate", "get_gpa")}, response=new_response)

    stale_record = next(record for record in env.translation_oracle.get_stats()["records"] if record["kind"] == "stale_ptw_response")
    assert stale_record["response_epoch"] != env.translation_epoch
    assert env.translation_oracle.get_active()["response_seen"] is True


def test_oracle_discards_sfence_dropped_response_before_same_key_refill() -> None:
    env, old_state = _state(
        scenario_id="oracle-sfence-old",
        va=0x8020_3000,
        pa=0x8040_3000,
        pte=TranslationPte(v=1, r=1, x=1, a=1),
    )
    old_request = old_state.expected_ptw_request
    env.translation_oracle.observe_ptw_request(10, **{key: old_request[key] for key in ("vpn", "s2xlate", "get_gpa")})
    env.pulse_sfence()
    env.translation_oracle.discard_pending_ptw_responses(env.current_cycle, agent_dropped=1)

    new_state = TranslationScenarioBuilder(env).build(
        TranslationScenario(
            scenario_id="oracle-sfence-new",
            va=old_state.scenario.va,
            pa=0x8060_3000,
            payload=b"\x13\x00\x00\x00",
        )
    )
    env.arm_translation_scenario(new_state)
    new_request = new_state.expected_ptw_request
    new_response = env.page_table.build_ptw_resp(
        new_request["vpn"], s2xlate=new_request["s2xlate"], get_gpa=new_request["get_gpa"]
    )
    env.translation_oracle.observe_ptw_request(20, **{key: new_request[key] for key in ("vpn", "s2xlate", "get_gpa")})
    env.translation_oracle.observe_ptw_response(
        21,
        **{key: new_request[key] for key in ("vpn", "s2xlate", "get_gpa")},
        response=new_response,
    )

    assert env.translation_oracle.get_active()["response_seen"] is True
    assert any(record["kind"] == "sfence_dropped_ptw_requests" for record in env.translation_oracle.get_stats()["records"])


def test_oracle_accepts_the_second_page_ptw_transaction_after_the_first_fetch() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    state = TranslationScenarioBuilder(env).build(
        TranslationScenario(
            scenario_id="oracle-cross-page",
            va=0x8020_0FFE,
            pa=0x8040_0FFE,
            payload=b"\x13\x00\x00\x00",
            page_count=2,
        )
    )
    env.arm_translation_scenario(state)
    first = state.expected_ptw_request
    first_response = env.page_table.build_ptw_resp(first["vpn"])
    env.translation_oracle.observe_ptw_request(10, **{key: first[key] for key in ("vpn", "s2xlate", "get_gpa")})
    env.translation_oracle.observe_ptw_response(11, **{key: first[key] for key in ("vpn", "s2xlate", "get_gpa")}, response=first_response)
    env.translation_oracle.observe_fetch_request(12, path="icache", pa=state.expected_outcome["pa"] & ~0x3F)

    second = {**first, "vpn": first["vpn"] + 1}
    second_response = env.page_table.build_ptw_resp(second["vpn"])
    env.translation_oracle.observe_ptw_request(13, **{key: second[key] for key in ("vpn", "s2xlate", "get_gpa")})
    env.translation_oracle.observe_ptw_response(14, **{key: second[key] for key in ("vpn", "s2xlate", "get_gpa")}, response=second_response)
    env.translation_oracle.observe_fetch_request(15, path="icache", pa=state.expected_page_outcomes[1]["pa"] & ~0x3F)

    stats = env.assert_translation_scenario()
    assert stats["error_count"] == 0
    assert stats["active"]["responded_ptw_vpns"] == [first["vpn"], second["vpn"]]
    assert stats["active"]["fetched_pages"] == [0, 1]


def test_oracle_can_verify_a_selected_page_without_a_new_ptw_request() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    state = TranslationScenarioBuilder(env).build(
        TranslationScenario(
            scenario_id="oracle-selected-page-hit",
            va=0x8020_0000,
            pa=0x8040_0000,
            payload=b"\x13\x00\x00\x00" * 1025,
            page_count=2,
        )
    )

    active = env.arm_translation_scenario(state, page_indexes=(1,), expect_ptw=False)
    second = state.expected_page_outcomes[1]
    env.translation_oracle.observe_fetch_request(10, path="icache", pa=int(second["pa"]) & ~0x3F)

    stats = env.assert_translation_scenario()
    assert active["expected_ptw_requests"] == []
    assert active["selected_pages"] == [1]
    assert stats["active"]["fetched_pages"] == [1]


def test_oracle_accepts_known_fetch_from_unselected_page() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    state = TranslationScenarioBuilder(env).build(
        TranslationScenario(
            scenario_id="oracle-selected-page-stale-fetch",
            va=0x8020_0F00,
            pa=0x8040_0F00,
            payload=b"\x13\x00\x00\x00" * 512,
            page_count=2,
        )
    )

    active = env.arm_translation_scenario(state, page_indexes=(0,))
    _observe_matching_ptw(env, state)
    second_page = state.expected_page_outcomes[1]
    second_page_line = int(second_page["pa"]) & ~0x3F
    env.translation_oracle.observe_fetch_request(10, path="icache", pa=second_page_line)

    assert active["allowed_out_of_scope_fetches"]
    assert env.translation_oracle.errors == []
    assert env.translation_oracle.get_active()["observed_out_of_scope_fetch_pas"] == [second_page_line]


def test_oracle_rejects_second_page_fetch_with_the_wrong_pma_path() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    scenario = TranslationScenario(
        scenario_id="oracle-cross-page-pma-path",
        va=0x8020_0FFE,
        pa=0x8040_0FFE,
        payload=b"\x13\x00\x00\x00",
        page_count=2,
        pma_entries=(
            TranslationPmpPmaEntry(
                "pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_0000, size=0x1000
            ),
            TranslationPmpPmaEntry(
                "pma", 1, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=False), 0x8040_1000, size=0x1000
            ),
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.arm_translation_scenario(state)
    first = state.expected_ptw_request
    first_response = env.page_table.build_ptw_resp(first["vpn"])
    env.translation_oracle.observe_ptw_request(10, **{key: first[key] for key in ("vpn", "s2xlate", "get_gpa")})
    env.translation_oracle.observe_ptw_response(11, **{key: first[key] for key in ("vpn", "s2xlate", "get_gpa")}, response=first_response)
    env.translation_oracle.observe_fetch_request(12, path="icache", pa=state.expected_page_outcomes[0]["pa"] & ~0x3F)
    second = {**first, "vpn": first["vpn"] + 1}
    second_response = env.page_table.build_ptw_resp(second["vpn"])
    env.translation_oracle.observe_ptw_request(13, **{key: second[key] for key in ("vpn", "s2xlate", "get_gpa")})
    env.translation_oracle.observe_ptw_response(14, **{key: second[key] for key in ("vpn", "s2xlate", "get_gpa")}, response=second_response)
    env.translation_oracle.observe_fetch_request(15, path="icache", pa=state.expected_page_outcomes[1]["pa"] & ~0x3F)

    with pytest.raises(AssertionError, match="translated_pa_or_path_mismatch"):
        env.assert_translation_scenario()


def test_oracle_accepts_pmp_configured_fetch_without_internal_permission_sampling() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    scenario = TranslationScenario(
        scenario_id="oracle-pmp-configured-fetch",
        va=0x8020_0000,
        pa=0x8040_0000,
        payload=b"\x13" * 16,
        pmp_entries=(
            TranslationPmpPmaEntry(
                "pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), 0x8040_0000, size=0x1000
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                "pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True), 0x8040_0000, size=0x1000
            ),
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.arm_translation_scenario(state)
    _observe_matching_ptw(env, state)
    env.translation_oracle.observe_fetch_request(13, path="icache", pa=scenario.pa & ~0x3F)
    assert env.assert_translation_scenario()["error_count"] == 0


def test_oracle_reads_internal_itlb_get_gpa_when_the_top_level_ptw_field_is_absent() -> None:
    dut = _PmpProbeDut()
    env = FrontendEnv(dut, register_callbacks=False)
    scenario = TranslationScenario(
        scenario_id="oracle-internal-get-gpa",
        va=0x8020_0404,
        gpa=0x8040_0404,
        pa=0x8060_0404,
        payload=b"\x13\x00\x00\x00",
        s2xlate=3,
        get_gpa=1,
        hgatp_vmid=3,
        s1_pte=TranslationPte(vmid=3),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.arm_translation_scenario(state)
    dut.io_ptw_req_0_valid.value = 1
    dut.io_ptw_req_0_ready.value = 1
    dut.io_ptw_req_0_bits_vpn.value = state.expected_ptw_request["vpn"]
    dut.io_ptw_req_0_bits_s2xlate.value = state.expected_ptw_request["s2xlate"]
    dut.io_ptw_req_0_bits_getGpa.value = 0
    dut._internal_signals[_ITLB_PTW_REQ_GET_GPA] = FakeSignal(1)

    env.translation_oracle.on_clock_edge(12)

    request = next(record for record in env.translation_oracle.get_stats()["records"] if record["kind"] == "ptw_request")
    assert request["actual"] == {
        "vpn": state.expected_ptw_request["vpn"],
        "s2xlate": state.expected_ptw_request["s2xlate"],
        "get_gpa": 1,
    }


def test_oracle_requires_the_guest_fault_gpa_followup_ptw_transaction() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    scenario = TranslationScenario(
        scenario_id="oracle-guest-fault-gpa-followup",
        va=0x8020_0404,
        gpa=0x8040_0404,
        pa=0x8060_0404,
        payload=b"\x13\x00\x00\x00",
        s2xlate=3,
        hgatp_vmid=3,
        s1_pte=TranslationPte(vmid=3),
        s2_gpf=1,
        expected_path="fault",
        expected_result="guest_fault",
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.arm_translation_scenario(state)
    first = state.expected_ptw_request
    followup = {**first, "get_gpa": 1}

    for cycle, request in ((10, first), (12, followup)):
        response = env.page_table.build_ptw_resp(
            request["vpn"], s2xlate=request["s2xlate"], get_gpa=request["get_gpa"]
        )
        env.translation_oracle.observe_ptw_request(cycle, **{key: request[key] for key in ("vpn", "s2xlate", "get_gpa")})
        env.translation_oracle.observe_ptw_response(cycle + 1, **{key: request[key] for key in ("vpn", "s2xlate", "get_gpa")}, response=response)
    env.translation_oracle.observe_cfvec(14, pc=scenario.va, exception_bits={20: 1})

    stats = env.assert_translation_scenario()
    assert stats["active"]["requested_ptw_request_keys"] == [
        (first["vpn"], first["s2xlate"], 0),
        (followup["vpn"], followup["s2xlate"], 1),
    ]


def test_oracle_requires_guest_fault_gpa_followup_only_for_faulting_cross_page() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    va = 0x8020_0FFE
    scenario = TranslationScenario(
        scenario_id="oracle-cross-page-second-guest-fault",
        va=va,
        gpa=0x8040_0FFE,
        pa=0x8060_0FFE,
        payload=b"\x13\x00\x00\x00",
        page_count=2,
        s2xlate=3,
        hgatp_vmid=3,
        s1_pte=TranslationPte(vmid=3),
        ptw_response_overrides=(
            TranslationPtwResponseOverride(
                vpn=(va >> 12) + 1,
                s2xlate=3,
                patch=(("s2_gpf", 1),),
            ),
            TranslationPtwResponseOverride(
                vpn=(va >> 12) + 1,
                s2xlate=3,
                get_gpa=1,
                patch=(("s2_gpf", 1),),
            ),
        ),
        expected_path="fault",
        expected_result="guest_fault",
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    active = env.arm_translation_scenario(state)

    assert [
        (request["vpn"], request["s2xlate"], request["get_gpa"])
        for request in active["expected_ptw_requests"]
    ] == [
        (va >> 12, 3, 0),
        ((va >> 12) + 1, 3, 0),
        ((va >> 12) + 1, 3, 1),
    ]


def test_oracle_does_not_require_untranslated_permission_probe() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    scenario = TranslationScenario(
        scenario_id="oracle-untranslated-probe",
        va=0x8020_0000,
        pa=0x8040_0000,
        payload=b"\x13\x00\x00\x00\x13\x00\x00\x00",
        s1_pf=1,
        permission_probes=(TranslationPermissionProbe(va=0x8020_0000, size=8),),
    )

    state = TranslationScenarioBuilder(env).build(scenario)
    env.arm_translation_scenario(state)
    _observe_matching_ptw(env, state)
    env.translation_oracle.observe_cfvec(12, pc=scenario.va, exception_bits={12: 1})

    assert env.assert_translation_scenario()["error_count"] == 0

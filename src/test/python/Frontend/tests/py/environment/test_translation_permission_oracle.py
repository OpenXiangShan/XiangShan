from __future__ import annotations

import pytest

from env.core.frontend_env import FrontendEnv
from env.runtime.dut_factory import FakeDUTFrontend, FakeSignal
from env.sequences import TranslationPmpPmaEntry, TranslationPte, TranslationScenario, TranslationScenarioBuilder
from env.support import PmpPmaConfig


_MAINPIPE_S1_VALID = "Frontend_top.Frontend.inner_icache.mainPipe.s1_valid"
_MAINPIPE_START_VADDR = "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_0_vAddr_0_addr"
_MAINPIPE_PTAG = "Frontend_top.Frontend.inner_icache.mainPipe.s1_wayLookupEntry_0_pTag"


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

    stats = env.assert_translation_scenario()
    assert stats["error_count"] == 0
    assert stats["active"]["responded_ptw_vpns"] == [first["vpn"], second["vpn"]]


def test_oracle_reconstructs_mainpipe_pmp_request_address_after_translation() -> None:
    dut = _PmpProbeDut()
    env = FrontendEnv(dut, register_callbacks=False)
    scenario = TranslationScenario(
        scenario_id="oracle-mainpipe-pmp-request",
        va=0x8020_0404,
        pa=0x8040_0404,
        payload=b"\x13\x00\x00\x00",
        pmp_entries=(
            TranslationPmpPmaEntry(
                "pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), 0x8040_0000, size=0x1000
            ),
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.arm_translation_scenario(state)
    _observe_matching_ptw(env, state)
    dut._internal_signals.update(
        {
            _MAINPIPE_S1_VALID: FakeSignal(1),
            _MAINPIPE_START_VADDR: FakeSignal(scenario.va >> 1),
            _MAINPIPE_PTAG: FakeSignal(scenario.pa >> 12),
        }
    )

    env.translation_oracle.on_clock_edge(12)
    env.translation_oracle.observe_fetch_request(13, path="icache", pa=scenario.pa & ~0x3F)

    stats = env.assert_translation_scenario()
    pmp_record = next(record for record in stats["records"] if record["kind"] == "mainpipe_pmp_request")
    assert (pmp_record["addr"], pmp_record["size"], pmp_record["end"]) == (scenario.pa, 8, scenario.pa + 7)


def test_oracle_does_not_require_pmp_request_after_page_fault() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    state = TranslationScenarioBuilder(env).build(
        TranslationScenario(
            scenario_id="oracle-page-fault-before-pmp",
            va=0x8020_0404,
            pa=0x8040_0404,
            payload=b"\x13\x00\x00\x00",
            s1_pte=TranslationPte(v=1, r=1, x=0, a=1),
            pmp_entries=(
                TranslationPmpPmaEntry(
                    "pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True), 0x8040_0000, size=0x1000
                ),
            ),
        )
    )

    active = env.arm_translation_scenario(state)

    assert active["expected_fault"] == "instruction_page_fault"
    assert active["permission_check_required"] is False


def test_oracle_rejects_mismatched_reconstructed_mainpipe_pmp_request_address() -> None:
    env, state = _state(
        scenario_id="oracle-mainpipe-pmp-request-mismatch",
        va=0x8020_0404,
        pa=0x8040_0404,
        pte=TranslationPte(v=1, r=1, x=1, a=1),
    )
    env.translation_oracle.active["permission_check_required"] = True
    env.translation_oracle.active["response_seen"] = True
    env.translation_oracle.observe_mainpipe_pmp_request(12, addr=state.expected_outcome["pa"] + 2)

    with pytest.raises(AssertionError, match="mainpipe_pmp_request_addr_mismatch"):
        env.assert_translation_scenario()

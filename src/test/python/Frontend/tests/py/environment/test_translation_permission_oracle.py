from __future__ import annotations

import pytest

from env.core.frontend_env import FrontendEnv
from env.runtime.dut_factory import FakeDUTFrontend
from env.sequences import TranslationPte, TranslationScenario, TranslationScenarioBuilder


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

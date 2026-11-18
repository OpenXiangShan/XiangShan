from __future__ import annotations

import os
from dataclasses import replace

import pytest

from env.core.transactions import RedirectTxn
from env.runtime.frontend_init import FrontendInitConfig, initialize_frontend
from env.sequences import (
    InjectRedirectSequence,
    TranslationPte,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from tests.py.support.dut_support import wait_until as _wait_until
from tests.py.support.translation_context_switch import (
    OLD_PA as _OLD_PA,
    NEW_PA as _NEW_PA,
    randomize_context_timing as _randomize_context_timing,
    same_vpn_context_scenarios as _same_vpn_context_scenarios,
    translation_complete as _translation_complete,
    run_satp_asid_switch,
)

from tests.py.zhaoxinran.support.translation import (
    _sfence_stage_scenario,
    _run_sfence_retranslation,
    _run_sfence_scope_after_refill,
)

_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"


def _run_to_refill(env, scenario: TranslationScenario):
    initialize_frontend(
        env,
        FrontendInitConfig(
            reset_vector=scenario.va,
            bare_mode=str(scenario.mode).lower() == "bare" and not scenario.s2xlate,
            reset_cycles=20,
        ),
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _wait_until(env, lambda: _translation_complete(env), description=f"{scenario.scenario_id} translation refill")
    env.assert_translation_scenario()
    return state


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_satp_asid_switch_restarts_translation_from_live_cfvec(env) -> None:
    run_satp_asid_switch(env)


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tlb_csr_change_retranslates_same_vpn_with_new_pte_signature(env) -> None:
    old_scenario, new_scenario = _same_vpn_context_scenarios(env)
    old = _randomize_context_timing(env, old_scenario)
    new = _randomize_context_timing(env, new_scenario)
    builder = TranslationScenarioBuilder(env)

    # Build the target oracle before the live source context; building a
    # scenario drives CSR inputs and therefore consumes DUT cycles.
    new_state = builder.build(new)
    initialize_frontend(
        env, FrontendInitConfig(reset_vector=old.va, bare_mode=False, reset_cycles=20)
    )
    old_state = builder.build(old)
    env.monitor.clear()
    env.monitor.set_expected_pc(old.va)
    env.arm_translation_scenario(old_state)
    _wait_until(
        env,
        lambda: any(int(observation.pc) == old.va for observation in env.monitor.observations)
        and int(env.ptw_agent.get_stats()["resp_count"]) > 0,
        description="old same-VPN live translation",
    )

    for page in range(new.page_count):
        env.page_table.map_page(
            (new.va >> 12) + page,
            (new.pa >> 12) + page,
            **new.s1_pte.as_mapping_kwargs(),
        )
    change = env.prepare_translation_context_change(satp_asid=new.satp_asid)
    assert change["changed"]["satp"]
    new_state = replace(new_state, translation_epoch=env.translation_epoch)
    env.monitor.set_expected_pc(new.va)
    env.arm_translation_scenario(new_state)
    env.translation_oracle.set_fetch_observation_ready(ready=False)
    redirect = InjectRedirectSequence(
        RedirectTxn(
            source_pc=old.va,
            target_pc=new.va,
            reason="tlb-csr-same-vpn-new-pte-signature",
            satp_flush=1,
        )
    )
    redirect.inject(env)
    env.step(1)
    env.release_translation_context_change(change)
    assert redirect.wait(env)
    env.translation_oracle.set_fetch_observation_ready(ready=True)
    _wait_until(env, lambda: _translation_complete(env), description="new same-VPN translation")
    env.assert_translation_scenario()
    assert int(env.ptw_agent.get_stats()["req_count"]) >= 2
    assert not env.get_errors()


@pytest.mark.parametrize(
    "scenario_id,context_update,changed_name",
    (
        ("translation-context-vsatp-after-refill", {"vsatp_asid": 9}, "vsatp"),
        ("translation-context-hgatp-after-refill", {"hgatp_vmid": 11}, "hgatp"),
        ("translation-context-priv-virt-after-refill", {"priv_virt": 0}, "priv_virt"),
    ),
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_all_stage_context_change_after_refill(env, scenario_id, context_update, changed_name) -> None:
    scenario = _randomize_context_timing(env, _sfence_stage_scenario(env, scenario_id, s2xlate=3))
    pa_delta = _NEW_PA - _OLD_PA
    new_scenario = replace(scenario, scenario_id=f"{scenario_id}-new", pa=scenario.pa + pa_delta)
    if changed_name == "vsatp":
        new_scenario = replace(
            new_scenario,
            gpa=scenario.gpa + pa_delta,
            vsatp_asid=context_update["vsatp_asid"],
            s1_pte=replace(scenario.s1_pte, asid=context_update["vsatp_asid"]),
        )
    elif changed_name == "hgatp":
        new_scenario = replace(
            new_scenario,
            hgatp_vmid=context_update["hgatp_vmid"],
            s1_pte=replace(scenario.s1_pte, vmid=context_update["hgatp_vmid"]),
            s2_pte=replace(scenario.s2_pte, vmid=context_update["hgatp_vmid"]),
        )
    else:
        new_scenario = replace(
            new_scenario,
            gpa=None,
            s2xlate=0,
            priv_virt=0,
            s1_pte=TranslationPte(asid=scenario.satp_asid),
        )
    builder = TranslationScenarioBuilder(env)
    new_state = builder.build(new_scenario)
    initialize_frontend(
        env, FrontendInitConfig(reset_vector=scenario.va, bare_mode=False, reset_cycles=20)
    )
    state = builder.build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _wait_until(env, lambda: _translation_complete(env), description="all-stage translation refill")
    env.assert_translation_scenario()
    old_active = env.translation_oracle.get_active()
    assert old_active is not None
    assert (scenario.pa & ~0x3F) in old_active["observed_fetch_pas"]

    req_count = int(env.ptw_agent.get_stats()["req_count"])
    for page in range(new_scenario.page_count):
        env.page_table.map_page(
            (scenario.va >> 12) + page,
            ((new_scenario.gpa if new_scenario.gpa is not None else new_scenario.pa) >> 12) + page,
            **new_scenario.s1_pte.as_mapping_kwargs(),
        )
        if new_scenario.gpa is not None:
            stage2_pte = new_scenario.s2_pte.as_mapping_kwargs()
            stage2_pte.pop("asid")
            env.page_table.map_stage2_page(
                (new_scenario.gpa >> 12) + page,
                (new_scenario.pa >> 12) + page,
                **stage2_pte,
            )
    env.load_program(new_scenario.payload, new_scenario.pa)
    change = env.prepare_translation_context_change(**context_update)
    assert change["changed"][changed_name]
    new_state = replace(new_state, translation_epoch=env.translation_epoch)
    env.monitor.set_expected_pc(new_scenario.va)
    env.arm_translation_scenario(new_state)
    env.translation_oracle.set_fetch_observation_ready(ready=False)
    redirect_count = int(env.monitor.redirect_count)
    redirect = InjectRedirectSequence(
        RedirectTxn(
            target_pc=new_scenario.va,
            reason=f"{scenario_id}-new-context",
        )
    )
    redirect.inject(env)
    env.step(1)
    env.release_translation_context_change(change)
    _wait_until(
        env,
        lambda: int(env.monitor.redirect_count) > redirect_count,
        description=f"redirect after {changed_name} change",
    )
    env.translation_oracle.set_fetch_observation_ready(ready=True)
    _wait_until(
        env,
        lambda: _translation_complete(env),
        description=f"complete translation after {changed_name} change",
    )
    env.assert_translation_scenario()
    active = env.translation_oracle.get_active()
    assert active is not None
    expected_ptw = {
        (int(request["vpn"]), int(request["s2xlate"]), int(request["get_gpa"]))
        for request in active["expected_ptw_requests"]
    }
    assert len(expected_ptw) == new_scenario.page_count
    assert expected_ptw.issubset(active["requested_ptw_request_keys"])
    assert expected_ptw.issubset(active["responded_ptw_request_keys"])
    assert (new_scenario.pa & ~0x3F) in active["observed_fetch_pas"]
    assert (scenario.pa & ~0x3F) not in active["observed_fetch_pas"]
    assert active["translation_epoch"] == env.translation_epoch
    assert int(env.ptw_agent.get_stats()["req_count"]) > req_count
    assert env.translation_epoch == state.translation_epoch + 1
    assert not env.get_errors()


@pytest.mark.parametrize(
    "scenario_id,rs1,rs2,ident,retranslation_page_indexes",
    (
        ("translation-sfence-all-address-all-id", 1, 1, 0, None),
        ("translation-sfence-single-address-all-id", 0, 1, 0, (0,)),
        ("translation-sfence-single-address-single-id", 0, 0, 3, (0,)),
    ),
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_sfence_scope_after_refill(
    env,
    scenario_id,
    rs1,
    rs2,
    ident,
    retranslation_page_indexes,
) -> None:
    return _run_sfence_scope_after_refill(
        env,
        scenario_id=scenario_id,
        rs1=rs1,
        rs2=rs2,
        ident=ident,
        retranslation_page_indexes=retranslation_page_indexes,
    )


@pytest.mark.parametrize(
    "scenario_id,s2xlate,hv,hg",
    (
        ("translation-sfence-only-stage1", 1, 1, 0),
        ("translation-sfence-only-stage2", 2, 0, 1),
        ("translation-sfence-all-stage-vs", 3, 1, 0),
        ("translation-sfence-all-stage-g", 3, 0, 1),
    ),
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_sfence_stage_after_refill(env, scenario_id, s2xlate, hv, hg) -> None:
    scenario = _randomize_context_timing(
        env,
        _sfence_stage_scenario(env, scenario_id, s2xlate=s2xlate),
    )
    _run_sfence_retranslation(
        env,
        scenario,
        addr=scenario.va,
        rs1=1,
        rs2=1,
        hv=hv,
        hg=hg,
    )

    assert not env.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_unmatched_sfence_during_ptw_wait_preserves_translation(env) -> None:
    scenario = _randomize_context_timing(env, replace(
        _sfence_stage_scenario(env, "translation-unmatched-sfence-ptw-wait", s2xlate=0),
        ptw_response_latency=32,
    ), latency_min=24, latency_max=48)
    initialize_frontend(
        env, FrontendInitConfig(reset_vector=scenario.va, bare_mode=False, reset_cycles=20)
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _wait_until(
        env,
        lambda: int(env.ptw_agent.get_stats()["pending"]) + int(env.ptw_agent.get_stats()["active_resp"]) > 0,
        description="inflight PTW response",
    )

    env.pulse_sfence(
        addr=scenario.va + 0x2000,
        rs1=1,
        rs2=1,
        ident=scenario.satp_asid,
        advance_translation_epoch=False,
    )

    _wait_until(env, lambda: _translation_complete(env), description="translation after unmatched SFENCE")
    env.assert_translation_scenario()
    env.translation_oracle.disarm()
    assert not env.get_errors()

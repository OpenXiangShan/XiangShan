"""SFENCE retranslation scenarios shared by directed tests."""

from __future__ import annotations

from dataclasses import replace

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
    OLD_VA as _OLD_VA,
    OLD_PA as _OLD_PA,
    OLD_GPA as _OLD_GPA,
    PAYLOAD as _PAYLOAD,
    permissions as _permissions,
    randomize_context_timing as _randomize_context_timing,
)


def _sfence_retranslation_complete(env) -> bool:
    active = env.translation_oracle.get_active()
    if active is None:
        return False
    expected_ptw = {
        (int(request["vpn"]), int(request["s2xlate"]), int(request["get_gpa"]))
        for request in active["expected_ptw_requests"]
    }
    responded_ptw = {tuple(map(int, request)) for request in active["responded_ptw_request_keys"]}
    return expected_ptw.issubset(responded_ptw) and bool(active["observed_normal_cfvec_pages"])


def _all_stage_scenario(scenario_id: str) -> TranslationScenario:
    pmp_entries, pma_entries = _permissions()
    return TranslationScenario(
        scenario_id=scenario_id,
        va=_OLD_VA,
        gpa=_OLD_GPA,
        pa=_OLD_PA,
        payload=_PAYLOAD,
        page_count=2,
        mode="sv39",
        stage2_mode="sv39",
        s2xlate=3,
        s1_pte=TranslationPte(asid=5, vmid=7),
        s2_pte=TranslationPte(vmid=7),
        vsatp_asid=5,
        hgatp_vmid=7,
        priv_virt=1,
        pmp_entries=pmp_entries,
        pma_entries=pma_entries,
    )


def _sfence_stage_scenario(scenario_id: str, *, s2xlate: int) -> TranslationScenario:
    if int(s2xlate) == 3:
        return _all_stage_scenario(scenario_id)

    pmp_entries, pma_entries = _permissions()
    common = {
        "scenario_id": scenario_id,
        "va": _OLD_VA,
        "pa": _OLD_PA,
        "payload": _PAYLOAD,
        "page_count": 2,
        "pmp_entries": pmp_entries,
        "pma_entries": pma_entries,
    }
    if int(s2xlate) == 0:
        return TranslationScenario(mode="sv39", satp_asid=3, s1_pte=TranslationPte(asid=3), **common)
    if int(s2xlate) == 1:
        return TranslationScenario(
            mode="sv39",
            s2xlate=1,
            s1_pte=TranslationPte(asid=5),
            vsatp_asid=5,
            priv_virt=1,
            **common,
        )
    if int(s2xlate) == 2:
        return TranslationScenario(
            mode="bare",
            stage2_mode="sv39",
            s2xlate=2,
            gpa=_OLD_GPA,
            s2_pte=TranslationPte(vmid=7),
            hgatp_vmid=7,
            priv_virt=1,
            **common,
        )
    raise ValueError(f"unsupported SFENCE stage s2xlate={s2xlate}")


def _run_sfence_retranslation(
    env,
    scenario: TranslationScenario,
    *,
    retranslation_page_indexes: tuple[int, ...] | None = None,
    **sfence_fields,
) -> dict:
    """Invalidate a live refill before its source FTQ entry is committed."""

    initialize_frontend(
        env, FrontendInitConfig(reset_vector=scenario.va, bare_mode=False, reset_cycles=20)
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _wait_until(
        env,
        lambda: any(int(observation.pc) == scenario.va for observation in env.monitor.observations)
        and int(env.ptw_agent.get_stats()["resp_count"]) > 0,
        description=f"{scenario.scenario_id} live refill",
    )

    prepared_sfence = env.prepare_sfence(**sfence_fields)
    retranslated = replace(state, translation_epoch=env.translation_epoch)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(retranslated, page_indexes=retranslation_page_indexes)
    env.translation_oracle.set_fetch_observation_ready(ready=False)
    redirect = InjectRedirectSequence(
        RedirectTxn(
            source_pc=scenario.va,
            target_pc=scenario.va,
            reason=f"{scenario.scenario_id}-retranslate",
            satp_flush=1,
        )
    )
    redirect.inject(env)
    env.step(1)
    env.release_sfence(prepared_sfence)
    assert redirect.wait_for_notification(env)
    env.translation_oracle.set_fetch_observation_ready(ready=True)
    assert redirect.wait(env)
    _wait_until(
        env,
        lambda: _sfence_retranslation_complete(env),
        description=f"{scenario.scenario_id} retranslation",
    )
    assert env.translation_oracle.get_stats()["error_count"] == 0
    env.translation_oracle.disarm()
    return {**prepared_sfence, "cycles": 1}


def _run_sfence_scope_after_refill(
    env,
    scenario_id,
    rs1,
    rs2,
    ident,
    retranslation_page_indexes,
) -> None:
    scenario = _randomize_context_timing(
        env,
        _sfence_stage_scenario(scenario_id, s2xlate=0),
    )
    _run_sfence_retranslation(
        env,
        scenario,
        retranslation_page_indexes=retranslation_page_indexes,
        addr=scenario.va,
        rs1=rs1,
        rs2=rs2,
        ident=ident,
    )

    assert not env.get_errors()

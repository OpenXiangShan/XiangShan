"""SFENCE retranslation scenarios shared by directed tests."""

from __future__ import annotations

import random
from dataclasses import replace

from env.core.transactions import RedirectTxn
from env.runtime.frontend_init import FrontendInitConfig, initialize_frontend
from env.sequences import (
    InjectRedirectSequence,
    TranslationPte,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from env.support import PcMapping, random_pc, record_scenario, scenario_rng
from tests.py.support.dut_support import wait_until as _wait_until
from tests.py.support.backend_fault_recovery import (
    _VA as _BACKEND_FAULT_VA,
    _PA as _BACKEND_FAULT_PA,
)
from tests.py.support.translation_context_switch import (
    OLD_VA as _OLD_VA,
    OLD_PA as _OLD_PA,
    OLD_GPA as _OLD_GPA,
    PAYLOAD as _PAYLOAD,
    _context_entry,
    permissions as _permissions,
    randomize_context_timing as _randomize_context_timing,
)
from tests.py.support.translation_scenarios import (
    ptw_responses_complete,
    randomize_ptw_response,
    relocate_translation_entry,
)


def _sfence_retranslation_complete(env) -> bool:
    active = env.translation_oracle.get_active()
    if active is None:
        return False
    return ptw_responses_complete(active) and bool(active["observed_normal_cfvec_pages"])


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


def _sfence_stage_scenario(env, scenario_id: str, *, s2xlate: int) -> TranslationScenario:
    scenario_key = f"zhaoxinran/translation/sfence/{scenario_id}"
    entry, base_seed, seed = _context_entry(scenario_key)
    gpa = _OLD_GPA + entry.va - _OLD_VA
    record_scenario(
        env, scenario_key, base_seed=base_seed, seed=seed,
        parameters={"va": entry.va, "pa": entry.pa, "gpa": gpa if s2xlate in (2, 3) else None,
                    "s2xlate": s2xlate},
    )
    if int(s2xlate) == 3:
        pmp_entries, pma_entries = _permissions(pa=entry.pa)
        return replace(
            _all_stage_scenario(scenario_id), va=entry.va, pa=entry.pa, gpa=gpa,
            pmp_entries=pmp_entries, pma_entries=pma_entries,
        )

    pmp_entries, pma_entries = _permissions(pa=entry.pa)
    common = {
        "scenario_id": scenario_id,
        "va": entry.va,
        "pa": entry.pa,
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
            gpa=gpa,
            s2_pte=TranslationPte(vmid=7),
            hgatp_vmid=7,
            priv_virt=1,
            **common,
        )
    raise ValueError(f"unsupported SFENCE stage s2xlate={s2xlate}")


def randomize_ptw_timing(
    scenario: TranslationScenario, rng: random.Random, *, seed: int, latency_limit: int | None,
) -> TranslationScenario:
    return randomize_ptw_response(
        scenario, rng, seed=seed,
        latency_range=(1, latency_limit) if latency_limit is not None else None,
        latency_max_limit=8 if latency_limit is not None and scenario.ptw_response_latency_max is not None else None,
        keep_latency_max=False,
        randomize_ready=True,
    )


def randomize_translation_layout(
    env, scenario: TranslationScenario, *, scenario_key: str, stride: int = 0x4000,
) -> TranslationScenario:
    base_seed, seed, rng = scenario_rng(scenario_key)
    span = max(0x20_0000, stride * 32)
    entry = random_pc(
        rng,
        mappings=(PcMapping(scenario.va & ~0x1F_FFFF, scenario.pa & ~0x1F_FFFF, span),),
        entry_offsets=tuple(range(scenario.va % stride, span, stride)),
        instruction_bytes=4,
    )
    va_delta = entry.va - scenario.va
    lane_delta = va_delta if scenario.s2xlate == 3 else entry.pa - scenario.pa
    relocated = replace(
        relocate_translation_entry(scenario, entry, s1_ppn_delta=lane_delta >> 12),
        ptw_response_overrides=tuple(
            replace(item, vpn=item.vpn + (va_delta >> 12)) for item in scenario.ptw_response_overrides
        ),
        permission_probes=tuple(replace(probe, va=probe.va + va_delta) for probe in scenario.permission_probes),
    )
    record_scenario(
        env, scenario_key, base_seed=base_seed, seed=seed,
        parameters={"va": relocated.va, "pa": relocated.pa, "gpa": relocated.gpa},
    )
    return relocated


def randomize_gstage_entry(env, scenario: TranslationScenario, *, stride: int = 0x4000) -> TranslationScenario:
    return randomize_translation_layout(
        env, scenario, scenario_key=f"zhaoxinran/translation/gstage/{scenario.scenario_id}/pc", stride=stride,
    )


def backend_fault_entry(env, scenario_key: str):
    base_seed, seed, rng = scenario_rng(scenario_key)
    entry = random_pc(
        rng,
        mappings=(PcMapping(_BACKEND_FAULT_VA & ~0xFFF, _BACKEND_FAULT_PA & ~0xFFF, 0x10_0000),),
        entry_offsets=tuple(range(0xF00, 0x10_0000, 0x4000)),
    )
    record_scenario(
        env, scenario_key, base_seed=base_seed, seed=seed,
        parameters={"reset_pc": entry.va + 0x100, "va": entry.va, "pa": entry.pa},
    )
    return entry


def randomize_permission_entry(env, scenario: TranslationScenario) -> TranslationScenario:
    scenario_key = f"zhaoxinran/translation/permission/{scenario.scenario_id}/pc"
    base_seed, seed, rng = scenario_rng(scenario_key)
    entry = random_pc(
        rng,
        mappings=(PcMapping(scenario.va & ~0x1F_FFFF, scenario.pa & ~0x1F_FFFF, 0x20_0000),),
        entry_offsets=tuple(slot * 0x4000 + (scenario.va & 0x3FFF) for slot in range(128)),
        instruction_bytes=4,
    )
    relocated = relocate_translation_entry(scenario, entry, relocate_gpa=False)
    record_scenario(
        env, scenario_key, base_seed=base_seed, seed=seed,
        parameters={"va": relocated.va, "pa": relocated.pa},
    )
    return relocated


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
        _sfence_stage_scenario(env, scenario_id, s2xlate=0),
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

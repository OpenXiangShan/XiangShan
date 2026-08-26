from __future__ import annotations

import os
from dataclasses import replace

import pytest

from env.core.transactions import RedirectTxn
from env.sequences import (
    InjectRedirectSequence,
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from env.support import PmpPmaConfig


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_OLD_VA = 0x8020_0F00
_NEW_VA = 0x8020_2F00
_OLD_PA = 0x8040_0F00
_NEW_PA = 0x8040_2F00
_OLD_GPA = 0x8060_0F00
_PAYLOAD = b"\x13\x00\x00\x00" * 512


def _permissions() -> tuple[tuple[TranslationPmpPmaEntry, ...], tuple[TranslationPmpPmaEntry, ...]]:
    region_base = _OLD_PA & ~0x3FFF
    return (
        (
            TranslationPmpPmaEntry(
                "pmp",
                0,
                PmpPmaConfig(match="napot", read=True, execute=True),
                region_base,
                size=0x4000,
            ),
        ),
        (
            TranslationPmpPmaEntry(
                "pma",
                0,
                PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True),
                region_base,
                size=0x4000,
            ),
        ),
    )


def _scenario(
    scenario_id: str,
    *,
    va: int,
    pa: int,
    asid: int,
    payload: bytes = _PAYLOAD,
    page_count: int = 2,
    **kwargs,
) -> TranslationScenario:
    pmp_entries, pma_entries = _permissions()
    return TranslationScenario(
        scenario_id=scenario_id,
        va=va,
        pa=pa,
        payload=payload,
        page_count=page_count,
        mode="sv39",
        satp_asid=asid,
        s1_pte=TranslationPte(asid=asid),
        pmp_entries=pmp_entries,
        pma_entries=pma_entries,
        **kwargs,
    )


def _wait_until(env, predicate, *, description: str, max_cycles: int = 2000) -> None:
    for _ in range(max_cycles):
        if predicate():
            return
        env.step(1)
    if not predicate():
        raise AssertionError(f"timed out waiting for {description}")


def _translation_complete(env) -> bool:
    active = env.translation_oracle.get_active()
    if active is None:
        return False
    expected_ptw = {
        (int(request["vpn"]), int(request["s2xlate"]), int(request["get_gpa"]))
        for request in active["expected_ptw_requests"]
    }
    responded_ptw = {tuple(map(int, request)) for request in active["responded_ptw_request_keys"]}
    return expected_ptw.issubset(responded_ptw) and len(active["expected_fetches"]) == len(
        active["observed_fetch_pas"]
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


def _run_to_refill(env, scenario: TranslationScenario):
    env.initialize(reset_vector=scenario.va, bare_mode=str(scenario.mode).lower() == "bare" and not scenario.s2xlate)
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _wait_until(env, lambda: _translation_complete(env), description=f"{scenario.scenario_id} translation refill")
    env.assert_translation_scenario()
    return state


def _run_sfence_retranslation(
    env,
    scenario: TranslationScenario,
    *,
    retranslation_page_indexes: tuple[int, ...] | None = None,
    **sfence_fields,
) -> dict:
    """Invalidate a live refill before its source FTQ entry is committed."""

    env.initialize(reset_vector=scenario.va, bare_mode=False)
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


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_satp_asid_switch_restarts_translation_from_live_cfvec(env) -> None:
    old = _scenario("translation-context-switch-old", va=_OLD_VA, pa=_OLD_PA, asid=3)
    new = _scenario("translation-context-switch-new", va=_NEW_VA, pa=_NEW_PA, asid=9)
    builder = TranslationScenarioBuilder(env)

    # Build the target oracle state before reset.  The old context is built after
    # reset so its PMP/PMA writes remain live when the DUT starts fetching.
    new_state = builder.build(new)
    env.initialize(reset_vector=_OLD_VA, bare_mode=False)
    old_state = builder.build(old)
    for page in range(new.page_count):
        env.page_table.map_page(
            (_NEW_VA >> 12) + page,
            (_NEW_PA >> 12) + page,
            **new.s1_pte.as_mapping_kwargs(),
        )

    env.monitor.clear()
    env.monitor.set_expected_pc(_OLD_VA)
    env.arm_translation_scenario(old_state)

    _wait_until(
        env,
        lambda: any(int(observation.pc) == _OLD_VA for observation in env.monitor.observations),
        description="old-context cfVec source",
    )

    prepared_context = env.prepare_translation_context_change(satp_asid=new.satp_asid)
    prepared_sfence = env.prepare_sfence(addr=_OLD_VA, rs1=1, rs2=1, ident=old.satp_asid)
    new_state = replace(new_state, translation_epoch=env.translation_epoch)
    env.monitor.set_expected_pc(_NEW_VA)
    env.arm_translation_scenario(new_state)
    env.translation_oracle.set_fetch_observation_ready(ready=False)

    redirect = InjectRedirectSequence(
        RedirectTxn(
            source_pc=_OLD_VA,
            target_pc=_NEW_VA,
            reason="satp-asid-switch",
            satp_flush=1,
        )
    )
    redirect.inject(env)
    env.step(1)
    env.release_translation_context_change(prepared_context)
    env.release_sfence(prepared_sfence)
    redirected = redirect.wait(env)

    assert redirected
    env.translation_oracle.set_fetch_observation_ready(ready=True)
    _wait_until(
        env,
        lambda: _translation_complete(env),
        description="complete new-context translation",
    )
    env.assert_translation_scenario()

    active = env.translation_oracle.get_active()
    assert active is not None
    assert active["translation_epoch"] == env.translation_epoch
    assert active["expected_ptw_requests"][0]["vpn"] == _NEW_VA >> 12
    assert any(int(observation.pc) == _NEW_VA for observation in env.monitor.observations)
    assert not env.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tlb_csr_change_retranslates_same_vpn_with_new_pte_signature(env) -> None:
    old = replace(
        _scenario(
            "translation-context-csr-signature-old",
            va=_OLD_VA,
            pa=_OLD_PA,
            asid=3,
        ),
        s1_pte=TranslationPte(asid=3, pbmt=0),
    )
    new = replace(
        _scenario(
            "translation-context-csr-signature-new",
            va=_OLD_VA,
            pa=_OLD_PA,
            asid=9,
        ),
        s1_pte=TranslationPte(asid=9, pbmt=1),
    )
    builder = TranslationScenarioBuilder(env)

    # Build the target oracle before the live source context; building a
    # scenario drives CSR inputs and therefore consumes DUT cycles.
    new_state = builder.build(new)
    env.initialize(reset_vector=_OLD_VA, bare_mode=False)
    old_state = builder.build(old)
    env.monitor.clear()
    env.monitor.set_expected_pc(_OLD_VA)
    env.arm_translation_scenario(old_state)
    _wait_until(
        env,
        lambda: any(int(observation.pc) == _OLD_VA for observation in env.monitor.observations)
        and int(env.ptw_agent.get_stats()["resp_count"]) > 0,
        description="old same-VPN live translation",
    )

    for page in range(new.page_count):
        env.page_table.map_page(
            (_OLD_VA >> 12) + page,
            (_OLD_PA >> 12) + page,
            **new.s1_pte.as_mapping_kwargs(),
        )
    change = env.prepare_translation_context_change(satp_asid=new.satp_asid)
    assert change["changed"]["satp"]
    new_state = replace(new_state, translation_epoch=env.translation_epoch)
    env.monitor.set_expected_pc(_OLD_VA)
    env.arm_translation_scenario(new_state)
    env.translation_oracle.set_fetch_observation_ready(ready=False)
    redirect = InjectRedirectSequence(
        RedirectTxn(
            source_pc=_OLD_VA,
            target_pc=_OLD_VA,
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


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_satp_switch_records_late_old_epoch_ptw_response(env) -> None:
    old = _scenario(
        "translation-context-stale-old-response",
        va=_OLD_VA,
        pa=_OLD_PA,
        asid=3,
        payload=b"\x13\x00\x00\x00" * 1088,
        page_count=2,
        ptw_response_latency=2,
        ptw_response_latency_max=80,
        ptw_response_seed=0x1F,
        ptw_flush_pending_on_sfence=False,
    )
    new = _scenario("translation-context-stale-new-response", va=_NEW_VA, pa=_NEW_PA, asid=9)
    builder = TranslationScenarioBuilder(env)

    new_state = builder.build(new)
    env.initialize(reset_vector=_OLD_VA, bare_mode=False)
    old_state = builder.build(old)
    for page in range(new.page_count):
        env.page_table.map_page(
            (_NEW_VA >> 12) + page,
            (_NEW_PA >> 12) + page,
            **new.s1_pte.as_mapping_kwargs(),
        )

    env.monitor.clear()
    env.monitor.set_expected_pc(_OLD_VA)
    env.arm_translation_scenario(old_state)
    _wait_until(
        env,
        lambda: any(int(observation.pc) == _OLD_VA for observation in env.monitor.observations)
        and int(env.ptw_agent.get_stats()["pending"]) + int(env.ptw_agent.get_stats()["active_resp"]) > 0,
        description="old-context live cfVec with late PTW response",
    )

    prepared_context = env.prepare_translation_context_change(satp_asid=new.satp_asid)
    prepared_sfence = env.prepare_sfence(addr=_OLD_VA, rs1=1, rs2=1, ident=old.satp_asid)
    new_state = replace(new_state, translation_epoch=env.translation_epoch)
    env.monitor.set_expected_pc(_NEW_VA)
    env.arm_translation_scenario(new_state)
    env.translation_oracle.set_fetch_observation_ready(ready=False)

    redirect = InjectRedirectSequence(
        RedirectTxn(
            source_pc=_OLD_VA,
            target_pc=_NEW_VA,
            reason="satp-asid-switch-stale-response",
            satp_flush=1,
        )
    )
    redirect.inject(env)
    env.step(1)
    env.release_translation_context_change(prepared_context)
    env.release_sfence(prepared_sfence)
    assert redirect.wait(env)
    env.translation_oracle.set_fetch_observation_ready(ready=True)
    _wait_until(
        env,
        lambda: any(record["kind"] == "stale_ptw_response" for record in env.translation_oracle.get_stats()["records"]),
        description="stale old-epoch PTW response record",
    )
    _wait_until(env, lambda: _translation_complete(env), description="complete new-context translation")
    env.assert_translation_scenario()
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
    scenario = _all_stage_scenario(scenario_id)
    env.initialize(reset_vector=scenario.va, bare_mode=False)
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _wait_until(env, lambda: _translation_complete(env), description="all-stage translation refill")
    env.assert_translation_scenario()

    change = env.update_translation_context(**context_update)

    assert change["changed"][changed_name]
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
    scenario = _sfence_stage_scenario(scenario_id, s2xlate=0)
    record = _run_sfence_retranslation(
        env,
        scenario,
        retranslation_page_indexes=retranslation_page_indexes,
        addr=scenario.va,
        rs1=rs1,
        rs2=rs2,
        ident=ident,
    )

    assert record["rs1"] == rs1
    assert record["rs2"] == rs2
    assert not env.get_errors()


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
    scenario = _sfence_stage_scenario(scenario_id, s2xlate=s2xlate)
    record = _run_sfence_retranslation(
        env,
        scenario,
        addr=scenario.va,
        rs1=1,
        rs2=1,
        hv=hv,
        hg=hg,
    )

    assert record["hv"] == hv
    assert record["hg"] == hg
    assert not env.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_unmatched_sfence_during_ptw_wait_preserves_translation(env) -> None:
    scenario = replace(
        _sfence_stage_scenario("translation-unmatched-sfence-ptw-wait", s2xlate=0),
        ptw_response_latency=32,
    )
    env.initialize(reset_vector=scenario.va, bare_mode=False)
    state = TranslationScenarioBuilder(env).build(scenario)
    env.monitor.clear()
    env.monitor.set_expected_pc(scenario.va)
    env.arm_translation_scenario(state)
    _wait_until(
        env,
        lambda: int(env.ptw_agent.get_stats()["pending"]) + int(env.ptw_agent.get_stats()["active_resp"]) > 0,
        description="inflight PTW response",
    )

    record = env.pulse_sfence(
        addr=_NEW_VA,
        rs1=1,
        rs2=1,
        ident=scenario.satp_asid,
        advance_translation_epoch=False,
    )

    assert record["addr"] == _NEW_VA
    _wait_until(env, lambda: _translation_complete(env), description="translation after unmatched SFENCE")
    env.assert_translation_scenario()
    env.translation_oracle.disarm()
    assert not env.get_errors()

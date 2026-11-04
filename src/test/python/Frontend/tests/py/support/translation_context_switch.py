"""Shared SATP context-switch stimulus and translation helpers."""

from dataclasses import replace

from env.core.transactions import RedirectTxn
from env.runtime.frontend_init import FrontendInitConfig, initialize_frontend
from env.sequences import (
    InjectRedirectSequence,
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from env.support import PmpPmaConfig, record_scenario, scenario_rng
from tests.py.support.dut_support import wait_until


OLD_VA = 0x8020_0F00
NEW_VA = 0x8020_2F00
OLD_PA = 0x8040_0F00
NEW_PA = 0x8040_2F00
OLD_GPA = 0x8060_0F00
PAYLOAD = b"\x13\x00\x00\x00" * 512


def randomize_context_timing(
    env,
    scenario: TranslationScenario,
    *,
    latency_min: int = 1,
    latency_max: int = 8,
    keep_latency_max: bool = False,
) -> TranslationScenario:
    scenario_key = f"zhaoxinran/translation/context/{scenario.scenario_id}"
    base_seed, seed, rng = scenario_rng(scenario_key)
    latency = rng.randint(int(latency_min), int(latency_max))
    response_latency_max = (
        rng.randint(latency, max(latency, int(scenario.ptw_response_latency_max)))
        if keep_latency_max and scenario.ptw_response_latency_max is not None
        else scenario.ptw_response_latency_max
    )
    randomized = replace(
        scenario,
        ptw_response_latency=latency,
        ptw_response_latency_max=response_latency_max,
        ptw_response_seed=seed,
    )
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "scenario_id": scenario.scenario_id,
            "va": scenario.va,
            "pa": scenario.pa,
            "s2xlate": scenario.s2xlate,
            "latency": latency,
            "latency_max": response_latency_max,
            "satp_asid": scenario.satp_asid,
            "vsatp_asid": scenario.vsatp_asid,
            "hgatp_vmid": scenario.hgatp_vmid,
            "expected_path": "context_or_sfence_retranslation",
        },
    )
    return randomized


def permissions() -> tuple[tuple[TranslationPmpPmaEntry, ...], tuple[TranslationPmpPmaEntry, ...]]:
    region_base = OLD_PA & ~0x3FFF
    return (
        (
            TranslationPmpPmaEntry(
                "pmp", 0, PmpPmaConfig(match="napot", read=True, execute=True),
                region_base, size=0x4000,
            ),
        ),
        (
            TranslationPmpPmaEntry(
                "pma", 0, PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True),
                region_base, size=0x4000,
            ),
        ),
    )


def scenario(
    scenario_id: str,
    *,
    va: int,
    pa: int,
    asid: int,
    payload: bytes = PAYLOAD,
    page_count: int = 2,
    **kwargs,
) -> TranslationScenario:
    pmp_entries, pma_entries = permissions()
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


def translation_complete(env) -> bool:
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


def run_satp_asid_switch(env) -> None:
    old = randomize_context_timing(
        env, scenario("translation-context-switch-old", va=OLD_VA, pa=OLD_PA, asid=3)
    )
    new = randomize_context_timing(
        env, scenario("translation-context-switch-new", va=NEW_VA, pa=NEW_PA, asid=9)
    )
    builder = TranslationScenarioBuilder(env)

    # Build the target oracle state before reset. The old context is built after
    # reset so its PMP/PMA writes remain live when the DUT starts fetching.
    new_state = builder.build(new)
    initialize_frontend(
        env, FrontendInitConfig(reset_vector=OLD_VA, bare_mode=False, reset_cycles=20)
    )
    old_state = builder.build(old)
    for page in range(new.page_count):
        env.page_table.map_page(
            (NEW_VA >> 12) + page,
            (NEW_PA >> 12) + page,
            **new.s1_pte.as_mapping_kwargs(),
        )

    env.monitor.clear()
    env.monitor.set_expected_pc(OLD_VA)
    env.arm_translation_scenario(old_state)

    wait_until(
        env,
        lambda: any(int(observation.pc) == OLD_VA for observation in env.monitor.observations),
        description="old-context cfVec source",
    )

    prepared_context = env.prepare_translation_context_change(satp_asid=new.satp_asid)
    prepared_sfence = env.prepare_sfence(addr=OLD_VA, rs1=1, rs2=1, ident=old.satp_asid)
    new_state = replace(new_state, translation_epoch=env.translation_epoch)
    env.monitor.set_expected_pc(NEW_VA)
    env.arm_translation_scenario(new_state)
    env.translation_oracle.set_fetch_observation_ready(ready=False)

    redirect = InjectRedirectSequence(
        RedirectTxn(
            source_pc=OLD_VA,
            target_pc=NEW_VA,
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
    wait_until(env, lambda: translation_complete(env), description="complete new-context translation")
    env.assert_translation_scenario()

    active = env.translation_oracle.get_active()
    assert active is not None
    assert active["translation_epoch"] == env.translation_epoch
    assert active["expected_ptw_requests"][0]["vpn"] == NEW_VA >> 12
    assert any(int(observation.pc) == NEW_VA for observation in env.monitor.observations)
    assert not env.get_errors()

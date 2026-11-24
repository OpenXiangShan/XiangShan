"""G-stage provenance stimulus shared by translation and IFU tests."""

from env.runtime.frontend_init import FrontendInitConfig, initialize_frontend
from env.sequences import (
    TranslationPte,
    TranslationPtwResponseOverride,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from tests.py.support.translation_scenarios import _randomize_ptw_response_timing


def scenario(
    scenario_id: str,
    *,
    va: int,
    gpa: int,
    pa: int,
    table_gpa: int,
    vsatp_ppn: int,
    fake_pte: bool,
    gpf: int,
    gaf: int,
) -> TranslationScenario:
    table_ppn = table_gpa >> 12
    s1_patch = (
        ("s1_entry_v", int(not fake_pte)),
        ("s1_entry_level", 0 if fake_pte else 1),
        ("s1_entry_ppn", table_ppn >> 3),
        ("s1_ppn_low", (table_ppn & 7,) * 8),
        ("s1_entry_perm_r", 0),
        ("s1_entry_perm_w", 0),
        ("s1_entry_perm_x", 0),
        ("s1_entry_perm_a", 0),
        ("s1_pf", 0),
        ("s1_af", 0),
    )
    return TranslationScenario(
        scenario_id=scenario_id,
        va=va,
        gpa=gpa,
        pa=pa,
        payload=b"\x13\x00\x00\x00" * 512,
        page_count=2,
        mode="sv39",
        stage2_mode="sv39",
        s2xlate=3,
        priv_virt=1,
        s1_pte=TranslationPte(asid=5, vmid=7),
        s2_pte=TranslationPte(vmid=7),
        vsatp_asid=5,
        vsatp_ppn=vsatp_ppn,
        hgatp_vmid=7,
        expected_path="fault",
        expected_result="guest_fault" if gpf else "access_fault",
        ptw_response_overrides=(
            TranslationPtwResponseOverride(
                vpn=va >> 12,
                s2xlate=3,
                patch=(*s1_patch, ("s2_gpf", gpf), ("s2_gaf", gaf)),
                vs_pte_fault=(table_gpa, 2 if fake_pte else 0) if gpf else None,
            ),
        ),
    )


def run_gstage_provenance(
    env,
    scenario: TranslationScenario,
    expected_fault: str,
) -> None:
    scenario = _randomize_ptw_response_timing(
        env,
        scenario,
        scenario_key=f"zhaoxinran/translation/gstage/{scenario.scenario_id}",
        parameters={
            "gpa": scenario.gpa,
            "expected_path": "fault",
            "expected_fault": expected_fault,
        },
    )
    initialize_frontend(
        env, FrontendInitConfig(reset_vector=scenario.va, bare_mode=False, reset_cycles=20)
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    def arm_before_reset_release() -> None:
        env.monitor.clear()
        env.monitor.set_expected_pc(scenario.va)
        env.arm_translation_scenario(state, page_indexes=(0,))

    env.reset(before_release=arm_before_reset_release)

    for _ in range(6000):
        env.step(1)
        active = env.translation_oracle.get_active()
        if active is not None and (
            active["fault_seen"]
            or env.get_errors()
            or env.translation_oracle.get_stats()["errors"]
        ):
            break

    assert state.expected_page_outcomes[0]["outcome"] == expected_fault
    checker_errors = env.translation_oracle.get_stats()["errors"]
    if checker_errors:
        raise AssertionError(f"translation checker failed: {checker_errors}")
    assert env.monitor.exception_mark_count > 0
    observations_after_fault = len(env.monitor.observations)
    env.step(64)
    assert len(env.monitor.observations) == observations_after_fault
    env.assert_translation_scenario()
    assert not env.get_errors()

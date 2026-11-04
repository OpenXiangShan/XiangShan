"""G-stage provenance stimulus shared by translation and IFU tests."""

from env.runtime.frontend_init import FrontendInitConfig, initialize_frontend
from env.sequences import (
    TranslationPte,
    TranslationPtwResponseOverride,
    TranslationScenario,
    TranslationScenarioBuilder,
)
from tests.py.support.translation_scenarios import _randomize_ptw_response_timing


VA = 0x8020_0F00
GPA = 0x8060_0F00
PA = 0x8040_0F00
PAYLOAD = b"\x13\x00\x00\x00" * 512


def scenario(
    scenario_id: str,
    *,
    s1_patch: tuple[tuple[str, int], ...],
    gpf: int,
    gaf: int,
) -> TranslationScenario:
    return TranslationScenario(
        scenario_id=scenario_id,
        va=VA,
        gpa=GPA,
        pa=PA,
        payload=PAYLOAD,
        page_count=2,
        mode="sv39",
        stage2_mode="sv39",
        s2xlate=3,
        priv_virt=1,
        s1_pte=TranslationPte(asid=5, vmid=7),
        s2_pte=TranslationPte(vmid=7),
        vsatp_asid=5,
        hgatp_vmid=7,
        expected_path="fault",
        expected_result="guest_fault" if gpf else "access_fault",
        ptw_response_overrides=(
            TranslationPtwResponseOverride(
                vpn=VA >> 12,
                s2xlate=3,
                patch=(*s1_patch, ("s2_gpf", gpf), ("s2_gaf", gaf)),
            ),
        ),
    )


def run_gstage_provenance(
    env,
    scenario: TranslationScenario,
    expected_fault: str,
    expect_gpaddr_write: bool,
) -> None:
    scenario = _randomize_ptw_response_timing(
        env,
        scenario,
        scenario_key=f"zhaoxinran/translation/gstage/{scenario.scenario_id}",
        parameters={
            "gpa": scenario.gpa,
            "expected_path": "fault",
            "expected_fault": expected_fault,
            "expect_gpaddr_write": expect_gpaddr_write,
        },
    )
    initialize_frontend(
        env, FrontendInitConfig(reset_vector=scenario.va, bare_mode=False, reset_cycles=20)
    )
    state = TranslationScenarioBuilder(env).build(scenario)
    gpaddr_writes = []

    def arm_before_reset_release() -> None:
        env.monitor.clear()
        env.monitor.set_expected_pc(scenario.va)
        env.arm_translation_scenario(state, page_indexes=(0,))

    env.reset(before_release=arm_before_reset_release)

    for _ in range(6000):
        env.step(1)
        gpaddr_if = env.backend_from_ifu_if
        if int(gpaddr_if.gpaddr_mem_wen.value):
            gpaddr_writes.append(
                (
                    int(gpaddr_if.gpaddr_mem_gpaddr.value),
                    int(gpaddr_if.gpaddr_mem_is_for_vs_nonleaf_pte.value),
                )
            )
        active = env.translation_oracle.get_active()
        if active is not None and (
            active["fault_seen"]
            or env.get_errors()
            or env.translation_oracle.get_stats()["errors"]
        ):
            break

    assert state.expected_page_outcomes[0]["outcome"] == expected_fault
    assert env.monitor.exception_mark_count > 0
    if expect_gpaddr_write:
        assert gpaddr_writes == [(GPA, 1)]
    else:
        assert not gpaddr_writes
    observations_after_fault = len(env.monitor.observations)
    env.step(64)
    assert len(env.monitor.observations) == observations_after_fault
    env.assert_translation_scenario()
    assert not env.get_errors()

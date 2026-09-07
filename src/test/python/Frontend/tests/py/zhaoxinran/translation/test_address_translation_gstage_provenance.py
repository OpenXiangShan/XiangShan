from __future__ import annotations

import os

import pytest

from env.sequences import (
    TranslationPte,
    TranslationPtwResponseOverride,
    TranslationScenario,
    TranslationScenarioBuilder,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_VA = 0x8020_0F00
_GPA = 0x8060_0F00
_PA = 0x8040_0F00
_PAYLOAD = b"\x13\x00\x00\x00" * 512


def _scenario(
    scenario_id: str,
    *,
    s1_patch: tuple[tuple[str, int], ...],
    gpf: int,
    gaf: int,
) -> TranslationScenario:
    return TranslationScenario(
        scenario_id=scenario_id,
        va=_VA,
        gpa=_GPA,
        pa=_PA,
        payload=_PAYLOAD,
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
                vpn=_VA >> 12,
                s2xlate=3,
                patch=(*s1_patch, ("s2_gpf", gpf), ("s2_gaf", gaf)),
            ),
        ),
    )


_PROVENANCE_CASES = (
    pytest.param(
        _scenario(
            "translation-gstage-fake-pte-gpf",
            s1_patch=(("s1_entry_v", 0), ("s1_pf", 0), ("s1_af", 0)),
            gpf=1,
            gaf=0,
        ),
        "instruction_guest_page_fault",
        True,
        id="fake-pte-gpf",
    ),
    pytest.param(
        _scenario(
            "translation-gstage-fake-pte-gaf",
            s1_patch=(("s1_entry_v", 0), ("s1_pf", 0), ("s1_af", 0)),
            gpf=0,
            gaf=1,
        ),
        "instruction_access_fault",
        False,
        id="fake-pte-gaf",
    ),
    pytest.param(
        _scenario(
            "translation-gstage-vs-nonleaf-gpf",
            s1_patch=(
                ("s1_entry_v", 1),
                ("s1_entry_perm_r", 0),
                ("s1_entry_perm_w", 0),
                ("s1_entry_perm_x", 0),
                ("s1_pf", 0),
                ("s1_af", 0),
            ),
            gpf=1,
            gaf=0,
        ),
        "instruction_guest_page_fault",
        True,
        id="vs-nonleaf-gpf",
    ),
    pytest.param(
        _scenario(
            "translation-gstage-vs-nonleaf-gaf",
            s1_patch=(
                ("s1_entry_v", 1),
                ("s1_entry_perm_r", 0),
                ("s1_entry_perm_w", 0),
                ("s1_entry_perm_x", 0),
                ("s1_pf", 0),
                ("s1_af", 0),
            ),
            gpf=0,
            gaf=1,
        ),
        "instruction_access_fault",
        False,
        id="vs-nonleaf-gaf",
    ),
)


@pytest.mark.parametrize("scenario,expected_fault,expect_gpaddr_write", _PROVENANCE_CASES)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_address_translation_gstage_provenance(
    env,
    scenario: TranslationScenario,
    expected_fault: str,
    expect_gpaddr_write: bool,
) -> None:
    env.initialize(reset_vector=scenario.va, bare_mode=False)
    state = TranslationScenarioBuilder(env).build(scenario)
    gpaddr_writes = []

    def arm_before_reset_release() -> None:
        env.monitor.clear()
        env.monitor.set_expected_pc(scenario.va)
        env.arm_translation_scenario(state, page_indexes=(0,))

    env.reset(before_release=arm_before_reset_release)

    for _ in range(6000):
        env.step(1)
        gpaddr_if = env.backend_observe_if
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
        assert gpaddr_writes == [(_GPA, 1)]
    else:
        assert not gpaddr_writes
    observations_after_fault = len(env.monitor.observations)
    env.step(64)
    assert len(env.monitor.observations) == observations_after_fault
    env.assert_translation_scenario()
    assert not env.get_errors()

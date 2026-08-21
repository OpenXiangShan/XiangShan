from __future__ import annotations

import os

import pytest

from env.sequences import (
    TranslationPte,
    TranslationScenario,
    TranslationScenarioBuilder,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_VA = 0x8020_0F00
_PA = 0x8040_0F00
_GPA = 0x8060_0F00
_PAGE_SIZE = 0x1000
_PAYLOAD = b"\x13\x00\x00\x00" * 512


def _scenario(scenario_id: str, **kwargs) -> TranslationScenario:
    return TranslationScenario(
        scenario_id=scenario_id,
        va=_VA,
        pa=_PA,
        payload=_PAYLOAD,
        page_count=2,
        expected_path="fault",
        **kwargs,
    )


_FAULT_CASES = (
    pytest.param(
        _scenario(
            "translation-fault-stage1-only-response-page",
            mode="sv39",
            s2xlate=1,
            priv_virt=1,
            s1_pf=1,
            expected_result="page_fault",
        ),
        "instruction_page_fault",
        id="stage1-only-response-page-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-stage1-only-response-access",
            mode="sv39",
            s2xlate=1,
            priv_virt=1,
            s1_af=1,
            expected_result="access_fault",
        ),
        "instruction_access_fault",
        id="stage1-only-response-access-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-stage1-pte-invalid",
            mode="sv39",
            s1_pte=TranslationPte(v=0),
            expected_result="page_fault",
        ),
        "instruction_page_fault",
        id="stage1-pte-invalid-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-stage1-pte-accessed-clear",
            mode="sv39",
            s1_pte=TranslationPte(a=0),
            expected_result="page_fault",
        ),
        "instruction_page_fault",
        id="stage1-pte-accessed-clear-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-only-stage2-guest-page",
            mode="bare",
            stage2_mode="sv39",
            s2xlate=2,
            priv_virt=1,
            s2_gpf=1,
            expected_result="guest_fault",
        ),
        "instruction_guest_page_fault",
        id="only-stage2-guest-page-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-only-stage2-guest-access",
            mode="bare",
            stage2_mode="sv39",
            s2xlate=2,
            priv_virt=1,
            s2_gaf=1,
            expected_result="access_fault",
        ),
        "instruction_access_fault",
        id="only-stage2-guest-access-fault",
    ),
    pytest.param(
        _scenario(
            "translation-fault-all-stage-guest-access",
            mode="sv39",
            stage2_mode="sv39",
            s2xlate=3,
            gpa=_GPA,
            s1_pte=TranslationPte(asid=5, vmid=7),
            s2_pte=TranslationPte(vmid=7),
            vsatp_asid=5,
            hgatp_vmid=7,
            priv_virt=1,
            s2_gaf=1,
            expected_result="access_fault",
        ),
        "instruction_access_fault",
        id="all-stage-guest-access-fault",
    ),
)


@pytest.mark.parametrize("scenario,expected_fault", _FAULT_CASES)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_address_translation_fault(env, scenario: TranslationScenario, expected_fault: str) -> None:
    env.initialize(reset_vector=scenario.va, bare_mode=False)
    state = TranslationScenarioBuilder(env).build(scenario)

    def arm_before_reset_release() -> None:
        env.monitor.clear()
        env.monitor.set_expected_pc(scenario.va)
        env.arm_translation_scenario(state, page_indexes=(0,))

    env.reset(before_release=arm_before_reset_release)

    for _ in range(6000):
        active = env.translation_oracle.get_active()
        if active is not None and (active["fault_seen"] or env.get_errors()):
            break
        env.step(1)

    assert state.expected_page_outcomes[0]["ok"] is False
    assert state.expected_page_outcomes[0]["outcome"] == expected_fault
    assert env.monitor.exception_mark_count > 0
    observations_after_fault = len(env.monitor.observations)
    env.step(64)
    assert len(env.monitor.observations) == observations_after_fault
    env.assert_translation_scenario()
    assert not env.get_errors()

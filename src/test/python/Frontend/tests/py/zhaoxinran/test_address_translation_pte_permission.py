from __future__ import annotations

import os

import pytest

from env.sequences import TranslationPmpPmaEntry, TranslationPte, TranslationScenario, TranslationScenarioBuilder
from env.support import PmpPmaConfig


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_VA = 0x8020_0F00
_PA = 0x8040_0F00
_GPA = 0x8060_0F00
_PAYLOAD = b"\x13\x00\x00\x00" * 512


def _s1_scenario(
    scenario_id: str,
    *,
    pte: TranslationPte,
    priv_imode: int = 1,
    **kwargs,
) -> TranslationScenario:
    return TranslationScenario(
        scenario_id=scenario_id,
        va=_VA,
        pa=_PA,
        payload=_PAYLOAD,
        page_count=2,
        mode="sv39",
        s1_pte=pte,
        priv_imode=priv_imode,
        expected_path="fault",
        expected_result="page_fault",
        **kwargs,
    )


def _s2_scenario(scenario_id: str, *, pte: TranslationPte, all_stage: bool) -> TranslationScenario:
    common = {
        "scenario_id": scenario_id,
        "va": _VA,
        "pa": _PA,
        "payload": _PAYLOAD,
        "page_count": 2,
        "stage2_mode": "sv39",
        "priv_virt": 1,
        "s2_pte": pte,
        "expected_path": "fault",
        "expected_result": "guest_fault",
    }
    if not all_stage:
        return TranslationScenario(mode="bare", s2xlate=2, **common)
    return TranslationScenario(
        mode="sv39",
        s2xlate=3,
        gpa=_GPA,
        s1_pte=TranslationPte(asid=5, vmid=7),
        s2_pte=TranslationPte(**{**pte.as_mapping_kwargs(), "vmid": 7}),
        vsatp_asid=5,
        hgatp_vmid=7,
        **{key: value for key, value in common.items() if key != "s2_pte"},
    )


_PTE_PERMISSION_CASES = (
    pytest.param(
        _s1_scenario(
            "translation-pte-s1-user-denied",
            pte=TranslationPte(u=0),
            priv_imode=0,
        ),
        "instruction_page_fault",
        id="stage1-user-denied",
    ),
    pytest.param(
        _s1_scenario("translation-pte-s1-supervisor-denied", pte=TranslationPte(u=1)),
        "instruction_page_fault",
        id="stage1-supervisor-denied",
    ),
    pytest.param(
        _s1_scenario("translation-pte-s1-write-without-read", pte=TranslationPte(r=0, w=1)),
        "instruction_page_fault",
        id="stage1-write-without-read",
    ),
    pytest.param(
        _s1_scenario("translation-pte-s1-execute-denied", pte=TranslationPte(x=0)),
        "instruction_page_fault",
        id="stage1-execute-denied",
    ),
    pytest.param(
        _s1_scenario(
            "translation-pte-s1-reserved-pbmt",
            pte=TranslationPte(pbmt=3),
            ptw_machine_pbmte=1,
            pmp_entries=(
                TranslationPmpPmaEntry(
                    "pmp",
                    0,
                    PmpPmaConfig(match="napot", read=True, execute=True),
                    _PA & ~0x1FFF,
                    size=0x2000,
                ),
            ),
            pma_entries=(
                TranslationPmpPmaEntry(
                    "pma",
                    0,
                    PmpPmaConfig(match="napot", read=True, execute=True, cacheable=True),
                    _PA & ~0x1FFF,
                    size=0x2000,
                ),
            ),
        ),
        "instruction_page_fault",
        id="stage1-reserved-pbmt",
    ),
    *(
        pytest.param(
            _s2_scenario(
                f"translation-pte-{'all-stage' if all_stage else 'only-stage2'}-{name}",
                pte=TranslationPte(**pte_kwargs),
                all_stage=all_stage,
            ),
            "instruction_guest_page_fault",
            id=f"{'all-stage' if all_stage else 'only-stage2'}-{name}",
        )
        for all_stage in (False, True)
        for name, pte_kwargs in (
            ("invalid", {"v": 0}),
            ("execute-denied", {"x": 0}),
            ("accessed-clear", {"a": 0}),
            ("write-without-read", {"r": 0, "w": 1}),
        )
    ),
)


@pytest.mark.parametrize("scenario,expected_fault", _PTE_PERMISSION_CASES)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_address_translation_pte_permission(env, scenario: TranslationScenario, expected_fault: str) -> None:
    env.initialize(reset_vector=scenario.va, bare_mode=False)
    state = TranslationScenarioBuilder(env).build(scenario)

    def arm_before_reset_release() -> None:
        env.monitor.clear()
        env.monitor.set_expected_pc(scenario.va)
        env.arm_translation_scenario(state, page_indexes=(0,))

    env.reset(before_release=arm_before_reset_release)

    for _ in range(6000):
        active = env.translation_oracle.get_active()
        if active is not None and (
            active["fault_seen"]
            or env.get_errors()
            or env.translation_oracle.get_stats()["errors"]
        ):
            break
        env.step(1)

    assert state.expected_page_outcomes[0]["outcome"] == expected_fault
    assert env.monitor.exception_mark_count > 0
    observations_after_fault = len(env.monitor.observations)
    env.step(64)
    assert len(env.monitor.observations) == observations_after_fault
    env.assert_translation_scenario()
    assert not env.get_errors()

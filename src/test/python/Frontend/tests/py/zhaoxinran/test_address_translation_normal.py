from __future__ import annotations

import os

import pytest

from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationScenario,
    TranslationScenarioPhase,
    TranslationScenarioSequence,
    TranslationSectorLane,
)
from env.support import PmpPmaConfig


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_VA = 0x8020_0F00
_PA = 0x8040_0F00
_GPA = 0x8060_0F00
_PAGE_SIZE = 0x1000
_PAYLOAD = b"\x13\x00\x00\x00" * 512


def _pmp_allow() -> tuple[TranslationPmpPmaEntry, ...]:
    return (
        TranslationPmpPmaEntry(
            "pmp",
            0,
            PmpPmaConfig(match="napot", read=True, execute=True),
            _PA & ~(_PAGE_SIZE - 1),
            size=0x2000,
        ),
    )


def _pma(*, first_cacheable: bool, second_cacheable: bool | None = None) -> tuple[TranslationPmpPmaEntry, ...]:
    page_base = _PA & ~(_PAGE_SIZE - 1)
    if second_cacheable is None:
        return (
            TranslationPmpPmaEntry(
                "pma",
                0,
                PmpPmaConfig(match="napot", read=True, execute=True, cacheable=first_cacheable),
                page_base,
                size=0x2000,
            ),
        )
    return (
        TranslationPmpPmaEntry(
            "pma",
            0,
            PmpPmaConfig(match="napot", read=True, execute=True, cacheable=first_cacheable),
            page_base,
            size=_PAGE_SIZE,
        ),
        TranslationPmpPmaEntry(
            "pma",
            1,
            PmpPmaConfig(match="napot", read=True, execute=True, cacheable=second_cacheable),
            page_base + _PAGE_SIZE,
            size=_PAGE_SIZE,
        ),
    )


def _scenario(
    scenario_id: str,
    *,
    expected_paths: tuple[str, str] = ("cacheable", "cacheable"),
    va: int = _VA,
    pa: int = _PA,
    **kwargs,
) -> tuple[TranslationScenario, tuple[str, str]]:
    first_cacheable = expected_paths[0] == "cacheable"
    second_cacheable = expected_paths[1] == "cacheable"
    return (
        TranslationScenario(
            scenario_id=scenario_id,
            va=va,
            pa=pa,
            payload=_PAYLOAD,
            page_count=2,
            pmp_entries=_pmp_allow(),
            pma_entries=_pma(
                first_cacheable=first_cacheable,
                second_cacheable=None if first_cacheable == second_cacheable else second_cacheable,
            ),
            **kwargs,
        ),
        expected_paths,
    )


_NORMAL_CASES = (
    pytest.param(
        *_scenario("translation-normal-bare-cross-page", va=_PA, pa=_PA, mode="bare"),
        id="bare-cross-page",
    ),
    pytest.param(
        *_scenario("translation-normal-stage1-sv39-cross-page", mode="sv39"),
        id="stage1-sv39-cross-page",
    ),
    pytest.param(
        *_scenario("translation-normal-stage1-sv48-cross-page", mode="sv48"),
        id="stage1-sv48-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-only-stage2-cross-page",
            mode="bare",
            stage2_mode="sv39",
            s2xlate=2,
            priv_virt=1,
        ),
        id="only-stage2-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-all-stage-cross-page",
            mode="sv39",
            stage2_mode="sv39",
            s2xlate=3,
            gpa=_GPA,
            s1_pte=TranslationPte(asid=5, vmid=7),
            s2_pte=TranslationPte(vmid=7),
            vsatp_asid=5,
            hgatp_vmid=7,
            priv_virt=1,
        ),
        id="all-stage-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-superpage-cross-page",
            mode="sv39",
            s1_pte=TranslationPte(level=1),
        ),
        id="superpage-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-sector-valid-lane-cross-page",
            mode="sv39",
            s1_sector_lanes=(TranslationSectorLane(lane=1, ppn=(_PA >> 12) + 1),),
        ),
        id="sector-valid-lane-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-uncache-cross-page",
            mode="sv39",
            expected_paths=("uncache", "uncache"),
        ),
        id="uncache-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-cacheable-to-uncache-cross-page",
            mode="sv39",
            expected_paths=("cacheable", "uncache"),
        ),
        id="cacheable-to-uncache-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-ptw-latency-cross-page",
            mode="sv39",
            ptw_response_latency=3,
            ptw_response_latency_max=7,
            ptw_response_seed=0x5A390002,
        ),
        id="ptw-latency-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-ptw-periodic-ready-cross-page",
            mode="sv39",
            ptw_req_ready_strategy="periodic",
            ptw_req_ready_high_cycles=1,
            ptw_req_ready_low_cycles=2,
        ),
        id="ptw-periodic-ready-cross-page",
    ),
)


@pytest.mark.parametrize("scenario,expected_paths", _NORMAL_CASES)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_address_translation_normal(env, scenario: TranslationScenario, expected_paths: tuple[str, str]) -> None:
    sequence = TranslationScenarioSequence(actions=(TranslationScenarioPhase(scenario=scenario),))
    sequence.initialize_first_phase(env)
    phase_results = [record for record in sequence.run(env) if record["kind"] == "phase"]

    assert len(phase_results) == 1
    state = phase_results[0]["state"]
    assert len(state.expected_page_outcomes) == 2
    assert [(outcome["ok"], outcome["expected_path"]) for outcome in state.expected_page_outcomes] == [
        (True, expected_paths[0]),
        (True, expected_paths[1]),
    ]

    observed_pcs = [int(observation.pc) for observation in env.monitor.observations]
    for outcome in state.expected_page_outcomes:
        page_va = int(outcome["va"])
        assert any(page_va <= pc < page_va + _PAGE_SIZE for pc in observed_pcs), (
            f"{scenario.scenario_id} did not deliver a normal cfVec from VA page 0x{page_va:x}"
        )

    assert not env.get_errors()

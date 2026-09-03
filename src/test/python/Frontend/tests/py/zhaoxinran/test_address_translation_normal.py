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
_SECTOR_VA = _VA + _PAGE_SIZE
_SECTOR_PA = _PA + _PAGE_SIZE
_HIGH_SECTOR_VA = _VA + 6 * _PAGE_SIZE
_HIGH_SECTOR_PA = _PA + 6 * _PAGE_SIZE
_WIDE_SECTOR_VA = _VA
_WIDE_SECTOR_PA = _PA
_PMA_REGION_BASE = _PA & ~0x3FFF
_PMA_BOUNDARY_VA = (_VA & ~(_PAGE_SIZE - 1)) + _PAGE_SIZE
_PMA_BOUNDARY_PA = _PMA_REGION_BASE + _PAGE_SIZE
_PAYLOAD = b"\x13\x00\x00\x00" * 512
_THREE_PAGE_PAYLOAD = b"\x13\x00\x00\x00" * 1089
_FOUR_PAGE_PAYLOAD = b"\x13\x00\x00\x00" * 2113


def _pmp_allow(*, base: int = _PA, size: int = 0x2000) -> tuple[TranslationPmpPmaEntry, ...]:
    return (
        TranslationPmpPmaEntry(
            "pmp",
            0,
            PmpPmaConfig(match="napot", read=True, execute=True),
            base & ~(size - 1),
            size=size,
        ),
    )


def _pma(
    *,
    first_cacheable: bool,
    second_cacheable: bool | None = None,
    base: int = _PA,
    size: int = 0x2000,
) -> tuple[TranslationPmpPmaEntry, ...]:
    page_base = base & ~(size - 1) if second_cacheable is None else base & ~(_PAGE_SIZE - 1)
    if second_cacheable is None:
        return (
            TranslationPmpPmaEntry(
                "pma",
                0,
                PmpPmaConfig(match="napot", read=True, execute=True, cacheable=first_cacheable),
                page_base,
                size=size,
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
    expected_paths: tuple[str, ...] = ("cacheable", "cacheable"),
    va: int = _VA,
    pa: int = _PA,
    page_count: int = 2,
    payload: bytes = _PAYLOAD,
    physical_size: int = 0x2000,
    **kwargs,
) -> tuple[TranslationScenario, tuple[str, ...]]:
    if len(expected_paths) != int(page_count):
        raise ValueError("translation normal expected paths must cover every declared page")
    if len(set(expected_paths)) > 1 and int(page_count) != 2:
        raise ValueError("mixed cacheability is currently defined only for two-page normal scenarios")
    first_cacheable = expected_paths[0] == "cacheable"
    second_cacheable = expected_paths[1] == "cacheable"
    return (
        TranslationScenario(
        scenario_id=scenario_id,
        va=va,
        pa=pa,
        payload=payload,
        page_count=page_count,
        pmp_entries=_pmp_allow(base=pa, size=physical_size),
        pma_entries=_pma(
            first_cacheable=first_cacheable,
            second_cacheable=None if first_cacheable == second_cacheable else second_cacheable,
            base=pa,
            size=physical_size,
            ),
            **kwargs,
        ),
        expected_paths,
    )


def _pbmt_nc_pma_boundary_scenario(
    scenario_id: str,
    *,
    va: int,
    pa: int,
    page_count: int = 2,
    payload: bytes = _PAYLOAD,
) -> tuple[TranslationScenario, tuple[str, ...]]:
    return (
        TranslationScenario(
            scenario_id=scenario_id,
            va=va,
            pa=pa,
            payload=payload,
            page_count=page_count,
            mode="sv39",
            s1_pte=TranslationPte(pbmt=1),
            ptw_machine_pbmte=1,
            pmp_entries=(
                TranslationPmpPmaEntry(
                    "pmp",
                    0,
                    PmpPmaConfig(match="napot", read=True, execute=True),
                    _PMA_REGION_BASE,
                    size=0x4000,
                ),
            ),
            pma_entries=tuple(
                TranslationPmpPmaEntry(
                    "pma",
                    index,
                    PmpPmaConfig(
                        match="napot",
                        read=True,
                        execute=True,
                        cacheable=cacheable,
                    ),
                    _PMA_REGION_BASE + index * _PAGE_SIZE,
                    size=_PAGE_SIZE,
                )
                for index, cacheable in enumerate((True, False, True))
            ),
        ),
        ("uncache",) * page_count,
    )


_NORMAL_CASES = (
    pytest.param(
        *_scenario("translation-normal-bare-cross-page", va=_PA, pa=_PA, mode="bare"),
        id="bare-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-virtual-bare-cross-page",
            mode="sv39",
            s2xlate=1,
            priv_virt=1,
        ),
        id="virtual-bare-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-bare-mmio-cross-page",
            va=_PA,
            pa=_PA,
            mode="bare",
            expected_paths=("uncache", "uncache"),
        ),
        id="bare-mmio-cross-page",
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
            "translation-normal-only-stage1-sv48-cross-page",
            mode="sv48",
            s2xlate=1,
            s1_pte=TranslationPte(asid=5),
            vsatp_asid=5,
            priv_virt=1,
        ),
        id="only-stage1-sv48-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-only-stage1-pbmt-nc-cross-page",
            mode="sv39",
            s2xlate=1,
            s1_pte=TranslationPte(asid=5, pbmt=1),
            vsatp_asid=5,
            priv_virt=1,
            expected_paths=("uncache", "uncache"),
        ),
        id="only-stage1-pbmt-nc-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-only-stage2-sv48-cross-page",
            mode="bare",
            stage2_mode="sv48",
            s2xlate=2,
            gpa=_GPA,
            s2_pte=TranslationPte(vmid=7),
            hgatp_vmid=7,
            priv_virt=1,
        ),
        id="only-stage2-sv48-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-only-stage2-pbmt-nc-cross-page",
            mode="bare",
            stage2_mode="sv39",
            s2xlate=2,
            gpa=_GPA,
            s2_pte=TranslationPte(vmid=7, pbmt=1),
            hgatp_vmid=7,
            priv_virt=1,
            expected_paths=("uncache", "uncache"),
        ),
        id="only-stage2-pbmt-nc-cross-page",
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
            "translation-normal-all-stage-sv48-sv39-cross-page",
            mode="sv48",
            stage2_mode="sv39",
            s2xlate=3,
            gpa=_GPA,
            s1_pte=TranslationPte(asid=5, vmid=7),
            s2_pte=TranslationPte(vmid=7),
            vsatp_asid=5,
            hgatp_vmid=7,
            priv_virt=1,
        ),
        id="all-stage-sv48-sv39-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-all-stage-sv39-sv48-cross-page",
            mode="sv39",
            stage2_mode="sv48",
            s2xlate=3,
            gpa=_GPA,
            s1_pte=TranslationPte(asid=5, vmid=7),
            s2_pte=TranslationPte(vmid=7),
            vsatp_asid=5,
            hgatp_vmid=7,
            priv_virt=1,
        ),
        id="all-stage-sv39-sv48-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-all-stage-sv48-sv48-cross-page",
            mode="sv48",
            stage2_mode="sv48",
            s2xlate=3,
            gpa=_GPA,
            s1_pte=TranslationPte(asid=5, vmid=7),
            s2_pte=TranslationPte(vmid=7),
            vsatp_asid=5,
            hgatp_vmid=7,
            priv_virt=1,
        ),
        id="all-stage-sv48-sv48-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-all-stage-mmio-cross-page",
            mode="sv39",
            stage2_mode="sv39",
            s2xlate=3,
            gpa=_GPA,
            s1_pte=TranslationPte(asid=5, vmid=7),
            s2_pte=TranslationPte(vmid=7),
            vsatp_asid=5,
            hgatp_vmid=7,
            priv_virt=1,
            expected_paths=("uncache", "uncache"),
        ),
        id="all-stage-mmio-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-all-stage-g-pbmt-nc-cross-page",
            mode="sv39",
            stage2_mode="sv39",
            s2xlate=3,
            gpa=_GPA,
            s1_pte=TranslationPte(asid=5, vmid=7),
            s2_pte=TranslationPte(vmid=7, pbmt=1),
            vsatp_asid=5,
            hgatp_vmid=7,
            priv_virt=1,
            expected_paths=("uncache", "uncache"),
        ),
        id="all-stage-g-pbmt-nc-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-all-stage-vs-pbmt-nc-cross-page",
            mode="sv39",
            stage2_mode="sv39",
            s2xlate=3,
            gpa=_GPA,
            s1_pte=TranslationPte(asid=5, vmid=7, pbmt=1),
            s2_pte=TranslationPte(vmid=7),
            vsatp_asid=5,
            hgatp_vmid=7,
            priv_virt=1,
            expected_paths=("uncache", "uncache"),
        ),
        id="all-stage-vs-pbmt-nc-cross-page",
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
            "translation-normal-superpage-three-page",
            mode="sv39",
            expected_paths=("cacheable", "cacheable", "cacheable"),
            page_count=3,
            payload=_THREE_PAGE_PAYLOAD,
            physical_size=0x4000,
            s1_pte=TranslationPte(level=1),
        ),
        id="superpage-three-page",
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
            "translation-normal-sector-three-page-multi-lane",
            mode="sv39",
            va=_SECTOR_VA,
            pa=_SECTOR_PA,
            expected_paths=("cacheable", "cacheable", "cacheable"),
            page_count=3,
            payload=_THREE_PAGE_PAYLOAD,
            physical_size=0x4000,
            s1_sector_lanes=(
                TranslationSectorLane(lane=2, ppn=(_SECTOR_PA >> 12) + 1),
                TranslationSectorLane(lane=3, ppn=(_SECTOR_PA >> 12) + 2),
            ),
        ),
        id="sector-three-page-multi-lane",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-sector-high-lane-cross-page",
            mode="sv39",
            va=_HIGH_SECTOR_VA,
            pa=_HIGH_SECTOR_PA,
            physical_size=0x2000,
            s1_sector_lanes=(TranslationSectorLane(lane=7, ppn=(_HIGH_SECTOR_PA >> 12) + 1),),
        ),
        id="sector-high-lane-cross-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-sector-wide-lanes-four-page",
            mode="sv39",
            va=_WIDE_SECTOR_VA,
            pa=_WIDE_SECTOR_PA,
            expected_paths=("cacheable", "cacheable", "cacheable", "cacheable"),
            page_count=4,
            payload=_FOUR_PAGE_PAYLOAD,
            physical_size=0x4000,
            s1_sector_lanes=(
                TranslationSectorLane(lane=1, ppn=(_WIDE_SECTOR_PA >> 12) + 1),
                TranslationSectorLane(lane=2, ppn=(_WIDE_SECTOR_PA >> 12) + 2),
                TranslationSectorLane(lane=3, ppn=(_WIDE_SECTOR_PA >> 12) + 3),
            ),
        ),
        id="sector-wide-lanes-four-page",
    ),
    pytest.param(
        *_scenario(
            "translation-normal-all-stage-sector-invalid-lane-rewalk",
            mode="sv39",
            stage2_mode="sv39",
            s2xlate=3,
            gpa=_GPA,
            s1_pte=TranslationPte(asid=5, vmid=7),
            s2_pte=TranslationPte(vmid=7),
            vsatp_asid=5,
            hgatp_vmid=7,
            priv_virt=1,
            s1_sector_lanes=(
                TranslationSectorLane(lane=0, ppn=(_GPA >> 12) + 1, valid=0, pte_present=1),
            ),
        ),
        id="all-stage-sector-invalid-lane-rewalk",
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
        *_pbmt_nc_pma_boundary_scenario(
            "translation-normal-pbmt-nc-pma-edge",
            va=_PMA_BOUNDARY_VA - 4,
            pa=_PMA_BOUNDARY_PA - 4,
        ),
        id="pbmt-nc-pma-edge",
    ),
    pytest.param(
        *_pbmt_nc_pma_boundary_scenario(
            "translation-normal-pbmt-nc-after-pma-boundary",
            va=_PMA_BOUNDARY_VA,
            pa=_PMA_BOUNDARY_PA,
            page_count=1,
        ),
        id="pbmt-nc-after-pma-boundary",
    ),
    pytest.param(
        *_pbmt_nc_pma_boundary_scenario(
            "translation-normal-pbmt-nc-cross-pma-regions",
            va=_PMA_BOUNDARY_VA - 2,
            pa=_PMA_BOUNDARY_PA - 2,
        ),
        id="pbmt-nc-cross-pma-regions",
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
            "translation-normal-uncache-to-cacheable-cross-page",
            mode="sv39",
            expected_paths=("uncache", "cacheable"),
        ),
        id="uncache-to-cacheable-cross-page",
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
def test_address_translation_normal(env, scenario: TranslationScenario, expected_paths: tuple[str, ...]) -> None:
    sequence = TranslationScenarioSequence(actions=(TranslationScenarioPhase(scenario=scenario),))
    sequence.initialize_first_phase(env)
    phase_results = [record for record in sequence.run(env) if record["kind"] == "phase"]

    assert len(phase_results) == 1
    state = phase_results[0]["state"]
    assert len(state.expected_page_outcomes) == len(expected_paths)
    assert [(outcome["ok"], outcome["expected_path"]) for outcome in state.expected_page_outcomes] == [
        (True, path) for path in expected_paths
    ]

    observed_pcs = [int(observation.pc) for observation in env.monitor.observations]
    for outcome in state.expected_page_outcomes:
        page_va = int(outcome["va"])
        assert any(page_va <= pc < page_va + _PAGE_SIZE for pc in observed_pcs), (
            f"{scenario.scenario_id} did not deliver a normal cfVec from VA page 0x{page_va:x}"
        )

    assert not env.get_errors()

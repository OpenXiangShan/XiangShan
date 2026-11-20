from __future__ import annotations

import os
from dataclasses import replace

import pytest

from env.sequences import (
    TranslationPmpPmaEntry,
    TranslationPte,
    TranslationScenario,
    TranslationScenarioPhase,
    TranslationScenarioSequence,
    TranslationSectorLane,
)
from env.support import PcMapping, PmpPmaConfig, random_pc, record_scenario, scenario_rng
from tests.py.support.cross_page_cases import run_mapped_cross_page_rvi
from tests.py.support.translation_scenarios import executable_napot_entry
from tests.py.zhaoxinran.support.translation import randomize_ptw_timing, relocate_translation_entry


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


def _randomize_normal_ptw_timing(
    env,
    scenario: TranslationScenario,
    expected_paths: tuple[str, ...],
) -> TranslationScenario:
    scenario_key = f"zhaoxinran/translation/normal/{scenario.scenario_id}"
    base_seed, seed, rng = scenario_rng(scenario_key)
    _, placement_seed, placement_rng = scenario_rng(f"{scenario_key}/pc")
    entry = random_pc(
        placement_rng,
        mappings=(PcMapping(scenario.va & ~0x1F_FFFF, scenario.pa & ~0x1F_FFFF, 0x20_0000),),
        entry_offsets=tuple(slot * 0x8000 + (scenario.va & 0x7FFF) for slot in range(64)),
        instruction_bytes=4,
    )
    lane_delta = entry.va - scenario.va if scenario.gpa is not None else entry.pa - scenario.pa
    relocated = relocate_translation_entry(scenario, entry, s1_ppn_delta=lane_delta >> 12)
    has_ptw = scenario.mode != "bare" or int(scenario.s2xlate) in (2, 3)
    randomized = randomize_ptw_timing(relocated, rng, seed=seed, latency_limit=5 if has_ptw else None)
    record_scenario(
        env,
        scenario_key,
        base_seed=base_seed,
        seed=seed,
        parameters={
            "scenario_id": scenario.scenario_id,
            "va": randomized.va,
            "pa": randomized.pa,
            "gpa": randomized.gpa,
            "placement_seed": placement_seed,
            "page_count": scenario.page_count,
            "translation_mode": scenario.mode,
            "stage2_mode": scenario.stage2_mode,
            "s2xlate": scenario.s2xlate,
            "ptw_timing_randomized": has_ptw,
            "latency": randomized.ptw_response_latency,
            "latency_max": randomized.ptw_response_latency_max,
            "ready_strategy": scenario.ptw_req_ready_strategy,
            "ready_high_cycles": randomized.ptw_req_ready_high_cycles,
            "ready_low_cycles": randomized.ptw_req_ready_low_cycles,
            "expected_paths": expected_paths,
        },
    )
    return randomized


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
    region_base = pa & ~(physical_size - 1)
    if first_cacheable == second_cacheable:
        pma_entries = (executable_napot_entry("pma", region_base, size=physical_size, cacheable=first_cacheable),)
    else:
        page_base = pa & ~(_PAGE_SIZE - 1)
        pma_entries = tuple(
            executable_napot_entry(
                "pma", page_base + index * _PAGE_SIZE, size=_PAGE_SIZE, index=index, cacheable=cacheable,
            )
            for index, cacheable in enumerate((first_cacheable, second_cacheable))
        )
    return (
        TranslationScenario(
        scenario_id=scenario_id,
        va=va,
        pa=pa,
        payload=payload,
        page_count=page_count,
            pmp_entries=(executable_napot_entry("pmp", region_base, size=physical_size),),
            pma_entries=pma_entries,
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


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_cacheable_cross_page_rvi_with_two_mapped_pages(env) -> None:
    run_mapped_cross_page_rvi(env)


@pytest.mark.parametrize("scenario,expected_paths", _NORMAL_CASES)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_address_translation_normal(env, scenario: TranslationScenario, expected_paths: tuple[str, ...]) -> None:
    scenario = _randomize_normal_ptw_timing(env, scenario, expected_paths)
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


def _assert_all_stage_pbmt_route(
    env, *, stage1_pbmt: int, stage2_pbmt: int, pma_cacheable: bool
) -> None:
    # Keep NC and IO distinct even though both use the InstrUncache port.
    effective_pbmt = stage1_pbmt if stage1_pbmt else stage2_pbmt
    assert effective_pbmt in (1, 2)
    scenario, expected_paths = _scenario(
        f"translation-normal-all-stage-vs-{stage1_pbmt}-g-{stage2_pbmt}-pma-{int(pma_cacheable)}",
        mode="sv39",
        stage2_mode="sv39",
        s2xlate=3,
        gpa=_GPA,
        s1_pte=TranslationPte(asid=5, vmid=7, pbmt=stage1_pbmt),
        s2_pte=TranslationPte(vmid=7, pbmt=stage2_pbmt),
        vsatp_asid=5,
        hgatp_vmid=7,
        priv_virt=1,
        expected_paths=("uncache", "uncache"),
    )
    scenario = replace(
        scenario, pma_entries=(executable_napot_entry("pma", _PA & ~0x1FFF, size=0x2000, cacheable=pma_cacheable),),
    )
    scenario = _randomize_normal_ptw_timing(env, scenario, expected_paths)
    pa_lo = scenario.pa & ~(_PAGE_SIZE - 1)
    pa_hi = pa_lo + scenario.page_count * _PAGE_SIZE - 1
    if not pma_cacheable:
        env.memory.mmio_ranges.append((pa_lo, pa_hi))
    sequence = TranslationScenarioSequence(actions=(TranslationScenarioPhase(scenario=scenario),))
    sequence.initialize_first_phase(env)

    # Read actual state registers with no default/toggle fallback. These samples
    # prove route selection, not a blocked-backend latency or canonical HIT.
    from tests.py.support import uncache_scenarios as uncache

    samples = []
    prefix = "Frontend_top.Frontend.inner_ifu.uncacheUnit."

    def capture(cycle, current_env):
        read = lambda field: uncache._require_first_dut_signal(current_env, (prefix + field,))
        state = read("uncacheState")
        raw_pa = read("uncachePAddr_addr")
        # PrunedAddr removes instOffsetBits=1; restore its full byte PA.
        pa = raw_pa << 1
        if state and pa_lo <= pa <= pa_hi:
            samples.append({"cycle": int(cycle), "state": state, "raw_pa": raw_pa, "pa": pa,
                            "pbmt": read("itlbPbmt"), "is_mmio": read("isMmio")})

    env.register_pre_drive_cycle_observer(capture)
    phases = [r for r in sequence.run(env) if r["kind"] == "phase"]
    assert len(phases) == 1
    outcomes = phases[0]["state"].expected_page_outcomes
    assert [(o["ok"], o["expected_path"]) for o in outcomes] == [(True, "uncache")] * 2
    assert {o["fetch_path"] for o in outcomes} == {"uncache" if effective_pbmt == 1 else "mmio"}
    for outcome in outcomes:
        va = int(outcome["va"])
        assert any(va <= int(o.pc) < va + _PAGE_SIZE for o in env.monitor.observations)
    assert not env.translation_checker.errors
    assert not env.get_errors()
    requests = [r for r in env.uncache_agent.request_records if pa_lo <= int(r["address"]) <= pa_hi]
    assert requests, "no InstrUncache request reached the translated PA pages"
    assert {int(r["mem_page_type_nc"]) for r in requests} == {int(effective_pbmt == 1)}
    assert {int(r["mem_back_type_mm"]) for r in requests} == {int(pma_cacheable)}
    assert all(env.memory.is_mmio(int(r["address"])) == (not pma_cacheable) for r in requests)
    assert samples, "missing native state evidence for the translated PA pages"
    assert {s["pbmt"] for s in samples} == {effective_pbmt}
    assert {s["is_mmio"] for s in samples} == {int(not pma_cacheable)}
    # RTL enters WaitLastCommit for PMA MMIO or PBMT.IO; NC in cacheable PMA
    # bypasses that state. First-instruction bypass still visits the state.
    expected_wait = (not pma_cacheable) or effective_pbmt == 2
    assert any(s["state"] == 1 for s in samples) == expected_wait, samples
    env._emit_event("translation.pbmt_priority_checked", {
        "stage1_pbmt": stage1_pbmt, "stage2_pbmt": stage2_pbmt,
        "effective_pbmt": effective_pbmt, "pma_cacheable": pma_cacheable,
        "wait_last_commit_expected": expected_wait, "native_samples": samples,
    })


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_all_stage_vs_mmio_overrides_g_uncache(env) -> None:
    _assert_all_stage_pbmt_route(env, stage1_pbmt=2, stage2_pbmt=1, pma_cacheable=False)


@pytest.mark.parametrize(
    "stage1_pbmt,stage2_pbmt,pma_cacheable",
    [
        pytest.param(2, 1, True, id="vs-io-g-nc-cacheable-pma"),
        pytest.param(1, 2, True, id="vs-nc-g-io-cacheable-pma"),
        pytest.param(1, 2, False, id="vs-nc-g-io-mmio-pma"),
        pytest.param(0, 2, True, id="vs-pma-falls-back-g-io"),
        pytest.param(0, 1, True, id="vs-pma-falls-back-g-nc"),
    ],
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_all_stage_pbmt_priority_and_pma_contract(env, stage1_pbmt, stage2_pbmt, pma_cacheable) -> None:
    _assert_all_stage_pbmt_route(env, stage1_pbmt=stage1_pbmt, stage2_pbmt=stage2_pbmt,
                               pma_cacheable=pma_cacheable)

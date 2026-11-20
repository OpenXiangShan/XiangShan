from __future__ import annotations

import random
from collections.abc import Mapping
from dataclasses import replace

from env.sequences import TranslationScenario
from env.support import PcMapping, RandomPc, random_pc, record_scenario, scenario_rng


FAULT_NOP_PAYLOAD = b"\x13\x00\x00\x00" * 512
_FAULT_VA_BASE = 0x8020_0000
_FAULT_PA_BASE = 0x8040_0000
_FAULT_GPA_BASE = 0x8060_0000
_SUPERPAGE_SIZE = 0x20_0000


def ptw_responses_complete(active: dict, *, required_only: bool = False) -> bool:
    """Check all expected PTW keys, or only those required by the oracle."""
    responded = active["responded_ptw_request_keys"]
    if required_only:
        return set(active["required_ptw_request_keys"]).issubset(responded)
    expected = {
        (int(request["vpn"]), int(request["s2xlate"]), int(request["get_gpa"]))
        for request in active["expected_ptw_requests"]
    }
    return expected.issubset(tuple(map(int, key)) for key in responded)


def relocate_translation_entry(
    scenario: TranslationScenario,
    entry: RandomPc,
    *,
    relocate_gpa: bool = True,
    s1_ppn_delta: int | None = None,
) -> TranslationScenario:
    pa_delta = entry.pa - scenario.pa
    changes = {
        "va": entry.va,
        "pa": entry.pa,
        "pmp_entries": tuple(replace(item, addr=item.addr + pa_delta) for item in scenario.pmp_entries),
        "pma_entries": tuple(replace(item, addr=item.addr + pa_delta) for item in scenario.pma_entries),
    }
    if relocate_gpa and scenario.gpa is not None:
        changes["gpa"] = scenario.gpa + entry.va - scenario.va
    if s1_ppn_delta is not None:
        changes["s1_sector_lanes"] = tuple(
            replace(lane, ppn=lane.ppn + s1_ppn_delta) for lane in scenario.s1_sector_lanes
        )
    return replace(scenario, **changes)


def randomize_ptw_response(
    scenario: TranslationScenario,
    rng: random.Random,
    *,
    seed: int,
    latency_range: tuple[int, int] | None,
    latency_max_limit: int | None = None,
    clamp_latency_max_limit: bool = False,
    keep_latency_max: bool = True,
    randomize_ready: bool = False,
) -> TranslationScenario:
    """Draw latency, optional maximum and ready timing in that order."""
    latency = rng.randint(*latency_range) if latency_range is not None else int(scenario.ptw_response_latency)
    latency_max = scenario.ptw_response_latency_max if keep_latency_max else None
    if latency_max_limit is not None:
        upper = int(latency_max_limit)
        if clamp_latency_max_limit:
            upper = max(latency, upper)
        latency_max = rng.randint(latency, upper)
    changes = {
        "ptw_response_latency": latency,
        "ptw_response_latency_max": latency_max,
        "ptw_response_seed": seed,
    }
    if randomize_ready:
        high = int(scenario.ptw_req_ready_high_cycles)
        low = int(scenario.ptw_req_ready_low_cycles)
        if scenario.ptw_req_ready_strategy == "periodic":
            high = rng.randint(1, 2)
            low = rng.randint(1, 4)
        changes.update(ptw_req_ready_high_cycles=high, ptw_req_ready_low_cycles=low)
    return replace(scenario, **changes)


def randomize_translation_fault_entry(
    env,
    scenario: TranslationScenario,
    *,
    scenario_key: str,
    expected_fault: str,
) -> TranslationScenario:
    """Relocate a two-page fault stream while preserving superpage mappings."""
    _, _, rng = scenario_rng(scenario_key)
    entry = random_pc(
        rng,
        mappings=(PcMapping(_FAULT_VA_BASE, _FAULT_PA_BASE, _SUPERPAGE_SIZE),),
        entry_offsets=tuple(page * 0x1000 + 0xF00 for page in range(0, 511, 2)),
        instruction_bytes=4,
    )
    relocated = replace(
        relocate_translation_entry(scenario, entry, relocate_gpa=False),
        gpa=(_FAULT_GPA_BASE + entry.va - _FAULT_VA_BASE) if scenario.gpa is not None else None,
    )
    return _randomize_ptw_response_timing(
        env,
        relocated,
        scenario_key=scenario_key,
        parameters={
            "page_count": relocated.page_count,
            "gpa": relocated.gpa,
            "s2xlate": relocated.s2xlate,
            "expected_path": "fault",
            "expected_fault": expected_fault,
        },
    )


def _randomize_ptw_response_timing(
    env,
    scenario: TranslationScenario,
    *,
    scenario_key: str,
    latency_min: int = 1,
    latency_max: int = 8,
    parameters: Mapping[str, object] | None = None,
) -> TranslationScenario:
    """Randomize the PTW response latency shared by translation scenarios."""
    base_seed, seed, rng = scenario_rng(str(scenario_key))
    randomized = randomize_ptw_response(
        scenario, rng, seed=seed, latency_range=(int(latency_min), int(latency_max)),
    )
    recorded_parameters = {
        "scenario_id": scenario.scenario_id,
        "va": scenario.va,
        "pa": scenario.pa,
        "latency": randomized.ptw_response_latency,
    }
    if parameters:
        recorded_parameters.update(dict(parameters))
    record_scenario(
        env,
        str(scenario_key),
        base_seed=base_seed,
        seed=seed,
        parameters=recorded_parameters,
    )
    return randomized


def _randomize_translation_timing(
    env,
    scenario: TranslationScenario,
    *,
    latency_min: int = 1,
    latency_max: int = 8,
) -> TranslationScenario:
    return _randomize_ptw_response_timing(
        env,
        scenario,
        scenario_key=f"zhaoxinran/uncache/translation/{scenario.scenario_id}",
        latency_min=latency_min,
        latency_max=latency_max,
        parameters={
            "page_count": scenario.page_count,
            "expected_path": scenario.expected_path,
            "expected_result": scenario.expected_result,
        },
    )

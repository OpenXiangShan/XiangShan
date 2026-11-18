from __future__ import annotations

from collections.abc import Mapping
from dataclasses import replace

from env.sequences import TranslationScenario
from env.support import PcMapping, random_pc, record_scenario, scenario_rng


FAULT_NOP_PAYLOAD = b"\x13\x00\x00\x00" * 512
_FAULT_VA_BASE = 0x8020_0000
_FAULT_PA_BASE = 0x8040_0000
_FAULT_GPA_BASE = 0x8060_0000
_SUPERPAGE_SIZE = 0x20_0000


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
        scenario,
        va=entry.va,
        pa=entry.pa,
        gpa=(_FAULT_GPA_BASE + entry.va - _FAULT_VA_BASE) if scenario.gpa is not None else None,
        pmp_entries=tuple(replace(item, addr=item.addr + entry.pa - scenario.pa) for item in scenario.pmp_entries),
        pma_entries=tuple(replace(item, addr=item.addr + entry.pa - scenario.pa) for item in scenario.pma_entries),
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
    latency = rng.randint(int(latency_min), int(latency_max))
    randomized = replace(
        scenario,
        ptw_response_latency=latency,
        ptw_response_seed=seed,
    )
    recorded_parameters = {
        "scenario_id": scenario.scenario_id,
        "va": scenario.va,
        "pa": scenario.pa,
        "latency": latency,
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

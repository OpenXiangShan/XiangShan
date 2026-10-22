from __future__ import annotations

from collections.abc import Mapping
from dataclasses import replace

from env.sequences import TranslationScenario
from env.support import record_scenario, scenario_rng


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

from __future__ import annotations

import os

import pytest

from env.sequences import TranslationScenarioSequence


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"


def _read_int(name: str, default: str) -> int:
    return int(os.getenv(name, default), 0)


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_translation_constrained_random_stream_dut(env) -> None:
    seed = _read_int("TB_TRANSLATION_RANDOM_SEED", "0x5a390001")
    count = _read_int("TB_TRANSLATION_RANDOM_COUNT", "6")
    start_ordinal = _read_int("TB_TRANSLATION_RANDOM_START_ORDINAL", "0")
    if count < 1:
        raise ValueError("TB_TRANSLATION_RANDOM_COUNT must be positive")
    if start_ordinal < 0:
        raise ValueError("TB_TRANSLATION_RANDOM_START_ORDINAL must be non-negative")

    actions = TranslationScenarioSequence.randomized_scenario_actions(
        seed,
        count,
        start_ordinal=start_ordinal,
    )
    sequence = TranslationScenarioSequence(actions=actions)
    sequence.initialize_first_phase(env)
    results = sequence.run(env)
    phase_results = [record for record in results if record["kind"] == "phase"]

    assert len(phase_results) == count
    assert [record["scenario_id"] for record in phase_results] == [
        f"translation-random-s{seed}-n{ordinal}" for ordinal in range(start_ordinal, start_ordinal + count)
    ]
    assert not env.get_errors()

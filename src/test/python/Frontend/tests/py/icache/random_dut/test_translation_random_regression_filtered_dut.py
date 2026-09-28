from __future__ import annotations

import os

import pytest

from env.sequences import (
    TranslationScenarioPhase,
    TranslationScenarioRandomizer,
    TranslationScenarioSequence,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"


def _read_int(name: str, default: str) -> int:
    return int(os.getenv(name, default), 0)


def _has_expected_fault(scenario) -> bool:
    """Identify fault-producing random cases whose Verilator cfVec PC is 0.

    Dedicated translation-fault tests retain these cases.  The constrained
    random smoke stream defaults to legal instruction translations so a known
    exception-PC propagation limitation does not turn the whole random stream
    red.  Set TB_TRANSLATION_RANDOM_ALLOW_FAULTS=1 to replay the original
    fault-inclusive stream explicitly.
    """
    if any(
        int(getattr(scenario, field, 0))
        for field in ("s1_pf", "s1_af", "s2_gpf", "s2_gaf")
    ):
        return True
    for pte_name in ("s1_pte", "s2_pte"):
        pte = getattr(scenario, pte_name, None)
        if pte is not None and (not int(pte.v) or not int(pte.r) or not int(pte.x) or not int(pte.a)):
            return True
    for entry in tuple(getattr(scenario, "pmp_entries", ())):
        if str(entry.config.match).lower() != "off" and not bool(entry.config.execute):
            return True
    for lane in tuple(getattr(scenario, "s1_sector_lanes", ())):
        if not int(lane.valid) or not int(lane.pte_present):
            return True
    return False


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_translation_constrained_random_filtered_stream_dut(env) -> None:
    seed = _read_int("TB_TRANSLATION_RANDOM_SEED", "0x5a390001")
    count = _read_int("TB_TRANSLATION_RANDOM_COUNT", "6")
    start_ordinal = _read_int("TB_TRANSLATION_RANDOM_START_ORDINAL", "0")
    if count < 1:
        raise ValueError("TB_TRANSLATION_RANDOM_COUNT must be positive")
    if start_ordinal < 0:
        raise ValueError("TB_TRANSLATION_RANDOM_START_ORDINAL must be non-negative")

    phase_results = []
    selected_ordinals = []
    allow_faults = os.getenv("TB_TRANSLATION_RANDOM_ALLOW_FAULTS", "0").strip() == "1"
    for ordinal in range(start_ordinal, start_ordinal + count):
        scenario = TranslationScenarioRandomizer(seed).next(ordinal).scenario
        if not allow_faults and _has_expected_fault(scenario):
            continue
        selected_ordinals.append(ordinal)
        sequence = TranslationScenarioSequence(actions=(
            TranslationScenarioPhase(scenario=scenario),
        ))
        sequence.initialize_first_phase(env)
        phase_results.extend(record for record in sequence.run(env) if record["kind"] == "phase")

    assert len(phase_results) == len(selected_ordinals)
    assert [record["scenario_id"] for record in phase_results] == [
        f"translation-random-s{seed}-n{ordinal}" for ordinal in selected_ordinals
    ]
    assert not env.get_errors()

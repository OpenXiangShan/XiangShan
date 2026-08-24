from __future__ import annotations

import pytest

from env.core.frontend_env import FrontendEnv
from env.core.transactions import RedirectTxn
from env.runtime.dut_factory import FakeDUTFrontend
from env.sequences import (
    TranslationScenarioBuilder,
    TranslationScenarioPhase,
    TranslationScenarioRandomizer,
    TranslationScenarioSequence,
)


def test_translation_randomizer_replays_the_same_seed() -> None:
    first = TranslationScenarioRandomizer(0x5EED).generate(24)
    second = TranslationScenarioRandomizer(0x5EED).generate(24)

    assert first == second
    assert [item.scenario.scenario_id for item in first] == [f"translation-random-s24301-n{index}" for index in range(24)]
    assert TranslationScenarioRandomizer(0x5EED).next(17) == first[17]


def test_translation_randomizer_builds_a_constrained_stream_across_translation_kinds() -> None:
    generated = TranslationScenarioRandomizer(0x1234).generate(64)
    observed_kinds = set()

    for item in generated:
        scenario = item.scenario
        env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
        state = TranslationScenarioBuilder(env).build(scenario)

        observed_kinds.add((scenario.mode, scenario.s2xlate))
        assert state.expected_page_outcomes
        assert state.expected_ptw_request["scenario_id"] == scenario.scenario_id
        assert env.memory.read_block(scenario.pa, len(scenario.payload)) == scenario.payload
        assert len(scenario.payload) == 1024
        assert scenario.page_count >= (scenario.va % 0x1000 + len(scenario.payload) + 0xFFF) // 0x1000
        if scenario.permission_probes:
            assert state.expected_permission_probes

    assert any(mode == "bare" and s2xlate == 0 for mode, s2xlate in observed_kinds)
    assert any(s2xlate == 0 and mode in {"sv39", "sv48"} for mode, s2xlate in observed_kinds)
    assert any(s2xlate == 1 for _mode, s2xlate in observed_kinds)
    assert any(s2xlate == 2 for _mode, s2xlate in observed_kinds)
    assert any(s2xlate == 3 for _mode, s2xlate in observed_kinds)
    assert any(item.scenario.s1_sector_lanes for item in generated)
    assert any(item.scenario.s1_pte.level == 1 for item in generated)
    assert any(
        item.scenario.s1_pf or item.scenario.s1_af or item.scenario.s2_gpf or item.scenario.s2_gaf
        for item in generated
    )
    assert any(any(entry.config.match == "tor" for entry in item.scenario.pmp_entries) for item in generated)
    assert any(
        len({entry.addr for entry in item.scenario.pmp_entries}) < len(item.scenario.pmp_entries)
        for item in generated
    )
    assert all(not entry.config.locked for item in generated for entry in item.scenario.pmp_entries)


def test_translation_random_control_stream_replays_and_runs_through_the_public_sequence() -> None:
    actions = TranslationScenarioSequence.randomized_control_actions(0xACE, 20)

    assert actions == TranslationScenarioSequence.randomized_control_actions(0xACE, 20)
    assert {type(action).__name__ for action in actions} == {
        "TranslationSfenceAction",
        "TranslationContextAction",
        "TranslationPmpPmaWriteAction",
    }

    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    records = TranslationScenarioSequence(actions=actions).run(env)
    assert len(records) == len(actions)


def test_translation_randomizer_requires_reset_reentry_between_random_phases() -> None:
    assert TranslationScenarioSequence.randomized_scenario_actions(
        0xBEEF,
        1,
        start_ordinal=7,
        include_controls=False,
    ) == (
        TranslationScenarioPhase(scenario=TranslationScenarioRandomizer(0xBEEF).next(7).scenario),
    )
    assert TranslationScenarioSequence.randomized_scenario_actions(0xBEEF, 0, include_controls=False) == ()

    with pytest.raises(ValueError, match="reset/re-entry"):
        TranslationScenarioSequence.randomized_scenario_actions(0xBEEF, 2, include_controls=False)
    with pytest.raises(ValueError, match="redirect source context"):
        TranslationScenarioSequence.randomized_scenario_actions(0xBEEF, 1)


def test_translation_random_sequence_initializes_from_the_first_phase() -> None:
    actions = TranslationScenarioSequence.randomized_scenario_actions(
        0xBEEF,
        1,
        start_ordinal=1,
        include_controls=False,
    )
    sequence = TranslationScenarioSequence(actions=actions)
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)

    sequence.initialize_first_phase(env)

    scenario = actions[0].scenario
    assert scenario is not None
    assert env.page_table.mode == "sv39"
    assert env.monitor.expected_pc == scenario.va


def test_translation_phase_rejects_target_only_redirect() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    scenario = TranslationScenarioRandomizer(0xBEEF).next(0).scenario

    with pytest.raises(ValueError, match="explicit source context"):
        TranslationScenarioSequence(actions=(TranslationScenarioPhase(scenario=scenario, redirect=True),)).run(env)


def test_translation_phase_requires_redirect_target_to_match_its_phase() -> None:
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    scenario = TranslationScenarioRandomizer(0xBEEF).next(0).scenario

    with pytest.raises(ValueError, match="target_pc must match"):
        TranslationScenarioSequence(
            actions=(
                TranslationScenarioPhase(
                    scenario=scenario,
                    redirect_txn=RedirectTxn(
                        target_pc=int(scenario.va) + 4,
                        reason="unit-source-bound-redirect",
                        source_pc=int(scenario.va),
                    ),
                ),
            )
        ).run(env)

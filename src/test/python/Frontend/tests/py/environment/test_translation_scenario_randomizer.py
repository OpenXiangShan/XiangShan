from __future__ import annotations

from env.core.frontend_env import FrontendEnv
from env.runtime.dut_factory import FakeDUTFrontend
from env.sequences import TranslationScenarioBuilder, TranslationScenarioRandomizer, TranslationScenarioSequence


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
        if scenario.page_count == 2:
            assert scenario.va % 0x1000 == 0xFF8
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
        "TranslationFlushPipeAction",
        "TranslationContextAction",
        "TranslationPmpPmaWriteAction",
        "TranslationPbmteAction",
    }

    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    records = TranslationScenarioSequence(actions=actions).run(env)
    assert len(records) == len(actions)


def test_translation_randomizer_composes_generic_phase_and_control_streams() -> None:
    actions = TranslationScenarioSequence.randomized_scenario_actions(0xBEEF, 12)
    replay = TranslationScenarioSequence.randomized_scenario_actions(0xBEEF, 12)

    assert actions == replay
    assert len(actions) == 23
    assert sum(type(action).__name__ == "TranslationScenarioPhase" for action in actions) == 12
    assert all(type(actions[index]).__name__ == "TranslationScenarioPhase" for index in range(0, len(actions), 2))
    assert TranslationScenarioSequence.randomized_scenario_actions(0xBEEF, 0) == ()


def test_translation_randomizer_replays_a_stream_from_one_ordinal() -> None:
    full = TranslationScenarioSequence.randomized_scenario_actions(0xBEEF, 12)
    replay = TranslationScenarioSequence.randomized_scenario_actions(0xBEEF, 3, start_ordinal=7)

    assert replay == full[14:19]


def test_translation_random_sequence_initializes_from_the_first_phase() -> None:
    actions = TranslationScenarioSequence.randomized_scenario_actions(0xBEEF, 1, start_ordinal=1)
    sequence = TranslationScenarioSequence(actions=actions)
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)

    sequence.initialize_first_phase(env)

    scenario = actions[0].scenario
    assert scenario is not None
    assert env.page_table.mode == "sv39"
    assert env.monitor.expected_pc == scenario.va

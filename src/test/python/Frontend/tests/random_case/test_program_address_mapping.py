import json
from dataclasses import replace
from types import SimpleNamespace

import pytest

from .run_random_campaign import _load_dut_timeline
from .scenario_contract import ScenarioSpec
from .scenario_coordinator import ScenarioCoordinator, TimingPlan


VA = 0x80000000
PA = 0x80400000


def setup(profile="translation_timing_cross", instructions=None):
    plan = replace(TimingPlan.from_seed(17, profile), warmup_response_lines=1000)
    raw_instructions = instructions if instructions is not None else [
        {"pc": VA + 4094, "episode_id": "page-tail", "crosses_page": True},
    ]
    program = {"binary_text_verified": True, "binary_sha256": "fixture",
               "instructions": [{"size": 4, **instruction} for instruction in raw_instructions]}
    coordinator = ScenarioCoordinator(scenario_id="mapped", timing_plan=plan, program_map=program)
    values = []
    coordinator.env = SimpleNamespace(backend_model=SimpleNamespace(set_can_accept=values.append))
    return coordinator, values


def event(address, kind="handshake.uncache_d", cycle=12):
    return {"type": kind, "source": "unit-fixture", "cycle": cycle,
            "payload": {"address": address, "beat_idx": 0}}


@pytest.mark.parametrize("kind", ["handshake.icache_d", "handshake.uncache_d"])
def test_physical_page_tail_response_triggers_program_boundary_not_periodic_stall(kind):
    coordinator, values = setup()
    coordinator.configure_translation_pages([{"va": VA, "pa": PA, "ok": True}])
    response = event(PA + 4088, kind)
    coordinator.handle_event(response)
    timeline = coordinator.build_timeline(execution_status="FAIL")
    context = timeline["timeline"][0]["program_context"]
    assert context["cacheline_address"] == VA + 4032
    assert context["physical_address"] == PA + 4088
    assert context["program_address_candidates"] == [VA + 4088]
    assert context["episode_ids"] == ["page-tail"]
    assert context["line_episode_ids"] == ["page-tail"]
    assert context["boundary_episode_ids"] == ["page-tail"]
    assert context["boundary_instruction_pcs"] == [VA + 4094]
    assert context["exact_address_instruction_pcs"] == []
    assert context["attribution_scope"] == "cacheline"
    assert context["exact_instruction_attribution"] is False
    assert context["mapping_status"] == "mapped"
    assert context["selected_boundary_line"] is True
    assert values == [0]  # periodic threshold is 1000, but this is only response 1
    assert timeline["actions"][0]["trigger_payload"]["address"] == PA + 4088
    assert response["payload"]["address"] == PA + 4088  # preserve raw evidence


@pytest.mark.parametrize("physical", [VA + 4088, PA + 4096, PA - 8])
def test_unmapped_physical_addresses_do_not_fall_back_to_virtual_identity(physical):
    coordinator, values = setup()
    coordinator.configure_translation_pages([{"va": VA, "pa": PA, "ok": True}])
    coordinator.handle_event(event(physical))
    context = coordinator.build_timeline(execution_status="FAIL")["timeline"][0]["program_context"]
    assert context["mapping_status"] == "unmapped"
    assert context["episode_ids"] == []
    assert context["cacheline_address"] is None
    assert values == []


def test_noncontiguous_mappings_resolve_each_page_independently():
    coordinator, _ = setup(instructions=[
        {"pc": VA + 4094, "episode_id": "first", "crosses_page": True},
        {"pc": VA + 4096, "episode_id": "second", "conditional_branch": True},
    ])
    coordinator.configure_translation_pages([
        {"va": VA, "pa": PA + 0x9000, "ok": True},
        {"va": VA + 4096, "pa": PA, "ok": True},
    ])
    context = coordinator._event_line_context(event(PA + 8))
    assert context["program_address_candidates"] == [VA + 4096 + 8]
    assert context["episode_ids"] == ["first", "second"]
    assert context["boundary_episode_ids"] == ["first", "second"]
    assert context["boundary_instruction_pcs"] == [VA + 4094, VA + 4096]


def test_physical_aliases_are_reported_without_guessing_program_ownership():
    coordinator, values = setup()
    coordinator.configure_translation_pages([
        {"va": VA, "pa": PA, "ok": True},
        {"va": VA + 4096, "pa": PA, "ok": True},
    ])
    coordinator.handle_event(event(PA + 4088))
    context = coordinator.build_timeline(execution_status="FAIL")["timeline"][0]["program_context"]
    assert context["mapping_status"] == "ambiguous_alias"
    assert len(context["program_address_candidates"]) == 2
    assert context["selected_boundary_line"] is False
    assert context["episode_ids"] == []
    assert context["boundary_instruction_pcs"] == []
    assert context["attribution_scope"] == "none"
    assert values == []


@pytest.mark.parametrize("control", ["control.sfence", "control.translation_context"])
def test_mapping_is_invalidated_by_context_changes(control):
    coordinator, values = setup()
    coordinator.configure_translation_pages([{"va": VA, "pa": PA, "ok": True}])
    coordinator.handle_event({"type": control, "cycle": 5, "payload": {}})
    coordinator.handle_event(event(PA + 4088))
    assert not coordinator.translation_mapping_valid
    assert values == []
    timeline = coordinator.build_timeline(execution_status="FAIL")
    assert timeline["program_address_mapping"]["valid"] is False
    assert timeline["timeline"][-1]["program_context"]["mapping_status"] == "unconfigured_or_invalidated"


def test_translation_requires_configuration_and_backend_pc_is_already_virtual():
    coordinator, _ = setup()
    assert coordinator._event_line_context(event(VA + 4088))["selected_boundary_line"] is False
    coordinator.configure_translation_pages([{"va": VA, "pa": PA, "ok": True}])
    context = coordinator._event_line_context({"type": "backend.redirect_queued",
                                              "payload": {"from_pc": VA + 4088}})
    assert context["selected_boundary_line"] is True
    assert context["address_space"] == "program"


def test_bare_programs_keep_identity_mapping_and_boundary_trigger():
    coordinator, values = setup("default")
    coordinator.handle_event(event(VA + 4088))
    assert values == [0]
    context = coordinator.build_timeline(execution_status="FAIL")["timeline"][0]["program_context"]
    assert context["mapping_status"] == "identity"
    assert context["episode_ids"] == ["page-tail"]


def test_response_line_attribution_does_not_claim_an_exact_instruction():
    coordinator, _ = setup(instructions=[
        {"pc": VA + 4032, "size": 4, "episode_id": "unrelated"},
        {"pc": VA + 4094, "size": 4, "episode_id": "page-tail", "crosses_page": True},
    ])
    coordinator.configure_translation_pages([
        {"va": VA, "pa": PA, "ok": True},
        {"va": VA + 4096, "pa": PA + 4096, "ok": True},
    ])
    line_response = coordinator._event_line_context(event(PA + 4032))
    assert line_response["line_episode_ids"] == ["page-tail", "unrelated"]
    assert line_response["boundary_episode_ids"] == ["page-tail"]
    assert line_response["boundary_instruction_pcs"] == [VA + 4094]
    assert line_response["exact_address_instruction_pcs"] == [VA + 4032]
    assert line_response["exact_instruction_attribution"] is False


def test_cross_cacheline_instruction_is_indexed_on_every_touched_line():
    coordinator, _ = setup(instructions=[
        {
            "pc": VA + 0x3E,
            "size": 4,
            "episode_id": "cross-line",
            "crosses_cacheline": True,
        },
    ])
    coordinator.configure_translation_pages([{"va": VA, "pa": PA, "ok": True}])
    context = coordinator._event_line_context(event(PA + 0x40))
    assert context["cacheline_address"] == VA + 0x40
    assert context["line_episode_ids"] == ["cross-line"]
    assert context["boundary_episode_ids"] == ["cross-line"]
    assert context["boundary_instruction_pcs"] == [VA + 0x3E]
    assert context["exact_address_instruction_pcs"] == [VA + 0x3E]
    assert context["selected_boundary_line"] is True


@pytest.mark.parametrize("pages, match", [
    ([], "cover ProgramMap"),
    ([{"va": VA, "pa": PA, "ok": False}], "cover ProgramMap"),
    ([{"va": VA, "pa": PA + 1, "ok": True}], "matching page offsets"),
    ([{"va": VA, "pa": PA, "ok": True}, {"va": VA, "pa": PA + 4096, "ok": True}], "conflicting"),
])
def test_invalid_mapping_is_rejected_atomically(pages, match):
    coordinator, _ = setup()
    with pytest.raises(ValueError, match=match):
        coordinator.configure_translation_pages(pages)
    assert not coordinator.translation_mapping_valid
    coordinator.configure_translation_pages([{"va": VA, "pa": PA, "ok": True}])
    with pytest.raises(RuntimeError, match="already configured"):
        coordinator.configure_translation_pages([{"va": VA, "pa": PA, "ok": True}])


@pytest.mark.parametrize("mapping_valid", [True, False, None])
def test_translation_timeline_requires_mapping_evidence(tmp_path, mapping_valid):
    coordinator, _ = setup()
    coordinator.configure_translation_pages([{"va": VA, "pa": PA, "ok": True}])
    payload = coordinator.build_timeline(execution_status="REALIZED", details={"monitor_errors": 0, "translation_oracle_error_count": 0, "translation_oracle_errors": []})
    if mapping_valid is None:
        del payload["program_address_mapping"]
    else:
        payload["program_address_mapping"]["valid"] = mapping_valid
    path = tmp_path / "timeline.json"
    path.write_text(json.dumps(payload))
    spec = ScenarioSpec(scenario_id="mapped", family="translation_timing_cross", ordinal=0,
                        program_seed=1, timing_seed=17, risk_tags=("boundary",),
                        parameters={"timing_plan": coordinator.timing_plan.to_dict()})
    _, events, status, reasons = _load_dut_timeline(path, spec, coordinator.program_map)
    assert status == ("valid" if mapping_valid else "invalid")
    if not mapping_valid:
        assert events == ()
        assert "scenario_timeline_translation_program_mapping_missing_or_invalid" in reasons


@pytest.mark.parametrize("pages", [None, [], [None], [{"va": VA, "pa": PA + 1}],
                                    [{"va": VA, "pa": PA}, {"va": VA, "pa": PA}]])
def test_timeline_valid_flag_cannot_hide_malformed_or_missing_pages(tmp_path, pages):
    coordinator, _ = setup()
    coordinator.configure_translation_pages([{"va": VA, "pa": PA, "ok": True}])
    payload = coordinator.build_timeline(execution_status="REALIZED", details={"monitor_errors": 0, "translation_oracle_error_count": 0, "translation_oracle_errors": []})
    payload["program_address_mapping"]["pages"] = pages
    path = tmp_path / "timeline.json"
    path.write_text(json.dumps(payload))
    spec = ScenarioSpec(scenario_id="mapped", family="translation_timing_cross", ordinal=0,
                        program_seed=1, timing_seed=17, risk_tags=("boundary",), parameters={})
    _, events, status, reasons = _load_dut_timeline(path, spec, coordinator.program_map)
    assert status == "invalid"
    assert events == ()
    assert "scenario_timeline_translation_program_pages_invalid" in reasons


@pytest.mark.parametrize("ordinal", [0, 1])
def test_real_translation_builder_supplies_mapping_for_both_attribute_directions(ordinal):
    from env.core.frontend_env import FrontendEnv
    from env.runtime.dut_factory import FakeDUTFrontend
    from env.sequences import TranslationScenarioBuilder
    from .generate_random_asm import CaseSpec, generate_case
    from .scenario_profiles import build_scenario_spec
    from .testpoint_catalog import DEFAULT_SOURCE, build_catalog
    from .translation_timing import build_translation_scenario

    _, record = generate_case(CaseSpec(family="translation_timing_cross", seed=17,
                                      ordinal=ordinal, blocks=48, directed_pattern="page_tail_pair"))
    spec = build_scenario_spec(record, timing_seed=31, testpoint_catalog=build_catalog(DEFAULT_SOURCE))
    coordinator, _ = setup()
    fake_env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    state = TranslationScenarioBuilder(fake_env).build(build_translation_scenario(spec, bytes(5 * 4096)))
    coordinator.configure_translation_pages(state.expected_page_outcomes)
    assert coordinator.translation_mapping_valid
    assert len(state.expected_page_outcomes) == 5
    for index, page in enumerate(state.expected_page_outcomes):
        assert page["va"] == VA + index * 4096
        assert page["pa"] == PA + index * 4096
        context = coordinator._event_line_context(event(page["pa"] + 4088))
        assert context["program_address_candidates"] == [page["va"] + 4088]
        assert context["mapping_status"] == "mapped"

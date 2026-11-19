"""Historical random scenario intent and observed-event contracts; no DUT run."""
from .scenario_contract import EventRequirement, RealizedEvent, ScenarioSpec, evaluate_realization


def test_scenario_contract_uses_dynamic_many_to_many_coverage() -> None:
    spec = ScenarioSpec(
        scenario_id="cfg-000",
        family="cfg_layout_mix",
        ordinal=0,
        program_seed=1,
        timing_seed=2,
        testpoint_ids=("TP-A", "TP-B"),
        risk_tags=("redirect", "page_boundary"),
        required_events=(
            EventRequirement(event="backend.redirect", relation="T+1", anchor="uncache.d"),
        ),
    )

    payload = spec.to_dict()

    assert payload["minimum_dynamic_instructions"] == 5000
    assert payload["coverage_attribution"]["policy"] == "dynamic_many_to_many"
    assert payload["coverage_attribution"]["single_target_bin"] is False
    assert payload["coverage_attribution"]["planned_testpoint_ids"] == ["TP-A", "TP-B"]


def test_realization_requires_actual_event_counts() -> None:
    spec = ScenarioSpec(
        scenario_id="case",
        family="cfg_layout_mix",
        ordinal=0,
        program_seed=1,
        timing_seed=2,
        risk_tags=("boundary",),
        required_events=(
            EventRequirement(event="trace.committed_instruction", min_count=5000),
            EventRequirement(event="trace.cross_page_instruction"),
        ),
    )
    events = (
        RealizedEvent(
            event="trace.committed_instruction",
            count=6000,
            evidence_source="nemu_committed_trace",
        ),
    )

    missing = evaluate_realization(
        spec,
        events,
        evidence_stage="nemu_program",
        execution_status="completed",
        exit_code=0,
    )
    realized = evaluate_realization(
        spec,
        (
            *events,
            RealizedEvent(
                event="trace.cross_page_instruction",
                count=1,
                evidence_source="nemu_committed_trace",
            ),
        ),
        evidence_stage="nemu_program",
        execution_status="completed",
        exit_code=0,
    )

    assert missing.status == "NOT_REALIZED"
    assert [item.event for item in missing.missing_events] == ["trace.cross_page_instruction"]
    assert realized.status == "REALIZED"


def test_event_from_other_evidence_stage_cannot_realize_dut_requirement():
    spec = ScenarioSpec("case", "cfg_layout_mix", 0, 1, 2, risk_tags=("cfg",),
        required_events=(EventRequirement("same-event", evidence_stage="dut"),))
    program_event = RealizedEvent("same-event", 1, "nemu_committed_trace")
    result = evaluate_realization(spec, (program_event,), evidence_stage="dut_program_plus_funcov",
                                  execution_status="completed", exit_code=0)
    assert result.status == "NOT_REALIZED"
    dut_event = RealizedEvent("same-event", 1, "dut_top_level_timeline")
    result = evaluate_realization(spec, (program_event, dut_event), evidence_stage="dut_program_plus_funcov",
                                  execution_status="completed", exit_code=0)
    assert result.status == "REALIZED"


def test_other_stages_do_not_satisfy_program_or_funcov_requirements():
    for stage, wrong_source, correct_source in (
        ("program", "dut_top_level_timeline", "nemu_committed_trace"),
        ("funcov", "dut_top_level_timeline", "compatible_functional_coverage_artifact"),
    ):
        spec = ScenarioSpec("case", "cfg_layout_mix", 0, 1, 2, risk_tags=("cfg",),
            required_events=(EventRequirement("event", evidence_stage=stage),))
        wrong = RealizedEvent("event", 1, wrong_source)
        good = RealizedEvent("event", 1, correct_source)
        kwargs = {"evidence_stage": "dut_program_plus_funcov", "execution_status": "completed", "exit_code": 0}
        assert evaluate_realization(spec, (wrong,), **kwargs).status == "NOT_REALIZED"
        assert evaluate_realization(spec, (good,), **kwargs).status == "REALIZED"


def test_empty_applicable_requirements_are_not_scenario_realization():
    spec = ScenarioSpec("case", "cfg_layout_mix", 0, 1, 2, risk_tags=("cfg",),
                        required_events=(EventRequirement("dut-event", evidence_stage="dut"),))
    result = evaluate_realization(spec, (), evidence_stage="nemu_program", execution_status="completed", exit_code=0)
    assert result.status == "NOT_REALIZED"
    assert result.details["no_applicable_requirements"] is True


def test_unknown_evidence_stage_rejected():
    spec = ScenarioSpec("case", "cfg_layout_mix", 0, 1, 2, risk_tags=("cfg",))
    try:
        evaluate_realization(spec, (), evidence_stage="unknown", execution_status="completed", exit_code=0)
    except ValueError:
        return
    raise AssertionError("unknown evidence stage must not be accepted")

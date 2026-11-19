"""Scenario JSON must not silently turn malformed limits into valid evidence."""
from copy import deepcopy

import pytest

from .scenario_contract import EventRequirement, ScenarioSpec, scenario_spec_from_dict


def _valid():
    return ScenarioSpec("case", "cfg_layout_mix", 0, 1, 1, risk_tags=("cfg",),
        required_events=(EventRequirement("dut.stress", evidence_stage="dut"),)).to_dict()


def test_current_scenario_round_trip():
    payload = _valid()
    assert scenario_spec_from_dict(payload).to_dict() == payload


@pytest.mark.parametrize("field,value", [
    ("ordinal", True), ("ordinal", 0.0), ("program_seed", "1"), ("program_seed", True),
    ("timing_seed", 1.0), ("minimum_dynamic_instructions", 0),
    ("minimum_dynamic_instructions", None), ("minimum_dynamic_instructions", "5000"),
    ("minimum_dynamic_instructions", True), ("required_events", None),
    ("parameters", []), ("risk_tags", "cfg"),
])
def test_malformed_scenario_fields_rejected(field, value):
    payload = deepcopy(_valid())
    payload[field] = value
    with pytest.raises(ValueError):
        scenario_spec_from_dict(payload)


@pytest.mark.parametrize("field,value", [("min_count", 0), ("min_count", True),
    ("min_count", "1"), ("min_count", 1.5), ("event", 99), ("relation", None),
    ("anchor", False), ("evidence_stage", None)])
def test_malformed_event_requirements_rejected(field, value):
    payload = deepcopy(_valid())
    payload["required_events"][0][field] = value
    with pytest.raises(ValueError):
        scenario_spec_from_dict(payload)


def test_omitted_optional_fields_keep_legacy_defaults_without_coercion():
    payload = {"scenario_id": "case", "family": "cfg_layout_mix", "ordinal": 0,
        "program_seed": 1, "timing_seed": 1, "risk_tags": ["cfg"],
        "required_events": [{"event": "dut.stress"}]}
    scenario = scenario_spec_from_dict(payload)
    assert scenario.minimum_dynamic_instructions == 5000
    assert scenario.required_events[0].min_count == 1
    assert scenario.required_events[0].evidence_stage == "any"

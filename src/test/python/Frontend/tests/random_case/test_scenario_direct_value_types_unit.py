"""Direct in-memory scenario constructors must preserve exact numeric contracts."""
from dataclasses import replace

import pytest

from .scenario_contract import EventRequirement, RealizedEvent, ScenarioSpec


@pytest.mark.parametrize("value", [True, 1.0, "1"])
def test_requirement_count_requires_integer(value):
    with pytest.raises((ValueError, TypeError)):
        EventRequirement("event", min_count=value).validate()


@pytest.mark.parametrize("value", [True, 1.0, "1"])
def test_realized_count_requires_integer(value):
    with pytest.raises((ValueError, TypeError)):
        RealizedEvent("event", value, "dut_top_level_timeline").validate()


@pytest.mark.parametrize("field,value", [("ordinal", False), ("program_seed", True),
    ("timing_seed", 1.0), ("minimum_dynamic_instructions", 5000.0)])
def test_direct_scenario_identity_and_limit_require_integer(field, value):
    spec = ScenarioSpec("case", "cfg", 0, 1, 2, risk_tags=("cfg",))
    with pytest.raises((ValueError, TypeError)):
        replace(spec, **{field: value}).validate()

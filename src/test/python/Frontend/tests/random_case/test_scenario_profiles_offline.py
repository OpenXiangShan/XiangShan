import pytest

from .generate_random_asm import CaseSpec, FAMILIES, generate_case
from .scenario_profiles import build_scenario_spec
from .scenario_timing import TimingPlan
from .testpoint_catalog import DEFAULT_SOURCE, build_catalog


@pytest.mark.parametrize("family", tuple(FAMILIES))
def test_profile_uses_a_seeded_timing_plan_and_reviewed_catalog(family):
    seed = 81
    _, record = generate_case(CaseSpec(family=family, seed=12, blocks=48))
    catalog = build_catalog(DEFAULT_SOURCE)
    scenario = build_scenario_spec(record, timing_seed=seed, testpoint_catalog=catalog)
    profile = family if family in {"translation_timing_cross", "ibuffer_pressure_cross"} else "default"
    assert scenario.parameters["timing_plan"] == TimingPlan.from_seed(seed, profile).to_dict()
    known = {entry["testpoint_id"] for entry in catalog["testpoints"]}
    assert scenario.testpoint_ids and set(scenario.testpoint_ids) <= known
    assert any(event.evidence_stage == "dut" for event in scenario.required_events)

    with pytest.raises(ValueError, match="no longer contains the reviewed"):
        build_scenario_spec(record, timing_seed=seed, testpoint_catalog={"testpoints": []})

import pytest

from .scenario_timing import TimingPlan


@pytest.mark.parametrize("profile", ["default", "ibuffer_pressure_cross", "translation_timing_cross"])
def test_seeded_plan_is_repeatable_and_round_trips(profile):
    plan = TimingPlan.from_seed(123, profile)
    assert plan == TimingPlan.from_seed(123, profile)
    assert TimingPlan.from_dict(plan.to_dict()) == plan
    assert len(plan.backpressure_durations) == plan.maximum_backpressure_windows


@pytest.mark.parametrize("seed", [-1, 1 << 64])
def test_invalid_timing_seed_is_rejected(seed):
    with pytest.raises(ValueError):
        TimingPlan.from_seed(seed)

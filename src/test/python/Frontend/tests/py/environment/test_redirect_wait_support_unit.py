from types import SimpleNamespace

import pytest

from tests.py.zhaoxinran.test_redirect_flush_recovery_scenarios import (
    _wait_for_monitor_taken_prediction,
    _wait_for_pc,
)


@pytest.mark.parametrize("taken", (False, True))
@pytest.mark.parametrize("max_cycles", (0, 1, 3))
@pytest.mark.parametrize("arrival", (-1, 0, 1, 2, 3))
def test_redirect_wait_preserves_cycle_boundary_and_diagnostics(
    taken, max_cycles, arrival,
) -> None:
    pc = 0x8000_0040
    observation = SimpleNamespace(pc=pc, pred_taken=True)
    observations = [observation] if arrival == 0 else []
    env = SimpleNamespace(
        current_cycle=0,
        monitor=SimpleNamespace(observations=observations, get_errors=lambda: ["diagnostic"]),
    )

    def step(cycles):
        assert cycles == 1
        env.current_cycle += 1
        if env.current_cycle == arrival:
            observations.append(observation)

    env.step = step
    wait = _wait_for_monitor_taken_prediction if taken else _wait_for_pc
    if 0 <= arrival < max_cycles:
        wait(env, pc, max_cycles=max_cycles)
        assert env.current_cycle == arrival
    else:
        with pytest.raises(AssertionError) as error:
            wait(env, pc, max_cycles=max_cycles)
        assert env.current_cycle == max_cycles
        if taken:
            assert error.value.args[0] == {
                "reason": "target CFI never reached cfVec with pred_taken",
                "pc": hex(pc),
                "cycles": max_cycles,
                "observed_tail": [{"pc": hex(pc), "pred_taken": True}] if observations else [],
            }
        else:
            assert error.value.args[0] == {
                "reason": "redirect target was not delivered",
                "target_pc": hex(pc),
                "observed_tail": [hex(pc)] if observations else [],
                "monitor_errors": ["diagnostic"],
            }


def test_redirect_taken_wait_rejects_not_taken_or_other_pc() -> None:
    pc = 0x8000_0040
    env = SimpleNamespace(
        monitor=SimpleNamespace(observations=[
            SimpleNamespace(pc=pc, pred_taken=False),
            SimpleNamespace(pc=pc),
            SimpleNamespace(pc=pc + 2, pred_taken=True),
        ]),
        step=lambda cycles: None,
    )
    with pytest.raises(AssertionError):
        _wait_for_monitor_taken_prediction(env, pc, max_cycles=1)

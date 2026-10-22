import pytest

from memblock_toffee.datadef import FlushCommand, FlushObservation
from memblock_toffee.model import L2FlushModel
from memblock_toffee.scoreboard import L2FlushScoreboard


def test_model_and_scoreboard_are_pin_independent() -> None:
    model = L2FlushModel()
    scoreboard = L2FlushScoreboard()
    command = FlushCommand("txn-0", enable=True, done=False)
    expected = model.accept(command)
    scoreboard.expect(expected)
    scoreboard.observe(FlushObservation("txn-0", True, False, True, False, 9))
    scoreboard.finish()
    assert model.transitions == 1
    assert model.enable_transitions == 1
    assert model.done_transitions == 0
    assert model.cross_bitmap == 0b100


def test_scoreboard_rejects_duplicate_observation() -> None:
    scoreboard = L2FlushScoreboard()
    command = FlushCommand("txn-0", enable=False, done=True)
    expected = L2FlushModel().accept(command)
    scoreboard.expect(expected)
    scoreboard.observe(FlushObservation("txn-0", False, False, False, True, 1))
    scoreboard.observe(FlushObservation("txn-0", False, False, False, True, 2))
    with pytest.raises(AssertionError, match="duplicate observation"):
        scoreboard.finish()


def test_scoreboard_rejects_combinational_done_instead_of_one_cycle_delay() -> None:
    model = L2FlushModel()
    scoreboard = L2FlushScoreboard()
    command = FlushCommand("txn-0", enable=False, done=True)
    scoreboard.expect(model.accept(command))
    scoreboard.observe(
        FlushObservation(
            transaction_id="txn-0",
            pre_outer_enable=False,
            pre_backend_done=True,
            post_outer_enable=False,
            post_backend_done=True,
            cycle=1,
        )
    )
    with pytest.raises(AssertionError, match="pre-edge backend done"):
        scoreboard.finish()

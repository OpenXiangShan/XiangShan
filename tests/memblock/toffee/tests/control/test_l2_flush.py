import pytest
import toffee_test

from memblock_toffee.sequence import L2FlushSequence


@pytest.mark.toffee_control
@toffee_test.testcase
async def test_l2_flush_pass_through(memblock_env):
    await memblock_env.control_driver.reset()
    sequence = L2FlushSequence(
        patterns=(
            (False, False),
            (True, False),
            (True, True),
            (False, True),
            (False, False),
            (True, True),
            (True, False),
            (False, True),
            (False, False),
            (False, False),
        )
    )
    await memblock_env.control_sequencer.execute(sequence)
    memblock_env.control_scoreboard.finish()
    assert memblock_env.control_model.enable_transitions >= 4
    assert memblock_env.control_model.done_transitions >= 4
    assert memblock_env.control_model.loop_enable_transitions >= 4
    assert memblock_env.control_model.loop_done_transitions >= 4
    assert memblock_env.control_model.cross_bitmap == 0xF
    memblock_env.control_coverage.mark_function(
        "l2_flush_enable",
        test_l2_flush_pass_through,
    )
    memblock_env.control_coverage.mark_function(
        "l2_flush_done",
        test_l2_flush_pass_through,
    )
    memblock_env.control_coverage.mark_function(
        "outer_l2_flush_enable",
        test_l2_flush_pass_through,
    )
    memblock_env.control_coverage.mark_function(
        "backend_l2_flush_done",
        test_l2_flush_pass_through,
    )
    print(
        "TOFFEE_L2_FLUSH_PASS "
        f"cycle={memblock_env.cycle_tracker.cycle} "
        "combinations=4 "
        "loop_enable_transitions="
        f"{memblock_env.control_model.loop_enable_transitions} "
        "loop_done_transitions="
        f"{memblock_env.control_model.loop_done_transitions}"
    )

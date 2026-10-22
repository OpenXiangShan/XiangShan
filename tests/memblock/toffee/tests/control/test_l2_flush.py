import memblock_toffee
import pytest

from memblock_toffee.sequence import L2FlushSequence


@pytest.mark.toffee_control
@memblock_toffee.testcase
async def test_l2_flush_pass_through(memblock_env):
    await memblock_env.reset()
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
    result = await memblock_env.run(sequence)
    assert result.enable_transitions >= 4
    assert result.done_transitions >= 4
    assert result.loop_enable_transitions >= 4
    assert result.loop_done_transitions >= 4
    assert result.cross_bitmap == 0xF
    memblock_env.mark_coverage(
        test_l2_flush_pass_through,
        "l2_flush_enable",
        "l2_flush_done",
        "outer_l2_flush_enable",
        "backend_l2_flush_done",
    )
    print(
        "TOFFEE_L2_FLUSH_PASS "
        f"cycle={result.cycle} "
        "combinations=4 "
        "loop_enable_transitions="
        f"{result.loop_enable_transitions} "
        "loop_done_transitions="
        f"{result.loop_done_transitions}"
    )

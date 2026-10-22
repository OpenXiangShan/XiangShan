import pytest
import toffee_test

@pytest.mark.toffee_smoke
@toffee_test.testcase
async def test_reset_and_clock_drive(memblock_env):
    await memblock_env.control_driver.reset()
    assert 9 <= memblock_env.cycle_tracker.cycle <= 24
    for _ in range(24):
        await memblock_env.control_bundle.step()
        token = memblock_env.cycle_tracker.next_token()
        memblock_env.cycle_tracker.commit(token)
        memblock_env.quiescence.require_idle()
    assert memblock_env.quiescence.observations == 24
    memblock_env.control_coverage.mark_function("l2_flush_enable", test_reset_and_clock_drive)
    print(
        "TOFFEE_SMOKE_PASS "
        f"cycle={memblock_env.cycle_tracker.cycle} checked_outputs=20"
    )

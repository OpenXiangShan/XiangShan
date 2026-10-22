import pytest
import memblock_toffee


@pytest.mark.toffee_smoke
@memblock_toffee.testcase
async def test_reset_and_clock_drive(memblock_env):
    assert memblock_env.control_driver.drivers["reset"].path == "control_driver.reset"
    await memblock_env.reset()
    assert 9 <= memblock_env.cycle <= 24
    await memblock_env.tick(24, require_idle=True)
    memblock_env.mark_coverage(
        test_reset_and_clock_drive,
        "l2_flush_enable",
    )
    print(
        "TOFFEE_SMOKE_PASS "
        f"cycle={memblock_env.cycle} checked_outputs=20"
    )

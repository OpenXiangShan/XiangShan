from __future__ import annotations

import pytest

from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


_JALR_X0_X1_0 = 0x00008067


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_jalr_return_is_delivered_as_indirect_control_flow(env):
    payload = int(_JALR_X0_X1_0).to_bytes(4, "little")
    payload += int(uncache._CNOP).to_bytes(2, "little") * 128
    env.memory.mmio_ranges.append((uncache._MMIO_BASE, uncache._MMIO_BASE + len(payload)))
    uncache.LoadProgramSequence(
        image=uncache.ProgramImage(payload=payload, base_addr=uncache._MMIO_BASE),
        step_cycles=0,
    ).run(env)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE)
    assert uncache._wait_for_observed_pc(env, uncache._MMIO_BASE, max_cycles=8000)
    observed = next(
        item for item in env.monitor.observations if int(item.pc) == uncache._MMIO_BASE
    )
    assert int(observed.instr) == _JALR_X0_X1_0
    assert not bool(observed.is_rvc)
    assert env.branch_checker.get_stats()["by_type"]["jump_indirect"] >= 1
    assert not env.monitor.get_errors()

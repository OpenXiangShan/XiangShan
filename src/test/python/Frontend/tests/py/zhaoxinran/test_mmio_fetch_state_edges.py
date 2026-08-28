from __future__ import annotations

import pytest

from env.funcov.py.ifu import mmio_nc_owner_funcov as owner_funcov
from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_send_req_a_fire_enters_wait_resp_without_duplicate_request(env):
    uncache._prepare_mmio_cnop_stream(env)
    env.uncache_agent.set_a_ready(0)
    snapshots: list[dict[str, int | None]] = []

    def capture(cycle: int, active_env) -> None:
        sample = owner_funcov._snapshot(active_env.functional_coverage, active_env.dut)
        if sample["tl_a_valid"] or sample["tl_d_valid"] or sample["entry_state"] == 2:
            snapshots.append({"cycle": int(cycle), **sample})

    env.register_cycle_observer(capture)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_a_valid_addr(env, uncache._MMIO_BASE, max_cycles=8000)
    stalled = [
        sample
        for sample in snapshots
        if sample["uncache_state"] == 3
        and sample["entry_state"] == 1
        and sample["tl_a_valid"] == 1
        and sample["tl_a_ready"] == 0
    ]
    assert stalled, {"snapshots": snapshots[-32:]}

    env.uncache_agent.set_a_ready(None)
    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE, max_cycles=8000)
    assert uncache._wait_for_uncache_resp(env, max_cycles=8000)

    transitions = [
        sample
        for sample in snapshots
        if sample["entry_state"] == 2 and sample["tl_a_valid"] == 0
    ]
    assert transitions, {"snapshots": snapshots[-32:]}
    wait_resp_cycle = min(
        sample["cycle"] for sample in snapshots if sample["entry_state"] == 2
    )
    first_d_cycle = min(
        sample["cycle"] for sample in snapshots if sample["tl_d_valid"] == 1
    )
    assert not any(
        wait_resp_cycle <= sample["cycle"] <= first_d_cycle
        and sample["tl_a_valid"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-32:]}
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_send_req_with_ibuffer_ready_drives_tl_a(env):
    uncache._prepare_mmio_cnop_stream(env)
    snapshots: list[dict[str, int | None]] = []

    def capture(cycle: int, active_env) -> None:
        sample = owner_funcov._snapshot(active_env.functional_coverage, active_env.dut)
        if sample["entry_state"] == 1 and sample["tl_a_valid"]:
            snapshots.append({"cycle": int(cycle), **sample})

    env.register_cycle_observer(capture)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_a_valid_addr(env, uncache._MMIO_BASE, max_cycles=8000)
    assert any(
        sample["ifu_stall"] == 0
        and sample["entry_state"] == 1
        and sample["tl_a_valid"] == 1
        and sample["tl_a_ready"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-32:]}
    assert uncache._wait_for_uncache_resp(env, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, uncache._MMIO_BASE, max_cycles=8000)
    assert not env.monitor.get_errors()

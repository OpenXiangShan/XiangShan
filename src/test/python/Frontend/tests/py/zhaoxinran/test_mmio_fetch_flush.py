from __future__ import annotations

import pytest

from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


def _read_dut_signal(env, name: str) -> int:
    signal = getattr(env.dut, str(name), None)
    if signal is None:
        signal = env.dut.GetInternalSignal(str(name))
    assert signal is not None, {"missing_dut_signal": name}
    value = getattr(signal, "value", None)
    assert value is not None, {"unreadable_dut_signal": name}
    return int(value)


def _flush_snapshot(env) -> dict[str, int]:
    ifu = "Frontend_top.Frontend.inner_ifu."
    from_ftq_redirect = _read_dut_signal(
        env, f"{ifu}__Vtogcov__io_fromFtq_redirect_valid"
    )
    wb_redirect = _read_dut_signal(env, f"{ifu}__Vtogcov__wbRedirect_valid")
    wb_not_flush = _read_dut_signal(env, f"{ifu}s2_wbNotFlush")
    return {
        "entry_state": _read_dut_signal(
            env, "Frontend_top.Frontend.inner_instrUncache.entries_0.state"
        ),
        "entry_resending": _read_dut_signal(
            env, "Frontend_top.Frontend.inner_instrUncache.entries_0.resending"
        ),
        "prev_end_half": _read_dut_signal(
            env, f"{ifu}s2_prevEndIsHalfRviInfo_valid"
        ),
        "tl_a_valid": _read_dut_signal(
            env, "Frontend_top.auto_inner_instrUncache_client_out_a_valid"
        ),
        "tl_d_valid": _read_dut_signal(
            env, "Frontend_top.auto_inner_instrUncache_client_out_d_valid"
        ),
        "ifu_flush": int(
            from_ftq_redirect == 1
            or (wb_redirect == 1 and wb_not_flush == 0)
        ),
    }


def _register_flush_snapshot_observer(env):
    snapshots: list[dict[str, int]] = []

    def capture(cycle: int, active_env) -> None:
        snapshots.append(
            {
                "cycle": int(cycle),
                **_flush_snapshot(active_env),
            }
        )

    env.register_cycle_observer(capture)
    return snapshots


def _redirect_before_unaccepted_request(env, *, pending_addr: int, target_pc: int) -> None:
    assert uncache._wait_for_uncache_a_valid_addr(env, pending_addr, max_cycles=8000)
    assert pending_addr not in env.uncache_agent.get_stats().get("request_addrs", [])

    uncache._force_redirect_to(env, target_pc)
    env.step(8)
    assert pending_addr not in env.uncache_agent.get_stats().get("request_addrs", [])

    env.uncache_agent.set_a_ready(None)
    assert uncache._wait_for_request_addr(env, target_pc, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, target_pc, max_cycles=8000)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_flush_cancels_unaccepted_8b_resend(env):
    uncache._prepare_cross_beat_rvi_stream(env)
    snapshots = _register_flush_snapshot_observer(env)
    uncache._initialize_mmio_fetch(env, reset_vector=uncache._CROSS_BEAT_PC)

    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE, max_cycles=8000)
    env.uncache_agent.set_a_ready(0)
    assert uncache._wait_for_uncache_resp(env, max_cycles=8000)

    target_pc = uncache._MMIO_BASE + 0x40
    _redirect_before_unaccepted_request(
        env,
        pending_addr=uncache._MMIO_BASE + uncache._UNCACHE_BEAT_BYTES,
        target_pc=target_pc,
    )
    assert not any(
        int(item.pc) == uncache._CROSS_BEAT_PC for item in env.monitor.observations
    )
    assert any(
        sample["entry_resending"] == 1 and sample["ifu_flush"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    assert any(
        index > 0
        and snapshots[index - 1]["entry_resending"] == 1
        and sample["entry_resending"] == 0
        and sample["tl_a_valid"] == 0
        for index, sample in enumerate(snapshots)
    ), {"snapshots": snapshots[-64:]}


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_flush_cancels_unaccepted_page_half_recheck(env):
    uncache._prepare_cross_page_rvi_stream(env)
    snapshots = _register_flush_snapshot_observer(env)
    uncache._initialize_mmio_fetch(env, reset_vector=uncache._CROSS_PAGE_PC)

    first_beat = uncache._CROSS_PAGE_PC & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    next_page = uncache._MMIO_BASE + uncache._SV39_PAGE_SIZE
    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=8000)
    env.uncache_agent.set_a_ready(0)
    assert uncache._wait_for_uncache_resp(env, max_cycles=8000)

    target_pc = uncache._MMIO_BASE + 0x40
    _redirect_before_unaccepted_request(env, pending_addr=next_page, target_pc=target_pc)
    assert not any(
        int(item.pc) == uncache._CROSS_PAGE_PC for item in env.monitor.observations
    )
    assert any(
        sample["prev_end_half"] == 1 and sample["ifu_flush"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    assert any(
        index > 0
        and snapshots[index - 1]["prev_end_half"] == 1
        and sample["prev_end_half"] == 0
        and sample["entry_resending"] == 0
        for index, sample in enumerate(snapshots)
    ), {"snapshots": snapshots[-64:]}


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_d_response_coincides_with_ifu_flush(env):
    """Deliver TL-D on the cycle a redirect flushes the pending MMIO."""
    uncache._prepare_mmio_cnop_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=32)
    snapshots = _register_flush_snapshot_observer(env)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE, max_cycles=8000)
    assert env.uncache_agent.pending
    response_cycle = int(env.uncache_agent.pending[0].ready_cycle)
    while int(env.current_cycle) < response_cycle - 2:
        env.step(1)

    target_pc = uncache._MMIO_BASE + 0x40
    uncache._force_redirect_to(env, target_pc)
    env.step(8)

    assert any(
        sample["entry_state"] == uncache._INSTR_UNCACHE_REFILL_RESP
        and sample["tl_d_valid"] == 1
        and sample["ifu_flush"] == 1
        for sample in snapshots
    ), {
        "response_cycle": response_cycle,
        "snapshots": snapshots[-64:],
    }
    assert uncache._wait_for_request_addr(env, target_pc, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, target_pc, max_cycles=8000)
    assert not any(int(item.pc) == uncache._MMIO_BASE for item in env.monitor.observations)
    assert not env.monitor.get_errors()

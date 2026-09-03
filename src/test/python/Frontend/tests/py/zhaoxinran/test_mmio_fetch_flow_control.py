from __future__ import annotations

import pytest

from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


_WAIT_LAST_COMMIT = 1
_SEND_REQ = 2


def _read_dut_signal(env, name: str) -> int:
    signal = getattr(env.dut, str(name), None)
    if signal is None:
        signal = env.dut.GetInternalSignal(str(name))
    assert signal is not None, {"missing_dut_signal": name}
    value = getattr(signal, "value", None)
    assert value is not None, {"unreadable_dut_signal": name}
    return int(value)


def _flow_snapshot(env) -> dict[str, int]:
    prefix = "Frontend_top.Frontend."
    ibuffer = f"{prefix}inner_ibuffer."
    enq_ptr = (
        _read_dut_signal(env, f"{ibuffer}enqPtrDup_0_flag"),
        _read_dut_signal(env, f"{ibuffer}enqPtrDup_0_value"),
    )
    deq_ptr = (
        _read_dut_signal(env, f"{ibuffer}deqPtrVec_0_flag"),
        _read_dut_signal(env, f"{ibuffer}deqPtrVec_0_value"),
    )
    return {
        "uncache_state": _read_dut_signal(
            env, f"{prefix}inner_ifu.uncacheUnit.uncacheState"
        ),
        "empty_after": _read_dut_signal(
            env, f"{prefix}inner_ifu.uncacheUnit.io_emptyAfter"
        ),
        "ifu_stall": 1
        - _read_dut_signal(env, f"{ibuffer}allowEnq"),
        "ibuffer_empty": int(
            enq_ptr == deq_ptr
            and _read_dut_signal(env, f"{ibuffer}outputEntries_0_valid") == 0
        ),
        "backend_empty": _read_dut_signal(env, "io_backend_backendEmpty"),
        "backend_accept": _read_dut_signal(
            env, "io_backend_toIBuf_decodeCanAccept"
        ),
        "tl_a_valid": _read_dut_signal(
            env, "Frontend_top.auto_inner_instrUncache_client_out_a_valid"
        ),
    }


def _register_flow_snapshot_observer(env):
    snapshots: list[dict[str, int]] = []

    def capture(cycle: int, active_env) -> None:
        sample = _flow_snapshot(active_env)
        sample["cycle"] = int(cycle)
        sample["cfvec_valid"] = int(
            any(int(signal.value) for signal in active_env.backend_observe_if.cfvec_valid)
        )
        snapshots.append(sample)

    env.register_cycle_observer(capture)
    return snapshots


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_wait_last_commit_holds_request_while_ibuffer_is_nonempty(env):
    uncache._prepare_mmio_cnop_stream(env)
    env.backend_model.set_can_accept(0)
    snapshots: list[dict[str, int]] = []

    def capture(cycle: int, active_env) -> None:
        sample = _flow_snapshot(active_env)
        sample["cycle"] = int(cycle)
        sample["cfvec_valid"] = int(
            any(int(signal.value) for signal in active_env.backend_observe_if.cfvec_valid)
        )
        snapshots.append(sample)

    env.register_cycle_observer(capture)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_req(env)
    assert uncache._wait_for_uncache_resp(env)
    start_req_count = int(env.uncache_agent.get_stats().get("req_count", 0))
    for _ in range(256):
        if any(
            sample["uncache_state"] == _WAIT_LAST_COMMIT
            and sample["ibuffer_empty"] == 0
            and sample["cfvec_valid"] == 1
            for sample in snapshots
        ):
            break
        env.step(1)

    matching = [
        sample
        for sample in snapshots
        if sample["uncache_state"] == _WAIT_LAST_COMMIT
        and sample["ibuffer_empty"] == 0
        and sample["cfvec_valid"] == 1
    ]
    assert matching, {
        "states": [
            (
                sample["cycle"],
                sample["uncache_state"],
                sample["ibuffer_empty"],
                sample["cfvec_valid"],
            )
            for sample in snapshots[-64:]
        ]
    }
    assert all(sample["tl_a_valid"] == 0 for sample in matching)
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == start_req_count

    env.backend_model.set_can_accept(1)
    assert uncache._wait_for_uncache_req(env)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_wait_last_commit_keeps_request_when_backend_nonempty_and_ibuffer_empty(env):
    """Construct WAIT_LAST_COMMIT with backend work visible but an empty IBuffer."""
    uncache._prepare_mmio_cnop_stream(env)
    env.backend_model.set_can_accept(1)
    env.backend_model.backend_empty_for_dut = lambda: 0
    snapshots = _register_flow_snapshot_observer(env)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_req(env)
    assert uncache._wait_for_uncache_resp(env)
    for _ in range(256):
        if any(
            sample["uncache_state"] == _WAIT_LAST_COMMIT
            and sample["backend_empty"] == 0
            and sample["ibuffer_empty"] == 1
            for sample in snapshots
        ):
            break
        env.step(1)

    assert any(
        sample["uncache_state"] == _WAIT_LAST_COMMIT
        and sample["backend_empty"] == 0
        and sample["ibuffer_empty"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_empty_release_enters_send_req_before_tl_a(env):
    """Release WAIT_LAST_COMMIT with both queues empty before TL-A starts."""
    uncache._prepare_mmio_cnop_stream(env)
    env.backend_model.set_can_accept(1)
    env.backend_model.backend_empty_for_dut = lambda: 1
    snapshots = _register_flow_snapshot_observer(env)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_req(env)
    assert uncache._wait_for_uncache_resp(env)
    for _ in range(512):
        if (
            snapshots
            and snapshots[-1]["uncache_state"] == _WAIT_LAST_COMMIT
            and snapshots[-1]["empty_after"] == 1
        ):
            break
        env.step(1)
    assert snapshots[-1]["uncache_state"] == _WAIT_LAST_COMMIT
    assert snapshots[-1]["empty_after"] == 1
    for _ in range(512):
        if any(
            sample["uncache_state"] == _SEND_REQ
            and sample["empty_after"] == 1
            and sample["ifu_stall"] == 0
            and sample["tl_a_valid"] == 0
            for sample in snapshots
        ):
            break
        env.step(1)

    assert any(
        sample["uncache_state"] == _SEND_REQ
        and sample["empty_after"] == 1
        and sample["ifu_stall"] == 0
        and sample["tl_a_valid"] == 0
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    assert uncache._wait_for_uncache_req(env)
    assert uncache._wait_for_uncache_resp(env)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_backend_can_accept_rise_coincides_with_cfvec_valid(env):
    """Raise backend acceptance only after a pending MMIO has produced cfVec."""
    uncache._prepare_mmio_cnop_stream(env)
    env.backend_model.set_can_accept(0)
    snapshots = _register_flow_snapshot_observer(env)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_req(env)
    assert uncache._wait_for_uncache_resp(env)
    for _ in range(512):
        if snapshots and snapshots[-1]["cfvec_valid"] == 1:
            break
        env.step(1)
    assert snapshots[-1]["backend_accept"] == 0
    assert snapshots[-1]["cfvec_valid"] == 1
    env.backend_model.set_can_accept(1)
    for _ in range(512):
        if any(
            index > 0
            and snapshots[index - 1]["backend_accept"] == 0
            and sample["backend_accept"] == 1
            and sample["cfvec_valid"] == 1
            for index, sample in enumerate(snapshots)
        ):
            break
        env.step(1)

    assert any(
        index > 0
        and snapshots[index - 1]["backend_accept"] == 0
        and sample["backend_accept"] == 1
        and sample["cfvec_valid"] == 1
        for index, sample in enumerate(snapshots)
    ), {"snapshots": snapshots[-64:]}
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_backend_can_accept_fall_happens_without_cfvec(env):
    """Drop backend acceptance while a pending MMIO has no visible cfVec."""
    uncache._prepare_mmio_cnop_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=32)
    env.backend_model.set_can_accept(1)
    snapshots = _register_flow_snapshot_observer(env)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_req(env)
    for _ in range(512):
        if (
            snapshots
            and snapshots[-1]["uncache_state"] != 0
            and snapshots[-1]["cfvec_valid"] == 0
        ):
            break
        env.step(1)
    assert snapshots[-1]["uncache_state"] != 0
    assert snapshots[-1]["cfvec_valid"] == 0, {"snapshots": snapshots[-64:]}
    env.backend_model.set_can_accept(0)
    for _ in range(512):
        if any(
            index > 0
            and snapshots[index - 1]["backend_accept"] == 1
            and sample["backend_accept"] == 0
            and sample["cfvec_valid"] == 0
            for index, sample in enumerate(snapshots)
        ):
            break
        env.step(1)

    assert any(
        index > 0
        and snapshots[index - 1]["backend_accept"] == 1
        and sample["backend_accept"] == 0
        and sample["cfvec_valid"] == 0
        for index, sample in enumerate(snapshots)
    ), {"snapshots": snapshots[-64:]}
    env.backend_model.set_can_accept(1)
    assert uncache._wait_for_uncache_resp(env)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_send_req_ibuffer_stall_suppresses_tl_a(env):
    """Use a legal PBMT.NC request to hold SEND_REQ under IBuffer stall."""
    expected, mapping = uncache._prepare_sv39_mapped_pbmt_nc_cfi_stream(
        env,
        vaddr=uncache._NORMAL_BASE,
        paddr=uncache._NORMAL_PHYS_BASE,
        instr_count=4096,
    )
    snapshots = _register_flow_snapshot_observer(env)
    uncache._initialize_sv39_fetch(env, reset_vector=mapping.vaddr)
    uncache._configure_exec_attrs_16k(env, base_addr=0x80000000)
    env.backend_model.set_can_accept(0)
    uncache._force_redirect_to(env, mapping.vaddr)

    for _ in range(4000):
        if any(
            sample["uncache_state"] == _SEND_REQ
            and sample["ifu_stall"] == 1
            and sample["tl_a_valid"] == 0
            for sample in snapshots
        ):
            break
        env.step(1)

    assert any(
        sample["uncache_state"] == _SEND_REQ
        and sample["ifu_stall"] == 1
        and sample["tl_a_valid"] == 0
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    env.backend_model.set_can_accept(1)
    assert uncache._wait_for_request_addr(env, mapping.paddr, max_cycles=6000)
    assert uncache._wait_for_uncache_resp(env)
    assert uncache._wait_for_observed_pc(env, expected[0][0], max_cycles=12000)
    assert not env.monitor.get_errors()

from __future__ import annotations

import pytest
from env.funcov.py.ifu import mmio_nc_owner_funcov as owner_funcov
from tests.py.zhaoxinran import test_instr_uncache_port_boundaries as uncache


def _load_mmio_payload(env, payload: bytes) -> None:
    env.memory.mmio_ranges.append(
        (uncache._MMIO_BASE, uncache._MMIO_BASE + len(payload))
    )
    uncache.LoadProgramSequence(
        image=uncache.ProgramImage(payload=bytes(payload), base_addr=uncache._MMIO_BASE),
        step_cycles=0,
    ).run(env)


def _rvi_rvc_payload(*, first: str) -> bytes:
    rvi = int(uncache._ADDI_X0_X0_0).to_bytes(4, "little")
    rvc = int(uncache._CNOP).to_bytes(2, "little")
    padding = rvc * 128
    return (rvi + rvc + padding) if first == "rvi" else (rvc + rvi + padding)


@pytest.mark.parametrize("offset", [0, 2, 4])
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_rvi_non_tail_8b_offsets_deliver_instruction(env, offset: int):
    payload = bytearray(int(uncache._CNOP).to_bytes(2, "little") * 64)
    payload[offset : offset + 4] = int(uncache._ADDI_X0_X0_0).to_bytes(4, "little")
    _load_mmio_payload(env, bytes(payload))
    pc = uncache._MMIO_BASE + int(offset)
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    first_beat = pc & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    assert uncache._wait_for_request_addr(env, first_beat)
    assert uncache._wait_for_observed_pc(env, pc, max_cycles=8000), {
        "pc": hex(pc),
        "observed": [(int(item.pc), int(item.instr)) for item in env.monitor.observations[-16:]],
        "uncache": env.uncache_agent.get_stats(),
    }
    observed = next(item for item in env.monitor.observations if int(item.pc) == pc)
    assert int(observed.instr) == uncache._ADDI_X0_X0_0
    assert not bool(observed.is_rvc)
    assert first_beat in env.uncache_agent.get_stats().get("request_addrs", [])
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_rvc_at_8b_tail_advances_by_2b_without_second_beat(env):
    payload = bytearray(int(uncache._CNOP).to_bytes(2, "little") * 64)
    payload[6:8] = int(uncache._CNOP).to_bytes(2, "little")
    _load_mmio_payload(env, bytes(payload))
    pc = uncache._MMIO_BASE + 6
    uncache._initialize_mmio_fetch(env, reset_vector=pc)

    first_beat = uncache._MMIO_BASE
    assert uncache._wait_for_request_addr(env, first_beat)
    assert uncache._wait_for_observed_pc(env, pc, max_cycles=8000)
    stats_at_delivery = env.uncache_agent.get_stats()
    assert stats_at_delivery.get("request_addrs", []).count(first_beat) == 1
    assert uncache._MMIO_BASE + 8 not in stats_at_delivery.get("request_addrs", [])
    assert uncache._wait_for_observed_pc(env, pc + 2, max_cycles=8000)
    observed = {
        int(item.pc): item
        for item in env.monitor.observations
        if int(item.pc) in {pc, pc + 2}
    }
    assert bool(observed[pc].is_rvc)
    assert bool(observed[pc + 2].is_rvc)
    assert not env.monitor.get_errors()


@pytest.mark.parametrize("first", ["rvc", "rvi"])
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_adjacent_rvc_rvi_stream_preserves_pc_progress(env, first: str):
    payload = _rvi_rvc_payload(first=first)
    _load_mmio_payload(env, payload)
    uncache._initialize_mmio_fetch(env)

    first_pc = uncache._MMIO_BASE
    second_pc = first_pc + (2 if first == "rvc" else 4)
    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE)
    assert uncache._wait_for_observed_pc(env, first_pc, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, second_pc, max_cycles=8000)
    observed = {
        int(item.pc): item
        for item in env.monitor.observations
        if int(item.pc) in {first_pc, second_pc}
    }
    assert int(observed[first_pc].instr) == uncache._ADDI_X0_X0_0
    assert int(observed[second_pc].instr) == uncache._ADDI_X0_X0_0
    assert bool(observed[first_pc].is_rvc) is (first == "rvc")
    assert bool(observed[second_pc].is_rvc) is (first != "rvc")
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_cross_8b_clean_rvi_requests_next_beat_and_delivers(env):
    uncache._prepare_cross_beat_rvi_stream(env)
    uncache._initialize_mmio_fetch(env, reset_vector=uncache._CROSS_BEAT_PC)

    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE)
    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE + 8)
    assert uncache._wait_for_observed_pc(env, uncache._CROSS_BEAT_PC, max_cycles=8000)
    observed = next(
        item for item in env.monitor.observations if int(item.pc) == uncache._CROSS_BEAT_PC
    )
    assert int(observed.instr) == uncache._ADDI_X0_X0_0
    assert not bool(observed.is_rvc)
    stats = env.uncache_agent.get_stats()
    assert stats.get("request_addrs", []).count(uncache._MMIO_BASE) == 1
    assert stats.get("request_addrs", []).count(uncache._MMIO_BASE + 8) == 1
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "fault",
    [
        pytest.param({"corrupt": 1}, id="corrupt"),
        pytest.param({"corrupt": 1, "denied": 1}, id="corrupt_and_denied"),
    ],
)
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_cross_8b_first_beat_fault_reports_without_resend(env, fault: dict[str, int]):
    uncache._prepare_cross_beat_rvi_stream(env)
    env.uncache_agent.inject_response_fault_at(
        uncache._MMIO_BASE,
        **fault,
    )
    uncache._initialize_mmio_fetch(env, reset_vector=uncache._CROSS_BEAT_PC)

    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE)
    assert uncache._wait_for_uncache_resp(env)
    assert uncache._wait_for_monitor_exception(env)
    stats = env.uncache_agent.get_stats()
    assert int(stats.get("corrupt_resp_count", 0)) == 1
    assert int(stats.get("denied_resp_count", 0)) == int(fault.get("denied", 0))
    assert stats.get("request_addrs", []).count(uncache._MMIO_BASE) == 1
    assert uncache._MMIO_BASE + 8 not in stats.get("request_addrs", [])
    assert not env.monitor.get_errors()


@pytest.mark.parametrize("fault", [None, "corrupt", "denied", "corrupt_and_denied"])
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_cross_8b_second_beat_response_modes(env, fault: str | None):
    uncache._prepare_cross_beat_rvi_stream(env)
    if fault is not None:
        env.uncache_agent.inject_response_fault_at(
            uncache._MMIO_BASE + 8,
            corrupt=int(fault in {"corrupt", "corrupt_and_denied"}),
            denied=int(fault in {"denied", "corrupt_and_denied"}),
        )
    uncache._initialize_mmio_fetch(env, reset_vector=uncache._CROSS_BEAT_PC)

    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE)
    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE + 8)
    assert uncache._wait_for_resp_count(env, 2)
    if fault is None:
        assert uncache._wait_for_observed_pc(env, uncache._CROSS_BEAT_PC, max_cycles=8000)
        observed = next(
            item for item in env.monitor.observations if int(item.pc) == uncache._CROSS_BEAT_PC
        )
        assert int(observed.instr) == uncache._ADDI_X0_X0_0
        assert not bool(observed.is_rvc)
        assert env.monitor.exception_mark_count == 0
    else:
        assert uncache._wait_for_monitor_exception(env)
        assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()


@pytest.mark.parametrize("fault", [{"corrupt": 1}, {"denied": 1}])
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_single_beat_d_response_fault_is_reported(env, fault: dict[str, int]):
    uncache._prepare_mmio_cnop_stream(env)
    env.uncache_agent.inject_next_response_fault(**fault)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_req(env)
    assert uncache._wait_for_uncache_resp(env)
    assert uncache._wait_for_monitor_exception(env)
    stats = env.uncache_agent.get_stats()
    if fault.get("corrupt"):
        assert int(stats.get("corrupt_resp_count", 0)) == 1
    else:
        assert int(stats.get("denied_resp_count", 0)) == 1
    assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_branch_instruction_is_delivered_as_control_flow(env):
    branch = int(0x00000263).to_bytes(4, "little")
    payload = branch + int(uncache._ADDI_X0_X0_0).to_bytes(4, "little")
    payload += int(uncache._CNOP).to_bytes(2, "little") * 128
    _load_mmio_payload(env, payload)
    snapshots: list[dict[str, int | None]] = []

    def capture(cycle: int, active_env) -> None:
        snapshots.append(
            {
                "cycle": int(cycle),
                **owner_funcov._snapshot(
                    active_env.functional_coverage, active_env.dut
                ),
            }
        )

    env.register_cycle_observer(capture)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE)
    assert any(
        sample["req_valid"] == 1
        and sample["is_first"] == 1
        and sample["uncache_state"] == uncache._IFU_UNCACHE_SEND_REQ
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    assert uncache._wait_for_observed_pc(env, uncache._MMIO_BASE, max_cycles=8000)
    observed = next(
        item for item in env.monitor.observations if int(item.pc) == uncache._MMIO_BASE
    )
    assert int(observed.instr) == 0x00000263
    assert not bool(observed.is_rvc)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_jal_instruction_is_delivered_as_control_flow(env):
    payload = int(uncache._JAL_X0_PLUS_4).to_bytes(4, "little")
    payload += int(uncache._ADDI_X0_X0_0).to_bytes(4, "little")
    payload += int(uncache._CNOP).to_bytes(2, "little") * 128
    _load_mmio_payload(env, payload)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE)
    assert uncache._wait_for_observed_pc(env, uncache._MMIO_BASE, max_cycles=8000)
    observed = next(
        item for item in env.monitor.observations if int(item.pc) == uncache._MMIO_BASE
    )
    assert int(observed.instr) == uncache._JAL_X0_PLUS_4
    assert not bool(observed.is_rvc)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_tl_a_backpressure_holds_request_until_accepted(env):
    uncache._prepare_mmio_cnop_stream(env)
    env.uncache_agent.set_a_ready(0)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_a_valid_addr(env, uncache._MMIO_BASE)
    stalled_stats = env.uncache_agent.get_stats()
    stalled_req_count = int(stalled_stats.get("req_count", 0))
    assert int(env.uncache_if.a_valid.value) == 1
    assert int(env.uncache_if.a_bits_address.value) == uncache._MMIO_BASE
    env.step(8)
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == stalled_req_count
    assert int(env.uncache_if.a_valid.value) == 1
    assert int(env.uncache_if.a_bits_address.value) == uncache._MMIO_BASE

    env.uncache_agent.set_a_ready(None)
    assert uncache._wait_for_request_addr(env, uncache._MMIO_BASE)
    assert uncache._wait_for_uncache_resp(env)
    assert uncache._wait_for_observed_pc(env, uncache._MMIO_BASE, max_cycles=8000)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_redirect_drops_a_ready_stalled_request(env):
    uncache._prepare_mmio_cnop_stream(env)
    target_pc = uncache._MMIO_BASE + 0x40
    env.uncache_agent.set_a_ready(0)
    snapshots: list[dict[str, int | None]] = []

    def capture(cycle: int, active_env) -> None:
        snapshots.append(
            {
                "cycle": int(cycle),
                **owner_funcov._snapshot(
                    active_env.functional_coverage, active_env.dut
                ),
            }
        )

    env.register_cycle_observer(capture)
    uncache._initialize_mmio_fetch(env)

    assert uncache._wait_for_uncache_a_valid_addr(env, uncache._MMIO_BASE)
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == 0
    uncache._force_redirect_to(env, target_pc)
    env.step(8)
    assert uncache._MMIO_BASE not in env.uncache_agent.get_stats().get("request_addrs", [])
    assert any(
        sample["uncache_state"] == uncache._IFU_UNCACHE_SEND_REQ
        and sample["tl_a_valid"] == 1
        and sample["tl_a_ready"] == 0
        and sample["backend_redirect"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}

    env.uncache_agent.set_a_ready(None)
    assert uncache._wait_for_request_addr(env, target_pc, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, target_pc, max_cycles=8000)
    assert not any(int(item.pc) == uncache._MMIO_BASE for item in env.monitor.observations)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_redirect_cancels_wait_last_commit_before_request(env):
    """Redirect a non-first MMIO while it is still commit ordered."""
    uncache._prepare_mmio_cnop_stream(env)
    env.backend_model.set_can_accept(1)
    env.backend_model.backend_empty_for_dut = lambda: 0
    snapshots: list[dict[str, int | None]] = []

    def capture(cycle: int, active_env) -> None:
        snapshots.append(
            {
                "cycle": int(cycle),
                **owner_funcov._snapshot(
                    active_env.functional_coverage, active_env.dut
                ),
            }
        )

    env.register_cycle_observer(capture)
    uncache._initialize_mmio_fetch(env)

    for _ in range(8000):
        if snapshots and snapshots[-1]["uncache_state"] == uncache._IFU_UNCACHE_WAIT_LAST_COMMIT:
            break
        env.step(1)
    assert snapshots[-1]["uncache_state"] == uncache._IFU_UNCACHE_WAIT_LAST_COMMIT, {
        "snapshots": snapshots[-64:]
    }

    target_pc = uncache._MMIO_BASE + 0x40
    old_req_count = int(env.uncache_agent.get_stats().get("req_count", 0))
    uncache._force_redirect_to(env, target_pc)
    env.step(8)

    assert any(
        sample["uncache_state"] == uncache._IFU_UNCACHE_WAIT_LAST_COMMIT
        and sample["backend_redirect"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    assert any(
        sample["uncache_state"] == uncache._IFU_UNCACHE_WAIT_LAST_COMMIT
        and sample["ifu_flush"] == 1
        for sample in snapshots
    ), {"snapshots": snapshots[-64:]}
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == old_req_count

    assert uncache._wait_for_request_addr(env, target_pc, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, target_pc, max_cycles=8000)
    assert not any(int(item.pc) == uncache._MMIO_BASE for item in env.monitor.observations)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_page_tail_rvi_rechecks_next_page_before_delivery(env):
    uncache._prepare_cross_page_rvi_stream(env)
    uncache._initialize_mmio_fetch(env, reset_vector=uncache._CROSS_PAGE_PC)

    first_beat = uncache._CROSS_PAGE_PC & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    next_page = uncache._MMIO_BASE + uncache._SV39_PAGE_SIZE
    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=8000)
    assert uncache._wait_for_request_addr(env, next_page, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, uncache._CROSS_PAGE_PC, max_cycles=8000)
    observed = next(
        item for item in env.monitor.observations if int(item.pc) == uncache._CROSS_PAGE_PC
    )
    assert int(observed.instr) == uncache._ADDI_X0_X0_0
    assert not bool(observed.is_rvc)
    stats = env.uncache_agent.get_stats()
    assert stats.get("request_addrs", []).count(first_beat) == 1
    assert stats.get("request_addrs", []).count(next_page) == 1
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_page_tail_rvc_delivers_before_next_page_fetch(env):
    uncache._prepare_cross_page_rvc_stream(env)
    uncache._initialize_mmio_fetch(env, reset_vector=uncache._CROSS_PAGE_PC)

    first_beat = uncache._CROSS_PAGE_PC & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    next_page = uncache._MMIO_BASE + uncache._SV39_PAGE_SIZE
    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=8000)
    assert uncache._wait_for_observed_pc(env, uncache._CROSS_PAGE_PC, max_cycles=8000)
    stats_at_delivery = env.uncache_agent.get_stats()
    assert next_page not in stats_at_delivery.get("request_addrs", [])
    assert uncache._wait_for_observed_pc(env, uncache._CROSS_PAGE_PC + 2, max_cycles=8000)
    observed = {
        int(item.pc): item
        for item in env.monitor.observations
        if int(item.pc) in {uncache._CROSS_PAGE_PC, uncache._CROSS_PAGE_PC + 2}
    }
    assert bool(observed[uncache._CROSS_PAGE_PC].is_rvc)
    assert bool(observed[uncache._CROSS_PAGE_PC + 2].is_rvc)
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "fault,expect_page_recheck",
    [
        pytest.param({"corrupt": 1}, False, id="corrupt"),
        pytest.param({"denied": 1}, True, id="denied"),
    ],
)
@pytest.mark.skipif(not uncache._RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_mmio_page_tail_first_beat_fault_preserves_resend_contract(
    env, fault: dict[str, int], expect_page_recheck: bool
):
    uncache._prepare_cross_page_rvi_stream(env)
    first_beat = uncache._CROSS_PAGE_PC & ~(uncache._UNCACHE_BEAT_BYTES - 1)
    next_page = uncache._MMIO_BASE + uncache._SV39_PAGE_SIZE
    env.uncache_agent.inject_response_fault_at(first_beat, **fault)
    uncache._initialize_mmio_fetch(env, reset_vector=uncache._CROSS_PAGE_PC)

    assert uncache._wait_for_request_addr(env, first_beat, max_cycles=8000)
    assert uncache._wait_for_uncache_resp(env)
    stats = env.uncache_agent.get_stats()
    assert int(stats.get("corrupt_resp_count", 0)) == int(fault.get("corrupt", 0))
    assert int(stats.get("denied_resp_count", 0)) == int(fault.get("denied", 0))
    if expect_page_recheck:
        assert uncache._wait_for_request_addr(env, next_page, max_cycles=8000)
        assert uncache._wait_for_observed_pc(env, uncache._CROSS_PAGE_PC, max_cycles=8000)
        assert env.monitor.exception_mark_count == 0
    else:
        assert uncache._wait_for_monitor_exception(env)
        assert next_page not in stats.get("request_addrs", [])
        assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()

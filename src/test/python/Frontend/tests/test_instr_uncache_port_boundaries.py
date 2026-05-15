from __future__ import annotations

import os

import pytest

from env.sequences import InjectRedirectSequence, LoadProgramSequence
from env.transactions import ProgramImage, RedirectTxn


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_MMIO_BASE = 0x10001000
_CROSS_BEAT_PC = _MMIO_BASE + 0x6
_CNOP = 0x0001
_ADDI_X0_X0_0 = 0x00000013


def _prepare_mmio_cnop_stream(env, *, instr_count: int = 256) -> None:
    payload = int(_CNOP).to_bytes(2, "little") * int(instr_count)
    env.memory.mmio_ranges.append((_MMIO_BASE, _MMIO_BASE + len(payload)))
    LoadProgramSequence(image=ProgramImage(payload=payload, base_addr=_MMIO_BASE), step_cycles=0).run(env)


def _prepare_cross_beat_rvi_stream(env) -> None:
    payload = bytearray()
    payload.extend(int(_CNOP).to_bytes(2, "little"))
    payload.extend(int(_CNOP).to_bytes(2, "little"))
    payload.extend(int(_CNOP).to_bytes(2, "little"))
    payload.extend(int(_ADDI_X0_X0_0).to_bytes(4, "little"))
    payload.extend(int(_CNOP).to_bytes(2, "little") * 128)
    env.memory.mmio_ranges.append((_MMIO_BASE, _MMIO_BASE + len(payload)))
    LoadProgramSequence(image=ProgramImage(payload=bytes(payload), base_addr=_MMIO_BASE), step_cycles=0).run(env)


def _initialize_mmio_fetch(env, *, reset_vector: int = _MMIO_BASE) -> None:
    env.initialize(reset_vector=int(reset_vector), bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(int(reset_vector))


def _wait_for_uncache_req(env, *, max_cycles: int = 2000) -> int:
    start = int(env.uncache_agent.get_stats().get("req_count", 0))
    for _ in range(int(max_cycles)):
        env.step(1)
        now = int(env.uncache_agent.get_stats().get("req_count", 0))
        if now > start:
            return now
    return int(env.uncache_agent.get_stats().get("req_count", 0))


def _wait_for_uncache_resp(env, *, max_cycles: int = 2000) -> int:
    start = int(env.uncache_agent.get_stats().get("resp_count", 0))
    for _ in range(int(max_cycles)):
        env.step(1)
        now = int(env.uncache_agent.get_stats().get("resp_count", 0))
        if now > start:
            return now
    return int(env.uncache_agent.get_stats().get("resp_count", 0))


def _wait_for_request_addr(env, addr: int, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if int(addr) in env.uncache_agent.get_stats().get("request_addrs", []):
            return True
        env.step(1)
    return int(addr) in env.uncache_agent.get_stats().get("request_addrs", [])


def _wait_for_resp_count(env, count: int, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if int(env.uncache_agent.get_stats().get("resp_count", 0)) >= int(count):
            return True
        env.step(1)
    return int(env.uncache_agent.get_stats().get("resp_count", 0)) >= int(count)


def _wait_for_frontend_exception(env, bin_name: str, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if env.functional_coverage.key_hit("frontend_exception_type", str(bin_name)):
            return True
        env.step(1)
    return env.functional_coverage.key_hit("frontend_exception_type", str(bin_name))


def _wait_for_observed_pc(env, pc: int, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if any(int(obs.pc) == int(pc) for obs in env.monitor.observations):
            return True
        env.step(1)
    return any(int(obs.pc) == int(pc) for obs in env.monitor.observations)


def _read_dut_signal(env, name: str, default: int = 0) -> int:
    try:
        value = getattr(env.dut, name).value
        return int(value)
    except Exception:
        return int(default)


def _force_redirect_to(env, target_pc: int) -> None:
    env.backend_model.inject_redirect(int(target_pc), "ctrl_redirect", delay_cycles=0)


def _count_mmio_observations(env) -> int:
    return sum(1 for obs in env.monitor.observations if _MMIO_BASE <= int(obs.pc) < (_MMIO_BASE + 0x1000))


def _wait_for_mmio_observations(env, *, min_count: int, max_cycles: int) -> int:
    for _ in range(max(0, int(max_cycles))):
        count = _count_mmio_observations(env)
        if count >= int(min_count):
            return count
        env.step(1)
    return _count_mmio_observations(env)


def _recent_mmio_pcs(env, *, window: int) -> list[int]:
    return [
        int(obs.pc)
        for obs in list(env.monitor.observations)[-max(0, int(window)) :]
        if _MMIO_BASE <= int(obs.pc) < (_MMIO_BASE + 0x1000)
    ]


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_a_ready_backpressure_holds_request(env):
    _prepare_mmio_cnop_stream(env)
    _initialize_mmio_fetch(env)

    first_req = _wait_for_uncache_req(env)
    first_resp = _wait_for_uncache_resp(env)
    assert first_req > 0
    assert first_resp > 0

    env.uncache_agent.set_a_ready(0)
    for _ in range(256):
        env.step(1)
        if int(env.uncache_if.a_valid.value) == 1:
            break

    req_before = int(env.uncache_agent.get_stats().get("req_count", 0))
    assert int(env.uncache_if.a_ready.value) == 0
    assert int(env.uncache_if.a_valid.value) == 1

    env.step(8)
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) == req_before

    env.uncache_agent.set_a_ready(None)
    req_after = _wait_for_uncache_req(env)

    assert req_after > req_before
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_corrupt_response_injection(env):
    _prepare_mmio_cnop_stream(env)
    env.uncache_agent.inject_next_response_fault(corrupt=1)
    _initialize_mmio_fetch(env)

    req_count = _wait_for_uncache_req(env)
    resp_count = _wait_for_uncache_resp(env)
    stats = env.uncache_agent.get_stats()

    assert req_count > 0
    assert resp_count > 0
    assert int(stats.get("corrupt_resp_count", 0)) == 1
    assert int(stats.get("denied_resp_count", 0)) == 0
    assert _wait_for_frontend_exception(env, "hwe")
    assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_denied_response_injection(env):
    _prepare_mmio_cnop_stream(env)
    env.uncache_agent.inject_next_response_fault(denied=1)
    _initialize_mmio_fetch(env)

    req_count = _wait_for_uncache_req(env)
    resp_count = _wait_for_uncache_resp(env)
    stats = env.uncache_agent.get_stats()

    assert req_count > 0
    assert resp_count > 0
    assert int(stats.get("denied_resp_count", 0)) == 1
    assert int(stats.get("corrupt_resp_count", 0)) == 0
    assert _wait_for_frontend_exception(env, "af")
    assert env.functional_coverage.key_hit("fetch_path_x_exception", "mmio_x_af")
    assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_wfi_req_blocks_new_acquire(env):
    _prepare_mmio_cnop_stream(env)
    env.backend_model.set_wfi_req(1)
    _initialize_mmio_fetch(env)

    req_before = int(env.uncache_agent.get_stats().get("req_count", 0))
    env.step(64)
    req_during_wfi = int(env.uncache_agent.get_stats().get("req_count", 0))

    env.backend_model.set_wfi_req(0)
    req_after = _wait_for_uncache_req(env)

    assert req_during_wfi == req_before
    assert req_after > req_during_wfi
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_pending_response_flushed_by_redirect(env):
    _prepare_mmio_cnop_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=32)
    _initialize_mmio_fetch(env)

    assert _wait_for_uncache_req(env)
    req_before_redirect = int(env.uncache_agent.get_stats().get("req_count", 0))
    _force_redirect_to(env, _MMIO_BASE + 0x40)
    assert _wait_for_observed_pc(env, _MMIO_BASE + 0x40)
    assert _wait_for_uncache_resp(env, max_cycles=4000)

    assert int(env.uncache_agent.get_stats().get("resp_count", 0)) > 0
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) >= req_before_redirect
    assert not any(int(obs.pc) == _MMIO_BASE for obs in env.monitor.observations)
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_resend_first_beat_corrupt_suppresses_resend(env):
    _prepare_cross_beat_rvi_stream(env)
    env.uncache_agent.inject_response_fault_at(
        _MMIO_BASE,
        corrupt=1,
    )
    _initialize_mmio_fetch(env, reset_vector=_CROSS_BEAT_PC)

    assert _wait_for_request_addr(env, _MMIO_BASE)
    assert _wait_for_uncache_resp(env)
    assert _wait_for_frontend_exception(env, "hwe")
    stats = env.uncache_agent.get_stats()

    assert stats.get("request_addrs", []).count(_MMIO_BASE) == 1
    assert (_MMIO_BASE + 8) not in stats.get("request_addrs", [])
    assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_resend_first_beat_denied_allows_resend(env):
    _prepare_cross_beat_rvi_stream(env)
    env.uncache_agent.inject_response_fault_at(
        _MMIO_BASE,
        denied=1,
    )
    _initialize_mmio_fetch(env, reset_vector=_CROSS_BEAT_PC)

    assert _wait_for_request_addr(env, _MMIO_BASE)
    assert _wait_for_request_addr(env, _MMIO_BASE + 8)
    assert _wait_for_resp_count(env, 2)
    stats = env.uncache_agent.get_stats()

    assert int(stats.get("denied_resp_count", 0)) == 1
    assert int(stats.get("corrupt_resp_count", 0)) == 0
    assert stats.get("request_addrs", []).count(_MMIO_BASE) == 1
    assert (_MMIO_BASE + 8) in stats.get("request_addrs", [])
    assert not env.functional_coverage.key_hit("frontend_exception_type", "af")
    assert not env.functional_coverage.key_hit("frontend_exception_type", "hwe")
    assert env.monitor.exception_mark_count == 0
    assert not env.monitor.get_errors()


@pytest.mark.parametrize("fault,exception", [("corrupt", "hwe"), ("denied", "af")])
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_resend_second_beat_fault_reports_exception(env, fault, exception):
    _prepare_cross_beat_rvi_stream(env)
    env.uncache_agent.inject_response_fault_at(
        _MMIO_BASE + 8,
        corrupt=1 if fault == "corrupt" else 0,
        denied=1 if fault == "denied" else 0,
    )
    _initialize_mmio_fetch(env, reset_vector=_CROSS_BEAT_PC)

    assert _wait_for_request_addr(env, _MMIO_BASE)
    assert _wait_for_request_addr(env, _MMIO_BASE + 8)
    assert _wait_for_resp_count(env, 2)
    assert _wait_for_frontend_exception(env, exception)
    stats = env.uncache_agent.get_stats()

    assert _MMIO_BASE in stats.get("request_addrs", [])
    assert (_MMIO_BASE + 8) in stats.get("request_addrs", [])
    assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_wfi_during_refill_resp_reports_not_safe(env):
    _prepare_mmio_cnop_stream(env)
    env.uncache_agent.configure(latency=2, mmio_latency=32)
    _initialize_mmio_fetch(env)

    assert _wait_for_uncache_req(env)
    env.backend_model.set_wfi_req(1)
    saw_not_safe = False
    for _ in range(64):
        env.step(1)
        if _read_dut_signal(env, "io_backend_wfi_wfiSafe", 1) == 0:
            saw_not_safe = True
            break
    env.backend_model.set_wfi_req(0)

    assert saw_not_safe
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_backend_backpressure_blocks_new_mmio_request(env):
    _prepare_mmio_cnop_stream(env)
    _initialize_mmio_fetch(env)

    assert _wait_for_uncache_req(env)
    assert _wait_for_uncache_resp(env)
    env.backend_model.set_can_accept(0)
    req_before = int(env.uncache_agent.get_stats().get("req_count", 0))
    env.step(64)
    req_during = int(env.uncache_agent.get_stats().get("req_count", 0))
    env.backend_model.set_can_accept(1)
    req_after = _wait_for_uncache_req(env)

    assert req_during == req_before
    assert req_after > req_during
    assert not env.monitor.get_errors()


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_uncache_mmio_commit_order_waits_last_commit(env):
    _prepare_mmio_cnop_stream(env)
    env.backend_model.set_can_accept(0)
    _initialize_mmio_fetch(env)

    assert _wait_for_uncache_req(env)
    assert _wait_for_uncache_resp(env)
    req_before_commit = int(env.uncache_agent.get_stats().get("req_count", 0))
    env.step(64)
    req_without_commit = int(env.uncache_agent.get_stats().get("req_count", 0))
    env.backend_model.set_can_accept(1)
    req_after_commit = _wait_for_uncache_req(env)

    assert req_without_commit == req_before_commit
    assert req_after_commit > req_without_commit
    assert not env.monitor.get_errors()


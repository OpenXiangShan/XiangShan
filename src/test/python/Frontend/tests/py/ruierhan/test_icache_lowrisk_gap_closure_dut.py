"""Low-risk DUT scenarios for the currently uncovered ICache bins.

This module deliberately drives only existing testbench/DUT top-level inputs.
It does not force internal MSHR, queue, ready, or coverage-model state.
"""

from __future__ import annotations

import os

import pytest

from tests.py.jiabowen.test_icache_mainpipe_miss_response import (
    _initialize_cacheable_stream,
)
from tests.py.jiabowen.test_two_fetch_directed_flow_dut import (
    _load_and_reset as _load_two_fetch_loop,
    _warm_frontend_execution as _warm_two_fetch_execution,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_NOP = 0x0000_0013


def _load_nops(env, base: int, *, words: int = 512) -> None:
    env.load_program((_NOP.to_bytes(4, "little")) * int(words), int(base))


def _run_until(env, predicate, *, max_cycles: int, label: str) -> None:
    for _ in range(int(max_cycles)):
        if predicate():
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": f"timeout while waiting for {label}",
            "max_cycles": int(max_cycles),
            "current_cycle": int(env.current_cycle),
            "stats": env.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _wait_funcov_hit(
    env,
    group: str,
    bin_name: str,
    *,
    max_cycles: int,
    label: str | None = None,
) -> None:
    _run_until(
        env,
        lambda: env.functional_coverage.key_hit(group, bin_name),
        max_cycles=max_cycles,
        label=label or f"{group}.{bin_name}",
    )


def _clear_soft_prefetch(env) -> None:
    for slot in range(3):
        valid = getattr(env.dut, f"io_softPrefetch_{slot}_valid", None)
        address = getattr(env.dut, f"io_softPrefetch_{slot}_bits_vaddr", None)
        if valid is not None:
            valid.value = 0
        if address is not None:
            address.value = 0


def _pulse_fencei(env) -> None:
    signal = getattr(env.clock_reset, "io_fencei", None)
    assert signal is not None, {"missing_signal": "io_fencei"}
    signal.value = 1
    env.step(1)
    signal.value = 0
    env.step(2)


def _drive_soft_prefetch(env, addresses: list[int]) -> None:
    _clear_soft_prefetch(env)
    for slot, address in enumerate(addresses[:3]):
        valid = getattr(env.dut, f"io_softPrefetch_{slot}_valid", None)
        value = getattr(env.dut, f"io_softPrefetch_{slot}_bits_vaddr", None)
        assert valid is not None and value is not None, {
            "missing_signal": f"io_softPrefetch_{slot}"
        }
        valid.value = 1
        value.value = int(address)
    env.step(1)
    _clear_soft_prefetch(env)


@pytest.fixture
def lowrisk_cleanup(env):
    """Restore top-level test inputs even when a scenario fails."""
    yield env
    _clear_soft_prefetch(env)
    fencei = getattr(env.clock_reset, "io_fencei", None)
    if fencei is not None:
        fencei.value = 0


@pytest.mark.funcov_bins("BIN-605", "BIN-606", "BIN-617", "BIN-629")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_mainpipe_flush_refill(env) -> None:
    base = 0x8004_0000
    redirect = base + 0x100
    _initialize_cacheable_stream(env, base, latency=32)

    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["req_count"]) >= 1,
        max_cycles=6000,
        label="initial MainPipe cache request",
    )
    env.backend_model.inject_redirect(redirect, "ctrl_redirect", delay_cycles=0)
    _run_until(
        env,
        lambda: any(int(observation.pc) == redirect for observation in env.monitor.observations),
        max_cycles=1024,
        label="redirect target delivery",
    )
    assert int(env.icache_agent.get_stats()["req_count"]) >= 2
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-655", "BIN-656", "BIN-657", "BIN-663", "BIN-677",
    "BIN-661", "BIN-778", "BIN-779", "BIN-780"
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_prefetch_soft_requests(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    base = 0x8004_0000
    _load_nops(env, base, words=512)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=1.0,
        seed=0x6255,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)

    for layout in (
        (base + 0x100, base + 0x180),
        (base + 0x200, base + 0x280),
        (base + 0x300,),
        (base + 0x380,),
    ):
        _drive_soft_prefetch(env, list(layout))
        env.step(8)

    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["req_count"]) >= 1,
        max_cycles=512,
        label="soft-prefetch or Fetch cache request",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-674", "BIN-602")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_mainpipe_s0_entry_two_fetch_dut(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    _load_two_fetch_loop(env)
    _warm_two_fetch_execution(env)

    _wait_funcov_hit(
        env,
        "icache_mainpipe_s0_entry",
        "dual_request_data_read",
        max_cycles=4096,
        label="dual FTQ/WayLookup request plus DataArray read",
    )
    _wait_funcov_hit(
        env,
        "icache_mainpipe_s0_entry",
        "ftq_waylookup_skew",
        max_cycles=4096,
        label="FTQ/WayLookup skew followed by atomic join and s1 latch",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-674", "BIN-603")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_mainpipe_s0_entry_data_backpressure_dut(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    base = 0x8000_0000

    _load_nops(env, base, words=512)
    _run_until(
        env,
        lambda: int(env.backend_model.get_stats().get("commit_count", 0)) >= 4,
        max_cycles=3000,
        label="warmup commits before backend pressure",
    )
    env.backend_model.set_can_accept(0)
    env.step(16)
    env.backend_model.set_can_accept(1)
    _wait_funcov_hit(
        env,
        "icache_mainpipe_s0_entry",
        "data_array_backpressure",
        max_cycles=4096,
        label="WayLookup held while DataArray request is not ready",
    )
    _wait_funcov_hit(
        env,
        "icache_mainpipe_s0_entry",
        "ftq_waylookup_skew",
        max_cycles=4096,
        label="FTQ/WayLookup skew followed by atomic join and s1 latch",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-610")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_mainpipe_single_bank_range_dut(env) -> None:
    base = 0x8008_0000
    target = base + 0x08

    _load_nops(env, base, words=512)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=16,
        miss_rate=1.0,
        seed=0x6610,
    )
    env.initialize(reset_vector=target, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(target)

    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_line_count"]) >= 1,
        max_cycles=1024,
        label="initial target line refill",
    )
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=16,
        miss_rate=0.0,
        seed=0x6611,
    )
    env.backend_model.inject_redirect(target, "ctrl_redirect", delay_cycles=0)
    _run_until(
        env,
        lambda: env.functional_coverage.key_hit(
            "icache_mainpipe_s1_sram",
            "single_line_bank_range",
        ),
        max_cycles=1024,
        label="single-line nonzero-bank SRAM hit",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-686", "BIN-690", "BIN-691", "BIN-692", "BIN-699", "BIN-705", "BIN-1005",
    "BIN-707", "BIN-708", "BIN-709", "BIN-710", "BIN-711", "BIN-712", "BIN-1006", "BIN-1007", "BIN-1008",
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_missunit_merge_and_fencei(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    base = 0x8004_0000
    _initialize_cacheable_stream(env, base, latency=96)

    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["req_count"]) >= 1,
        max_cycles=6000,
        label="initial MissUnit request",
    )
    _drive_soft_prefetch(env, [base + 0x100, base + 0x180, base + 0x200])
    _pulse_fencei(env)
    env.backend_model.inject_redirect(base + 0x400, "ctrl_redirect", delay_cycles=0)
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_beat_count"]) >= 2,
        max_cycles=1024,
        label="post-Fence.i refill response",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-720", "BIN-731", "BIN-734", "BIN-735", "BIN-741",
    "BIN-744", "BIN-746", "BIN-748", "BIN-750", "BIN-751", "BIN-752",
    "BIN-753", "BIN-754", "BIN-756", "BIN-758",
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_waylookup_updates_and_flush(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    base = 0x8006_0000
    _load_nops(env, base, words=2048)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=24,
        miss_rate=1.0,
        seed=0x6272,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)

    for offset in range(0x100, 0x900, 0x80):
        _drive_soft_prefetch(env, [base + offset, base + offset + 0x40])
        env.step(4)
    env.backend_model.inject_redirect(base + 0x1000, "ctrl_redirect", delay_cycles=0)
    _run_until(
        env,
        lambda: any(int(observation.pc) == base + 0x1000 for observation in env.monitor.observations),
        max_cycles=1024,
        label="WayLookup flush recovery target",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-763", "BIN-765", "BIN-767", "BIN-768")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_hitmiss_refill_sequence(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    base = 0x8004_0000
    _load_nops(env, base, words=1024)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=1.0,
        seed=0x6276,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)

    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_line_count"]) >= 1,
        max_cycles=1024,
        label="clean cache refill",
    )
    _drive_soft_prefetch(env, [base, base + 0x40])
    env.step(64)
    assert int(env.icache_agent.get_stats()["resp_line_count"]) >= 1
    assert not env.monitor.get_errors()

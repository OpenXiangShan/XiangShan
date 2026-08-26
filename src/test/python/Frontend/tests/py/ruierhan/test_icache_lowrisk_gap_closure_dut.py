"""Low-risk DUT scenarios for the currently uncovered ICache bins.

This module deliberately drives only existing testbench/DUT top-level inputs.
It does not force internal MSHR, queue, ready, or coverage-model state.
"""

from __future__ import annotations

import os

import pytest

from env.sequences import (
    TranslationScenario,
    TranslationScenarioBuilder,
)
from env.funcov.py.icache.icache_waylookup_funcov import (
    _SIGNALS as _WAYLOOKUP_SIGNALS,
)
from tests.py.jiabowen.test_icache_mainpipe_miss_response import (
    _initialize_cacheable_stream,
)
from tests.py.jiabowen.test_two_fetch_directed_flow_dut import (
    _load_and_reset as _load_two_fetch_loop,
    _warm_frontend_execution as _warm_two_fetch_execution,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_NOP = 0x0000_0013
_MAIN = "Frontend_top.Frontend.inner_icache.mainPipe."
_ICACHE = "Frontend_top.Frontend.inner_icache."


def _try_read_internal(env, names: tuple[str, ...]) -> int | None:
    cache = getattr(env, "_ruierhan_internal_signal_cache", None)
    if cache is None:
        cache = {}
        setattr(env, "_ruierhan_internal_signal_cache", cache)
    cache_key = tuple(str(name) for name in names)
    if cache_key in cache:
        signal = cache[cache_key]
        value = None if signal is None else getattr(signal, "value", None)
        return None if value is None else int(value)

    for name in names:
        try:
            signal = getattr(env.dut, str(name), None)
            if signal is None:
                getter = getattr(env.dut, "GetInternalSignal", None)
                signal = getter(str(name)) if callable(getter) else None
            value = None if signal is None else getattr(signal, "value", None)
            if value is not None:
                cache[cache_key] = signal
                return int(value)
        except Exception:
            continue
    cache[cache_key] = None
    return None


def _miss_request_snapshot(env) -> dict[str, int | None]:
    return {
        "cycle": int(env.current_cycle),
        "valid": _try_read_internal(
            env, (_MAIN + "__Vtogcov__io_missReq_valid",)
        ),
        "ready": _try_read_internal(
            env, (_MAIN + "__Vtogcov__io_missReq_ready",)
        ),
        "vset": _try_read_internal(
            env,
            (
                _MAIN + "__Vtogcov__io_missReq_bits_vSetIdx",
                _ICACHE + "_mainPipe_io_missReq_bits_vSetIdx",
            ),
        ),
        "paddr": _try_read_internal(
            env,
            (
                _MAIN + "__Vtogcov__io_missReq_bits_blkPAddr",
                _ICACHE + "_mainPipe_io_missReq_bits_blkPAddr",
            ),
        ),
    }


def _waylookup_value(env, key: str) -> int | None:
    return _try_read_internal(env, tuple(_WAYLOOKUP_SIGNALS[str(key)]))


def _waylookup_snapshot(env) -> dict[str, int | None]:
    return {
        "cycle": int(env.current_cycle),
        "empty": _waylookup_value(env, "empty"),
        "num_valid": _waylookup_value(env, "num_valid"),
        "read_flag": _waylookup_value(env, "read_flag"),
        "read_value": _waylookup_value(env, "read_value"),
        "write_flag": _waylookup_value(env, "write_flag"),
        "write_value": _waylookup_value(env, "write_value"),
        "exception_valid": _waylookup_value(env, "exception_valid"),
        "write0_valid": _waylookup_value(env, "write0_valid"),
        "write0_ready": _waylookup_value(env, "write0_ready"),
        "write1_valid": _waylookup_value(env, "write1_valid"),
        "write1_ready": _waylookup_value(env, "write1_ready"),
        "to_valid": _waylookup_value(env, "to_valid"),
        "to_ready": _waylookup_value(env, "to_ready"),
        "update_valid": _waylookup_value(env, "update_valid"),
        "flush": _waylookup_value(env, "flush"),
        "bpu_flush": _waylookup_value(env, "bpu_flush"),
        "bpu_flush_match": _waylookup_value(env, "bpu_flush_match"),
    }


def _wait_waylookup_occupancy(env, minimum: int, *, max_cycles: int) -> None:
    _run_until(
        env,
        lambda: (
            _waylookup_value(env, "num_valid") is not None
            and int(_waylookup_value(env, "num_valid")) >= int(minimum)
        ),
        max_cycles=max_cycles,
        label=f"WayLookup occupancy >= {int(minimum)}",
    )


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
    env.backend_model.set_can_accept(1)


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


@pytest.mark.funcov_bins("BIN-629")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_missunit_backpressure(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    if os.getenv("TB_ICACHE_FETCH_MSHR_PRESSURE_SUPPORTED") != "1":
        pytest.xfail(
            "the current top-level environment cannot keep PrefetchPipe from "
            "allocating the cold line ahead of MainPipe; a supported prefetch "
            "isolation control is required to fill all four Fetch MSHRs"
        )
    samples: list[dict[str, int | None]] = []
    env.register_cycle_observer(
        lambda _cycle, active_env: samples.append(
            _miss_request_snapshot(active_env)
        )
    )
    base = 0x800C_0000
    redirect_stride = 0x1_0000
    _load_nops(env, base, words=(6 * redirect_stride) // 4)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=16384,
        miss_rate=1.0,
        seed=0x6629,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)

    # Let PrefetchPipe occupy its ten MSHRs first.  A redirect to a distant
    # cold address can then reach MainPipe without PrefetchPipe allocating the
    # same line ahead of it.  Fetch MSHRs survive frontend redirects, so four
    # such requests fill sources 0..3 and the fifth request is backpressured.
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["pending"]) >= 10,
        max_cycles=6000,
        label="all ten prefetch MSHRs outstanding",
    )
    for index in range(4):
        env.backend_model.inject_redirect(
            base + (index + 1) * redirect_stride,
            "ctrl_redirect",
            delay_cycles=0,
        )
        _run_until(
            env,
            lambda expected=index + 1: len(
                {
                    int(record["source"])
                    for record in env.icache_agent.get_stats()["request_records"]
                    if int(record["source"]) < 4
                }
            )
            >= expected,
            max_cycles=6000,
            label=f"demand miss {index + 1} allocated a fetch MSHR",
        )
    env.backend_model.inject_redirect(
        base + 5 * redirect_stride,
        "ctrl_redirect",
        delay_cycles=0,
    )
    _wait_funcov_hit(
        env,
        "icache_mainpipe_s1_miss",
        "missunit_backpressure_stable",
        max_cycles=6000,
        label="two-cycle stable MainPipe miss under MissUnit backpressure",
    )
    stats = env.icache_agent.get_stats()
    assert int(stats["max_pending_depth"]) >= 14, stats
    assert int(stats["pending"]) >= 14, stats
    assert any(
        previous["valid"] == 1
        and previous["ready"] == 0
        and current["valid"] == 1
        and current["ready"] == 0
        and previous["vset"] is not None
        and previous["paddr"] is not None
        and previous["vset"] == current["vset"]
        and previous["paddr"] == current["paddr"]
        for previous, current in zip(samples, samples[1:])
    ), {"tail": samples[-32:]}
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
    "BIN-720", "BIN-726", "BIN-727", "BIN-728", "BIN-731", "BIN-733",
    "BIN-734", "BIN-735", "BIN-736", "BIN-737", "BIN-738", "BIN-739",
    "BIN-1010", "BIN-740", "BIN-741", "BIN-742", "BIN-743", "BIN-744",
    "BIN-745", "BIN-746", "BIN-748", "BIN-749", "BIN-750", "BIN-751",
    "BIN-753", "BIN-754", "BIN-756", "BIN-757", "BIN-1011", "BIN-758",
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
    _wait_funcov_hit(
        env,
        "icache_waylookup_flush",
        "flush_recovery",
        max_cycles=256,
        label="WayLookup flush recovery coverage",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-720", "BIN-726", "BIN-727",
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_waylookup_queue_read_dut(lowrisk_cleanup) -> None:
    """Build normal WayLookup entries, then exercise read-side backpressure."""
    env = lowrisk_cleanup
    _load_two_fetch_loop(env)
    _warm_two_fetch_execution(env)

    _wait_waylookup_occupancy(env, 2, max_cycles=4096)
    _wait_funcov_hit(
        env,
        "icache_waylookup_read",
        "dual_entry_dequeue",
        max_cycles=4096,
        label="WayLookup dual-entry dequeue coverage",
    )
    env.backend_model.set_can_accept(0)
    env.step(12)
    blocked = _waylookup_snapshot(env)
    env.backend_model.set_can_accept(1)
    env.step(32)

    assert blocked["num_valid"] is not None and int(blocked["num_valid"]) >= 1
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-737", "BIN-738", "BIN-739", "BIN-1010", "BIN-740", "BIN-741",
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_waylookup_exception_entry_dut(lowrisk_cleanup) -> None:
    """Drive a cacheable instruction page fault through the existing PTW model."""
    env = lowrisk_cleanup
    va = 0x8020_0F00
    pa = 0x8040_0F00
    payload = (_NOP.to_bytes(4, "little")) * 512
    scenario = TranslationScenario(
        scenario_id="waylookup-cacheable-instruction-page-fault",
        va=va,
        pa=pa,
        payload=payload,
        page_count=2,
        expected_path="fault",
        mode="sv39",
        s1_pf=1,
        expected_result="page_fault",
    )
    env.initialize(reset_vector=va, bare_mode=False)
    state = TranslationScenarioBuilder(env).build(scenario)

    def arm_before_reset_release() -> None:
        env.monitor.clear()
        env.monitor.set_expected_pc(va)
        env.arm_translation_scenario(state, page_indexes=(0,))

    env.reset(before_release=arm_before_reset_release)
    _run_until(
        env,
        lambda: bool(env.translation_oracle.get_active())
        and bool(env.translation_oracle.get_active().get("fault_seen")),
        max_cycles=6000,
        label="cacheable instruction page fault",
    )
    _wait_funcov_hit(
        env,
        "icache_waylookup_exception",
        "exception_capture",
        max_cycles=256,
        label="WayLookup exception capture coverage",
    )
    env.step(16)
    assert env.monitor.exception_mark_count > 0
    env.assert_translation_scenario()
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-753", "BIN-754", "BIN-756", "BIN-757", "BIN-1011", "BIN-758",
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_waylookup_capacity_wrap_dut(lowrisk_cleanup) -> None:
    """Hold MainPipe behind a real frontend stream to approach FIFO boundaries."""
    env = lowrisk_cleanup
    _load_two_fetch_loop(env)
    _warm_two_fetch_execution(env)
    env.backend_model.set_can_accept(0)
    _run_until(
        env,
        lambda: (
            _waylookup_value(env, "num_valid") is not None
            and int(_waylookup_value(env, "num_valid")) >= 2
        ),
        max_cycles=4096,
        label="WayLookup entries accumulate under backend backpressure",
    )
    env.step(128)
    held = _waylookup_snapshot(env)
    env.backend_model.set_can_accept(1)
    env.step(256)

    assert held["num_valid"] is not None and int(held["num_valid"]) >= 1
    assert held["read_value"] is not None
    assert held["write_value"] is not None
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

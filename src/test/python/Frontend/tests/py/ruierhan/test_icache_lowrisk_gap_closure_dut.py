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
from env.sequences import TranslationPmpPmaEntry
from env.funcov.py.icache.icache_waylookup_funcov import (
    _SIGNALS as _WAYLOOKUP_SIGNALS,
)
from env.support.pmp_pma import PmpPmaConfig
from tests.py.jiabowen.test_icache_mainpipe_miss_response import (
    _initialize_cacheable_stream,
)
from tests.py.jiabowen.test_two_fetch_directed_flow_dut import (
    _load_and_reset as _load_two_fetch_loop,
    _trained_short_block_loop,
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
    snapshot = {
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
        "ftq_req1_valid": _try_read_internal(
            env,
            (_MAIN + "io_fromFtq_bits_req_1_valid",),
        ),
        "info1_valid": _try_read_internal(
            env,
            (
                _MAIN + "io_fromWayLookup_bits_wayLookupInfo_1_valid",
                _MAIN + "__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_1_valid",
            ),
        ),
        "real_two": _try_read_internal(
            env,
            (_ICACHE + "__Vtogcov__io_toFtq_fromMainPipe_realTwoFetchValid",),
        ),
        "info0_mmio": _try_read_internal(
            env,
            (
                _MAIN + "io_fromWayLookup_bits_wayLookupInfo_0_bits_entry_isMmio",
                _MAIN + "__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_0_bits_entry_isMmio",
            ),
        ),
        "info1_mmio": _try_read_internal(
            env,
            (
                _MAIN + "io_fromWayLookup_bits_wayLookupInfo_1_bits_entry_isMmio",
                _MAIN + "__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_1_bits_entry_isMmio",
            ),
        ),
        "info0_exception": _try_read_internal(
            env,
            (_MAIN + "__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_0_bits_exceptionEntry_itlbException_value",),
        ),
        "info1_exception": _try_read_internal(
            env,
            (_MAIN + "__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_1_bits_exceptionEntry_itlbException_value",),
        ),
        "write0_exception": _try_read_internal(
            env,
            (_ICACHE + "prefetcher.__Vtogcov__io_wayLookupWrite_0_bits_exceptionEntry_itlbException_value",),
        ),
    }
    return snapshot


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
    max_cycles = int(os.getenv("TB_ICACHE_LOWRISK_MAX_CYCLES", str(max_cycles)), 0)
    target = (group, bin_name)
    for _ in range(max_cycles):
        if env.functional_coverage.key_hit(*target):
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": f"timeout while waiting for {label or f'{group}.{bin_name}'}",
            "missing": [f"{group}.{bin_name}"],
            "max_cycles": max_cycles,
            "current_cycle": int(env.current_cycle),
            "waylookup": _waylookup_snapshot(env),
            "miss_request": _miss_request_snapshot(env),
            "coverage_state": getattr(
                env.functional_coverage, "_icache_hitmiss_cov_state", None
            ),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _wait_funcov_hits(
    env,
    targets: tuple[tuple[str, str], ...],
    *,
    max_cycles: int,
    label: str,
) -> None:
    max_cycles = int(os.getenv("TB_ICACHE_LOWRISK_MAX_CYCLES", str(max_cycles)), 0)
    for _ in range(max_cycles):
        missing = [
            (group, name)
            for group, name in targets
            if not env.functional_coverage.key_hit(group, name)
        ]
        if not missing:
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": f"timeout while waiting for {label}",
            "missing": [f"{group}.{name}" for group, name in missing],
            "max_cycles": max_cycles,
            "current_cycle": int(env.current_cycle),
            "waylookup": _waylookup_snapshot(env),
            "miss_request": _miss_request_snapshot(env),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _wait_for_target_response(
    env,
    target: int,
    *,
    max_cycles: int,
    label: str,
    after_cycle: int | None = None,
) -> None:
    """Wait for the specific cache line requested by a redirect."""
    line = int(target) & ~0x3F
    for _ in range(int(max_cycles)):
        if any(
            int(record.get("address", -1)) == line
            and int(record.get("beat_idx", -1)) == 1
            and (
                after_cycle is None
                or int(record.get("cycle", -1)) > int(after_cycle)
            )
            for record in env.icache_agent.get_stats().get("response_records", [])
        ):
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": f"timeout while waiting for {label}",
            "target_line": line,
            "current_cycle": int(env.current_cycle),
            "stats": env.icache_agent.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
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


def _set_predictors(env, enabled: bool) -> None:
    value = 1 if enabled else 0
    env.set_bp_ctrl_enable(
        ubtb_enable=value,
        abtb_enable=value,
        mbtb_enable=value,
        tage_enable=value,
        sc_enable=value,
        ittage_enable=value,
    )


@pytest.fixture
def lowrisk_cleanup(env):
    """Restore top-level test inputs even when a scenario fails."""
    yield env
    _clear_soft_prefetch(env)
    fencei = getattr(env.clock_reset, "io_fencei", None)
    if fencei is not None:
        fencei.value = 0
    env.backend_model.set_can_accept(1)
    _set_predictors(env, True)


@pytest.mark.funcov_bins("BIN-685")
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
    _wait_funcov_hit(
        env,
        "icache_prefetchpipe_s2_miss",
        "missunit_backpressure_recovery",
        max_cycles=256,
    )
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
    "BIN-745", "BIN-746", "BIN-748", "BIN-749", "BIN-750", "BIN-751", "BIN-752",
    "BIN-753", "BIN-754", "BIN-756", "BIN-757", "BIN-1011", "BIN-758",
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_waylookup_updates_and_flush(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    base = 0x8006_0000
    _load_nops(env, base, words=32768)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=24,
        miss_rate=1.0,
        seed=0x6272,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)

    for offset in range(0x100, 0x2100, 0x80):
        _drive_soft_prefetch(env, [base + offset, base + offset + 0x40])
        if offset % 0x200 == 0:
            env.backend_model.inject_redirect(
                base + offset,
                "ctrl_redirect",
                delay_cycles=0,
            )
        env.step(2)

    corrupt_target = base + 0x3000
    env.icache_agent.inject_response_fault_at(corrupt_target, corrupt=1)
    env.backend_model.inject_redirect(corrupt_target, "ctrl_redirect", delay_cycles=0)
    env.step(64)

    # Align a global redirect with an accepted WayLookup write.
    for attempt in range(256):
        if env.functional_coverage.key_hit(
            "icache_waylookup_flush", "flush_wins_write"
        ):
            break
        if _waylookup_value(env, "write0_valid") == 1:
            env.backend_model.inject_redirect(
                base + 0x4000 + attempt * 0x40,
                "ctrl_redirect",
                delay_cycles=0,
            )
        env.step(1)
    env.backend_model.inject_redirect(base + 0x5000, "ctrl_redirect", delay_cycles=0)
    _run_until(
        env,
        lambda: any(
            int(observation.pc) == base + 0x5000
            for observation in env.monitor.observations
        ),
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

    # Predictor enable transitions produce supported BPU s3 flush traffic.
    # Keep a non-empty queue and dual-prefetch stream active while cycling it.
    for attempt in range(4096):
        if all(
            env.functional_coverage.key_hit("icache_waylookup_flush", name)
            for name in ("bpu_flush_rewinds_tail", "bpu_flush_two_prefetch")
        ):
            break
        if attempt % 16 == 0:
            _set_predictors(env, False)
        elif attempt % 16 == 6:
            _set_predictors(env, True)
        env.step(1)
    _set_predictors(env, True)

    _wait_funcov_hits(
        env,
        (
            ("icache_waylookup_update", "update_head"),
            ("icache_waylookup_update", "update_same_way_new_tag"),
            ("icache_waylookup_update", "update_corrupt_ignored"),
            ("icache_waylookup_update", "update_write_concurrent"),
            ("icache_waylookup_update", "update_second_entry_stall"),
            ("icache_waylookup_flush", "flush_wins_write"),
            ("icache_waylookup_flush", "bpu_flush_rewinds_tail"),
            ("icache_waylookup_flush", "bpu_flush_two_prefetch"),
        ),
        max_cycles=4096,
        label="WayLookup update and flush edge coverage",
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
    _wait_funcov_hits(
        env,
        (
            ("icache_waylookup_queue", "entry_fields"),
            ("icache_waylookup_read", "dual_entry_dequeue"),
        ),
        max_cycles=4096,
        label="WayLookup entry integrity and dual-entry dequeue coverage",
    )
    env.backend_model.set_can_accept(0)
    env.step(12)
    blocked = _waylookup_snapshot(env)
    env.backend_model.set_can_accept(1)
    # Refill updates can make the second queued entry temporarily unreadable,
    # which is the RTL's intended single-service fallback condition.
    _pulse_fencei(env)
    _set_predictors(env, False)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=24,
        miss_rate=1.0,
        seed=0x727,
    )
    _wait_funcov_hit(
        env,
        "icache_waylookup_read",
        "single_entry_fallback",
        max_cycles=4096,
        label="WayLookup single-entry fallback coverage",
    )
    _set_predictors(env, True)

    assert blocked["num_valid"] is not None and int(blocked["num_valid"]) >= 1
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins(
    "BIN-737", "BIN-738", "BIN-739", "BIN-1010", "BIN-740", "BIN-741", "BIN-762",
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
    _initialize_cacheable_stream(env, pa, latency=24)
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_line_count"]) >= 1,
        max_cycles=4096,
        label="cacheline refill before ITLB exception",
    )
    state = TranslationScenarioBuilder(env).build(scenario)

    env.monitor.clear()
    env.monitor.set_expected_pc(va)
    env.arm_translation_scenario(state, page_indexes=(0,))
    # Exercise both producers of WayLookup entries.  The backend redirect
    # drives MainPipe while the soft-prefetch request gives PrefetchPipe an
    # opportunity to capture the same translated exception entry.
    env.backend_model.inject_redirect(va, "ctrl_redirect", delay_cycles=0)
    # Capture is a one-cycle write fire.  Poll coverage while the faulting
    # request is presented; fault_seen denotes the later blocked state.
    _drive_soft_prefetch(env, [va])
    for _ in range(6000):
        if env.functional_coverage.key_hit(
            "icache_waylookup_exception", "exception_capture"
        ):
            break
        env.step(1)
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
    _wait_funcov_hit(
        env,
        "icache_waylookup_exception",
        "exception_no_bypass",
        max_cycles=256,
        label="WayLookup exception write without empty-queue bypass",
    )
    _wait_funcov_hit(
        env,
        "icache_waylookup_exception",
        "exception_blocks_dual_write",
        max_cycles=512,
        label="WayLookup exception backpressure for a dual write",
    )
    env.assert_translation_scenario()

    # The blocked transaction remains dual until the exception is cleared.
    # Start a second fault episode with predictors disabled so PrefetchPipe
    # presents a port-0-only request behind the persistent exception entry.
    _pulse_fencei(env)
    _set_predictors(env, False)
    env.arm_translation_scenario(state, page_indexes=(0,))
    env.backend_model.inject_redirect(va, "ctrl_redirect", delay_cycles=0)
    _wait_funcov_hit(
        env,
        "icache_waylookup_exception",
        "exception_blocks_single_write",
        max_cycles=6000,
        label="WayLookup exception backpressure for a single write",
    )
    _set_predictors(env, True)
    _wait_funcov_hit(
        env,
        "icache_hit_path",
        "hit_itlb_exception",
        max_cycles=256,
        label="cache hit with ITLB exception coverage",
    )
    env.step(16)
    assert env.monitor.exception_mark_count > 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-763")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_hit_pmp_exception(lowrisk_cleanup) -> None:
    """Present a cacheable line with execute permission denied by the PMP model."""
    env = lowrisk_cleanup
    # NAPOT entries are encoded at page granularity by the existing PMP/PMA
    # support.  Keep the instruction offset in the page while aligning the
    # entry base to the required 4-KiB grain.
    va = 0x8021_0000
    pa = 0x8041_0000
    payload = (_NOP.to_bytes(4, "little")) * 512
    scenario = TranslationScenario(
        scenario_id="cacheable-hit-pmp-instruction-access-fault",
        va=va,
        pa=pa,
        payload=payload,
        page_count=2,
        mode="sv39",
        expected_path="fault",
        expected_result="access_fault",
        pmp_entries=(
            TranslationPmpPmaEntry(
                kind="pmp",
                index=0,
                config=PmpPmaConfig(match="napot", read=True, write=True, execute=False),
                addr=pa,
                size=0x1000,
            ),
        ),
        pma_entries=(
            TranslationPmpPmaEntry(
                kind="pma",
                index=0,
                config=PmpPmaConfig(
                    match="napot",
                    read=True,
                    write=True,
                    execute=True,
                    cacheable=True,
                    atomic=True,
                ),
                addr=pa,
                size=0x1000,
            ),
        ),
    )
    _initialize_cacheable_stream(env, pa, latency=24)
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_line_count"]) >= 1,
        max_cycles=4096,
        label="cacheline refill before PMP exception",
    )
    state = TranslationScenarioBuilder(env).build(scenario)

    env.monitor.clear()
    env.monitor.set_expected_pc(va)
    env.arm_translation_scenario(state, page_indexes=(0,))
    env.backend_model.inject_redirect(va, "ctrl_redirect", delay_cycles=0)
    _run_until(
        env,
        lambda: bool(env.translation_oracle.get_active())
        and bool(env.translation_oracle.get_active().get("fault_seen")),
        max_cycles=6000,
        label="cacheable hit with PMP execute fault",
    )
    _wait_funcov_hit(
        env,
        "icache_hit_path",
        "hit_pmp_exception",
        max_cycles=256,
        label="cache hit with PMP exception coverage",
    )
    env.step(16)
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
    pressure_base = 0x8001_0000
    _load_nops(env, pressure_base, words=32768)
    env.backend_model.set_can_accept(0)
    for attempt in range(16384):
        occupancy = _waylookup_value(env, "num_valid")
        if occupancy is not None and int(occupancy) >= 31:
            break
        if attempt % 16 == 0:
            env.backend_model.inject_redirect(
                pressure_base + ((attempt // 16) % 512) * 0x80,
                "ctrl_redirect",
                delay_cycles=0,
            )
        env.step(1)
    held = _waylookup_snapshot(env)
    assert held["num_valid"] is not None and int(held["num_valid"]) >= 31, held

    # Keep producers active for the 31/32-entry write blocking samples, then
    # release MainPipe so the same run crosses the read/write boundary.
    env.step(8)
    env.backend_model.set_can_accept(1)
    _wait_funcov_hits(
        env,
        (
            ("icache_waylookup_capacity", "full_blocks_write"),
            ("icache_waylookup_capacity", "one_slot_blocks_dual"),
            ("icache_waylookup_capacity", "read_write_boundary"),
        ),
        max_cycles=8192,
        label="WayLookup exact capacity boundaries",
    )

    for _ in range(32768):
        if all(
            env.functional_coverage.key_hit("icache_waylookup_wrap", name)
            for name in ("single_read_wrap", "single_write_wrap", "dual_wrap")
        ):
            break
        env.step(1)
    _wait_funcov_hits(
        env,
        (
            ("icache_waylookup_wrap", "single_read_wrap"),
            ("icache_waylookup_wrap", "single_write_wrap"),
            ("icache_waylookup_wrap", "dual_wrap"),
        ),
        max_cycles=1,
        label="WayLookup read/write pointer wrap coverage",
    )

    assert held["num_valid"] is not None and int(held["num_valid"]) >= 1
    assert held["read_value"] is not None
    assert held["write_value"] is not None
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-759", "BIN-760")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_hit_path_sequences(lowrisk_cleanup) -> None:
    """Fill adjacent lines, then let the normal sequential stream exercise clean hits."""
    env = lowrisk_cleanup
    base = 0x800A_0000
    env.load_program(_trained_short_block_loop(), base)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=24,
        miss_rate=1.0,
        seed=0x6275,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["resp_line_count"]) >= 4,
        max_cycles=4096,
        label="two cacheline refills for hit-path sequence",
    )
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=24,
        miss_rate=0.0,
        seed=0x6275,
    )
    _wait_funcov_hits(
        env,
        (
            ("icache_hit_path", "continuous_same_line_sram_hit"),
            ("icache_hit_path", "continuous_cross_line_sram_hit"),
        ),
        max_cycles=4096,
        label="same-line and cross-line clean SRAM hit coverage",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-761")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_dual_independent_hit(lowrisk_cleanup) -> None:
    """Train the existing two-fetch loop with both requested lines resident."""
    env = lowrisk_cleanup
    base = 0x8000_0000
    env.load_program(_trained_short_block_loop(), base)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=24,
        miss_rate=1.0,
        seed=0x6276,
    )
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)
    _warm_two_fetch_execution(env)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=24,
        miss_rate=0.0,
        seed=0x6276,
    )
    _wait_funcov_hit(
        env,
        "icache_hit_path",
        "dual_request_independent_hit",
        max_cycles=4096,
        label="dual independent clean SRAM hit coverage",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-765", "BIN-767", "BIN-768")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_lowrisk_hitmiss_refill_sequence(lowrisk_cleanup) -> None:
    env = lowrisk_cleanup
    base = 0x8004_0000
    _load_nops(env, base, words=32768)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=1.0,
        seed=0x6276,
    )
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 0
    env.initialize(reset_vector=base, bare_mode=True, reset_cycles=20)
    # Keep the first FTQ context live throughout the directed redirects.  This
    # prevents the synthetic redirect from selecting a committed FTQ context
    # while the ICache is deliberately stalled on refills.
    env.backend_model.set_can_accept(0)
    env.monitor.clear()
    env.monitor.set_expected_pc(base)

    target = base + 0x100
    # A sequential NOP stream can produce unrelated refills while the redirect
    # is in flight, so align the monitor and wait for this target line itself.
    env.monitor.set_expected_pc(target)
    env.backend_model.inject_redirect(target, "ctrl_redirect", delay_cycles=0)
    _wait_for_target_response(env, target, max_cycles=4096, label="clean demand cache refill")
    env.monitor.clear()
    env.monitor.set_expected_pc(target)
    # The first refill is intentionally forced to miss.  Subsequent soft
    # prefetches must see the refilled line as an SRAM hit.
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=0.0,
        seed=0x6276,
    )
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 1
    for _ in range(32):
        _drive_soft_prefetch(env, [target])
        if env.functional_coverage.key_hit("icache_miss_path", "fetch_refill_prefetch_hit"):
            break
    _wait_funcov_hit(
        env,
        "icache_miss_path",
        "fetch_refill_prefetch_hit",
        max_cycles=4096,
        label="prefetch SRAM hit after clean fetch refill",
    )
    _pulse_fencei(env)
    # Use one extra warmup tag because the first post-fence refill can overlap
    # the final invalidation/read response.  Five tags reliably leave all four
    # ways valid; the sixth exercises the real miss/victim request path.
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=1.0,
        seed=0x6276,
    )
    same_set_stride = 0x4000
    for index in range(5):
        fill_target = base + index * same_set_stride
        request_cycle = int(env.current_cycle)
        _drive_soft_prefetch(env, [fill_target])
        _wait_for_target_response(
            env,
            fill_target,
            max_cycles=4096,
            label=f"same-set refill {index + 1}",
            after_cycle=request_cycle,
        )
        # The TileLink response precedes MissUnit's MetaArray write.  Do not
        # launch the next same-set lookup until that write is observable.
        env.step(8)
    victim_target = base + 5 * same_set_stride
    victim_request_cycle = int(env.current_cycle)
    env.backend_model.inject_redirect(victim_target, "ctrl_redirect", delay_cycles=0)
    _wait_funcov_hit(
        env,
        "icache_miss_path",
        "plru_victim_on_miss",
        max_cycles=4096,
        label="full-set PLRU victim miss coverage",
    )
    _wait_for_target_response(
        env,
        victim_target,
        max_cycles=4096,
        label="PLRU victim demand refill",
        after_cycle=victim_request_cycle,
    )

    # Train a finite loop before invalidating the cache.  After fence.i the
    # trained predictions revisit each block without another redirect, giving
    # the sampler an uninterrupted fetch-refill -> MSHR-release -> SRAM-hit
    # sequence for the same line.
    env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = 0
    loop_base = base + 0x1_8000
    env.load_program(_trained_short_block_loop(), loop_base)
    env.backend_model.set_can_accept(1)
    env.monitor.clear()
    env.monitor.set_expected_pc(loop_base)
    env.backend_model.inject_redirect(loop_base, "ctrl_redirect", delay_cycles=0)
    _warm_two_fetch_execution(env)
    trained_commit_count = int(env.backend_model.get_stats().get("commit_count", 0))
    _run_until(
        env,
        lambda: int(env.backend_model.get_stats().get("commit_count", 0))
        >= trained_commit_count + 256,
        max_cycles=4096,
        label="stable trained short-block loop",
    )
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=1.0,
        seed=0x6276,
    )
    _pulse_fencei(env)
    _wait_funcov_hit(
        env,
        "icache_miss_path",
        "refill_then_fetch_hit",
        max_cycles=4096,
        label="demand SRAM hit after refill and MSHR release",
    )
    assert int(env.icache_agent.get_stats()["resp_line_count"]) >= 1
    assert not env.monitor.get_errors()

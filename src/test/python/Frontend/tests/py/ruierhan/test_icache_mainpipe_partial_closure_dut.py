"""Directed DUT closure for ICache MainPipe partial functional coverage.

This file intentionally reuses helper code from the existing jiabowen DUT
tests without modifying that source file.
"""

from __future__ import annotations

import os
from collections.abc import Callable

import pytest

from tests.py.jiabowen.test_functional_coverage_baremode import (
    test_baremode_mmio_uncache_redirect_pilot as _run_mmio_uncache_redirect,
)
from tests.py.jiabowen.test_icache_mainpipe_miss_response import (
    _BASE,
    _DUAL_BASE,
    _completed_dual_transactions,
    _initialize_cacheable_stream,
    _load_nops,
    _load_two_fetch_loop,
    _register_mainpipe_observer,
)
from tests.py.zhaoxinran.test_instr_uncache_port_boundaries import (
    test_uncache_cacheable_pending_redirect_to_pbmt_nc_has_enough_requests
    as _run_pbmt_nc_redirect,
)
from tests.py.zhaoxinran.test_multi_branch import (
    test_large_loop_multi_segment as _run_large_loop_multi_segment,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"


def _cycle_limit(name: str, default: int) -> int:
    raw = os.getenv(str(name), "").strip()
    if not raw:
        return int(default)
    value = int(raw, 0)
    assert value > 0, f"{name} must be positive"
    return int(value)


def _run_until(env, predicate: Callable[[], bool], *, max_cycles: int, label: str) -> None:
    for _ in range(int(max_cycles)):
        if predicate():
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": f"timeout while waiting for {label}",
            "max_cycles": int(max_cycles),
            "icache": env.icache_agent.get_stats(),
            "backend": env.backend_model.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _wait_hit(env, group: str, bin_name: str, *, max_cycles: int = 6000) -> None:
    _run_until(
        env,
        lambda: env.functional_coverage.key_hit(group, bin_name),
        max_cycles=max_cycles,
        label=f"{group}.{bin_name}",
    )


def _init_stream_with_observer(env, base: int, *, latency: int) -> list[dict]:
    samples = _register_mainpipe_observer(env)
    _initialize_cacheable_stream(env, int(base), latency=int(latency), samples=samples)
    return samples


@pytest.mark.funcov_bins("BIN-623")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_refill_mismatch_dut(env) -> None:
    _init_stream_with_observer(env, _BASE, latency=48)
    _wait_hit(
        env,
        "icache_mainpipe_s1_refill",
        "nonmatching_refill_ignored",
        max_cycles=_cycle_limit("TB_ICACHE_MAINPIPE_REFILL_MISMATCH_WAIT", 12000),
    )
    assert not env.monitor.get_errors()


@pytest.mark.parametrize("fault", [{"corrupt": 1}, {"denied": 1}])
@pytest.mark.funcov_bins("BIN-624", "BIN-636", "BIN-676")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_fault_refill_dut(env, fault) -> None:
    env.icache_agent.inject_response_fault_at(_BASE, **fault)
    _initialize_cacheable_stream(env, _BASE, latency=12)

    _wait_hit(env, "icache_mainpipe_s1_refill", "corrupt_refill_saved")
    _wait_hit(env, "icache_mainpipe_s1_protection", "tl_error_to_exception")
    _wait_hit(
        env,
        "icache_mainpipe_s1_refill",
        "error_state_cleared_on_new_request",
        max_cycles=_cycle_limit("TB_ICACHE_MAINPIPE_ERROR_CLEAR_WAIT", 12000),
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-626")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_selective_refill_dut(env) -> None:
    samples = _register_mainpipe_observer(env)
    _load_two_fetch_loop(env, _DUAL_BASE)
    env.icache_agent.configure(hit_latency=8, miss_latency=8, miss_rate=1.0, seed=0x2524)
    env.initialize(reset_vector=_DUAL_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_DUAL_BASE)

    _run_until(
        env,
        lambda: any(
            item["pattern"] == "hit_hit" for item in _completed_dual_transactions(samples)
        ),
        max_cycles=_cycle_limit("TB_ICACHE_MAINPIPE_SELECTIVE_WARM_WAIT", 3000),
        label="resident dual hit training",
    )
    fencei = getattr(env.dut, "io_fencei", None)
    assert fencei is not None, {"missing_dut_signal": "io_fencei"}
    fencei.value = 1
    env.step(1)
    fencei.value = 0

    _wait_hit(
        env,
        "icache_mainpipe_s1_refill",
        "refill_request_line_selective",
        max_cycles=_cycle_limit("TB_ICACHE_MAINPIPE_SELECTIVE_REFILL_WAIT", 6000),
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-625")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.xfail(
    reason="current ICacheAgent has no per-line variable refill-latency control for deterministic split refill",
    strict=False,
)
def test_tc_icache_mainpipe_split_refill_dut(env) -> None:
    pytest.xfail(
        "current ICacheAgent has no per-line variable refill-latency control for deterministic split refill"
    )
    _load_two_fetch_loop(env, _DUAL_BASE)
    env.icache_agent.configure(hit_latency=8, miss_latency=8, miss_rate=1.0, seed=0x2524)
    env.initialize(reset_vector=_DUAL_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_DUAL_BASE)
    fencei = getattr(env.dut, "io_fencei", None)
    assert fencei is not None, {"missing_dut_signal": "io_fencei"}
    fencei.value = 1
    env.step(1)
    fencei.value = 0
    _wait_hit(env, "icache_mainpipe_s1_refill", "cross_line_split_refill")


@pytest.mark.funcov_bins("BIN-628")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.xfail(
    reason="current directed setup does not reliably force both requests to be cross-line with all four lines missing",
    strict=False,
)
def test_tc_icache_mainpipe_four_line_arb_dut(env) -> None:
    pytest.xfail(
        "current directed setup does not reliably force both requests to be cross-line with all four lines missing"
    )
    _load_two_fetch_loop(env, _DUAL_BASE)
    env.icache_agent.configure(hit_latency=8, miss_latency=32, miss_rate=1.0, seed=0x6628)
    env.initialize(reset_vector=_DUAL_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_DUAL_BASE)
    _wait_hit(env, "icache_mainpipe_s1_miss", "four_line_fixed_priority")


@pytest.mark.funcov_bins("BIN-629")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_miss_stall_dut(env) -> None:
    _load_nops(env, _BASE, words=1024)
    env.icache_agent.configure(hit_latency=1, miss_latency=96, miss_rate=1.0, seed=0x6629)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)
    _wait_hit(
        env,
        "icache_mainpipe_s1_miss",
        "missunit_backpressure_stable",
        max_cycles=_cycle_limit("TB_ICACHE_MAINPIPE_MISS_STALL_WAIT", 12000),
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-634")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_pmp_fault_dut(env) -> None:
    _run_mmio_uncache_redirect(env)
    _wait_hit(env, "icache_mainpipe_s1_protection", "pmp_exception_suppresses_miss")
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-635")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_uncache_suppress_dut(env) -> None:
    _run_pbmt_nc_redirect(env)
    _wait_hit(env, "icache_mainpipe_s1_protection", "mmio_pbmt_suppresses_refill")
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-637")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.xfail(
    reason="needs a directed dual-fetch transaction aligned with PMP/PBMT protection in the same MainPipe s1 window",
    strict=False,
)
def test_tc_icache_mainpipe_dual_protection_dut(env) -> None:
    pytest.xfail(
        "needs a directed dual-fetch transaction aligned with PMP/PBMT protection in the same MainPipe s1 window"
    )
    _run_pbmt_nc_redirect(env)
    _wait_hit(env, "icache_mainpipe_s1_protection", "dual_request_shared_protection")


@pytest.mark.funcov_bins("BIN-645")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_s2_flush_dut(env) -> None:
    _run_large_loop_multi_segment(env)
    _wait_hit(
        env,
        "icache_mainpipe_s2_ecc",
        "global_flush_clears_s2_bpu_does_not",
        max_cycles=_cycle_limit("TB_ICACHE_MAINPIPE_S2_FLUSH_WAIT", 12000),
    )
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    ("bin_id", "bin_name"),
    [
        ("BIN-641", "meta_code_or_multiway_corrupt"),
        ("BIN-642", "data_ecc_selected_bank_only"),
        ("BIN-643", "mshr_bypass_skips_data_ecc"),
        ("BIN-644", "corrupt_sideband_per_line"),
    ],
)
@pytest.mark.funcov_bins("BIN-641", "BIN-642", "BIN-643", "BIN-644")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.xfail(
    reason="current Python ICache environment exposes TL response faults but not Meta/DataArray ECC injection hooks",
    strict=False,
)
def test_tc_icache_mainpipe_ecc_injection_required_dut(env, bin_id, bin_name) -> None:
    del bin_id
    pytest.xfail(
        "current Python ICache environment exposes TL response faults but not Meta/DataArray ECC injection hooks"
    )
    _load_nops(env, _BASE, words=1024)
    env.icache_agent.configure(hit_latency=1, miss_latency=16, miss_rate=1.0, seed=0x6641)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)
    _wait_hit(env, "icache_mainpipe_s2_ecc", bin_name)

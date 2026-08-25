"""Directed DUT closure for ICache MainPipe partial functional coverage.

This file intentionally reuses helper code from the existing jiabowen DUT
tests without modifying that source file.
"""

from __future__ import annotations

import os
from collections.abc import Callable

import pytest

from tests.py.jiabowen.test_icache_mainpipe_miss_response import (
    _BASE,
    _DUAL_BASE,
    _completed_dual_transactions,
    _initialize_cacheable_stream,
    _load_two_fetch_loop,
    _register_mainpipe_observer,
)
from tests.py.zhaoxinran.test_instr_uncache_port_boundaries import (
    test_uncache_sv39_cross_page_rvi_uses_second_page_pma_path
    as _run_cross_page_pma_path,
    test_uncache_sv39_pmp_execute_denied_reports_instruction_access_fault
    as _run_pmp_execute_denied,
    test_uncache_sv39_sector_lane_reuses_refill_on_adjacent_page
    as _run_sector_lane_refill,
    test_uncache_cacheable_pending_redirect_to_pbmt_nc_has_enough_requests
    as _run_pbmt_nc_redirect,
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


@pytest.mark.parametrize("fault", [{"corrupt": 1, "denied": 0}, {"corrupt": 1, "denied": 1}])
@pytest.mark.funcov_bins("BIN-624", "BIN-636")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_fault_refill_dut(env, fault) -> None:
    env.icache_agent.inject_response_fault_at(_BASE, **fault)
    _initialize_cacheable_stream(env, _BASE, latency=12)

    _wait_hit(env, "icache_mainpipe_s1_refill", "corrupt_refill_saved")
    _wait_hit(env, "icache_mainpipe_s1_protection", "tl_error_to_exception")
    stats = env.icache_agent.get_stats()
    assert int(stats["corrupt_resp_count"]) >= 1
    if int(fault["denied"]):
        assert int(stats["denied_resp_count"]) >= 1
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


@pytest.mark.funcov_bins("BIN-623")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_nonmatching_refill_dut(env) -> None:
    _run_sector_lane_refill(env)
    _wait_hit(
        env,
        "icache_mainpipe_s1_refill",
        "nonmatching_refill_ignored",
        max_cycles=1,
    )
    assert int(env.icache_agent.get_stats().get("resp_line_count", 0)) >= 1
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-634")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_pmp_fault_dut(env) -> None:
    _run_pmp_execute_denied(env)
    _wait_hit(env, "icache_mainpipe_s1_protection", "pmp_exception_suppresses_miss")
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-635")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_pma_mmio_suppress_dut(env) -> None:
    _run_cross_page_pma_path(env)
    _wait_hit(env, "icache_mainpipe_s1_protection", "pmp_mmio_suppresses_refill")
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) >= 1
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-638")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_uncache_suppress_dut(env) -> None:
    _run_pbmt_nc_redirect(env)
    _wait_hit(env, "icache_mainpipe_s1_protection", "pbmt_uncache_suppresses_refill")
    assert not env.monitor.get_errors()

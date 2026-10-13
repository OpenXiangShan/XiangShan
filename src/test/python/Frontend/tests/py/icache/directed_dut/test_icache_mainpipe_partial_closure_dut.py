"""Directed DUT closure for ICache MainPipe partial functional coverage.

Scenario preparation and transaction observations are owned by local support
modules and use the shared Frontend environment interfaces.
"""

from __future__ import annotations

from env.runtime.frontend_init import (
    ConfigureAgent,
    FrontendInitConfig,
    initialize_frontend,
    run_initialization_steps,
)

import os
from collections.abc import Callable

import pytest
from tests.py.icache.directed_dut.support import cycle_limit as _cycle_limit, wait_until

from tests.py.icache.directed_dut.mainpipe_support import (
    CACHEABLE_BASE as _BASE,
    DUAL_FETCH_BASE as _DUAL_BASE,
    completed_dual_transactions as _completed_dual_transactions,
    initialize_cacheable_stream as _initialize_cacheable_stream,
    load_two_fetch_loop as _load_two_fetch_loop,
    register_mainpipe_observer as _register_mainpipe_observer,
)
from tests.py.icache.directed_dut.protection_scenarios import (
    run_cross_page_pma_path as _run_cross_page_pma_path,
    run_pmp_execute_denied as _run_pmp_execute_denied,
    run_sector_lane_refill as _run_sector_lane_refill,
    run_cacheable_pending_redirect_to_pbmt_nc as _run_pbmt_nc_redirect,
)
pytestmark = pytest.mark.skipif(
    os.getenv("TB_ENABLE_DUT_TESTS") != "1",
    reason="requires compiled DUT",
)


def _run_until(env, predicate: Callable[[], bool], *, max_cycles: int, label: str) -> None:
    wait_until(
        env,
        predicate,
        max_cycles=max_cycles,
        label=label,
        diagnostics=lambda: {
            "icache": env.icache_agent.get_stats(),
            "backend": env.backend_model.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        },
    )


def _wait_hit(env, group: str, bin_name: str, *, max_cycles: int = 6000) -> None:
    _run_until(
        env,
        lambda: env.functional_coverage.key_hit(group, bin_name),
        max_cycles=max_cycles,
        label=f"{group}.{bin_name}",
    )


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
@pytest.mark.parametrize(
    "fault,refill_bin",
    [
        ({"corrupt": 1, "denied": 0, "beat": 0}, "error_first_beat_no_sram_write"),
        ({"corrupt": 1, "denied": 1, "beat": 1}, "error_second_beat_no_sram_write"),
    ],
    ids=("first-beat-corrupt", "second-beat-denied"),
)
@pytest.mark.funcov_bins("BIN-624", "BIN-636", "BIN-715", "BIN-1009")
def test_tc_icache_mainpipe_fault_refill_dut(env, fault, refill_bin) -> None:
    env.icache_agent.inject_response_fault_at(_BASE, **fault)
    _initialize_cacheable_stream(env, _BASE, latency=12)

    _wait_hit(env, "icache_mainpipe_s1_refill", "corrupt_refill_saved")
    _wait_hit(env, "icache_mainpipe_s1_protection", "tl_error_to_exception")
    _wait_hit(env, "icache_missunit_refill", refill_bin)
    stats = env.icache_agent.get_stats()
    assert int(stats["corrupt_resp_count"]) >= 1
    if int(fault["denied"]):
        assert int(stats["denied_resp_count"]) >= 1
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
@pytest.mark.funcov_bins("BIN-626")
def test_tc_icache_mainpipe_selective_refill_dut(env) -> None:
    samples = _register_mainpipe_observer(env)
    _load_two_fetch_loop(env, _DUAL_BASE)
    run_initialization_steps(
        env,
        (
            ConfigureAgent(
                "icache",
                {"hit_latency": 8, "miss_latency": 8, "miss_rate": 1.0, "seed": 0x2524},
            ),
        ),
    )
    initialize_frontend(
        env,
        FrontendInitConfig(
            env_config=env.config,
            reset_vector=_DUAL_BASE,
            bare_mode=True,
            reset_cycles=20,
        ),
    )
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


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
@pytest.mark.funcov_bins("BIN-623")
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


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
@pytest.mark.funcov_bins("BIN-634")
def test_tc_icache_mainpipe_pmp_fault_dut(env) -> None:
    _run_pmp_execute_denied(env)
    _wait_hit(env, "icache_mainpipe_s1_protection", "pmp_exception_suppresses_miss")
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
@pytest.mark.funcov_bins("BIN-635")
def test_tc_icache_mainpipe_pma_mmio_suppress_dut(env) -> None:
    _run_cross_page_pma_path(env)
    _wait_hit(env, "icache_mainpipe_s1_protection", "pmp_mmio_suppresses_refill")
    assert int(env.uncache_agent.get_stats().get("req_count", 0)) >= 1
    assert not env.monitor.get_errors()


@pytest.mark.parametrize(
    "frontend_init_config", [FrontendInitConfig(defer=True)], indirect=True
)
@pytest.mark.funcov_bins("BIN-638")
def test_tc_icache_mainpipe_uncache_suppress_dut(env) -> None:
    _run_pbmt_nc_redirect(env)
    _wait_hit(env, "icache_mainpipe_s1_protection", "pbmt_uncache_suppresses_refill")
    assert not env.monitor.get_errors()

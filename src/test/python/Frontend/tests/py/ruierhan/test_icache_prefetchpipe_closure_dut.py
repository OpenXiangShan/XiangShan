"""Top-level stimulus closure for low-risk ICache PrefetchPipe bins.

Only existing frontend controls, memory/translation models, redirects,
predictor enables and soft-prefetch ports are driven. Internal signals are
sampled only to align public stimulus with the transaction stage under test.
"""

from __future__ import annotations

import os
from collections.abc import Iterable

import pytest

from env.funcov.py.icache.icache_prefetchpipe_funcov import _read_prefetch
from env.sequences import TranslationScenario, TranslationScenarioBuilder
from tests.py.jiabowen.test_icache_mainpipe_miss_response import (
    test_icache_trained_two_fetch_hit_hit_then_fencei_miss_miss as _run_trained_refill,
)
from tests.py.zhaoxinran.test_multi_branch import (
    test_large_loop_multi_segment as _run_large_loop,
)
from tests.py.ruierhan.test_icache_mainpipe_s0_flush_closure_dut import (
    _initialize_bpu_s3_stream,
    _restore_predictors,
    _s0_sampling_window,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_NOP = 0x0000_0013
_SOFT_BASE = 0x8004_0000


def _recorder(env):
    recorder = getattr(env, "functional_coverage", None)
    assert recorder is not None, "PrefetchPipe closure requires functional coverage"
    return recorder


def _hit(env, group: str, bin_name: str) -> bool:
    return bool(_recorder(env).key_hit(group, bin_name))


def _wait_bins(
    env,
    targets: Iterable[tuple[str, str]],
    *,
    max_cycles: int = 6000,
) -> None:
    remaining = set(targets)
    for _ in range(int(max_cycles)):
        remaining = {
            target for target in remaining if not _hit(env, target[0], target[1])
        }
        if not remaining:
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": "PrefetchPipe functional coverage targets were not reached",
            "missing": sorted(f"{group}.{name}" for group, name in remaining),
            "cycle": int(env.current_cycle),
            "icache": env.icache_agent.get_stats(),
            "backend": env.backend_model.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _signal(env, key: str) -> int | None:
    return _read_prefetch(_recorder(env), key)


def _load_nops(env, base: int, *, words: int = 8192) -> None:
    env.load_program((_NOP.to_bytes(4, "little")) * int(words), int(base))


def _prepare_nops(env, base: int, *, latency: int, seed: int) -> None:
    _load_nops(env, base)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=int(latency),
        miss_rate=1.0,
        seed=int(seed),
    )
    env.initialize(reset_vector=int(base), bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(int(base))


def _clear_soft_prefetch(env) -> None:
    for slot in range(3):
        valid = getattr(env.dut, f"io_softPrefetch_{slot}_valid", None)
        address = getattr(env.dut, f"io_softPrefetch_{slot}_bits_vaddr", None)
        if valid is not None:
            valid.value = 0
        if address is not None:
            address.value = 0


def _present_soft_prefetch(env, addresses: Iterable[int]) -> None:
    _clear_soft_prefetch(env)
    for slot, address in enumerate(tuple(addresses)[:3]):
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
def prefetchpipe_env(env):
    try:
        yield env
    finally:
        _clear_soft_prefetch(env)
        env.backend_model.set_can_accept(1)
        _restore_predictors(env)


@pytest.mark.funcov_bins("BIN-656", "BIN-657", "BIN-663", "BIN-677")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_soft_arbitration(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _prepare_nops(env, _SOFT_BASE, latency=48, seed=0x6657)
    targets = {
        ("icache_prefetchpipe_s0_entry", "soft_priority_over_ftq"),
        ("icache_prefetchpipe_s0_entry", "multi_soft_single_accept"),
        ("icache_prefetchpipe_s0_entry", "soft_ftq_same_cycle_capture"),
        ("icache_prefetchpipe_s1_meta", "soft_probe_no_waylookup_ftq"),
    }

    for attempt in range(128):
        if all(_hit(env, *target) for target in targets):
            break
        for _ in range(32):
            if _signal(env, "soft_pending") == 0 and _signal(
                env, "ftq_prefetch_valid"
            ) == 1:
                break
            env.step(1)
        offset = 0x100 + (attempt % 32) * 0x80
        _present_soft_prefetch(
            env,
            (_SOFT_BASE + offset, _SOFT_BASE + offset + 0x40),
        )
        env.step(2)

    _wait_bins(env, targets, max_cycles=2048)
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-654")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_bpu_flush(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _initialize_bpu_s3_stream(env)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=96,
        miss_rate=1.0,
        seed=0x6654,
    )
    for _ in range(64):
        if _s0_sampling_window(env):
            break
        env.step(1)
    assert _s0_sampling_window(env), "MainPipe s0 did not reach a BPU trigger window"
    env.set_bp_ctrl_enable(
        ubtb_enable=0,
        abtb_enable=0,
        mbtb_enable=0,
        tage_enable=0,
        sc_enable=0,
        ittage_enable=0,
    )
    fencei = getattr(env.dut, "io_fencei", None)
    assert fencei is not None, {"missing_signal": "io_fencei"}
    fencei.value = 1
    env.step(1)
    fencei.value = 0
    _wait_bins(
        env,
        [("icache_prefetchpipe_s0_entry", "bpu_flush_match_blocks_hw")],
        max_cycles=32,
    )
    assert not env.monitor.get_errors()


def _translation_state(
    env,
    *,
    scenario_id: str,
    va: int,
    pa: int,
    latency: int,
    page_fault: bool = False,
):
    scenario = TranslationScenario(
        scenario_id=scenario_id,
        va=int(va),
        pa=int(pa),
        payload=(_NOP.to_bytes(4, "little")) * 1024,
        page_count=2,
        mode="sv39",
        ptw_response_latency=int(latency),
        s1_pf=1 if page_fault else 0,
        expected_path="fault" if page_fault else "cacheable",
        expected_result="page_fault" if page_fault else "miss_refill",
    )
    return TranslationScenarioBuilder(env).build(scenario)


@pytest.mark.funcov_bins("BIN-658", "BIN-678")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_itlb_control(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    pa0 = 0x8040_0F00
    va0 = 0x4020_0F00
    _prepare_nops(env, pa0, latency=32, seed=0x6678)
    first = _translation_state(
        env,
        scenario_id="prefetchpipe-itlb-resend",
        va=va0,
        pa=pa0,
        latency=8,
    )
    env.monitor.clear()
    env.monitor.set_expected_pc(va0)
    env.arm_translation_scenario(first, page_indexes=(0, 1))
    env.backend_model.inject_redirect(va0, "ctrl_redirect", delay_cycles=0)
    _wait_bins(
        env,
        [("icache_prefetchpipe_s1_meta", "itlb_miss_resend_meta_retry")],
        max_cycles=6000,
    )

    pa1 = 0x8042_0F00
    va1 = 0x4022_0F00
    second = _translation_state(
        env,
        scenario_id="prefetchpipe-itlb-wait-flush",
        va=va1,
        pa=pa1,
        latency=64,
    )
    env.monitor.clear()
    env.monitor.set_expected_pc(va1)
    env.arm_translation_scenario(second, page_indexes=(0, 1))
    env.backend_model.inject_redirect(va1, "ctrl_redirect", delay_cycles=0)
    for attempt in range(32):
        for _ in range(512):
            if _signal(env, "s1_valid") == 1 and _signal(
                env, "s1_wait_itlb"
            ) == 1:
                break
            env.step(1)
        env.backend_model.inject_redirect(
            va1 + ((attempt + 1) % 8) * 0x40,
            "ctrl_redirect",
            delay_cycles=0,
        )
        env.step(2)
        if _hit(env, "icache_prefetchpipe_s1_meta", "flush_cancels_itlb_wait"):
            break
    _wait_bins(
        env,
        [("icache_prefetchpipe_s1_meta", "flush_cancels_itlb_wait")],
        max_cycles=512,
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-659", "BIN-661", "BIN-666")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_refill_layout(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _run_trained_refill(env)
    targets = {
        ("icache_prefetchpipe_s1_meta", "clean_refill_updates_meta"),
        ("icache_prefetchpipe_s1_meta", "dual_layout_same_line"),
        ("icache_prefetchpipe_s2_miss", "sram_or_clean_mshr_hit"),
    }
    _wait_bins(env, targets, max_cycles=4000)
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-778", "BIN-780")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_large_loop_layout(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _run_large_loop(env)
    _wait_bins(
        env,
        [
            ("icache_prefetchpipe_s1_meta", "dual_layout_overlap1"),
            ("icache_prefetchpipe_s1_meta", "dual_layout_interleave"),
        ],
        max_cycles=6000,
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-681", "BIN-683", "BIN-685")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_s2_pressure(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _run_trained_refill(env)
    pressure_targets = (
        ("icache_prefetchpipe_s1_completion", "s2_busy_enters_s2_recovery"),
        ("icache_prefetchpipe_s2_miss", "clean_mshr_cancels_backpressured_miss"),
        ("icache_prefetchpipe_s2_miss", "missunit_backpressure_recovery"),
    )
    _wait_bins(env, pressure_targets, max_cycles=4000)
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-682", "BIN-672")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_flush_boundaries(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    _run_trained_refill(env)
    targets = {
        ("icache_prefetchpipe_s1_completion", "flush_blocks_s1_completion"),
        ("icache_prefetchpipe_s2_miss", "redirect_flush_ready_boundary"),
    }
    _wait_bins(env, targets, max_cycles=1000)
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-668")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_prefetchpipe_protection(prefetchpipe_env) -> None:
    env = prefetchpipe_env
    pa = 0x8044_0F00
    va = 0x4024_0F00
    _prepare_nops(env, pa, latency=32, seed=0x6668)
    state = _translation_state(
        env,
        scenario_id="prefetchpipe-cacheable-page-fault",
        va=va,
        pa=pa,
        latency=8,
        page_fault=True,
    )
    env.monitor.clear()
    env.monitor.set_expected_pc(va)
    env.arm_translation_scenario(state, page_indexes=(0, 1))
    env.backend_model.inject_redirect(va, "ctrl_redirect", delay_cycles=0)
    _wait_bins(
        env,
        [("icache_prefetchpipe_s2_miss", "exception_or_mmio_suppresses")],
        max_cycles=6000,
    )
    assert not env.monitor.get_errors()

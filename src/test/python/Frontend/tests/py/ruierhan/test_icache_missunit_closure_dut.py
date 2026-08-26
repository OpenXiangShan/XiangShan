"""Directed DUT closure scenarios for the cacheable ICache MissUnit bins.

The tests in this module use only the existing cacheable instruction stream,
soft-prefetch ports, backend redirect injection and fence.i input.  Internal
signals are sampled for transaction evidence; they are never driven or forced.
"""

from __future__ import annotations

import os
from collections.abc import Callable

import pytest


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x8010_0000
_NOP = 0x0000_0013
_MAIN = "Frontend_top.Frontend.inner_icache.mainPipe."
_MISS = "Frontend_top.Frontend.inner_icache.missUnit."
_ICACHE = "Frontend_top.Frontend.inner_icache."
_TOP = "Frontend_top."


def _read(env, name: str, *aliases: str) -> int | None:
    cache = getattr(env, "_missunit_signal_cache", None)
    if cache is None:
        cache = {}
        setattr(env, "_missunit_signal_cache", cache)
    names = (name, *aliases)
    key = tuple(names)
    if key in cache:
        signal = cache[key]
        return None if signal is None else int(getattr(signal, "value", 0))
    for candidate in names:
        try:
            signal = getattr(env.dut, candidate, None)
            if signal is None:
                getter = getattr(env.dut, "GetInternalSignal", None)
                signal = getter(candidate) if callable(getter) else None
            if signal is not None and getattr(signal, "value", None) is not None:
                cache[key] = signal
                return int(signal.value)
        except Exception:
            continue
    cache[key] = None
    return None


def _snapshot(env) -> dict[str, int | None]:
    sample: dict[str, int | None] = {
        "cycle": int(env.current_cycle),
        "miss_valid": _read(env, _MAIN + "__Vtogcov__io_missReq_valid"),
        "miss_ready": _read(env, _MAIN + "__Vtogcov__io_missReq_ready"),
        "miss_paddr": _read(env, _MAIN + "__Vtogcov__io_missReq_bits_blkPAddr"),
        "miss_vset": _read(env, _MAIN + "__Vtogcov__io_missReq_bits_vSetIdx"),
        "flush": _read(env, _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush"),
        "fencei": _read(env, _TOP + "io_fencei"),
        "a_valid": _read(env, _TOP + "auto_inner_icache_client_out_a_valid"),
        "a_ready": _read(env, _TOP + "auto_inner_icache_client_out_a_ready"),
        "a_source": _read(env, _TOP + "auto_inner_icache_client_out_a_bits_source"),
        "d_valid": _read(env, _TOP + "auto_inner_icache_client_out_d_valid"),
        "d_source": _read(env, _TOP + "auto_inner_icache_client_out_d_bits_source"),
        "last_fire_next": _read(env, _MISS + "lastFireNext"),
        "id_next": _read(env, _MISS + "idNext"),
    }
    for index in range(14):
        prefix = f"{_MISS}allMshr_{index}."
        for field in ("valid", "issue", "flush", "fencei", "blkPAddr", "vSetIdx"):
            sample[f"mshr_{index}_{field}"] = _read(env, prefix + field)
    return sample


def _load_nops(env, base: int, *, words: int = 4096) -> None:
    env.load_program((_NOP.to_bytes(4, "little")) * int(words), int(base))


def _clear_soft_prefetch(env) -> None:
    for slot in range(3):
        valid = getattr(env.dut, f"io_softPrefetch_{slot}_valid", None)
        address = getattr(env.dut, f"io_softPrefetch_{slot}_bits_vaddr", None)
        if valid is not None:
            valid.value = 0
        if address is not None:
            address.value = 0


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


def _pulse_fencei(env) -> None:
    signal = getattr(env.clock_reset, "io_fencei", None)
    assert signal is not None, {"missing_signal": "io_fencei"}
    signal.value = 1
    env.step(1)
    signal.value = 0
    env.step(1)


def _pulse_fencei_redirect(env, target: int) -> None:
    signal = getattr(env.clock_reset, "io_fencei", None)
    assert signal is not None, {"missing_signal": "io_fencei"}
    signal.value = 1
    env.backend_model.inject_redirect(int(target), "ctrl_redirect", delay_cycles=0)
    env.step(1)
    signal.value = 0
    env.step(1)


def _run_until(env, predicate: Callable[[], bool], *, max_cycles: int, label: str) -> None:
    for _ in range(int(max_cycles)):
        if predicate():
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": f"timeout while waiting for {label}",
            "cycle": int(env.current_cycle),
            "stats": env.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _wait_bins(env, targets: list[tuple[str, str]], *, max_cycles: int = 6000) -> None:
    remaining = set(targets)
    for _ in range(int(max_cycles)):
        remaining = {
            target for target in remaining if not env.functional_coverage.key_hit(*target)
        }
        if not remaining:
            return
        env.step(1)
    remaining = {
        target for target in remaining if not env.functional_coverage.key_hit(*target)
    }
    if not remaining:
        return
    raise AssertionError(
        {
            "reason": "functional coverage targets were not reached",
            "missing": sorted(f"{group}.{name}" for group, name in remaining),
            "cycle": int(env.current_cycle),
            "stats": env.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _prepare(env, *, latency: int = 4096) -> list[dict[str, int | None]]:
    samples: list[dict[str, int | None]] = []
    _load_nops(env, _BASE)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=int(latency),
        miss_rate=1.0,
        seed=0x686,
    )
    env.register_cycle_observer(
        lambda _cycle, active_env: samples.append(_snapshot(active_env))
    )
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)
    return samples


def _assert_clean(env) -> None:
    _clear_soft_prefetch(env)
    fencei = getattr(env.clock_reset, "io_fencei", None)
    if fencei is not None:
        fencei.value = 0
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-686", "BIN-690", "BIN-691", "BIN-692")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_request_and_concurrent_dut(env) -> None:
    samples = _prepare(env)
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["req_count"]) >= 1,
        max_cycles=6000,
        label="initial fetch miss",
    )
    # Distinct cold targets create demand/prefetch overlap without forcing
    # internal request signals.
    for offset in (0x100, 0x180, 0x200):
        _drive_soft_prefetch(env, [_BASE + offset, _BASE + offset + 0x40])
    for offset in (0x280, 0x300, 0x380):
        env.backend_model.inject_redirect(_BASE + offset, "ctrl_redirect", delay_cycles=0)
        env.step(2)
    _wait_bins(
        env,
        [
            ("icache_missunit_request", "fetch_mshr_allocate"),
            ("icache_missunit_request", "same_key_fetch_prefetch_merge"),
            ("icache_missunit_request", "distinct_key_parallel_allocate"),
            ("icache_missunit_request", "same_paddr_diff_vset_separate"),
        ],
    )
    assert samples
    _assert_clean(env)


@pytest.mark.funcov_bins("BIN-689")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_prefetch_capacity_dut(env) -> None:
    samples = _prepare(env, latency=16384)
    for base_offset in range(0x100, 0x380, 0xC0):
        _drive_soft_prefetch(
            env,
            [_BASE + base_offset, _BASE + base_offset + 0x40, _BASE + base_offset + 0x80],
        )
    _run_until(
        env,
        lambda: int(env.icache_agent.get_stats()["pending"]) >= 10,
        max_cycles=6000,
        label="prefetch MSHRs full",
    )
    _drive_soft_prefetch(env, [_BASE + 0x800, _BASE + 0x840, _BASE + 0x880])
    _wait_bins(env, [("icache_missunit_capacity", "prefetch_full_backpressure")])
    assert any(
        item["miss_valid"] == 1 and item["miss_ready"] == 0 for item in samples
    ) or env.functional_coverage.key_hit("icache_missunit_capacity", "prefetch_full_backpressure")
    _assert_clean(env)


@pytest.mark.funcov_bins("BIN-693", "BIN-694")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_acquire_priority_dut(env) -> None:
    _prepare(env, latency=16384)
    for offset in (0x100, 0x180, 0x200):
        _drive_soft_prefetch(env, [_BASE + offset])
    for offset in (0x400, 0x500, 0x600):
        env.backend_model.inject_redirect(_BASE + offset, "ctrl_redirect", delay_cycles=0)
        env.step(2)
    _wait_bins(
        env,
        [
            ("icache_missunit_acquire", "fetch_priority_over_prefetch"),
            ("icache_missunit_acquire", "fetch_index_priority"),
        ]
    )
    _assert_clean(env)


@pytest.mark.funcov_bins("BIN-699", "BIN-700")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_dedup_dut(env) -> None:
    _prepare(env, latency=16384)
    target = _BASE + 0x100
    _drive_soft_prefetch(env, [target])
    _drive_soft_prefetch(env, [target])
    env.backend_model.inject_redirect(target, "ctrl_redirect", delay_cycles=0)
    env.step(2)
    env.backend_model.inject_redirect(target, "ctrl_redirect", delay_cycles=0)
    _drive_soft_prefetch(env, [target])
    _wait_bins(
        env,
        [
            ("icache_missunit_dedup", "fetch_merge_any_mshr"),
            ("icache_missunit_dedup", "prefetch_merge_any_mshr"),
        ]
    )
    _assert_clean(env)


@pytest.mark.funcov_bins(
    "BIN-702", "BIN-703", "BIN-704", "BIN-705", "BIN-1005", "BIN-706"
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_redirect_flush_dut(env) -> None:
    samples = _prepare(env, latency=16384)
    # A new nonduplicate prefetch is presented during redirect.
    _drive_soft_prefetch(env, [_BASE + 0x100])
    env.backend_model.inject_redirect(_BASE + 0x700, "ctrl_redirect", delay_cycles=0)
    _drive_soft_prefetch(env, [_BASE + 0x180])
    env.step(2)
    # Keep both request classes outstanding while redirecting them at different
    # issue stages.  The observer provides the source/issue evidence used by
    # the checkpoint checks in the coverage sampler.
    _drive_soft_prefetch(env, [_BASE + 0x200, _BASE + 0x240, _BASE + 0x280])
    env.backend_model.inject_redirect(_BASE + 0x780, "ctrl_redirect", delay_cycles=0)
    env.step(2)
    env.backend_model.inject_redirect(_BASE + 0x800, "ctrl_redirect", delay_cycles=0)
    _wait_bins(
        env,
        [
            ("icache_missunit_flush", "redirect_blocks_new_prefetch"),
            ("icache_missunit_flush", "redirect_cancels_unissued_prefetch"),
            ("icache_missunit_flush", "redirect_marks_issued_prefetch"),
            ("icache_missunit_flush", "redirect_keeps_unissued_fetch_mshr"),
            ("icache_missunit_flush", "redirect_keeps_issued_fetch_mshr"),
            ("icache_missunit_flush", "redirect_suppresses_sram_write"),
        ],
        max_cycles=12000,
    )
    assert any(item["flush"] == 1 for item in samples)
    _assert_clean(env)


@pytest.mark.funcov_bins(
    "BIN-707", "BIN-708", "BIN-709", "BIN-710", "BIN-711", "BIN-712",
    "BIN-1006", "BIN-1007", "BIN-1008",
)
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_icache_missunit_fencei_dut(env) -> None:
    samples = _prepare(env, latency=16384)
    _drive_soft_prefetch(env, [_BASE + 0x100, _BASE + 0x180, _BASE + 0x200])
    _pulse_fencei(env)
    env.backend_model.inject_redirect(_BASE + 0x900, "ctrl_redirect", delay_cycles=0)
    _drive_soft_prefetch(env, [_BASE + 0x280, _BASE + 0x2C0, _BASE + 0x300])
    _pulse_fencei_redirect(env, _BASE + 0x980)
    _drive_soft_prefetch(env, [_BASE + 0x340, _BASE + 0x380, _BASE + 0x3C0])
    _pulse_fencei_redirect(env, _BASE + 0xA00)
    _wait_bins(
        env,
        [
            ("icache_missunit_fencei", "fencei_blocks_new_nonduplicate"),
            ("icache_missunit_fencei", "fencei_cancels_unissued_mshr"),
            ("icache_missunit_fencei", "fencei_marks_issued_mshr"),
            ("icache_missunit_fencei", "fencei_suppresses_sram_write"),
            ("icache_missunit_fencei", "fencei_clears_prefetch_fifo"),
            ("icache_missunit_fencei", "fencei_redirect_fetch_unissued"),
            ("icache_missunit_fencei", "fencei_redirect_fetch_issued"),
            ("icache_missunit_fencei", "fencei_redirect_prefetch_unissued"),
            ("icache_missunit_fencei", "fencei_redirect_prefetch_issued"),
        ],
        max_cycles=12000,
    )
    assert any(item["fencei"] == 1 for item in samples)
    _assert_clean(env)

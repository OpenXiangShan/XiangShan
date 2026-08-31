"""Directed DUT closure for ICache MainPipe s0 flush functional coverage."""

from __future__ import annotations

import os
from collections.abc import Sequence

import pytest

from env.funcov.py.icache.flush_from_bpu import (
    BpuS3Flush,
    ftq_ptr_matches_or_before,
    ftq_ptr_is_strictly_after_current,
)
from tests.py.jiabowen.test_icache_mainpipe_miss_response import (
    _initialize_cacheable_stream,
)
from tests.py.jiabowen.test_two_fetch_directed_flow_dut import (
    _load_and_reset as _load_two_fetch_loop,
    _warm_frontend_execution as _warm_two_fetch_execution,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x8004_0000
_MAIN = "Frontend_top.Frontend.inner_icache.mainPipe."
_ICACHE = "Frontend_top.Frontend.inner_icache."
_IFU = "Frontend_top.Frontend.inner_ifu."

_SIGNALS = {
    "from_valid": (
        _MAIN + "io_fromWayLookup_valid",
        _MAIN + "__Vtogcov__io_fromWayLookup_valid",
    ),
    "data_ready": (
        _ICACHE + "dataArray.io_read_req_ready",
        _ICACHE + "dataArray.__Vtogcov__io_read_req_ready",
    ),
    "io_flush": (
        _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush",
    ),
    "s0_flush": (
        _MAIN + "s0_flush",
        _IFU + "s0_flush",
        _MAIN + "__Vtogcov__s0_flush",
    ),
    "bpu_valid": (
        _MAIN + "io_flushFromBpu_s3_valid",
    ),
    "bpu_flag": (
        _ICACHE + "__Vtogcov__io_fromFtq_flushFromBpu_s3_bits_flag",
        _MAIN + "io_flushFromBpu_s3_bits_flag",
    ),
    "bpu_value": (
        _ICACHE + "__Vtogcov__io_fromFtq_flushFromBpu_s3_bits_value",
        _MAIN + "io_flushFromBpu_s3_bits_value",
    ),
    "s0_ftq_flag": (
        _MAIN + "io_fromFtq_bits_req_0_ftqIdx_flag",
        _ICACHE + "__Vtogcov__io_fromFtq_toMainPipe_bits_req_0_ftqIdx_flag",
    ),
    "s0_ftq_value": (
        _MAIN + "io_fromFtq_bits_req_0_ftqIdx_value",
        _ICACHE + "__Vtogcov__io_fromFtq_toMainPipe_bits_req_0_ftqIdx_value",
    ),
    "s1_ready": (
        _MAIN + "s1_ready",
        _MAIN + "__Vtogcov__s1_ready",
    ),
    "s1_valid": (
        _MAIN + "s1_valid",
        _MAIN + "__Vtogcov__s1_valid",
    ),
    "s0_fire": (
        _MAIN + "s0_fire",
        _MAIN + "__Vtogcov__s0_fire",
    ),
    "ftq_valid": (
        _MAIN + "io_fromFtq_valid",
        _ICACHE + "__Vtogcov__io_fromFtq_toMainPipe_valid",
    ),
    "ftq_ready": (
        _MAIN + "io_fromFtq_ready",
        _ICACHE + "__Vtogcov__io_fromFtq_toMainPipe_ready",
    ),
}


def _cycle_limit(name: str, default: int) -> int:
    raw = os.getenv(str(name), "").strip()
    if not raw:
        return int(default)
    value = int(raw, 0)
    assert value > 0, f"{name} must be positive"
    return int(value)


def _try_read(env, names: Sequence[str]) -> int | None:
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


def _read(env, key: str, default: int = 0) -> int:
    value = _try_read(env, _SIGNALS[key])
    return int(default) if value is None else int(value)


def _can_read(env, key: str) -> bool:
    return _try_read(env, _SIGNALS[key]) is not None


def _require_bpu_s3_ftq_observable(env) -> None:
    missing = [
        key
        for key in ("bpu_flag", "bpu_value", "s0_ftq_flag", "s0_ftq_value")
        if not _can_read(env, key)
    ]
    if missing:
        pytest.xfail(
            "BPU s3 flag/value or MainPipe s0 ftqIdx are not exported: "
            + ",".join(missing)
        )


def _snapshot(env) -> dict:
    return {
        "cycle": int(env.current_cycle),
        **{key: _read(env, key) for key in _SIGNALS},
    }


def _s0_sampling_window(env) -> bool:
    return (
        _read(env, "from_valid") == 1
        and _read(env, "data_ready") == 1
        and _read(env, "s1_ready") == 1
    )


def _bpu_is_after_s0(sample: dict) -> bool:
    return ftq_ptr_is_strictly_after_current(
        BpuS3Flush(
            valid=sample["bpu_valid"],
            flag=sample["bpu_flag"],
            value=sample["bpu_value"],
        ),
        (sample["s0_ftq_flag"], sample["s0_ftq_value"]),
    ) is True


def _bpu_matches_s0(sample: dict) -> bool:
    return ftq_ptr_matches_or_before(
        BpuS3Flush(
            valid=sample["bpu_valid"],
            flag=sample["bpu_flag"],
            value=sample["bpu_value"],
        ),
        (sample["s0_ftq_flag"], sample["s0_ftq_value"]),
    ) is True


def _wait_for_s0_sampling_window(env, *, max_cycles: int) -> dict:
    last = None
    for _ in range(int(max_cycles)):
        last = _snapshot(env)
        if _s0_sampling_window(env):
            return last
        env.step(1)
    raise AssertionError(
        {
            "reason": "MainPipe s0 coverage sampling window did not appear",
            "max_cycles": int(max_cycles),
            "last": last,
            "icache": env.icache_agent.get_stats(),
            "backend": env.backend_model.get_stats(),
        }
    )


def _wait_hit(env, bin_name: str, *, max_cycles: int) -> None:
    group = "icache_mainpipe_s0_flush"
    last = None
    for _ in range(int(max_cycles)):
        if env.functional_coverage.key_hit(group, bin_name):
            return
        last = _snapshot(env)
        env.step(1)
    assert env.functional_coverage.key_hit(group, bin_name), {
        "reason": "coverage bin did not hit",
        "group": group,
        "bin": bin_name,
        "max_cycles": int(max_cycles),
        "last": last,
        "icache": env.icache_agent.get_stats(),
        "backend": env.backend_model.get_stats(),
        "monitor_errors": env.monitor.get_errors(),
    }


@pytest.mark.funcov_bins("BIN-605")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_s0_global_flush(env) -> None:
    samples: list[dict] = []
    env.register_cycle_observer(
        lambda _cycle, active_env: samples.append(_snapshot(active_env))
    )
    _initialize_cacheable_stream(env, _BASE, latency=8)

    attempts = _cycle_limit("TB_ICACHE_S0_GLOBAL_FLUSH_ATTEMPTS", 24)
    for attempt in range(attempts):
        window = _wait_for_s0_sampling_window(
            env,
            max_cycles=_cycle_limit("TB_ICACHE_S0_WINDOW_WAIT", 6000),
        )
        target = _BASE + 0x100 + ((attempt & 0x1F) * 0x40)
        env.backend_model.inject_redirect(target, "ctrl_redirect", delay_cycles=0)
        try:
            _wait_hit(env, "global_flush_cancels_entry", max_cycles=32)
            assert any(
                previous["ftq_valid"] == 1
                and previous["from_valid"] == 1
                and previous["data_ready"] == 1
                and previous["s1_ready"] == 1
                and current["io_flush"] == 1
                and current["s0_flush"] == 1
                and current["s0_fire"] == 0
                for previous, current in zip(samples[-65:-1], samples[-64:])
            ), {"tail": samples[-64:]}
            assert not env.monitor.get_errors()
            return
        except AssertionError:
            if attempt + 1 >= attempts:
                raise AssertionError(
                    {
                        "reason": "backend redirect never aligned with MainPipe s0 global flush window",
                        "attempts": int(attempts),
                        "last_window": window,
                        "last": _snapshot(env),
                        "icache": env.icache_agent.get_stats(),
                        "backend": env.backend_model.get_stats(),
                    }
                )


def _initialize_bpu_s3_stream(env) -> None:
    _load_two_fetch_loop(env)
    _warm_two_fetch_execution(env)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=0.0,
        seed=0x6605,
    )
    env.set_bp_ctrl_enable(
        ubtb_enable=1,
        abtb_enable=1,
        mbtb_enable=1,
        tage_enable=1,
        sc_enable=1,
        ittage_enable=1,
    )


def _restore_predictors(env) -> None:
    env.set_bp_ctrl_enable(
        ubtb_enable=1,
        abtb_enable=1,
        mbtb_enable=1,
        tage_enable=1,
        sc_enable=1,
        ittage_enable=1,
    )


def _trigger_bpu_s3_flush(env) -> None:
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


def _drive_bpu_s3_until_hit(env, bin_name: str, *, max_cycles: int) -> None:
    disable_cycles: list[int] = []
    bpu_samples: list[dict] = []
    s0_windows = 0
    elapsed = 0

    try:
        while elapsed < int(max_cycles):
            if env.functional_coverage.key_hit("icache_mainpipe_s0_flush", bin_name):
                return

            sample = _snapshot(env)
            if _s0_sampling_window(env):
                s0_windows += 1
                disable_cycles.append(int(env.current_cycle))
                _trigger_bpu_s3_flush(env)
                elapsed += 1
                for _ in range(min(40, int(max_cycles) - elapsed)):
                    sample = _snapshot(env)
                    if sample["bpu_valid"] == 1:
                        bpu_samples.append(sample)
                        bpu_samples[:] = bpu_samples[-16:]
                    if env.functional_coverage.key_hit(
                        "icache_mainpipe_s0_flush", bin_name
                    ):
                        return
                    env.step(1)
                    elapsed += 1
                _restore_predictors(env)
            else:
                env.step(1)
                elapsed += 1
    finally:
        _restore_predictors(env)

    assert env.functional_coverage.key_hit("icache_mainpipe_s0_flush", bin_name), {
        "reason": "BPU s3 flush did not align with requested MainPipe s0 flush bin",
        "bin": bin_name,
        "max_cycles": int(max_cycles),
        "s0_windows": int(s0_windows),
        "predictor_disable_cycles": disable_cycles,
        "last_bpu_samples": bpu_samples,
        "last": _snapshot(env),
        "icache": env.icache_agent.get_stats(),
        "backend": env.backend_model.get_stats(),
        "monitor_errors": env.monitor.get_errors(),
    }


@pytest.mark.funcov_bins("BIN-606")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_s0_bpu_match(env) -> None:
    pytest.xfail(
        "the top-level test API cannot select the BPU stage3 FTQ pointer needed "
        "to match the concurrent MainPipe s0 entry"
    )
    _require_bpu_s3_ftq_observable(env)
    samples: list[dict] = []
    env.register_cycle_observer(
        lambda _cycle, active_env: samples.append(_snapshot(active_env))
    )
    _initialize_bpu_s3_stream(env)
    _drive_bpu_s3_until_hit(
        env,
        "bpu_match_cancels_entry",
        max_cycles=_cycle_limit("TB_ICACHE_S0_BPU_MATCH_MAX_CYCLES", 2048),
    )
    assert any(
        sample["ftq_valid"] == 1
        and sample["from_valid"] == 1
        and sample["io_flush"] == 0
        and sample["bpu_valid"] == 1
        and sample["s0_flush"] == 1
        and sample["data_ready"] == 1
        and sample["s1_ready"] == 1
        and sample["s0_fire"] == 0
        and _bpu_matches_s0(sample)
        for sample in samples
    ), {"tail": samples[-64:]}
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-607", "BIN-749")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_s0_bpu_miss(env) -> None:
    _require_bpu_s3_ftq_observable(env)
    samples: list[dict] = []
    env.register_cycle_observer(lambda _cycle, active_env: samples.append(_snapshot(active_env)))
    _initialize_bpu_s3_stream(env)
    _drive_bpu_s3_until_hit(
        env,
        "bpu_miss_allows_entry",
        max_cycles=_cycle_limit("TB_ICACHE_S0_BPU_MISS_MAX_CYCLES", 512),
    )
    assert any(
        sample["ftq_valid"] == 1
        and sample["from_valid"] == 1
        and sample["data_ready"] == 1
        and sample["s1_ready"] == 1
        and sample["io_flush"] == 0
        and sample["s0_flush"] == 0
        and sample["s0_fire"] == 1
        and _bpu_is_after_s0(sample)
        for sample in samples
    ), {"tail": samples[-64:]}
    assert env.functional_coverage.key_hit(
        "icache_waylookup_flush", "bpu_flush_nonmatching"
    )
    assert not env.monitor.get_errors()

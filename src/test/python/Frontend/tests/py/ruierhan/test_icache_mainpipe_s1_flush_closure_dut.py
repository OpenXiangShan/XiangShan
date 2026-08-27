"""Directed DUT closure for ICache MainPipe s1 flush functional coverage."""

from __future__ import annotations

import os
from collections.abc import Callable, Sequence

import pytest

from env.funcov.py.icache.flush_from_bpu import (
    BpuS3Flush,
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
_REDIRECT_BASE = 0x8005_0000
_MAIN = "Frontend_top.Frontend.inner_icache.mainPipe."
_ICACHE = "Frontend_top.Frontend.inner_icache."
_IFU = "Frontend_top.Frontend.inner_ifu."


def _aliases(name: str) -> tuple[str, str]:
    return (name, f"TOP.{name}")


_SIGNALS = {
    "last_fire": _aliases(_ICACHE + "missUnit.lastFire"),
    "last_fire_next": _aliases(_ICACHE + "missUnit.lastFireNext"),
    "io_flush": (
        _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush",
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
    "s1_valid": _aliases(_MAIN + "s1_valid"),
    "s1_ready": _aliases(_MAIN + "s1_ready"),
    "s1_fire": _aliases(_MAIN + "s1_fire"),
    "s1_flush": (_MAIN + "s1_flush", _IFU + "s1_flush", f"TOP.{_MAIN}s1_flush"),
    "s1_fetch_finish": (
        _MAIN + "io_toIfu_req_valid",
    ),
    "s1_ftq0_flag": _aliases(_MAIN + "s1_req_0_ftqIdx_flag"),
    "s1_ftq0_value": _aliases(_MAIN + "s1_req_0_ftqIdx_value"),
    "miss_req_valid": (
        _MAIN + "__Vtogcov__io_missReq_valid",
    ),
    "miss_req_ready": (
        _MAIN + "__Vtogcov__io_missReq_ready",
    ),
    "miss_resp_valid": (
        _MAIN + "io_missResp_valid",
        _MAIN + "__Vtogcov__io_missResp_valid",
    ),
    "miss_resp_corrupt": (_MAIN + "__Vtogcov__io_missResp_bits_corrupt",),
    "miss_resp_denied": (_MAIN + "__Vtogcov__io_missResp_bits_denied",),
    "to_ifu_valid": _aliases(_MAIN + "io_toIfu_req_valid"),
    "to_ifu_ready": _aliases(_MAIN + "io_toIfu_req_ready"),
    "s2_valid": (
        _MAIN + "s2_valid",
        _MAIN + "__Vtogcov__s2_valid",
        "TOP." + _MAIN + "s2_valid",
    ),
}

for _index in range(4):
    _SIGNALS[f"should_fetch_{_index}"] = _aliases(
        f"{_MAIN}s1_shouldFetch_{_index}"
    )
    _SIGNALS[f"mshr_valid_{_index}"] = _aliases(
        f"{_MAIN}s1_mshrValid_{_index // 2}_{_index % 2}"
    )
    _SIGNALS[f"mshr_valid_reg_{_index}"] = _aliases(
        f"{_MAIN}s1_mshrValidReg_{_index // 2}_{_index % 2}"
    )


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
        for key in ("bpu_flag", "bpu_value", "s1_ftq0_flag", "s1_ftq0_value")
        if not _can_read(env, key)
    ]
    if missing:
        pytest.xfail(
            "BPU s3 flag/value or MainPipe s1 ftqIdx are not exported: "
            + ",".join(missing)
        )


def _snapshot(env) -> dict:
    return {
        "cycle": int(env.current_cycle),
        **{key: _read(env, key) for key in _SIGNALS},
    }


def _register_s1_observer(env) -> list[dict]:
    samples: list[dict] = []

    def observe(cycle, active_env) -> None:
        samples.append(
            {
                "cycle": int(cycle),
                **{key: _read(active_env, key) for key in _SIGNALS},
            }
        )

    env.register_cycle_observer(observe)
    return samples


def _run_until(env, predicate: Callable[[], bool], *, max_cycles: int, label: str) -> None:
    for _ in range(int(max_cycles)):
        if predicate():
            return
        env.step(1)
    raise AssertionError(
        {
            "reason": f"timeout while waiting for {label}",
            "max_cycles": int(max_cycles),
            "last": _snapshot(env),
            "icache": env.icache_agent.get_stats(),
            "backend": env.backend_model.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


def _wait_hit(env, bin_name: str, *, max_cycles: int) -> None:
    group = "icache_mainpipe_s1_flush"
    _run_until(
        env,
        lambda: env.functional_coverage.key_hit(group, bin_name),
        max_cycles=max_cycles,
        label=f"{group}.{bin_name}",
    )


def _wait_group_hit(env, group: str, bin_name: str, *, max_cycles: int) -> None:
    _run_until(
        env,
        lambda: env.functional_coverage.key_hit(group, bin_name),
        max_cycles=max_cycles,
        label=f"{group}.{bin_name}",
    )


def _bpu_is_after_s1(sample: dict) -> bool:
    return ftq_ptr_is_strictly_after_current(
        BpuS3Flush(
            valid=sample["bpu_valid"],
            flag=sample["bpu_flag"],
            value=sample["bpu_value"],
        ),
        (sample["s1_ftq0_flag"], sample["s1_ftq0_value"]),
    ) is True


def _pending_response(env, *, min_cycles: int = 0):
    candidates = [
        item
        for item in getattr(env.icache_agent, "pending", ())
        if int(getattr(item, "ready_cycle", -1))
        >= int(env.current_cycle) + int(min_cycles)
    ]
    return min(candidates, key=lambda item: int(getattr(item, "ready_cycle"))) if candidates else None


def _redirect_target(attempt: int) -> int:
    return _REDIRECT_BASE + ((int(attempt) & 0x3F) * 0x40)


def _pending_miss(sample: dict) -> bool:
    return (
        int(sample["s1_valid"]) == 1
        and int(sample["s1_fetch_finish"]) == 0
        and any(int(sample[f"should_fetch_{index}"]) for index in range(4))
    )


def _raw_refill_match(sample: dict) -> bool:
    return int(sample["miss_resp_valid"]) == 1 and any(
        int(sample[f"mshr_valid_{index}"]) for index in range(4)
    )


def _initialize_bpu_s3_stream(env) -> None:
    _load_two_fetch_loop(env)
    _warm_two_fetch_execution(env)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=0.0,
        seed=0x6617,
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


def _drive_bpu_s3_until_s1_hit(env, bin_name: str, *, max_cycles: int) -> None:
    predictor_disabled = False
    disabled_at = -1
    s1_windows = 0
    bpu_samples: list[dict] = []

    try:
        for _ in range(int(max_cycles)):
            if env.functional_coverage.key_hit("icache_mainpipe_s1_flush", bin_name):
                return

            sample = _snapshot(env)
            if int(sample["s1_valid"]) == 1:
                s1_windows += 1
            if int(sample["bpu_valid"]) == 1:
                bpu_samples.append(sample)
                bpu_samples[:] = bpu_samples[-16:]

            if (
                int(sample["s1_valid"]) == 1
                and int(sample["s1_flush"]) == 0
                and not predictor_disabled
            ):
                env.set_bp_ctrl_enable(
                    ubtb_enable=0,
                    abtb_enable=0,
                    mbtb_enable=0,
                    tage_enable=0,
                    sc_enable=0,
                    ittage_enable=0,
                )
                predictor_disabled = True
                disabled_at = int(env.current_cycle)
            elif predictor_disabled and int(env.current_cycle) - disabled_at >= 6:
                _restore_predictors(env)
                predictor_disabled = False

            env.step(1)
    finally:
        _restore_predictors(env)

    assert env.functional_coverage.key_hit("icache_mainpipe_s1_flush", bin_name), {
        "reason": "BPU s3 flush did not align with requested MainPipe s1 flush bin",
        "bin": bin_name,
        "max_cycles": int(max_cycles),
        "s1_windows": int(s1_windows),
        "last_bpu_samples": bpu_samples,
        "last": _snapshot(env),
        "icache": env.icache_agent.get_stats(),
        "backend": env.backend_model.get_stats(),
        "monitor_errors": env.monitor.get_errors(),
    }


@pytest.mark.funcov_bins("BIN-616", "BIN-736", "BIN-742", "BIN-743", "BIN-745")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_s1_global_flush_hit(env) -> None:
    samples = _register_s1_observer(env)
    _initialize_cacheable_stream(env, _BASE, latency=1, samples=samples)

    attempts = _cycle_limit("TB_ICACHE_S1_GLOBAL_FLUSH_HIT_ATTEMPTS", 48)
    for attempt in range(attempts):
        _run_until(
            env,
            lambda: _read(env, "s1_valid") == 1
            and _read(env, "s1_fetch_finish") == 1
            and _read(env, "s1_flush") == 0,
            max_cycles=_cycle_limit("TB_ICACHE_S1_HIT_WINDOW_WAIT", 6000),
            label="completed s1 hit/fetch response",
        )
        env.backend_model.inject_redirect(
            _redirect_target(attempt),
            "ctrl_redirect",
            delay_cycles=0,
        )
        try:
            _wait_hit(env, "global_flush_clears_s1_hit", max_cycles=32)
            assert any(
                sample["s1_flush"] == 1
                and sample["to_ifu_valid"] == 0
                and sample["s1_fire"] == 0
                for sample in samples[-64:]
            ), {"tail": samples[-64:]}
            for group, bin_name in (
                ("icache_waylookup_update", "update_flush_same_cycle"),
                ("icache_waylookup_flush", "global_flush_clears_all"),
                ("icache_waylookup_flush", "flush_wins_read"),
                ("icache_waylookup_flush", "flush_wins_update"),
            ):
                _wait_group_hit(env, group, bin_name, max_cycles=32)
            assert not env.monitor.get_errors()
            return
        except AssertionError:
            if attempt + 1 >= attempts:
                raise


@pytest.mark.funcov_bins("BIN-769")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_s1_global_flush_pending_miss(env) -> None:
    samples = _register_s1_observer(env)
    _initialize_cacheable_stream(env, _BASE, latency=48, samples=samples)

    _run_until(
        env,
        lambda: _pending_miss(_snapshot(env)),
        max_cycles=_cycle_limit("TB_ICACHE_S1_PENDING_MISS_WAIT", 6000),
        label="s1 pending miss",
    )
    env.backend_model.inject_redirect(_REDIRECT_BASE, "ctrl_redirect", delay_cycles=0)
    _wait_hit(env, "global_flush_clears_s1_pending_miss", max_cycles=64)
    assert any(
        sample["s1_flush"] == 1
        and sample["to_ifu_valid"] == 0
        and sample["s1_fire"] == 0
        for sample in samples[-64:]
    ), {"tail": samples[-64:]}
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-618")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_s1_bpu_miss(env) -> None:
    _require_bpu_s3_ftq_observable(env)
    samples = _register_s1_observer(env)
    _initialize_bpu_s3_stream(env)
    _drive_bpu_s3_until_s1_hit(
        env,
        "bpu_miss_keeps_s1",
        max_cycles=_cycle_limit("TB_ICACHE_S1_BPU_MISS_MAX_CYCLES", 512),
    )
    assert any(
        sample["s1_valid"] == 1
        and sample["io_flush"] == 0
        and sample["s1_flush"] == 0
        and _bpu_is_after_s1(sample)
        for sample in samples
    ), {"tail": samples[-64:]}
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-620")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_late_refill_after_flush(env) -> None:
    _initialize_cacheable_stream(env, _BASE, latency=64)

    _run_until(
        env,
        lambda: _pending_miss(_snapshot(env)),
        max_cycles=_cycle_limit("TB_ICACHE_S1_LATE_REFILL_PENDING_WAIT", 6000),
        label="s1 pending miss before refill",
    )
    delayed = _pending_response(env, min_cycles=4)
    assert delayed is not None, {
        "reason": "no delayed ICache response is associated with the pending miss",
        "pending": list(getattr(env.icache_agent, "pending", ())),
    }
    delayed_key = (
        int(getattr(delayed, "source")),
        int(getattr(delayed, "addr")),
    )
    env.backend_model.inject_redirect(_REDIRECT_BASE, "ctrl_redirect", delay_cycles=0)
    _wait_hit(env, "global_flush_clears_s1_pending_miss", max_cycles=64)
    _wait_hit(
        env,
        "late_refill_ignored_after_flush",
        max_cycles=1,
    )
    _run_until(
        env,
        lambda: any(
            (int(record["source"]), int(record["address"])) == delayed_key
            and int(record["beat_idx"]) == 1
            for record in env.icache_agent.get_stats().get("response_records", [])
        ),
        max_cycles=_cycle_limit("TB_ICACHE_S1_LATE_RESPONSE_WAIT", 256),
        label="late response after the old s1 context was flushed",
    )
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-621", "BIN-706")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_flush_refill_race(env) -> None:
    samples = _register_s1_observer(env)
    _initialize_cacheable_stream(env, _BASE, latency=1, samples=samples)

    attempts = _cycle_limit("TB_ICACHE_S1_REFILL_RACE_ATTEMPTS", 64)
    for attempt in range(attempts):
        _run_until(
            env,
            lambda: _read(env, "s1_valid") == 1
            and _read(env, "s1_fetch_finish") == 1
            and _read(env, "s1_flush") == 0,
            max_cycles=_cycle_limit("TB_ICACHE_S1_REFILL_RACE_FIRE_WAIT", 6000),
            label="completed s1 response while one-cycle refills continue",
        )
        env.backend_model.inject_redirect(
            _redirect_target(attempt),
            "ctrl_redirect",
            delay_cycles=0,
        )
        try:
            _wait_hit(env, "flush_wins_matching_refill", max_cycles=32)
            break
        except AssertionError:
            if attempt + 1 >= attempts:
                raise
    _wait_group_hit(
        env,
        "icache_missunit_flush",
        "redirect_suppresses_sram_write",
        max_cycles=64,
    )
    assert any(
        sample["s1_flush"] == 1
        and _raw_refill_match(sample)
        and sample["to_ifu_valid"] == 0
        and sample["s1_fire"] == 0
        for sample in samples[-256:]
    ), {"tail": samples[-256:]}
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-675")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_flush_registered_refill(env) -> None:
    samples = _register_s1_observer(env)
    _initialize_cacheable_stream(env, _BASE, latency=32, samples=samples)

    attempts = _cycle_limit("TB_ICACHE_S1_REGISTERED_REFILL_FLUSH_ATTEMPTS", 64)
    for attempt in range(attempts):
        _run_until(
            env,
            lambda: _raw_refill_match(_snapshot(env)) and _read(env, "s1_flush") == 0,
            max_cycles=2048,
            label="raw refill before registered-refill flush window",
        )
        env.backend_model.inject_redirect(
            _redirect_target(attempt),
            "ctrl_redirect",
            delay_cycles=0,
        )
        env.step(1)
        if env.functional_coverage.key_hit(
            "icache_mainpipe_s1_flush", "flush_cancels_registered_refill"
        ):
            assert any(
                sample["s1_flush"] == 1
                and sample["to_ifu_valid"] == 0
                and sample["s1_fire"] == 0
                for sample in samples[-64:]
            ), {"tail": samples[-64:]}
            assert not env.monitor.get_errors()
            return
    _wait_hit(env, "flush_cancels_registered_refill", max_cycles=1)


@pytest.mark.funcov_bins("BIN-645")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_global_flush_clears_s2(env) -> None:
    if not _can_read(env, "s2_valid"):
        pytest.xfail(
            "MainPipe s2_valid is not exported by the current Verilator build; "
            "BIN-645 cannot be sampled without rebuilding the DUT with this signal"
        )
    samples = _register_s1_observer(env)
    _initialize_cacheable_stream(env, _BASE, latency=1, samples=samples)

    attempts = _cycle_limit("TB_ICACHE_S2_GLOBAL_FLUSH_ATTEMPTS", 64)
    for attempt in range(attempts):
        _run_until(
            env,
            lambda: _read(env, "s2_valid") == 1
            and _read(env, "io_flush") == 0
            and _read(env, "bpu_valid") == 0,
            max_cycles=_cycle_limit("TB_ICACHE_S2_VALID_WAIT", 6000),
            label="active s2 context without a BPU s3 flush",
        )
        env.backend_model.inject_redirect(
            _redirect_target(attempt),
            "ctrl_redirect",
            delay_cycles=0,
        )
        try:
            _wait_group_hit(
                env,
                "icache_mainpipe_s2_ecc",
                "global_flush_clears_s2",
                max_cycles=32,
            )
            break
        except AssertionError:
            if attempt + 1 >= attempts:
                raise

    env.step(1)
    assert any(
        current["s2_valid"] == 1
        and current["io_flush"] == 1
        and current["bpu_valid"] == 0
        and current["s1_fire"] == 0
        and following["s2_valid"] == 0
        for current, following in zip(samples, samples[1:])
    ), {"tail": samples[-64:]}
    assert not env.monitor.get_errors()

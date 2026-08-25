"""Directed DUT closure for ICache MainPipe hit and backpressure bins."""

from __future__ import annotations

import os
import random
from collections.abc import Callable, Sequence

import pytest

from tests.py.jiabowen.test_functional_coverage_baremode import (
    _load_nop_program,
    _warmup_commits,
)
from tests.py.zhaoxinran.test_multi_branch import (
    test_multi_branch_random_positions as _run_multi_branch_positions,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BASE = 0x8008_0000
_CROSS_BASE = 0x8009_0000
_NOP = 0x0000_0013
_MAIN = "Frontend_top.Frontend.inner_icache.mainPipe."


def _aliases(name: str) -> tuple[str, str]:
    return name, f"TOP.{name}"


_SIGNALS = {
    "s1_valid": _aliases(_MAIN + "s1_valid"),
    "s1_flush": _aliases(_MAIN + "s1_flush"),
    "cross0": (
        _MAIN + "s1_req_0_isCrossLine",
        _MAIN + "accessTrace_crossLine",
    ),
    "cross1": (
        _MAIN + "s1_req_1_isCrossLine",
        _MAIN + "s1_isCrossLine_1",
    ),
    "req1_valid": _aliases(_MAIN + "s1_req_1_valid"),
    "s1_addr": _aliases(_MAIN + "s1_req_0_vAddr_0_addr"),
    "s1_ftq_flag": _aliases(_MAIN + "s1_req_0_ftqIdx_flag"),
    "s1_ftq_value": _aliases(_MAIN + "s1_req_0_ftqIdx_value"),
    "fetch_finish": _aliases(_MAIN + "io_toIfu_req_valid"),
    "to_ifu_valid": _aliases(_MAIN + "io_toIfu_req_valid"),
    "to_ifu_ready": _aliases(_MAIN + "io_toIfu_req_ready"),
    "miss_resp_valid": _aliases(_MAIN + "io_missResp_valid"),
}

for _index in range(4):
    _req = _index // 2
    _line = _index % 2
    _hit_r = _MAIN + "s1_hits_r" + (f"_{_index}" if _index else "")
    _SIGNALS[f"hit_{_index}"] = (
        _MAIN + f"s1_hits_{_req}_{_line}",
        _hit_r,
        "TOP." + _MAIN + f"s1_hits_{_req}_{_line}",
        "TOP." + _hit_r,
    )
    _SIGNALS[f"should_{_index}"] = _aliases(
        _MAIN + f"s1_shouldFetch_{_index}"
    )
    _SIGNALS[f"waymask_{_index}"] = _aliases(
        _MAIN + f"s1_wayLookupEntry_{_req}_waymask_{_line}"
    )
    _SIGNALS[f"mshr_{_index}"] = _aliases(
        _MAIN + f"s1_mshrValid_{_req}_{_line}"
    )
    _SIGNALS[f"mshr_reg_{_index}"] = _aliases(
        _MAIN + f"s1_mshrValidReg_{_req}_{_line}"
    )

_SIGNALS["sram_valid_0"] = _aliases(_MAIN + "s1_sramRespValid")
_SIGNALS["sram_valid_1"] = _aliases(_MAIN + "s1_sramValid_0_1")


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


def _snapshot(env) -> dict[str, int]:
    return {
        "cycle": int(env.current_cycle),
        **{key: _read(env, key) for key in _SIGNALS},
    }


def _register_observer(env) -> list[dict[str, int]]:
    samples: list[dict[str, int]] = []

    def observe(_cycle, active_env) -> None:
        samples.append(_snapshot(active_env))

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


def _wait_hit(env, group: str, bin_name: str, *, max_cycles: int = 6000) -> None:
    _run_until(
        env,
        lambda: env.functional_coverage.key_hit(group, bin_name),
        max_cycles=max_cycles,
        label=f"{group}.{bin_name}",
    )


def _jal(rd: int, offset: int) -> int:
    imm = int(offset) & 0x1F_FFFF
    return (
        (((imm >> 20) & 1) << 31)
        | (((imm >> 1) & 0x3FF) << 21)
        | (((imm >> 11) & 1) << 20)
        | (((imm >> 12) & 0xFF) << 12)
        | ((int(rd) & 0x1F) << 7)
        | 0x6F
    )


def _load_fetch_loop(env, base: int, *, target_offset: int) -> int:
    target = int(base) + int(target_offset)
    payload = bytearray((_NOP.to_bytes(4, "little")) * 64)
    if target_offset:
        payload[0:4] = _jal(0, target_offset).to_bytes(4, "little")
    for index in range(7):
        offset = int(target_offset) + index * 4
        payload[offset : offset + 4] = _NOP.to_bytes(4, "little")
    tail = int(target_offset) + 7 * 4
    payload[tail : tail + 4] = _jal(0, -7 * 4).to_bytes(4, "little")
    env.load_program(bytes(payload), int(base))
    return target


def _initialize_loop(env, base: int, *, target_offset: int, latency: int) -> list[dict[str, int]]:
    target = _load_fetch_loop(env, int(base), target_offset=int(target_offset))
    env.icache_agent.configure(
        hit_latency=int(latency),
        miss_latency=int(latency),
        miss_rate=1.0,
        seed=0x6608 + int(target_offset),
    )
    samples = _register_observer(env)
    env.initialize(reset_vector=int(base), bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(int(base))
    _run_until(
        env,
        lambda: any(int(observation.pc) == target for observation in env.monitor.observations),
        max_cycles=_cycle_limit("TB_ICACHE_HITPATH_TARGET_WAIT", 6000),
        label=f"loop target 0x{target:x}",
    )
    return samples


@pytest.mark.funcov_bins("BIN-608")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_single_line_sram_hit(env) -> None:
    samples = _initialize_loop(env, _BASE, target_offset=0, latency=8)
    _wait_hit(env, "icache_mainpipe_s1_sram", "single_line_sram_hit")
    assert any(
        sample["s1_valid"] == 1
        and sample["cross0"] == 0
        and sample["sram_valid_0"] == 1
        and sample["waymask_0"] != 0
        and sample["hit_0"] == 1
        for sample in samples
    ), {"tail": samples[-64:]}
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-609", "BIN-611")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_cross_line_dual_sram_hit(env) -> None:
    samples = _initialize_loop(env, _CROSS_BASE, target_offset=0x34, latency=8)
    _wait_hit(env, "icache_mainpipe_s1_sram", "cross_line_dual_sram_hit")
    _wait_hit(env, "icache_mainpipe_s1_sram", "cross_line_bank_mapping")
    assert any(
        sample["s1_valid"] == 1
        and sample["cross0"] == 1
        and sample["sram_valid_0"] == 1
        and sample["sram_valid_1"] == 1
        and sample["waymask_0"] != 0
        and sample["waymask_1"] != 0
        and sample["hit_0"] == 1
        and sample["hit_1"] == 1
        and ((sample["s1_addr"] << 1) & 0x3F) >= 8
        for sample in samples
    ), {"tail": samples[-96:]}
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-612")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_dual_request_independent(env) -> None:
    samples = _register_observer(env)
    random_state = random.getstate()
    random.seed(1)
    try:
        _run_multi_branch_positions(env)
    finally:
        random.setstate(random_state)

    _wait_hit(
        env,
        "icache_mainpipe_s1_sram",
        "dual_request_independent",
        max_cycles=1,
    )
    assert any(
        sample["s1_valid"] == 1
        and sample["req1_valid"] == 1
        and sample["cross0"] != sample["cross1"]
        and (
            tuple(sample[f"waymask_{index}"] for index in range(2))
            != tuple(sample[f"waymask_{index}"] for index in range(2, 4))
            or tuple(sample[f"hit_{index}"] for index in range(2))
            != tuple(sample[f"hit_{index}"] for index in range(2, 4))
            or tuple(sample[f"should_{index}"] for index in range(2))
            != tuple(sample[f"should_{index}"] for index in range(2, 4))
        )
        for sample in samples
    ), {"tail": samples[-96:]}
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-613")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_hit_response_stall(env) -> None:
    samples = _initialize_loop(env, _BASE, target_offset=0, latency=8)
    _wait_hit(env, "icache_mainpipe_s1_sram", "single_line_sram_hit")

    env.backend_model.set_can_accept(0)
    try:
        _wait_hit(
            env,
            "icache_mainpipe_s1_backpressure",
            "hit_response_stall",
            max_cycles=_cycle_limit("TB_ICACHE_HIT_STALL_WAIT", 4096),
        )
        env.step(2)
    finally:
        env.backend_model.set_can_accept(1)

    stalled = [
        sample
        for sample in samples
        if sample["s1_valid"] == 1
        and sample["to_ifu_valid"] == 1
        and sample["to_ifu_ready"] == 0
        and not any(sample[f"should_{index}"] for index in range(4))
        and not any(sample[f"mshr_reg_{index}"] for index in range(4))
    ]
    assert stalled, {"tail": samples[-96:]}
    assert any(
        previous["s1_valid"] == 1
        and previous["to_ifu_valid"] == 1
        and previous["to_ifu_ready"] == 0
        and current["s1_valid"] == 1
        and current["to_ifu_valid"] == 1
        and current["to_ifu_ready"] == 0
        and (
            previous["s1_addr"],
            previous["s1_ftq_flag"],
            previous["s1_ftq_value"],
        )
        == (
            current["s1_addr"],
            current["s1_ftq_flag"],
            current["s1_ftq_value"],
        )
        for previous, current in zip(samples, samples[1:])
    ), {"tail": samples[-96:]}
    assert not env.monitor.get_errors()


@pytest.mark.funcov_bins("BIN-614")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_tc_icache_mainpipe_refill_completion_stall(env) -> None:
    samples = _register_observer(env)
    _load_nop_program(env, words=2048)
    assert _warmup_commits(env, target_count=4, max_cycles=6000) >= 4

    env.backend_model.set_can_accept(0)
    try:
        _wait_hit(
            env,
            "icache_mainpipe_s1_backpressure",
            "refill_completion_stall",
            max_cycles=_cycle_limit("TB_ICACHE_REFILL_STALL_WAIT", 6000),
        )
    finally:
        env.backend_model.set_can_accept(1)

    assert any(
        previous["miss_resp_valid"] == 1
        and any(previous[f"mshr_{index}"] for index in range(4))
        and current["s1_valid"] == 1
        and current["fetch_finish"] == 1
        and current["to_ifu_valid"] == 1
        and current["to_ifu_ready"] == 0
        and any(current[f"mshr_reg_{index}"] for index in range(4))
        for previous, current in zip(samples, samples[1:])
    ), {"tail": samples[-96:]}
    assert not env.monitor.get_errors()

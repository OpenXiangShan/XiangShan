"""Generic helpers shared by Frontend DUT integration tests.

This module deliberately has no dependency on a particular Frontend block,
coverage domain, or test scenario.
"""

from __future__ import annotations

import os
from collections.abc import Callable, Mapping, Sequence


def cycle_limit(name: str, default: int) -> int:
    raw = os.getenv(str(name), "").strip()
    if not raw:
        return int(default)
    value = int(raw, 0)
    assert value > 0, f"{name} must be positive"
    return int(value)


def read_dut_signal(
    env,
    names: Sequence[str] | str,
    *,
    default: int | None = None,
    required: bool = False,
) -> int | None:
    """Read the first available DUT signal from an ordered alias list."""
    if isinstance(names, str):
        names = (names,)
    aliases = tuple(str(name) for name in names)

    cache = getattr(env, "_ruierhan_internal_signal_cache", None)
    if cache is None:
        cache = {}
        setattr(env, "_ruierhan_internal_signal_cache", cache)
    cache_key = aliases
    if cache_key in cache:
        signal = cache[cache_key]
        value = None if signal is None else getattr(signal, "value", None)
        if value is not None:
            return int(value)
        if required:
            raise AssertionError({
                "reason": "required DUT signal is unavailable",
                "candidates": aliases,
            })
        return default

    for name in aliases:
        try:
            signal = getattr(env.dut, name, None)
            if signal is None:
                getter = getattr(env.dut, "GetInternalSignal", None)
                signal = getter(name) if callable(getter) else None
            value = None if signal is None else getattr(signal, "value", None)
            if value is not None:
                cache[cache_key] = signal
                return int(value)
        except Exception:
            continue
    cache[cache_key] = None
    if required:
        raise AssertionError({
            "reason": "required DUT signal is unavailable",
            "candidates": aliases,
        })
    return default


def poll_until(env, predicate: Callable[[], bool], *, max_cycles: int) -> bool:
    """Check a predicate before each DUT step."""
    for _ in range(int(max_cycles)):
        if predicate():
            return True
        env.step(1)
    return False


def wait_until(
    env,
    predicate: Callable[[], bool],
    *,
    max_cycles: int = 2000,
    label: str | None = None,
    description: str | None = None,
    snapshot: Callable[[], object] | None = None,
    diagnostics: Callable[[], Mapping[str, object]] | None = None,
) -> None:
    """Wait with a consistent timeout report for any DUT condition."""
    label = str(label if label is not None else description or "condition")
    if poll_until(env, predicate, max_cycles=max_cycles):
        return
    detail: dict[str, object] = {
        "reason": f"timeout while waiting for {label}",
        "cycle": int(env.current_cycle),
        "max_cycles": int(max_cycles),
    }
    if snapshot is not None:
        detail["last"] = snapshot()
    if diagnostics is not None:
        detail.update(dict(diagnostics()))
    raise AssertionError(detail)


def _first_observed_index(env, pc: int) -> int:
    for idx, observation in enumerate(env.monitor.observations):
        if int(observation.pc) == int(pc):
            return int(idx)
    return -1


def _assert_observed_instrs(env, expected_block: list[tuple[int, int, bool]]) -> None:
    observed_by_pc = {
        int(observation.pc): observation
        for observation in env.monitor.observations
        if any(int(observation.pc) == int(pc) for pc, _, _ in expected_block)
    }
    for pc, instr, is_rvc in expected_block:
        observation = observed_by_pc[int(pc)]
        assert int(observation.instr) == int(instr), {
            "pc": hex(int(pc)),
            "expected_instr": hex(int(instr)),
            "observed_instr": hex(int(observation.instr)),
        }
        assert bool(observation.is_rvc) == bool(is_rvc), {
            "pc": hex(int(pc)),
            "expected_is_rvc": bool(is_rvc),
            "observed_is_rvc": bool(observation.is_rvc),
        }


def _wait_for_req_count(env, agent, count: int, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if int(agent.get_stats().get("req_count", 0)) >= int(count):
            return True
        env.step(1)
    return int(agent.get_stats().get("req_count", 0)) >= int(count)


def _wait_for_req_delta(env, agent, delta: int = 1, *, max_cycles: int = 2000) -> int:
    start = int(agent.get_stats().get("req_count", 0))
    target = start + int(delta)
    for _ in range(int(max_cycles)):
        env.step(1)
        now = int(agent.get_stats().get("req_count", 0))
        if now >= target:
            return now
    return int(agent.get_stats().get("req_count", 0))


def _wait_for_resp_delta(env, agent, delta: int = 1, *, max_cycles: int = 2000) -> int:
    start = int(agent.get_stats().get("resp_count", 0))
    target = start + int(delta)
    for _ in range(int(max_cycles)):
        env.step(1)
        now = int(agent.get_stats().get("resp_count", 0))
        if now >= target:
            return now
    return int(agent.get_stats().get("resp_count", 0))


def _wait_for_request_addr(
    env,
    agent,
    addr: int,
    *,
    max_cycles: int = 2000,
) -> bool:
    for _ in range(int(max_cycles)):
        if int(addr) in agent.get_stats().get("request_addrs", []):
            return True
        env.step(1)
    return int(addr) in agent.get_stats().get("request_addrs", [])


def _wait_for_resp_count(
    env,
    agent,
    count: int,
    *,
    max_cycles: int = 2000,
) -> bool:
    for _ in range(int(max_cycles)):
        if int(agent.get_stats().get("resp_count", 0)) >= int(count):
            return True
        env.step(1)
    return int(agent.get_stats().get("resp_count", 0)) >= int(count)


def _wait_for_monitor_exception(env, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if int(env.monitor.exception_mark_count) > 0:
            return True
        env.step(1)
    return int(env.monitor.exception_mark_count) > 0


def _wait_for_observed_pc(env, pc: int, *, max_cycles: int = 2000) -> bool:
    for _ in range(int(max_cycles)):
        if any(int(obs.pc) == int(pc) for obs in env.monitor.observations):
            return True
        env.step(1)
    return any(int(obs.pc) == int(pc) for obs in env.monitor.observations)


def _wait_for_observed_pc_sequence(
    env,
    pcs: list[int],
    *,
    max_cycles: int = 2000,
) -> bool:
    expected = {int(pc) for pc in pcs}
    for _ in range(int(max_cycles)):
        seen = {
            int(obs.pc)
            for obs in env.monitor.observations
            if int(obs.pc) in expected
        }
        if expected.issubset(seen):
            return True
        env.step(1)
    seen = {
        int(obs.pc)
        for obs in env.monitor.observations
        if int(obs.pc) in expected
    }
    return expected.issubset(seen)

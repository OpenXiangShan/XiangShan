"""Generic helpers shared by Frontend DUT integration tests.

This module deliberately has no dependency on a particular Frontend block,
coverage domain, or test scenario.
"""

from __future__ import annotations

import os
from collections.abc import Callable, Mapping, Sequence

from env.support.signal_utils import read_first_signal


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
    wide: bool = False,
) -> int | None:
    """Read the first available DUT signal from an ordered alias list."""
    if isinstance(names, str):
        names = (names,)
    aliases = tuple(str(name) for name in names)

    cache = getattr(env, "_ruierhan_internal_signal_cache", None)
    if cache is None:
        cache = {}
        setattr(env, "_ruierhan_internal_signal_cache", cache)
    value = read_first_signal(env.dut, aliases, wide=wide, cache=cache)
    if value is not None:
        return value
    if required:
        raise AssertionError({
            "reason": "required DUT signal is unavailable",
            "candidates": aliases,
        })
    return default


def _poll(
    env, predicate: Callable[[], bool], *, max_cycles: int, step_first: bool = False,
) -> bool:
    for _ in range(int(max_cycles)):
        if not step_first and predicate():
            return True
        env.step(1)
        if step_first and predicate():
            return True
    return False


def poll_until(env, predicate: Callable[[], bool], *, max_cycles: int) -> bool:
    """Check a predicate before each DUT step."""
    return _poll(env, predicate, max_cycles=max_cycles)


def run_until(env, predicate: Callable[[], bool], *, max_cycles: int) -> bool:
    """Include a final predicate check after the last DUT step."""
    return poll_until(env, predicate, max_cycles=max_cycles) or bool(predicate())


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


def _wait_for_stat_count(env, agent, stat: str, count: int, *, max_cycles: int) -> bool:
    return run_until(
        env, lambda: int(agent.get_stats().get(stat, 0)) >= int(count), max_cycles=max_cycles,
    )


def _wait_for_stat_delta(env, agent, stat: str, delta: int, *, max_cycles: int) -> int:
    start = int(agent.get_stats().get(stat, 0))
    target = start + int(delta)
    now = start

    def reached() -> bool:
        nonlocal now
        now = int(agent.get_stats().get(stat, 0))
        return now >= target

    if _poll(env, reached, max_cycles=max_cycles, step_first=True):
        return now
    return int(agent.get_stats().get(stat, 0))


def _wait_for_req_count(env, agent, count: int, *, max_cycles: int = 2000) -> bool:
    return _wait_for_stat_count(env, agent, "req_count", count, max_cycles=max_cycles)


def _wait_for_req_delta(env, agent, delta: int = 1, *, max_cycles: int = 2000) -> int:
    return _wait_for_stat_delta(env, agent, "req_count", delta, max_cycles=max_cycles)


def _wait_for_resp_delta(env, agent, delta: int = 1, *, max_cycles: int = 2000) -> int:
    return _wait_for_stat_delta(env, agent, "resp_count", delta, max_cycles=max_cycles)


def _wait_for_request_addr(
    env,
    agent,
    addr: int,
    *,
    max_cycles: int = 2000,
) -> bool:
    return run_until(
        env, lambda: int(addr) in agent.get_stats().get("request_addrs", []), max_cycles=max_cycles,
    )


def _wait_for_resp_count(
    env,
    agent,
    count: int,
    *,
    max_cycles: int = 2000,
) -> bool:
    return _wait_for_stat_count(env, agent, "resp_count", count, max_cycles=max_cycles)


def _wait_for_monitor_exception(env, *, max_cycles: int = 2000) -> bool:
    return run_until(env, lambda: int(env.monitor.exception_mark_count) > 0, max_cycles=max_cycles)


def _wait_for_observed_pc(env, pc: int, *, max_cycles: int = 2000) -> bool:
    return run_until(
        env, lambda: any(int(obs.pc) == int(pc) for obs in env.monitor.observations), max_cycles=max_cycles,
    )


def _wait_for_observed_pc_sequence(
    env,
    pcs: list[int],
    *,
    max_cycles: int = 2000,
) -> bool:
    expected = {int(pc) for pc in pcs}

    def complete() -> bool:
        seen = {
            int(obs.pc)
            for obs in env.monitor.observations
            if int(obs.pc) in expected
        }
        return expected.issubset(seen)

    return run_until(env, complete, max_cycles=max_cycles)

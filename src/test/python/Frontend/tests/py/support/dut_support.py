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
    max_cycles: int,
    label: str,
    snapshot: Callable[[], object] | None = None,
    diagnostics: Callable[[], Mapping[str, object]] | None = None,
) -> None:
    """Wait with a consistent timeout report for any DUT condition."""
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

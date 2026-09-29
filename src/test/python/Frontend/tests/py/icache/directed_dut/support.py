"""Shared mechanics for directed ICache DUT scenarios.

Keep transaction-specific stimulus, sampling, and failure evidence in each
test module. These helpers only centralize operations with identical behavior.
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


def read_cached_signal(env, names: Sequence[str]) -> int | None:
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


def poll_until(env, predicate: Callable[[], bool], *, max_cycles: int) -> bool:
    """Check before each step, exactly as the directed wait loops do."""
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
    """Wait with a consistent timeout report while preserving local timing."""
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


def wait_coverage_hit(
    env,
    group: str,
    bin_name: str,
    *,
    max_cycles: int,
    snapshot: Callable[[], object] | None = None,
    diagnostics: Callable[[], Mapping[str, object]] | None = None,
) -> None:
    """Wait for one functional-coverage key using the common timeout format."""
    wait_until(
        env,
        lambda: bool(env.functional_coverage.key_hit(group, bin_name)),
        max_cycles=max_cycles,
        label=f"{group}.{bin_name}",
        snapshot=snapshot,
        diagnostics=diagnostics,
    )


def clear_soft_prefetch(env) -> None:
    for slot in range(3):
        valid = getattr(env.dut, f"io_softPrefetch_{slot}_valid", None)
        address = getattr(env.dut, f"io_softPrefetch_{slot}_bits_vaddr", None)
        if valid is not None:
            valid.value = 0
        if address is not None:
            address.value = 0


def set_predictors(env, enabled: bool) -> None:
    value = 1 if enabled else 0
    env.set_bp_ctrl_enable(
        ubtb_enable=value,
        abtb_enable=value,
        mbtb_enable=value,
        tage_enable=value,
        sc_enable=value,
        ittage_enable=value,
    )


def restore_predictors(env) -> None:
    set_predictors(env, True)

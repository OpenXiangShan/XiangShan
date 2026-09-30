"""Shared mechanics for directed ICache DUT scenarios.

Keep transaction-specific stimulus, sampling, and failure evidence in each
test module. These helpers only centralize operations with identical behavior.
"""

from __future__ import annotations

from collections.abc import Callable, Iterable, Mapping, Sequence

from tests.py.support.dut_support import (
    cycle_limit,
    poll_until,
    read_dut_signal as _read_generic_dut_signal,
    wait_until,
)


_BPU_S0_WINDOW_SIGNALS = {
    "from_valid": (
        "Frontend_top.Frontend.inner_icache.mainPipe.io_fromWayLookup_valid",
        "Frontend_top.Frontend.inner_icache.mainPipe.__Vtogcov__io_fromWayLookup_valid",
    ),
    "data_ready": (
        "Frontend_top.Frontend.inner_icache.dataArray.io_read_req_ready",
        "Frontend_top.Frontend.inner_icache.dataArray.__Vtogcov__io_read_req_ready",
    ),
    "s1_ready": (
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_ready",
        "Frontend_top.Frontend.inner_icache.mainPipe.__Vtogcov__s1_ready",
    ),
}


def read_dut_signal(
    env,
    names: Sequence[str] | str,
    *,
    default: int | None = None,
    required: bool = False,
    prefer_recorder: bool = False,
) -> int | None:
    """Read the first available DUT signal from an ordered alias list.

    ``prefer_recorder`` preserves the cycle-snapshot behavior used by tests
    that sample through the functional-coverage recorder.  All other tests
    use the same direct DUT lookup and cache below.
    """
    if isinstance(names, str):
        names = (names,)
    aliases = tuple(str(name) for name in names)

    if prefer_recorder:
        recorder = getattr(env, "functional_coverage", None)
        reader = getattr(recorder, "_read_first_dut_signal", None)
        if callable(reader):
            value = reader(env.dut, aliases)
            if value is not None:
                return int(value)
            if required:
                raise AssertionError({
                    "reason": "required DUT signal is unavailable",
                    "candidates": aliases,
                })
            return default

    return _read_generic_dut_signal(
        env,
        aliases,
        default=default,
        required=required,
    )


def read_cached_signal(env, names: Sequence[str]) -> int | None:
    """Compatibility wrapper for callers using the old helper name."""
    return read_dut_signal(env, names)


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


def wait_coverage_hits(
    env,
    targets: Iterable[tuple[str, str]],
    *,
    max_cycles: int,
    snapshot: Callable[[], object] | None = None,
    diagnostics: Callable[[], Mapping[str, object]] | None = None,
) -> None:
    """Wait until every requested functional-coverage key is hit."""
    expected = tuple((str(group), str(name)) for group, name in targets)
    remaining = set(expected)

    def all_hit() -> bool:
        # Evaluate the predicate against a stable snapshot before mutating the
        # set of outstanding targets. Updating ``remaining`` while its own
        # iterator is active raises ``RuntimeError`` for multi-target waits.
        hit_targets = {
            target
            for target in tuple(remaining)
            if env.functional_coverage.key_hit(*target)
        }
        remaining.difference_update(hit_targets)
        return not remaining

    if all_hit():
        return
    wait_until(
        env,
        all_hit,
        max_cycles=max_cycles,
        label="functional coverage targets",
        snapshot=snapshot,
        diagnostics=lambda: {
            "missing": sorted(f"{group}.{name}" for group, name in remaining),
            **(dict(diagnostics()) if diagnostics is not None else {}),
        },
    )


def clear_soft_prefetch(env) -> None:
    for slot in range(3):
        valid = getattr(env.dut, f"io_softPrefetch_{slot}_valid", None)
        address = getattr(env.dut, f"io_softPrefetch_{slot}_bits_vaddr", None)
        if valid is not None:
            valid.value = 0
        if address is not None:
            address.value = 0


def set_soft_prefetch(env, addresses: Iterable[int]) -> None:
    """Present up to three soft-prefetch inputs without advancing the DUT."""
    clear_soft_prefetch(env)
    for slot, address in enumerate(tuple(addresses)[:3]):
        valid = getattr(env.dut, f"io_softPrefetch_{slot}_valid", None)
        value = getattr(env.dut, f"io_softPrefetch_{slot}_bits_vaddr", None)
        assert valid is not None and value is not None, {
            "missing_signal": f"io_softPrefetch_{slot}"
        }
        valid.value = 1
        value.value = int(address)


def drive_soft_prefetch(env, addresses: Iterable[int]) -> None:
    """Present soft-prefetch inputs for one cycle and then clear them."""
    set_soft_prefetch(env, addresses)
    env.step(1)
    clear_soft_prefetch(env)


def initialize_bpu_s3_stream(env, *, seed: int = 0x6605) -> None:
    """Prepare the shared live BPU s3 stream used by ICache scenarios."""
    from tests.py.jiabowen.test_two_fetch_directed_flow_dut import (
        _load_and_reset as _load_two_fetch_loop,
        _warm_frontend_execution as _warm_two_fetch_execution,
    )

    _load_two_fetch_loop(env)
    _warm_two_fetch_execution(env)
    env.icache_agent.configure(
        hit_latency=1,
        miss_latency=32,
        miss_rate=0.0,
        seed=int(seed),
    )
    set_predictors(env, True)


def trigger_bpu_s3_flush(env) -> None:
    env.bpu_ftq_scheduler.pulse_predictor_transition()


def s0_sampling_window(env) -> bool:
    return all(
        read_dut_signal(env, aliases) == 1
        for aliases in _BPU_S0_WINDOW_SIGNALS.values()
    )


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

"""Shared mechanics for directed ICache DUT scenarios.

Keep transaction-specific stimulus, sampling, and failure evidence in each
test module. These helpers only centralize operations with identical behavior.
"""

from __future__ import annotations

from env.runtime.frontend_init import (
    ConfigureAgent,
    FrontendInitConfig,
    LoadProgram,
    ProgramImage,
    initialize_frontend,
    run_initialization_steps,
)

from collections.abc import Callable, Iterable, Mapping, Sequence

from tests.py.support.dut_support import (
    cycle_limit,
    read_dut_signal as _read_generic_dut_signal,
    wait_until,
)


from .programs import (
    BPU_TRAINING_BASE as _BPU_TRAINING_BASE,
    build_bpu_training_program as _build_bpu_training_program,
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


_BPU_TRAINING_SIGNALS = {
    "ftq_valid": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_fromFtq_toMainPipe_valid",
    ),
    "ftq_ready": (
        "Frontend_top.Frontend._inner_icache_io_fromFtq_toMainPipe_ready",
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_fromFtq_toMainPipe_ready",
    ),
    "ftq_req1_valid": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_fromFtq_toMainPipe_bits_req_1_valid",
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
                raise AssertionError(
                    {
                        "reason": "required DUT signal is unavailable",
                        "candidates": aliases,
                    }
                )
            return default

    return _read_generic_dut_signal(
        env,
        aliases,
        default=default,
        required=required,
    )


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
    wait_coverage_hits(
        env,
        ((group, bin_name),),
        max_cycles=max_cycles,
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
    label = (
        f"{expected[0][0]}.{expected[0][1]}"
        if len(expected) == 1
        else "functional coverage targets"
    )

    def timeout_diagnostics() -> dict[str, object]:
        # Rebuild from the targets still outstanding after the last checked
        # cycle. A hit produced by that final step is not visible to the
        # predicate that just timed out.
        detail = {
            "missing": sorted(f"{group}.{name}" for group, name in remaining),
        }
        if diagnostics is not None:
            detail.update(dict(diagnostics()))
        return detail

    try:
        wait_until(
            env,
            all_hit,
            max_cycles=max_cycles,
            label=label,
            snapshot=snapshot,
            diagnostics=timeout_diagnostics,
        )
    except AssertionError as error:
        # One target may be sampled by the last step itself. Re-check without
        # advancing again so a hit on that cycle still counts. Refresh the
        # reported remainder too: the timeout was built before this re-check.
        if all_hit():
            return
        detail = error.args[0] if error.args else None
        if isinstance(detail, dict):
            detail["missing"] = sorted(f"{group}.{name}" for group, name in remaining)
        raise


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


def _read_bpu_training_signal(recorder, key: str) -> int:
    names = _BPU_TRAINING_SIGNALS[key]
    value = recorder._read_first_dut_signal(recorder.env.dut, names)
    assert value is not None, {
        "reason": "required directed-flow observation is unavailable",
        "label": str(key),
        "candidates": list(names),
    }
    return int(value)


def warm_bpu_training_stream(env, *, max_cycles: int | None = None) -> dict:
    recorder = getattr(env, "functional_coverage", None)
    assert (
        recorder is not None
    ), "directed flow tests require the canonical funcov recorder"
    assert getattr(recorder, "env", None) is env
    if max_cycles is None:
        max_cycles = cycle_limit("TB_TWO_FETCH_TRAIN_MAX_CYCLES", 4000)
    for _ in range(max(0, int(max_cycles))):
        env.step(1)
        dual_request_fire = (
            _read_bpu_training_signal(recorder, "ftq_valid") == 1
            and _read_bpu_training_signal(recorder, "ftq_ready") == 1
            and _read_bpu_training_signal(recorder, "ftq_req1_valid") == 1
        )
        if (
            int(env.backend_model.get_stats().get("commit_count", 0)) >= 1
            and int(env.icache_agent.get_stats().get("req_count", 0)) >= 4
            and recorder.key_hit("ifu_cfi_decode_type", "jal")
            and recorder.key_hit("two_fetch_ftq_eligibility", "eligible_dual")
            and recorder.key_hit("two_fetch_delivery", "dual_fire")
            and dual_request_fire
        ):
            return {
                "cycle": int(env.current_cycle),
                "icache": env.icache_agent.get_stats(),
                "backend": env.backend_model.get_stats(),
            }
    raise AssertionError(
        {
            "reason": "short-block loop did not reach dual-eligible frontend execution",
            "max_cycles": int(max_cycles),
            "icache": env.icache_agent.get_stats(),
            "backend": env.backend_model.get_stats(),
            "monitor_errors": env.monitor.get_errors(),
        }
    )


_warm_bpu_training_stream = warm_bpu_training_stream


def load_and_reset_bpu_training_stream(env) -> None:
    """Load the BPU training program and reset, without warming or configuring agents."""
    run_initialization_steps(
        env,
        (
            LoadProgram(
                ProgramImage(
                    payload=_build_bpu_training_program(), base_addr=_BPU_TRAINING_BASE
                )
            ),
        ),
    )
    initialize_frontend(
        env,
        FrontendInitConfig(
            env_config=env.config,
            reset_vector=_BPU_TRAINING_BASE,
            bare_mode=True,
            reset_cycles=20,
        ),
    )
    env.monitor.clear()
    env.monitor.set_expected_pc(_BPU_TRAINING_BASE)


def initialize_bpu_s3_stream(env, *, seed: int = 0x6605) -> None:
    """Prepare the shared live BPU s3 stream used by ICache scenarios."""
    load_and_reset_bpu_training_stream(env)
    warm_bpu_training_stream(env)
    run_initialization_steps(
        env,
        (
            ConfigureAgent(
                "icache",
                {
                    "hit_latency": 1,
                    "miss_latency": 32,
                    "miss_rate": 0.0,
                    "seed": int(seed),
                },
            ),
        ),
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

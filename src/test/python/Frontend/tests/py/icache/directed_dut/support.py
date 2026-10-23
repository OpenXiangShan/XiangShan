"""ICache startup plans, observations, and runtime test helpers."""

from __future__ import annotations

from collections.abc import Callable, Iterable, Mapping, Sequence
from dataclasses import dataclass, replace
from random import Random

from env.core.transactions import CommitTarget
from env.funcov.py.icache.signal_contract import MAIN_EXCEPTIONS, TO_IFU_EXCEPTIONS
from env.runtime.frontend_init import (
    ClearMonitor,
    ConfigureAgent,
    FrontendInitConfig,
    InitStep,
    LoadProgram,
    ProgramImage,
    SetBpCtrl,
    SetExpectedPc,
    run_initialization_steps,
)
from env.sequences import RunUntilCommitSequence
from env.support.env_config import EnvConfig
from tests.py.support.dut_support import (
    cycle_limit,
    read_dut_signal as _read_generic_dut_signal,
    wait_until,
)

from .programs import (
    BPU_TRAINING_BASE,
    DUAL_FETCH_BASE,
    build_bpu_training_program,
    build_cross_line_probe_program,
    build_dual_fetch_program,
    build_fetch_loop_program,
    build_idle_loop_program,
    build_nop_program,
    build_overlap2_program,
    build_same_set_jump_program,
    encode_jal,
)


# Signal lookup and bounded coverage waits.


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


# MainPipe cycle observations and dual-fetch transaction evidence.


def _aliases(name: str) -> tuple[str, str]:
    return (name, f"TOP.{name}")


_MAINPIPE_SIGNALS = {
    "last_fire": _aliases("Frontend_top.Frontend.inner_icache.missUnit.lastFire"),
    "last_fire_next": _aliases(
        "Frontend_top.Frontend.inner_icache.missUnit.lastFireNext"
    ),
    "miss_resp_valid": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.io_missResp_valid"
    ),
    "miss_resp_corrupt": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.__Vtogcov__io_missResp_bits_corrupt"
    ),
    "miss_resp_denied": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.__Vtogcov__io_missResp_bits_denied"
    ),
    "s1_valid": _aliases("Frontend_top.Frontend.inner_icache.mainPipe.s1_valid"),
    "s1_req1_valid": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_1_valid"
    ),
    "s1_req0_addr": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_0_vAddr_0_addr"
    ),
    "s1_req1_addr": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_1_vAddr_0_addr"
    ),
    "s1_ftq0_flag": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_0_ftqIdx_flag"
    ),
    "s1_ftq0_value": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_0_ftqIdx_value"
    ),
    "s1_ftq1_flag": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_1_ftqIdx_flag"
    ),
    "s1_ftq1_value": _aliases(
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_1_ftqIdx_value"
    ),
    "s1_exception_0": MAIN_EXCEPTIONS[0],
    "s1_exception_1": MAIN_EXCEPTIONS[1],
    "s1_mmio": _aliases("Frontend_top.Frontend.inner_icache.mainPipe.s1_isMmio"),
    "backend_redirect": _aliases(
        "Frontend_top.Frontend.inner_ftq.backendRedirect_valid"
    ),
    "s1_fire": _aliases("Frontend_top.Frontend.inner_icache.mainPipe.s1_fire"),
    "s1_flush": _aliases("Frontend_top.Frontend.inner_icache.mainPipe.s1_flush"),
    "to_ifu_valid": (
        "Frontend_top.Frontend._inner_icache_io_toIfu_req_valid",
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_valid",
        "TOP.Frontend_top.Frontend._inner_icache_io_toIfu_req_valid",
    ),
    "to_ifu_exception_0": TO_IFU_EXCEPTIONS[0],
    "to_ifu_exception_1": TO_IFU_EXCEPTIONS[1],
}


for _index in range(4):
    _MAINPIPE_SIGNALS[f"should_fetch_{_index}"] = _aliases(
        f"Frontend_top.Frontend.inner_icache.mainPipe.s1_shouldFetch_{_index}"
    )
    _MAINPIPE_SIGNALS[f"mshr_valid_{_index}"] = _aliases(
        f"Frontend_top.Frontend.inner_icache.mainPipe.s1_mshrValid_{_index // 2}_{_index % 2}"
    )
    _MAINPIPE_SIGNALS[f"mshr_valid_reg_{_index}"] = _aliases(
        f"Frontend_top.Frontend.inner_icache.mainPipe.s1_mshrValidReg_{_index // 2}_{_index % 2}"
    )


def _read_mainpipe_signal(env, names: Sequence[str]) -> int | None:
    """Resolve aliases anew; unlike generic lookup this does not cache misses."""
    for name in names:
        try:
            signal = getattr(env.dut, str(name), None)
            if signal is None:
                getter = getattr(env.dut, "GetInternalSignal", None)
                signal = getter(str(name)) if callable(getter) else None
            value = None if signal is None else getattr(signal, "value", None)
            if value is not None:
                return int(value)
        except Exception:
            continue
    return None


def _require_mainpipe_signal(env, names: Sequence[str]) -> int:
    value = _read_mainpipe_signal(env, names)
    assert value is not None, {"missing_internal_signals": list(names)}
    return int(value)


def _sample_mainpipe(env, cycle: int) -> dict:
    return {
        "cycle": int(cycle),
        **{
            name: _require_mainpipe_signal(env, aliases)
            for name, aliases in _MAINPIPE_SIGNALS.items()
        },
    }


def register_mainpipe_observer(env) -> list[dict]:
    # Validate before initialization/reset invokes the observer for the first time.
    for aliases in _MAINPIPE_SIGNALS.values():
        _require_mainpipe_signal(env, aliases)
    samples: list[dict] = []

    def observe(cycle, active_env) -> None:
        samples.append(_sample_mainpipe(active_env, cycle))

    env.register_cycle_observer(observe)
    return samples


def _dual_tag(sample: dict) -> tuple[tuple[int, int], tuple[int, int]]:
    return (
        (int(sample["s1_ftq0_flag"]), int(sample["s1_ftq0_value"])),
        (int(sample["s1_ftq1_flag"]), int(sample["s1_ftq1_value"])),
    )


def completed_dual_transactions(samples: Sequence[dict]) -> list[dict]:
    completed: list[dict] = []
    active: dict | None = None
    last_started_tag = None
    for sample in samples:
        if (
            sample["s1_valid"] == 1
            and sample["s1_req1_valid"] == 1
            and (sample["s1_exception_0"] == 0)
            and (sample["s1_exception_1"] == 0)
            and (sample["s1_mmio"] == 0)
            and (sample["s1_flush"] == 0)
        ):
            tag = _dual_tag(sample)
            if tag != last_started_tag:
                misses = [bool(sample[f"should_fetch_{index}"]) for index in range(4)]
                req0_miss = bool(misses[0] or misses[1])
                req1_miss = bool(misses[2] or misses[3])
                active = {
                    "tag": tag,
                    "start_cycle": int(sample["cycle"]),
                    "req0_addr": int(sample["s1_req0_addr"]) << 1,
                    "req1_addr": int(sample["s1_req1_addr"]) << 1,
                    "pattern": {
                        (False, False): "hit_hit",
                        (False, True): "hit_miss",
                        (True, False): "miss_hit",
                        (True, True): "miss_miss",
                    }[req0_miss, req1_miss],
                    "required": misses,
                    "raw_cycles": [None] * 4,
                    "registered_cycles": [None] * 4,
                }
                last_started_tag = tag
        if active is None:
            continue
        for index, required in enumerate(active["required"]):
            if not required:
                continue
            if (
                sample[f"mshr_valid_{index}"] == 1
                and active["raw_cycles"][index] is None
            ):
                active["raw_cycles"][index] = int(sample["cycle"])
            if (
                sample[f"mshr_valid_reg_{index}"] == 1
                and active["registered_cycles"][index] is None
            ):
                active["registered_cycles"][index] = int(sample["cycle"])
        if (
            sample["s1_valid"] == 1
            and sample["s1_req1_valid"] == 1
            and (_dual_tag(sample) == active["tag"])
            and (sample["s1_fire"] == 1)
        ):
            active["fire_cycle"] = int(sample["cycle"])
            active["registered_exact"] = all(
                (
                    not required
                    or (
                        active["raw_cycles"][index] is not None
                        and active["registered_cycles"][index]
                        == active["raw_cycles"][index] + 1
                    )
                    for index, required in enumerate(active["required"])
                )
            )
            completed.append(active)
            active = None
    return completed


# Runtime controls and program loading.


def randomize_resolve_delays(rng: Random, env) -> tuple:
    """Randomize backend resolve delays and return (min_d, max_d)."""
    min_d = rng.randint(1, 6)
    max_d = rng.randint(min_d + 1, min_d + 12)
    env.backend_model.resolve_min_delay = min_d
    env.backend_model.resolve_max_delay = max_d
    return (min_d, max_d)


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


def run_until(env, predicate: Callable[[], bool], *, max_cycles: int) -> bool:
    """Boolean polling with a final check, including a zero-cycle window."""
    for _ in range(int(max_cycles)):
        if predicate():
            return True
        env.step(1)
    return bool(predicate())


def load_nop_program(
    env,
    words: int = 256,
    base_addr: int = BPU_TRAINING_BASE,
    *,
    step_cycles: int = 1,
) -> None:
    # Preserve the historical one-cycle default; runtime-only loads can opt out.
    run_initialization_steps(
        env,
        (
            LoadProgram(
                ProgramImage(build_nop_program(words), base_addr),
                step_cycles=step_cycles,
            ),
        ),
    )


def warmup_commits(env, target_count: int = 4, max_cycles: int = 3000) -> int:
    return RunUntilCommitSequence(
        target=CommitTarget(target_count=int(target_count), max_cycles=int(max_cycles)),
    ).run(env)


# Startup steps and declarative initialization plans.

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


def _read_bpu_training_signal(env, key: str) -> int:
    names = _BPU_TRAINING_SIGNALS[key]
    recorder = getattr(env, "functional_coverage", None)
    if recorder is not None:
        value = recorder._read_first_dut_signal(env.dut, names)
    else:
        value = _read_generic_dut_signal(env, names)
    assert value is not None, {
        "reason": "required directed-flow observation is unavailable",
        "label": str(key),
        "candidates": list(names),
    }
    return int(value)


@dataclass(frozen=True)
class WarmBpuTraining:
    max_cycles: int | None = None

    def run(self, env) -> dict:
        recorder = getattr(env, "functional_coverage", None)
        max_cycles = self.max_cycles
        if max_cycles is None:
            max_cycles = cycle_limit("TB_TWO_FETCH_TRAIN_MAX_CYCLES", 4000)
        for _ in range(max(0, int(max_cycles))):
            env.step(1)
            dual_request_fire = (
                _read_bpu_training_signal(env, "ftq_valid") == 1
                and _read_bpu_training_signal(env, "ftq_ready") == 1
                and _read_bpu_training_signal(env, "ftq_req1_valid") == 1
            )
            if (
                int(env.backend_model.get_stats().get("commit_count", 0)) >= 1
                and int(env.icache_agent.get_stats().get("req_count", 0)) >= 4
                and (
                    recorder is None
                    or (
                        recorder.key_hit("ifu_cfi_decode_type", "jal")
                        and recorder.key_hit("two_fetch_ftq_eligibility", "eligible_dual")
                        and recorder.key_hit("two_fetch_delivery", "dual_fire")
                    )
                )
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


@dataclass(frozen=True)
class NotifySatpChanged:
    """Preserve the optional one-cycle notification used by SV39 startup."""

    def run(self, env) -> None:
        try:
            env.dut.io_tlbCsr_satp_changed.value = 1
            env.step(1)
            env.dut.io_tlbCsr_satp_changed.value = 0
        except Exception:
            # Existing SV39 helpers support DUTs without this optional pin.
            pass


@dataclass(frozen=True)
class SetIcachePrefetch:
    enabled: bool

    def run(self, env) -> None:
        env.csr_ctrl_if.io_csrCtrl_pf_ctrl_l1I_pf_enable.value = int(self.enabled)


@dataclass(frozen=True)
class SetBackendCanAccept:
    enabled: bool

    def run(self, env) -> None:
        env.backend_model.set_can_accept(int(self.enabled))


def make_program_init_config(
    env_config: EnvConfig,
    *,
    reset_vector: int,
    images: tuple[ProgramImage, ...] = (),
    icache_parameters: Mapping[str, object] | None = None,
    before_reset: tuple[InitStep, ...] = (),
    before_release: tuple[InitStep, ...] = (),
    after_reset: tuple[InitStep, ...] | None = None,
    bare_mode: bool = True,
    reset_cycles: int = 20,
) -> FrontendInitConfig:
    """Load images, configure the agent, then execute the specified boot.

    The default post-reset steps explicitly clear the monitor and restore its
    expected PC. Supply after_reset=() to retain evidence, or an explicit tuple
    when a scenario requires a different ordering. Extra before_reset steps run
    after image loading and agent configuration; LoadProgram(step_cycles=...) can
    instead be supplied there for loaders with a historical clock edge.
    """
    prepare = tuple(LoadProgram(image) for image in images)
    if icache_parameters is not None:
        prepare += (ConfigureAgent("icache", dict(icache_parameters)),)
    if after_reset is None:
        after_reset = (ClearMonitor(), SetExpectedPc(int(reset_vector)))
    return FrontendInitConfig(
        env_config=env_config,
        reset_vector=int(reset_vector),
        bare_mode=bare_mode,
        reset_cycles=reset_cycles,
        before_reset=prepare + tuple(before_reset),
        before_release=tuple(before_release),
        after_reset=tuple(after_reset),
    )


def make_nop_init_config(
    env_config: EnvConfig,
    base: int,
    *,
    latency: int,
    seed: int,
    words: int = 256,
    hit_latency: int = 1,
    miss_rate: float = 1.0,
    reset_vector: int | None = None,
    before_reset: tuple[InitStep, ...] = (),
    after_reset: tuple[InitStep, ...] | None = None,
) -> FrontendInitConfig:
    return make_program_init_config(
        env_config,
        reset_vector=int(base) if reset_vector is None else int(reset_vector),
        images=(ProgramImage(build_nop_program(words), int(base)),),
        icache_parameters={
            "hit_latency": int(hit_latency),
            "miss_latency": int(latency),
            "miss_rate": miss_rate,
            "seed": int(seed),
        },
        before_reset=before_reset,
        after_reset=after_reset,
    )


def make_cacheable_init_config(
    env_config: EnvConfig, base: int, *, latency: int
) -> FrontendInitConfig:
    return make_nop_init_config(
        env_config, base, latency=latency, hit_latency=latency, seed=0x6219
    )


def make_bpu_training_init_config(
    env_config: EnvConfig, *, warmup: bool = False
) -> FrontendInitConfig:
    return make_program_init_config(
        env_config,
        reset_vector=BPU_TRAINING_BASE,
        images=(ProgramImage(build_bpu_training_program(), BPU_TRAINING_BASE),),
        after_reset=(ClearMonitor(), SetExpectedPc(BPU_TRAINING_BASE))
        + ((WarmBpuTraining(),) if warmup else ()),
    )


def make_bpu_s3_init_config(
    env_config: EnvConfig, *, seed: int = 0x6605
) -> FrontendInitConfig:
    config = make_bpu_training_init_config(env_config, warmup=True)

    return replace(
        config,
        after_reset=config.after_reset
        + (
            ConfigureAgent(
                "icache",
                {
                    "hit_latency": 1,
                    "miss_latency": 32,
                    "miss_rate": 0.0,
                    "seed": int(seed),
                },
            ),
            SetBpCtrl(),
        ),
    )


def make_dual_refill_init_config(
    env_config: EnvConfig, *, seed: int = 0x2524
) -> FrontendInitConfig:
    return make_program_init_config(
        env_config,
        reset_vector=DUAL_FETCH_BASE,
        images=(ProgramImage(build_dual_fetch_program(), DUAL_FETCH_BASE),),
        icache_parameters={
            "hit_latency": 8,
            "miss_latency": 8,
            "miss_rate": 1.0,
            "seed": int(seed),
        },
    )


def make_sv39_init_config(
    env_config: EnvConfig,
    *,
    reset_vector: int,
    before_reset: tuple[InitStep, ...] = (),
) -> FrontendInitConfig:
    return make_program_init_config(
        env_config,
        reset_vector=reset_vector,
        bare_mode=False,
        before_reset=before_reset,
        after_reset=(NotifySatpChanged(), ClearMonitor(), SetExpectedPc(reset_vector)),
    )


def make_pbmt_redirect_init_config(
    env_config: EnvConfig,
    *,
    reset_vector: int,
    uncache_latency: int = 16,
    icache_latency: int = 64,
    agent_seed: int = 4,
) -> FrontendInitConfig:
    return make_sv39_init_config(
        env_config,
        reset_vector=reset_vector,
        before_reset=(
            ConfigureAgent(
                "uncache",
                {
                    "latency": uncache_latency,
                    "mmio_latency": uncache_latency,
                },
            ),
            ConfigureAgent(
                "icache",
                {
                    "hit_latency": icache_latency,
                    "miss_latency": icache_latency,
                    "miss_rate": 0.0,
                    "seed": agent_seed,
                },
            ),
        ),
    )


def make_fetch_loop_init_config(
    env_config: EnvConfig, base: int, *, target_offset: int, latency: int
) -> FrontendInitConfig:
    return make_program_init_config(
        env_config,
        reset_vector=base,
        images=(
            ProgramImage(
                build_fetch_loop_program(target_offset=target_offset), int(base)
            ),
        ),
        icache_parameters={
            "hit_latency": int(latency),
            "miss_latency": int(latency),
            "miss_rate": 1.0,
            "seed": 0x6608 + int(target_offset),
        },
    )


def make_overlap2_init_config(env_config: EnvConfig, base: int) -> FrontendInitConfig:
    return make_program_init_config(
        env_config,
        reset_vector=int(base) + 0x60,
        images=(ProgramImage(build_overlap2_program(), int(base)),),
        icache_parameters={
            "hit_latency": 1,
            "miss_latency": 16,
            "miss_rate": 1.0,
            "seed": 0x6779,
        },
    )


def make_missunit_init_config(
    env_config: EnvConfig,
    *,
    latency: int = 4096,
    words: int = 4096,
    prefetch_enabled: bool = True,
    backend_can_accept: bool = True,
) -> FrontendInitConfig:
    base = 0x8010_0000
    return make_nop_init_config(
        env_config,
        base,
        latency=latency,
        words=words,
        seed=0x686,
        before_reset=(SetIcachePrefetch(prefetch_enabled),),
        after_reset=(
            SetBackendCanAccept(backend_can_accept),
            ClearMonitor(),
            SetExpectedPc(base),
        ),
    )


def make_live_cfi_init_config(env_config: EnvConfig, base: int) -> FrontendInitConfig:
    # Decoded JAL x0,+4 supplies a legal identity and an exact resolve target.
    return make_program_init_config(
        env_config,
        reset_vector=base,
        images=(ProgramImage(encode_jal(0, 4).to_bytes(4, "little") * 16, int(base)),),
    )


def make_trained_loop_init_config(
    env_config: EnvConfig,
    base: int,
    *,
    seed: int,
    warmup: bool = False,
) -> FrontendInitConfig:
    return make_program_init_config(
        env_config,
        reset_vector=base,
        images=(ProgramImage(build_bpu_training_program(), int(base)),),
        icache_parameters={
            "hit_latency": 1,
            "miss_latency": 24,
            "miss_rate": 1.0,
            "seed": int(seed),
        },
        after_reset=(ClearMonitor(), SetExpectedPc(base))
        + ((WarmBpuTraining(),) if warmup else ()),
    )


def make_same_set_jump_init_config(
    env_config: EnvConfig,
    base: int,
    *,
    stride: int,
    line_count: int,
) -> FrontendInitConfig:
    return make_program_init_config(
        env_config,
        reset_vector=base,
        images=(
            ProgramImage(
                build_same_set_jump_program(base, stride=stride, line_count=line_count),
                int(base),
            ),
        ),
        icache_parameters={
            "hit_latency": 1,
            "miss_latency": 48,
            "miss_rate": 1.0,
            "seed": 0x731,
        },
        # This scenario deliberately retains monitor evidence across the reset.
        after_reset=(SetBpCtrl(), SetBackendCanAccept(True)),
    )


def make_mixed_sources_init_config(
    env_config: EnvConfig,
    *,
    idle_base: int,
    target: int,
) -> FrontendInitConfig:
    return make_program_init_config(
        env_config,
        reset_vector=idle_base,
        images=(
            ProgramImage(build_idle_loop_program(), int(idle_base)),
            ProgramImage(build_cross_line_probe_program(), int(target)),
        ),
        icache_parameters={
            "hit_latency": 1,
            "miss_latency": 48,
            "miss_rate": 1.0,
            "seed": 0x1139,
        },
        after_reset=(
            ClearMonitor(),
            SetExpectedPc(idle_base),
            SetBpCtrl(0, 0, 0, 0, 0, 0),
            SetBackendCanAccept(False),
            SetIcachePrefetch(True),
        ),
    )

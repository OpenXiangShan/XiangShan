"""Declarative startup plans for directed ICache tests.

Factories never access an env or advance the DUT. Execute their result with
env.runtime.frontend_init.initialize_frontend. Runtime stimulus and assertions
remain in the scenarios; startup-specific waits implement the InitStep protocol.
"""

from __future__ import annotations

from collections.abc import Mapping
from dataclasses import dataclass, replace

from env.runtime.frontend_init import (
    ClearMonitor,
    ConfigureAgent,
    FrontendInitConfig,
    InitStep,
    LoadProgram,
    ProgramImage,
    SetBpCtrl,
    SetExpectedPc,
)
from env.support.env_config import EnvConfig
from tests.py.support.dut_support import cycle_limit

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


def _read_bpu_training_signal(recorder, key: str) -> int:
    names = _BPU_TRAINING_SIGNALS[key]
    value = recorder._read_first_dut_signal(recorder.env.dut, names)
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
        assert (
            recorder is not None
        ), "directed flow tests require the canonical funcov recorder"
        assert getattr(recorder, "env", None) is env
        max_cycles = self.max_cycles
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

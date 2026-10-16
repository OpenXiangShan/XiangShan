"""Shared, ordered initialization for Frontend DUT tests.

Fixtures install observers before calling this module. Plans contain immutable
instructions; execution results belong to the environment, never to the plan.
Hardware reset remains available separately as a testcase stimulus.
"""

from __future__ import annotations

import copy
import os
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Callable, Mapping, Protocol, TYPE_CHECKING

from ..core.transactions import ProgramImage
from ..model.golden_trace import GoldenTrace
from ..sequences.program_sequences import LoadProgramFileSequence, LoadProgramSequence
from ..support.env_config import BAREMODE_ENV_CONFIG, EnvConfig, SV39_ENV_CONFIG
from ..support.pmp_pma import PmpPmaConfig

if TYPE_CHECKING:
    from ..core.frontend_env import FrontendEnv


class InitStep(Protocol):
    """An initialization operation, including existing reusable Sequences."""

    def run(self, env: FrontendEnv) -> Any: ...


LoadProgram = LoadProgramSequence
LoadProgramFile = LoadProgramFileSequence


@dataclass(frozen=True)
class WaitCycles:
    """Advance time at an explicit preparation boundary without a reset."""

    cycles: int

    def run(self, env: FrontendEnv) -> None:
        cycles = int(self.cycles)
        if cycles < 0:
            raise ValueError("wait cycles must be non-negative")
        if cycles:
            env.step(cycles)


@dataclass(frozen=True)
class ConfigureAgent:
    agent: str
    parameters: Mapping[str, Any] = field(default_factory=dict)

    def run(self, env: FrontendEnv) -> None:
        if self.agent not in {"icache", "uncache", "ptw"}:
            raise ValueError(f"unsupported initialization agent: {self.agent}")
        getattr(env, f"{self.agent}_agent").configure(**dict(self.parameters))


@dataclass(frozen=True)
class ClearMonitor:
    """Explicitly discard monitor errors, observations, and counters."""

    def run(self, env: FrontendEnv) -> None:
        env.monitor.clear()


@dataclass(frozen=True)
class SetExpectedPc:
    pc: int

    def run(self, env: FrontendEnv) -> None:
        env.monitor.set_expected_pc(int(self.pc))


@dataclass(frozen=True)
class SetBpCtrl:
    ubtb_enable: int = 1
    abtb_enable: int = 1
    mbtb_enable: int = 1
    tage_enable: int = 1
    sc_enable: int = 1
    ittage_enable: int = 1

    def run(self, env: FrontendEnv) -> None:
        env.set_bp_ctrl_enable(
            ubtb_enable=int(self.ubtb_enable),
            abtb_enable=int(self.abtb_enable),
            mbtb_enable=int(self.mbtb_enable),
            tage_enable=int(self.tage_enable),
            sc_enable=int(self.sc_enable),
            ittage_enable=int(self.ittage_enable),
        )


@dataclass(frozen=True)
class MapPage:
    vpn: int
    ppn: int
    parameters: Mapping[str, Any] = field(default_factory=dict)
    stage: int = 1

    def run(self, env: FrontendEnv) -> None:
        if self.stage not in {1, 2}:
            raise ValueError("page mapping stage must be 1 or 2")
        method = (
            env.page_table.map_page
            if self.stage == 1
            else env.page_table.map_stage2_page
        )
        method(int(self.vpn), int(self.ppn), **dict(self.parameters))


@dataclass(frozen=True)
class ClearPageTable:
    def run(self, env: FrontendEnv) -> None:
        env.page_table.clear()


@dataclass(frozen=True)
class SetMemoryRegion:
    start: int
    end: int
    mmio: bool = True

    def run(self, env: FrontendEnv) -> None:
        if int(self.end) <= int(self.start):
            raise ValueError("memory region end must be greater than its start")
        region = (int(self.start), int(self.end))
        if self.mmio:
            env.memory.mmio_ranges.append(region)
        else:
            env.memory.mmio_ranges[:] = [
                item for item in env.memory.mmio_ranges if item != region
            ]


@dataclass(frozen=True)
class WritePmp:
    index: int
    config: PmpPmaConfig
    addr: int
    size: int | None = None
    settle_cycles: int = 0

    def run(self, env: FrontendEnv) -> dict:
        return env.write_pmp_entry(
            self.index,
            self.config,
            self.addr,
            size=self.size,
            settle_cycles=self.settle_cycles,
        )


@dataclass(frozen=True)
class WritePma:
    index: int
    config: PmpPmaConfig
    addr: int
    size: int | None = None
    settle_cycles: int = 0

    def run(self, env: FrontendEnv) -> dict:
        return env.write_pma_entry(
            self.index,
            self.config,
            self.addr,
            size=self.size,
            settle_cycles=self.settle_cycles,
        )


@dataclass(frozen=True)
class WriteCsr:
    addr: int
    data: int
    settle_cycles: int = 0

    def run(self, env: FrontendEnv) -> dict:
        return env.write_distributed_csr(
            self.addr, self.data, settle_cycles=self.settle_cycles
        )


@dataclass(frozen=True)
class LoadGoldenTrace:
    """Attach a fresh trace each time, so plans never share a mutable cursor."""

    path: str | Path | None = None
    factory: Callable[[], GoldenTrace] | None = None
    start_index: int = 0

    def run(self, env: FrontendEnv) -> int:
        if (self.path is None) == (self.factory is None):
            raise ValueError("provide exactly one golden trace path or factory")
        if int(self.start_index) < 0:
            raise ValueError("golden trace start_index must be non-negative")
        if self.path is not None:
            return env.load_golden_trace_file(
                str(self.path), start_index=int(self.start_index)
            )
        trace = copy.deepcopy(self.factory())
        if not isinstance(trace, GoldenTrace):
            raise TypeError("golden trace factory must return GoldenTrace")
        env.backend_model.set_golden_trace(trace, start_cursor=int(self.start_index))
        return len(trace.entries)


@dataclass(frozen=True)
class SetBackendPolicy:
    mode: str | None = None
    explicit_injection: bool | None = None
    reason: str = "Frontend initialization"
    parameters: Mapping[str, int] = field(default_factory=dict)

    def run(self, env: FrontendEnv) -> None:
        allowed = {
            f"{stage}_{bound}_delay"
            for stage in ("resolve", "commit", "redirect")
            for bound in ("min", "max")
        }
        for name, value in self.parameters.items():
            if name not in allowed or int(value) < 0:
                raise ValueError(
                    f"invalid backend initialization parameter: {name}={value}"
                )
        for name, value in self.parameters.items():
            setattr(env.backend_model, name, int(value))
        if self.mode is not None:
            env.backend_model.set_backend_mode(self.mode)
        if self.explicit_injection is not None:
            env.backend_model.set_explicit_injection_enabled(
                self.explicit_injection, reason=self.reason
            )


@dataclass(frozen=True)
class AttachGoldenTrace:
    """Attach a testcase-owned trace, preserving references used by its checker.

    Shared fixture plans should use LoadGoldenTrace(path=.../factory=...) to
    obtain a new cursor for each test. This adapter preserves runtime APIs.
    """

    trace: GoldenTrace | None
    start_cursor: int = 0

    def run(self, env: FrontendEnv) -> int:
        env.backend_model.set_golden_trace(self.trace, start_cursor=self.start_cursor)
        return 0 if self.trace is None else len(self.trace.entries)


@dataclass(frozen=True)
class ConfigureTrigger:
    """Write explicitly supplied, required trigger input names from the DUT ABI."""

    values: Mapping[str, int]
    settle_cycles: int = 0

    def run(self, env: FrontendEnv) -> None:
        if int(self.settle_cycles) < 0:
            raise ValueError("trigger settle_cycles must be non-negative")
        signals = []
        for name, value in self.values.items():
            if not str(name).startswith("io_csrCtrl_frontend_trigger_"):
                raise ValueError(f"not a FrontendTrigger input: {name}")
            signal = getattr(env.dut, str(name), None)
            if signal is None:
                raise ValueError(f"missing FrontendTrigger DUT input: {name}")
            signals.append((signal, int(value)))
        for signal, value in signals:
            signal.value = value
        if self.settle_cycles:
            env.step(self.settle_cycles)


@dataclass(frozen=True)
class PrepareTranslationScenario:
    scenario: Any
    arm: bool = True
    page_indexes: tuple[int, ...] | Callable[[Any], Any] | None = None

    def run(self, env: FrontendEnv) -> Any:
        from ..sequences.translation_scenarios import TranslationScenarioBuilder

        state = TranslationScenarioBuilder(env).build(self.scenario)
        if self.arm:
            indexes = (
                self.page_indexes(state)
                if callable(self.page_indexes)
                else self.page_indexes
            )
            env.arm_translation_scenario(state, page_indexes=indexes)
        return state


@dataclass(frozen=True)
class ArmTranslationScenario:
    """Activate a testcase-owned state without rebuilding or clearing evidence."""

    state: Any
    page_indexes: tuple[int, ...] | None = None
    expect_ptw: bool = True

    def run(self, env: FrontendEnv) -> dict:
        return env.arm_translation_scenario(
            self.state,
            page_indexes=self.page_indexes,
            expect_ptw=self.expect_ptw,
        )


@dataclass(frozen=True)
class RunSequence:
    """Adapt an existing initializer without embedding DUT writes in the testcase."""

    sequence: InitStep

    def run(self, env: FrontendEnv) -> Any:
        return self.sequence.run(env)


@dataclass(frozen=True)
class InitCallback:
    """Compatibility adapter for existing reset-release setup/checker callbacks."""

    callback: Callable[[FrontendEnv], Any]

    def run(self, env: FrontendEnv) -> Any:
        return self.callback(env)


@dataclass(frozen=True)
class FrontendInitConfig:
    env_config: EnvConfig = field(default_factory=EnvConfig)
    reset_vector: int | None = None
    bare_mode: bool | None = None
    reset_cycles: int | None = None
    before_reset: tuple[InitStep, ...] = ()
    before_release: tuple[InitStep, ...] = ()
    after_reset: tuple[InitStep, ...] = ()
    # Runtime-generated streams need an assembled env before declaring a plan.
    # Its startup is then performed explicitly through initialize_frontend.
    defer: bool = False


@dataclass
class InitStepResult:
    phase: str
    operation: str
    start_cycle: int
    end_cycle: int
    value: Any = None


@dataclass
class FrontendInitResult:
    reset_vector: int
    mode: str
    reset_cycles: int
    start_cycle: int
    end_cycle: int
    steps: list[InitStepResult] = field(default_factory=list)
    completed: bool = False

    @property
    def program_bytes(self) -> int:
        return sum(
            int(step.value)
            for step in self.steps
            if step.value is not None
            and step.operation
            in {
                "LoadProgramSequence",
                "LoadProgramFileSequence",
            }
        )

    @property
    def trace_entries(self) -> int:
        return sum(
            int(step.value)
            for step in self.steps
            if step.value is not None
            and step.operation
            in {"LoadGoldenTrace", "LoadGoldenTraceSequence", "AttachGoldenTrace"}
        )

    def metadata(self) -> dict:
        return {
            "reset_vector": self.reset_vector,
            "mode": self.mode,
            "reset_cycles": self.reset_cycles,
            "start_cycle": self.start_cycle,
            "end_cycle": self.end_cycle,
            "completed": self.completed,
            "program_bytes": self.program_bytes,
            "trace_entries": self.trace_entries,
            "steps": [
                {
                    "phase": step.phase,
                    "operation": step.operation,
                    "start_cycle": step.start_cycle,
                    "end_cycle": step.end_cycle,
                }
                for step in self.steps
            ],
        }


def _integer_setting(explicit: int | None, name: str, default: int) -> int:
    if explicit is not None:
        return int(explicit)
    raw = os.getenv(name, "").strip()
    if not raw:
        return int(default)
    try:
        return int(raw, 0)
    except ValueError as exc:
        raise ValueError(f"{name} must be a valid integer, got: {raw}") from exc


def _run_steps(
    env: FrontendEnv,
    steps: tuple[InitStep, ...],
    phase: str,
    result: FrontendInitResult,
) -> None:
    for step in steps:
        cycle = int(getattr(env, "current_cycle", 0))
        operation = step.sequence if isinstance(step, RunSequence) else step
        record = InitStepResult(phase, type(operation).__name__, cycle, cycle)
        result.steps.append(record)
        try:
            record.value = step.run(env)
        finally:
            record.end_cycle = int(getattr(env, "current_cycle", 0))


def initialize_frontend(
    env: FrontendEnv, config: FrontendInitConfig | None = None
) -> FrontendInitResult:
    """Initialize an assembled env after its observers have been installed.

    Explicit parameters take precedence over TB_RESET_VECTOR/TB_RESET_CYCLES,
    then the constructed env's SequenceConfig. Existing envs retain their
    construction config; runtime changes are expressed by initialization steps.
    Neither errors nor accumulated coverage are cleared. Failures propagate.
    """
    config = config or FrontendInitConfig(env_config=env.config)
    reset_vector = _integer_setting(
        config.reset_vector, "TB_RESET_VECTOR", env.config.sequence.reset_vector
    )
    reset_cycles = _integer_setting(
        config.reset_cycles, "TB_RESET_CYCLES", env.config.sequence.reset_cycles
    )
    if reset_vector < 0 or reset_vector % 2:
        raise ValueError("reset_vector must be a non-negative halfword-aligned address")
    if reset_cycles < 0:
        raise ValueError("reset_cycles must be non-negative")
    mode = (
        env.config.ptw.mode
        if config.bare_mode is None
        else ("bare" if config.bare_mode else "sv39")
    )
    if mode not in {"bare", "sv39"}:
        raise ValueError(f"unsupported frontend init mode: {mode}")
    for phase in (config.before_reset, config.before_release, config.after_reset):
        if any(not callable(getattr(step, "run", None)) for step in phase):
            raise TypeError("initialization steps must provide run(env)")

    result = FrontendInitResult(
        reset_vector, mode, reset_cycles, int(env.current_cycle), int(env.current_cycle)
    )
    env.init_result = result
    history = vars(env).setdefault("init_history", [])
    history.append(result)
    try:
        _run_steps(env, config.before_reset, "before_reset", result)
        ptw_config = (
            env.config.ptw
            if env.config.ptw.mode == mode
            else (BAREMODE_ENV_CONFIG.ptw if mode == "bare" else SV39_ENV_CONFIG.ptw)
        )
        env.logger.info(
            "initialize env: reset_vector=0x%x mode=%s reset_cycles=%d",
            reset_vector,
            mode,
            reset_cycles,
        )
        env._write(env.clock_reset.io_reset_vector_addr, reset_vector >> 1)
        env.page_table.set_mode(mode)
        for field_name in (
            "priv_virt",
            "priv_imode",
            "satp_mode",
            "satp_asid",
            "satp_ppn",
            "vsatp_mode",
            "vsatp_asid",
            "vsatp_ppn",
            "hgatp_mode",
            "hgatp_vmid",
            "hgatp_ppn",
        ):
            env._write(
                getattr(env.tlb_csr_if, f"io_tlbCsr_{field_name}"),
                int(getattr(ptw_config, field_name)),
            )
        env.reset(
            reset_cycles,
            before_release=(
                (
                    lambda: _run_steps(
                        env, config.before_release, "before_release", result
                    )
                )
                if config.before_release
                else None
            ),
        )
        env.flow_checker.set_expected_pc(reset_vector)
        _run_steps(env, config.after_reset, "after_reset", result)
        result.completed = True
    finally:
        result.end_cycle = int(env.current_cycle)
        env._emit_event(
            "initialization.complete" if result.completed else "initialization.failed",
            result.metadata(),
        )
    return result


def run_initialization_steps(
    env: FrontendEnv, steps: tuple[InitStep, ...], *, phase: str = "prepare"
) -> list[InitStepResult]:
    """Run preparation for an already initialized stream without another reset.

    Legacy program generators and warmup helpers use this same module; this
    deliberately preserves their original load/step/reset ordering.
    """
    cycle = int(getattr(env, "current_cycle", 0))
    result = FrontendInitResult(
        0, getattr(getattr(env, "page_table", None), "mode", "unknown"), 0, cycle, cycle
    )
    history = vars(env).setdefault("init_preparation_history", [])
    try:
        _run_steps(env, steps, phase, result)
    finally:
        history.extend(result.steps)
    return result.steps


__all__ = [
    "FrontendInitConfig",
    "FrontendInitResult",
    "InitStep",
    "InitStepResult",
    "ProgramImage",
    "LoadProgram",
    "LoadProgramFile",
    "LoadGoldenTrace",
    "WaitCycles",
    "ConfigureAgent",
    "ClearMonitor",
    "SetExpectedPc",
    "SetBpCtrl",
    "MapPage",
    "ClearPageTable",
    "SetMemoryRegion",
    "WritePmp",
    "WritePma",
    "WriteCsr",
    "ConfigureTrigger",
    "SetBackendPolicy",
    "AttachGoldenTrace",
    "PrepareTranslationScenario",
    "ArmTranslationScenario",
    "RunSequence",
    "InitCallback",
    "initialize_frontend",
    "run_initialization_steps",
]

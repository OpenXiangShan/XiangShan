from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import Callable

from ..transactions import CommitTarget, GoldenTraceSource


def _default_format_optional_pc(value) -> str:
    if value is None:
        return "none"
    return f"0x{int(value):x}"


def _default_current_golden_pc(trace, backend_model):
    getter = getattr(backend_model, "current_golden_pc", None)
    if callable(getter):
        return getter()
    cur = trace.peek()
    if cur is None:
        return None
    return int(cur.pc)


def _pending_work_count(backend_model) -> int:
    getter = getattr(backend_model, "pending_work_count", None)
    if callable(getter):
        return int(getter())
    has_pending_work = getattr(backend_model, "has_pending_work", None)
    if callable(has_pending_work):
        return 1 if has_pending_work() else 0
    pending_events = getattr(backend_model, "pending_events", [])
    return len(pending_events)


def _golden_completion_pending_work_count(backend_model) -> int:
    getter = getattr(backend_model, "golden_completion_pending_work_count", None)
    if callable(getter):
        return int(getter())
    return _pending_work_count(backend_model)


@dataclass(frozen=True)
class LoadGoldenTraceSequence:
    source: GoldenTraceSource
    step_cycles: int = 0

    def run(self, env) -> int:
        count = int(env.load_golden_trace_file(self.source.path))
        if int(self.step_cycles) > 0:
            env.step(int(self.step_cycles))
        return count


@dataclass(frozen=True)
class RunUntilCommitSequence:
    target: CommitTarget

    def run(self, env) -> int:
        return int(env.backend_model.wait_for_commits(self.target.target_count, self.target.max_cycles))


@dataclass(frozen=True)
class RunUntilGoldenTraceCompleteSequence:
    max_cycles: int = 10000
    progress_interval: int = 0
    stall_snapshot_interval: int = 0
    stagnant_cycles_limit: int = 0
    logger: logging.Logger | None = None
    current_golden_pc_getter: Callable | None = None
    format_optional_pc: Callable | None = None
    stall_snapshot_capture: Callable | None = None
    stall_snapshot_formatter: Callable | None = None

    def run(self, env) -> "RunUntilGoldenTraceResult":
        trace = getattr(env.backend_model, "golden_trace", None)
        if trace is None:
            raise ValueError("golden trace must be loaded before waiting for golden completion")

        logger = self.logger
        current_golden_pc_getter = self.current_golden_pc_getter or _default_current_golden_pc
        format_optional_pc = self.format_optional_pc or _default_format_optional_pc
        total_entries = int(len(trace.entries))
        max_cycles = int(self.max_cycles)
        progress_interval = int(self.progress_interval)
        stall_snapshot_interval = int(self.stall_snapshot_interval)
        stagnant_cycles_limit = int(self.stagnant_cycles_limit)
        cycles_run = 0
        last_cursor = int(trace.cursor)
        stagnant_cycles = 0

        while max_cycles <= 0 or cycles_run < max_cycles:
            env.step(1)
            cycles_run += 1
            current_cursor = int(trace.cursor)
            if current_cursor == last_cursor:
                stagnant_cycles += 1
            else:
                stagnant_cycles = 0
                last_cursor = current_cursor

            monitor_errors = env.monitor.get_errors()
            if logger is not None and progress_interval > 0 and (cycles_run % progress_interval) == 0:
                logger.info(
                    (
                        "run until golden progress checkpoint: cycles=%d cursor=%d/%d "
                        "golden_pc=%s wrong_path_target_pc=%s recovery_target_pc=%s "
                        "wrong_path_origin=%s recovery_stage=%s pending_work=%d monitor_errors=%d"
                    ),
                    cycles_run,
                    int(trace.cursor),
                    total_entries,
                    format_optional_pc(current_golden_pc_getter(trace, env.backend_model)),
                    format_optional_pc(env.backend_model._current_wrong_path_target_pc()),
                    format_optional_pc(env.backend_model._current_recovery_target_pc()),
                    (
                        "none"
                        if env.backend_model._active_wrong_path_origin_index() is None
                        else str(int(env.backend_model._active_wrong_path_origin_index()))
                    ),
                    (
                        "recovery"
                        if env.backend_model._recovery_phase_active()
                        else (
                            "pre_redirect"
                            if env.backend_model._has_active_wrong_path_episode()
                            else "none"
                        )
                    ),
                    _pending_work_count(env.backend_model),
                    len(monitor_errors),
                )
            if (
                logger is not None
                and stall_snapshot_interval > 0
                and stagnant_cycles > 0
                and (stagnant_cycles % stall_snapshot_interval) == 0
                and self.stall_snapshot_capture is not None
                and self.stall_snapshot_formatter is not None
            ):
                snapshot = self.stall_snapshot_capture(env)
                logger.warning(
                    "run until golden stall snapshot: stagnant_cycles=%d cursor=%d/%d %s",
                    stagnant_cycles,
                    int(trace.cursor),
                    total_entries,
                    self.stall_snapshot_formatter(snapshot),
                )
            if stagnant_cycles_limit > 0 and stagnant_cycles >= stagnant_cycles_limit:
                if logger is not None:
                    if self.stall_snapshot_capture is not None and self.stall_snapshot_formatter is not None:
                        snapshot = self.stall_snapshot_capture(env)
                        logger.error(
                            "run until golden early stop by stagnant_cycles_limit=%d: "
                            "stagnant_cycles=%d cursor=%d/%d %s",
                            stagnant_cycles_limit,
                            stagnant_cycles,
                            int(trace.cursor),
                            total_entries,
                            self.stall_snapshot_formatter(snapshot),
                        )
                    else:
                        logger.error(
                            "run until golden early stop by stagnant_cycles_limit=%d: stagnant_cycles=%d cursor=%d/%d",
                            stagnant_cycles_limit,
                            stagnant_cycles,
                            int(trace.cursor),
                            total_entries,
                        )
                return RunUntilGoldenTraceResult(
                    ok=False,
                    completed=False,
                    status="stagnant_limit",
                    cycles_run=cycles_run,
                    cursor=int(trace.cursor),
                    total_entries=total_entries,
                    pending_work=_pending_work_count(env.backend_model),
                    monitor_error_count=len(monitor_errors),
                )
            if monitor_errors:
                if logger is not None:
                    logger.warning("run until golden complete aborted by monitor errors")
                return RunUntilGoldenTraceResult(
                    ok=False,
                    completed=False,
                    status="monitor_error",
                    cycles_run=cycles_run,
                    cursor=int(trace.cursor),
                    total_entries=total_entries,
                    pending_work=_pending_work_count(env.backend_model),
                    monitor_error_count=len(monitor_errors),
                )
            completion_pending_work = _golden_completion_pending_work_count(env.backend_model)
            if int(trace.cursor) >= total_entries and completion_pending_work == 0:
                if logger is not None:
                    logger.info(
                        "run until golden complete: cursor=%d entries=%d cycles=%d "
                        "completion_pending_work=%d backend_pending_work=%d",
                        int(trace.cursor),
                        total_entries,
                        cycles_run,
                        completion_pending_work,
                        _pending_work_count(env.backend_model),
                    )
                return RunUntilGoldenTraceResult(
                    ok=True,
                    completed=True,
                    status="completed",
                    cycles_run=cycles_run,
                    cursor=int(trace.cursor),
                    total_entries=total_entries,
                    pending_work=completion_pending_work,
                    monitor_error_count=len(monitor_errors),
                )

        if logger is not None:
            logger.warning(
                "run until golden complete timeout: cursor=%d entries=%d pending_work=%d",
                int(trace.cursor),
                total_entries,
                _pending_work_count(env.backend_model),
            )
        return RunUntilGoldenTraceResult(
            ok=(max_cycles > 0),
            completed=False,
            status=("budget_exhausted" if max_cycles > 0 else "timeout"),
            cycles_run=cycles_run,
            cursor=int(trace.cursor),
            total_entries=total_entries,
            pending_work=_pending_work_count(env.backend_model),
            monitor_error_count=len(env.monitor.get_errors()),
        )


@dataclass(frozen=True)
class RunUntilGoldenTraceResult:
    ok: bool
    completed: bool
    status: str
    cycles_run: int
    cursor: int
    total_entries: int
    pending_work: int
    monitor_error_count: int

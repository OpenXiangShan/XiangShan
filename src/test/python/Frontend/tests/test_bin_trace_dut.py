import os
from pathlib import Path

import pytest

from env.api import api_Frontend_load_golden_trace, api_Frontend_load_program_file


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_RUN_PIPELINE_TEST = os.getenv("TB_BIN_TRACE_PIPELINE") == "1"
_RUN_TO_TRACE_COMPLETION = os.getenv("TB_RUN_TO_TRACE_COMPLETION") == "1"


def _require_env(name: str) -> str:
    val = os.getenv(name, "").strip()
    if not val:
        raise AssertionError(f"missing required env: {name}")
    return val


def _parse_int(raw: str) -> int:
    return int(str(raw), 0)


def _should_run_to_trace_completion(bin_path: Path) -> bool:
    return _RUN_TO_TRACE_COMPLETION or Path(bin_path).name == "microbench.bin"


def _trace_cursor(env) -> int | None:
    backend_model = getattr(env, "backend_model", None)
    golden_trace = getattr(backend_model, "golden_trace", None)
    if golden_trace is None:
        return None
    return int(golden_trace.cursor)


def _has_trace_remaining(env) -> bool:
    backend_model = getattr(env, "backend_model", None)
    golden_trace = getattr(backend_model, "golden_trace", None)
    if golden_trace is None:
        return False
    return golden_trace.peek() is not None


def _has_pending_backend_work(env) -> bool:
    backend_model = getattr(env, "backend_model", None)
    if backend_model is None:
        return False
    if hasattr(backend_model, "has_pending_work"):
        return bool(backend_model.has_pending_work())
    if hasattr(backend_model, "pending_work_count"):
        return int(backend_model.pending_work_count()) > 0
    return False


def _run_until_trace_completion(env, idle_cycle_limit: int = 100000) -> None:
    backend_model = getattr(env, "backend_model", None)
    assert backend_model is not None, "env.backend_model is required for trace completion mode"
    assert getattr(backend_model, "golden_trace", None) is not None, "golden trace must be loaded before trace completion mode"

    idle_limit = max(1, int(idle_cycle_limit))
    idle_cycles = 0
    last_cursor = _trace_cursor(env)
    last_commit_count = int(getattr(backend_model, "commit_count", 0))
    last_pending_work = _has_pending_backend_work(env)

    while _has_trace_remaining(env) or _has_pending_backend_work(env):
        env.step(1)
        cursor = _trace_cursor(env)
        commit_count = int(getattr(backend_model, "commit_count", 0))
        pending_work = _has_pending_backend_work(env)

        progressed = (
            cursor != last_cursor
            or commit_count != last_commit_count
            or pending_work != last_pending_work
        )
        if progressed:
            idle_cycles = 0
            last_cursor = cursor
            last_commit_count = commit_count
            last_pending_work = pending_work
            continue

        idle_cycles += 1
        if idle_cycles >= idle_limit:
            raise AssertionError(
                "trace completion made no progress: "
                f"cursor={cursor} commit_count={commit_count} pending_work={int(pending_work)} idle_cycles={idle_cycles}"
            )

@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.skipif(not _RUN_PIPELINE_TEST, reason="pipeline-only test")
def test_bin_trace(env):
    bin_path = Path(_require_env("TB_BIN_PATH"))
    trace_path = Path(_require_env("TB_TRACE_PATH"))
    base_addr = _parse_int(os.getenv("TB_BASE_ADDR", "0x80000000"))
    step_cycles = int(os.getenv("TB_STEP_CYCLES", "0"))

    assert bin_path.is_file(), f"bin file not found: {bin_path}"
    assert trace_path.is_file(), f"trace file not found: {trace_path}"

    bin_size = int(api_Frontend_load_program_file(env, str(bin_path), base_addr))
    trace_entries = int(api_Frontend_load_golden_trace(env, str(trace_path)))

    if _should_run_to_trace_completion(bin_path):
        _run_until_trace_completion(env)
    elif step_cycles > 0:
        env.step(step_cycles)

    assert bin_size > 0
    assert trace_entries > 0
    assert not env.monitor.get_errors()

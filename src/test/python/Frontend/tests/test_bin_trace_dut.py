import os
from pathlib import Path

import pytest

from env.request_apis import run_until_golden_trace_complete
from env.sequences import (
    LoadGoldenTraceSequence,
    LoadProgramFileSequence,
)
from env.transactions import GoldenTraceSource


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


_TRACE_MAX_CYCLES = _parse_int(os.getenv("TB_TRACE_MAX_CYCLES", "0"))


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.skipif(not _RUN_PIPELINE_TEST, reason="pipeline-only test")
def test_bin_trace(env):
    bin_path = Path(_require_env("TB_BIN_PATH"))
    trace_path = Path(_require_env("TB_TRACE_PATH"))
    base_addr = _parse_int(os.getenv("TB_BASE_ADDR", "0x80000000"))
    step_cycles = int(os.getenv("TB_STEP_CYCLES", "0"))

    assert bin_path.is_file(), f"bin file not found: {bin_path}"
    assert trace_path.is_file(), f"trace file not found: {trace_path}"

    bin_size = LoadProgramFileSequence(
        path=str(bin_path),
        base_addr=base_addr,
        step_cycles=1,
    ).run(env)
    trace_entries = LoadGoldenTraceSequence(
        source=GoldenTraceSource(path=str(trace_path)),
    ).run(env)

    if _should_run_to_trace_completion(bin_path):
        completed = run_until_golden_trace_complete(
            env,
            max_cycles=_TRACE_MAX_CYCLES,
        )
        assert completed is True
    elif step_cycles > 0:
        env.step(step_cycles)

    assert bin_size > 0
    assert trace_entries > 0
    assert not env.monitor.get_errors()

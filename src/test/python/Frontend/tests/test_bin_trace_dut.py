import os
from pathlib import Path

import pytest

from env.api import (
    api_Frontend_load_golden_trace,
    api_Frontend_load_program_file,
    api_Frontend_run_until_golden_complete,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_RUN_PIPELINE_TEST = os.getenv("TB_BIN_TRACE_PIPELINE") == "1"


def _require_env(name: str) -> str:
    val = os.getenv(name, "").strip()
    if not val:
        raise AssertionError(f"missing required env: {name}")
    return val


def _parse_int(raw: str) -> int:
    return int(str(raw), 0)


def _trace_cycle_limit() -> int:
    value = _parse_int(os.getenv("TB_TRACE_MAX_CYCLES", "0"))
    if value < 0:
        raise AssertionError("TB_TRACE_MAX_CYCLES must be >= 0")
    return int(value)


def _trace_start_index() -> int:
    value = _parse_int(os.getenv("TB_TRACE_START_INDEX", "0"))
    if value < 0:
        raise AssertionError("TB_TRACE_START_INDEX must be >= 0")
    return int(value)


def _reset_vector() -> int:
    return _parse_int(os.getenv("TB_RESET_VECTOR", os.getenv("TB_BASE_ADDR", "0x80000000")))


@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
@pytest.mark.skipif(not _RUN_PIPELINE_TEST, reason="pipeline-only test")
def test_bin_trace(env):
    bin_path = Path(_require_env("TB_BIN_PATH"))
    trace_path = Path(_require_env("TB_TRACE_PATH"))
    base_addr = _parse_int(os.getenv("TB_BASE_ADDR", "0x80000000"))
    reset_vector = _reset_vector()
    trace_start_index = _trace_start_index()

    assert bin_path.is_file(), f"bin file not found: {bin_path}"
    assert trace_path.is_file(), f"trace file not found: {trace_path}"

    env.initialize(reset_vector=reset_vector, bare_mode=True, reset_cycles=20)
    bin_size = api_Frontend_load_program_file(env, str(bin_path), base_addr, max_cycles=0)
    trace_entries = api_Frontend_load_golden_trace(env, str(trace_path), max_cycles=0, start_index=trace_start_index)
    completed = api_Frontend_run_until_golden_complete(env, max_cycles=_trace_cycle_limit())
    assert completed is True

    assert bin_size > 0
    assert trace_entries > 0
    assert not env.monitor.get_errors()

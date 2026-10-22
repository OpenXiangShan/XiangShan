from __future__ import annotations

import os
from collections.abc import AsyncIterator
from pathlib import Path
from typing import Any

import toffee_test

from memblock_toffee.adapters import MemBlockDutAdapter, load_dut_class
from memblock_toffee.coverage import control_coverage
from memblock_toffee.env import MemBlockEnv
from memblock_toffee.runtime import validate_runtime
from memblock_toffee.toffee_runtime import start_toffee_clock, stop_toffee_clock


def _load_dut_class() -> type[Any]:
    generated_root = Path(os.environ.get("MEMBLOCK_TOFFEE_PYTHON_PATH", ""))
    validate_runtime(generated_root.resolve())
    return load_dut_class(generated_root)


@toffee_test.fixture
async def memblock_env(
    toffee_request: toffee_test.ToffeeRequest,
) -> AsyncIterator[MemBlockEnv]:
    dut_class = _load_dut_class()
    raw_dut = toffee_request.create_dut(dut_class)
    adapter = MemBlockDutAdapter(raw_dut)
    env = MemBlockEnv(adapter)
    clock_started = False
    try:
        start_toffee_clock(adapter)
        clock_started = True
        env.control_coverage = control_coverage(env)
        toffee_request.add_cov_groups(env.control_coverage)
        yield env
    finally:
        if clock_started:
            await stop_toffee_clock()

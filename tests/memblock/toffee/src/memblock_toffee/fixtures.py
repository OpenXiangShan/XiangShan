from __future__ import annotations

import os
from collections.abc import AsyncIterator
from pathlib import Path
from typing import Any

import toffee_test

from memblock_toffee.adapters import MemBlockDutAdapter, load_dut_class
from memblock_toffee.config import VerificationConfig
from memblock_toffee.env import MemBlockEnv
from memblock_toffee.randomness import RandomStreams
from memblock_toffee.runtime import validate_runtime


def _load_dut_class() -> type[Any]:
    generated_root = Path(os.environ.get("MEMBLOCK_TOFFEE_PYTHON_PATH", ""))
    validate_runtime(generated_root.resolve())
    return load_dut_class(generated_root)


@toffee_test.fixture
async def memblock_env(
    verification_config: VerificationConfig,
    random_streams: RandomStreams,
    toffee_request: toffee_test.ToffeeRequest,
) -> AsyncIterator[MemBlockEnv]:
    dut_class = _load_dut_class()
    raw_dut = toffee_request.create_dut(dut_class)
    adapter = MemBlockDutAdapter(raw_dut)
    env = MemBlockEnv(
        adapter,
        config=verification_config,
        rng=random_streams,
    )
    try:
        await env.start()
        for group in env.coverage_groups:
            toffee_request.add_cov_groups(group)
        yield env
    finally:
        await env.close()

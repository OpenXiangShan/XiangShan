"""Real-DUT coverage for all taken-CFI end offsets in a V3 fetch block."""

from __future__ import annotations

import os

import pytest

from env.core.transactions import ProgramImage
from env.sequences import LoadProgramSequence

from tests.py.jiabowen.test_ifu_predchecker_v3_dut import (
    _BASE,
    _CNOP,
    _c_j,
)


_RUN_DUT = os.getenv("TB_ENABLE_DUT_TESTS") == "1"
_BLOCK_BYTES = 64
_BLOCK_COUNT = 16


def _all_taken_offset_blocks() -> bytes:
    halfwords: list[int] = []
    for block in range(_BLOCK_COUNT):
        current = [_CNOP] * 32
        branch_pc = _BASE + block * _BLOCK_BYTES + block * 2
        target = _BASE + ((block + 1) % _BLOCK_COUNT) * _BLOCK_BYTES
        current[block] = _c_j(target - branch_pc)
        halfwords.extend(current)
    return b"".join(value.to_bytes(2, "little") for value in halfwords)


@pytest.mark.funcov_bins("BIN-974")
@pytest.mark.skipif(not _RUN_DUT, reason="set TB_ENABLE_DUT_TESTS=1 to run DUT integration")
def test_ifu_owner_taken_cfi_covers_all_end_offsets(env) -> None:
    LoadProgramSequence(
        image=ProgramImage(payload=_all_taken_offset_blocks(), base_addr=_BASE),
        step_cycles=0,
    ).run(env)
    env.initialize(reset_vector=_BASE, bare_mode=True, reset_cycles=20)
    env.monitor.clear()
    env.monitor.set_expected_pc(_BASE)

    for _ in range(12000):
        if env.functional_coverage.key_hit(
            "ifu_v3_boundary_owner_model", "owner_leaf_076"
        ):
            break
        env.step(1)

    assert env.functional_coverage.key_hit(
        "ifu_v3_boundary_owner_model", "owner_leaf_076"
    )
    assert not env.monitor.get_errors()

"""Position-coordinate and generated-RTL contracts, not BIN-978 HIT proof."""

from pathlib import Path

import pytest

from tests.py.jiabowen.test_ifu_cross_taken_position_v3_dut import (
    _BASE, _cross_program, _cross_trace, _position_coordinates,
)


def test_cross_canary_program_matches_independent_trace():
    payload, trace = _cross_program(), _cross_trace(laps=2)
    assert len(trace.entries) == 98
    for i, entry in enumerate(trace.entries):
        offset = entry.pc - _BASE
        assert int.from_bytes(payload[offset:offset + entry.size], "little") == entry.instr
        if i + 1 < len(trace.entries):
            assert trace.entries[i + 1].pc == (entry.target_pc if entry.taken else entry.pc + entry.size)
    assert (trace.entries[16].pc, trace.entries[16].size) == (_BASE + 0x5E, 4)


def test_cross_instruction_has_four_distinct_position_coordinates():
    assert _position_coordinates(_BASE + 0x5E, _BASE + 0x3E, _BASE + 0x60) == {
        "instruction_start_relative_to_first": 16,
        "instruction_start_relative_to_aligned_first": 31,
        "instruction_start_within_32B": 15,
        "instruction_end_relative_to_second": 0,
    }


@pytest.mark.parametrize("start_low", range(0, 32, 2))
def test_non_page_shortened_64B_fallthrough_cannot_end_at_relative_15(start_low):
    start = _BASE + 0x20 + start_low
    next_start = (start + 64) & -32
    relative_last_halfword = (next_start - start) // 2 - 1
    assert 16 <= relative_last_halfword <= 31
    assert relative_last_halfword != 15


def test_default_geometry_and_cross_owner_source_contract():
    root = Path(__file__).resolve().parents[7]
    frontend = (root / "src/main/scala/xiangshan/frontend/FrontendParameters.scala").read_text()
    bpu = (root / "src/main/scala/xiangshan/frontend/bpu/Parameters.scala").read_text()
    fallthrough = (root / "src/main/scala/xiangshan/frontend/bpu/FallThroughPredictor.scala").read_text()
    boundary = (root / "src/main/scala/xiangshan/frontend/ifu/InstrBoundary.scala").read_text()
    ifu = (root / "src/main/scala/xiangshan/frontend/ifu/Ifu.scala").read_text()
    generated = (root / "build-frontend/rtl/Ifu.sv").read_text()
    assert "FetchBlockSize:           Int = 64" in frontend
    assert "FetchBlockAlignSize.getOrElse(FetchBlockSize / 2)" in bpu
    assert "getAlignedPc(s1_startPc + FetchBlockSize.U)" in fallthrough
    assert "!io.req.fetchBlock(0).takenCfiOffset.valid" in boundary
    assert "instr.isCrossBlockInstr := crossBlockFallThrough && !maybeRvc(i)" in boundary
    assert "!s1_baseAlignedInstrVec(i).blockSel && s1_baseAlignedInstrVec(i).isCrossBlockInstr,\n      0.U" in ifu
    assert "s2_alignedInstrVec_0_endOffset" in generated
    assert "s2_alignedInstrVec_0_isCrossBlockInstr" in generated

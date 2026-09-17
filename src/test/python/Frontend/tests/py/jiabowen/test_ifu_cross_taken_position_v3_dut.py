"""Diagnostic canary for BIN-978's unresolved 'position 15' coordinate.

This is not a BIN-978 testcase: it deliberately has no earlier unpredicted
JAL in the target window. Do not backannotate incidental runtime hits.
"""

import os

import pytest

from env.core.transactions import ProgramImage
from env.funcov.py.ifu.compact_funcov import _read_ifu_internal_with_path
from env.model.golden_trace import GoldenTrace, TraceEntry
from env.sequences import LoadProgramSequence
from env.support.pc_utils import fold_pc
from tests.py.jiabowen.test_ifu_predchecker_v3_dut import _BASE, _CNOP, _c_j, _jal_x0


def _cross_program():
    payload = bytearray(_CNOP.to_bytes(2, "little") * 256)
    payload[0x5E:0x62] = _jal_x0(0x100 - 0x5E).to_bytes(4, "little")
    payload[0x13E:0x140] = _c_j(0x3E - 0x13E).to_bytes(2, "little")
    return bytes(payload)


def _cross_trace(laps=32):
    entries = []
    for _ in range(laps):
        for offset in range(0x3E, 0x5E, 2):
            entries.append(TraceEntry(len(entries), _BASE + offset, _CNOP, 2))
        entries.append(TraceEntry(len(entries), _BASE + 0x5E, _jal_x0(0xA2),
                                  4, "jump", True, _BASE + 0x100))
        for offset in range(0x100, 0x13E, 2):
            entries.append(TraceEntry(len(entries), _BASE + offset, _CNOP, 2))
        entries.append(TraceEntry(len(entries), _BASE + 0x13E, _c_j(-0x100),
                                  2, "jump", True, _BASE + 0x3E))
    return GoldenTrace(entries)


def _position_coordinates(pc, first_start, second_start, *, aligned_bytes=32):
    assert pc & 1 == first_start & 1 == second_start & 1 == 0
    assert second_start == pc + 2
    assert aligned_bytes > 0 and aligned_bytes & (aligned_bytes - 1) == 0
    return {
        "instruction_start_relative_to_first": (pc - first_start) // 2,
        "instruction_start_relative_to_aligned_first": (pc - (first_start & -aligned_bytes)) // 2,
        "instruction_start_within_32B": (pc & 31) // 2,
        "instruction_end_relative_to_second": (pc + 2 - second_start) // 2,
    }


@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT")
def test_cross_taken_position_coordinate_canary(env):
    LoadProgramSequence(image=ProgramImage(payload=_cross_program(), base_addr=_BASE),
                        step_cycles=0).run(env)
    assert not env.monitor.get_errors()
    env.initialize(reset_vector=_BASE + 0x3E, bare_mode=True, reset_cycles=20)
    env.monitor.set_expected_pc(_BASE + 0x3E)
    trace = _cross_trace()
    env.backend_model.set_golden_trace(trace)
    recorder = env.functional_coverage
    checkpoints, paths, seen = [], {}, set()

    def read(stem):
        value, path = _read_ifu_internal_with_path(recorder, env.dut, stem)
        assert value is not None, {"missing_probe": stem}
        paths[stem] = path
        return int(value)

    def observe(cycle, _env):
        if read("s2_valid_valid") != 1 or read("s2_fire") != 1 or read("s2_flush") != 0:
            return
        if any(read(f"s2_fetchBlock_{b}_valid") != 1 for b in range(2)):
            return
        for slot in range(35):
            prefix = f"s2_alignedInstrVec_{slot}_"
            if read(prefix + "valid") != 1 or read(prefix + "isCrossBlockInstr") != 1:
                continue
            if read(prefix + "isPredTaken") != 1:
                continue
            pc = read(f"s2_alignedInstrPcVec_{slot}_addr") << 1
            if pc != _BASE + 0x5E:
                continue
            blocks = [{stem: read(f"s2_fetchBlock_{b}_" + stem) for stem in (
                "startVAddr_addr", "ftqIdx_flag", "ftqIdx_value",
                "takenCfiOffset_valid", "takenCfiOffset_bits",
            )} for b in range(2)]
            assert read(prefix + "blockSel") == 0
            assert read(prefix + "isRvc") == read(prefix + "invalidTaken") == 0
            assert read(prefix + "data") == _jal_x0(0xA2)
            assert read(prefix + "endOffset") == 0
            assert blocks[0]["takenCfiOffset_valid"] == 0
            assert blocks[1]["takenCfiOffset_valid"] == 1
            assert blocks[1]["takenCfiOffset_bits"] == 0
            assert read(f"s2_alignedPdInfoVec_{slot}_brAttribute_branchType") == 2
            assert read(f"s2_alignedPdInfoVec_{slot}_isRVC") == 0
            assert read("io_toIBuffer_valid") == read("io_toIBuffer_ready") == 1
            assert read("io_toIBuffer_bits_enqEnable") & (1 << slot)
            assert read(f"io_toIBuffer_bits_instrs_{slot}") == _jal_x0(0xA2)
            assert read(f"io_toIBuffer_bits_foldpc_{slot}") == fold_pc(pc)
            assert read(f"io_toIBuffer_bits_instrEndOffset_{slot}_offset") == 0
            for field in ("flag", "value"):
                assert read(f"io_toIBuffer_bits_ftqPtr_{slot}_{field}") == blocks[1]["ftqIdx_" + field]
            coordinates = _position_coordinates(pc, blocks[0]["startVAddr_addr"] << 1,
                                                blocks[1]["startVAddr_addr"] << 1)
            identity = (blocks[0]["ftqIdx_flag"], blocks[0]["ftqIdx_value"])
            if identity in seen:
                continue
            seen.add(identity)
            readings = {}
            for stem in (prefix + "isCrossBlockInstr", prefix + "blockSel",
                         prefix + "endOffset", prefix + "isPredTaken",
                         "s2_fetchBlock_0_startVAddr_addr", "s2_fetchBlock_1_startVAddr_addr"):
                values = {}
                for root in ("Frontend_top.Frontend.inner_ifu.",
                             "Frontend_top.Frontend.inner_ifu.__Vtogcov__",
                             "Frontend_top.Frontend._inner_ifu_"):
                    actual = recorder._try_read_dut_signal(env.dut, root + stem)
                    if actual is not None:
                        values[root + stem] = int(actual)
                assert values and len(set(values.values())) == 1, values
                readings[stem] = values
            checkpoint = dict(cycle=cycle, slot=slot, pc=pc, blocks=blocks,
                              coordinates=coordinates, signal_paths=dict(paths), canary=readings)
            checkpoints.append(checkpoint)
            env._emit_event("ifu.cross_taken_position_canary", checkpoint)

    env.register_cycle_observer(observe)
    for _ in range(4096):
        env.step(1)
        if trace.cursor == len(trace.entries):
            break
    recorder.risk_observations.append(dict(event="bin978_position_canary_diagnostic",
        checkpoints=checkpoints, trace_cursor=trace.cursor, trace_length=len(trace.entries),
        explicitly_not_bin978="no earlier unpredicted JAL in this window"))
    assert trace.cursor == len(trace.entries)
    assert len(checkpoints) >= 2, "no independently accepted cross-taken transactions"
    assert not recorder.key_hit("ifu_v3_boundary_owner_model", "owner_leaf_080")
    assert not env.monitor.get_errors()

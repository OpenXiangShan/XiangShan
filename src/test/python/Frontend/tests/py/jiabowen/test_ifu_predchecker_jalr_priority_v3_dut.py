"""JALR priority with an ISA-derived target, distinct from checker seqNext."""

from __future__ import annotations

import os

import pytest

from env.funcov.py.ifu.compact_funcov import (
    _read_ifu_internal_with_path,
    _read_predchecker_with_path,
)
from env.model.golden_trace import GoldenTrace, TraceEntry
from env.support.pc_utils import fold_pc
from env.support.rvc_decoder import expand_rvc
from tests.py.jiabowen.test_ifu_predchecker_v3_dut import (
    _ADDI_X0_X0_0, _BASE, _BRANCH_SAME_TARGET, _CNOP, _c_j, _jal_x0,
    _load_and_reset, _replace_u32s_while_fencei_held, _warm_until_prediction,
)

_AUIPC_X6_ZERO = 0x00000317
_JALR_X0_X6_60 = 0x03C30067


def _architectural_trace(branch_halfword):
    """Fixed program oracle: AUIPC@base+4 sets x6, JALR adds 60 and clears bit 0.

    No prediction, DUT result or observed next-PC is used to construct this trace.
    Other blocks retain the known C.NOP/JAL training loop.
    """
    target = ((_BASE + 4) + 60) & ~1
    entries = []

    def append(pc, instr, size, kind="normal", next_pc=None):
        entries.append(TraceEntry(len(entries), pc, instr, size, kind,
                                  next_pc is not None, next_pc))

    for _ in range(8):
        append(_BASE, _CNOP, 2)
        append(_BASE + 2, _CNOP, 2)
        append(_BASE + 4, _AUIPC_X6_ZERO, 4)
        append(_BASE + 8, _JALR_X0_X6_60, 4, "jump_indirect", target)
        for block in range(1, 8):
            start = _BASE + block * 64
            for halfword in range(branch_halfword):
                append(start + halfword * 2, _CNOP, 2)
            pc = start + branch_halfword * 2
            next_pc = _BASE + (block + 1) * 64 if block < 7 else _BASE
            rvi = branch_halfword != 15
            append(pc, _jal_x0(next_pc - pc) if rvi else _c_j(next_pc - pc),
                   4 if rvi else 2, "jump", next_pc)
    return GoldenTrace(entries)


@pytest.mark.parametrize(
    "later_invalid_taken,target_bin",
    [pytest.param(True, "BIN-990", marks=pytest.mark.funcov_bins("BIN-990"), id="invalid-tail"),
     pytest.param(False, "BIN-983", marks=pytest.mark.funcov_bins("BIN-983"), id="taken-jal")],
)
@pytest.mark.skipif(os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT")
def test_earlier_jalr_wins_later_prediction(env, later_invalid_taken, target_bin):
    branch_halfword = 15 if later_invalid_taken else 13
    _load_and_reset(env, branch_halfword=branch_halfword, rvi_jal=not later_invalid_taken)
    earlier_pc, later_pc = _BASE + 8, _BASE + 2 * branch_halfword
    target_pc, checker_target = _BASE + 64, earlier_pc + 4
    later_instr = _BRANCH_SAME_TARGET if later_invalid_taken else _jal_x0(target_pc - later_pc)
    _warm_until_prediction(env, later_pc)
    recorder = env.functional_coverage
    paths, checkpoints = {}, []
    pending = None
    trace = _architectural_trace(branch_halfword)
    checked_arch = {}
    backend_redirects = []

    def observe_backend_input(cycle, _env):
        if not checkpoints:
            return
        prefix = "io_backend_toFtq_redirect_"

        def port(stem):
            name = prefix + stem
            signal = getattr(env.dut, name, None)
            assert signal is not None, {"missing_port": name}
            paths[name] = name
            return int(signal.value)

        if port("valid") != 1:
            return
        record = dict(cycle=cycle, pc=port("bits_pc"), target=port("bits_target"),
                      ftq=[port("bits_ftqIdx_flag"), port("bits_ftqIdx_value")],
                      offset=port("bits_ftqOffset"), level=port("bits_level"))
        assert record["pc"] == earlier_pc and record["target"] == target_pc, record
        assert record["ftq"] == checkpoints[0]["request"]["ftq"], record
        assert record["offset"] == 5 and record["level"] == 0, record
        backend_redirects.append(record)
        env._emit_event("ifu.predchecker.jalr_backend_recovery", record)

    def read(stem, pred=False):
        reader = _read_predchecker_with_path if pred else _read_ifu_internal_with_path
        value, path = reader(recorder, env.dut, stem)
        assert value is not None, {"missing_probe": stem, "predchecker": pred}
        paths[stem] = path
        return int(value)

    def observe(cycle, _env):
        nonlocal pending
        if pending is not None:
            assert cycle == pending["cycle"] + 1
            prefix = "io_resp_stage2Out_checkerRedirect_"
            redirect = {key: read(prefix + stem, True) for key, stem in {
                "valid": "valid", "target_addr": "bits_target_addr",
                "end_offset": "bits_endOffset", "branch_type": "bits_attribute_branchType",
                "ras_action": "bits_attribute_rasAction", "invalid": "bits_invalidTaken",
                "raw_owner": "bits_blockSel", "cross": "bits_isCrossBlockInstr",
            }.items()}
            assert redirect == dict(valid=1, target_addr=checker_target >> 1,
                                    end_offset=5, branch_type=3, ras_action=0,
                                    invalid=0, raw_owner=0, cross=0), redirect
            assert read("io_toFtq_wbRedirect_valid") == 1
            assert read("io_toFtq_wbRedirect_bits_target") == checker_target
            assert read("io_toFtq_wbRedirect_bits_pc") == _BASE
            assert read("io_toFtq_wbRedirect_bits_ftqOffset") == 5
            assert read("io_toFtq_wbRedirect_bits_ftqIdx_flag") == pending["ftq"][0]
            assert read("io_toFtq_wbRedirect_bits_ftqIdx_value") == pending["ftq"][1]
            assert read("io_toFtq_wbRedirect_bits_canTrain") == 0
            checkpoints.append(dict(request=pending, redirect=redirect, signal_paths=dict(paths)))
            env._emit_event("ifu.predchecker.jalr_priority_checkpoint", checkpoints[-1])
            pending = None
        if checkpoints:
            # Check ISA-expected bits on the backend's correctly matched path;
            # wrong-path speculative delivery before backend recovery is legal.
            for entry in env.backend_model._cfvec_queue:
                if entry.golden_index is None or entry.path_state != "correct":
                    continue
                golden = trace.entries[entry.golden_index]
                expected = expand_rvc(golden.instr) if golden.size == 2 else golden.instr
                assert entry.pc == golden.pc and entry.instr == expected
                checked_arch[entry.golden_index] = (entry.pc, entry.instr)
            return
        if read("s2_valid_valid") != 1 or read("s2_flush") or read("s2_fire") != 1:
            return
        entries = []
        for slot in range(35):
            prefix = f"s2_alignedInstrVec_{slot}_"
            valid, invalid = read(prefix + "valid"), read(prefix + "invalidTaken")
            if valid == 0 and invalid == 0:
                continue
            entries.append(dict(slot=slot, valid=valid, invalid=invalid,
                pc=read(f"s2_alignedInstrPcVec_{slot}_addr") << 1,
                data=read(prefix + "data"), taken=read(prefix + "isPredTaken"),
                branch_type=read(f"s2_alignedPdInfoVec_{slot}_brAttribute_branchType"),
                ras_action=read(f"s2_alignedPdInfoVec_{slot}_brAttribute_rasAction")))
        earlier = next((e for e in entries if e["pc"] == earlier_pc), None)
        later = next((e for e in entries if e["pc"] == later_pc), None)
        if not (earlier and later and earlier["branch_type"] == 3
                and earlier["taken"] == 0 and later["taken"] == 1
                and later["invalid"] == int(later_invalid_taken)):
            return
        assert earlier["valid"] == 1 and earlier["invalid"] == earlier["ras_action"] == 0
        assert earlier["data"] == _JALR_X0_X6_60 and later["data"] == later_instr
        assert later["valid"] == int(not later_invalid_taken)
        assert later["branch_type"] == (0 if later_invalid_taken else 2)
        assert read("s2_fetchBlock_0_takenCfiOffset_valid") == 1
        assert read("s2_fetchBlock_0_takenCfiOffset_bits") == (15 if later_invalid_taken else 14)
        ftq = [read("s2_fetchBlock_0_ftqIdx_flag"), read("s2_fetchBlock_0_ftqIdx_value")]
        assert read("io_toIBuffer_valid") == read("io_toIBuffer_ready") == 1
        fixed, enq = read("s2_fixedInstrValid"), read("io_toIBuffer_bits_enqEnable")
        expected_bits = {_BASE: _ADDI_X0_X0_0, _BASE + 2: _ADDI_X0_X0_0,
                         _BASE + 4: _AUIPC_X6_ZERO, earlier_pc: _JALR_X0_X6_60}
        expected_mask = sum(1 << e["slot"] for e in entries if e["slot"] <= earlier["slot"])
        assert fixed == enq == expected_mask
        assert all(e["branch_type"] == e["taken"] == e["invalid"] == 0
                   for e in entries if e["slot"] < earlier["slot"])
        for entry in entries:
            slot = entry["slot"]
            keep = int(slot <= earlier["slot"])
            assert (fixed >> slot) & 1 == (enq >> slot) & 1 == keep
            if keep:
                assert entry["pc"] in expected_bits
                assert read(f"io_toIBuffer_bits_instrs_{slot}") == expected_bits[entry["pc"]]
                assert read(f"io_toIBuffer_bits_foldpc_{slot}") == fold_pc(entry["pc"])
                assert read(f"io_toIBuffer_bits_ftqPtr_{slot}_flag") == ftq[0]
                assert read(f"io_toIBuffer_bits_ftqPtr_{slot}_value") == ftq[1]
                offset = (entry["pc"] - _BASE) // 2 + int(entry["pc"] >= _BASE + 4)
                assert read(f"io_toIBuffer_bits_instrEndOffset_{slot}_offset") == offset
        pending = dict(cycle=cycle, earlier=earlier, later=later, ftq=ftq,
                       fixed_mask=fixed, enqueue_mask=enq)
        # Attach at the S2 request edge, before the first IBuffer output from
        # this window. Attaching at the registered redirect edge is too late.
        env.backend_model.set_golden_trace(trace)
        env._emit_event("ifu.predchecker.jalr_oracle", {
            "auipc_pc": _BASE + 4, "x6": _BASE + 4,
            "jalr_immediate": 60, "architectural_target": target_pc,
            "checker_seq_target": checker_target, "trace_entries": len(trace.entries),
        })

    env.register_cycle_observer(observe)
    env.register_pre_drive_cycle_observer(observe_backend_input)
    _replace_u32s_while_fencei_held(env, (
        (_BASE + 4, _AUIPC_X6_ZERO), (earlier_pc, _JALR_X0_X6_60), (later_pc, later_instr),
    ))
    env.backend_model.inject_redirect(_BASE, f"jalr_priority_{target_bin}", delay_cycles=1)
    for _ in range(1024):
        env.step(1)
        if checkpoints and all(i in checked_arch for i in range(12)):
            break
    assert checkpoints, f"missing same-window {target_bin} checkpoint"
    assert all(i in checked_arch for i in range(12)), {
        "reason": "ISA-correct JALR and target delivery not completed",
        "checked": checked_arch, "cursor": trace.cursor, "backend": env.backend_model.get_stats(),
    }
    assert checked_arch[3][0] == earlier_pc and checked_arch[4][0] == target_pc
    assert len(backend_redirects) == 1, backend_redirects
    recovered = [item for item in env.monitor.observations
                 if item.cycle > backend_redirects[0]["cycle"] + 1]
    assert len(recovered) >= 8, "insufficient post-redirect backend delivery"
    assert [item.pc for item in recovered[:8]] == [target_pc + 2 * i for i in range(8)]
    assert all(item.instr == _ADDI_X0_X0_0 and item.is_rvc for item in recovered[:8])
    env._emit_event("ifu.predchecker.jalr_priority_delivery", {
        "checked_prefix": [checked_arch[i] for i in range(12)],
        "architectural_target": target_pc, "checker_seq_target": checker_target,
        "trace_cursor": trace.cursor,
        "backend_recovery": backend_redirects, "signal_paths": dict(paths),
        "post_recovery_pcs": [item.pc for item in recovered],
    })
    spec = recorder.definition_by_bin_id[target_bin]
    assert recorder.key_hit(spec.coverage_group, spec.bin_name, coverpoint=spec.coverpoint)
    assert not env.monitor.get_errors()

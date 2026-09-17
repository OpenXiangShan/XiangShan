"""Same-window PredChecker priority through legal stale-prediction traffic."""

from __future__ import annotations

import os

import pytest

from env.funcov.py.ifu.compact_funcov import (
    _read_ifu_internal_with_path,
    _read_predchecker_with_path,
)
from env.support.pc_utils import fold_pc
from tests.py.jiabowen.test_ifu_predchecker_v3_dut import (
    _ADDI_X0_X0_0,
    _BASE,
    _BRANCH_SAME_TARGET,
    _jal_x0,
    _load_and_reset,
    _replace_u32s_while_fencei_held,
    _warm_until_prediction,
)


@pytest.mark.funcov_bins("BIN-993")
@pytest.mark.skipif(
    os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT"
)
def test_earlier_jal_wins_later_non_cfi_taken(env):
    """The earliest JAL wins a later stale Non-CFI taken in the same request."""
    _check_earlier_jal_priority(env, later_invalid_taken=False)


@pytest.mark.funcov_bins("BIN-989")
@pytest.mark.skipif(
    os.getenv("TB_ENABLE_DUT_TESTS") != "1", reason="requires real Frontend DUT"
)
def test_earlier_jal_wins_later_invalid_taken(env):
    """A JAL beats a younger branch whose predicted end is its first halfword."""
    _check_earlier_jal_priority(env, later_invalid_taken=True)


def _check_earlier_jal_priority(env, *, later_invalid_taken):
    branch_halfword = 15 if later_invalid_taken else 13
    _load_and_reset(env, branch_halfword=branch_halfword, rvi_jal=not later_invalid_taken)
    older_pc = _BASE + 8
    younger_pc = _BASE + 2 * branch_halfword
    target_pc = _BASE + 64
    younger_instruction = _BRANCH_SAME_TARGET if later_invalid_taken else _ADDI_X0_X0_0
    target_bin = "BIN-989" if later_invalid_taken else "BIN-993"
    _warm_until_prediction(env, younger_pc)
    recorder = env.functional_coverage
    paths = {}
    pending = None
    checkpoints = []

    def read(stem, *, pred=False):
        reader = _read_predchecker_with_path if pred else _read_ifu_internal_with_path
        value, path = reader(recorder, env.dut, stem)
        assert value is not None, {"missing_required_probe": stem, "predchecker": pred}
        paths[stem] = path
        return int(value)

    def observe(cycle, _env):
        nonlocal pending
        if pending is not None:
            # PredChecker registers exactly this request on the preceding edge.
            assert cycle == pending["cycle"] + 1
            prefix = "io_resp_stage2Out_checkerRedirect_"
            observed = {
                "valid": read(prefix + "valid", pred=True),
                "target": read(prefix + "bits_target_addr", pred=True) << 1,
                "end_offset": read(prefix + "bits_endOffset", pred=True),
                "branch_type": read(prefix + "bits_attribute_branchType", pred=True),
                "invalid_taken": read(prefix + "bits_invalidTaken", pred=True),
                "raw_owner": read(prefix + "bits_blockSel", pred=True),
                "cross_block": read(prefix + "bits_isCrossBlockInstr", pred=True),
            }
            assert observed == {
                "valid": 1, "target": target_pc, "end_offset": 5,
                "branch_type": 2, "invalid_taken": 0, "raw_owner": 0, "cross_block": 0,
            }, {"request": pending, "redirect": observed}
            assert read("io_toFtq_wbRedirect_valid") == 1
            assert read("io_toFtq_wbRedirect_bits_target") == target_pc
            assert read("io_toFtq_wbRedirect_bits_ftqIdx_flag") == pending["ftq"][0]
            assert read("io_toFtq_wbRedirect_bits_ftqIdx_value") == pending["ftq"][1]
            assert read("io_toFtq_wbRedirect_bits_pc") == _BASE
            assert read("io_toFtq_wbRedirect_bits_ftqOffset") == 5
            checkpoint = {"request": pending, "redirect": observed, "signal_paths": dict(paths)}
            checkpoints.append(checkpoint)
            env._emit_event("ifu.predchecker.priority_checkpoint", checkpoint)
            pending = None

        if (read("s2_valid_valid") != 1 or read("s2_flush") != 0
                or read("s2_fire") != 1):
            return
        entries = []
        for slot in range(35):
            stem = f"s2_alignedInstrVec_{slot}_"
            valid = read(stem + "valid")
            invalid_taken = read(stem + "invalidTaken")
            if valid != 1 and invalid_taken != 1:
                continue
            entries.append({
                "slot": slot,
                "valid": valid,
                "pc": read(f"s2_alignedInstrPcVec_{slot}_addr") << 1,
                "pred_taken": read(stem + "isPredTaken"),
                "branch_type": read(f"s2_alignedPdInfoVec_{slot}_brAttribute_branchType"),
                "invalid_taken": invalid_taken,
                "is_rvc": read(stem + "isRvc"),
                "end_offset": read(stem + "endOffset"),
            })
        older = next((x for x in entries if x["pc"] == older_pc), None)
        younger = next((x for x in entries if x["pc"] == younger_pc), None)
        if not (older and younger and older["branch_type"] == 2
                and older["pred_taken"] == 0
                and younger["branch_type"] == 0
                and younger["pred_taken"] == 1
                and younger["invalid_taken"] == int(later_invalid_taken)):
            return
        assert older["invalid_taken"] == 0
        if later_invalid_taken:
            # The incomplete RVI is removed from instrValid and predecode,
            # while its fault marker survives for PredChecker priority.
            assert younger["valid"] == 0 and younger["is_rvc"] == 0
            assert younger["end_offset"] == 16
            assert read("s2_fetchBlock_0_takenCfiOffset_valid") == 1
            assert read("s2_fetchBlock_0_takenCfiOffset_bits") == 15
        else:
            assert younger["valid"] == 1
        assert all(x["branch_type"] == 0 and x["pred_taken"] == 0
                   and x["invalid_taken"] == 0 for x in entries if x["slot"] < older["slot"])
        fixed = read("s2_fixedInstrValid")
        enq = read("io_toIBuffer_bits_enqEnable")
        ftq = [read("s2_fetchBlock_0_ftqIdx_flag"), read("s2_fetchBlock_0_ftqIdx_value")]
        assert read("io_toIBuffer_valid") == read("io_toIBuffer_ready") == 1
        for entry in entries:
            expected = int(entry["slot"] <= older["slot"])
            assert (fixed >> entry["slot"]) & 1 == expected
            assert (enq >> entry["slot"]) & 1 == expected
            if expected:
                slot = entry["slot"]
                is_jal = entry["pc"] == older_pc
                assert read(f"io_toIBuffer_bits_instrs_{slot}") == (
                    _jal_x0(target_pc - older_pc) if is_jal else _ADDI_X0_X0_0
                )
                assert read(f"io_toIBuffer_bits_foldpc_{slot}") == fold_pc(entry["pc"])
                assert read(f"io_toIBuffer_bits_ftqPtr_{slot}_flag") == ftq[0]
                assert read(f"io_toIBuffer_bits_ftqPtr_{slot}_value") == ftq[1]
                assert read(f"io_toIBuffer_bits_instrEndOffset_{slot}_offset") == (
                    5 if is_jal else (entry["pc"] - _BASE) // 2
                )
        assert read(f"s2_alignedInstrVec_{older['slot']}_data") == _jal_x0(target_pc - older_pc)
        younger_data = read(f"s2_alignedInstrVec_{younger['slot']}_data")
        # Raw data includes the next halfword even though the invalidTaken
        # instruction is outside the complete-instruction validity mask.
        assert younger_data == younger_instruction
        pending = {
            "cycle": cycle, "older": older, "younger": younger,
            "fixed_mask": fixed, "enqueue_mask": enq,
            "ftq": ftq, "monitor_start": len(env.monitor.observations),
        }

    env.register_cycle_observer(observe)
    _replace_u32s_while_fencei_held(env, (
        (older_pc, _jal_x0(target_pc - older_pc)),
        (younger_pc, younger_instruction),
    ))
    env.backend_model.inject_redirect(_BASE, f"earlier_jal_priority_{target_bin}", delay_cycles=1)
    for _ in range(1024):
        env.step(1)
        if checkpoints:
            break
    assert checkpoints, f"no same-window priority checkpoint for {target_bin}"
    monitor_start = checkpoints[0]["request"]["monitor_start"]
    for _ in range(256):
        delivered = env.monitor.observations[monitor_start:]
        if any(item.pc == older_pc and item.instr == _jal_x0(target_pc - older_pc)
               for item in delivered) and any(item.pc == target_pc for item in delivered):
            break
        env.step(1)
    delivered = env.monitor.observations[monitor_start:]
    assert any(item.pc == older_pc and item.instr == _jal_x0(target_pc - older_pc)
               for item in delivered), "winning JAL never reached backend"
    assert any(item.pc == target_pc for item in delivered), "redirect target never reached backend"
    assert not any(older_pc < item.pc < target_pc for item in delivered), (
        "younger instructions escaped the priority mask", delivered
    )
    env._emit_event("ifu.predchecker.priority_delivery", {
        "request_cycle": checkpoints[0]["request"]["cycle"],
        "delivered_pcs": [item.pc for item in delivered],
        "winning_jal_delivered": True, "target_delivered": True,
        "younger_delivery_count": 0,
    })
    spec = recorder.definition_by_bin_id[target_bin]
    assert recorder.key_hit(spec.coverage_group, spec.bin_name, coverpoint=spec.coverpoint)
    assert not env.monitor.get_errors()

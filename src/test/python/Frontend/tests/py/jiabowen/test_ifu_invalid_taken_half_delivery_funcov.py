"""Causal BIN-940 proof: saved redirect half, S1 stitch and fired S2."""

import pytest
from pathlib import Path

from env.funcov.py.ifu.compact_funcov import (
    _sample_invalid_taken_half_delivery,
    _sample_predchecker_wb_half_rvi_selection,
)
from env.funcov.py.ifu.cfvec_funcov import sample_cfvec_coverage
from env.support.pc_utils import fold_pc
from tests.py.jiabowen.test_ifu_compact_functional_coverage import (
    _make_recorder, _PREFIX, _set_predchecker_wb_half_rvi_redirect,
    _set_predchecker_request,
)

# Recovery starts at cacheline halfword 16, but its fetch-relative offset
# must still be zero. A cacheline-boundary fixture hides this distinction.
_PC = 0x8000021E
_INST = 0x02000163


def _fixture(tmp_path):
    recorder, env, dut, _memory = _make_recorder(tmp_path)
    _set_predchecker_wb_half_rvi_redirect(dut, raw_block_sel=0, is_cross_block_instr=0)
    dut.set(_PREFIX + "wbFirstEndHalfRvi_bits_pc_addr", _PC >> 1)
    dut.set(_PREFIX + "wbFirstEndHalfRvi_bits_data", _INST & 0xFFFF)
    dut.set(_PREFIX + "predChecker.io_resp_stage2Out_checkerRedirect_bits_target_addr", (_PC + 2) >> 1)
    dut.set(_PREFIX + "io_toFtq_wbRedirect_bits_target", _PC + 2)
    _sample_predchecker_wb_half_rvi_selection(recorder, dut, 10)
    assert recorder._ifu_invalid_taken_half_delivery is not None
    for stem in ("io_fromFtq_redirect_valid", "uncacheRedirect_valid", "wbRedirect_valid",
                 "s0_flush", "s1_flush", "s2_flush", "s1_useUncacheFetch", "s2_useUncacheFetch"):
        dut.set(_PREFIX + stem, 0)
    dut.set(_PREFIX + "s0_fire", 1)
    _sample_invalid_taken_half_delivery(recorder, dut, 11)
    dut.set(_PREFIX + "s0_fire", 0)
    for stem, value in {
        "s1_valid": 1, "s1_fire": 1,
        "s1_prevEndHalfRviInfo_valid": 1,
        "s1_prevEndHalfRviInfo_bits_pc_addr": _PC >> 1,
        "s1_prevEndHalfRviInfo_bits_data": _INST & 0xFFFF,
        "s1_prevIBufEnqPtrDup_dup_0_value": 15,
        "s1_alignedInstrPcVec_3_addr": _PC >> 1,
        "s1_baseInstrData_3": _INST >> 16,
        "s1_alignedInstrVec_3_data": _INST,
        "s1_alignedInstrVec_3_isRvc": 0,
        "_s1_alignedInstrValid_T": 8,
        "s1_fetchBlock_0_valid": 1,
        "s1_fetchBlock_0_startVAddr_addr": (_PC + 2) >> 1,
        "s1_fetchBlock_0_ftqIdx_flag": 0, "s1_fetchBlock_0_ftqIdx_value": 26,
        "s2_valid_valid": 1, "s2_fire": 1,
        "s2_alignedInstrVec_3_valid": 1, "s2_alignedInstrVec_3_invalidTaken": 0,
        "s2_alignedInstrVec_3_blockSel": 0, "s2_alignedInstrVec_3_isCrossBlockInstr": 0,
        "s2_alignedInstrVec_3_data": _INST, "s2_alignedInstrVec_3_isRvc": 0,
        "s2_alignedInstrVec_3_endOffset": 0, "s2_alignedInstrPcVec_3_addr": _PC >> 1,
        "s2_fetchBlock_0_ftqIdx_flag": 0, "s2_fetchBlock_0_ftqIdx_value": 26,
        "s2_fetchBlock_0_startVAddr_addr": (_PC + 2) >> 1,
        "s2_fixedInstrValid": 8, "io_toIBuffer_valid": 1, "io_toIBuffer_ready": 1,
        "io_toIBuffer_bits_valid": 8, "io_toIBuffer_bits_enqEnable": 8,
        "io_toIBuffer_bits_instrs_3": _INST, "io_toIBuffer_bits_isRvc_3": 0,
        "io_toIBuffer_bits_foldpc_3": fold_pc(_PC),
        "io_toIBuffer_bits_instrEndOffset_3_offset": 0,
        "io_toIBuffer_bits_ftqPtr_3_flag": 0, "io_toIBuffer_bits_ftqPtr_3_value": 26,
    }.items():
        dut.set(_PREFIX + stem, value)
    for stage in ("s1", "s2"):
        dut.set(_PREFIX + f"{stage}_alignedPdInfoVec_3_brAttribute_branchType", 1)
    dut.set(_PREFIX + "s2_alignedPdInfoVec_3_isRVC", 0)
    dut.set(_PREFIX + "s2_alignedPdInfoVec_3_brAttribute_rasAction", 0)
    dut.set(_PREFIX + "s2_alignedJumpOffsetVec_3_addr", 17)
    return recorder, env, dut


def _hit(recorder):
    return recorder.key_hit("ifu_v3_pipeline_owner_model", "owner_leaf_042")


def test_bin940_requires_all_phases_and_fired_delivery(tmp_path):
    recorder, _env, dut = _fixture(tmp_path)
    assert not _hit(recorder)
    dut.set(_PREFIX + "s1_fire", 0)
    _sample_invalid_taken_half_delivery(recorder, dut, 12)
    assert not _hit(recorder)
    dut.set(_PREFIX + "s1_fire", 1)
    _sample_invalid_taken_half_delivery(recorder, dut, 13)
    dut.set(_PREFIX + "s2_fire", 0)
    dut.set(_PREFIX + "io_toIBuffer_ready", 0)
    _sample_invalid_taken_half_delivery(recorder, dut, 14)
    assert not _hit(recorder)
    dut.set(_PREFIX + "s2_fire", 1)
    dut.set(_PREFIX + "io_toIBuffer_ready", 1)
    _sample_invalid_taken_half_delivery(recorder, dut, 15)
    assert _hit(recorder)
    evidence = recorder.hits[("ifu_v3_pipeline_owner_model", "verified_leaf_event", "owner_leaf_042")].evidence[-1]
    assert evidence["producer"] == "ifu_invalid_taken_half_delivery_sampler"
    assert [evidence["observations"][k] for k in ("redirect_cycle", "s0_cycle", "s1_cycle", "delivery_cycle")] == [10, 11, 13, 15]
    assert evidence["observations"]["signal_paths"]["s1_baseInstrData_3"]


@pytest.mark.parametrize("phase,stem,value", [
    ("s1", "s1_prevEndHalfRviInfo_bits_pc_addr", 1),
    ("s1", "s1_prevEndHalfRviInfo_bits_data", 1),
    ("s1", "s1_prevEndHalfRviInfo_valid", 0),
    ("s1", "s1_alignedInstrVec_3_data", 0x13),
    ("s1", "s1_baseInstrData_3", None),
    ("s1", "s1_alignedPdInfoVec_3_brAttribute_branchType", 2),
    ("s1", "_s1_alignedInstrValid_T", None),
    ("s1", "_s1_alignedInstrValid_T", 0),
    ("s1", "s1_flush", 1),
    ("s1", "io_fromFtq_redirect_valid", 1),
    ("s1", "uncacheRedirect_valid", 1),
    ("s1", "wbRedirect_valid", 1),
    ("s2", "s2_alignedInstrVec_3_data", 0x13),
    ("s2", "s2_alignedInstrVec_3_isCrossBlockInstr", 1),
    ("s2", "s2_fetchBlock_0_ftqIdx_value", 25),
    ("s2", "s2_fetchBlock_0_startVAddr_addr", 1),
    ("s2", "s2_alignedPdInfoVec_3_brAttribute_branchType", 2),
    ("s2", "s2_alignedPdInfoVec_3_brAttribute_rasAction", 1),
    ("s2", "s2_alignedJumpOffsetVec_3_addr", 18),
    ("s2", "s2_alignedJumpOffsetVec_3_addr", None),
    ("s2", "s2_alignedInstrVec_3_endOffset", 16),
    ("s2", "s2_flush", 1),
    ("s2", "io_toIBuffer_bits_ftqPtr_3_value", 25),
    ("s2", "io_toIBuffer_bits_foldpc_3", 0),
    ("s2", "io_toIBuffer_bits_enqEnable", 0),
    ("s2", "io_toIBuffer_bits_instrEndOffset_3_offset", 1),
    ("s2", "io_toIBuffer_bits_instrs_3", None),
])
def test_bin940_rejects_missing_stale_or_cancelled_evidence(tmp_path, phase, stem, value):
    recorder, _env, dut = _fixture(tmp_path)
    if phase == "s2":
        _sample_invalid_taken_half_delivery(recorder, dut, 12)
    if value is None:
        delattr(dut, _PREFIX + stem)
    else:
        dut.set(_PREFIX + stem, value)
    _sample_invalid_taken_half_delivery(recorder, dut, 13 if phase == "s2" else 12)
    assert not _hit(recorder)
    assert recorder._ifu_invalid_taken_half_delivery is None
    assert any(e.get("event") == "ifu_invalid_taken_half_delivery_rejected"
               for e in recorder.risk_observations)


def test_bin940_cancellation_is_seen_during_backend_guard(tmp_path):
    recorder, env, dut = _fixture(tmp_path)
    recorder._ifu_redirect_skip_until_cycle = 15
    dut.set(_PREFIX + "io_fromFtq_redirect_valid", 1)
    sample_cfvec_coverage(recorder, env, 12)
    assert recorder._ifu_invalid_taken_half_delivery is None
    assert not _hit(recorder)
    dut.set(_PREFIX + "io_fromFtq_redirect_valid", 0)
    for cycle in (13, 14, 15):
        _sample_invalid_taken_half_delivery(recorder, dut, cycle)
    assert not _hit(recorder)  # Later intact payload cannot resurrect cancelled proof.


@pytest.mark.parametrize("missing", [False, True])
def test_bin940_without_selected_half_context_cannot_arm(tmp_path, missing):
    recorder, _env, dut, _memory = _make_recorder(tmp_path)
    _set_predchecker_wb_half_rvi_redirect(dut, raw_block_sel=0, is_cross_block_instr=0,
                                        selected_half_valid=0)
    if missing:
        delattr(dut, _PREFIX + "wbFirstEndHalfRvi_valid")
    _sample_predchecker_wb_half_rvi_selection(recorder, dut, 10)
    assert getattr(recorder, "_ifu_invalid_taken_half_delivery", None) is None
    assert not _hit(recorder)


def test_bin940_reset_and_sampling_gap_discard_proof(tmp_path):
    recorder, _env, dut = _fixture(tmp_path)
    _sample_invalid_taken_half_delivery(recorder, dut, 13)
    assert not _hit(recorder) and recorder._ifu_invalid_taken_half_delivery is None
    recorder, _env, dut = _fixture(tmp_path)
    recorder._clear_transient_sampling_state()
    _sample_invalid_taken_half_delivery(recorder, dut, 12)
    assert not _hit(recorder) and recorder._ifu_invalid_taken_half_delivery is None


def test_bin940_same_cycle_half_valid_is_not_a_recovery_proof(tmp_path):
    recorder, env, dut, _memory = _make_recorder(tmp_path)
    _set_predchecker_request(dut, [{"slot": 15, "branch_type": 1, "pred_taken": 1,
                                   "invalid_taken": 1, "end_offset": 15}])
    dut.set(_PREFIX + "s2_prevEndIsHalfRviInfo_valid", 1)
    sample_cfvec_coverage(recorder, env, 1)
    assert not _hit(recorder)


def test_bin940_s0_flush_without_fire_discards_proof(tmp_path):
    recorder, _env, dut = _fixture(tmp_path)
    recorder._ifu_invalid_taken_half_delivery.update(phase="s0")
    dut.set(_PREFIX + "s0_fire", 0)
    dut.set(_PREFIX + "s0_flush", 1)
    _sample_invalid_taken_half_delivery(recorder, dut, 12)
    assert recorder._ifu_invalid_taken_half_delivery is None
    assert not _hit(recorder)


def test_bin940_generated_predecode_sources_and_saved_half_offset_contract():
    root = Path(__file__).resolve().parents[7]
    rtl = (root / "build-frontend/rtl/Ifu.sv").read_text()
    source = (root / "src/main/scala/xiangshan/frontend/ifu/Ifu.scala").read_text()
    names = {line[len("  - name: "):].strip() for line in
             (root / "build-frontend/pylib-verilator/Frontend/Frontend_offset.yaml").read_text().splitlines()
             if line.startswith("  - name: ")}
    # These are exact generated sources, not invented pdInfo defaults.
    assert "Frontend_top.Frontend.inner_ifu._s1_alignedInstrValid_T" in names
    for lane in range(4):
        assert f"s2_alignedPdInfoVec_{lane}_isRVC <= s1_alignedInstrVec_{lane}_isRvc;" in rtl
        assert (f"s2_alignedPdInfoVec_{lane}_brAttribute_branchType <=\n"
                f"        s1_alignedPdInfoVec_{lane}_brAttribute_branchType;") in rtl
        for stage in ("s1", "s2"):
            assert _PREFIX + f"{stage}_alignedPdInfoVec_{lane}_brAttribute_branchType" in names
    stitch = source.split("s1_prevEndHalfRviInfo.bits.data)", 1)[1].split("\n    }", 1)[0]
    assert "s1_alignedInstrVec(i).endOffset        := 0.U" in stitch

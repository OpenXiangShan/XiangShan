from pathlib import Path
from types import SimpleNamespace

from env.funcov.py.ifu.sampler import sample_cfvec_coverage
from env.funcov.recorder import FunctionalCoverageRecorder, default_pilot_csv_path
from env.support.rvc_decoder import expand_rvc


_PREFIX = "Frontend_top.Frontend.inner_ifu.__Vtogcov__"


class _Signal:
    def __init__(self, value=0):
        self.value = int(value)


class _FakeDut:
    def set(self, name, value):
        signal = getattr(self, str(name), None)
        if signal is None:
            signal = _Signal()
            setattr(self, str(name), signal)
        signal.value = int(value)


class _Memory:
    def __init__(self):
        self._bytes = {}

    def write16(self, address, value):
        self._write(address, value, 2)

    def write32(self, address, value):
        self._write(address, value, 4)

    def _write(self, address, value, width):
        for offset in range(int(width)):
            self._bytes[int(address) + offset] = (int(value) >> (8 * offset)) & 0xFF

    def read_u8(self, address):
        return self._bytes.get(int(address), 0)

    @staticmethod
    def is_mmio(_address):
        return False


def _make_recorder(tmp_path):
    dut = _FakeDut()
    memory = _Memory()
    env = SimpleNamespace(dut=dut, memory=memory)
    recorder = FunctionalCoverageRecorder.from_pilot_csv(
        default_pilot_csv_path(),
        testcase_name="ifu_compact_unit",
        artifact_tag="ifu_compact_unit",
        output_dir=tmp_path,
    )
    recorder.attach(env)
    return recorder, env, dut, memory


def _set_ifu_output(
    dut,
    entries,
    *,
    exception_type=0,
    valid_mask_extra=0,
    prev_ibuf_enq_ptr=0,
    instr_count=None,
    s2_req_is_uncache=0,
    s2_prev_end_is_half_rvi=0,
    s2_prev_end_half_pc=0,
    s2_prev_end_half_data=0,
    s2_fetch_block_start_pc=None,
):
    enq_enable = 0
    valid_mask = 0
    dut.set(_PREFIX + "io_toIBuffer_ready", 1)
    dut.set(_PREFIX + "io_toIBuffer_valid", 1)
    dut.set(_PREFIX + "io_toIBuffer_bits_exceptionType_value", exception_type)
    for slot, pc, instr, is_rvc, end_offset, ftq_flag, ftq_value, exception_mask in entries:
        slot = int(slot)
        enq_enable |= 1 << slot
        valid_mask |= 1 << slot
        # IFU's PrunedAddr omits bit 0, matching the generated DUT signal.
        dut.set(_PREFIX + f"io_toIBuffer_bits_pc_{slot}_addr", int(pc) >> 1)
        dut.set(_PREFIX + f"io_toIBuffer_bits_instrs_{slot}", instr)
        dut.set(_PREFIX + f"io_toIBuffer_bits_isRvc_{slot}", is_rvc)
        dut.set(_PREFIX + f"io_toIBuffer_bits_instrEndOffset_{slot}_offset", end_offset)
        dut.set(_PREFIX + f"io_toIBuffer_bits_instrEndOffset_{slot}_predTaken", slot == entries[-1][0])
        dut.set(_PREFIX + f"io_toIBuffer_bits_instrEndOffset_{slot}_fixedTaken", 0)
        dut.set(_PREFIX + f"io_toIBuffer_bits_isLastInFtqEntry_{slot}", slot == entries[-1][0])
        dut.set(_PREFIX + f"io_toIBuffer_bits_ftqPtr_{slot}_flag", ftq_flag)
        dut.set(_PREFIX + f"io_toIBuffer_bits_ftqPtr_{slot}_value", ftq_value)
        dut.set(_PREFIX + f"io_toIBuffer_bits_exceptionMask_{slot}", exception_mask)
    dut.set(_PREFIX + "io_toIBuffer_bits_enqEnable", enq_enable)
    dut.set(_PREFIX + "io_toIBuffer_bits_valid", valid_mask | int(valid_mask_extra))
    dut.set(_PREFIX + "s2_prevIBufEnqPtr_value", prev_ibuf_enq_ptr)
    dut.set(_PREFIX + "s2_alignShiftNum", int(prev_ibuf_enq_ptr) & 0x3)
    dut.set(_PREFIX + "s2_instrCount", len(entries) if instr_count is None else instr_count)
    dut.set(_PREFIX + "s2_fire", 1)
    dut.set(_PREFIX + "s2_reqIsUncache", s2_req_is_uncache)
    dut.set(_PREFIX + "s2_prevEndIsHalfRvi", s2_prev_end_is_half_rvi)
    dut.set(_PREFIX + "s2_prevEndHalfPc_addr", int(s2_prev_end_half_pc) >> 1)
    dut.set(_PREFIX + "s2_prevEndHalfRviData", s2_prev_end_half_data)
    if s2_fetch_block_start_pc is None:
        s2_fetch_block_start_pc = entries[0][1]
    dut.set(
        _PREFIX + "s2_fetchBlock_0_startVAddr_addr",
        int(s2_fetch_block_start_pc) >> 1,
    )
    dut.set(_PREFIX + "wbRedirect_valid", 0)
    dut.set(_PREFIX + "uncacheRedirect_valid", 0)


def _set_aligned_slot(dut, slot, entry, *, block_sel, branch_type, rd=0, rs=0):
    _slot, pc, instr, is_rvc, end_offset, _flag, _value, _exception = entry
    dut.set(_PREFIX + f"s2_alignedInstrVec_{slot}_valid", 1)
    dut.set(_PREFIX + f"s2_alignedInstrPcVec_{slot}_addr", int(pc) >> 1)
    dut.set(_PREFIX + f"s2_alignedInstrVec_{slot}_isRvc", is_rvc)
    dut.set(_PREFIX + f"s2_alignedInstrVec_{slot}_blockSel", block_sel)
    dut.set(_PREFIX + f"s2_alignedInstrVec_{slot}_endOffset", end_offset)
    dut.set(_PREFIX + f"s2_expandedInstrDataVec_{slot}", instr)
    dut.set(_PREFIX + f"s2_alignedPdInfoVec_{slot}_brAttribute_branchType", branch_type)
    dut.set(_PREFIX + f"s2_alignedPdInfoVec_{slot}_brAttribute_rd", rd)
    dut.set(_PREFIX + f"s2_alignedPdInfoVec_{slot}_brAttribute_rs", rs)


def _set_predchecker_request(dut, entries):
    dut.set(_PREFIX + "predChecker.io_req_valid", 1)
    dut.set(_PREFIX + "predChecker.io_resp_stage2Out_checkerRedirect_valid", 0)
    for slot in range(36):
        dut.set(_PREFIX + f"predChecker.io_req_bits_instrVec_{slot}_valid", 0)
    for entry in entries:
        slot = int(entry["slot"])
        prefix = _PREFIX + f"predChecker.io_req_bits_instrVec_{slot}_"
        dut.set(prefix + "valid", 1)
        dut.set(prefix + "isPredTaken", entry.get("pred_taken", 0))
        dut.set(prefix + "invalidTaken", entry.get("invalid_taken", 0))
        dut.set(prefix + "isRvc", entry.get("is_rvc", 0))
        dut.set(prefix + "blockSel", entry.get("block_sel", 0))
        dut.set(prefix + "endOffset", entry.get("end_offset", slot))
        dut.set(
            _PREFIX
            + f"predChecker.io_req_bits_pdInfoVec_{slot}_brAttribute_branchType",
            entry.get("branch_type", 0),
        )
        dut.set(
            _PREFIX
            + f"predChecker.io_req_bits_pdInfoVec_{slot}_brAttribute_rasAction",
            entry.get("ras_action", 0),
        )
        dut.set(
            _PREFIX + f"predChecker.io_req_bits_instrPcVec_{slot}_addr",
            entry.get("pc_addr", 0x40000000 + slot * 2),
        )
        dut.set(
            _PREFIX + f"predChecker.io_req_bits_jumpOffsetVec_{slot}_addr",
            entry.get("jump_offset_addr", 4),
        )
        dut.set(
            _PREFIX + f"predChecker.io_resp_stage1Out_fixedInstrValid_{slot}",
            entry.get("fixed_valid", 1),
        )


def _set_predchecker_redirect(dut, pending, *, target):
    dut.set(_PREFIX + "predChecker.io_req_valid", 0)
    base = _PREFIX + "predChecker.io_resp_stage2Out_checkerRedirect_"
    dut.set(base + "valid", 1)
    dut.set(base + "bits_target_addr", target)
    dut.set(base + "bits_taken", 1)
    dut.set(base + "bits_invalidTaken", pending.get("invalid_taken", 0))
    dut.set(base + "bits_isRVC", pending.get("is_rvc", 0))
    dut.set(base + "bits_selectBlock", pending.get("block_sel", 0))
    dut.set(
        base + "bits_attribute_branchType",
        0 if pending.get("invalid_taken", 0) else pending.get("branch_type", 0),
    )
    dut.set(
        base + "bits_attribute_rasAction",
        0 if pending.get("invalid_taken", 0) else pending.get("ras_action", 0),
    )
    dut.set(base + "bits_endOffset", pending.get("end_offset", pending["slot"]))


def test_ifu_predchecker_v3_fault_types_and_no_fault_are_observed(tmp_path):
    recorder, env, dut, _memory = _make_recorder(tmp_path)
    fault_entries = (
        ("jal_not_taken", {"slot": 0, "branch_type": 2}),
        ("jalr_not_taken", {"slot": 0, "branch_type": 3}),
        ("ret_not_taken", {"slot": 0, "branch_type": 3, "ras_action": 1}),
        ("not_cfi_taken", {"slot": 0, "branch_type": 0, "pred_taken": 1}),
        ("invalid_taken", {"slot": 0, "branch_type": 0, "invalid_taken": 1}),
    )
    for cycle, (bin_name, entry) in enumerate(fault_entries, start=1):
        _set_predchecker_request(dut, [entry])
        sample_cfvec_coverage(recorder, env, cycle)
        assert recorder.key_hit("ifu_predchecker_v3_fault", bin_name)

    _set_predchecker_request(
        dut,
        [{"slot": 0, "branch_type": 0, "pred_taken": 0}],
    )
    sample_cfvec_coverage(recorder, env, 10)
    dut.set(_PREFIX + "predChecker.io_req_valid", 0)
    sample_cfvec_coverage(recorder, env, 11)

    assert recorder.key_hit("ifu_predchecker_v3_fault", "no_remask_fault")


def test_ifu_predchecker_v3_registered_invalid_taken_survives_back_to_back_s2(tmp_path):
    recorder, env, dut, _memory = _make_recorder(tmp_path)
    dut.set(_PREFIX + "predChecker.io_resp_stage2Out_checkerRedirect_valid", 1)
    dut.set(_PREFIX + "predChecker.invalidTakenNext", 1)

    sample_cfvec_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_predchecker_v3_fault", "invalid_taken")


def test_ifu_predchecker_v3_selects_first_fault_and_masks_younger_slots(tmp_path):
    recorder, env, dut, _memory = _make_recorder(tmp_path)
    _set_predchecker_request(
        dut,
        [
            {"slot": 1, "branch_type": 2, "fixed_valid": 1},
            {"slot": 3, "branch_type": 3, "fixed_valid": 0},
        ],
    )

    sample_cfvec_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_predchecker_v3_range", "earliest_fault_selected")
    assert recorder.key_hit("ifu_predchecker_v3_range", "fault_inclusive_younger_masked")


def test_ifu_predchecker_v3_redirect_target_and_metadata_follow_fault_kind(tmp_path):
    recorder, env, dut, _memory = _make_recorder(tmp_path)
    jal = {
        "slot": 0,
        "branch_type": 2,
        "pc_addr": 0x40000000,
        "jump_offset_addr": 4,
        "block_sel": 0,
        "end_offset": 3,
    }
    _set_predchecker_request(dut, [jal])
    sample_cfvec_coverage(recorder, env, 1)
    _set_predchecker_redirect(dut, jal, target=jal["pc_addr"] + jal["jump_offset_addr"])
    sample_cfvec_coverage(recorder, env, 2)

    jalr = {
        "slot": 2,
        "branch_type": 3,
        "pc_addr": 0x40000008,
        "block_sel": 1,
        "end_offset": 7,
    }
    _set_predchecker_request(dut, [jalr])
    sample_cfvec_coverage(recorder, env, 3)
    _set_predchecker_redirect(dut, jalr, target=jalr["pc_addr"] + 2)
    sample_cfvec_coverage(recorder, env, 4)

    assert recorder.key_hit("ifu_predchecker_v3_redirect", "target_by_fault_kind")
    assert recorder.key_hit(
        "ifu_predchecker_v3_redirect", "metadata_matches_earliest_fault"
    )


def test_ifu_compact_and_expander_bins_use_to_ibuffer_fire(tmp_path):
    recorder, env, dut, memory = _make_recorder(tmp_path)
    base = 0x80000000
    compressed = 0x0001
    memory.write16(base, compressed)
    memory.write16(base + 2, compressed)
    memory.write32(base + 4, 0x00000013)
    memory.write32(base + 8, 0x00000013)
    expanded = expand_rvc(compressed)
    _set_ifu_output(
        dut,
        [
            (0, base, expanded, 1, 0, 0, 1, 0),
            (1, base + 2, expanded, 1, 1, 0, 1, 0),
            (2, base + 4, 0x00000013, 0, 3, 0, 1, 0),
            (3, base + 8, 0x00000013, 0, 5, 0, 1, 0),
        ],
    )

    sample_cfvec_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_instr_compact", "contiguous_slots")
    assert recorder.key_hit("ifu_instr_compact", "rvi_single_slot")
    assert recorder.key_hit("ifu_instr_compact", "rvc_multi_slot")
    assert recorder.key_hit("ifu_instr_end_offset", "rvc_rvi_end_offset")
    assert recorder.key_hit("ifu_rvc_expander", "legal_rvc_expanded")
    assert recorder.key_hit("ifu_rvc_expander", "rvi_passthrough")
    assert recorder.key_hit("ifu_ibuffer_output", "instr_pc_isrvc_observed")
    assert recorder.key_hit("ifu_ibuffer_output", "ftq_offset_observed")
    assert recorder.key_hit("ifu_ibuffer_output", "last_in_ftq_entry")
    assert recorder.key_hit("ifu_ibuffer_output", "taken_end_metadata")
    assert recorder.key_hit("ifu_cacheable_boundary", "mixed_rvc_rvi")
    assert recorder.key_hit("ifu_cacheable_boundary", "rvi_high_half_rvc_like")
    assert recorder.key_hit("ifu_cacheable_compact", "raw_start_slots_observed")
    assert recorder.key_hit("ifu_cacheable_compact", "mixed_end_offset_observed")
    assert recorder.key_hit("ifu_cacheable_compact", "contiguous_slots_observed")
    assert recorder.key_hit("ifu_cacheable_expander", "legal_rvc_input_seen")
    assert recorder.key_hit("ifu_cacheable_expander", "rvi_input_seen")


def test_ifu_cacheable_boundary_homogeneous_sequences_are_observed(tmp_path):
    base = 0x80000000

    rvi, rvi_env, rvi_dut, rvi_memory = _make_recorder(tmp_path / "rvi")
    for offset in (0, 4, 8):
        rvi_memory.write32(base + offset, 0x00000013)
    _set_ifu_output(
        rvi_dut,
        [
            (0, base, 0x00000013, 0, 1, 0, 1, 0),
            (1, base + 4, 0x00000013, 0, 3, 0, 1, 0),
            (2, base + 8, 0x00000013, 0, 5, 0, 1, 0),
        ],
    )
    sample_cfvec_coverage(rvi, rvi_env, 1)
    assert rvi.key_hit("ifu_cacheable_boundary", "all_rvi_4b")

    rvc, rvc_env, rvc_dut, rvc_memory = _make_recorder(tmp_path / "rvc")
    expanded = expand_rvc(0x0001)
    for offset in (0, 2, 4):
        rvc_memory.write16(base + offset, 0x0001)
    _set_ifu_output(
        rvc_dut,
        [
            (0, base, expanded, 1, 0, 0, 1, 0),
            (1, base + 2, expanded, 1, 1, 0, 1, 0),
            (2, base + 4, expanded, 1, 2, 0, 1, 0),
        ],
    )
    sample_cfvec_coverage(rvc, rvc_env, 1)
    assert rvc.key_hit("ifu_cacheable_boundary", "all_rvc_2b")


def test_ifu_ibuffer_pointer_alignment_bins_use_real_s2_fields(tmp_path):
    recorder, env, dut, _memory = _make_recorder(tmp_path)
    base = 0x80000000

    _set_ifu_output(
        dut,
        [(0, base, 0x00000013, 0, 1, 0, 1, 0)],
        prev_ibuf_enq_ptr=0,
    )
    sample_cfvec_coverage(recorder, env, 1)
    assert recorder.key_hit("ifu_ibuffer_alignment", "zero_pointer_slot_zero")

    _set_ifu_output(
        dut,
        [
            (2, base + 4, 0x00000013, 0, 3, 0, 1, 0),
            (3, base + 8, 0x00000013, 0, 5, 0, 1, 0),
        ],
        prev_ibuf_enq_ptr=6,
    )
    sample_cfvec_coverage(recorder, env, 2)
    assert recorder.key_hit("ifu_ibuffer_alignment", "nonzero_shift_matches_slot")

    _set_ifu_output(
        dut,
        [
            (1, base + 12, 0x00000013, 0, 7, 0, 1, 0),
            (2, base + 16, 0x00000013, 0, 9, 0, 1, 0),
            (3, base + 20, 0x00000013, 0, 11, 0, 1, 0),
        ],
        prev_ibuf_enq_ptr=9,
        instr_count=40,
    )
    sample_cfvec_coverage(recorder, env, 3)
    assert recorder.key_hit("ifu_ibuffer_alignment", "wide_window_bounded")


def test_ifu_ibuffer_pointer_update_tracks_count_and_wraps(tmp_path):
    recorder, env, dut, _memory = _make_recorder(tmp_path)
    base = 0x80000000

    _set_ifu_output(
        dut,
        [(0, base, 0x00000013, 0, 1, 0, 1, 0)],
        prev_ibuf_enq_ptr=40,
        instr_count=12,
    )
    sample_cfvec_coverage(recorder, env, 1)
    assert not recorder.key_hit("ifu_ibuffer_alignment", "pointer_advance_matches_count")

    _set_ifu_output(
        dut,
        [(0, base + 4, 0x00000013, 0, 3, 0, 1, 0)],
        prev_ibuf_enq_ptr=4,
        instr_count=7,
    )
    sample_cfvec_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_ibuffer_alignment", "pointer_advance_matches_count")


def test_ifu_ibuffer_pointer_update_does_not_cross_redirect_or_uncache(tmp_path):
    recorder, env, dut, _memory = _make_recorder(tmp_path)
    base = 0x80000000

    _set_ifu_output(
        dut,
        [(1, base, 0x00000013, 0, 1, 0, 1, 0)],
        prev_ibuf_enq_ptr=5,
        instr_count=7,
    )
    sample_cfvec_coverage(recorder, env, 1)

    dut.set(_PREFIX + "wbRedirect_valid", 1)
    sample_cfvec_coverage(recorder, env, 2)
    _set_ifu_output(
        dut,
        [(0, base + 4, 0x00000013, 0, 3, 0, 1, 0)],
        prev_ibuf_enq_ptr=12,
        instr_count=4,
        s2_req_is_uncache=1,
    )
    sample_cfvec_coverage(recorder, env, 3)
    _set_ifu_output(
        dut,
        [(0, base + 8, 0x00000013, 0, 5, 0, 1, 0)],
        prev_ibuf_enq_ptr=16,
        instr_count=1,
    )
    sample_cfvec_coverage(recorder, env, 4)

    assert not recorder.key_hit("ifu_ibuffer_alignment", "pointer_advance_matches_count")


def test_ifu_instr_boundary_tail_half_is_sampled_on_cacheable_s1_fire(tmp_path):
    recorder, env, dut, _memory = _make_recorder(tmp_path)
    dut.set(_PREFIX + "s1_valid", 1)
    dut.set(_PREFIX + "s1_fire", 1)
    dut.set(_PREFIX + "s1_flush", 0)
    dut.set(_PREFIX + "s1_reqIsUncache", 0)
    dut.set(_PREFIX + "s1_totalEndIsHalfRvi", 1)
    dut.set(_PREFIX + "s1_totalEndPos", 31)

    sample_cfvec_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_instr_boundary_half", "tail_half_detected")
    assert recorder.key_hit("ifu_instr_boundary_v3", "tail_half_state")


def test_ifu_instr_boundary_cross_block_rvi_checks_data_and_pc(tmp_path):
    recorder, env, dut, memory = _make_recorder(tmp_path)
    pc = 0x8000003E
    raw = 0x00108093
    memory.write32(pc, raw)
    _set_ifu_output(
        dut,
        [(0, pc, raw, 0, 0, 0, 1, 0)],
        s2_prev_end_is_half_rvi=1,
        s2_prev_end_half_pc=pc,
        s2_prev_end_half_data=raw & 0xFFFF,
        s2_fetch_block_start_pc=pc + 2,
    )

    sample_cfvec_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_instr_boundary_half", "head_half_completion")
    assert recorder.key_hit("ifu_instr_boundary_half", "stitched_data_matches")
    assert recorder.key_hit("ifu_instr_boundary_half", "stitched_pc_uses_half_pc")
    assert recorder.key_hit("ifu_instr_boundary_source", "saved_half_selected")
    assert recorder.key_hit("ifu_instr_boundary_half", "saved_half_forwarded")
    assert recorder.key_hit("ifu_instr_boundary_alignment", "stitched_at_align_head")
    assert recorder.key_hit("ifu_instr_boundary_expansion", "stitched_single_rvi")
    assert recorder.key_hit("ifu_instr_boundary_v3", "next_block_completion")
    assert not recorder.key_hit("ifu_instr_boundary_v3", "continuation_after_stitch")


def test_ifu_instr_boundary_cross_block_continuation_checks_pc_steps(tmp_path):
    recorder, env, dut, memory = _make_recorder(tmp_path)
    pc = 0x8000003E
    raw = 0x00108093
    memory.write32(pc, raw)
    memory.write16(pc + 4, 0x0001)
    memory.write32(pc + 6, 0x00000013)
    _set_ifu_output(
        dut,
        [
            (0, pc, raw, 0, 0, 0, 1, 0),
            (1, pc + 4, expand_rvc(0x0001), 1, 1, 0, 1, 0),
            (2, pc + 6, 0x00000013, 0, 3, 0, 1, 0),
        ],
        s2_prev_end_is_half_rvi=1,
        s2_prev_end_half_pc=pc,
        s2_prev_end_half_data=raw & 0xFFFF,
        s2_fetch_block_start_pc=pc + 2,
    )

    sample_cfvec_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_instr_boundary_v3", "continuation_after_stitch")


def test_ifu_instr_boundary_cross_block_rvi_rejects_wrong_data_and_pc(tmp_path):
    recorder, env, dut, memory = _make_recorder(tmp_path)
    pc = 0x8000003E
    raw = 0x00108093
    memory.write32(pc, raw)
    _set_ifu_output(
        dut,
        [(0, pc, raw, 0, 31, 0, 1, 0)],
        s2_prev_end_is_half_rvi=1,
        s2_prev_end_half_pc=pc + 2,
        s2_prev_end_half_data=(raw + 1) & 0xFFFF,
        s2_fetch_block_start_pc=pc + 2,
    )

    sample_cfvec_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_instr_boundary_half", "head_half_completion")
    assert not recorder.key_hit("ifu_instr_boundary_half", "stitched_data_matches")
    assert not recorder.key_hit("ifu_instr_boundary_half", "stitched_pc_uses_half_pc")
    assert not recorder.key_hit("ifu_instr_boundary_source", "saved_half_selected")
    assert not recorder.key_hit("ifu_instr_boundary_half", "saved_half_forwarded")
    assert not recorder.key_hit("ifu_instr_boundary_v3", "next_block_completion")
    assert not recorder.key_hit("ifu_instr_boundary_v3", "continuation_after_stitch")


def test_ifu_ibuffer_output_range_clipping_requires_valid_slot_not_enabled(tmp_path):
    recorder, env, dut, memory = _make_recorder(tmp_path)
    base = 0x80000000
    memory.write32(base, 0x00000013)
    _set_ifu_output(
        dut,
        [(0, base, 0x00000013, 0, 1, 0, 1, 0)],
        valid_mask_extra=1 << 1,
    )

    sample_cfvec_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_ibuffer_output", "fixed_range_clipped")


def test_ifu_compact_two_fetch_source_requires_expected_ftq_order(tmp_path):
    recorder, env, dut, _memory = _make_recorder(tmp_path)
    recorder._two_fetch_expected_cfvec = {"tags": ((0, 3), (0, 4)), "cycle": 1}
    _set_ifu_output(
        dut,
        [
            (0, 0x80000000, 0x00000013, 0, 1, 0, 3, 0),
            (1, 0x80000004, 0x00000013, 0, 3, 0, 4, 0),
        ],
    )

    sample_cfvec_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_instr_compact_source", "two_fetch_select_block")
    assert recorder.key_hit("ifu_cacheable_compact", "two_fetch_source_observed")


def test_ifu_dual_slice_alignment_and_predecode_are_coherent(tmp_path):
    recorder, env, dut, memory = _make_recorder(tmp_path)
    base = 0x80000000
    entries = [
        (0, base, 0x00000013, 0, 1, 0, 3, 0),
        (1, base + 4, 0x00000063, 0, 3, 0, 3, 0),
        (2, base + 8, 0x000000EF, 0, 5, 0, 4, 0),
        (3, base + 12, 0x00008067, 0, 7, 0, 4, 0),
    ]
    for entry in entries:
        memory.write32(entry[1], entry[2])
    _set_ifu_output(dut, entries)
    for slot, entry in enumerate(entries):
        branch_type = (0, 1, 2, 3)[slot]
        rd = 1 if slot == 2 else 0
        rs = 1 if slot == 3 else 0
        _set_aligned_slot(
            dut,
            slot,
            entry,
            block_sel=0 if slot < 2 else 1,
            branch_type=branch_type,
            rd=rd,
            rs=rs,
        )
        if branch_type in {1, 2}:
            dut.set(_PREFIX + f"predChecker.io_req_bits_jumpOffsetVec_{slot}_addr", 0)
    dut.set(_PREFIX + "s2_fetchBlock_0_valid", 1)
    dut.set(_PREFIX + "s2_fetchBlock_1_valid", 1)
    dut.set(_PREFIX + "s2_fetchBlock_0_ftqIdx_flag", 0)
    dut.set(_PREFIX + "s2_fetchBlock_0_ftqIdx_value", 3)
    dut.set(_PREFIX + "s2_fetchBlock_1_ftqIdx_flag", 0)
    dut.set(_PREFIX + "s2_fetchBlock_1_ftqIdx_value", 4)

    sample_cfvec_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_cacheable_main_path", "dual_clean_delivery")
    assert recorder.key_hit("ifu_data_slice", "first_block_coherent")
    assert recorder.key_hit("ifu_data_slice", "second_block_source_coherent")
    assert recorder.key_hit("ifu_instr_compact_rank", "rank_matches_output_slot")
    assert recorder.key_hit("ifu_aligned_slot", "pc_data_valid_coherent")
    assert recorder.key_hit("ifu_predecode", "non_cfi_correct")
    assert recorder.key_hit("ifu_predecode", "branch_jal_jalr_correct")
    assert recorder.key_hit("ifu_predecode", "call_return_correct")
    assert recorder.key_hit("ifu_predecode", "cfi_offset_correct")
    assert recorder.key_hit("ifu_predecode", "slot_mapping_coherent")
    assert recorder.key_hit("ifu_ibuffer_output", "predecode_matches_encoding")


def test_ifu_backpressure_release_and_writeback_match_enqueue(tmp_path):
    recorder, env, dut, memory = _make_recorder(tmp_path)
    base = 0x80000000
    entries = [
        (0, base, 0x00000013, 0, 1, 0, 3, 0),
        (1, base + 4, 0x00000013, 0, 3, 0, 4, 0),
    ]
    for entry in entries:
        memory.write32(entry[1], entry[2])
    _set_ifu_output(dut, entries)
    dut.set(_PREFIX + "s1_valid", 0)
    dut.set(_PREFIX + "s1_ready", 1)
    dut.set(_PREFIX + "wbValid", 0)
    dut.set(_PREFIX + "wbInstrCount", 0)
    sample_cfvec_coverage(recorder, env, 1)

    dut.set(_PREFIX + "io_toIBuffer_ready", 0)
    dut.set(_PREFIX + "s1_valid", 1)
    dut.set(_PREFIX + "s1_ready", 0)
    sample_cfvec_coverage(recorder, env, 2)
    sample_cfvec_coverage(recorder, env, 3)

    dut.set(_PREFIX + "io_toIBuffer_ready", 1)
    dut.set(_PREFIX + "wbValid", 1)
    dut.set(_PREFIX + "wbInstrCount", 2)
    dut.set(_PREFIX + "wbAlignFetchBlock_0_ftqIdx_flag", 0)
    dut.set(_PREFIX + "wbAlignFetchBlock_0_ftqIdx_value", 3)
    dut.set(_PREFIX + "wbAlignFetchBlock_1_ftqIdx_flag", 0)
    dut.set(_PREFIX + "wbAlignFetchBlock_1_ftqIdx_value", 4)
    sample_cfvec_coverage(recorder, env, 4)

    assert recorder.key_hit("ifu_ibuffer_backpressure", "payload_stable")
    assert recorder.key_hit("ifu_ibuffer_backpressure", "held_payload_delivered")
    assert recorder.key_hit("ifu_ibuffer_backpressure", "upstream_stalled")
    assert recorder.key_hit("ifu_writeback", "ordinary_no_redirect")
    assert recorder.key_hit("ifu_writeback", "dual_fetch_sources_match")
    assert recorder.key_hit("ifu_writeback", "instr_count_matches_enq")


def test_ifu_illegal_rvc_and_fetch_exception_priority_bins(tmp_path):
    recorder, env, dut, memory = _make_recorder(tmp_path / "illegal")
    base = 0x80000000
    memory.write16(base, 0x0000)
    _set_ifu_output(dut, [(0, base, 0x00000000, 1, 0, 0, 1, 1)], exception_type=4)

    sample_cfvec_coverage(recorder, env, 1)

    assert recorder.key_hit("ifu_rvc_exception", "illegal_rvc")
    assert not recorder.key_hit("ifu_rvc_exception", "fetch_exception_over_illegal_rvc")

    priority, priority_env, priority_dut, priority_memory = _make_recorder(tmp_path / "fetch_priority")
    priority_memory.write16(base, 0x0000)
    _set_ifu_output(
        priority_dut,
        [(0, base, 0x00000000, 1, 0, 0, 1, 1)],
        exception_type=1,
    )

    sample_cfvec_coverage(priority, priority_env, 1)

    assert priority.key_hit("ifu_rvc_exception", "fetch_exception_over_illegal_rvc")
    assert not priority.key_hit("ifu_rvc_exception", "illegal_rvc")


def test_invalid_taken_fetch_exception_cross_is_stimulus_coverage(tmp_path):
    recorder, env, dut, _memory = _make_recorder(tmp_path)
    dut.set(_PREFIX + "s1_valid", 1)
    dut.set(_PREFIX + "s1_invalidTaken_0", 1)
    dut.set(_PREFIX + "s1_icacheMeta_0_exception_value", 0)
    dut.set(_PREFIX + "s1_instrCount", 1)
    dut.set(_PREFIX + "s1_flush", 0)

    sample_cfvec_coverage(recorder, env, 1)
    assert not recorder.key_hit("ifu_invalid_taken_exception", "observed")

    dut.set(_PREFIX + "s1_icacheMeta_0_exception_value", 3)
    # Coverage records the stimulus even if a pre-fix DUT computes the wrong
    # count. A checker must independently require instrCount == 1.
    dut.set(_PREFIX + "s1_instrCount", 4)
    sample_cfvec_coverage(recorder, env, 2)

    assert recorder.key_hit("ifu_invalid_taken_exception", "observed")


def test_ifu_compact_sampler_signals_are_present_in_generated_contract():
    root = Path(__file__).resolve().parents[7]
    offset = root / "build-frontend/pylib-verilator/Frontend/Frontend_offset.yaml"
    names = {
        line[len("  - name: ") :].strip()
        for line in offset.read_text(encoding="utf-8").splitlines()
        if line.startswith("  - name: ")
    }
    required = {
        _PREFIX + "io_toIBuffer_ready",
        _PREFIX + "io_toIBuffer_valid",
        _PREFIX + "io_toIBuffer_bits_enqEnable",
        _PREFIX + "io_toIBuffer_bits_valid",
        _PREFIX + "io_toIBuffer_bits_pc_0_addr",
        _PREFIX + "io_toIBuffer_bits_instrs_0",
        _PREFIX + "io_toIBuffer_bits_isRvc_0",
        _PREFIX + "io_toIBuffer_bits_instrEndOffset_0_offset",
        _PREFIX + "io_toIBuffer_bits_instrEndOffset_0_predTaken",
        _PREFIX + "io_toIBuffer_bits_instrEndOffset_0_fixedTaken",
        _PREFIX + "io_toIBuffer_bits_isLastInFtqEntry_0",
        _PREFIX + "io_toIBuffer_bits_exceptionType_value",
        _PREFIX + "io_toIBuffer_bits_exceptionMask_0",
        _PREFIX + "io_toIBuffer_bits_ftqPtr_0_flag",
        _PREFIX + "io_toIBuffer_bits_ftqPtr_0_value",
        _PREFIX + "s1_valid",
        _PREFIX + "s1_invalidTaken_0",
        _PREFIX + "s1_icacheMeta_0_exception_value",
        _PREFIX + "s1_instrCount",
        _PREFIX + "s1_fire",
        _PREFIX + "s1_flush",
        _PREFIX + "s1_reqIsUncache",
        _PREFIX + "s1_totalEndIsHalfRvi",
        _PREFIX + "s1_totalEndPos",
        _PREFIX + "s2_prevIBufEnqPtr_value",
        _PREFIX + "s2_instrCount",
        _PREFIX + "s2_fire",
        _PREFIX + "s2_reqIsUncache",
        _PREFIX + "s2_alignShiftNum",
        _PREFIX + "s2_prevEndIsHalfRvi",
        _PREFIX + "s2_prevEndHalfPc_addr",
        _PREFIX + "s2_prevEndHalfRviData",
        _PREFIX + "s2_fetchBlock_0_startVAddr_addr",
        _PREFIX + "wbRedirect_valid",
        _PREFIX + "uncacheRedirect_valid",
        "Frontend_top.Frontend.inner_ifu.predChecker.invalidTakenNext",
    }
    assert required <= names

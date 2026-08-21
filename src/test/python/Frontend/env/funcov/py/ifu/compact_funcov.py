from __future__ import annotations

from typing import Any, Optional

from ..common.dut import _read
from ..common.fetch_memory import _read_expected_fetch_raw, _recover_unavailable_instr
from ....support.rvc_decoder import expand_rvc


_IFU_INTERNAL_PREFIXES = (
    "Frontend_top.Frontend.inner_ifu.__Vtogcov__",
    "Frontend_top.Frontend.inner_ifu.",
    "Frontend_top.Frontend._inner_ifu_",
)
_IFU_OUTPUT_SLOT_COUNT = 36
_IBUFFER_ENTRY_COUNT = 48
_FETCH_EXCEPTION_VALUES = frozenset({1, 2, 3, 5})


def _read_ifu_internal(recorder, dut, stem: str) -> Optional[int]:
    return recorder._read_first_dut_signal(
        dut,
        tuple(prefix + str(stem) for prefix in _IFU_INTERNAL_PREFIXES),
    )


def _read_ifu_output_slot(recorder, dut, field: str, slot: int, suffix: str = "") -> Optional[int]:
    return _read_ifu_internal(recorder, dut, f"io_toIBuffer_bits_{field}_{int(slot)}{suffix}")


def _decode_pruned_pc(encoded_pc: Optional[int]) -> Optional[int]:
    """Restore the byte address carried by the IFU PrunedAddr bundle.

    ``PrunedAddr.addr`` omits the instruction-alignment bit, so the generated
    DUT signal is a halfword address rather than a byte address.
    """

    return None if encoded_pc is None else int(encoded_pc) << 1


def _active_ifu_output_slots(enq_enable: int, valid_mask: int) -> list[int]:
    active_mask = int(enq_enable) & int(valid_mask)
    return [slot for slot in range(_IFU_OUTPUT_SLOT_COUNT) if active_mask & (1 << slot)]


def _is_contiguous(slots: list[int]) -> bool:
    return len(slots) >= 2 and slots == list(range(slots[0], slots[-1] + 1))


def _records_follow_instruction_boundaries(records: list[dict[str, Any]]) -> bool:
    if len(records) < 2:
        return False
    for before, after in zip(records, records[1:]):
        if None in {before["pc"], before["is_rvc"], after["pc"]}:
            return False
        expected_pc = int(before["pc"]) + (2 if int(before["is_rvc"]) else 4)
        if int(after["pc"]) != expected_pc:
            return False
    return True


def _read_raw_instruction(env, pc: int, is_rvc: bool) -> Optional[int]:
    raw, metadata = _read_expected_fetch_raw(env, int(pc), 2 if bool(is_rvc) else 4)
    if raw is None or not bool(metadata.get("ok", False)):
        return None
    return int(raw) & (0xFFFF if bool(is_rvc) else 0xFFFFFFFF)


def _sample_invalid_taken_exception_cross(recorder, dut, cycle: int) -> None:
    s1_valid = _read_ifu_internal(recorder, dut, "s1_valid")
    invalid_taken = _read_ifu_internal(recorder, dut, "s1_invalidTaken_0")
    exception = _read_ifu_internal(recorder, dut, "s1_icacheMeta_0_exception_value")
    instr_count = _read_ifu_internal(recorder, dut, "s1_instrCount")
    s1_flush = _read_ifu_internal(recorder, dut, "s1_flush")
    if None in {s1_valid, invalid_taken, exception, instr_count, s1_flush}:
        return
    if (
        int(s1_valid) == 1
        and int(invalid_taken) == 1
        and int(exception) in _FETCH_EXCEPTION_VALUES
        and int(s1_flush) == 0
    ):
        recorder.mark(
            "ifu_invalid_taken_exception",
            "observed",
            cycle,
            {
                "event": "ifu_s1_invalid_taken_with_fetch_exception",
                "exception_type": int(exception),
                "s1_instr_count": int(instr_count),
            },
        )


def _sample_instr_boundary_tail(recorder, dut, cycle: int) -> None:
    s1_valid = _read_ifu_internal(recorder, dut, "s1_valid")
    s1_fire = _read_ifu_internal(recorder, dut, "s1_fire")
    s1_flush = _read_ifu_internal(recorder, dut, "s1_flush")
    s1_req_is_uncache = _read_ifu_internal(recorder, dut, "s1_reqIsUncache")
    total_end_is_half_rvi = _read_ifu_internal(recorder, dut, "s1_totalEndIsHalfRvi")
    total_end_pos = _read_ifu_internal(recorder, dut, "s1_totalEndPos")
    if None in {s1_valid, s1_fire, s1_flush, s1_req_is_uncache, total_end_is_half_rvi}:
        return
    if (
        int(s1_valid) == 1
        and int(s1_fire) == 1
        and int(s1_flush) == 0
        and int(s1_req_is_uncache) == 0
        and int(total_end_is_half_rvi) == 1
    ):
        recorder.mark(
            "ifu_instr_boundary_half",
            "tail_half_detected",
            cycle,
            {
                "event": "ifu_s1_boundary_tail_half_rvi",
                "s1_total_end_pos": total_end_pos,
                "s1_total_end_is_half_rvi": 1,
            },
        )
        recorder.mark(
            "ifu_instr_boundary_v2",
            "tail_half_state",
            cycle,
            {
                "event": "ifu_s1_v2_tail_half_state",
                "s1_total_end_pos": total_end_pos,
                "s1_total_end_is_half_rvi": 1,
            },
        )


def _sample_instr_compact_coverage(recorder, env, cycle: int) -> None:
    dut = getattr(env, "dut", None)
    if dut is None:
        return

    _sample_invalid_taken_exception_cross(recorder, dut, cycle)
    _sample_instr_boundary_tail(recorder, dut, cycle)

    wb_redirect = _read_ifu_internal(recorder, dut, "wbRedirect_valid")
    uncache_redirect = _read_ifu_internal(recorder, dut, "uncacheRedirect_valid")
    pointer_redirect = wb_redirect == 1 or uncache_redirect == 1
    if pointer_redirect:
        recorder._ifu_ibuffer_alignment_pending = None

    ready = _read_ifu_internal(recorder, dut, "io_toIBuffer_ready")
    valid = _read_ifu_internal(recorder, dut, "io_toIBuffer_valid")
    enq_enable = _read_ifu_internal(recorder, dut, "io_toIBuffer_bits_enqEnable")
    valid_mask = _read_ifu_internal(recorder, dut, "io_toIBuffer_bits_valid")
    if None in {ready, valid, enq_enable, valid_mask}:
        return
    if int(ready) != 1 or int(valid) != 1:
        return

    slots = _active_ifu_output_slots(int(enq_enable), int(valid_mask))
    if not slots:
        return

    exception_type = _read_ifu_internal(recorder, dut, "io_toIBuffer_bits_exceptionType_value")
    records: list[dict[str, Any]] = []
    for slot in slots:
        pc = _decode_pruned_pc(_read_ifu_output_slot(recorder, dut, "pc", slot, "_addr"))
        instr = _read_ifu_output_slot(recorder, dut, "instrs", slot)
        is_rvc = _read_ifu_output_slot(recorder, dut, "isRvc", slot)
        end_offset = _read_ifu_output_slot(recorder, dut, "instrEndOffset", slot, "_offset")
        pred_taken = _read_ifu_output_slot(recorder, dut, "instrEndOffset", slot, "_predTaken")
        fixed_taken = _read_ifu_output_slot(recorder, dut, "instrEndOffset", slot, "_fixedTaken")
        is_last_in_ftq_entry = _read_ifu_output_slot(recorder, dut, "isLastInFtqEntry", slot)
        exception_mask = _read_ifu_output_slot(recorder, dut, "exceptionMask", slot)
        ftq_flag = _read_ifu_output_slot(recorder, dut, "ftqPtr", slot, "_flag")
        ftq_value = _read_ifu_output_slot(recorder, dut, "ftqPtr", slot, "_value")
        records.append(
            {
                "slot": int(slot),
                "pc": pc,
                "instr": instr,
                "is_rvc": is_rvc,
                "end_offset": end_offset,
                "pred_taken": pred_taken,
                "fixed_taken": fixed_taken,
                "is_last_in_ftq_entry": is_last_in_ftq_entry,
                "exception_mask": exception_mask,
                "ftq_ptr": None if ftq_flag is None or ftq_value is None else (int(ftq_flag), int(ftq_value)),
            }
        )

    evidence = {
        "event": "ifu_to_ibuffer_fire",
        "slots": records,
        "enq_enable": int(enq_enable),
        "valid_mask": int(valid_mask),
        "exception_type": exception_type,
    }

    prev_ibuf_enq_ptr = _read_ifu_internal(recorder, dut, "s2_prevIBufEnqPtr_value")
    align_shift_num = _read_ifu_internal(recorder, dut, "s2_alignShiftNum")
    instr_count = _read_ifu_internal(recorder, dut, "s2_instrCount")
    s2_fire = _read_ifu_internal(recorder, dut, "s2_fire")
    s2_req_is_uncache = _read_ifu_internal(recorder, dut, "s2_reqIsUncache")
    if (
        None not in {prev_ibuf_enq_ptr, align_shift_num, instr_count, s2_fire}
        and int(s2_fire) == 1
    ):
        alignment_evidence = {
            **evidence,
            "event": "ifu_s2_ibuffer_alignment",
            "prev_ibuf_enq_ptr": int(prev_ibuf_enq_ptr),
            "align_shift_num": int(align_shift_num),
            "instr_count": int(instr_count),
        }
        first_slot = int(slots[0])
        if int(prev_ibuf_enq_ptr) == 0 and int(align_shift_num) == 0 and first_slot == 0:
            recorder.mark(
                "ifu_ibuffer_alignment",
                "zero_pointer_slot_zero",
                cycle,
                alignment_evidence,
            )
        if (
            int(prev_ibuf_enq_ptr) != 0
            and int(align_shift_num) != 0
            and int(align_shift_num) == (int(prev_ibuf_enq_ptr) & 0x3)
            and first_slot == int(align_shift_num)
        ):
            recorder.mark(
                "ifu_ibuffer_alignment",
                "nonzero_shift_matches_slot",
                cycle,
                alignment_evidence,
            )
        if int(instr_count) > _IFU_OUTPUT_SLOT_COUNT and len(slots) <= _IFU_OUTPUT_SLOT_COUNT:
            recorder.mark(
                "ifu_ibuffer_alignment",
                "wide_window_bounded",
                cycle,
                alignment_evidence,
            )

        pending = getattr(recorder, "_ifu_ibuffer_alignment_pending", None)
        if s2_req_is_uncache == 0 and not pointer_redirect:
            if pending is not None:
                expected_ptr = (
                    int(pending["prev_ibuf_enq_ptr"]) + int(pending["instr_count"])
                ) % _IBUFFER_ENTRY_COUNT
                update_evidence = {
                    **alignment_evidence,
                    "event": "ifu_s2_ibuffer_pointer_update",
                    "previous_cycle": int(pending["cycle"]),
                    "previous_prev_ibuf_enq_ptr": int(pending["prev_ibuf_enq_ptr"]),
                    "previous_instr_count": int(pending["instr_count"]),
                    "expected_prev_ibuf_enq_ptr": int(expected_ptr),
                }
                if int(prev_ibuf_enq_ptr) == expected_ptr:
                    recorder.mark(
                        "ifu_ibuffer_alignment",
                        "pointer_advance_matches_count",
                        cycle,
                        update_evidence,
                    )
                else:
                    recorder.risk_observations.append(
                        {**update_evidence, "event": "ifu_s2_ibuffer_pointer_update_mismatch"}
                    )
            recorder._ifu_ibuffer_alignment_pending = {
                "cycle": int(cycle),
                "prev_ibuf_enq_ptr": int(prev_ibuf_enq_ptr),
                "instr_count": int(instr_count),
            }
        else:
            recorder._ifu_ibuffer_alignment_pending = None

        prev_end_is_half_rvi = _read_ifu_internal(recorder, dut, "s2_prevEndIsHalfRvi")
        prev_end_half_pc = _decode_pruned_pc(
            _read_ifu_internal(recorder, dut, "s2_prevEndHalfPc_addr")
        )
        prev_end_half_data = _read_ifu_internal(recorder, dut, "s2_prevEndHalfRviData")
        fetch_block_start_pc = _decode_pruned_pc(
            _read_ifu_internal(recorder, dut, "s2_fetchBlock_0_startVAddr_addr")
        )
        first_record = records[0]
        if (
            s2_req_is_uncache == 0
            and prev_end_is_half_rvi == 1
            and first_record["is_rvc"] == 0
            and int(first_record["slot"]) == int(align_shift_num)
        ):
            half_evidence = {
                **alignment_evidence,
                "event": "ifu_s2_cross_block_rvi_completion",
                "previous_half_pc": prev_end_half_pc,
                "previous_half_data": prev_end_half_data,
                "fetch_block_start_pc": fetch_block_start_pc,
                "first_record": first_record,
            }
            recorder.mark(
                "ifu_instr_boundary_half",
                "head_half_completion",
                cycle,
                half_evidence,
            )
            pc_matches = first_record["pc"] == prev_end_half_pc
            if pc_matches:
                recorder.mark(
                    "ifu_instr_boundary_half",
                    "stitched_pc_uses_half_pc",
                    cycle,
                    half_evidence,
                )
            data_matches = False
            raw = None
            if None not in {first_record["pc"], first_record["instr"], prev_end_half_data}:
                raw = _read_raw_instruction(env, int(first_record["pc"]), False)
                data_matches = (
                    raw is not None
                    and (int(raw) & 0xFFFF) == int(prev_end_half_data)
                    and (int(first_record["instr"]) & 0xFFFFFFFF) == int(raw)
                )
                if data_matches:
                    recorder.mark(
                        "ifu_instr_boundary_half",
                        "stitched_data_matches",
                        cycle,
                        {**half_evidence, "raw": int(raw)},
                    )
            complete_evidence = {
                **half_evidence,
                "raw": raw,
                "pc_matches": bool(pc_matches),
                "data_matches": bool(data_matches),
            }
            starts_on_high_half = (
                fetch_block_start_pc is not None
                and prev_end_half_pc is not None
                and int(fetch_block_start_pc) == int(prev_end_half_pc) + 2
            )
            if starts_on_high_half and pc_matches and data_matches:
                recorder.mark(
                    "ifu_instr_boundary_source",
                    "saved_half_selected",
                    cycle,
                    complete_evidence,
                )
                recorder.mark(
                    "ifu_instr_boundary_v2",
                    "next_block_completion",
                    cycle,
                    complete_evidence,
                )
            if pc_matches and data_matches:
                recorder.mark(
                    "ifu_instr_boundary_half",
                    "saved_half_forwarded",
                    cycle,
                    complete_evidence,
                )
            if first_record["end_offset"] == 0:
                recorder.mark(
                    "ifu_instr_boundary_alignment",
                    "stitched_at_align_head",
                    cycle,
                    complete_evidence,
                )
            no_halfword_duplicate = all(
                record["pc"] is None
                or first_record["pc"] is None
                or int(record["pc"]) != int(first_record["pc"]) + 2
                for record in records[1:]
            )
            if pc_matches and data_matches and no_halfword_duplicate:
                recorder.mark(
                    "ifu_instr_boundary_expansion",
                    "stitched_single_rvi",
                    cycle,
                    complete_evidence,
                )
            if (
                starts_on_high_half
                and pc_matches
                and data_matches
                and _records_follow_instruction_boundaries(records)
            ):
                recorder.mark(
                    "ifu_instr_boundary_v2",
                    "continuation_after_stitch",
                    cycle,
                    complete_evidence,
                )

    raw_records = []
    for record in records:
        if None in {record["pc"], record["instr"], record["is_rvc"]}:
            continue
        raw = _read_raw_instruction(env, int(record["pc"]), bool(record["is_rvc"]))
        if raw is not None:
            raw_records.append({"slot": record["slot"], "raw": raw})
    if raw_records:
        recorder.mark(
            "ifu_ibuffer_output",
            "instr_pc_isrvc_observed",
            cycle,
            {**evidence, "raw_records": raw_records},
        )
        recorder.mark(
            "ifu_cacheable_compact",
            "raw_start_slots_observed",
            cycle,
            {**evidence, "raw_records": raw_records},
        )

    if all(record["ftq_ptr"] is not None and record["end_offset"] is not None for record in records):
        recorder.mark("ifu_ibuffer_output", "ftq_offset_observed", cycle, evidence)

    if int(valid_mask) & ~int(enq_enable):
        recorder.mark("ifu_ibuffer_output", "fixed_range_clipped", cycle, evidence)

    if any(record["is_last_in_ftq_entry"] == 1 for record in records):
        recorder.mark("ifu_ibuffer_output", "last_in_ftq_entry", cycle, evidence)

    if all(
        None not in {record["pred_taken"], record["fixed_taken"], record["end_offset"]}
        for record in records
    ) and any(record["pred_taken"] == 1 or record["fixed_taken"] == 1 for record in records):
        recorder.mark("ifu_ibuffer_output", "taken_end_metadata", cycle, evidence)
    if _is_contiguous(slots):
        recorder.mark("ifu_instr_compact", "contiguous_slots", cycle, evidence)
        recorder.mark(
            "ifu_cacheable_compact", "contiguous_slots_observed", cycle, evidence
        )

    typed_records = [
        record
        for record in records
        if record["pc"] is not None and record["is_rvc"] in {0, 1}
    ]
    if len(typed_records) >= 2:
        all_rvi = all(record["is_rvc"] == 0 for record in typed_records) and all(
            int(after["pc"]) - int(before["pc"]) == 4
            for before, after in zip(typed_records, typed_records[1:])
        )
        all_rvc = all(record["is_rvc"] == 1 for record in typed_records) and all(
            int(after["pc"]) - int(before["pc"]) == 2
            for before, after in zip(typed_records, typed_records[1:])
        )
        mixed = {record["is_rvc"] for record in typed_records} == {0, 1} and all(
            int(after["pc"]) - int(before["pc"])
            == (2 if before["is_rvc"] == 1 else 4)
            for before, after in zip(typed_records, typed_records[1:])
        )
        if all_rvi:
            recorder.mark("ifu_cacheable_boundary", "all_rvi_4b", cycle, evidence)
        if all_rvc:
            recorder.mark("ifu_cacheable_boundary", "all_rvc_2b", cycle, evidence)
        if mixed:
            recorder.mark("ifu_cacheable_boundary", "mixed_rvc_rvi", cycle, evidence)

    for record in typed_records:
        if record["is_rvc"] != 0:
            continue
        raw = _read_raw_instruction(env, int(record["pc"]), False)
        if raw is not None and ((int(raw) >> 16) & 0x3) != 0x3:
            recorder.mark(
                "ifu_cacheable_boundary",
                "rvi_high_half_rvc_like",
                cycle,
                {**evidence, "slot": record["slot"], "raw": int(raw)},
            )
            break

    if any(record["is_rvc"] == 0 for record in records):
        recorder.mark("ifu_instr_compact", "rvi_single_slot", cycle, evidence)

    for before, after in zip(records, records[1:]):
        if (
            before["is_rvc"] == 1
            and after["is_rvc"] == 1
            and before["pc"] is not None
            and after["pc"] is not None
            and int(after["pc"]) - int(before["pc"]) == 2
        ):
            recorder.mark("ifu_instr_compact", "rvc_multi_slot", cycle, evidence)
            break

    has_rvc_end = False
    has_rvi_end = False
    for before, after in zip(records, records[1:]):
        if any(value is None for value in (before["pc"], after["pc"], before["end_offset"], after["end_offset"])):
            continue
        pc_step = int(after["pc"]) - int(before["pc"])
        end_step = int(after["end_offset"]) - int(before["end_offset"])
        has_rvc_end |= before["is_rvc"] == 1 and pc_step == 2 and end_step == 1
        has_rvi_end |= before["is_rvc"] == 0 and pc_step == 4 and end_step == 2
    if has_rvc_end and has_rvi_end:
        recorder.mark("ifu_instr_end_offset", "rvc_rvi_end_offset", cycle, evidence)
        recorder.mark(
            "ifu_cacheable_compact", "mixed_end_offset_observed", cycle, evidence
        )

    expected = getattr(recorder, "_two_fetch_expected_cfvec", None)
    expected_tags = tuple(expected.get("tags") or ()) if isinstance(expected, dict) else ()
    source_tags: list[tuple[int, int]] = []
    for record in records:
        tag = record["ftq_ptr"]
        if tag is not None and tag not in source_tags:
            source_tags.append(tag)
    if len(expected_tags) == 2 and tuple(source_tags[:2]) == expected_tags:
        recorder.mark("ifu_instr_compact_source", "two_fetch_select_block", cycle, evidence)
        recorder.mark(
            "ifu_cacheable_compact", "two_fetch_source_observed", cycle, evidence
        )

    for record in records:
        pc = record["pc"]
        instr = record["instr"]
        is_rvc = record["is_rvc"]
        if pc is None or instr is None or is_rvc is None:
            continue
        raw = _read_raw_instruction(env, int(pc), bool(is_rvc))
        if raw is None:
            continue
        if int(is_rvc) == 0:
            recorder.mark(
                "ifu_cacheable_expander",
                "rvi_input_seen",
                cycle,
                {**evidence, "slot": record["slot"], "raw": raw},
            )
            if int(instr) & 0xFFFFFFFF == int(raw):
                recorder.mark(
                    "ifu_rvc_expander",
                    "rvi_passthrough",
                    cycle,
                    {**evidence, "slot": record["slot"], "raw": raw},
                )
            continue

        try:
            expanded = int(expand_rvc(int(raw))) & 0xFFFFFFFF
        except ValueError:
            mask_hit = record["exception_mask"] == 1
            if int(exception_type or 0) == 4 and mask_hit:
                recorder.mark(
                    "ifu_rvc_exception",
                    "illegal_rvc",
                    cycle,
                    {**evidence, "slot": record["slot"], "raw": raw},
                )
            elif int(exception_type or 0) in _FETCH_EXCEPTION_VALUES and mask_hit:
                recorder.mark(
                    "ifu_rvc_exception",
                    "fetch_exception_over_illegal_rvc",
                    cycle,
                    {**evidence, "slot": record["slot"], "raw": raw},
                )
            continue
        recorder.mark(
            "ifu_cacheable_expander",
            "legal_rvc_input_seen",
            cycle,
            {**evidence, "slot": record["slot"], "raw": raw},
        )
        if int(instr) & 0xFFFFFFFF == expanded:
            recorder.mark(
                "ifu_rvc_expander",
                "legal_rvc_expanded",
                cycle,
                {**evidence, "slot": record["slot"], "raw": raw, "expanded": expanded},
            )


COMPACT_COVERPOINTS = {
    "ifu_cacheable_boundary": "sequence_shape",
    "ifu_cacheable_compact": "output_shape",
    "ifu_cacheable_expander": "input_type",
    "ifu_ibuffer_alignment": "pointer_alignment",
    "ifu_ibuffer_output": "field_observation",
    "ifu_invalid_taken_exception": "stimulus_cross",
    "ifu_instr_boundary_alignment": "output_slot",
    "ifu_instr_boundary_expansion": "width_preservation",
    "ifu_instr_boundary_half": "cross_block_state",
    "ifu_instr_boundary_source": "high_half_entry",
    "ifu_instr_boundary_v2": "cross_block_delivery",
    "ifu_instr_compact": "instruction_layout",
    "ifu_instr_compact_source": "two_fetch_source",
    "ifu_instr_end_offset": "end_offset",
    "ifu_rvc_expander": "expansion_mode",
    "ifu_rvc_exception": "exception_mode",
}

COMPACT_SAMPLER_BIN_KEYS = frozenset(
    {
        ("ifu_cacheable_boundary", "all_rvi_4b"),
        ("ifu_cacheable_boundary", "all_rvc_2b"),
        ("ifu_cacheable_boundary", "mixed_rvc_rvi"),
        ("ifu_cacheable_boundary", "rvi_high_half_rvc_like"),
        ("ifu_cacheable_compact", "raw_start_slots_observed"),
        ("ifu_cacheable_compact", "two_fetch_source_observed"),
        ("ifu_cacheable_compact", "mixed_end_offset_observed"),
        ("ifu_cacheable_compact", "contiguous_slots_observed"),
        ("ifu_cacheable_expander", "legal_rvc_input_seen"),
        ("ifu_cacheable_expander", "rvi_input_seen"),
        ("ifu_ibuffer_alignment", "zero_pointer_slot_zero"),
        ("ifu_ibuffer_alignment", "nonzero_shift_matches_slot"),
        ("ifu_ibuffer_alignment", "wide_window_bounded"),
        ("ifu_ibuffer_alignment", "pointer_advance_matches_count"),
        ("ifu_ibuffer_output", "instr_pc_isrvc_observed"),
        ("ifu_ibuffer_output", "ftq_offset_observed"),
        ("ifu_ibuffer_output", "fixed_range_clipped"),
        ("ifu_ibuffer_output", "last_in_ftq_entry"),
        ("ifu_ibuffer_output", "taken_end_metadata"),
        ("ifu_invalid_taken_exception", "observed"),
        ("ifu_instr_boundary_alignment", "stitched_at_align_head"),
        ("ifu_instr_boundary_expansion", "stitched_single_rvi"),
        ("ifu_instr_boundary_half", "tail_half_detected"),
        ("ifu_instr_boundary_half", "head_half_completion"),
        ("ifu_instr_boundary_half", "saved_half_forwarded"),
        ("ifu_instr_boundary_half", "stitched_data_matches"),
        ("ifu_instr_boundary_half", "stitched_pc_uses_half_pc"),
        ("ifu_instr_boundary_source", "saved_half_selected"),
        ("ifu_instr_boundary_v2", "tail_half_state"),
        ("ifu_instr_boundary_v2", "next_block_completion"),
        ("ifu_instr_boundary_v2", "continuation_after_stitch"),
        ("ifu_instr_compact", "contiguous_slots"),
        ("ifu_instr_compact", "rvi_single_slot"),
        ("ifu_instr_compact", "rvc_multi_slot"),
        ("ifu_instr_compact_source", "two_fetch_select_block"),
        ("ifu_instr_end_offset", "rvc_rvi_end_offset"),
        ("ifu_rvc_expander", "legal_rvc_expanded"),
        ("ifu_rvc_expander", "rvi_passthrough"),
        ("ifu_rvc_exception", "illegal_rvc"),
        ("ifu_rvc_exception", "fetch_exception_over_illegal_rvc"),
    }
)


def sample_compact_coverage(recorder, env, cycle: int) -> None:
    _sample_instr_compact_coverage(recorder, env, cycle)


__all__ = ["COMPACT_COVERPOINTS", "COMPACT_SAMPLER_BIN_KEYS", "sample_compact_coverage"]

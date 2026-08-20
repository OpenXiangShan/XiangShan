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


def _sample_instr_compact_coverage(recorder, env, cycle: int) -> None:
    dut = getattr(env, "dut", None)
    if dut is None:
        return

    _sample_invalid_taken_exception_cross(recorder, dut, cycle)

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

    expected = getattr(recorder, "_two_fetch_expected_cfvec", None)
    expected_tags = tuple(expected.get("tags") or ()) if isinstance(expected, dict) else ()
    source_tags: list[tuple[int, int]] = []
    for record in records:
        tag = record["ftq_ptr"]
        if tag is not None and tag not in source_tags:
            source_tags.append(tag)
    if len(expected_tags) == 2 and tuple(source_tags[:2]) == expected_tags:
        recorder.mark("ifu_instr_compact_source", "two_fetch_select_block", cycle, evidence)

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
        if int(instr) & 0xFFFFFFFF == expanded:
            recorder.mark(
                "ifu_rvc_expander",
                "legal_rvc_expanded",
                cycle,
                {**evidence, "slot": record["slot"], "raw": raw, "expanded": expanded},
            )


COMPACT_COVERPOINTS = {
    "ifu_ibuffer_output": "field_observation",
    "ifu_invalid_taken_exception": "stimulus_cross",
    "ifu_instr_compact": "instruction_layout",
    "ifu_instr_compact_source": "two_fetch_source",
    "ifu_instr_end_offset": "end_offset",
    "ifu_rvc_expander": "expansion_mode",
    "ifu_rvc_exception": "exception_mode",
}

COMPACT_SAMPLER_BIN_KEYS = frozenset(
    {
        ("ifu_ibuffer_output", "instr_pc_isrvc_observed"),
        ("ifu_ibuffer_output", "ftq_offset_observed"),
        ("ifu_ibuffer_output", "fixed_range_clipped"),
        ("ifu_ibuffer_output", "last_in_ftq_entry"),
        ("ifu_ibuffer_output", "taken_end_metadata"),
        ("ifu_invalid_taken_exception", "observed"),
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

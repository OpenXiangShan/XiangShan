from __future__ import annotations

from typing import Any, Optional

from ..common.dut import _read
from ..common.fetch_memory import _read_expected_fetch_raw, _recover_unavailable_instr
from ....rvc_decoder import expand_rvc


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


def _sample_instr_compact_coverage(recorder, env, cycle: int) -> None:
    dut = getattr(env, "dut", None)
    if dut is None:
        return

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
        pc = _read_ifu_output_slot(recorder, dut, "pc", slot, "_addr")
        instr = _read_ifu_output_slot(recorder, dut, "instrs", slot)
        is_rvc = _read_ifu_output_slot(recorder, dut, "isRvc", slot)
        end_offset = _read_ifu_output_slot(recorder, dut, "instrEndOffset", slot, "_offset")
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
                "exception_mask": exception_mask,
                "ftq_ptr": None if ftq_flag is None or ftq_value is None else (int(ftq_flag), int(ftq_value)),
            }
        )

    evidence = {
        "event": "ifu_to_ibuffer_fire",
        "slots": records,
        "exception_type": exception_type,
    }
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

    has_rvc_end = any(record["is_rvc"] == 1 and record["end_offset"] == 0 for record in records)
    has_rvi_end = any(record["is_rvc"] == 0 and record["end_offset"] == 1 for record in records)
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


def initialize_ifu_coverage_state(recorder) -> None:
    recorder._ifu_last_cfvec = None
    recorder._ifu_redirect_skip_until_cycle = None


def reset_ifu_coverage_state(recorder) -> None:
    initialize_ifu_coverage_state(recorder)


def handle_ifu_event(recorder, event: dict) -> None:
    if str(event.get("type", "")) != "backend.redirect":
        return
    cycle = int(event.get("cycle", 0))
    recorder._ifu_last_cfvec = None
    recorder._ifu_redirect_skip_until_cycle = cycle + 1


def _classify_block_pos(pc: int) -> str:
    halfword = (int(pc) & 0x3F) >> 1
    if halfword <= 1:
        return "head"
    if halfword >= 30:
        return "tail"
    return "mid"


def _classify_cfi_kind(instr: int, is_rvc: bool) -> str:
    instr = int(instr) & 0xFFFFFFFF
    # Frontend cfVec/IBuffer carries the expanded 32-bit instruction even
    # when bits_isRvc is set. Decode the expanded opcode; only the width
    # flag remains compressed metadata.
    opcode = instr & 0x7F
    if opcode == 0x63:
        return "branch"
    if opcode == 0x6F:
        return "jal"
    if opcode == 0x67:
        return "jalr"
    return "non_cfi"


def _sample_ifu_cfvec_coverage(recorder, cycle: int, slot: int, pc: int, instr: int, is_rvc: bool) -> None:
    size_bin = "rvc_seen" if bool(is_rvc) else "rvi_seen"
    pos_bin = _classify_block_pos(int(pc))
    cfi_bin = _classify_cfi_kind(int(instr), bool(is_rvc))
    evidence = {
        "event": "cfvec",
        "slot": int(slot),
        "pc": int(pc),
        "instr": int(instr) & 0xFFFFFFFF,
        "is_rvc": int(bool(is_rvc)),
        "block_pos": pos_bin,
        "cfi_kind": cfi_bin,
    }

    recorder.mark("ifu_instr_size_type", size_bin, cycle, evidence)
    recorder.mark("ifu_fetch_block_position", pos_bin, cycle, evidence)
    if cfi_bin != "jalr":
        recorder.mark("ifu_cfi_decode_type", cfi_bin, cycle, evidence)

    page = int(pc) & ~0xFFF
    page_tail = recorder._uncache_page_tail_requests.get(page)
    if bool(is_rvc) and (int(pc) & 0xFFF) == 0xFFE and page_tail is not None:
        if not bool(page_tail.get("next_page_requested", False)):
            recorder.mark(
                "uncache_page_boundary",
                "rvc_tail_no_resend_before_delivery",
                cycle,
                {**evidence, **page_tail},
            )
    if not bool(is_rvc) and (int(pc) & 0xFFF) == 0xFFE and page_tail is not None:
        if bool(page_tail.get("next_page_requested", False)):
            recorder.mark(
                "uncache_page_boundary",
                "rvi_tail_resend_next_page",
                cycle,
                {**evidence, **page_tail},
            )

    if bool(is_rvc):
        recorder.mark("ifu_boundary_event", "rvc_start", cycle, evidence)
    else:
        recorder.mark("ifu_boundary_event", "rvi_start", cycle, evidence)

    last = getattr(recorder, "_ifu_last_cfvec", None)
    if last is not None:
        last_pc = int(last.get("pc", 0))
        last_is_rvc = bool(last.get("is_rvc", 0))
        expected_step = 2 if last_is_rvc else 4
        actual_step = int(pc) - last_pc
        if 0 < actual_step <= 4:
            if actual_step == expected_step:
                recorder.mark(
                    "ifu_pc_step_type",
                    "step_2b_rvc" if last_is_rvc else "step_4b_rvi",
                    cycle,
                    {**evidence, "last_pc": last_pc, "actual_step": actual_step},
                )
                if last_is_rvc != bool(is_rvc) and (last_pc & ~0x3F) == (int(pc) & ~0x3F):
                    recorder.mark(
                        "ifu_pc_step_type",
                        "mixed_no_gap_no_dup",
                        cycle,
                        {**evidence, "last_pc": last_pc, "actual_step": actual_step},
                    )
            if not last_is_rvc and actual_step == 4:
                recorder.mark(
                    "ifu_boundary_event",
                    "rvi_high_half_suppressed",
                    cycle,
                    {**evidence, "last_pc": last_pc},
                )
    recorder._ifu_last_cfvec = {"pc": int(pc), "is_rvc": int(bool(is_rvc)), "slot": int(slot)}


def sample_cfvec_coverage(recorder, env, cycle: int) -> None:
    dut = getattr(env, "dut", None)
    if dut is None:
        return

    cycle = int(cycle)
    skip_until = getattr(recorder, "_ifu_redirect_skip_until_cycle", None)
    if _read(recorder, "io_backend_toFtq_redirect_valid", 0) == 1:
        skip_until = max(int(skip_until or cycle), cycle + 1)
        recorder._ifu_redirect_skip_until_cycle = skip_until
    if skip_until is not None and cycle <= int(skip_until):
        recorder._ifu_last_cfvec = None
        return
    recorder._ifu_redirect_skip_until_cycle = None

    valid_slots: list[int] = []
    cf_entries: list[dict] = []
    for slot in range(8):
        base = f"io_backend_cfVec_{slot}_"
        if recorder._read_dut_signal(dut, base + "valid", 0) != 1:
            continue

        valid_slots.append(slot)
        pc = int(recorder._read_dut_signal(dut, base + "bits_pc", 0))
        instr = int(recorder._read_dut_signal(dut, base + "bits_instr", 0)) & 0xFFFFFFFF
        is_rvc = bool(recorder._read_dut_signal(dut, base + "bits_isRvc", 0))
        ftq_flag = recorder._read_dut_signal(dut, base + "bits_ftqPtr_flag", 0)
        ftq_value = recorder._read_dut_signal(dut, base + "bits_ftqPtr_value", 0)
        ex_sum = (
            recorder._read_dut_signal(dut, base + "bits_exceptionVec_1", 0)
            + recorder._read_dut_signal(dut, base + "bits_exceptionVec_2", 0)
            + recorder._read_dut_signal(dut, base + "bits_exceptionVec_12", 0)
            + recorder._read_dut_signal(dut, base + "bits_exceptionVec_19", 0)
            + recorder._read_dut_signal(dut, base + "bits_exceptionVec_20", 0)
        )
        instr = _recover_unavailable_instr(env, int(pc), int(instr), bool(is_rvc), int(ex_sum))
        cf_entries.append(
            {
                "slot": int(slot),
                "pc": int(pc),
                "is_rvc": int(bool(is_rvc)),
                "ftq_ptr": (int(ftq_flag), int(ftq_value)),
            }
        )
        _sample_ifu_cfvec_coverage(recorder, cycle, slot, pc, instr, is_rvc)

    if {entry["is_rvc"] for entry in cf_entries} == {0, 1}:
        recorder.mark(
            "ifu_instr_size_type",
            "mixed_rvi_rvc_seen",
            cycle,
            {"event": "cfvec_mixed_width_window", "entries": cf_entries},
        )

    unique_ftq_ptrs = []
    for entry in cf_entries:
        if entry["ftq_ptr"] not in unique_ftq_ptrs:
            unique_ftq_ptrs.append(entry["ftq_ptr"])

    # FTQ creates this transaction state before IFU observes its delivery.
    redirect_pending = getattr(recorder, "_two_fetch_redirect_pending", None)
    if redirect_pending is not None:
        old_tags = set(redirect_pending.get("old_tags") or ())
        delivered_old = [entry for entry in cf_entries if entry["ftq_ptr"] in old_tags]
        if delivered_old:
            recorder.risk_observations.append(
                {
                    "event": "two_fetch_redirect_old_tag_cfvec_delivery",
                    "cycle": cycle,
                    "old_tags": [list(tag) for tag in sorted(old_tags)],
                    "entries": delivered_old,
                }
            )

    expected_cfvec = getattr(recorder, "_two_fetch_expected_cfvec", None)
    expected_tags = None
    if expected_cfvec is not None:
        expected_cycle = int(expected_cfvec.get("cycle", cycle))
        if cycle - expected_cycle > 64:
            recorder._two_fetch_expected_cfvec = None
        else:
            expected_tags = tuple(expected_cfvec.get("tags") or ())
    if expected_tags is None and len(unique_ftq_ptrs) == 2:
        expected_tags = tuple(unique_ftq_ptrs)
    exact_two_source_delivery = (
        len(expected_tags or ()) == 2
        and unique_ftq_ptrs == [expected_tags[0], expected_tags[1]]
    )
    _sample_instr_compact_coverage(recorder, env, cycle)
    if exact_two_source_delivery:
        evidence = {
            "event": "backend_cfvec_exact_two_ftq_sources",
            "expected_ftq_ptrs": [list(ptr) for ptr in expected_tags],
            "ftq_ptrs": [list(ptr) for ptr in unique_ftq_ptrs],
            "entries": cf_entries,
        }
        recorder.mark("two_fetch_ifu_source", "two_ftq_sources", cycle, evidence)
        recorder.mark("two_fetch_delivery", "two_ftq_entries_same_cycle", cycle, evidence)
        if {entry["is_rvc"] for entry in cf_entries} == {0, 1}:
            recorder.mark("two_fetch_cross_block", "mixed_rvc_rvi", cycle, evidence)

        for before, after in zip(cf_entries, cf_entries[1:]):
            if before["ftq_ptr"] != after["ftq_ptr"] and before["is_rvc"] == 1:
                recorder.mark(
                    "two_fetch_cross_block",
                    "rvc_independent",
                    cycle,
                    {**evidence, "boundary_slots": [before["slot"], after["slot"]]},
                )
                break
        recorder._two_fetch_expected_cfvec = None

CFVEC_SAMPLER_BIN_KEYS = frozenset(
    {
        ("ifu_instr_size_type", "rvi_seen"),
        ("ifu_instr_size_type", "rvc_seen"),
        ("ifu_instr_size_type", "mixed_rvi_rvc_seen"),
        ("ifu_pc_step_type", "step_4b_rvi"),
        ("ifu_pc_step_type", "step_2b_rvc"),
        ("ifu_pc_step_type", "mixed_no_gap_no_dup"),
        ("ifu_boundary_event", "rvc_start"),
        ("ifu_boundary_event", "rvi_start"),
        ("ifu_boundary_event", "rvi_high_half_suppressed"),
        ("ifu_fetch_block_position", "head"),
        ("ifu_fetch_block_position", "mid"),
        ("ifu_fetch_block_position", "tail"),
        ("ifu_cfi_decode_type", "non_cfi"),
        ("ifu_cfi_decode_type", "branch"),
        ("ifu_cfi_decode_type", "jal"),
        ("ifu_instr_compact", "contiguous_slots"),
        ("ifu_instr_compact", "rvi_single_slot"),
        ("ifu_instr_compact", "rvc_multi_slot"),
        ("ifu_instr_compact_source", "two_fetch_select_block"),
        ("ifu_instr_end_offset", "rvc_rvi_end_offset"),
        ("ifu_rvc_expander", "legal_rvc_expanded"),
        ("ifu_rvc_expander", "rvi_passthrough"),
        ("ifu_rvc_exception", "illegal_rvc"),
        ("ifu_rvc_exception", "fetch_exception_over_illegal_rvc"),
        ("uncache_page_boundary", "rvc_tail_no_resend_before_delivery"),
        ("uncache_page_boundary", "rvi_tail_resend_next_page"),
    }
)

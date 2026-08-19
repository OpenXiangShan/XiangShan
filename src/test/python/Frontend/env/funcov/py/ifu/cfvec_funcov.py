from __future__ import annotations

from typing import Optional

from ..common.dut import _read
from ..common.fetch_memory import _recover_unavailable_instr
from .compact_funcov import _sample_instr_compact_coverage


def initialize_ifu_coverage_state(recorder) -> None:
    recorder._ifu_last_cfvec = None
    recorder._ifu_redirect_skip_until_cycle = None
    recorder._ifu_cacheable_backend_blocked = False
    recorder._ifu_cacheable_pending_cfi = None
    recorder._ifu_cacheable_last_delivery_entry = None


def reset_ifu_coverage_state(recorder) -> None:
    initialize_ifu_coverage_state(recorder)


def handle_ifu_event(recorder, event: dict) -> None:
    if str(event.get("type", "")) != "backend.redirect":
        return
    cycle = int(event.get("cycle", 0))
    recorder._ifu_last_cfvec = None
    recorder._ifu_cacheable_pending_cfi = None
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


def _sign_extend(value: int, width: int) -> int:
    sign = 1 << (int(width) - 1)
    return (int(value) ^ sign) - sign


def _branch_target(pc: int, instr: int) -> int:
    instr = int(instr) & 0xFFFFFFFF
    immediate = (
        (((instr >> 31) & 0x1) << 12)
        | (((instr >> 7) & 0x1) << 11)
        | (((instr >> 25) & 0x3F) << 5)
        | (((instr >> 8) & 0xF) << 1)
    )
    return int(pc) + _sign_extend(immediate, 13)


def _jal_target(pc: int, instr: int) -> int:
    instr = int(instr) & 0xFFFFFFFF
    immediate = (
        (((instr >> 31) & 0x1) << 20)
        | (((instr >> 12) & 0xFF) << 12)
        | (((instr >> 20) & 0x1) << 11)
        | (((instr >> 21) & 0x3FF) << 1)
    )
    return int(pc) + _sign_extend(immediate, 21)


def _cacheable_cf_entries(recorder, entries: list[dict], cycle: int) -> list[dict]:
    windows = [
        window
        for window in getattr(recorder, "_ifu_cacheable_verified_windows", ())
        if int(cycle) - int(window["cycle"]) <= 128
    ]
    result = []
    for entry in entries:
        matching = next(
            (
                window
                for window in reversed(windows)
                if entry["ftq_ptr"] == window["ftq_ptr"]
                and int(window["pc_start"]) <= int(entry["pc"]) < int(window["pc_limit"])
            ),
            None,
        )
        if matching is not None:
            result.append({**entry, "cacheable_source": matching})
    return result


def _contiguous_pairs(entries: list[dict]):
    for before, after in zip(entries, entries[1:]):
        expected = int(before["pc"]) + (2 if before["is_rvc"] else 4)
        if (
            int(after["pc"]) == expected
            and before["ftq_ptr"] == after["ftq_ptr"]
            and (int(before["pc"]) & ~0x3F) == (int(after["pc"]) & ~0x3F)
        ):
            yield before, after


def _sample_cacheable_delivery(recorder, cycle: int, entries: list[dict], can_accept: int) -> None:
    if int(can_accept) == 0:
        recorder._ifu_cacheable_backend_blocked = True
        recorder._ifu_cacheable_last_delivery_entry = None
        return
    if not entries:
        return

    prior = getattr(recorder, "_ifu_cacheable_last_delivery_entry", None)
    ordered_entries = ([prior] if prior is not None else []) + entries
    pairs = list(_contiguous_pairs(ordered_entries))
    evidence = {"event": "cacheable_cfvec_delivery", "entries": entries}
    if getattr(recorder, "_ifu_cacheable_backend_blocked", False) and pairs:
        recorder.mark("ifu_cacheable_delivery", "backend_recovery_multi_instr", cycle, evidence)
        recorder._ifu_cacheable_backend_blocked = False

    if any(before["is_rvc"] == after["is_rvc"] == 1 for before, after in pairs):
        recorder.mark("ifu_cacheable_delivery", "same_cacheline_multi_rvc", cycle, evidence)
    if any(before["is_rvc"] == after["is_rvc"] == 0 for before, after in pairs):
        recorder.mark("ifu_cacheable_delivery", "same_cacheline_multi_rvi", cycle, evidence)
    if any(before["is_rvc"] == 1 and after["is_rvc"] == 0 for before, after in pairs):
        recorder.mark("ifu_cacheable_delivery", "rvc_then_rvi", cycle, evidence)
    if any(before["is_rvc"] == 0 and after["is_rvc"] == 1 for before, after in pairs):
        recorder.mark("ifu_cacheable_delivery", "rvi_then_rvc", cycle, evidence)
    recorder._ifu_cacheable_last_delivery_entry = entries[-1]


def _sample_cacheable_cfi_flow(recorder, cycle: int, entries: list[dict]) -> None:
    pending = getattr(recorder, "_ifu_cacheable_pending_cfi", None)
    for entry in entries:
        if pending is not None:
            pc = int(entry["pc"])
            if pc in pending["expected_pcs"]:
                recorder.mark(
                    "ifu_cacheable_cfi_flow",
                    pending["bin_name"],
                    cycle,
                    {
                        "event": "cacheable_cfi_next_pc",
                        "cfi": pending,
                        "successor": entry,
                    },
                )
            pending = None

        kind = _classify_cfi_kind(entry["instr"], bool(entry["is_rvc"]))
        if kind == "branch":
            pending = {
                "bin_name": "branch_next_pc_matches_decode",
                "pc": int(entry["pc"]),
                "instr": int(entry["instr"]),
                "expected_pcs": (
                    int(entry["pc"]) + (2 if entry["is_rvc"] else 4),
                    _branch_target(entry["pc"], entry["instr"]),
                ),
            }
        elif kind == "jal":
            pending = {
                "bin_name": "jal_target_without_stale_delivery",
                "pc": int(entry["pc"]),
                "instr": int(entry["instr"]),
                "expected_pcs": (_jal_target(entry["pc"], entry["instr"]),),
            }
    recorder._ifu_cacheable_pending_cfi = pending


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
        recorder._ifu_cacheable_pending_cfi = None
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
                "instr": int(instr) & 0xFFFFFFFF,
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

    cacheable_entries = _cacheable_cf_entries(recorder, cf_entries, cycle)
    can_accept = _read(recorder, "io_backend_canAccept", 0)
    _sample_cacheable_delivery(recorder, cycle, cacheable_entries, can_accept)
    _sample_cacheable_cfi_flow(recorder, cycle, cacheable_entries)

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
        ("ifu_cacheable_delivery", "backend_recovery_multi_instr"),
        ("ifu_cacheable_delivery", "same_cacheline_multi_rvc"),
        ("ifu_cacheable_delivery", "same_cacheline_multi_rvi"),
        ("ifu_cacheable_delivery", "rvc_then_rvi"),
        ("ifu_cacheable_delivery", "rvi_then_rvc"),
        ("ifu_cacheable_cfi_flow", "branch_next_pc_matches_decode"),
        ("ifu_cacheable_cfi_flow", "jal_target_without_stale_delivery"),
    }
)

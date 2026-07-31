from __future__ import annotations

from typing import Any, Iterable, Optional


_WL = "Frontend_top.Frontend.inner_icache.wayLookup."
_ICACHE = "Frontend_top.Frontend.inner_icache."
_MAIN = _ICACHE + "mainPipe."
_PREFETCH = _ICACHE + "prefetcher."

_UPDATE_COUNT = 64
_ENTRY_FIELD_NAMES = (
    "vSetIdx_0",
    "vSetIdx_1",
    "bankSel_0",
    "bankSel_1",
    "isCrossLine",
    "waymask_0",
    "waymask_1",
    "pTag",
    "isMmio",
    "itlbPbmt",
)


ICACHE_WAYLOOKUP_COVERPOINTS = {
    "icache_waylookup_queue": "queue_behavior",
    "icache_waylookup_read": "read_behavior",
    "icache_waylookup_update": "update_behavior",
    "icache_waylookup_exception": "exception_behavior",
    "icache_waylookup_flush": "flush_behavior",
    "icache_waylookup_capacity": "capacity_behavior",
    "icache_waylookup_wrap": "wrap_behavior",
}


ICACHE_WAYLOOKUP_SAMPLER_BIN_KEYS = frozenset(
    {
        ("icache_waylookup_queue", "reset_empty"),
        ("icache_waylookup_queue", "single_write_read"),
        ("icache_waylookup_queue", "multi_entry_read"),
        ("icache_waylookup_queue", "entry_fields"),
        ("icache_waylookup_queue", "empty_write_next_read"),
        ("icache_waylookup_queue", "nonempty_read"),
        ("icache_waylookup_queue", "push_pop_same_cycle"),
        ("icache_waylookup_queue", "mainpipe_backpressure"),
        ("icache_waylookup_read", "ftq_waylookup_skew"),
        ("icache_waylookup_read", "dual_entry_dequeue"),
        ("icache_waylookup_read", "single_entry_fallback"),
        ("icache_waylookup_update", "update_head"),
        ("icache_waylookup_update", "update_nonhead"),
        ("icache_waylookup_update", "update_same_tag"),
        ("icache_waylookup_update", "update_same_way_new_tag"),
        ("icache_waylookup_update", "update_unrelated"),
        ("icache_waylookup_update", "update_corrupt_ignored"),
        ("icache_waylookup_update", "update_write_concurrent"),
        ("icache_waylookup_update", "update_second_entry_stall"),
        ("icache_waylookup_update", "update_flush_same_cycle"),
        ("icache_waylookup_exception", "exception_capture"),
        ("icache_waylookup_exception", "exception_dequeue"),
        ("icache_waylookup_exception", "exception_blocks_write"),
        ("icache_waylookup_exception", "exception_waits_flush"),
        ("icache_waylookup_exception", "exception_no_bypass"),
        ("icache_waylookup_flush", "global_flush_clears_all"),
        ("icache_waylookup_flush", "flush_wins_read"),
        ("icache_waylookup_flush", "flush_wins_write"),
        ("icache_waylookup_flush", "flush_wins_update"),
        ("icache_waylookup_flush", "flush_recovery"),
        ("icache_waylookup_flush", "bpu_flush_empty"),
        ("icache_waylookup_flush", "bpu_flush_rewinds_tail"),
        ("icache_waylookup_flush", "bpu_flush_nonmatching"),
        ("icache_waylookup_flush", "bpu_flush_exception_tail"),
        ("icache_waylookup_flush", "bpu_flush_preserves_other_exception"),
        ("icache_waylookup_flush", "bpu_flush_two_prefetch"),
        ("icache_waylookup_capacity", "full_blocks_write"),
        ("icache_waylookup_capacity", "one_slot_blocks_dual"),
        ("icache_waylookup_capacity", "dual_write_atomic"),
        ("icache_waylookup_capacity", "read_write_boundary"),
        ("icache_waylookup_wrap", "single_wrap"),
        ("icache_waylookup_wrap", "dual_wrap"),
    }
)


_SIGNALS = {
    "empty": (_WL + "io_perf_empty", _WL + "__Vtogcov__io_perf_empty"),
    "num_valid": (_WL + "numValidEntries",),
    "read_flag": (_WL + "readPtr_flag",),
    "read_value": (_WL + "readPtr_value",),
    "write_flag": (_WL + "writePtr_flag",),
    "write_value": (_WL + "writePtr_value",),
    "tail_flag": (_WL + "tailFtqIdx_flag",),
    "tail_value": (_WL + "tailFtqIdx_value",),
    "exception_valid": (_WL + "exceptionEntry_valid",),
    "exception_ptr_flag": (_WL + "exceptionPtr_flag",),
    "exception_ptr_value": (_WL + "exceptionPtr_value",),
    "bpu_flush_match": (_WL + "bpuS3FlushValid",),
    "to_valid": (
        _MAIN + "io_fromWayLookup_valid",
        _MAIN + "__Vtogcov__io_fromWayLookup_valid",
    ),
    "to_ready": (
        _MAIN + "io_fromWayLookup_ready",
        _MAIN + "__Vtogcov__io_fromWayLookup_ready",
    ),
    "info1_valid": (
        _MAIN + "io_fromWayLookup_bits_wayLookupInfo_1_valid",
        _MAIN + "__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_1_valid",
    ),
    "real_two": (
        _ICACHE + "__Vtogcov__io_toFtq_fromMainPipe_realTwoFetchValid",
    ),
    "ftq_valid": (_MAIN + "io_fromFtq_valid",),
    "write0_valid": (
        _PREFETCH + "io_wayLookupWrite_0_valid",
        _PREFETCH + "__Vtogcov__io_wayLookupWrite_0_valid",
    ),
    "write1_valid": (
        _PREFETCH + "io_wayLookupWrite_1_valid",
        _PREFETCH + "__Vtogcov__io_wayLookupWrite_1_valid",
    ),
    "write0_ready": (
        _PREFETCH + "io_wayLookupWrite_0_ready",
        _PREFETCH + "__Vtogcov__io_wayLookupWrite_0_ready",
    ),
    "update_valid": (
        _MAIN + "io_missResp_valid",
        _MAIN + "__Vtogcov__io_missResp_valid",
    ),
    "update_corrupt": (
        _MAIN + "__Vtogcov__io_missResp_bits_corrupt",
    ),
    "update_updated": tuple(
        _WL + "entryUpdate_updated" + (f"_{index}" if index else "")
        for index in range(_UPDATE_COUNT)
    ),
    "update_same_tag": tuple(
        _WL + "entryUpdate_pTagSame" + (f"_{entry * 2}" if entry else "")
        for entry in range(32)
        for _ in range(2)
    ),
    "update_same_way": tuple(
        _WL + "entryUpdate_waySame" + (f"_{index}" if index else "")
        for index in range(_UPDATE_COUNT)
    ),
    "write0_exception": (
        _PREFETCH + "__Vtogcov__io_wayLookupWrite_0_bits_exceptionEntry_itlbException_value",
    ),
    "flush": (
        _ICACHE + "__Vtogcov__io_fromFtq_redirectFlush",
    ),
    "bpu_flush": (
        _ICACHE + "__Vtogcov__io_fromFtq_flushFromBpu_s3_valid",
        _MAIN + "io_flushFromBpu_s3_valid",
    ),
}


def _read(recorder, key: str) -> Optional[int]:
    dut = getattr(getattr(recorder, "env", None), "dut", None)
    if dut is None:
        return None
    return recorder._read_first_dut_signal(dut, _SIGNALS[key])


def _read_names(recorder, names: Iterable[str]) -> tuple[Optional[int], ...]:
    dut = getattr(getattr(recorder, "env", None), "dut", None)
    if dut is None:
        return tuple(None for _ in names)
    return tuple(recorder._read_first_dut_signal(dut, (name,)) for name in names)


def _read_entry_fields(recorder, read_value: Optional[int]) -> tuple[Optional[int], ...]:
    if read_value is None:
        return tuple(None for _ in _ENTRY_FIELD_NAMES)
    index = int(read_value) & 0x1F
    names = tuple(
        _WL + f"__Vtogcov__entries_{index}_{field}"
        for field in _ENTRY_FIELD_NAMES
    )
    return _read_names(recorder, names)


def _on(value: Optional[int]) -> bool:
    return value is not None and int(value) != 0


def _off(value: Optional[int]) -> bool:
    return value is not None and int(value) == 0


def _zero(value: Optional[int]) -> bool:
    return value is not None and int(value) == 0


def _known(values: Iterable[Optional[int]]) -> bool:
    return all(value is not None for value in values)


def _any_on(values: Iterable[Optional[int]]) -> bool:
    return any(_on(value) for value in values)


def _mark(recorder, group: str, name: str, cycle: int, condition: bool, evidence: dict[str, Any]) -> None:
    if condition:
        recorder.mark(group, name, cycle, evidence, coverpoint=ICACHE_WAYLOOKUP_COVERPOINTS[group])


def reset_icache_waylookup_coverage_state(recorder) -> None:
    recorder._icache_waylookup_cov_state = {"prev": None}


def _snapshot(recorder) -> dict[str, Any]:
    scalar = {key: _read(recorder, key) for key in _SIGNALS}
    scalar["update_updated"] = _read_names(recorder, _SIGNALS["update_updated"])
    scalar["update_same_tag"] = _read_names(recorder, _SIGNALS["update_same_tag"])
    scalar["update_same_way"] = _read_names(recorder, _SIGNALS["update_same_way"])
    scalar["entry_fields"] = _read_entry_fields(recorder, scalar["read_value"])
    return scalar


def _fire(snapshot: dict[str, Any], valid: str, ready: str) -> bool:
    return _on(snapshot[valid]) and _on(snapshot[ready])


def _ptr(snapshot: dict[str, Any], prefix: str) -> Optional[tuple[int, int]]:
    flag = snapshot[f"{prefix}_flag"]
    value = snapshot[f"{prefix}_value"]
    if flag is None or value is None:
        return None
    return int(flag), int(value)


def _previous_ptr(ptr: Optional[tuple[int, int]]) -> Optional[tuple[int, int]]:
    if ptr is None:
        return None
    raw = (((int(ptr[0]) & 1) << 5) | (int(ptr[1]) & 0x1F)) - 1
    raw &= 0x3F
    return raw >> 5, raw & 0x1F


def _update_values(
    snapshot: dict[str, Any],
    key: str,
    offsets: Iterable[int],
) -> tuple[Optional[int], ...]:
    read_value = snapshot["read_value"]
    if read_value is None:
        return tuple()
    values = snapshot[key]
    selected = []
    for offset in offsets:
        entry_index = (int(read_value) + int(offset)) & 0x1F
        base = entry_index * 2
        selected.extend(values[base : base + 2])
    return tuple(selected)


def sample_icache_waylookup_coverage(recorder, env, cycle: int) -> None:
    del env
    state = getattr(recorder, "_icache_waylookup_cov_state", None)
    if state is None:
        reset_icache_waylookup_coverage_state(recorder)
        state = recorder._icache_waylookup_cov_state

    s = _snapshot(recorder)
    prev = state["prev"]
    evidence = {key: value for key, value in s.items() if key != "entry_fields" and value is not None}
    evidence["entry_fields"] = [value for value in s["entry_fields"] if value is not None]
    to_fire = _fire(s, "to_valid", "to_ready")
    write0_fire = _fire(s, "write0_valid", "write0_ready")
    write1_fire = _fire(s, "write1_valid", "write0_ready")
    read_two = _on(s["info1_valid"]) and _on(s["real_two"])
    update = _on(s["update_valid"])
    num_valid = None if s["num_valid"] is None else int(s["num_valid"])
    queue_offsets = tuple(range(min(max(num_valid or 0, 0), 32)))
    update_head = _any_on(_update_values(s, "update_updated", (0,)))
    update_nonhead = _any_on(_update_values(s, "update_updated", queue_offsets[1:]))
    update_second = _any_on(_update_values(s, "update_updated", (1,)))
    update_any = _any_on(_update_values(s, "update_updated", queue_offsets))
    update_same_tag = _any_on(
        updated
        for updated, same_tag in zip(
            _update_values(s, "update_updated", queue_offsets),
            _update_values(s, "update_same_tag", queue_offsets),
        )
        if _on(same_tag)
    )
    update_same_way = _any_on(
        updated
        for updated, same_way in zip(
            _update_values(s, "update_updated", queue_offsets),
            _update_values(s, "update_same_way", queue_offsets),
        )
        if _on(same_way)
    )
    flush = _on(s["flush"])
    bpu_flush = _on(s["bpu_flush"])
    prev_empty = bool(prev) and _on(prev["empty"])
    prev_write0_fire = bool(prev) and _fire(prev, "write0_valid", "write0_ready")
    prev_update = bool(prev) and _on(prev["update_valid"])
    exception_at_head = (
        _on(s["exception_valid"])
        and _ptr(s, "exception_ptr") == _ptr(s, "read")
    )
    exception_at_tail = (
        _on(s["exception_valid"])
        and _ptr(s, "exception_ptr") == _previous_ptr(_ptr(s, "write"))
    )

    _mark(recorder, "icache_waylookup_queue", "reset_empty", cycle,
          _known((s["empty"], s["read_flag"], s["read_value"], s["write_flag"], s["write_value"], s["exception_valid"]))
          and _on(s["empty"]) and _off(s["read_flag"]) and int(s["read_value"]) == 0
          and _off(s["write_flag"]) and int(s["write_value"]) == 0 and _off(s["exception_valid"]), evidence)
    _mark(recorder, "icache_waylookup_queue", "single_write_read", cycle,
          prev_write0_fire and not _on(prev["write1_valid"]) and _on(s["to_valid"])
          and not _on(s["info1_valid"]) and to_fire, evidence)
    _mark(recorder, "icache_waylookup_queue", "multi_entry_read", cycle,
          num_valid is not None and num_valid >= 2 and to_fire, evidence)
    _mark(recorder, "icache_waylookup_queue", "entry_fields", cycle,
          _on(s["to_valid"]) and _known(s["entry_fields"]), evidence)
    _mark(recorder, "icache_waylookup_queue", "empty_write_next_read", cycle,
          prev_empty and prev_write0_fire and _on(s["to_valid"]), evidence)
    _mark(recorder, "icache_waylookup_queue", "nonempty_read", cycle,
          to_fire and not _on(s["info1_valid"]), evidence)
    _mark(recorder, "icache_waylookup_queue", "push_pop_same_cycle", cycle,
          to_fire and write0_fire, evidence)
    _mark(recorder, "icache_waylookup_queue", "mainpipe_backpressure", cycle,
          _on(s["to_valid"]) and _off(s["to_ready"]), evidence)

    _mark(recorder, "icache_waylookup_read", "ftq_waylookup_skew", cycle,
          _known((s["ftq_valid"], s["to_valid"])) and (_on(s["ftq_valid"]) != _on(s["to_valid"])), evidence)
    _mark(recorder, "icache_waylookup_read", "dual_entry_dequeue", cycle, to_fire and read_two, evidence)
    _mark(recorder, "icache_waylookup_read", "single_entry_fallback", cycle,
          to_fire and _off(s["info1_valid"]) and _off(s["real_two"]), evidence)

    _mark(recorder, "icache_waylookup_update", "update_head", cycle, update and update_head, evidence)
    _mark(recorder, "icache_waylookup_update", "update_nonhead", cycle,
          update and update_nonhead, evidence)
    _mark(recorder, "icache_waylookup_update", "update_same_tag", cycle, update and update_same_tag, evidence)
    _mark(recorder, "icache_waylookup_update", "update_same_way_new_tag", cycle,
          update and not update_same_tag and update_same_way, evidence)
    _mark(recorder, "icache_waylookup_update", "update_unrelated", cycle,
          update and not update_any, evidence)
    _mark(recorder, "icache_waylookup_update", "update_corrupt_ignored", cycle,
          update and _on(s["update_corrupt"]) and not update_any, evidence)
    _mark(recorder, "icache_waylookup_update", "update_write_concurrent", cycle,
          update and write0_fire, evidence)
    _mark(recorder, "icache_waylookup_update", "update_second_entry_stall", cycle,
          update and update_second and _on(s["info1_valid"]), evidence)
    _mark(recorder, "icache_waylookup_update", "update_flush_same_cycle", cycle,
          bool(prev) and prev_update and _on(prev["flush"])
          and _zero(s["read_flag"]) and _zero(s["write_flag"])
          and _zero(s["read_value"]) and _zero(s["write_value"]), evidence)

    _mark(recorder, "icache_waylookup_exception", "exception_capture", cycle,
          write0_fire and _on(s["write0_exception"]), evidence)
    _mark(recorder, "icache_waylookup_exception", "exception_dequeue", cycle,
          to_fire and exception_at_head, evidence)
    _mark(recorder, "icache_waylookup_exception", "exception_blocks_write", cycle,
          _on(s["exception_valid"]) and _on(s["write0_valid"]) and _off(s["write0_ready"]), evidence)
    _mark(recorder, "icache_waylookup_exception", "exception_waits_flush", cycle,
          _on(s["exception_valid"]) and _on(s["empty"]) and not flush, evidence)
    _mark(recorder, "icache_waylookup_exception", "exception_no_bypass", cycle,
          prev and _on(prev["empty"]) and _fire(prev, "write0_valid", "write0_ready")
          and _on(prev["write0_exception"])
          and _on(s["exception_valid"]) and _off(s["to_valid"]), evidence)

    _mark(recorder, "icache_waylookup_flush", "global_flush_clears_all", cycle, flush, evidence)
    _mark(recorder, "icache_waylookup_flush", "flush_wins_read", cycle,
          flush and num_valid is not None and num_valid > 0 and _off(s["to_valid"]), evidence)
    _mark(recorder, "icache_waylookup_flush", "flush_wins_write", cycle,
          bool(prev) and _on(prev["flush"])
          and _fire(prev, "write0_valid", "write0_ready")
          and _zero(s["read_flag"]) and _zero(s["write_flag"])
          and _zero(s["read_value"]) and _zero(s["write_value"]), evidence)
    _mark(recorder, "icache_waylookup_flush", "flush_wins_update", cycle,
          bool(prev) and _on(prev["flush"]) and _on(prev["update_valid"])
          and _zero(s["read_flag"]) and _zero(s["write_flag"])
          and _zero(s["read_value"]) and _zero(s["write_value"]), evidence)
    _mark(recorder, "icache_waylookup_flush", "flush_recovery", cycle,
          bool(prev) and _on(prev["flush"]) and write0_fire, evidence)
    _mark(recorder, "icache_waylookup_flush", "bpu_flush_empty", cycle,
          bpu_flush and _on(s["empty"]), evidence)
    _mark(recorder, "icache_waylookup_flush", "bpu_flush_rewinds_tail", cycle,
          bpu_flush and _off(s["empty"]) and _on(s["bpu_flush_match"]), evidence)
    _mark(recorder, "icache_waylookup_flush", "bpu_flush_nonmatching", cycle,
          bpu_flush and _off(s["bpu_flush_match"]), evidence)
    _mark(recorder, "icache_waylookup_flush", "bpu_flush_exception_tail", cycle,
          bpu_flush and _on(s["bpu_flush_match"]) and exception_at_tail, evidence)
    _mark(recorder, "icache_waylookup_flush", "bpu_flush_preserves_other_exception", cycle,
          bpu_flush and _on(s["bpu_flush_match"])
          and _on(s["exception_valid"]) and not exception_at_tail, evidence)
    _mark(recorder, "icache_waylookup_flush", "bpu_flush_two_prefetch", cycle,
          bpu_flush and _on(s["bpu_flush_match"]) and write1_fire, evidence)

    _mark(recorder, "icache_waylookup_capacity", "full_blocks_write", cycle,
          num_valid == 32 and _on(s["write0_valid"]) and _off(s["write0_ready"]), evidence)
    _mark(recorder, "icache_waylookup_capacity", "one_slot_blocks_dual", cycle,
          num_valid == 31 and _on(s["write0_valid"]) and _on(s["write1_valid"])
          and _off(s["write0_ready"]), evidence)
    _mark(recorder, "icache_waylookup_capacity", "dual_write_atomic", cycle,
          write0_fire and write1_fire, evidence)
    _mark(recorder, "icache_waylookup_capacity", "read_write_boundary", cycle,
          num_valid == 31 and to_fire and write0_fire and write1_fire, evidence)

    previous_read = _ptr(prev, "read") if prev else None
    previous_write = _ptr(prev, "write") if prev else None
    current_read = _ptr(s, "read")
    current_write = _ptr(s, "write")
    read_wrap = previous_read is not None and current_read is not None and current_read[1] < previous_read[1]
    write_wrap = previous_write is not None and current_write is not None and current_write[1] < previous_write[1]
    _mark(recorder, "icache_waylookup_wrap", "single_wrap", cycle, read_wrap and not write_wrap, evidence)
    _mark(recorder, "icache_waylookup_wrap", "dual_wrap", cycle, write_wrap and write1_fire, evidence)

    state["prev"] = s


__all__ = (
    "ICACHE_WAYLOOKUP_COVERPOINTS",
    "ICACHE_WAYLOOKUP_SAMPLER_BIN_KEYS",
    "reset_icache_waylookup_coverage_state",
    "sample_icache_waylookup_coverage",
)

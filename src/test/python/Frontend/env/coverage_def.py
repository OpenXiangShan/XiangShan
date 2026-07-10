from __future__ import annotations

from functools import lru_cache
from pathlib import Path

try:
    import toffee.funcov as fc
except Exception:  # pragma: no cover
    fc = None

from .funcov import _TWO_FETCH_SIGNALS


@lru_cache(maxsize=1)
def _registered_internal_signals():
    offset = Path(__file__).resolve().parents[5] / "build-frontend/pylib/Frontend/Frontend_offset.yaml"
    if not offset.exists():
        return None
    prefix = "  - name: "
    with offset.open(encoding="utf-8") as f:
        return {line[len(prefix) :].strip() for line in f if line.startswith(prefix)}


def _lookup_signal(dut, name: str):
    signal = getattr(dut, str(name), None)
    if signal is not None:
        return signal
    registered = _registered_internal_signals()
    if registered is not None and str(name) not in registered:
        return None
    getter = getattr(dut, "GetInternalSignal", None)
    if not callable(getter):
        return None
    try:
        return getter(str(name))
    except Exception:
        return None


def _read_signal_value(dut, name: str, default: int = 0) -> int:
    signal = _lookup_signal(dut, str(name))
    if signal is None:
        return int(default)
    value = getattr(signal, "value", None)
    if value is None:
        return int(default)
    return int(value)


def _read_first_signal_value(dut, names, default=None):
    for name in names:
        signal = _lookup_signal(dut, str(name))
        if signal is None:
            continue
        value = getattr(signal, "value", None)
        if value is None:
            continue
        try:
            return int(value)
        except Exception:
            continue
    return default


def _tf_value(dut, key: str, default=None):
    return _read_first_signal_value(dut, _TWO_FETCH_SIGNALS[str(key)], default)


def _tf_fire(dut, valid_key: str, ready_key: str) -> bool:
    return _tf_value(dut, valid_key) == 1 and _tf_value(dut, ready_key) == 1


def _tf_ptr_distance(newer_flag, newer_value, older_flag, older_value, size=64):
    if None in (newer_flag, newer_value, older_flag, older_value):
        return None
    modulo = int(size) * 2
    newer = (int(newer_flag) & 1) * int(size) + int(newer_value) % int(size)
    older = (int(older_flag) & 1) * int(size) + int(older_value) % int(size)
    return (newer - older) % modulo


def _tf_ftq_blocked_by(dut, reason: str) -> bool:
    if not _tf_fire(dut, "ftq_valid", "ftq_ready") or _tf_value(dut, "ftq_req1_valid") != 0:
        return False
    start0 = _tf_value(dut, "ftq_req0_start")
    start1 = _tf_value(dut, "ftq_req1_start")
    end0 = _tf_value(dut, "ftq_req0_end")
    end1 = _tf_value(dut, "ftq_req1_end")
    distance = _tf_ptr_distance(
        _tf_value(dut, "bpu_ptr_flag"),
        _tf_value(dut, "bpu_ptr_value"),
        _tf_value(dut, "fetch_ptr_flag"),
        _tf_value(dut, "fetch_ptr_value"),
    )
    req0_exception = _tf_value(dut, "ftq_req0_exception") == 1
    # PrunedAddr.addr is halfword-addressed, so its bit 11 is virtual-address bit 12.
    cross_page = None not in (start0, start1) and (int(start0) >> 11) != (int(start1) >> 11)
    size_block = None not in (end0, end1) and (int(end0) + int(end1) + 2) > 32
    runahead = distance is not None and int(distance) <= 3
    exception = req0_exception
    if reason == "backend_exception":
        return exception
    if reason == "cross_page":
        return cross_page
    if reason == "size":
        return size_block
    if reason == "runahead":
        return runahead
    return False


def _tf_vector_values(dut, module: str, template: str, count: int):
    values = []
    for index in range(int(count)):
        signal = str(template).format(index=index, suffix="" if index == 0 else f"_{index}")
        values.append(_read_first_signal_value(dut, (f"Frontend_top.Frontend.{module}.{signal}",)))
    return values


def _tf_waylookup_blocked_by(dut, reason: str) -> bool:
    if (
        not _tf_fire(dut, "way_out_valid", "way_out_ready")
        or _tf_value(dut, "way_req1_valid") != 1
        or _tf_value(dut, "way_real_two") != 0
    ):
        return False

    num_valid = _tf_value(dut, "way_num_valid")
    read_flag = _tf_value(dut, "way_read_ptr_flag")
    read_value = _tf_value(dut, "way_read_ptr_value")
    exception_valid = _tf_value(dut, "way_exception_valid")
    exception_flag = _tf_value(dut, "way_exception_ptr_flag")
    exception_value = _tf_value(dut, "way_exception_ptr_value")
    if None in (num_valid, read_flag, read_value, exception_valid, exception_flag, exception_value):
        return False

    index0 = int(read_value) % 32
    index1 = (index0 + 1) % 32
    updates = _tf_vector_values(dut, "inner_icache.wayLookup", "entryUpdate_updated{suffix}", 64)
    mmio = _tf_vector_values(dut, "inner_icache.wayLookup", "entries_{index}_isMmio", 32)
    if any(value is None for value in updates) or any(value is None for value in mmio):
        return False

    update_stall0 = bool(int(updates[index0 * 2]) or int(updates[index0 * 2 + 1]))
    update_stall1 = bool(int(updates[index1 * 2]) or int(updates[index1 * 2 + 1]))
    can_deq_second = int(num_valid) > 1 and not update_stall0 and not update_stall1
    has_mmio = bool(int(mmio[index0]) or int(mmio[index1]))
    next_ptr = (int(read_flag) ^ (1 if index0 == 31 else 0), index1)
    exception_ptr = (int(exception_flag), int(exception_value) % 32)
    has_itlb_exception = int(exception_valid) == 1 and exception_ptr in (
        (int(read_flag), index0),
        next_ptr,
    )
    reasons = {
        "insufficient_meta": not can_deq_second,
        "mmio": has_mmio,
        "itlb_exception": has_itlb_exception,
        "data_bank_conflict": can_deq_second and not has_mmio and not has_itlb_exception,
    }
    return bool(reasons.get(str(reason), False))


def _tf_mainpipe_pattern(dut, expected: str) -> bool:
    if _tf_value(dut, "main_s1_valid") != 1 or _tf_value(dut, "main_req1_valid") != 1:
        return False
    values = [
        _read_first_signal_value(
            dut,
            (
                f"Frontend_top.Frontend.inner_icache.mainPipe.s1_shouldFetch_{index}",
                f"Frontend_top.Frontend.inner_icache.inner_mainPipe.s1_shouldFetch_{index}",
            ),
        )
        for index in range(4)
    ]
    if any(value is None for value in values):
        return False
    req0_miss = bool(int(values[0]) or int(values[1]))
    req1_miss = bool(int(values[2]) or int(values[3]))
    actual = {
        (False, False): "hit_hit",
        (False, True): "hit_miss",
        (True, False): "miss_hit",
        (True, True): "miss_miss",
    }[(req0_miss, req1_miss)]
    return actual == str(expected)


def _tf_checker_selected(dut, block: int) -> bool:
    return (
        _tf_value(dut, "checker_valid") == 1
        and _tf_value(dut, "checker_select") == int(block)
        and _tf_value(dut, "ifu_second_valid") == 1
    )


def _tf_ifu_dual_fire(dut) -> bool:
    return _tf_fire(dut, "ifu_valid", "ifu_ready") and _tf_value(dut, "ifu_req1_valid") == 1


def _tf_ifu_raw_flag(dut, field: str, expected: bool) -> bool:
    if not _tf_ifu_dual_fire(dut):
        return False
    values = _tf_vector_values(
        dut,
        "inner_ifu.instrBoundary",
        f"io_resp_rawInstrVec_{{index}}_{field}",
        31,
    )
    readable = [int(value) for value in values if value is not None]
    return bool(readable) and any(readable) == bool(expected)


def _tf_backend_entries(dut):
    entries = []
    for slot in range(8):
        if _read_signal_value(dut, f"io_backend_cfVec_{slot}_valid") != 1:
            continue
        entries.append(
            {
                "slot": slot,
                "is_rvc": _read_signal_value(dut, f"io_backend_cfVec_{slot}_bits_isRvc"),
                "ftq": (
                    _read_signal_value(dut, f"io_backend_cfVec_{slot}_bits_ftqPtr_flag"),
                    _read_signal_value(dut, f"io_backend_cfVec_{slot}_bits_ftqPtr_value"),
                ),
            }
        )
    return entries


def _tf_backend_two_sources(dut) -> bool:
    return len({entry["ftq"] for entry in _tf_backend_entries(dut)}) >= 2


def _tf_backend_mixed_rvc_rvi(dut) -> bool:
    entries = _tf_backend_entries(dut)
    return len({entry["ftq"] for entry in entries}) >= 2 and {entry["is_rvc"] for entry in entries} == {0, 1}


def _tf_backend_rvc_boundary(dut) -> bool:
    entries = _tf_backend_entries(dut)
    return any(
        before["ftq"] != after["ftq"] and before["is_rvc"] == 1
        for before, after in zip(entries, entries[1:])
    )


_TF_TEMPORAL_STATE = {}


def _tf_refill_then_dual(dut) -> bool:
    state = _TF_TEMPORAL_STATE.setdefault(id(dut), {"waiting_refill": False})
    values = _tf_vector_values(dut, "inner_icache.mainPipe", "s1_shouldFetch_{index}", 4)
    if (
        _tf_value(dut, "main_s1_valid") == 1
        and _tf_value(dut, "main_req1_valid") == 1
        and all(value is not None for value in values)
        and any(int(value) for value in values)
    ):
        state["waiting_refill"] = True
    hit = bool(state["waiting_refill"] and _tf_ifu_dual_fire(dut))
    if hit:
        state["waiting_refill"] = False
    return hit


def _safe_add_watch_point(group, dut, bins, name):
    try:
        group.add_watch_point(dut, bins, name=name)
    except Exception:
        pass


def get_coverage_groups(dut):
    if fc is None or dut is None:
        return []

    fg_fetch = fc.CovGroup("FG-FETCH")
    fg_branch = fc.CovGroup("FG-BRANCH")
    fg_redirect = fc.CovGroup("FG-REDIRECT")
    fg_exception = fc.CovGroup("FG-EXCEPTION")
    fg_perf = fc.CovGroup("FG-PERFORMANCE")
    two_fetch_ftq_eligibility = fc.CovGroup("two_fetch_ftq_eligibility")
    two_fetch_pointer_advance = fc.CovGroup("two_fetch_pointer_advance")
    two_fetch_flush_flow = fc.CovGroup("two_fetch_flush_flow")
    two_prefetch_layout = fc.CovGroup("two_prefetch_layout")
    two_fetch_waylookup_result = fc.CovGroup("two_fetch_waylookup_result")
    two_fetch_waylookup_block_reason = fc.CovGroup("two_fetch_waylookup_block_reason")
    two_fetch_mainpipe_hit_pattern = fc.CovGroup("two_fetch_mainpipe_hit_pattern")
    two_fetch_mainpipe_completion = fc.CovGroup("two_fetch_mainpipe_completion")
    two_fetch_ifu_window = fc.CovGroup("two_fetch_ifu_window")
    two_fetch_ifu_source = fc.CovGroup("two_fetch_ifu_source")
    two_fetch_cross_block = fc.CovGroup("two_fetch_cross_block")
    two_fetch_invalid_taken = fc.CovGroup("two_fetch_invalid_taken")
    two_fetch_checker_priority = fc.CovGroup("two_fetch_checker_priority")
    two_fetch_checker_redirect = fc.CovGroup("two_fetch_checker_redirect")
    two_fetch_delivery = fc.CovGroup("two_fetch_delivery")

    _safe_add_watch_point(
        fg_fetch,
        dut,
        {
            "CK-WIDTH-1P": lambda x: sum(
                int(getattr(x, f"io_backend_cfVec_{i}_valid").value) for i in range(8)
            ) >= 1,
            "CK-WIDTH-4P": lambda x: sum(
                int(getattr(x, f"io_backend_cfVec_{i}_valid").value) for i in range(8)
            ) >= 4,
            "CK-RVC": lambda x: sum(
                int(getattr(x, f"io_backend_cfVec_{i}_bits_isRvc").value)
                for i in range(8)
                if int(getattr(x, f"io_backend_cfVec_{i}_valid").value) == 1
            ) > 0,
        },
        name="FC-FETCH",
    )

    _safe_add_watch_point(
        fg_branch,
        dut,
        {
            "CK-PRED-TAKEN": lambda x: sum(
                int(getattr(x, f"io_backend_cfVec_{i}_bits_predTaken").value)
                for i in range(8)
                if int(getattr(x, f"io_backend_cfVec_{i}_valid").value) == 1
            ) > 0,
            "CK-FIXED-TAKEN": lambda x: sum(
                int(getattr(x, f"io_backend_cfVec_{i}_bits_fixedTaken").value)
                for i in range(8)
                if int(getattr(x, f"io_backend_cfVec_{i}_valid").value) == 1
            ) > 0,
        },
        name="FC-BRANCH",
    )

    _safe_add_watch_point(
        fg_redirect,
        dut,
        {
            "CK-REDIRECT": lambda x: int(x.io_backend_toFtq_redirect_valid.value) == 1,
        },
        name="FC-REDIRECT",
    )

    _safe_add_watch_point(
        fg_exception,
        dut,
        {
            "CK-EXC": lambda x: sum(
                int(getattr(x, f"io_backend_cfVec_0_bits_exceptionVec_{k}").value)
                for k in (1, 2, 12, 19, 20)
            ) > 0,
        },
        name="FC-EXCEPTION",
    )

    _safe_add_watch_point(
        fg_perf,
        dut,
        {
            "CK-ICACHE-FIRE": lambda x: _read_signal_value(x, "auto_inner_icache_client_out_a_valid") == 1
            and _read_signal_value(x, "auto_inner_icache_client_out_a_ready") == 1,
            "CK-IBUF-FULL": lambda x: _read_signal_value(x, "io_frontendInfo_ibufFull") == 1,
        },
        name="FC-PERFORMANCE",
    )

    _safe_add_watch_point(
        two_fetch_ftq_eligibility,
        dut,
        {
            "eligible_dual": lambda x: _tf_fire(x, "ftq_valid", "ftq_ready")
            and _tf_value(x, "ftq_req1_valid") == 1,
            "blocked_runahead": lambda x: _tf_ftq_blocked_by(x, "runahead"),
            "blocked_size": lambda x: _tf_ftq_blocked_by(x, "size"),
            "blocked_cross_page": lambda x: _tf_ftq_blocked_by(x, "cross_page"),
            "blocked_backend_exception": lambda x: _tf_ftq_blocked_by(x, "backend_exception"),
        },
        name="request_eligibility",
    )

    _safe_add_watch_point(
        two_fetch_pointer_advance,
        dut,
        {
            "step_two": lambda x: _tf_fire(x, "way_out_valid", "way_out_ready")
            and _tf_value(x, "way_real_two") == 1,
            "step_one": lambda x: _tf_fire(x, "way_out_valid", "way_out_ready")
            and _tf_value(x, "way_real_two") == 0,
            "wrap_step_two": lambda x: _tf_fire(x, "way_out_valid", "way_out_ready")
            and _tf_value(x, "way_real_two") == 1
            and (_tf_value(x, "fetch_ptr_value", -1) >= 62),
        },
        name="fetch_ptr_step",
    )

    _safe_add_watch_point(
        two_fetch_flush_flow,
        dut,
        {
            "bpu_s3_drop_before_issue": lambda x: _tf_value(x, "bpu_s3_flush") == 1
            and _tf_value(x, "ftq_valid") == 1
            and _tf_value(x, "ftq_ready") == 0,
            "backend_redirect_drops_inflight": lambda x: _read_signal_value(
                x, "io_backend_toFtq_redirect_valid"
            )
            == 1
            and _tf_value(x, "ifu_flush") == 1,
        },
        name="flush_stage",
    )

    _safe_add_watch_point(
        two_prefetch_layout,
        dut,
        {
            "same_line": lambda x: _tf_fire(x, "prefetch_valid", "prefetch_ready")
            and _tf_value(x, "prefetch_case") == 1,
            "overlap1": lambda x: _tf_fire(x, "prefetch_valid", "prefetch_ready")
            and _tf_value(x, "prefetch_case") == 2,
            "overlap2": lambda x: _tf_fire(x, "prefetch_valid", "prefetch_ready")
            and _tf_value(x, "prefetch_case") == 4,
            "interleave": lambda x: _tf_fire(x, "prefetch_valid", "prefetch_ready")
            and _tf_value(x, "prefetch_case") == 8,
        },
        name="address_layout",
    )

    _safe_add_watch_point(
        two_fetch_waylookup_result,
        dut,
        {
            "dual_served": lambda x: _tf_fire(x, "way_out_valid", "way_out_ready")
            and _tf_value(x, "way_req1_valid") == 1
            and _tf_value(x, "way_real_two") == 1,
            "single_fallback": lambda x: _tf_fire(x, "way_out_valid", "way_out_ready")
            and _tf_value(x, "way_req1_valid") == 1
            and _tf_value(x, "way_real_two") == 0,
        },
        name="serve_width",
    )

    _safe_add_watch_point(
        two_fetch_waylookup_block_reason,
        dut,
        {
            "insufficient_meta": lambda x: _tf_waylookup_blocked_by(x, "insufficient_meta"),
            "data_bank_conflict": lambda x: _tf_waylookup_blocked_by(x, "data_bank_conflict"),
            "mmio": lambda x: _tf_waylookup_blocked_by(x, "mmio"),
            "itlb_exception": lambda x: _tf_waylookup_blocked_by(x, "itlb_exception"),
        },
        name="fallback_reason",
    )

    _safe_add_watch_point(
        two_fetch_mainpipe_hit_pattern,
        dut,
        {
            "hit_hit": lambda x: _tf_mainpipe_pattern(x, "hit_hit"),
            "hit_miss": lambda x: _tf_mainpipe_pattern(x, "hit_miss"),
            "miss_hit": lambda x: _tf_mainpipe_pattern(x, "miss_hit"),
            "miss_miss": lambda x: _tf_mainpipe_pattern(x, "miss_miss"),
        },
        name="dual_hit_pattern",
    )

    _safe_add_watch_point(
        two_fetch_mainpipe_completion,
        dut,
        {
            "wait_refill_then_dual": _tf_refill_then_dual,
        },
        name="completion_mode",
    )

    _safe_add_watch_point(
        two_fetch_ifu_window,
        dut,
        {
            "dual_window": lambda x: _tf_fire(x, "ifu_valid", "ifu_ready")
            and _tf_value(x, "ifu_req1_valid") == 1,
        },
        name="window_width",
    )

    _safe_add_watch_point(
        two_fetch_ifu_source,
        dut,
        {
            "blocksel_switch": lambda x: _tf_ifu_raw_flag(x, "blockSel", True),
            "two_ftq_sources": _tf_backend_two_sources,
        },
        name="source_mapping",
    )

    _safe_add_watch_point(
        two_fetch_cross_block,
        dut,
        {
            "taken_separates_blocks": lambda x: _tf_ifu_dual_fire(x)
            and _tf_value(x, "ifu_first_taken") == 1
            and _tf_ifu_raw_flag(x, "isCrossBlockInstr", False),
            "rvi_stitch": lambda x: _tf_ifu_raw_flag(x, "isCrossBlockInstr", True),
            "rvc_independent": _tf_backend_rvc_boundary,
            "mixed_rvc_rvi": _tf_backend_mixed_rvc_rvi,
        },
        name="boundary_kind",
    )

    _safe_add_watch_point(
        two_fetch_invalid_taken,
        dut,
        {
            "first_masks_second": lambda x: _tf_ifu_dual_fire(x)
            and _tf_value(x, "ifu_first_invalid") == 1
            and _tf_value(x, "ifu_fixed_second_valid") == 0,
            "second_redirect": lambda x: _tf_checker_selected(x, 1)
            and _tf_value(x, "checker_invalid") == 1,
        },
        name="invalid_taken_block",
    )

    _safe_add_watch_point(
        two_fetch_checker_priority,
        dut,
        {
            "first_masks_second": lambda x: _tf_checker_selected(x, 0),
            "second_after_first_valid": lambda x: _tf_checker_selected(x, 1),
        },
        name="earliest_fault",
    )

    _safe_add_watch_point(
        two_fetch_checker_redirect,
        dut,
        {
            "first_block": lambda x: _tf_checker_selected(x, 0),
            "second_block": lambda x: _tf_checker_selected(x, 1),
        },
        name="select_block",
    )

    _safe_add_watch_point(
        two_fetch_delivery,
        dut,
        {
            "two_ftq_entries_same_cycle": _tf_backend_two_sources,
            "dual_fire": lambda x: _tf_value(x, "ifu_second_valid") == 1
            and _tf_fire(x, "to_ibuffer_valid", "to_ibuffer_ready"),
            "dual_stall": lambda x: _tf_value(x, "ifu_second_valid") == 1
            and _tf_value(x, "to_ibuffer_valid") == 1
            and _tf_value(x, "to_ibuffer_ready") == 0,
        },
        name="delivery_state",
    )

    return [
        fg_fetch,
        fg_branch,
        fg_redirect,
        fg_exception,
        fg_perf,
        two_fetch_ftq_eligibility,
        two_fetch_pointer_advance,
        two_fetch_flush_flow,
        two_prefetch_layout,
        two_fetch_waylookup_result,
        two_fetch_waylookup_block_reason,
        two_fetch_mainpipe_hit_pattern,
        two_fetch_mainpipe_completion,
        two_fetch_ifu_window,
        two_fetch_ifu_source,
        two_fetch_cross_block,
        two_fetch_invalid_taken,
        two_fetch_checker_priority,
        two_fetch_checker_redirect,
        two_fetch_delivery,
    ]

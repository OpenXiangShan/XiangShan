from __future__ import annotations

import hashlib
from typing import Optional

from ..common.dut import _dut, _read_first
from ..common.fetch_memory import _read_expected_fetch_raw
from ..common.utils import circular_distance


def initialize_ftq_coverage_state(recorder) -> None:
    recorder._two_fetch_last_fetch_ptr = None
    recorder._two_fetch_expected_ptr_step = None
    recorder._two_fetch_refill_pending = None
    recorder._two_fetch_last_main_s1_tag = None
    recorder._two_fetch_ftq_pending = False
    recorder._two_fetch_last_dual_cycle = None
    recorder._two_fetch_last_waylookup_write_state = None
    recorder._two_fetch_stalled_payload = None
    recorder._two_fetch_expected_cfvec = None
    recorder._two_fetch_redirect_pending = None
    recorder._two_fetch_recent_inflight_tags = None


def reset_ftq_coverage_state(recorder) -> None:
    initialize_ftq_coverage_state(recorder)


def _count_truthy(values) -> int:
    return sum(1 for value in values if int(value or 0) != 0)


_WAYLOOKUP_PREFIX = "Frontend_top.Frontend.inner_icache.wayLookup."
_MAINPIPE_PREFIX = "Frontend_top.Frontend.inner_icache.mainPipe."
_IFU_PREFIX = "Frontend_top.Frontend.inner_ifu."


_TWO_FETCH_SIGNALS = {
    "ftq_valid": (
        f"{_MAINPIPE_PREFIX}io_fromFtq_valid",
    ),
    "ftq_ready": (
        f"{_MAINPIPE_PREFIX}io_fromFtq_ready",
    ),
    "ftq_req1_valid": (
        f"{_MAINPIPE_PREFIX}io_fromFtq_bits_req_1_valid",
    ),
    "ftq_req0_start": (
        f"{_MAINPIPE_PREFIX}io_fromFtq_bits_req_0_vAddr_0_addr",
    ),
    "ftq_req1_start": (
        f"{_MAINPIPE_PREFIX}io_fromFtq_bits_req_1_vAddr_0_addr",
    ),
    "ftq_req0_end": (
        f"{_MAINPIPE_PREFIX}io_fromFtq_bits_req_0_endPosition",
    ),
    "ftq_req1_end": (
        f"{_MAINPIPE_PREFIX}io_fromFtq_bits_req_1_endPosition",
    ),
    "ftq_req0_exception": (
        f"{_MAINPIPE_PREFIX}io_fromFtq_bits_req_0_hasBackendException",
    ),
    "ftq_backend_exception": (
        "Frontend_top.Frontend.inner_ftq.backendException_value",
    ),
    "ftq_backend_exception_ptr_flag": (
        "Frontend_top.Frontend.inner_ftq.backendExceptionPtr_flag",
    ),
    "ftq_backend_exception_ptr_value": (
        "Frontend_top.Frontend.inner_ftq.backendExceptionPtr_value",
    ),
    "bpu_ptr_flag": (
        "Frontend_top.Frontend.inner_ftq.bpuPtr_ptrs_0_flag",
    ),
    "bpu_ptr_value": (
        "Frontend_top.Frontend.inner_ftq.bpuPtr_ptrs_0_value",
    ),
    "fetch_ptr_flag": (
        "Frontend_top.Frontend.inner_ftq.fetchPtr_ptrs_0_flag",
    ),
    "fetch_ptr_value": (
        "Frontend_top.Frontend.inner_ftq.fetchPtr_ptrs_0_value",
    ),
    "bpu_s3_flush": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_fromFtq_flushFromBpu_s3_valid",
    ),
    "prefetch_valid": (
        "Frontend_top.Frontend.inner_icache.io_fromFtq_toPrefetch_valid",
    ),
    "prefetch_ready": (
        "Frontend_top.Frontend.inner_icache.io_fromFtq_toPrefetch_ready",
    ),
    "prefetch_case": (
        "Frontend_top.Frontend.inner_ftq.io_toICache_toPrefetch_bits_twoPrefetchCase_value_0",
    ),
    "way_req1_valid": (
        f"{_MAINPIPE_PREFIX}io_fromFtq_bits_req_1_valid",
    ),
    "way_out_valid": (
        f"{_MAINPIPE_PREFIX}io_fromFtq_valid",
    ),
    "way_out_ready": (
        f"{_MAINPIPE_PREFIX}io_fromFtq_ready",
    ),
    "way_real_two": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toFtq_fromMainPipe_realTwoFetchValid",
    ),
    "main_s0_fire": (
        f"{_MAINPIPE_PREFIX}s0_fire",
    ),
    "bpu_s3_flush_ptr_flag": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_fromFtq_flushFromBpu_s3_bits_flag",
    ),
    "bpu_s3_flush_ptr_value": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_fromFtq_flushFromBpu_s3_bits_value",
    ),
    "way_num_valid": (
        "Frontend_top.Frontend.inner_icache.wayLookup.numValidEntries",
    ),
    "way_read_ptr_flag": (
        "Frontend_top.Frontend.inner_icache.wayLookup.readPtr_flag",
    ),
    "way_read_ptr_value": (
        "Frontend_top.Frontend.inner_icache.wayLookup.readPtr_value",
    ),
    "way_exception_valid": (
        "Frontend_top.Frontend.inner_icache.wayLookup.exceptionEntry_valid",
    ),
    "way_exception_ptr_flag": (
        "Frontend_top.Frontend.inner_icache.wayLookup.exceptionPtr_flag",
    ),
    "way_exception_ptr_value": (
        "Frontend_top.Frontend.inner_icache.wayLookup.exceptionPtr_value",
    ),
    "way_empty": (
        "Frontend_top.Frontend.inner_icache.wayLookup.io_perf_empty",
        "Frontend_top.Frontend.inner_icache.wayLookup.__Vtogcov__io_perf_empty",
    ),
    "way_write_ptr_flag": (
        "Frontend_top.Frontend.inner_icache.wayLookup.writePtr_flag",
        "Frontend_top.Frontend.inner_icache.wayLookup.__Vtogcov__writePtr_flag",
    ),
    "way_write_ptr_value": (
        "Frontend_top.Frontend.inner_icache.wayLookup.writePtr_value",
        "Frontend_top.Frontend.inner_icache.wayLookup.__Vtogcov__writePtr_value",
    ),
    "main_wli1_valid": (
        f"{_MAINPIPE_PREFIX}io_fromWayLookup_bits_wayLookupInfo_1_valid",
        f"{_MAINPIPE_PREFIX}__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_1_valid",
    ),
    "main_wli0_is_mmio": (
        f"{_MAINPIPE_PREFIX}io_fromWayLookup_bits_wayLookupInfo_0_bits_entry_isMmio",
        f"{_MAINPIPE_PREFIX}__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_0_bits_entry_isMmio",
    ),
    "main_wli1_is_mmio": (
        f"{_MAINPIPE_PREFIX}io_fromWayLookup_bits_wayLookupInfo_1_bits_entry_isMmio",
        f"{_MAINPIPE_PREFIX}__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_1_bits_entry_isMmio",
    ),
    "main_wli0_itlb_exception": (
        f"{_MAINPIPE_PREFIX}__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_0_bits_exceptionEntry_itlbException_value",
    ),
    "main_wli1_itlb_exception": (
        f"{_MAINPIPE_PREFIX}__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_1_bits_exceptionEntry_itlbException_value",
    ),
    "main_wli0_vset0": (
        f"{_MAINPIPE_PREFIX}io_fromWayLookup_bits_wayLookupInfo_0_bits_entry_vSetIdx_0",
        f"{_MAINPIPE_PREFIX}__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_0_bits_entry_vSetIdx_0",
    ),
    "main_wli0_vset1": (
        f"{_MAINPIPE_PREFIX}io_fromWayLookup_bits_wayLookupInfo_0_bits_entry_vSetIdx_1",
        f"{_MAINPIPE_PREFIX}__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_0_bits_entry_vSetIdx_1",
    ),
    "main_wli1_vset0": (
        f"{_MAINPIPE_PREFIX}io_fromWayLookup_bits_wayLookupInfo_1_bits_entry_vSetIdx_0",
        f"{_MAINPIPE_PREFIX}__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_1_bits_entry_vSetIdx_0",
    ),
    "main_wli1_vset1": (
        f"{_MAINPIPE_PREFIX}io_fromWayLookup_bits_wayLookupInfo_1_bits_entry_vSetIdx_1",
        f"{_MAINPIPE_PREFIX}__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_1_bits_entry_vSetIdx_1",
    ),
    "main_s1_valid": (
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_valid",
    ),
    "main_req1_valid": (
        "Frontend_top.Frontend.inner_icache.mainPipe.s1_req_1_valid",
    ),
    "ifu_valid": (
        "Frontend_top.Frontend.inner_icache.mainPipe.io_toIfu_req_valid",
    ),
    "ifu_ready": (
        "Frontend_top.Frontend.inner_icache.mainPipe.io_toIfu_req_ready",
    ),
    "ifu_req1_valid": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_1_valid",
    ),
    "ifu_req0_size": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_0_size",
    ),
    "ifu_first_taken": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_0_takenCfiOffset_valid",
    ),
    "ifu_first_invalid": (
        f"{_IFU_PREFIX}s1_invalidTaken_0",
    ),
    "ifu_s1_valid": (
        f"{_IFU_PREFIX}s1_valid",
    ),
    "ifu_s1_instr_count": (
        f"{_IFU_PREFIX}s1_instrCount",
    ),
    "ifu_second_valid": (
        "Frontend_top.Frontend.inner_ifu.s2_fetchBlock_1_valid",
    ),
    "ifu_s2_valid": (
        "Frontend_top.Frontend.inner_ifu.s2_valid_valid",
    ),
    "to_ibuffer_valid": (
        "Frontend_top.Frontend.inner_ifu.io_toIBuffer_valid",
    ),
    "to_ibuffer_ready": (
        "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_ready",
    ),
    "ifu_flush": (
        "Frontend_top.Frontend.inner_ifu.s1_flush",
    ),
    "checker_valid": (
        "Frontend_top.Frontend.inner_ifu.predChecker.io_resp_stage2Out_checkerRedirect_valid",
    ),
    "checker_select": (
        "Frontend_top.Frontend.inner_ifu.predChecker.__Vtogcov__io_resp_stage2Out_checkerRedirect_bits_selectBlock",
    ),
    "checker_invalid": (
        "Frontend_top.Frontend.inner_ifu.predChecker.__Vtogcov__io_resp_stage2Out_checkerRedirect_bits_invalidTaken",
    ),
    "fixed_instr_valid": (
        f"{_IFU_PREFIX}s2_fixedInstrValid",
        f"{_IFU_PREFIX}__Vtogcov__s2_fixedInstrValid",
    ),
    "backend_redirect": (
        "Frontend_top.io_backend_toFtq_redirect_valid",
    ),
    "main_s1_ftq0_flag": (
        f"{_MAINPIPE_PREFIX}s1_req_0_ftqIdx_flag",
    ),
    "main_s1_ftq0_value": (
        f"{_MAINPIPE_PREFIX}s1_req_0_ftqIdx_value",
    ),
    "main_s1_ftq1_flag": (
        f"{_MAINPIPE_PREFIX}s1_req_1_ftqIdx_flag",
    ),
    "main_s1_ftq1_value": (
        f"{_MAINPIPE_PREFIX}s1_req_1_ftqIdx_value",
    ),
    "main_s1_fire": (
        f"{_MAINPIPE_PREFIX}s1_fire",
    ),
    "main_s1_flush": (
        f"{_MAINPIPE_PREFIX}s1_flush",
    ),
    "main_s1_exception": (
        f"{_MAINPIPE_PREFIX}s1_exception_value",
        f"{_MAINPIPE_PREFIX}__Vtogcov__s1_exception_value",
    ),
    "main_s1_mmio": (
        f"{_MAINPIPE_PREFIX}s1_isMmio",
        f"{_MAINPIPE_PREFIX}__Vtogcov__s1_isMmio",
    ),
    "ifu_req0_ftq_flag": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_0_ftqIdx_flag",
    ),
    "ifu_req0_ftq_value": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_0_ftqIdx_value",
    ),
    "ifu_req1_ftq_flag": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_1_ftqIdx_flag",
    ),
    "ifu_req1_ftq_value": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_1_ftqIdx_value",
    ),
    "ifu_req0_start": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_0_startVAddr_addr",
    ),
    "ifu_req1_start": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_bits_1_startVAddr_addr",
    ),
    "ifu_s2_ftq0_flag": (
        f"{_IFU_PREFIX}s2_fetchBlock_0_ftqIdx_flag",
    ),
    "ifu_s2_ftq0_value": (
        f"{_IFU_PREFIX}s2_fetchBlock_0_ftqIdx_value",
    ),
    "ifu_s2_ftq1_flag": (
        f"{_IFU_PREFIX}s2_fetchBlock_1_ftqIdx_flag",
    ),
    "ifu_s2_ftq1_value": (
        f"{_IFU_PREFIX}s2_fetchBlock_1_ftqIdx_value",
    ),
    "backend_redirect_target": (
        "Frontend_top.io_backend_toFtq_redirect_bits_target",
        "Frontend_top.__Vtogcov__io_backend_toFtq_redirect_bits_target",
    ),
}


# Single source of truth for the 2-fetch coverpoint names used by the registry,
# JSON funcov sampler, and testpoint back-annotation.
TWO_FETCH_COVERPOINTS = {
    "two_fetch_ftq_eligibility": "request_eligibility",
    "two_fetch_pointer_advance": "fetch_ptr_step",
    "two_fetch_flush_flow": "flush_stage",
    "two_prefetch_layout": "address_layout",
    "two_fetch_waylookup_result": "serve_width",
    "two_fetch_waylookup_block_reason": "fallback_reason",
    "two_fetch_mainpipe_hit_pattern": "dual_hit_pattern",
    "two_fetch_mainpipe_completion": "completion_mode",
    "two_fetch_ifu_window": "window_width",
    "two_fetch_ifu_source": "source_mapping",
    "two_fetch_cross_block": "boundary_kind",
    "two_fetch_invalid_taken": "invalid_taken_block",
    "two_fetch_checker_priority": "earliest_fault",
    "two_fetch_checker_redirect": "select_block",
    "two_fetch_delivery": "delivery_state",
}


_RAW_INSTR_FIELDS = ("valid", "data", "isRvc", "blockSel", "isCrossBlockInstr", "startOffset")


def _tf_raw_instr_field_candidates(index: int, field: str) -> tuple[str, str]:
    base = "Frontend_top.Frontend.inner_ifu.instrBoundary."
    return (
        f"{base}io_resp_rawInstrVec_{index}_{field}",
        f"{base}__Vtogcov__io_resp_rawInstrVec_{index}_{field}",
    )


TWO_FETCH_SAMPLER_BIN_KEYS = frozenset(
    {
        ("two_fetch_ftq_eligibility", "eligible_dual"),
        ("two_fetch_ftq_eligibility", "blocked_runahead"),
        ("two_fetch_ftq_eligibility", "blocked_size"),
        ("two_fetch_ftq_eligibility", "blocked_cross_page"),
        ("two_fetch_ftq_eligibility", "blocked_backend_exception"),
        ("two_fetch_pointer_advance", "step_two"),
        ("two_fetch_pointer_advance", "step_one"),
        ("two_fetch_pointer_advance", "wrap_step_two"),
        ("two_fetch_flush_flow", "bpu_s3_drop_before_issue"),
        ("two_prefetch_layout", "same_line"),
        ("two_prefetch_layout", "overlap1"),
        ("two_prefetch_layout", "overlap2"),
        ("two_prefetch_layout", "interleave"),
        ("two_fetch_waylookup_result", "dual_served"),
        ("two_fetch_waylookup_block_reason", "insufficient_meta"),
        ("two_fetch_waylookup_block_reason", "data_bank_conflict"),
        ("two_fetch_waylookup_block_reason", "mmio"),
        ("two_fetch_waylookup_block_reason", "itlb_exception"),
        ("two_fetch_waylookup_result", "single_fallback"),
        ("two_fetch_mainpipe_hit_pattern", "hit_hit"),
        ("two_fetch_mainpipe_hit_pattern", "hit_miss"),
        ("two_fetch_mainpipe_hit_pattern", "miss_hit"),
        ("two_fetch_mainpipe_hit_pattern", "miss_miss"),
        ("two_fetch_mainpipe_completion", "wait_refill_then_dual"),
        ("two_fetch_ifu_window", "dual_window"),
        ("two_fetch_ifu_source", "blocksel_switch"),
        ("two_fetch_cross_block", "taken_separates_blocks"),
        ("two_fetch_ifu_source", "two_ftq_sources"),
        ("two_fetch_cross_block", "rvi_stitch"),
        ("two_fetch_cross_block", "rvc_independent"),
        ("two_fetch_cross_block", "mixed_rvc_rvi"),
        ("two_fetch_invalid_taken", "first_masks_second"),
        ("two_fetch_invalid_taken", "second_redirect"),
        ("two_fetch_checker_priority", "first_masks_second"),
        ("two_fetch_checker_redirect", "first_block"),
        ("two_fetch_checker_redirect", "second_block"),
        ("two_fetch_delivery", "two_ftq_entries_same_cycle"),
        ("two_fetch_delivery", "dual_fire"),
        ("two_fetch_delivery", "dual_stall"),
        ("two_fetch_flush_flow", "backend_redirect_drops_inflight"),
        ("two_fetch_checker_priority", "second_after_first_valid"),
    }
)


def _tf_read(recorder, key: str) -> int | None:
    return _read_first(recorder, _TWO_FETCH_SIGNALS[key])


def _tf_signal_or(recorder, names) -> int | None:
    values = [_read_first(recorder, (name,)) for name in names]
    readable = [int(value) for value in values if value is not None]
    if not readable:
        return None
    return int(any(readable))


def _tf_ftq_start(recorder, slot: int) -> int | None:
    """Read a raw two-fetch candidate address from the active FTQ entries."""
    direct = _tf_read(recorder, f"ftq_req{int(slot)}_start")
    if direct is not None:
        return direct
    fetch_ptr = _tf_read(recorder, "fetch_ptr_value")
    if fetch_ptr is None:
        return None
    index = (int(fetch_ptr) + int(slot)) % 64
    return _read_first(
        recorder,
        (f"Frontend_top.Frontend.inner_ftq.entryQueue_{index}_startPc_addr",),
    )


def _tf_fetch_size(start: int | None, end_position: int | None) -> int | None:
    """Match ``FtqFetchReq.size`` for the current 64 B / 32 B-aligned frontend.

    ``PrunedAddr.addr`` is halfword-addressed.  The low four bits therefore
    correspond to ``startVAddr(FetchBlockAlignWidth - 1, instOffsetBits)`` for
    the configured 32 B alignment.  ``endPosition`` is not a relative
    ``takenCfiOffset``; using it directly would overstate the request length.
    """
    if start is None or end_position is None:
        return None
    size = int(end_position) + 1 - (int(start) & 0xF)
    return size if 0 < size <= 32 else None


def _tf_vector(recorder, module: str, template: str, count: int) -> list[int | None]:
    values: list[int | None] = []
    for index in range(int(count)):
        signal = str(template).format(index=index, suffix="" if index == 0 else f"_{index}")
        values.append(
            _read_first(
                recorder,
                (
                    f"Frontend_top.Frontend.{module}.{signal}",
                ),
            )
        )
    return values


def _tf_evidence(event: str, **values) -> dict:
    return {"event": str(event), **{key: value for key, value in values.items() if value is not None}}


def _tf_next_ptr(flag: int, value: int, size: int) -> tuple[int, int]:
    value = int(value) + 1
    if value >= int(size):
        return int(flag) ^ 1, 0
    return int(flag), value


def _tf_ptr_at_or_after(left: tuple[int, int], right: tuple[int, int]) -> bool:
    left_flag, left_value = (int(left[0]), int(left[1]))
    right_flag, right_value = (int(right[0]), int(right[1]))
    return bool((left_flag != right_flag) ^ (left_value >= right_value))


def _tf_mainpipe_fail_reason(recorder) -> dict[str, bool] | None:
    """Reconstruct observable MainPipe s0 two-fetch fallback reasons.

    The current signal inventory does not expose ``perf_twoFetchFailReason``
    directly.  Do not guess it.  Only mark a reason when the same-transaction
    WayLookupInfo inputs prove the priority-encoded reason.  Data-bank conflict
    needs bankSel and waymask from the s0 transaction, which are not currently
    present in the inventory, so this helper never fabricates that bin.
    """
    waylookup1_valid = _tf_read(recorder, "main_wli1_valid")
    if waylookup1_valid is None:
        return None
    if int(waylookup1_valid) == 0:
        return {
            "insufficient_meta": True,
            "data_bank_conflict": False,
            "mmio": False,
            "itlb_exception": False,
        }

    # A sufficient proof that dataConflict is false: all cross-request line
    # pairs have identical vSetIdx, making the RTL differentSet term false
    # regardless of the unobservable bankSel/waymask inputs.
    vsets = [
        _tf_read(recorder, "main_wli0_vset0"),
        _tf_read(recorder, "main_wli0_vset1"),
        _tf_read(recorder, "main_wli1_vset0"),
        _tf_read(recorder, "main_wli1_vset1"),
    ]
    if any(value is None for value in vsets) or len({int(value) for value in vsets}) != 1:
        return None

    mmio_values = [
        _tf_read(recorder, "main_wli0_is_mmio"),
        _tf_read(recorder, "main_wli1_is_mmio"),
    ]
    itlb_values = [
        _tf_read(recorder, "main_wli0_itlb_exception"),
        _tf_read(recorder, "main_wli1_itlb_exception"),
    ]
    if any(value is None for value in [*mmio_values, *itlb_values]):
        return None

    has_mmio = any(int(value) != 0 for value in mmio_values)
    has_itlb_exception = any(int(value) != 0 for value in itlb_values)
    return {
        "insufficient_meta": False,
        "data_bank_conflict": False,
        "mmio": has_mmio,
        "itlb_exception": (not has_mmio) and has_itlb_exception,
    }


def _tf_tag(recorder, prefix: str) -> tuple[int, int] | None:
    flag = _tf_read(recorder, f"{prefix}_flag")
    value = _tf_read(recorder, f"{prefix}_value")
    if flag is None or value is None:
        return None
    return int(flag), int(value)


def _tf_any_vector(recorder, module: str, template: str, count: int) -> bool | None:
    values = _tf_vector(recorder, module, template, count)
    if any(value is None for value in values):
        return None
    return any(int(value) != 0 for value in values)


def _tf_line_vector(recorder, stem: str) -> list[int | None] | None:
    """Read both cache-line lanes for both MainPipe requests.

    The direct and Vtogcov spellings are both accepted because generated
    Verilator inventories expose one or the other depending on optimization.
    A partially readable vector is deliberately rejected by returning None.
    """
    values: list[int | None] = []
    for req_idx in range(2):
        for line_idx in range(2):
            suffix = f"_{req_idx}_{line_idx}"
            values.append(
                _read_first(
                    recorder,
                    (
                        f"{_MAINPIPE_PREFIX}{stem}{suffix}",
                        f"{_MAINPIPE_PREFIX}__Vtogcov__{stem}{suffix}",
                    ),
                )
            )
    if any(value is None for value in values):
        return None
    return [int(value) for value in values]


def _tf_ibuffer_payload(recorder) -> tuple[int, ...] | None:
    """Snapshot all observable IBuffer payload fields used by Decoupled stability.

    The generated offset inventory contains the Vtogcov view for every lane.  A
    missing field means the contract is incomplete, so this helper returns None
    instead of treating it as zero.
    """
    base = "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_bits_"
    registered = getattr(recorder, "_registered_internal_signals", None)
    if registered is None:
        return None
    names = sorted(str(name) for name in registered if str(name).startswith(base))
    if not names:
        return None
    values = [_read_first(recorder, (name,)) for name in names]
    if any(value is None for value in values):
        return None
    return tuple(int(value) for value in values)


def _tf_raw_instr_vector(recorder) -> list[dict] | None:
    """Read the complete InstrBoundary result for one IFU s0 transaction."""
    result: list[dict] = []
    for index in range(32):
        item = {}
        for field in _RAW_INSTR_FIELDS:
            value = _read_first(recorder, _tf_raw_instr_field_candidates(index, field))
            if value is None:
                if field == "isCrossBlockInstr":
                    item[field] = None
                    continue
                return None
            item[field] = int(value)
        result.append(item)
    return result


def _tf_first_block_raw_instr_count(recorder) -> int | None:
    raw = _tf_raw_instr_vector(recorder)
    if raw is None:
        return None
    return sum(1 for item in raw if item["valid"] == 1 and item["blockSel"] == 0)


def _tf_ibuffer_entries(recorder) -> list[dict] | None:
    """Read every enabled IBuffer lane with its PC, width, and FTQ identity."""
    base = "Frontend_top.Frontend.inner_ifu.__Vtogcov__io_toIBuffer_bits_"
    enable = _read_first(recorder, (base + "enqEnable",))
    if enable is None:
        return None
    entries: list[dict] = []
    for index in range(36):
        if ((int(enable) >> index) & 1) == 0:
            continue
        values = {
            "pc": _read_first(recorder, (f"{base}pc_{index}_addr",)),
            "is_rvc": _read_first(recorder, (f"{base}isRvc_{index}",)),
            "ftq_flag": _read_first(recorder, (f"{base}ftqPtr_{index}_flag",)),
            "ftq_value": _read_first(recorder, (f"{base}ftqPtr_{index}_value",)),
        }
        if any(value is None for value in values.values()):
            return None
        entries.append(
            {
                "slot": index,
                "pc": int(values["pc"]),
                "is_rvc": int(values["is_rvc"]),
                "ftq_ptr": (int(values["ftq_flag"]), int(values["ftq_value"])),
            }
        )
    return entries


def _tf_waylookup_write_observation(recorder, cycle: int) -> None:
    way_empty = _tf_read(recorder, "way_empty")
    write_flag = _tf_read(recorder, "way_write_ptr_flag")
    write_value = _tf_read(recorder, "way_write_ptr_value")
    write_ptr = None
    if write_flag is not None and write_value is not None:
        write_ptr = [int(write_flag), int(write_value)]

    previous = getattr(recorder, "_two_fetch_last_waylookup_write_state", None)
    ptr_changed = bool(
        write_ptr is not None
        and previous is not None
        and previous.get("write_ptr") is not None
        and write_ptr != previous.get("write_ptr")
    )
    was_empty = previous is not None and previous.get("empty") == 1
    should_record = bool(
        way_empty is not None
        and (int(way_empty) == 1 or ptr_changed or was_empty)
        and (write_ptr is not None or previous is not None)
    )
    if should_record:
        recorder.risk_observations.append(
            _tf_evidence(
                "waylookup_empty_write_timing",
                empty=int(way_empty),
                write_ptr=write_ptr,
                write_ptr_changed=int(ptr_changed),
                previous_empty=int(was_empty),
                previous_write_ptr=(None if previous is None else previous.get("write_ptr")),
                write_fire_observed="unavailable",
                cycle=cycle,
            )
        )

    if way_empty is not None or write_ptr is not None:
        recorder._two_fetch_last_waylookup_write_state = {
            "empty": None if way_empty is None else int(way_empty),
            "write_ptr": write_ptr,
        }


def _tf_remask_fault_count(recorder) -> int | None:
    """Return the observed prediction-fault count, or None if not observable."""
    dut = _dut(recorder)
    if dut is None:
        return None
    values: list[int | None] = []
    for index in range(34):
        values.append(
            _read_first(
                recorder,
                (
                    f"Frontend_top.Frontend.inner_ifu.predChecker.remaskFault_{index}",
                    f"Frontend_top.Frontend.inner_ifu.predChecker.__Vtogcov__remaskFault_{index}",
                ),
            )
        )
    if any(value is None for value in values):
        return None
    return sum(int(value) != 0 for value in values)


class _CoverageGroupRecorder:
    """Delegate recorder state while exposing marks for selected groups only."""

    def __init__(self, recorder, groups):
        object.__setattr__(self, "_recorder", recorder)
        object.__setattr__(self, "_groups", frozenset(groups))

    def __getattr__(self, name):
        return getattr(self._recorder, name)

    def __setattr__(self, name, value):
        setattr(self._recorder, name, value)

    def mark(self, group, *args, **kwargs):
        if group in self._groups:
            return self._recorder.mark(group, *args, **kwargs)
        return None


def sample_two_fetch_coverage(recorder, env, cycle: int, groups=None) -> None:
    if groups is not None:
        recorder = _CoverageGroupRecorder(recorder, groups)
    if _dut(recorder) is None:
        return

    cycle = int(cycle)
    _tf_waylookup_write_observation(recorder, cycle)
    backend_cfg = getattr(getattr(env, "config", None), "backend", None)
    ftq_size = int(getattr(backend_cfg, "ftq_size", 64) or 64)

    ftq_valid = _tf_read(recorder, "ftq_valid")
    ftq_ready = _tf_read(recorder, "ftq_ready")
    ftq_req1_valid = _tf_read(recorder, "ftq_req1_valid")
    ftq_fire = ftq_valid == 1 and ftq_ready == 1

    bpu_s3_flush = _tf_read(recorder, "bpu_s3_flush")
    if bpu_s3_flush == 1 and recorder._two_fetch_ftq_pending:
        pending = recorder._two_fetch_ftq_pending
        if isinstance(pending, dict):
            pending_ptr = pending.get("fetch_ptr")
            pending_cycle = pending.get("cycle")
        else:
            pending_ptr = None
            pending_cycle = None
        flush_flag = _tf_read(recorder, "bpu_s3_flush_ptr_flag")
        flush_value = _tf_read(recorder, "bpu_s3_flush_ptr_value")
        flush_ptr = None if None in (flush_flag, flush_value) else (int(flush_flag), int(flush_value))
        rollback_match = (
            pending_ptr is not None
            and flush_ptr is not None
            and _tf_ptr_at_or_after(tuple(pending_ptr), tuple(flush_ptr))
        )
        if rollback_match:
            rollback_distance = circular_distance(
                int(pending_ptr[0]), int(pending_ptr[1]), int(flush_ptr[0]), int(flush_ptr[1]), ftq_size
            )
            recorder.mark(
                "two_fetch_flush_flow",
                "bpu_s3_drop_before_issue",
                cycle,
                _tf_evidence(
                    "bpu_s3_flush_pending_dual",
                    pending_ptr=list(pending_ptr),
                    flush_ptr=list(flush_ptr),
                    rollback_distance=rollback_distance,
                    pending_cycle=pending_cycle,
                ),
            )
        else:
            recorder.risk_observations.append(
                _tf_evidence(
                    "bpu_s3_pending_dual_flush_ptr_unmatched_or_unobservable",
                    pending_ptr=None if pending_ptr is None else list(pending_ptr),
                    flush_ptr=None if flush_ptr is None else list(flush_ptr),
                    pending_cycle=pending_cycle,
                    cycle=cycle,
                )
            )
    main_s1_flush = _tf_read(recorder, "main_s1_flush")
    backend_redirect = _tf_read(recorder, "backend_redirect")
    if bpu_s3_flush == 1 or main_s1_flush == 1:
        # A flush invalidates both the s1 miss association and any held
        # payload.  Do not let a later response be attributed to the old FTQ
        # pair.
        recorder._two_fetch_refill_pending = None
        recorder._two_fetch_last_main_s1_tag = None
        recorder._two_fetch_stalled_payload = None
        recorder._two_fetch_stalled_payload_stable = True
        recorder._two_fetch_stalled_since = None
        if backend_redirect != 1:
            recorder._two_fetch_recent_inflight_tags = None
    pending_fetch_flag = _tf_read(recorder, "fetch_ptr_flag")
    pending_fetch_value = _tf_read(recorder, "fetch_ptr_value")
    recorder._two_fetch_ftq_pending = (
        {
            "cycle": cycle,
            "fetch_ptr": (
                None
                if None in (pending_fetch_flag, pending_fetch_value)
                else (int(pending_fetch_flag), int(pending_fetch_value))
            ),
        }
        if ftq_valid == 1 and ftq_ready == 0 and ftq_req1_valid == 1
        else False
    )

    if ftq_fire and ftq_req1_valid is not None:
        start0 = _tf_ftq_start(recorder, 0)
        start1 = _tf_ftq_start(recorder, 1)
        end0 = _tf_read(recorder, "ftq_req0_end")
        end1 = _tf_read(recorder, "ftq_req1_end")
        exc0 = _tf_read(recorder, "ftq_req0_exception")
        backend_exc = _tf_read(recorder, "ftq_backend_exception")
        backend_exc_flag = _tf_read(recorder, "ftq_backend_exception_ptr_flag")
        backend_exc_value = _tf_read(recorder, "ftq_backend_exception_ptr_value")
        bpu_flag = _tf_read(recorder, "bpu_ptr_flag")
        bpu_value = _tf_read(recorder, "bpu_ptr_value")
        fetch_flag = _tf_read(recorder, "fetch_ptr_flag")
        fetch_value = _tf_read(recorder, "fetch_ptr_value")
        runahead_distance = None
        if None not in (bpu_flag, bpu_value, fetch_flag, fetch_value):
            runahead_distance = circular_distance(
                int(bpu_flag), int(bpu_value), int(fetch_flag), int(fetch_value), ftq_size
            )
        # PrunedAddr.addr is halfword-addressed, so its bit 11 is virtual-address bit 12.
        cross_page = None not in (start0, start1) and (int(start0) >> 11) != (int(start1) >> 11)
        size0 = _tf_fetch_size(start0, end0)
        size1 = _tf_fetch_size(start1, end1)
        size_block = None not in (size0, size1) and int(size0) + int(size1) > 32
        backend_exception_candidate = False
        if None not in (
            backend_exc,
            backend_exc_flag,
            backend_exc_value,
            fetch_flag,
            fetch_value,
        ):
            exception_ptr = (int(backend_exc_flag), int(backend_exc_value))
            candidate0 = (int(fetch_flag), int(fetch_value))
            candidate1 = _tf_next_ptr(fetch_flag, fetch_value, ftq_size)
            backend_exception_candidate = int(backend_exc) != 0 and exception_ptr in (candidate0, candidate1)
        evidence = _tf_evidence(
            "ftq_to_mainpipe",
            req1_valid=int(ftq_req1_valid),
            start0=start0,
            start1=start1,
            end0=end0,
            end1=end1,
            size0=size0,
            size1=size1,
            exception0=exc0,
            backend_exception=backend_exc,
            backend_exception_ptr=(
                None
                if backend_exc_flag is None or backend_exc_value is None
                else [int(backend_exc_flag), int(backend_exc_value)]
            ),
            runahead_distance=runahead_distance,
        )
        if int(ftq_req1_valid) == 1:
            recorder.mark("two_fetch_ftq_eligibility", "eligible_dual", cycle, evidence)
        else:
            if runahead_distance is not None and int(runahead_distance) <= 3:
                recorder.mark("two_fetch_ftq_eligibility", "blocked_runahead", cycle, evidence)
            if size_block:
                recorder.mark("two_fetch_ftq_eligibility", "blocked_size", cycle, evidence)
            if cross_page:
                recorder.mark("two_fetch_ftq_eligibility", "blocked_cross_page", cycle, evidence)
            if exc0 == 1 or backend_exception_candidate:
                recorder.mark("two_fetch_ftq_eligibility", "blocked_backend_exception", cycle, evidence)

    fetch_flag = _tf_read(recorder, "fetch_ptr_flag")
    fetch_value = _tf_read(recorder, "fetch_ptr_value")
    if None not in (fetch_flag, fetch_value):
        current_ptr = (int(fetch_flag), int(fetch_value))
        last_ptr = recorder._two_fetch_last_fetch_ptr
        if last_ptr is not None:
            delta = circular_distance(
                current_ptr[0], current_ptr[1], last_ptr[0], last_ptr[1], ftq_size
            )
            evidence = _tf_evidence("fetch_ptr_advance", before=list(last_ptr), after=list(current_ptr), delta=delta)
            expected_step = recorder._two_fetch_expected_ptr_step
            if expected_step == 1 and delta == 1:
                recorder.mark("two_fetch_pointer_advance", "step_one", cycle, evidence)
            elif expected_step == 2 and delta == 2:
                recorder.mark("two_fetch_pointer_advance", "step_two", cycle, evidence)
                if current_ptr[1] < last_ptr[1] or current_ptr[0] != last_ptr[0]:
                    recorder.mark("two_fetch_pointer_advance", "wrap_step_two", cycle, evidence)
        recorder._two_fetch_expected_ptr_step = None
        recorder._two_fetch_last_fetch_ptr = current_ptr

    # #6221 moves the accepted transaction and the real-two decision to
    # MainPipe s0.  Keep the historical key names for registry compatibility,
    # but read only the MainPipe handshake and same-transaction result.
    main_req1_candidate = _tf_read(recorder, "way_req1_valid")
    main_valid = _tf_read(recorder, "way_out_valid")
    main_ready = _tf_read(recorder, "way_out_ready")
    main_real_two = _tf_read(recorder, "way_real_two")
    main_s0_fire = _tf_read(recorder, "main_s0_fire")
    main_fire = main_s0_fire == 1
    if main_fire and main_req1_candidate == 1 and main_real_two is not None:
        recorder._two_fetch_expected_ptr_step = 2 if int(main_real_two) == 1 else 1

    prefetch_valid = _tf_read(recorder, "prefetch_valid")
    prefetch_ready = _tf_read(recorder, "prefetch_ready")
    prefetch_case = _tf_read(recorder, "prefetch_case")
    if prefetch_valid == 1 and prefetch_ready == 1 and prefetch_case is not None:
        layout = {1: "same_line", 2: "overlap1", 4: "overlap2", 8: "interleave"}.get(int(prefetch_case))
        if layout is not None:
            recorder.mark(
                "two_prefetch_layout",
                layout,
                cycle,
                _tf_evidence("two_prefetch", case=int(prefetch_case)),
            )

    if main_fire and main_req1_candidate == 1 and main_real_two is not None:
        way_empty = _tf_read(recorder, "way_empty")
        write_flag = _tf_read(recorder, "way_write_ptr_flag")
        write_value = _tf_read(recorder, "way_write_ptr_value")
        evidence = _tf_evidence(
            "mainpipe_s0_fire",
            real_two=int(main_real_two),
        )
        if int(main_real_two) == 1:
            recorder.mark("two_fetch_waylookup_result", "dual_served", cycle, evidence)
        else:
            main_reason = _tf_mainpipe_fail_reason(recorder)
            if main_reason is not None:
                evidence["mainpipe_fail_reason_flags"] = {
                    key: int(value) for key, value in main_reason.items()
                }
            if way_empty is not None:
                evidence["waylookup_empty"] = int(way_empty)
            if write_flag is not None and write_value is not None:
                evidence["waylookup_write_ptr"] = [int(write_flag), int(write_value)]
            recorder.mark("two_fetch_waylookup_result", "single_fallback", cycle, evidence)
            if main_reason is not None:
                for bin_name, hit in main_reason.items():
                    if hit:
                        recorder.mark("two_fetch_waylookup_block_reason", bin_name, cycle, evidence)
            else:
                recorder.risk_observations.append(
                    _tf_evidence(
                        "mainpipe_fallback_reason_unobservable",
                        real_two=int(main_real_two),
                        cycle=cycle,
                    )
                )

    main_valid = _tf_read(recorder, "main_s1_valid")
    main_req1 = _tf_read(recorder, "main_req1_valid")
    main_exception = _tf_read(recorder, "main_s1_exception")
    main_mmio = _tf_read(recorder, "main_s1_mmio")
    should_fetch = _tf_vector(recorder, "inner_icache.mainPipe", "s1_shouldFetch_{index}", 4)
    main_s1_tag = None
    if main_valid == 1 and main_req1 == 1:
        tag0 = _tf_tag(recorder, "main_s1_ftq0")
        tag1 = _tf_tag(recorder, "main_s1_ftq1")
        if tag0 is not None and tag1 is not None:
            main_s1_tag = (tag0, tag1)
    is_new_main_transaction = main_s1_tag is not None and main_s1_tag != getattr(
        recorder, "_two_fetch_last_main_s1_tag", None
    )
    if (
        is_new_main_transaction
        and main_exception == 0
        and main_mmio == 0
        and main_s1_flush != 1
        and backend_redirect != 1
        and all(value is not None for value in should_fetch)
    ):
        previous_pending = getattr(recorder, "_two_fetch_refill_pending", None)
        if previous_pending is not None:
            recorder.risk_observations.append(
                _tf_evidence(
                    "two_fetch_refill_replaced_before_completion",
                    previous_tag=previous_pending.get("tag"),
                    new_tag=[list(main_s1_tag[0]), list(main_s1_tag[1])],
                    cycle=cycle,
                )
            )
        req0_miss = bool(int(should_fetch[0]) or int(should_fetch[1]))
        req1_miss = bool(int(should_fetch[2]) or int(should_fetch[3]))
        pattern = {
            (False, False): "hit_hit",
            (False, True): "hit_miss",
            (True, False): "miss_hit",
            (True, True): "miss_miss",
        }[(req0_miss, req1_miss)]
        recorder._two_fetch_recent_inflight_tags = {
            "tags": main_s1_tag,
            "cycle": cycle,
            "has_miss": bool(req0_miss or req1_miss),
            "pattern": pattern,
        }
        recorder._two_fetch_last_main_s1_tag = main_s1_tag
        recorder._two_fetch_refill_pending = {
            "tag": main_s1_tag,
            "pattern": pattern,
            "miss_cycle": cycle,
            "has_miss": bool(req0_miss or req1_miss),
            "required_lines": [bool(int(value)) for value in should_fetch],
            "raw_refill_cycles": [None, None, None, None],
            "registered_refill_cycles": [None, None, None, None],
        }

    pending_refill = getattr(recorder, "_two_fetch_refill_pending", None)
    raw_refill = _tf_line_vector(recorder, "s1_mshrValid")
    registered_refill = _tf_line_vector(recorder, "s1_mshrValidReg")
    if pending_refill is not None:
        required_lines = pending_refill.get("required_lines", [False] * 4)
        raw_cycles = pending_refill.setdefault("raw_refill_cycles", [None] * 4)
        registered_cycles = pending_refill.setdefault("registered_refill_cycles", [None] * 4)
        if raw_refill is not None and registered_refill is not None:
            for index, required in enumerate(required_lines):
                if not required:
                    continue
                if int(raw_refill[index]) != 0 and raw_cycles[index] is None:
                    raw_cycles[index] = cycle
                if int(registered_refill[index]) != 0 and registered_cycles[index] is None:
                    registered_cycles[index] = cycle

    ifu_valid = _tf_read(recorder, "ifu_valid")
    ifu_ready = _tf_read(recorder, "ifu_ready")
    ifu_req1 = _tf_read(recorder, "ifu_req1_valid")
    ifu_fire = ifu_valid == 1 and ifu_ready == 1
    if ifu_fire and ifu_req1 == 1:
        recorder._two_fetch_last_dual_cycle = cycle
        recorder.mark("two_fetch_ifu_window", "dual_window", cycle, _tf_evidence("icache_to_ifu_dual"))
        if pending_refill is not None:
            ifu_tag0 = _tf_tag(recorder, "ifu_req0_ftq")
            ifu_tag1 = _tf_tag(recorder, "ifu_req1_ftq")
            observed_tag = None if ifu_tag0 is None or ifu_tag1 is None else (ifu_tag0, ifu_tag1)
            expected_tag = pending_refill.get("tag")
            required_lines = pending_refill.get("required_lines", [False] * 4)
            raw_cycles = pending_refill.get("raw_refill_cycles", [None] * 4)
            registered_cycles = pending_refill.get("registered_refill_cycles", [None] * 4)
            registered_complete = all(
                not required
                or (
                    raw_cycles[index] is not None
                    and registered_cycles[index] is not None
                    and int(registered_cycles[index]) == int(raw_cycles[index]) + 1
                )
                for index, required in enumerate(required_lines)
            )
            if observed_tag == expected_tag and registered_complete:
                completion_evidence = _tf_evidence(
                    "dual_after_mainpipe_completion",
                    ftq_tag=[list(expected_tag[0]), list(expected_tag[1])],
                    initial_cycle=pending_refill.get("miss_cycle"),
                    raw_refill_cycles=raw_cycles,
                    registered_refill_cycles=registered_cycles,
                )
                recorder.mark(
                    "two_fetch_mainpipe_hit_pattern",
                    str(pending_refill.get("pattern")),
                    cycle,
                    completion_evidence,
                )
                if pending_refill.get("has_miss"):
                    recorder.mark(
                        "two_fetch_mainpipe_completion",
                        "wait_refill_then_dual",
                        cycle,
                        completion_evidence,
                    )
                recorder._two_fetch_refill_pending = None
            else:
                recorder.risk_observations.append(
                    _tf_evidence(
                        "two_fetch_refill_tag_mismatch",
                        expected_tag=(None if expected_tag is None else [list(expected_tag[0]), list(expected_tag[1])]),
                        observed_tag=(None if observed_tag is None else [list(observed_tag[0]), list(observed_tag[1])]),
                        raw_refill_cycles=raw_cycles,
                        registered_refill_cycles=registered_cycles,
                        registered_complete=int(registered_complete),
                        cycle=cycle,
                    )
                )

        first_size = _tf_read(recorder, "ifu_req0_size")
        raw_instr = _tf_raw_instr_vector(recorder)
        unknown_cross_indices = (
            []
            if raw_instr is None
            else [index for index, item in enumerate(raw_instr) if item.get("isCrossBlockInstr") is None]
        )
        if unknown_cross_indices and not getattr(recorder, "_two_fetch_raw_cross_unknown_reported", False):
            recorder.risk_observations.append(
                _tf_evidence(
                    "raw_instr_cross_flag_unobservable",
                    indices=unknown_cross_indices,
                    note=(
                        "missing isCrossBlockInstr is not defaulted; bins requiring this flag "
                        "must remain unhit unless another observable relation proves them"
                    ),
                )
            )
            recorder._two_fetch_raw_cross_unknown_reported = True
        cross_indices = (
            []
            if raw_instr is None
            else [index for index, item in enumerate(raw_instr) if item.get("isCrossBlockInstr") == 1]
        )
        blocksel_exact = False
        if raw_instr is not None and first_size is not None and 0 < int(first_size) < len(raw_instr):
            size = int(first_size)
            expected_blocksels = []
            for index, item in enumerate(raw_instr):
                cross_flag = item.get("isCrossBlockInstr")
                if index == size - 1 and cross_flag is None:
                    expected_blocksels = []
                    break
                expected_blocksels.append(
                    item["blockSel"]
                    == int(index >= size or (index == size - 1 and cross_flag == 1))
                )
            blocksel_exact = bool(expected_blocksels) and all(expected_blocksels)
            first_valid = any(item["valid"] == 1 and item["blockSel"] == 0 for item in raw_instr)
            second_valid = any(item["valid"] == 1 and item["blockSel"] == 1 for item in raw_instr)
            if blocksel_exact and first_valid and second_valid:
                recorder.mark(
                    "two_fetch_ifu_source",
                    "blocksel_switch",
                    cycle,
                    _tf_evidence(
                        "ifu_blocksel_exact",
                        first_size=size,
                        cross_indices=cross_indices,
                    ),
                )

            if len(cross_indices) == 1:
                cross_index = cross_indices[0]
                cross_item = raw_instr[cross_index]
                start0 = _tf_read(recorder, "ifu_req0_start")
                start1 = _tf_read(recorder, "ifu_req1_start")
                cross_pc = None if start0 is None else int(start0) + cross_index
                low_half = high_half = None
                if cross_pc is not None:
                    low_raw, _ = _read_expected_fetch_raw(env, int(cross_pc) << 1, 2)
                    low_half = None if low_raw is None else int(low_raw) & 0xFFFF
                if start1 is not None:
                    high_raw, _ = _read_expected_fetch_raw(env, int(start1) << 1, 2)
                    high_half = None if high_raw is None else int(high_raw) & 0xFFFF
                expected_data = (
                    None
                    if low_half is None or high_half is None
                    else int(low_half) | (int(high_half) << 16)
                )
                next_half_not_duplicated = (
                    cross_index + 1 < len(raw_instr) and raw_instr[cross_index + 1]["valid"] == 0
                )
                if (
                    blocksel_exact
                    and cross_index == size - 1
                    and cross_item["valid"] == 1
                    and cross_item["isRvc"] == 0
                    and cross_item["blockSel"] == 1
                    and cross_item["startOffset"] == 31
                    and start0 is not None
                    and start1 is not None
                    and int(start1) == int(start0) + size
                    and expected_data is not None
                    and cross_item["data"] == expected_data
                    and next_half_not_duplicated
                ):
                    recorder.mark(
                        "two_fetch_cross_block",
                        "rvi_stitch",
                        cycle,
                        _tf_evidence(
                            "cross_block_rvi_exact",
                            raw_index=cross_index,
                            pc=int(cross_pc) << 1,
                            data=int(cross_item["data"]),
                            first_size=size,
                        ),
                    )

        if (
            _tf_read(recorder, "ifu_first_taken") == 1
            and raw_instr is not None
            and blocksel_exact
            and not cross_indices
        ):
            recorder.mark(
                "two_fetch_cross_block",
                "taken_separates_blocks",
                cycle,
                _tf_evidence("first_taken_no_cross_stitch"),
            )

    if (
        _tf_read(recorder, "ifu_s1_valid") == 1
        and _tf_read(recorder, "ifu_first_invalid") == 1
        and _tf_read(recorder, "ifu_req1_valid") == 1
        and _tf_read(recorder, "ifu_flush") != 1
    ):
        ifu_s1_instr_count = _tf_read(recorder, "ifu_s1_instr_count")
        first_block_count = _tf_first_block_raw_instr_count(recorder)
        if ifu_s1_instr_count is not None and first_block_count is not None and int(ifu_s1_instr_count) == int(first_block_count):
            recorder.mark(
                "two_fetch_invalid_taken",
                "first_masks_second",
                cycle,
                _tf_evidence("s1_first_invalid_taken_masks_second"),
            )

    checker_valid = _tf_read(recorder, "checker_valid")
    checker_select = _tf_read(recorder, "checker_select")
    checker_invalid = _tf_read(recorder, "checker_invalid")
    fixed_instr_valid = _tf_read(recorder, "fixed_instr_valid")
    remask_fault_count = _tf_remask_fault_count(recorder)
    recent_dual = recorder._two_fetch_last_dual_cycle is not None and (
        cycle - int(recorder._two_fetch_last_dual_cycle)
    ) <= 8
    if checker_valid == 1 and checker_select is not None and recent_dual:
        selected_bin = "second_block" if int(checker_select) else "first_block"
        recorder.mark(
            "two_fetch_checker_redirect",
            selected_bin,
            cycle,
            _tf_evidence("checker_redirect", select_block=int(checker_select), invalid_taken=checker_invalid),
        )
        if int(checker_select) == 0 and remask_fault_count is not None and remask_fault_count >= 2:
            recorder.mark(
                "two_fetch_checker_priority",
                "first_masks_second",
                cycle,
                _tf_evidence("checker_first_block", remask_fault_count=remask_fault_count),
            )
        elif (
            int(checker_select) == 1
            and int(checker_invalid or 0) == 1
            and fixed_instr_valid is not None
            and (int(fixed_instr_valid) & 0x1) != 0
        ):
            recorder.mark(
                "two_fetch_checker_priority",
                "second_after_first_valid",
                cycle,
                _tf_evidence(
                    "checker_second_block",
                    fixed_instr_valid=int(fixed_instr_valid),
                ),
            )
            if checker_invalid == 1:
                recorder.mark(
                    "two_fetch_invalid_taken",
                    "second_redirect",
                    cycle,
                    _tf_evidence("checker_second_invalid_taken"),
                )

    second_valid = _tf_read(recorder, "ifu_second_valid")
    s2_valid = _tf_read(recorder, "ifu_s2_valid")
    to_ibuffer_valid = _tf_read(recorder, "to_ibuffer_valid")
    to_ibuffer_ready = _tf_read(recorder, "to_ibuffer_ready")
    if (
        second_valid == 1
        and s2_valid == 1
        and to_ibuffer_valid == 1
        and backend_redirect != 1
        and bpu_s3_flush != 1
        and main_s1_flush != 1
        and _tf_read(recorder, "ifu_flush") != 1
    ):
        recorder._two_fetch_last_dual_cycle = cycle
        if to_ibuffer_ready == 1:
            current_payload = _tf_ibuffer_payload(recorder)
            stalled_payload = getattr(recorder, "_two_fetch_stalled_payload", None)
            if stalled_payload is not None:
                stable_ok = bool(getattr(recorder, "_two_fetch_stalled_payload_stable", True))
                stalled_since = getattr(recorder, "_two_fetch_stalled_since", None)
                if stable_ok:
                    recorder.mark(
                        "two_fetch_delivery",
                        "dual_stall",
                        cycle,
                        _tf_evidence(
                            "to_ibuffer_dual_stall_payload_stable_until_fire",
                            stalled_since=stalled_since,
                            fire_cycle=cycle,
                            release_sample_matches_held=(
                                None
                                if current_payload is None
                                else int(current_payload == stalled_payload)
                            ),
                            payload_sha=hashlib.sha256(repr(stalled_payload).encode("ascii")).hexdigest(),
                        ),
                    )
                elif current_payload is None or current_payload != stalled_payload:
                    recorder.risk_observations.append(
                        _tf_evidence(
                            "ibuffer_payload_changed_under_backpressure",
                            cycle=cycle,
                            stalled_since=stalled_since,
                            current_payload_observable=int(current_payload is not None),
                        )
                    )
                recorder._two_fetch_stalled_payload = None
                recorder._two_fetch_stalled_payload_stable = True
                recorder._two_fetch_stalled_since = None
            entries = _tf_ibuffer_entries(recorder)
            s2_tag0 = _tf_tag(recorder, "ifu_s2_ftq0")
            s2_tag1 = _tf_tag(recorder, "ifu_s2_ftq1")
            expected_tags = None if s2_tag0 is None or s2_tag1 is None else (s2_tag0, s2_tag1)
            compressed_tags = []
            if entries is not None:
                for entry in entries:
                    if not compressed_tags or entry["ftq_ptr"] != compressed_tags[-1]:
                        compressed_tags.append(entry["ftq_ptr"])
            pc_ordered = bool(entries)
            if entries:
                for before, after in zip(entries, entries[1:]):
                    if before["ftq_ptr"] == after["ftq_ptr"]:
                        expected_step = 1 if before["is_rvc"] else 2
                        if int(after["pc"]) - int(before["pc"]) != expected_step:
                            pc_ordered = False
                            break
            exact_dual_payload = (
                expected_tags is not None
                and entries is not None
                and compressed_tags == [expected_tags[0], expected_tags[1]]
                and pc_ordered
            )
            if exact_dual_payload:
                evidence = _tf_evidence(
                    "to_ibuffer_dual_fire_exact",
                    ftq_tags=[list(expected_tags[0]), list(expected_tags[1])],
                    entries=entries,
                )
                recorder.mark("two_fetch_delivery", "dual_fire", cycle, evidence)
                recorder._two_fetch_expected_cfvec = {
                    "tags": expected_tags,
                    "cycle": cycle,
                }

        elif to_ibuffer_ready == 0:
            payload = _tf_ibuffer_payload(recorder)
            previous_payload = getattr(recorder, "_two_fetch_stalled_payload", None)
            if payload is not None and previous_payload is None:
                recorder._two_fetch_stalled_since = cycle
                recorder._two_fetch_stalled_payload_stable = True
            elif payload is not None and previous_payload is not None and payload != previous_payload:
                recorder.risk_observations.append(
                    _tf_evidence(
                        "ibuffer_payload_changed_under_backpressure",
                        cycle=cycle,
                    )
                )
                recorder._two_fetch_stalled_payload_stable = False
            recorder._two_fetch_stalled_payload = payload
        else:
            recorder._two_fetch_stalled_payload = None
            recorder._two_fetch_stalled_payload_stable = True
            recorder._two_fetch_stalled_since = None
    elif (
        recorder._two_fetch_stalled_payload is not None
        and to_ibuffer_valid == 1
        and to_ibuffer_ready == 0
    ):
        payload = _tf_ibuffer_payload(recorder)
        if payload is None or payload != recorder._two_fetch_stalled_payload:
            recorder.risk_observations.append(
                _tf_evidence("ibuffer_payload_changed_under_backpressure", cycle=cycle)
            )
            recorder._two_fetch_stalled_payload_stable = False
        recorder._two_fetch_stalled_payload = payload
    elif to_ibuffer_valid != 1:
        recorder._two_fetch_stalled_payload = None
        recorder._two_fetch_stalled_payload_stable = True
        recorder._two_fetch_stalled_since = None

    redirect_pending = getattr(recorder, "_two_fetch_redirect_pending", None)
    if redirect_pending is not None and to_ibuffer_valid == 1 and to_ibuffer_ready == 1:
        entries = _tf_ibuffer_entries(recorder)
        if entries is not None:
            compressed_tags = []
            for entry in entries:
                if not compressed_tags or entry["ftq_ptr"] != compressed_tags[-1]:
                    compressed_tags.append(entry["ftq_ptr"])
            old_tags = set(redirect_pending.get("old_tags") or ())
            delivered_old = any(entry["ftq_ptr"] in old_tags for entry in entries)
            target = redirect_pending.get("target")
            first_pc = int(entries[0]["pc"]) << 1 if entries else None
            if delivered_old:
                recorder.risk_observations.append(
                    _tf_evidence(
                        "two_fetch_redirect_old_tag_delivery",
                        old_tags=[list(tag) for tag in sorted(old_tags)],
                        delivered_tags=[list(tag) for tag in compressed_tags],
                        cycle=cycle,
                    )
                )
            elif target is not None and first_pc == int(target):
                recorder.mark(
                    "two_fetch_flush_flow",
                    "backend_redirect_drops_inflight",
                    cycle,
                    _tf_evidence(
                        "backend_redirect_first_new_delivery",
                        target=int(target),
                        first_pc=first_pc,
                        old_tags=[list(tag) for tag in sorted(old_tags)],
                        delivered_tags=[list(tag) for tag in compressed_tags],
                    ),
                )
            else:
                recorder.risk_observations.append(
                    _tf_evidence(
                        "two_fetch_redirect_wrong_first_delivery",
                        target=target,
                        first_pc=first_pc,
                        cycle=cycle,
                    )
                )
            recorder._two_fetch_redirect_pending = None

    if backend_redirect == 1:
        in_flight_tags = None
        pending_refill = getattr(recorder, "_two_fetch_refill_pending", None)
        expected_cfvec = getattr(recorder, "_two_fetch_expected_cfvec", None)
        current_s2_tag0 = _tf_tag(recorder, "ifu_s2_ftq0")
        current_s2_tag1 = _tf_tag(recorder, "ifu_s2_ftq1")
        if pending_refill is not None:
            in_flight_tags = pending_refill.get("tag")
        elif expected_cfvec is not None:
            in_flight_tags = expected_cfvec.get("tags")
        elif second_valid == 1 and s2_valid == 1 and None not in (current_s2_tag0, current_s2_tag1):
            in_flight_tags = (current_s2_tag0, current_s2_tag1)
        else:
            recent = getattr(recorder, "_two_fetch_recent_inflight_tags", None)
            if recent is not None and cycle - int(recent.get("cycle", cycle)) <= 128:
                in_flight_tags = recent.get("tags")
        redirect_target = _tf_read(recorder, "backend_redirect_target")
        if (
            in_flight_tags is not None
            and redirect_target is not None
        ):
            recorder._two_fetch_redirect_pending = {
                "old_tags": tuple(in_flight_tags),
                "target": int(redirect_target),
                "cycle": cycle,
                "ifu_flush": _tf_read(recorder, "ifu_flush"),
                "main_s1_flush": main_s1_flush,
            }
        recorder._two_fetch_last_fetch_ptr = None
        recorder._two_fetch_refill_pending = None
        recorder._two_fetch_ftq_pending = False
        recorder._two_fetch_last_dual_cycle = None
        recorder._two_fetch_last_waylookup_write_state = None
        recorder._two_fetch_last_main_s1_tag = None
        recorder._two_fetch_stalled_payload = None
        recorder._two_fetch_stalled_payload_stable = True
        recorder._two_fetch_stalled_since = None
        recorder._two_fetch_expected_cfvec = None
        recorder._two_fetch_recent_inflight_tags = None

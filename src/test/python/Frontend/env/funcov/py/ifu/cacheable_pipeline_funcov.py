from __future__ import annotations

from collections import deque
from typing import Optional

from ..common.dut import _dut, _read_first
from .owner_v3_funcov import mark_owner_v3_checked


_ICACHE_PREFIX = "Frontend_top.Frontend.inner_icache."
_IFU_PREFIX = "Frontend_top.Frontend.inner_ifu."
_MAINPIPE_PREFIX = f"{_ICACHE_PREFIX}mainPipe."


IFU_CACHEABLE_PIPELINE_COVERPOINTS = {
    "ifu_cacheable_ingress": "ingress_state",
    "ifu_cacheable_transfer": "transfer_state",
    "ifu_cacheable_window": "window_shape",
    "ifu_cacheable_metadata": "metadata_transfer",
    "ifu_cacheable_address": "start_position",
    "ifu_cacheable_range": "range_shape",
    "ifu_cacheable_s1": "handshake_path",
    "ifu_cacheable_flush": "flush_behavior",
}


IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS = frozenset(
    {
        ("ifu_cacheable_ingress", "accepted"),
        ("ifu_cacheable_ingress", "backpressured"),
        ("ifu_cacheable_ingress", "backpressure_payload_stable"),
        ("ifu_cacheable_transfer", "s1_payload_stable"),
        ("ifu_cacheable_ingress", "back_to_back_accept"),
        ("ifu_cacheable_transfer", "gapped_metadata_isolated"),
        ("ifu_cacheable_window", "single_block"),
        ("ifu_cacheable_window", "dual_block"),
        ("ifu_cacheable_metadata", "first_ftq_preserved"),
        ("ifu_cacheable_metadata", "second_ftq_preserved"),
        ("ifu_cacheable_metadata", "not_taken_preserved"),
        ("ifu_cacheable_address", "head_mid_tail_seen"),
        ("ifu_cacheable_address", "align_2b_4b_seen"),
        ("ifu_cacheable_range", "sequential_full_fetch"),
        ("ifu_cacheable_metadata", "taken_offset_preserved"),
        ("ifu_cacheable_window", "cross_cacheline_dual_block"),
        ("ifu_cacheable_range", "fetch_size_variation"),
        ("ifu_cacheable_metadata", "ftq_pointer_progression"),
        ("ifu_cacheable_s1", "fire_to_s2"),
        ("ifu_cacheable_s1", "response_backpressured_by_s2"),
        ("ifu_cacheable_s1", "source_ftq_address_matched"),
        ("ifu_cacheable_s1", "single_cacheable_path"),
        ("ifu_cacheable_s1", "dual_cacheable_path"),
        ("ifu_cacheable_s1", "cacheable_no_uncache"),
        ("ifu_cacheable_s1", "s0_accept_to_s1_valid"),
        ("ifu_cacheable_flush", "backend_redirect_blocks"),
        ("ifu_cacheable_flush", "wb_redirect_blocks"),
        ("ifu_cacheable_flush", "bpu_match_blocks"),
        ("ifu_cacheable_flush", "bpu_miss_allows"),
        ("ifu_cacheable_flush", "flush_wins_fire"),
    }
)


_SIGNALS = {
    "req_valid": (f"{_ICACHE_PREFIX}mainPipe.io_toIfu_req_valid",),
    "req_ready": (f"{_ICACHE_PREFIX}mainPipe.io_toIfu_req_ready",),
    "s0_fire": (f"{_IFU_PREFIX}s0_fire", f"{_IFU_PREFIX}__Vtogcov__s0_fire"),
    "s0_flush": (f"{_IFU_PREFIX}s0_flush", f"{_IFU_PREFIX}__Vtogcov__s0_flush"),
    "s0_flush_bpu": (
        f"{_IFU_PREFIX}s0_flushFromBpu",
        f"{_IFU_PREFIX}__Vtogcov__s0_flushFromBpu",
    ),
    "s1_valid": (f"{_IFU_PREFIX}s1_valid",),
    "s1_ready": (f"{_IFU_PREFIX}s1_ready", f"{_IFU_PREFIX}__Vtogcov__s1_ready"),
    "s1_flush": (f"{_IFU_PREFIX}s1_flush", f"{_IFU_PREFIX}__Vtogcov__s1_flush"),
    "s1_fire": (f"{_IFU_PREFIX}s1_fire", f"{_IFU_PREFIX}__Vtogcov__s1_fire"),
    "s1_req_uncache": (
        f"{_IFU_PREFIX}s1_reqIsUncache",
        f"{_IFU_PREFIX}__Vtogcov__s1_reqIsUncache",
    ),
    "s1_exception": (
        f"{_IFU_PREFIX}s1_icacheMeta_0_exception_value",
        f"{_IFU_PREFIX}__Vtogcov__s1_icacheMeta_0_exception_value",
    ),
    "backend_redirect": ("Frontend_top.io_backend_toFtq_redirect_valid",),
    "wb_redirect": (
        f"{_IFU_PREFIX}__Vtogcov__wbRedirect_valid",
        f"{_IFU_PREFIX}__Vtogcov__io_toFtq_wbRedirect_valid",
    ),
    "bpu_s3_flush": (
        f"{_ICACHE_PREFIX}__Vtogcov__io_fromFtq_flushFromBpu_s3_valid",
        f"{_ICACHE_PREFIX}mainPipe.io_flushFromBpu_s3_valid",
    ),
}

_UPSTREAM_SIGNALS = {
    "mainpipe_fire": (f"{_MAINPIPE_PREFIX}s0_fire",),
    "second_requested": (f"{_MAINPIPE_PREFIX}io_fromFtq_bits_req_1_valid",),
    "second_waylookup_valid": (
        f"{_MAINPIPE_PREFIX}io_fromWayLookup_bits_wayLookupInfo_1_valid",
    ),
    "first_mmio": (
        f"{_MAINPIPE_PREFIX}io_fromWayLookup_bits_wayLookupInfo_0_bits_entry_isMmio",
    ),
    "second_mmio": (
        f"{_MAINPIPE_PREFIX}io_fromWayLookup_bits_wayLookupInfo_1_bits_entry_isMmio",
    ),
    "first_itlb_exception": (
        f"{_MAINPIPE_PREFIX}io_fromWayLookup_bits_wayLookupInfo_0_bits_exceptionEntry_itlbException_value",
        f"{_MAINPIPE_PREFIX}__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_0_bits_exceptionEntry_itlbException_value",
    ),
    "second_itlb_exception": (
        f"{_MAINPIPE_PREFIX}io_fromWayLookup_bits_wayLookupInfo_1_bits_exceptionEntry_itlbException_value",
        f"{_MAINPIPE_PREFIX}__Vtogcov__io_fromWayLookup_bits_wayLookupInfo_1_bits_exceptionEntry_itlbException_value",
    ),
    "first_ftq_flag": (
        f"{_MAINPIPE_PREFIX}io_fromFtq_bits_req_0_ftqIdx_flag",
        "Frontend_top.Frontend.inner_ftq.fetchPtr_ptrs_0_flag",
    ),
    "first_ftq_value": (
        f"{_MAINPIPE_PREFIX}io_fromFtq_bits_req_0_ftqIdx_value",
        "Frontend_top.Frontend.inner_ftq.fetchPtr_ptrs_0_value",
    ),
    "real_two_fetch": (
        f"{_MAINPIPE_PREFIX}s0_realTwoFetchValid",
        "inner_icache.mainPipe.s0_realTwoFetchValid",
        f"{_ICACHE_PREFIX}dataArray.io_read_req_bits_1_valid",
    ),
}

_LATE_FAULT_SIGNALS = {
    "line0_corrupt": (f"{_MAINPIPE_PREFIX}io_toIfu_corrupt_0_0",),
    "line1_corrupt": (f"{_MAINPIPE_PREFIX}io_toIfu_corrupt_0_1",),
    "s1_meta_in_exception": (
        f"{_IFU_PREFIX}s1_icacheMetaIn_0_exception_value",
    ),
    "s1_merged_exception": (
        f"{_IFU_PREFIX}s1_icacheMeta_0_exception_value",
    ),
    "s1_ftq_flag": (f"{_IFU_PREFIX}s1_fetchBlock_0_ftqIdx_flag",),
    "s1_ftq_value": (f"{_IFU_PREFIX}s1_fetchBlock_0_ftqIdx_value",),
    "s2_valid": (f"{_IFU_PREFIX}s2_valid_valid",),
    "s2_ftq_flag": (f"{_IFU_PREFIX}s2_fetchBlock_0_ftqIdx_flag",),
    "s2_ftq_value": (f"{_IFU_PREFIX}s2_fetchBlock_0_ftqIdx_value",),
    "s2_exception": (f"{_IFU_PREFIX}s2_icacheMeta_0_exception_value",),
    "s2_instr_count": (f"{_IFU_PREFIX}s2_instrCount",),
    "to_ibuffer_valid": (f"{_IFU_PREFIX}io_toIBuffer_valid",),
    "to_ibuffer_ready": (
        f"{_IFU_PREFIX}__Vtogcov__io_toIBuffer_ready",
        "inner_ifu.io_toIBuffer_ready",
    ),
    "to_ibuffer_exception": (
        f"{_IFU_PREFIX}__Vtogcov__io_toIBuffer_bits_exceptionType_value",
    ),
    "to_ibuffer_enq": (
        f"{_IFU_PREFIX}__Vtogcov__io_toIBuffer_bits_enqEnable",
    ),
}


def initialize_ifu_cacheable_pipeline_state(recorder) -> None:
    recorder._ifu_cacheable_pending_transfer = None
    recorder._ifu_cacheable_ingress_stall = None
    recorder._ifu_cacheable_s1_stall = None
    recorder._ifu_cacheable_last_accept_cycle = None
    recorder._ifu_cacheable_last_verified = None
    recorder._ifu_cacheable_verified_windows = deque(maxlen=64)
    recorder._ifu_cacheable_start_regions = set()
    recorder._ifu_cacheable_alignments = set()
    recorder._ifu_cacheable_fetch_sizes = set()
    recorder._ifu_cacheable_last_ftq_ptr = None
    recorder._ifu_upstream_suppression_pending = deque(maxlen=8)
    recorder._ifu_late_fault_stall_pending = None
    recorder._ifu_late_fault_delivery_pending = None
    recorder._ifu_late_fault_flush_pending = None


def reset_ifu_cacheable_pipeline_state(recorder) -> None:
    initialize_ifu_cacheable_pipeline_state(recorder)


def _read_signal(recorder, key: str) -> Optional[int]:
    value = _read_first(recorder, _SIGNALS[key])
    return None if value is None else int(value)


def _read_upstream_signal(recorder, key: str) -> Optional[int]:
    value = _read_first(recorder, _UPSTREAM_SIGNALS[key])
    return None if value is None else int(value)


def _read_late_fault_signal(recorder, key: str) -> Optional[int]:
    value = _read_first(recorder, _LATE_FAULT_SIGNALS[key])
    return None if value is None else int(value)


def _read_exception_mask(recorder) -> Optional[int]:
    mask = 0
    for slot in range(35):
        value = _read_first(
            recorder,
            (
                f"{_IFU_PREFIX}__Vtogcov__io_toIBuffer_bits_exceptionMask_{slot}",
            ),
        )
        if value is None:
            return None
        mask |= (int(value) & 1) << slot
    return mask


def _mark_late_fault_owner(recorder, cycle: int, evidence: dict) -> None:
    mark_owner_v3_checked(
        recorder,
        "BIN-906",
        cycle,
        evidence,
        producer="ifu_cacheable_late_fault_sampler",
    )


def _sample_late_fault_attribution(recorder, cycle: int) -> None:
    flush_pending = recorder._ifu_late_fault_flush_pending
    if flush_pending is not None and int(cycle) > int(flush_pending["cycle"]):
        s2_valid = _read_late_fault_signal(recorder, "s2_valid")
        s2_flag = _read_late_fault_signal(recorder, "s2_ftq_flag")
        s2_value = _read_late_fault_signal(recorder, "s2_ftq_value")
        same_flushed_window = (
            s2_valid == 1
            and None not in {s2_flag, s2_value}
            and (int(s2_flag), int(s2_value)) == flush_pending["ftq_identity"]
        )
        if not same_flushed_window:
            _mark_late_fault_owner(
                recorder,
                cycle,
                {
                    **flush_pending,
                    "event": "ifu_line0_late_fault_flush_suppressed",
                    "s2_same_window_valid": False,
                },
            )
        else:
            _record_mismatch(
                recorder,
                "ifu_line0_late_fault_survived_flush",
                cycle,
                pending=flush_pending,
            )
        recorder._ifu_late_fault_flush_pending = None

    delivery_pending = recorder._ifu_late_fault_delivery_pending
    if delivery_pending is not None and int(cycle) > int(delivery_pending["cycle"]):
        values = {
            key: _read_late_fault_signal(recorder, key)
            for key in (
                "s2_valid",
                "s2_ftq_flag",
                "s2_ftq_value",
                "s2_exception",
                "s2_instr_count",
                "to_ibuffer_valid",
                "to_ibuffer_ready",
                "to_ibuffer_exception",
                "to_ibuffer_enq",
            )
        }
        values["to_ibuffer_exception_mask"] = _read_exception_mask(recorder)
        if all(value is not None for value in values.values()):
            same_window = (
                values["s2_valid"] == 1
                and (values["s2_ftq_flag"], values["s2_ftq_value"])
                == delivery_pending["ftq_identity"]
            )
            single_exception_slot = (
                values["s2_instr_count"] == 1
                and int(values["to_ibuffer_enq"]).bit_count() == 1
                and int(values["to_ibuffer_exception_mask"]).bit_count() == 1
            )
            if (
                same_window
                and values["s2_exception"] in {3, 5}
                and values["to_ibuffer_valid"] == 1
                and values["to_ibuffer_ready"] == 1
                and values["to_ibuffer_exception"] == values["s2_exception"]
                and single_exception_slot
            ):
                _mark_late_fault_owner(
                    recorder,
                    cycle,
                    {
                        **delivery_pending,
                        "event": "ifu_line0_late_fault_stall_preserved",
                        "s2_observation": values,
                    },
                )
                recorder._ifu_late_fault_delivery_pending = None
            elif int(cycle) - int(delivery_pending["cycle"]) > 2:
                _record_mismatch(
                    recorder,
                    "ifu_line0_late_fault_delivery_missing",
                    cycle,
                    pending=delivery_pending,
                    observed=values,
                )
                recorder._ifu_late_fault_delivery_pending = None

    s1_values = {
        "valid": _read_signal(recorder, "s1_valid"),
        "ready": _read_signal(recorder, "s1_ready"),
        "fire": _read_signal(recorder, "s1_fire"),
        "flush": _read_signal(recorder, "s1_flush"),
        **{
            key: _read_late_fault_signal(recorder, key)
            for key in (
                "line0_corrupt",
                "line1_corrupt",
                "s1_meta_in_exception",
                "s1_merged_exception",
                "s1_ftq_flag",
                "s1_ftq_value",
            )
        },
    }
    if any(value is None for value in s1_values.values()) or s1_values["valid"] != 1:
        return
    identity = (int(s1_values["s1_ftq_flag"]), int(s1_values["s1_ftq_value"]))
    if s1_values["line1_corrupt"] == 1:
        _record_mismatch(
            recorder,
            "ifu_second_cacheline_late_fault_unattributed",
            cycle,
            ftq_identity=list(identity),
            merged_exception=int(s1_values["s1_merged_exception"]),
            blocked_bin_id="BIN-909",
        )

    first_line_fault = (
        s1_values["line0_corrupt"] == 1
        and s1_values["s1_merged_exception"] == 5
    ) or s1_values["s1_meta_in_exception"] in {3, 5}
    if not first_line_fault:
        return
    evidence = {
        "cycle": int(cycle),
        "ftq_identity": identity,
        "line0_corrupt": int(s1_values["line0_corrupt"]),
        "s1_meta_in_exception": int(s1_values["s1_meta_in_exception"]),
        "s1_merged_exception": int(s1_values["s1_merged_exception"]),
    }
    if s1_values["flush"] == 1 and s1_values["fire"] == 0:
        recorder._ifu_late_fault_flush_pending = evidence
        return
    if s1_values["ready"] == 0 and s1_values["fire"] == 0:
        recorder._ifu_late_fault_stall_pending = evidence
        return
    stall_pending = recorder._ifu_late_fault_stall_pending
    if (
        s1_values["fire"] == 1
        and stall_pending is not None
        and identity == stall_pending["ftq_identity"]
        and s1_values["s1_merged_exception"] in {3, 5}
    ):
        recorder._ifu_late_fault_delivery_pending = {
            **stall_pending,
            "cycle": int(cycle),
            "release_exception": int(s1_values["s1_merged_exception"]),
        }
        recorder._ifu_late_fault_stall_pending = None


def _sample_upstream_window_invariants(recorder, cycle: int) -> None:
    pending = recorder._ifu_upstream_suppression_pending
    req_valid = _read_signal(recorder, "req_valid")
    req1_valid = None
    output_ftq_flag = None
    output_ftq_value = None
    if req_valid == 1:
        req1_valid = _read_req_field(recorder, 1, "valid")
        output_ftq_flag = _read_req_field(recorder, 0, "ftqIdx_flag")
        output_ftq_value = _read_req_field(recorder, 0, "ftqIdx_value")
    if None not in {req1_valid, output_ftq_flag, output_ftq_value}:
        output_identity = (int(output_ftq_flag), int(output_ftq_value))
        matches = [item for item in pending if item["ftq_identity"] == output_identity]
        for match in matches:
            evidence = {
                **match["input"],
                "event": match["event"],
                "input_cycle": int(match["cycle"]),
                "output_ftq_identity": list(output_identity),
                "output_second_valid": int(req1_valid),
                "illegal_mixed_response_emitted": int(req1_valid) == 1,
            }
            if int(req1_valid) == 0:
                mark_owner_v3_checked(
                    recorder,
                    match["bin_id"],
                    cycle,
                    evidence,
                    producer="ifu_cacheable_upstream_invariant_sampler",
                )
            else:
                _record_mismatch(
                    recorder,
                    "ifu_cacheable_upstream_suppression_failed",
                    cycle,
                    evidence=evidence,
                )
            pending.remove(match)
    while pending and int(cycle) - int(pending[0]["cycle"]) > 4:
        expired = pending.popleft()
        _record_mismatch(
            recorder,
            "ifu_cacheable_upstream_suppression_unobserved",
            cycle,
            pending=expired,
        )

    values = {key: _read_upstream_signal(recorder, key) for key in _UPSTREAM_SIGNALS}
    if any(
        values[key] is None
        for key in (
            "mainpipe_fire",
            "second_requested",
            "second_waylookup_valid",
            "first_ftq_flag",
            "first_ftq_value",
            "real_two_fetch",
        )
    ):
        return
    if (
        values["mainpipe_fire"] != 1
        or values["second_requested"] != 1
        or values["second_waylookup_valid"] != 1
    ):
        return
    if (
        values["real_two_fetch"] == 0
        and (values["first_mmio"] == 1 or values["second_mmio"] == 1)
    ):
        pending.append(
            {
                "bin_id": "BIN-904",
                "event": "icache_mainpipe_mixed_window_suppressed",
                "cycle": int(cycle),
                "ftq_identity": (
                    int(values["first_ftq_flag"]),
                    int(values["first_ftq_value"]),
                ),
                "input": values,
            }
        )
    if (
        values["real_two_fetch"] == 0
        and values["first_itlb_exception"] == 0
        and values["second_itlb_exception"] not in {None, 0}
    ):
        pending.append(
            {
                "bin_id": "BIN-908",
                "event": "icache_mainpipe_second_itlb_suppresses_dual_fetch",
                "cycle": int(cycle),
                "ftq_identity": (
                    int(values["first_ftq_flag"]),
                    int(values["first_ftq_value"]),
                ),
                "input": {
                    **values,
                    "second_block_pmp_independently_checked": False,
                },
            }
        )


def _req_signal_names(index: int, field: str) -> tuple[str, ...]:
    return (
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_info_{index}_{field}",
        f"{_ICACHE_PREFIX}mainPipe.io_toIfu_req_bits_info_{index}_{field}",
    )


def _read_req_field(recorder, index: int, field: str) -> Optional[int]:
    value = _read_first(recorder, _req_signal_names(index, field))
    return None if value is None else int(value)


def _read_req_top_field(recorder, field: str) -> Optional[int]:
    value = _read_first(
        recorder,
        (
            f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_{field}",
            f"{_MAINPIPE_PREFIX}io_toIfu_req_bits_{field}",
        ),
    )
    return None if value is None else int(value)


def _read_req_data(recorder, index: int) -> Optional[int]:
    chunks = []
    for local_bank in range(8):
        bank = int(index) * 8 + local_bank
        data_index = 3 * bank + int(bank >= 8)
        data_suffix = "" if bank == 0 else f"_{data_index}"
        valid_suffix = f"_{data_index + 1}"
        reg_suffix = "" if bank == 0 else f"_{bank}"
        live = _read_first(
            recorder,
            (f"{_MAINPIPE_PREFIX}_s1_data_T{valid_suffix}",),
        )
        if live is None:
            return None
        stem = f"_s1_data_T{data_suffix}" if int(live) else f"s1_data_r{reg_suffix}"
        chunk = _read_first(recorder, (f"{_MAINPIPE_PREFIX}{stem}",))
        if chunk is None:
            return None
        chunks.append(int(chunk))
    return sum(chunk << (64 * bank) for bank, chunk in enumerate(chunks))


def _read_s1_field(recorder, index: int, field: str) -> Optional[int]:
    value = _read_first(
        recorder,
        (
            f"{_IFU_PREFIX}s1_fetchBlock_{index}_{field}",
            f"{_IFU_PREFIX}__Vtogcov__s1_fetchBlock_{index}_{field}",
        ),
    )
    return None if value is None else int(value)


def _request_snapshot(recorder) -> Optional[dict]:
    req1_valid = _read_req_field(recorder, 1, "valid")
    if req1_valid is None:
        return None

    blocks = []
    for index, valid in ((0, 1), (1, int(req1_valid))):
        block = {"valid": int(valid)}
        if valid:
            for field in (
                "ftqIdx_flag",
                "ftqIdx_value",
                "startVAddr_addr",
                "takenCfiOffset_valid",
                "takenCfiOffset_bits",
                "size",
            ):
                value = _read_req_field(recorder, index, field)
                if value is None:
                    return None
                block[field] = int(value)
        blocks.append(block)

    first_range = _read_req_top_field(recorder, "firstRange")
    total_range = _read_req_top_field(recorder, "totalRange")
    maybe_rvc_map = _read_req_top_field(recorder, "maybeRvcMap")
    if None in {first_range, total_range, maybe_rvc_map}:
        return None
    payload = {
        "blocks": blocks,
        "firstRange": int(first_range),
        "totalRange": int(total_range),
        "maybeRvcMap": int(maybe_rvc_map),
    }
    for index in range(2):
        if not blocks[index]["valid"]:
            continue
        data = _read_req_data(recorder, index)
        if data is None:
            return None
        payload[f"req{index}_data"] = int(data)
    return payload


def _s1_snapshot(recorder) -> Optional[dict]:
    blocks = []
    for index in range(2):
        valid = _read_s1_field(recorder, index, "valid")
        if valid is None:
            return None
        block = {"valid": int(valid)}
        if valid:
            for field in (
                "ftqIdx_flag",
                "ftqIdx_value",
                "startVAddr_addr",
                "takenCfiOffset_valid",
                "takenCfiOffset_bits",
                "size",
            ):
                value = _read_s1_field(recorder, index, field)
                if value is None:
                    return None
                block[field] = int(value)
        blocks.append(block)
    first_range = _read_first(
        recorder,
        (f"{_IFU_PREFIX}s1_firstRange", f"{_IFU_PREFIX}__Vtogcov__s1_firstRange"),
    )
    total_range = _read_first(
        recorder,
        (f"{_IFU_PREFIX}s1_totalRange", f"{_IFU_PREFIX}__Vtogcov__s1_totalRange"),
    )
    if None in {first_range, total_range}:
        return None
    payload = {
        "blocks": blocks,
        "firstRange": int(first_range),
        "totalRange": int(total_range),
    }
    for index, stem in enumerate(("s1_firstICacheData", "s1_secondICacheData")):
        if not blocks[index]["valid"]:
            continue
        data = _read_first(recorder, (f"{_IFU_PREFIX}{stem}",))
        if data is None:
            return None
        payload[f"req{index}_data"] = int(data)
    return payload


def _metadata_snapshot(snapshot: dict) -> dict:
    fields = (
        "valid",
        "ftqIdx_flag",
        "ftqIdx_value",
        "startVAddr_addr",
        "takenCfiOffset_valid",
        "takenCfiOffset_bits",
        "size",
    )
    result = {
        "blocks": [
            {field: block[field] for field in fields if field in block}
            for block in snapshot["blocks"]
        ],
        "firstRange": snapshot["firstRange"],
        "totalRange": snapshot["totalRange"],
    }
    for index, block in enumerate(snapshot["blocks"]):
        if block.get("valid") == 1:
            result[f"req{index}_data"] = snapshot[f"req{index}_data"]
    return result


def _record_mismatch(recorder, event: str, cycle: int, **details) -> None:
    recorder.risk_observations.append({"event": event, "cycle": int(cycle), **details})


def _sample_verified_transfer(recorder, cycle: int, s1_snapshot: Optional[dict]) -> None:
    pending = recorder._ifu_cacheable_pending_transfer
    if pending is not None and _read_signal(recorder, "s1_flush") == 1:
        _record_mismatch(
            recorder,
            "ifu_cacheable_pending_transfer_flushed",
            cycle,
            accepted_cycle=pending["cycle"],
        )
        recorder._ifu_cacheable_pending_transfer = None
        return
    if pending is None or s1_snapshot is None or _read_signal(recorder, "s1_valid") != 1:
        return

    expected = _metadata_snapshot(pending["snapshot"])
    observed = _metadata_snapshot(s1_snapshot)
    if observed != expected:
        _record_mismatch(
            recorder,
            "ifu_cacheable_s1_metadata_mismatch",
            cycle,
            accepted_cycle=pending["cycle"],
            expected=expected,
            observed=observed,
        )
        recorder._ifu_cacheable_pending_transfer = None
        return

    evidence = {
        "event": "icache_to_ifu_s1_transfer",
        "accepted_cycle": pending["cycle"],
        "source": expected,
        "s1": observed,
    }
    if int(cycle) == int(pending["cycle"]) + 1:
        recorder.mark(
            "ifu_cacheable_s1",
            "s0_accept_to_s1_valid",
            cycle,
            {**evidence, "pipeline_latency": 1},
        )
    recorder.mark("ifu_cacheable_s1", "source_ftq_address_matched", cycle, evidence)
    expected_blocks = expected["blocks"]
    second_valid = expected_blocks[1]["valid"] == 1
    recorder.mark(
        "ifu_cacheable_window",
        "dual_block" if second_valid else "single_block",
        cycle,
        evidence,
    )
    recorder.mark("ifu_cacheable_metadata", "first_ftq_preserved", cycle, evidence)
    if second_valid:
        recorder.mark("ifu_cacheable_metadata", "second_ftq_preserved", cycle, evidence)
    if expected_blocks[0]["takenCfiOffset_valid"] == 0:
        recorder.mark("ifu_cacheable_metadata", "not_taken_preserved", cycle, evidence)
    if (
        _read_signal(recorder, "s1_req_uncache") == 0
        and _read_signal(recorder, "s1_exception") == 0
    ):
        recorder.mark(
            "ifu_cacheable_s1",
            "dual_cacheable_path" if second_valid else "single_cacheable_path",
            cycle,
            evidence,
        )

    valid_blocks = [block for block in expected_blocks if block.get("valid") == 1]
    for block in valid_blocks:
        halfword_index = int(block["startVAddr_addr"]) & 0x1F
        if halfword_index <= 10:
            region = "head"
        elif halfword_index <= 20:
            region = "mid"
        else:
            region = "tail"
        recorder._ifu_cacheable_start_regions.add(region)
        recorder._ifu_cacheable_alignments.add(
            "2b_only" if int(block["startVAddr_addr"]) & 1 else "4b"
        )
        recorder._ifu_cacheable_fetch_sizes.add(int(block["size"]))

        block_evidence = {
            **evidence,
            "block": block,
            "halfword_index": halfword_index,
            "seen_regions": sorted(recorder._ifu_cacheable_start_regions),
            "seen_alignments": sorted(recorder._ifu_cacheable_alignments),
            "seen_fetch_sizes": sorted(recorder._ifu_cacheable_fetch_sizes),
        }
        if recorder._ifu_cacheable_start_regions >= {"head", "mid", "tail"}:
            recorder.mark(
                "ifu_cacheable_address", "head_mid_tail_seen", cycle, block_evidence
            )
        if recorder._ifu_cacheable_alignments >= {"2b_only", "4b"}:
            recorder.mark(
                "ifu_cacheable_address", "align_2b_4b_seen", cycle, block_evidence
            )
        if (
            int(block["takenCfiOffset_valid"]) == 0
            and int(block["size"]) == 32
        ):
            recorder.mark(
                "ifu_cacheable_range", "sequential_full_fetch", cycle, block_evidence
            )
        if int(block["takenCfiOffset_valid"]) == 1:
            recorder.mark(
                "ifu_cacheable_metadata", "taken_offset_preserved", cycle, block_evidence
            )
        if len(recorder._ifu_cacheable_fetch_sizes) >= 2:
            recorder.mark(
                "ifu_cacheable_range", "fetch_size_variation", cycle, block_evidence
            )

        current_ftq_ptr = (int(block["ftqIdx_flag"]), int(block["ftqIdx_value"]))
        previous_ftq_ptr = recorder._ifu_cacheable_last_ftq_ptr
        if previous_ftq_ptr is not None:
            continuous = (
                current_ftq_ptr[0] == previous_ftq_ptr[0]
                and current_ftq_ptr[1] == previous_ftq_ptr[1] + 1
            )
            wrapped = (
                current_ftq_ptr[0] != previous_ftq_ptr[0]
                and current_ftq_ptr[1] < previous_ftq_ptr[1]
            )
            if continuous or wrapped:
                recorder.mark(
                    "ifu_cacheable_metadata",
                    "ftq_pointer_progression",
                    cycle,
                    {
                        **block_evidence,
                        "previous_ftq_ptr": list(previous_ftq_ptr),
                        "current_ftq_ptr": list(current_ftq_ptr),
                        "transition": "wrap" if wrapped else "continuous",
                    },
                )
        recorder._ifu_cacheable_last_ftq_ptr = current_ftq_ptr

    if len(valid_blocks) == 2:
        first_line = (int(valid_blocks[0]["startVAddr_addr"]) << 1) >> 6
        second_line = (int(valid_blocks[1]["startVAddr_addr"]) << 1) >> 6
        if first_line != second_line:
            recorder.mark(
                "ifu_cacheable_window",
                "cross_cacheline_dual_block",
                cycle,
                {
                    **evidence,
                    "first_cacheline": first_line,
                    "second_cacheline": second_line,
                },
            )

    previous = recorder._ifu_cacheable_last_verified
    if pending["gapped"] and previous is not None and previous["source"] != expected:
        recorder.mark(
            "ifu_cacheable_transfer",
            "gapped_metadata_isolated",
            cycle,
            {**evidence, "previous": previous},
        )
    recorder._ifu_cacheable_last_verified = {"cycle": int(cycle), "source": expected}
    for block in expected_blocks:
        if block.get("valid") != 1:
            continue
        recorder._ifu_cacheable_verified_windows.append(
            {
                "cycle": int(cycle),
                "ftq_ptr": (int(block["ftqIdx_flag"]), int(block["ftqIdx_value"])),
                # PrunedAddr omits bit zero; size is expressed in halfwords.
                "pc_start": int(block["startVAddr_addr"]) << 1,
                "pc_limit": (int(block["startVAddr_addr"]) << 1) + 2 * int(block["size"]),
                "source": block,
            }
        )
    recorder._ifu_cacheable_pending_transfer = None


def sample_ifu_cacheable_pipeline_coverage(recorder, env, cycle: int) -> None:
    if _dut(recorder) is None:
        return

    cycle = int(cycle)
    _sample_upstream_window_invariants(recorder, cycle)
    _sample_late_fault_attribution(recorder, cycle)
    req_valid = _read_signal(recorder, "req_valid")
    req_ready = _read_signal(recorder, "req_ready")
    s0_fire = _read_signal(recorder, "s0_fire")
    s0_flush = _read_signal(recorder, "s0_flush")
    source = _request_snapshot(recorder) if req_valid == 1 else None
    s1_snapshot = _s1_snapshot(recorder) if _read_signal(recorder, "s1_valid") == 1 else None

    _sample_verified_transfer(recorder, cycle, s1_snapshot)

    if req_valid == 1 and req_ready == 0:
        recorder.mark(
            "ifu_cacheable_ingress",
            "backpressured",
            cycle,
            {"event": "icache_to_ifu_backpressure", "source": source},
        )
        previous = recorder._ifu_cacheable_ingress_stall
        if source is not None and previous is not None:
            if source == previous["snapshot"]:
                recorder.mark(
                    "ifu_cacheable_ingress",
                    "backpressure_payload_stable",
                    cycle,
                    {
                        "event": "icache_to_ifu_stable_backpressure",
                        "since_cycle": previous["cycle"],
                        "source": source,
                    },
                )
            else:
                _record_mismatch(
                    recorder,
                    "ifu_cacheable_backpressure_payload_changed",
                    cycle,
                    previous=previous["snapshot"],
                    current=source,
                )
        if source is not None:
            recorder._ifu_cacheable_ingress_stall = {"cycle": cycle, "snapshot": source}
    else:
        recorder._ifu_cacheable_ingress_stall = None

    s1_ready = _read_signal(recorder, "s1_ready")
    s1_valid = _read_signal(recorder, "s1_valid")
    s1_fire = _read_signal(recorder, "s1_fire")
    s1_flush = _read_signal(recorder, "s1_flush")
    if s1_valid == 1 and s1_fire == 1 and s1_flush != 1:
        s1_evidence = {
            "event": "ifu_s1_fire",
            "s1": s1_snapshot,
            "s1_req_is_uncache": _read_signal(recorder, "s1_req_uncache"),
            "s1_exception": _read_signal(recorder, "s1_exception"),
        }
        recorder.mark("ifu_cacheable_s1", "fire_to_s2", cycle, s1_evidence)
        if s1_evidence["s1_req_is_uncache"] == 0 and s1_evidence["s1_exception"] == 0:
            recorder.mark("ifu_cacheable_s1", "cacheable_no_uncache", cycle, s1_evidence)
    if req_valid == 1 and req_ready == 0 and s1_valid == 1 and s1_ready == 0 and s1_flush != 1:
        recorder.mark(
            "ifu_cacheable_s1",
            "response_backpressured_by_s2",
            cycle,
            {"event": "icache_response_held_by_s1_backpressure", "source": source, "s1": s1_snapshot},
        )
    if s1_snapshot is not None and s1_ready == 0 and _read_signal(recorder, "s1_flush") != 1:
        previous = recorder._ifu_cacheable_s1_stall
        if previous is not None:
            if s1_snapshot == previous["snapshot"]:
                recorder.mark(
                    "ifu_cacheable_transfer",
                    "s1_payload_stable",
                    cycle,
                    {
                        "event": "ifu_s1_stable_backpressure",
                        "since_cycle": previous["cycle"],
                        "s1": s1_snapshot,
                    },
                )
            else:
                _record_mismatch(
                    recorder,
                    "ifu_cacheable_s1_payload_changed",
                    cycle,
                    previous=previous["snapshot"],
                    current=s1_snapshot,
                )
        recorder._ifu_cacheable_s1_stall = {"cycle": cycle, "snapshot": s1_snapshot}
    else:
        recorder._ifu_cacheable_s1_stall = None

    backend_redirect = _read_signal(recorder, "backend_redirect")
    wb_redirect = _read_signal(recorder, "wb_redirect")
    bpu_s3_flush = _read_signal(recorder, "bpu_s3_flush")
    s0_flush_bpu = _read_signal(recorder, "s0_flush_bpu")
    flush_blocks = req_valid == 1 and s0_flush == 1 and s0_fire == 0
    flush_evidence = {
        "event": "ifu_s0_flush_blocks_icache_return",
        "req_ready": req_ready,
        "backend_redirect": backend_redirect,
        "wb_redirect": wb_redirect,
        "bpu_s3_flush": bpu_s3_flush,
        "s0_flush_bpu": s0_flush_bpu,
        "source": source,
    }
    if flush_blocks:
        recorder.mark("ifu_cacheable_flush", "flush_wins_fire", cycle, flush_evidence)
        if backend_redirect == 1:
            recorder.mark("ifu_cacheable_flush", "backend_redirect_blocks", cycle, flush_evidence)
        if wb_redirect == 1:
            recorder.mark("ifu_cacheable_flush", "wb_redirect_blocks", cycle, flush_evidence)
        if bpu_s3_flush == 1 and s0_flush_bpu == 1:
            recorder.mark("ifu_cacheable_flush", "bpu_match_blocks", cycle, flush_evidence)
            if source is not None and source["blocks"][1].get("valid") == 1:
                mark_owner_v3_checked(
                    recorder,
                    "BIN-901",
                    cycle,
                    {
                        **flush_evidence,
                        "window_identity": "block0_ftq_idx",
                        "window_blocks": 2,
                        "whole_window_fired": False,
                    },
                    producer="ifu_cacheable_bpu_window_sampler",
                )

    accepted = req_valid == 1 and req_ready == 1 and s0_fire == 1 and s0_flush == 0
    if accepted and source is not None:
        last_accept = recorder._ifu_cacheable_last_accept_cycle
        evidence = {"event": "icache_to_ifu_accept", "source": source}
        recorder.mark("ifu_cacheable_ingress", "accepted", cycle, evidence)
        if last_accept is not None and cycle == int(last_accept) + 1:
            recorder.mark("ifu_cacheable_ingress", "back_to_back_accept", cycle, evidence)
        if bpu_s3_flush == 1 and s0_flush_bpu == 0:
            recorder.mark("ifu_cacheable_flush", "bpu_miss_allows", cycle, evidence)
        recorder._ifu_cacheable_pending_transfer = {
            "cycle": cycle,
            "snapshot": source,
            "gapped": last_accept is not None and cycle > int(last_accept) + 1,
        }
        recorder._ifu_cacheable_last_accept_cycle = cycle


__all__ = [
    "IFU_CACHEABLE_PIPELINE_COVERPOINTS",
    "IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS",
    "initialize_ifu_cacheable_pipeline_state",
    "reset_ifu_cacheable_pipeline_state",
    "sample_ifu_cacheable_pipeline_coverage",
]

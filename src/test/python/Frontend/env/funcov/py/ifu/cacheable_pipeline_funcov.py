from __future__ import annotations

from collections import deque
from typing import Optional

from ..common.dut import _dut, _read_first
from .owner_v3_funcov import mark_owner_v3_checked


_ICACHE_PREFIX = "Frontend_top.Frontend.inner_icache."
_IFU_PREFIX = "Frontend_top.Frontend.inner_ifu."
_MAINPIPE_PREFIX = f"{_ICACHE_PREFIX}mainPipe."
_FETCH_BLOCK_INST_NUM = 32
_ICACHE_HALFWORDS = 32
_PRUNED_GUARDED_ADDR_BITS = 50
_REGISTERED_TRANSACTION_SLOT_COUNT = 35
_CONTRACT_FAILURE_EVENTS = frozenset(
    {
        "ifu_s1_alignment_probe_unobservable",
        "ifu_s1_alignment_semantic_mismatch",
        "ifu_s1_s2_transaction_control_unobservable",
        "ifu_s1_s2_transaction_probe_unobservable",
        "ifu_s1_s2_registered_transaction_mismatch",
        "ifu_s2_registered_semantic_mismatch",
        "ifu_s1_s2_transaction_pending_collision",
        "ifu_s1_s2_transaction_timeout",
        "ifu_cacheable_backend_flush_source_unobservable",
        "ifu_cacheable_backend_flush_internal_unobservable",
        "ifu_cacheable_backend_flush_source_changed",
        "ifu_cacheable_backend_flush_lost_to_fire",
        "ifu_cacheable_backend_flush_timeout",
        "ifu_line0_late_fault_source_pending_collision",
        "ifu_line0_late_fault_source_unobservable",
    }
)


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
    "ifu_backend_redirect": (
        f"{_IFU_PREFIX}__Vtogcov__io_fromFtq_redirect_valid",
    ),
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
    "line0_tl_corrupt": (
        f"{_MAINPIPE_PREFIX}s1_tlCorrupt_r",
        f"{_MAINPIPE_PREFIX}__Vtogcov__s1_tlCorrupt_r",
    ),
    "line0_tl_denied": (
        f"{_MAINPIPE_PREFIX}s1_tlDenied_r",
        f"{_MAINPIPE_PREFIX}__Vtogcov__s1_tlDenied_r",
    ),
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
    recorder._ifu_cacheable_s1_s2_pending = None
    recorder._ifu_cacheable_verified_windows = deque(maxlen=64)
    recorder._ifu_cacheable_start_regions = set()
    recorder._ifu_cacheable_alignments = set()
    recorder._ifu_cacheable_fetch_sizes = set()
    recorder._ifu_cacheable_last_ftq_ptr = None
    recorder._ifu_cacheable_backend_flush_pending = None
    recorder._ifu_upstream_suppression_pending = deque(maxlen=8)
    recorder._ifu_late_fault_stall_pending = None
    recorder._ifu_late_fault_delivery_pending = None
    recorder._ifu_late_fault_flush_pending = None
    recorder._ifu_late_fault_source_pending = None
    recorder._ifu_invalid_taken_exception_pending = None


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


def _read_late_fault_signal_with_path(
    recorder, key: str
) -> tuple[Optional[int], Optional[str]]:
    dut = _dut(recorder)
    if dut is None:
        return None, None
    for name in _LATE_FAULT_SIGNALS[key]:
        value = recorder._try_read_dut_signal(dut, name)
        if value is not None:
            return int(value), str(name)
    return None, None


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


def _capture_line0_tl_fault_source(recorder, cycle: int) -> None:
    handshake = {
        "req_valid": _read_signal(recorder, "req_valid"),
        "req_ready": _read_signal(recorder, "req_ready"),
        "s0_fire": _read_signal(recorder, "s0_fire"),
    }
    if any(value is None for value in handshake.values()) or any(
        value != 1 for value in handshake.values()
    ):
        return

    identity = (
        _read_req_field(recorder, 0, "ftqIdx_flag"),
        _read_req_field(recorder, 0, "ftqIdx_value"),
    )
    payload_exception = _read_req_field(
        recorder, 0, "icacheMeta_exception_value"
    )
    tl_corrupt, tl_corrupt_path = _read_late_fault_signal_with_path(
        recorder, "line0_tl_corrupt"
    )
    tl_denied, tl_denied_path = _read_late_fault_signal_with_path(
        recorder, "line0_tl_denied"
    )
    if None in {*identity, payload_exception}:
        return
    if tl_corrupt is None or tl_denied is None:
        if int(payload_exception) in {3, 5}:
            _record_mismatch(
                recorder,
                "ifu_line0_late_fault_source_unobservable",
                cycle,
                ftq_identity=list(identity),
                payload_exception=int(payload_exception),
                missing_source_probes=[
                    key
                    for key, value in (
                        ("line0_tl_corrupt", tl_corrupt),
                        ("line0_tl_denied", tl_denied),
                    )
                    if value is None
                ],
            )
        return

    source_exception = 3 if int(tl_denied) else 5 if int(tl_corrupt) else 0
    if source_exception == 0:
        return
    if int(payload_exception) != source_exception:
        _record_mismatch(
            recorder,
            "ifu_line0_late_fault_masked_by_prior_exception",
            cycle,
            ftq_identity=list(identity),
            payload_exception=int(payload_exception),
            source_exception=int(source_exception),
            line0_tl_corrupt=int(tl_corrupt),
            line0_tl_denied=int(tl_denied),
        )
        return

    if recorder._ifu_late_fault_source_pending is not None:
        _record_mismatch(
            recorder,
            "ifu_line0_late_fault_source_pending_collision",
            cycle,
            previous=recorder._ifu_late_fault_source_pending,
            ftq_identity=list(identity),
        )
        recorder._ifu_late_fault_source_pending = None
        return
    recorder._ifu_late_fault_source_pending = {
        "source_cycle": int(cycle),
        "ftq_identity": tuple(int(value) for value in identity),
        "fault_source": "tl_denied" if int(tl_denied) else "tl_corrupt",
        "source_exception": int(source_exception),
        "line0_tl_corrupt": int(tl_corrupt),
        "line0_tl_denied": int(tl_denied),
        "source_signal_paths": {
            "line0_tl_corrupt": tl_corrupt_path,
            "line0_tl_denied": tl_denied_path,
        },
    }


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
        "req_is_uncache": _read_signal(recorder, "s1_req_uncache"),
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
        _capture_line0_tl_fault_source(recorder, cycle)
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

    source_pending = recorder._ifu_late_fault_source_pending
    matched_tl_source = (
        source_pending is not None
        and identity == source_pending["ftq_identity"]
        and 0 < int(cycle) - int(source_pending["source_cycle"]) <= 2
        and s1_values["s1_meta_in_exception"] == source_pending["source_exception"]
        and s1_values["s1_merged_exception"] == source_pending["source_exception"]
    )
    ecc_fault = (
        s1_values["line0_corrupt"] == 1
        and s1_values["s1_merged_exception"] == 5
    )
    stall_pending = recorder._ifu_late_fault_stall_pending
    verified_stall_context = (
        stall_pending is not None
        and identity == stall_pending["ftq_identity"]
        and s1_values["s1_merged_exception"] == stall_pending["source_exception"]
    )
    if source_pending is not None and (
        matched_tl_source
        or int(cycle) - int(source_pending["source_cycle"]) > 2
    ):
        recorder._ifu_late_fault_source_pending = None
    if s1_values["req_is_uncache"] != 0 or not (
        ecc_fault or matched_tl_source or verified_stall_context
    ):
        _capture_line0_tl_fault_source(recorder, cycle)
        return
    source_evidence = (
        dict(source_pending)
        if matched_tl_source
        else dict(stall_pending)
        if verified_stall_context
        else {
            "source_cycle": int(cycle),
            "fault_source": "ecc",
            "source_exception": 5,
            "source_signal_paths": {
                "line0_corrupt": _LATE_FAULT_SIGNALS["line0_corrupt"][0]
            },
        }
    )
    evidence = {
        **source_evidence,
        "cycle": int(cycle),
        "ftq_identity": identity,
        "s1_req_is_uncache": int(s1_values["req_is_uncache"]),
        "line0_corrupt": int(s1_values["line0_corrupt"]),
        "s1_meta_in_exception": int(s1_values["s1_meta_in_exception"]),
        "s1_merged_exception": int(s1_values["s1_merged_exception"]),
    }
    if s1_values["flush"] == 1 and s1_values["fire"] == 0:
        recorder._ifu_late_fault_flush_pending = evidence
        recorder._ifu_late_fault_stall_pending = None
        _capture_line0_tl_fault_source(recorder, cycle)
        return
    if s1_values["ready"] == 0 and s1_values["fire"] == 0:
        recorder._ifu_late_fault_stall_pending = evidence
        _capture_line0_tl_fault_source(recorder, cycle)
        return
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
    _capture_line0_tl_fault_source(recorder, cycle)


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
    if event in _CONTRACT_FAILURE_EVENTS:
        recorder.record_contract_error(event, cycle, details)


def _bit(value: int, index: int) -> int:
    return (int(value) >> int(index)) & 1


def _pruned_signed_offset(value: int, width: int) -> int:
    value &= (1 << int(width)) - 1
    if value & (1 << (int(width) - 1)):
        value -= 1 << int(width)
    return (value >> 1) & ((1 << _PRUNED_GUARDED_ADDR_BITS) - 1)


def _decode_branch_type(instruction: int) -> int:
    instruction = int(instruction) & 0xFFFFFFFF
    low = instruction & 0xFFFF
    if (low & 0xEFFF) == 0x8002:  # RTL C.EBREAK BitPat precedes C.JR/C.JALR.
        return 0
    if (low & 0xE003) == 0xA001:
        return 2
    if (low & 0xE07F) == 0x8002:
        return 3
    if (low & 0xC003) == 0xC001:
        return 1
    if (instruction & 0x7F) == 0x6F:
        return 2
    if (instruction & 0x707F) == 0x67:
        return 3
    if (instruction & 0x7F) == 0x63:
        return 1
    return 0


def _decode_ras_action(instruction: int, branch_type: int) -> int:
    instruction = int(instruction) & 0xFFFFFFFF
    is_rvc = (instruction & 0x3) != 0x3
    rd = ((instruction >> 12) & 1) if is_rvc else ((instruction >> 7) & 0x1F)
    if is_rvc:
        rs = 0 if int(branch_type) == 2 else ((instruction >> 7) & 0x1F)
    else:
        rs = (instruction >> 15) & 0x1F
    rd_is_link = rd in {1, 5}
    rs_is_link = rs in {1, 5}
    has_push = (
        int(branch_type) == 2 and rd_is_link and not is_rvc
    ) or (int(branch_type) == 3 and rd_is_link)
    has_pop = int(branch_type) == 3 and rs_is_link and rd != rs
    return (int(has_push) << 1) | int(has_pop)


def _decode_jump_offset(instruction: int, is_rvc: int, branch_type: int) -> int:
    instruction = int(instruction) & 0xFFFFFFFF
    if int(branch_type) == 1:
        if int(is_rvc):
            immediate = (
                (((instruction >> 12) & 1) << 8)
                | (((instruction >> 5) & 0x3) << 6)
                | (((instruction >> 2) & 1) << 5)
                | (((instruction >> 10) & 0x3) << 3)
                | (((instruction >> 3) & 0x3) << 1)
            )
            return _pruned_signed_offset(immediate, 9)
        immediate = (
            (((instruction >> 31) & 1) << 12)
            | (((instruction >> 7) & 1) << 11)
            | (((instruction >> 25) & 0x3F) << 5)
            | (((instruction >> 8) & 0xF) << 1)
        )
        return _pruned_signed_offset(immediate, 13)
    if int(is_rvc):
        immediate = (
            (((instruction >> 12) & 1) << 11)
            | (((instruction >> 8) & 1) << 10)
            | (((instruction >> 9) & 0x3) << 8)
            | (((instruction >> 6) & 1) << 7)
            | (((instruction >> 7) & 1) << 6)
            | (((instruction >> 2) & 1) << 5)
            | (((instruction >> 11) & 1) << 4)
            | (((instruction >> 3) & 0x7) << 1)
        )
        return _pruned_signed_offset(immediate, 12)
    immediate = (
        (((instruction >> 31) & 1) << 20)
        | (((instruction >> 12) & 0xFF) << 12)
        | (((instruction >> 20) & 1) << 11)
        | (((instruction >> 21) & 0x3FF) << 1)
    )
    return _pruned_signed_offset(immediate, 21)


def _cut_cache_line(cache_line: int) -> list[int]:
    halfwords = [
        (int(cache_line) >> (16 * index)) & 0xFFFF
        for index in range(_ICACHE_HALFWORDS)
    ]
    return [
        halfwords[index] | (halfwords[(index + 1) % _ICACHE_HALFWORDS] << 16)
        for index in range(_ICACHE_HALFWORDS)
    ]


def _expected_s1_semantic_slots(
    aggregate: dict,
    *,
    align_shift: int,
    prev_half_info: dict,
) -> dict[int, dict]:
    blocks = aggregate["blocks"]
    first = blocks[0]
    second = blocks[1]
    first_range = int(aggregate["firstRange"])
    total_range = int(aggregate["totalRange"])
    maybe_rvc_map = int(aggregate["maybeRvcMap"])
    previous_half = int(prev_half_info["valid"])
    first_data = _cut_cache_line(aggregate["req0_data"])
    second_data = (
        _cut_cache_line(aggregate["req1_data"])
        if int(second.get("valid", 0)) == 1
        else None
    )

    boundaries = []
    raw = []
    active_positions = min(
        _FETCH_BLOCK_INST_NUM,
        max(1, int(total_range).bit_length()),
    )
    for position in range(active_positions):
        boundary = (
            not bool(previous_half)
            if position == 0
            else (not boundaries[position - 1])
            or bool(_bit(maybe_rvc_map, position - 1))
        )
        boundaries.append(bool(boundary))
        valid = (
            bool(previous_half) or bool(boundary)
            if position == 0
            else bool(boundary)
        )
        if not valid:
            continue

        from_first = position < int(first["size"])
        if from_first:
            index = (int(first["startVAddr_addr"]) + position) & 0x1F
        else:
            index = (
                int(second["startVAddr_addr"])
                + position
                - int(first["size"])
            ) & 0x1F
        raw_block_sel = int(
            int(second.get("valid", 0)) == 1 and not _bit(first_range, position)
        )
        next_block_sel = int(
            position + 1 < active_positions
            and int(second.get("valid", 0)) == 1
            and not _bit(first_range, position + 1)
        )
        is_cross_block_instr = int(
            position + 1 < active_positions
            and int(second.get("valid", 0)) == 1
            and int(first["takenCfiOffset_valid"]) == 0
            and position == int(first["takenCfiOffset_bits"])
            and raw_block_sel == 0
            and next_block_sel == 1
            and _bit(maybe_rvc_map, position) == 0
        )
        is_rvc = int(bool(boundary) and bool(_bit(maybe_rvc_map, position)))
        start_offset = (
            position - int(first["size"]) if raw_block_sel else position
        )

        if raw_block_sel:
            data = int(second_data[index])
        elif is_cross_block_instr:
            second_start_index = int(second["startVAddr_addr"]) & 0x1F
            data = (int(second_data[second_start_index]) & 0xFFFF) << 16
            data |= int(first_data[index]) & 0xFFFF
        else:
            data = int(first_data[index])

        pc = (
            int(second["startVAddr_addr"]) - 1
            if is_cross_block_instr
            else int(blocks[raw_block_sel]["startVAddr_addr"]) + start_offset
        ) & ((1 << _PRUNED_GUARDED_ADDR_BITS) - 1)
        end_offset = start_offset if is_rvc else start_offset + 1
        if is_cross_block_instr:
            end_offset = 0
        raw.append(
            {
                "raw_position": int(position),
                "index": int(index),
                "data": int(data) & 0xFFFFFFFF,
                "is_rvc": int(is_rvc),
                "raw_block_sel": int(raw_block_sel),
                "is_cross_block_instr": int(is_cross_block_instr),
                "effective_owner": int(raw_block_sel) | int(is_cross_block_instr),
                "start_offset": int(start_offset),
                "end_offset": int(end_offset),
                "pc": int(pc),
                "is_prev_end_half_rvi": False,
            }
        )

    expected = {}
    for compacted_index, item in enumerate(raw):
        slot = int(compacted_index) + int(align_shift)
        if slot >= _REGISTERED_TRANSACTION_SLOT_COUNT:
            continue
        item = dict(item)
        item["slot"] = int(slot)
        if bool(prev_half_info["valid"]) and slot == int(align_shift):
            item["data"] = (
                ((int(item["data"]) & 0xFFFF) << 16)
                | (int(prev_half_info["data"]) & 0xFFFF)
            )
            item["pc"] = int(prev_half_info["pc"])
            item["end_offset"] = 0
            item["is_prev_end_half_rvi"] = True
        item["branch_type"] = _decode_branch_type(item["data"])
        item["predecode_is_rvc"] = int(item["is_rvc"])
        item["ras_action"] = _decode_ras_action(
            item["data"], item["branch_type"]
        )
        item["jump_offset"] = _decode_jump_offset(
            item["data"], item["is_rvc"], item["branch_type"]
        )
        expected[slot] = item
    return expected


def _read_ifu_pipeline_internal(recorder, stem: str) -> Optional[int]:
    candidates = [
        f"{_IFU_PREFIX}{stem}",
        f"{_IFU_PREFIX}__Vtogcov__{stem}",
    ]
    if stem == "s1_alignedInstrValid":
        candidates.append(f"{_IFU_PREFIX}_s1_alignedInstrValid_T")
    if stem.startswith("s1_alignedInstrVec_") and stem.endswith("_data"):
        slot = stem.removeprefix("s1_alignedInstrVec_").removesuffix("_data")
        if slot.isdigit() and int(slot) >= 4:
            candidates.extend(
                (
                    f"{_IFU_PREFIX}s1_baseInstrData_{slot}",
                    f"{_IFU_PREFIX}__Vtogcov__s1_baseInstrData_{slot}",
                )
            )
    value = _read_first(
        recorder,
        tuple(candidates),
    )
    return None if value is None else int(value)


def _registered_stage_snapshot(recorder, stage: str) -> tuple[Optional[dict], list[str]]:
    valid_mask = _read_ifu_pipeline_internal(recorder, f"{stage}_alignedInstrValid")
    missing = [] if valid_mask is not None else [f"{stage}_alignedInstrValid"]
    if valid_mask is None:
        return None, missing

    snapshot = {
        "valid_mask": int(valid_mask),
        "instr_count": _read_ifu_pipeline_internal(recorder, f"{stage}_instrCount"),
        "fetch_blocks": [],
        "slots": [],
    }
    if snapshot["instr_count"] is None:
        missing.append(f"{stage}_instrCount")
    for block in range(2):
        block_snapshot = {}
        for field in ("valid", "ftqIdx_flag", "ftqIdx_value", "startVAddr_addr"):
            stem = f"{stage}_fetchBlock_{block}_{field}"
            value = _read_ifu_pipeline_internal(recorder, stem)
            block_snapshot[field] = value
            if value is None:
                missing.append(stem)
        snapshot["fetch_blocks"].append(block_snapshot)

    for slot in range(_REGISTERED_TRANSACTION_SLOT_COUNT):
        if ((int(valid_mask) >> slot) & 1) == 0:
            continue
        fields = {
            "data": f"{stage}_alignedInstrVec_{slot}_data",
            "is_rvc": f"{stage}_alignedInstrVec_{slot}_isRvc",
            "raw_block_sel": f"{stage}_alignedInstrVec_{slot}_blockSel",
            "is_cross_block_instr": f"{stage}_alignedInstrVec_{slot}_isCrossBlockInstr",
            "pc": f"{stage}_alignedInstrPcVec_{slot}_addr",
            "branch_type": f"{stage}_alignedPdInfoVec_{slot}_brAttribute_branchType",
        }
        values = {name: _read_ifu_pipeline_internal(recorder, stem) for name, stem in fields.items()}
        missing.extend(stem for name, stem in fields.items() if values[name] is None)
        values["slot"] = int(slot)
        values["effective_owner"] = (
            None
            if values["raw_block_sel"] is None or values["is_cross_block_instr"] is None
            else int(values["raw_block_sel"]) | int(values["is_cross_block_instr"])
        )
        snapshot["slots"].append(values)
    return snapshot, missing


_S1_SEMANTIC_FIELDS = (
    "slot",
    "index",
    "data",
    "is_rvc",
    "raw_block_sel",
    "is_cross_block_instr",
    "effective_owner",
    "pc",
    "branch_type",
)

_S2_SEMANTIC_FIELDS = (
    "slot",
    "data",
    "is_rvc",
    "raw_block_sel",
    "is_cross_block_instr",
    "effective_owner",
    "pc",
    "branch_type",
    "predecode_is_rvc",
    "ras_action",
    "end_offset",
    "jump_offset",
)


def _semantic_view(item: dict, fields: tuple[str, ...]) -> dict:
    return {field: item[field] for field in fields}


def _check_s1_aggregate_semantics(
    recorder,
    cycle: int,
    aggregate: dict,
    s1_snapshot: dict,
) -> Optional[dict]:
    instr_align_ptr = _read_ifu_pipeline_internal(
        recorder, "s1_prevIBufEnqPtrDup_dup_0_value"
    )
    valid_align_ptr = _read_ifu_pipeline_internal(
        recorder, "s1_prevIBufEnqPtrDup_dup_1_value"
    )
    prev_valid = _read_ifu_pipeline_internal(
        recorder, "s1_prevEndHalfRviInfo_valid"
    )
    missing = [
        name
        for name, value in (
            ("s1_prevIBufEnqPtrDup_dup_0_value", instr_align_ptr),
            ("s1_prevIBufEnqPtrDup_dup_1_value", valid_align_ptr),
            ("s1_prevEndHalfRviInfo_valid", prev_valid),
        )
        if value is None
    ]
    prev_data = None
    prev_pc = None
    if prev_valid == 1:
        prev_data = _read_ifu_pipeline_internal(
            recorder, "s1_prevEndHalfRviInfo_bits_data"
        )
        prev_pc = _read_ifu_pipeline_internal(
            recorder, "s1_prevEndHalfRviInfo_bits_pc_addr"
        )
        missing.extend(
            name
            for name, value in (
                ("s1_prevEndHalfRviInfo_bits_data", prev_data),
                ("s1_prevEndHalfRviInfo_bits_pc_addr", prev_pc),
            )
            if value is None
        )
    if missing:
        _record_mismatch(
            recorder,
            "ifu_s1_alignment_probe_unobservable",
            cycle,
            stage="s1",
            missing=missing,
            aggregate=aggregate,
        )
        return None

    expected_by_slot = _expected_s1_semantic_slots(
        aggregate,
        align_shift=int(instr_align_ptr) & 0x3,
        prev_half_info={
            "valid": int(prev_valid),
            "data": 0 if prev_data is None else int(prev_data),
            "pc": 0 if prev_pc is None else int(prev_pc),
        },
    )
    observed_items = []
    expected_items = []
    mismatches = []
    instr_align_shift = int(instr_align_ptr) & 0x3
    valid_align_shift = int(valid_align_ptr) & 0x3
    instr_count = int(s1_snapshot["instr_count"])
    valid_mask = int(s1_snapshot["valid_mask"])
    expected_valid_mask = (
        ((1 << instr_count) - 1) << valid_align_shift
        if instr_count > 0
        else 0
    )
    if instr_align_shift != valid_align_shift:
        mismatches.append(
            {
                "reason": "duplicated_ibuffer_alignment_pointer_mismatch",
                "instr_align_shift": instr_align_shift,
                "valid_align_shift": valid_align_shift,
            }
        )
    if valid_mask.bit_count() != instr_count or valid_mask != expected_valid_mask:
        mismatches.append(
            {
                "reason": "valid_mask_instr_count_or_compaction_mismatch",
                "valid_mask": valid_mask,
                "instr_count": instr_count,
                "expected_valid_mask": expected_valid_mask,
                "valid_align_shift": valid_align_shift,
            }
        )
    expected_slots = set(range(instr_align_shift, instr_align_shift + instr_count))
    observed_slots = {int(item["slot"]) for item in s1_snapshot["slots"]}
    if observed_slots != expected_slots:
        mismatches.append(
            {
                "reason": "registered_valid_slots_do_not_match_compacted_transaction",
                "expected_slots": sorted(expected_slots),
                "observed_slots": sorted(observed_slots),
            }
        )
    for slot_item in s1_snapshot["slots"]:
        slot = int(slot_item["slot"])
        index_stem = f"s1_alignedInstrVec_{slot}_index"
        index = _read_ifu_pipeline_internal(recorder, index_stem)
        if index is None:
            missing.append(index_stem)
            continue
        observed = {**slot_item, "index": int(index)}
        expected = expected_by_slot.get(slot)
        observed_view = _semantic_view(observed, _S1_SEMANTIC_FIELDS)
        observed_items.append(observed_view)
        if expected is None:
            mismatches.append(
                {
                    "slot": slot,
                    "reason": "valid_slot_has_no_aggregate_instruction",
                    "observed": observed_view,
                }
            )
            continue
        expected_view = _semantic_view(expected, _S1_SEMANTIC_FIELDS)
        expected_items.append(expected_view)
        if observed_view != expected_view:
            mismatches.append(
                {
                    "slot": slot,
                    "reason": "index_extract_stitch_pc_or_predecode_mismatch",
                    "expected": expected_view,
                    "observed": observed_view,
                }
            )

    if missing:
        _record_mismatch(
            recorder,
            "ifu_s1_alignment_probe_unobservable",
            cycle,
            stage="s1",
            missing=missing,
            aggregate=aggregate,
        )
        return None
    if mismatches:
        _record_mismatch(
            recorder,
            "ifu_s1_alignment_semantic_mismatch",
            cycle,
            aggregate=aggregate,
            mismatches=mismatches,
        )
        return None

    expected_s2 = [
        _semantic_view(expected_by_slot[int(item["slot"])], _S2_SEMANTIC_FIELDS)
        for item in observed_items
    ]
    evidence = {
        "event": "ifu_s1_alignment_semantic_pass",
        "cycle": int(cycle),
        "aggregate": aggregate,
        "instr_align_shift": instr_align_shift,
        "valid_align_shift": valid_align_shift,
        "instr_count": instr_count,
        "valid_mask": valid_mask,
        "observed": observed_items,
        "expected": expected_items,
    }
    recorder.risk_observations.append(evidence)
    return {"s1": observed_items, "s2_expected": expected_s2, "evidence": evidence}


def _s2_semantic_snapshot(recorder, snapshot: dict) -> tuple[list[dict], list[str]]:
    observed = []
    missing = []
    for slot_item in snapshot["slots"]:
        slot = int(slot_item["slot"])
        end_stem = f"s2_alignedInstrVec_{slot}_endOffset"
        jump_stem = f"s2_alignedJumpOffsetVec_{slot}_addr"
        pd_is_rvc_stem = f"s2_alignedPdInfoVec_{slot}_isRVC"
        ras_action_stem = f"s2_alignedPdInfoVec_{slot}_brAttribute_rasAction"
        end_offset = _read_ifu_pipeline_internal(recorder, end_stem)
        jump_offset = _read_ifu_pipeline_internal(recorder, jump_stem)
        predecode_is_rvc = _read_ifu_pipeline_internal(recorder, pd_is_rvc_stem)
        ras_action = _read_ifu_pipeline_internal(recorder, ras_action_stem)
        missing.extend(
            name
            for name, value in (
                (end_stem, end_offset),
                (jump_stem, jump_offset),
                (pd_is_rvc_stem, predecode_is_rvc),
                (ras_action_stem, ras_action),
            )
            if value is None
        )
        if (
            end_offset is None
            or jump_offset is None
            or predecode_is_rvc is None
            or ras_action is None
        ):
            continue
        observed.append(
            _semantic_view(
                {
                    **slot_item,
                    "end_offset": int(end_offset),
                    "jump_offset": int(jump_offset),
                    "predecode_is_rvc": int(predecode_is_rvc),
                    "ras_action": int(ras_action),
                },
                _S2_SEMANTIC_FIELDS,
            )
        )
    return observed, missing


def _sample_registered_s1_s2_transaction(recorder, cycle: int) -> None:
    pending = recorder._ifu_cacheable_s1_s2_pending
    if pending is not None:
        s2_flush = _read_ifu_pipeline_internal(recorder, "s2_flush")
        s2_valid = _read_ifu_pipeline_internal(recorder, "s2_valid_valid")
        if s2_flush is None or s2_valid is None:
            _record_mismatch(
                recorder,
                "ifu_s1_s2_transaction_control_unobservable",
                cycle,
                s1_cycle=pending["s1_cycle"],
                missing=[
                    name
                    for name, value in (("s2_flush", s2_flush), ("s2_valid_valid", s2_valid))
                    if value is None
                ],
            )
            recorder._ifu_cacheable_s1_s2_pending = None
        elif s2_flush == 1:
            _record_mismatch(
                recorder,
                "ifu_s1_s2_transaction_flushed",
                cycle,
                s1_cycle=pending["s1_cycle"],
                aggregate=pending["aggregate"],
            )
            recorder._ifu_cacheable_s1_s2_pending = None
        elif s2_valid == 1:
            observed, missing = _registered_stage_snapshot(recorder, "s2")
            if missing:
                _record_mismatch(
                    recorder,
                    "ifu_s1_s2_transaction_probe_unobservable",
                    cycle,
                    stage="s2",
                    s1_cycle=pending["s1_cycle"],
                    missing=missing,
                )
            elif observed != pending["s1"]:
                _record_mismatch(
                    recorder,
                    "ifu_s1_s2_registered_transaction_mismatch",
                    cycle,
                    s1_cycle=pending["s1_cycle"],
                    aggregate=pending["aggregate"],
                    expected=pending["s1"],
                    observed=observed,
                )
            else:
                s2_semantic, semantic_missing = _s2_semantic_snapshot(
                    recorder, observed
                )
                if semantic_missing:
                    _record_mismatch(
                        recorder,
                        "ifu_s1_s2_transaction_probe_unobservable",
                        cycle,
                        stage="s2_semantics",
                        s1_cycle=pending["s1_cycle"],
                        missing=semantic_missing,
                    )
                elif s2_semantic != pending["semantics"]["s2_expected"]:
                    _record_mismatch(
                        recorder,
                        "ifu_s2_registered_semantic_mismatch",
                        cycle,
                        s1_cycle=pending["s1_cycle"],
                        aggregate=pending["aggregate"],
                        expected=pending["semantics"]["s2_expected"],
                        observed=s2_semantic,
                    )
                else:
                    recorder.risk_observations.append(
                        {
                            "event": "ifu_s1_s2_registered_transaction_pass",
                            "cycle": int(cycle),
                            "s0_cycle": int(pending["s0_cycle"]),
                            "s1_cycle": int(pending["s1_cycle"]),
                            "aggregate": pending["aggregate"],
                            "s1": pending["s1"],
                            "s2": observed,
                            "semantics": pending["semantics"],
                        }
                    )
            recorder._ifu_cacheable_s1_s2_pending = None
        elif int(cycle) > int(pending["s1_cycle"]) + 2:
            _record_mismatch(
                recorder,
                "ifu_s1_s2_transaction_timeout",
                cycle,
                s1_cycle=pending["s1_cycle"],
                aggregate=pending["aggregate"],
            )
            recorder._ifu_cacheable_s1_s2_pending = None

    verified = recorder._ifu_cacheable_last_verified
    if (
        verified is None
        or int(verified["cycle"]) != int(cycle)
        or _read_signal(recorder, "s1_fire") != 1
        or _read_signal(recorder, "s1_flush") == 1
    ):
        return
    if recorder._ifu_cacheable_s1_s2_pending is not None:
        _record_mismatch(
            recorder,
            "ifu_s1_s2_transaction_pending_collision",
            cycle,
            previous=recorder._ifu_cacheable_s1_s2_pending,
            new_aggregate=verified["source"],
        )
        return
    s1_snapshot, missing = _registered_stage_snapshot(recorder, "s1")
    if missing:
        _record_mismatch(
            recorder,
            "ifu_s1_s2_transaction_probe_unobservable",
            cycle,
            stage="s1",
            aggregate=verified["source"],
            missing=missing,
        )
        return
    semantics = _check_s1_aggregate_semantics(
        recorder,
        cycle,
        verified["aggregate"],
        s1_snapshot,
    )
    if semantics is None:
        return
    recorder._ifu_cacheable_s1_s2_pending = {
        "s0_cycle": int(verified["accepted_cycle"]),
        "s1_cycle": int(cycle),
        "aggregate": verified["aggregate"],
        "s1": s1_snapshot,
        "semantics": semantics,
    }


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
    recorder._ifu_cacheable_last_verified = {
        "cycle": int(cycle),
        "accepted_cycle": int(pending["cycle"]),
        "source": expected,
        "aggregate": pending["snapshot"],
    }
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


def _arm_backend_flush_causality(
    recorder,
    cycle: int,
    *,
    source: Optional[dict],
    req_valid: Optional[int],
    s0_fire: Optional[int],
    backend_redirect: Optional[int],
) -> None:
    if backend_redirect != 1 or req_valid != 1 or s0_fire != 0:
        return
    if source is None:
        _record_mismatch(
            recorder,
            "ifu_cacheable_backend_flush_source_unobservable",
            cycle,
            input_path=_SIGNALS["backend_redirect"][0],
        )
        return
    pending = recorder._ifu_cacheable_backend_flush_pending
    if pending is not None:
        if pending["source"] == source:
            return
        _record_mismatch(
            recorder,
            "ifu_cacheable_backend_flush_source_changed",
            cycle,
            redirect_cycle=int(pending["redirect_cycle"]),
            expected=pending["source"],
            observed=source,
        )
    recorder._ifu_cacheable_backend_flush_pending = {
        "redirect_cycle": int(cycle),
        "source": source,
        "input_path": _SIGNALS["backend_redirect"][0],
        "ifu_path": _SIGNALS["ifu_backend_redirect"][0],
    }


def _resolve_backend_flush_causality(
    recorder,
    cycle: int,
    *,
    source: Optional[dict],
    req_valid: Optional[int],
    req_ready: Optional[int],
    s0_fire: Optional[int],
    s0_flush: Optional[int],
    ifu_backend_redirect: Optional[int],
) -> bool:
    pending = recorder._ifu_cacheable_backend_flush_pending
    if pending is None:
        return False
    age = int(cycle) - int(pending["redirect_cycle"])
    if age <= 0:
        return False
    if ifu_backend_redirect is None:
        _record_mismatch(
            recorder,
            "ifu_cacheable_backend_flush_internal_unobservable",
            cycle,
            redirect_cycle=int(pending["redirect_cycle"]),
            path=pending["ifu_path"],
        )
        recorder._ifu_cacheable_backend_flush_pending = None
        return False
    if req_valid == 1 and source is None:
        _record_mismatch(
            recorder,
            "ifu_cacheable_backend_flush_source_unobservable",
            cycle,
            redirect_cycle=int(pending["redirect_cycle"]),
            input_path=pending["input_path"],
        )
        recorder._ifu_cacheable_backend_flush_pending = None
        return False
    if req_valid == 1 and source != pending["source"]:
        _record_mismatch(
            recorder,
            "ifu_cacheable_backend_flush_source_changed",
            cycle,
            redirect_cycle=int(pending["redirect_cycle"]),
            expected=pending["source"],
            observed=source,
        )
        recorder._ifu_cacheable_backend_flush_pending = None
        return False
    if ifu_backend_redirect == 1:
        if s0_fire == 1 or s0_flush != 1:
            _record_mismatch(
                recorder,
                "ifu_cacheable_backend_flush_lost_to_fire",
                cycle,
                redirect_cycle=int(pending["redirect_cycle"]),
                s0_fire=s0_fire,
                s0_flush=s0_flush,
                req_valid=req_valid,
            )
            recorder._ifu_cacheable_backend_flush_pending = None
            return False
        evidence = {
            "event": "ifu_s0_backend_redirect_blocks_aggregate_response",
            "redirect_cycle": int(pending["redirect_cycle"]),
            "flush_cycle": int(cycle),
            "pipeline_latency": int(age),
            "req_valid_at_flush": req_valid,
            "req_ready_at_flush": req_ready,
            "s0_fire": s0_fire,
            "s0_flush": s0_flush,
            "backend_redirect_input_path": pending["input_path"],
            "ifu_backend_redirect_path": pending["ifu_path"],
            "source": pending["source"],
        }
        recorder.mark("ifu_cacheable_flush", "flush_wins_fire", cycle, evidence)
        recorder.mark("ifu_cacheable_flush", "backend_redirect_blocks", cycle, evidence)
        recorder._ifu_cacheable_backend_flush_pending = None
        return True
    if s0_fire == 1:
        _record_mismatch(
            recorder,
            "ifu_cacheable_backend_flush_lost_to_fire",
            cycle,
            redirect_cycle=int(pending["redirect_cycle"]),
            s0_fire=s0_fire,
            s0_flush=s0_flush,
            req_valid=req_valid,
        )
        recorder._ifu_cacheable_backend_flush_pending = None
        return False
    if age > 3:
        _record_mismatch(
            recorder,
            "ifu_cacheable_backend_flush_timeout",
            cycle,
            redirect_cycle=int(pending["redirect_cycle"]),
            age=int(age),
            req_valid=req_valid,
            s0_fire=s0_fire,
            s0_flush=s0_flush,
            ifu_backend_redirect=ifu_backend_redirect,
        )
        recorder._ifu_cacheable_backend_flush_pending = None
    return False


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
    _sample_registered_s1_s2_transaction(recorder, cycle)

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
    ifu_backend_redirect = _read_signal(recorder, "ifu_backend_redirect")
    wb_redirect = _read_signal(recorder, "wb_redirect")
    bpu_s3_flush = _read_signal(recorder, "bpu_s3_flush")
    s0_flush_bpu = _read_signal(recorder, "s0_flush_bpu")
    flush_blocks = req_valid == 1 and s0_flush == 1 and s0_fire == 0
    flush_evidence = {
        "event": "ifu_s0_flush_blocks_icache_return",
        "req_ready": req_ready,
        "backend_redirect": backend_redirect,
        "ifu_backend_redirect": ifu_backend_redirect,
        "wb_redirect": wb_redirect,
        "bpu_s3_flush": bpu_s3_flush,
        "s0_flush_bpu": s0_flush_bpu,
        "source": source,
    }
    had_backend_flush_pending = recorder._ifu_cacheable_backend_flush_pending is not None
    causal_backend_flush = _resolve_backend_flush_causality(
        recorder,
        cycle,
        source=source,
        req_valid=req_valid,
        req_ready=req_ready,
        s0_fire=s0_fire,
        s0_flush=s0_flush,
        ifu_backend_redirect=ifu_backend_redirect,
    )
    if flush_blocks:
        if not causal_backend_flush:
            recorder.mark("ifu_cacheable_flush", "flush_wins_fire", cycle, flush_evidence)
        if (
            (backend_redirect == 1 or ifu_backend_redirect == 1)
            and not causal_backend_flush
            and not had_backend_flush_pending
        ):
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

    if not flush_blocks:
        _arm_backend_flush_causality(
            recorder,
            cycle,
            source=source,
            req_valid=req_valid,
            s0_fire=s0_fire,
            backend_redirect=backend_redirect,
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

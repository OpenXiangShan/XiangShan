from __future__ import annotations

from collections import deque
from typing import Optional

from ..common.dut import _dut, _read_first


_ICACHE_PREFIX = "Frontend_top.Frontend.inner_icache."
_IFU_PREFIX = "Frontend_top.Frontend.inner_ifu."


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


def reset_ifu_cacheable_pipeline_state(recorder) -> None:
    initialize_ifu_cacheable_pipeline_state(recorder)


def _read_signal(recorder, key: str) -> Optional[int]:
    value = _read_first(recorder, _SIGNALS[key])
    return None if value is None else int(value)


def _req_signal_names(index: int, field: str) -> tuple[str, ...]:
    return (
        f"{_ICACHE_PREFIX}__Vtogcov__io_toIfu_req_bits_{index}_{field}",
        f"{_ICACHE_PREFIX}mainPipe.io_toIfu_req_bits_{index}_{field}",
    )


def _read_req_field(recorder, index: int, field: str) -> Optional[int]:
    value = _read_first(recorder, _req_signal_names(index, field))
    return None if value is None else int(value)


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
                "range",
                "size",
            ):
                value = _read_req_field(recorder, index, field)
                if value is None:
                    return None
                block[field] = int(value)
        blocks.append(block)

    payload = {"blocks": blocks}
    for index in range(2):
        if not blocks[index]["valid"]:
            continue
        for field in ("data", "maybeRvcMap"):
            value = _read_req_field(recorder, index, field)
            if value is not None:
                payload[f"req{index}_{field}"] = int(value)
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
                "range",
                "size",
            ):
                value = _read_s1_field(recorder, index, field)
                if value is None:
                    return None
                block[field] = int(value)
        blocks.append(block)
    return {"blocks": blocks}


def _metadata_blocks(snapshot: dict) -> list[dict]:
    fields = (
        "valid",
        "ftqIdx_flag",
        "ftqIdx_value",
        "startVAddr_addr",
        "takenCfiOffset_valid",
        "takenCfiOffset_bits",
        "range",
        "size",
    )
    return [{field: block[field] for field in fields if field in block} for block in snapshot["blocks"]]


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

    expected = _metadata_blocks(pending["snapshot"])
    observed = _metadata_blocks(s1_snapshot)
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
    second_valid = expected[1]["valid"] == 1
    recorder.mark(
        "ifu_cacheable_window",
        "dual_block" if second_valid else "single_block",
        cycle,
        evidence,
    )
    recorder.mark("ifu_cacheable_metadata", "first_ftq_preserved", cycle, evidence)
    if second_valid:
        recorder.mark("ifu_cacheable_metadata", "second_ftq_preserved", cycle, evidence)
    if expected[0]["takenCfiOffset_valid"] == 0:
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

    valid_blocks = [block for block in expected if block.get("valid") == 1]
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
    for block in expected:
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

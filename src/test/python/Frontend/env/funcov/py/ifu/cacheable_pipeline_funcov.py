from __future__ import annotations

from typing import Optional

from ..common.dut import _dut, _read_first


_ICACHE_PREFIX = "Frontend_top.Frontend.inner_icache."
_IFU_PREFIX = "Frontend_top.Frontend.inner_ifu."


IFU_CACHEABLE_PIPELINE_COVERPOINTS = {
    "ifu_cacheable_ingress": "ingress_state",
    "ifu_cacheable_transfer": "transfer_state",
    "ifu_cacheable_window": "window_shape",
    "ifu_cacheable_metadata": "metadata_transfer",
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

    previous = recorder._ifu_cacheable_last_verified
    if pending["gapped"] and previous is not None and previous["source"] != expected:
        recorder.mark(
            "ifu_cacheable_transfer",
            "gapped_metadata_isolated",
            cycle,
            {**evidence, "previous": previous},
        )
    recorder._ifu_cacheable_last_verified = {"cycle": int(cycle), "source": expected}
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

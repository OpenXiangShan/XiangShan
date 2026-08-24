from __future__ import annotations

from typing import Any, Optional


MMIO_V3_CHECKED_EVENT_TYPE = "ifu.v3.mmio.checked"
MMIO_V3_COVERPOINTS = {
    "ifu_mmio_tl_a_stall": "request_context",
    "ifu_mmio_page_tail": "rvc_progress",
    "ifu_mmio_exception_priority": "first_page_iaf",
    "ifu_mmio_backpressure": "result_handoff",
}
MMIO_V3_SAMPLER_BIN_KEYS = frozenset(
    {
        ("ifu_mmio_tl_a_stall", "stable_until_accept"),
        ("ifu_mmio_page_tail", "next_pc_plus_2b"),
        ("ifu_mmio_exception_priority", "second_page_exec_not_illegal"),
        ("ifu_mmio_backpressure", "reserved_slot_fire"),
    }
)

_IFU_PREFIXES = (
    "Frontend_top.Frontend.inner_ifu.",
    "Frontend.inner_ifu.",
    "Frontend_top.Frontend.inner_ifu.__Vtogcov__",
    "Frontend_top.Frontend._inner_ifu_",
    "Frontend._inner_ifu_",
)
_UNCACHE_UNIT_PREFIXES = (
    "Frontend_top.Frontend.inner_ifu.uncacheUnit.",
    "Frontend.inner_ifu.uncacheUnit.",
    "Frontend_top.Frontend.inner_ifu.uncacheUnit.__Vtogcov__",
    "Frontend.inner_ifu.uncacheUnit.__Vtogcov__",
    "Frontend_top.Frontend.inner_ifu._uncacheUnit_",
    "Frontend.inner_ifu._uncacheUnit_",
)


def _read(recorder, dut, *names: str) -> Optional[int]:
    return recorder._read_first_dut_signal(dut, tuple(str(name) for name in names))


def _ifu_names(stem: str) -> tuple[str, ...]:
    return tuple(prefix + str(stem) for prefix in _IFU_PREFIXES)


def _read_ifu(recorder, dut, stem: str) -> Optional[int]:
    return _read(recorder, dut, *_ifu_names(stem))


def _read_uncache_unit(recorder, dut, stem: str) -> Optional[int]:
    return _read(
        recorder,
        dut,
        *(prefix + str(stem) for prefix in _UNCACHE_UNIT_PREFIXES),
    )


def initialize_mmio_v3_coverage_state(recorder) -> None:
    recorder._ifu_mmio_stalled_a = None
    recorder._ifu_mmio_page_tail_rvc = None
    recorder._ifu_mmio_observation_index = 0


def reset_mmio_v3_coverage_state(recorder) -> None:
    initialize_mmio_v3_coverage_state(recorder)


def _is_mmio_pc(env, pc: int) -> bool:
    memory = getattr(env, "memory", None)
    return any(
        int(start) <= int(pc) < int(end)
        for start, end in getattr(memory, "mmio_ranges", ())
    )


def _sample_page_tail_rvc(recorder, env, dut, cycle: int) -> None:
    observations = list(getattr(getattr(env, "monitor", None), "observations", ()))
    index = int(getattr(recorder, "_ifu_mmio_observation_index", 0))
    if index > len(observations):
        index = 0
    recorder._ifu_mmio_observation_index = len(observations)

    pending = getattr(recorder, "_ifu_mmio_page_tail_rvc", None)
    delivered_exception = _read_ifu(recorder, dut, "io_toIBuffer_bits_exceptionType_value")
    need_resend = _read_uncache_unit(recorder, dut, "io_resp_bits_needResend")

    for observation in observations[index:]:
        pc = int(observation.pc)
        is_mmio = _is_mmio_pc(env, pc)
        if pending is not None:
            if is_mmio and pc == int(pending["pc"]) + 2:
                recorder.mark(
                    "ifu_mmio_page_tail",
                    "next_pc_plus_2b",
                    cycle,
                    {
                        "event": "mmio_page_tail_rvc_next_delivery",
                        "response_cycle": int(pending["cycle"]),
                        "previous_pc": int(pending["pc"]),
                        "current_pc": pc,
                    },
                )
            pending = None
            recorder._ifu_mmio_page_tail_rvc = None

        clean_page_tail_rvc = (
            is_mmio
            and (pc & 0xFFF) == 0xFFE
            and bool(observation.is_rvc)
            and delivered_exception == 0
            and need_resend == 0
        )
        if clean_page_tail_rvc:
            pending = {"cycle": int(cycle), "pc": pc}
            recorder._ifu_mmio_page_tail_rvc = pending

    if _flush_or_redirect(recorder, dut):
        recorder._ifu_mmio_page_tail_rvc = None


def _flush_or_redirect(recorder, dut) -> bool:
    flush = _read_ifu(recorder, dut, "s2_flush")
    redirect = _read(
        recorder,
        dut,
        "Frontend_top.io_backend_toFtq_redirect_valid",
        "io_backend_toFtq_redirect_valid",
    )
    return flush == 1 or redirect == 1


def _sample_tl_a_stall(recorder, dut, cycle: int) -> None:
    valid = _read(recorder, dut, "auto_inner_instrUncache_client_out_a_valid")
    ready = _read(recorder, dut, "auto_inner_instrUncache_client_out_a_ready")
    address = _read(recorder, dut, "auto_inner_instrUncache_client_out_a_bits_address")
    context = (
        address,
        _read_ifu(recorder, dut, "s2_fetchBlock_0_startVAddr_addr"),
        _read_ifu(recorder, dut, "s2_icacheMeta_0_pmpMmio"),
        _read_ifu(recorder, dut, "s2_icacheMeta_0_itlbPbmt"),
        _read_ifu(recorder, dut, "s2_icacheMeta_0_exception_value"),
    )
    pending = getattr(recorder, "_ifu_mmio_stalled_a", None)
    if valid == 1 and ready == 0 and None not in context:
        if pending is not None and tuple(pending["context"]) == context:
            recorder.mark(
                "ifu_mmio_tl_a_stall",
                "stable_until_accept",
                cycle,
                {
                    "event": "mmio_tl_a_stall_context_stable",
                    "stall_start_cycle": int(pending["cycle"]),
                    "address": int(address),
                    "s2_pc": int(context[1]),
                    "pmp_mmio": int(context[2]),
                    "pbmt": int(context[3]),
                    "exception": int(context[4]),
                },
            )
        elif pending is None:
            recorder._ifu_mmio_stalled_a = {"cycle": int(cycle), "context": context}
        else:
            recorder._ifu_mmio_stalled_a = None
            recorder.risk_observations.append(
                {
                    "event": "mmio_tl_a_stall_context_changed",
                    "cycle": int(cycle),
                    "before": list(pending["context"]),
                    "after": list(context),
                }
            )
    elif ready == 1 or valid == 0 or _flush_or_redirect(recorder, dut):
        recorder._ifu_mmio_stalled_a = None


def sample_mmio_v3_coverage(recorder, env, cycle: int) -> None:
    dut = getattr(env, "dut", None)
    if dut is None:
        return
    _sample_tl_a_stall(recorder, dut, int(cycle))
    _sample_page_tail_rvc(recorder, env, dut, int(cycle))
    _sample_result_backpressure(recorder, dut, int(cycle))


def _sample_result_backpressure(recorder, dut, cycle: int) -> None:
    to_valid = _read_ifu(recorder, dut, "io_toIBuffer_valid")
    to_ready = _read_ifu(recorder, dut, "io_toIBuffer_ready")
    resp_valid = _read_uncache_unit(recorder, dut, "io_resp_valid")
    req_uncache = _read_ifu(recorder, dut, "s2_reqIsUncache")
    pmp_mmio = _read_ifu(recorder, dut, "s2_icacheMeta_0_pmpMmio")
    if not (
        resp_valid == 1
        and to_valid == 1
        and req_uncache == 1
        and pmp_mmio == 1
    ):
        return

    evidence = {
        "event": "mmio_result_reserved_ibuffer_slot",
        "response_data": _read_uncache_unit(recorder, dut, "io_resp_bits_uncacheData"),
        "response_exception": _read_uncache_unit(
            recorder, dut, "io_resp_bits_exception_value"
        ),
        "response_need_resend": _read_uncache_unit(
            recorder, dut, "io_resp_bits_needResend"
        ),
        "enq_enable": _read_ifu(recorder, dut, "io_toIBuffer_bits_enqEnable"),
        "delivery_exception": _read_ifu(
            recorder, dut, "io_toIBuffer_bits_exceptionType_value"
        ),
    }
    if to_ready == 1:
        recorder.mark(
            "ifu_mmio_backpressure",
            "reserved_slot_fire",
            cycle,
            evidence,
        )
    elif to_ready == 0:
        recorder.risk_observations.append(
            {
                **evidence,
                "event": "mmio_result_missing_reserved_ibuffer_slot",
                "cycle": int(cycle),
            }
        )


def handle_mmio_v3_checked_event(recorder, event: dict[str, Any]) -> bool:
    if str(event.get("type", "")) != MMIO_V3_CHECKED_EVENT_TYPE:
        return False
    cycle = int(event.get("cycle", 0))
    payload = event.get("payload")
    if not isinstance(payload, dict):
        payload = {}
    observations = payload.get("observations")
    accepted = (
        payload.get("bin_id") == "BIN-1014"
        and payload.get("condition_met") is True
        and payload.get("checkpoint_passed") is True
        and isinstance(observations, dict)
        and observations.get("first_page_execute") is False
        and observations.get("second_page_execute") is True
        and observations.get("delivered_exception") == 3
        and observations.get("illegal_instruction") is False
    )
    if not accepted:
        recorder.risk_observations.append(
            {
                "event": "ifu_mmio_v3_checked_event_rejected",
                "cycle": cycle,
                "bin_id": payload.get("bin_id"),
            }
        )
        return False
    recorder.mark(
        "ifu_mmio_exception_priority",
        "second_page_exec_not_illegal",
        cycle,
        {
            "event": MMIO_V3_CHECKED_EVENT_TYPE,
            "bin_id": "BIN-1014",
            "observations": observations,
            "producer": str(payload.get("producer", "directed_checker")),
        },
    )
    return True


__all__ = [
    "MMIO_V3_CHECKED_EVENT_TYPE",
    "MMIO_V3_COVERPOINTS",
    "MMIO_V3_SAMPLER_BIN_KEYS",
    "handle_mmio_v3_checked_event",
    "initialize_mmio_v3_coverage_state",
    "reset_mmio_v3_coverage_state",
    "sample_mmio_v3_coverage",
]

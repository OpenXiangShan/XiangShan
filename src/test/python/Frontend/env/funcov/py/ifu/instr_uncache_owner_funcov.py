from __future__ import annotations

from typing import Any, Optional

from ....support import fold_pc


INSTR_UNCACHE_OWNER_GROUP = "ifu_instruncache_owner_v3"
INSTR_UNCACHE_OWNER_COVERPOINT = "protocol_leaf"
INSTR_UNCACHE_OWNER_LEAF_COUNT = 38
INSTR_UNCACHE_OWNER_COVERPOINTS = {
    INSTR_UNCACHE_OWNER_GROUP: INSTR_UNCACHE_OWNER_COVERPOINT,
}
INSTR_UNCACHE_OWNER_SAMPLER_BIN_KEYS = frozenset(
    (INSTR_UNCACHE_OWNER_GROUP, f"instruncache_leaf_{index:03d}")
    for index in range(1, INSTR_UNCACHE_OWNER_LEAF_COUNT + 1)
)

_ENTRY_IDLE = 0
_ENTRY_REFILL_REQ = 1
_ENTRY_REFILL_RESP = 2
_REDIRECTED_RESEND_TIMEOUT_CYCLES = 512


def initialize_instr_uncache_owner_coverage_state(recorder) -> None:
    recorder._ifu_instr_uncache_owner_state = {
        "previous_entry_state": _ENTRY_IDLE,
        "previous_entry_resending": False,
        "previous_tl_a_fire": False,
        "stalled_a": None,
        "d_response_pending": None,
        "cross_8b_pending": None,
        "cross_page_pending": None,
        "first_page_tl_fault_pending": None,
        "latest_accepted_request": None,
        "active_instruncache_request": None,
        "redirected_wait_d_pending": None,
        "redirected_resend_pending": None,
        "second_page_exception_types": set(),
        "non_cross_rvi_offsets": set(),
        "attribute_modes": set(),
        "accepted_request_attributes": None,
        "attribute_transactions": [],
        "completed_attribute_transactions": [],
        "wfi_retracted_a": None,
    }


def reset_instr_uncache_owner_coverage_state(recorder) -> None:
    initialize_instr_uncache_owner_coverage_state(recorder)


def _mark(recorder, index: int, cycle: int, evidence: dict[str, Any]) -> None:
    recorder.mark(
        INSTR_UNCACHE_OWNER_GROUP,
        f"instruncache_leaf_{int(index):03d}",
        int(cycle),
        {"event": "instr_uncache_protocol_observation", **evidence},
    )


def _record_risk(recorder, event: str, cycle: int, evidence: dict[str, Any]) -> None:
    risks = getattr(recorder, "risk_observations", None)
    if isinstance(risks, list):
        risks.append({"event": str(event), "cycle": int(cycle), **evidence})


def _response_half(data: Optional[int], pruned_addr: Optional[int]) -> Optional[int]:
    if data is None or pruned_addr is None:
        return None
    shift = (int(pruned_addr) & 0x3) * 16
    return (int(data) >> shift) & 0xFFFF


def _response_is_rvi(data: Optional[int], pruned_addr: Optional[int]) -> bool:
    half = _response_half(data, pruned_addr)
    return half is not None and (half & 0x3) == 0x3


def _response_is_rvc(data: Optional[int], pruned_addr: Optional[int]) -> bool:
    half = _response_half(data, pruned_addr)
    return half is not None and (half & 0x3) != 0x3


def _selected_word(data: Optional[int], pruned_addr: Optional[int]) -> Optional[int]:
    if data is None or pruned_addr is None:
        return None
    shift = (int(pruned_addr) & 0x3) * 16
    return (int(data) >> shift) & 0xFFFFFFFF


def _entry_evidence(s: dict[str, Optional[int]]) -> dict[str, Any]:
    return {
        "req_is_mmio": s["req_is_mmio"],
        "req_pbmt": s["req_pbmt"],
        "entry_state": s["entry_state"],
        "entry_resending": s["entry_resending"],
        "entry_req_addr": s["entry_req_addr"],
        "entry_mem_back_type_mm": s["entry_mem_back_type_mm"],
        "entry_mem_page_type_nc": s["entry_mem_page_type_nc"],
        "tl_a_addr": s["tl_a_addr"],
        "tl_a_mem_back_type_mm": s["tl_a_mem_back_type_mm"],
        "tl_a_mem_page_type_nc": s["tl_a_mem_page_type_nc"],
        "tl_d_corrupt": s["tl_d_corrupt"],
        "tl_d_denied": s["tl_d_denied"],
        "instr_resp_need_resend": s["instr_resp_need_resend"],
        "to_valid": s["to_valid"],
        "to_ready": s["to_ready"],
        "to_enq": s["to_enq"],
        "to_pc": s["to_pc"],
        "to_foldpc": s["to_foldpc"],
        "to_ftq_flag": s["to_ftq_flag"],
        "to_ftq_value": s["to_ftq_value"],
        "to_ftq_offset": s["to_ftq_offset"],
        "s2_pc": s["s2_pc"],
        "s2_instr_pc": s["s2_instr_pc"],
        "to_is_rvc": s["to_is_rvc"],
        "to_exception": s["to_exception"],
        "s2_uncache_data": s["s2_uncache_data"],
        "prev_end_half": s["prev_end_half"],
        "prev_half_pc": s["prev_half_pc"],
        "prev_half_data": s["prev_half_data"],
        "to_uncache_valid": s["to_uncache_valid"],
        "to_uncache_ready": s["to_uncache_ready"],
        "to_uncache_addr": s["to_uncache_addr"],
        "uncache_pc": s["uncache_pc"],
    }


def sample_instr_uncache_owner_coverage(
    recorder,
    cycle: int,
    s: dict[str, Optional[int]],
) -> None:
    state = getattr(recorder, "_ifu_instr_uncache_owner_state", None)
    if not isinstance(state, dict):
        initialize_instr_uncache_owner_coverage_state(recorder)
        state = recorder._ifu_instr_uncache_owner_state

    evidence = _entry_evidence(s)
    entry_state = s["entry_state"]
    entry_addr = s["entry_req_addr"]
    entry_resending = s["entry_resending"] == 1
    tl_a_valid = s["tl_a_valid"] == 1
    tl_a_fire = tl_a_valid and s["tl_a_ready"] == 1
    tl_d_valid = s["tl_d_valid"] == 1 and entry_state == _ENTRY_REFILL_RESP
    page_tail = entry_addr is not None and (int(entry_addr) & 0x7FF) == 0x7FF
    beat_tail = entry_addr is not None and (int(entry_addr) & 0x3) == 0x3
    response_is_rvi = _response_is_rvi(s["tl_d_data"], entry_addr)
    clean_d = tl_d_valid and s["tl_d_corrupt"] == 0 and s["tl_d_denied"] == 0
    single_delivery = s["to_enq"] is not None and int(s["to_enq"]).bit_count() == 1

    if (
        s["req_valid"] == 1
        and s["req_ready"] == 1
        and s["req_is_mmio"] is not None
        and s["req_pbmt"] is not None
    ):
        mode = None
        if s["req_is_mmio"] == 0 and s["req_pbmt"] == 1:
            mode = "nc"
        elif s["req_is_mmio"] == 1 and s["req_pbmt"] != 1:
            mode = "mmio"
        accepted_attributes = {
            "mem_back_type_mm": int(s["req_is_mmio"] == 0),
            "mem_page_type_nc": int(s["req_pbmt"] == 1),
            "is_mmio": int(s["req_is_mmio"]),
            "pbmt": int(s["req_pbmt"]),
            "mode": mode,
        }
        state["accepted_request_attributes"] = accepted_attributes
        state["latest_accepted_request"] = {
            "s2_ftq_flag": s["s2_ftq_flag"],
            "s2_ftq_value": s["s2_ftq_value"],
            "s2_instr_pc": s["s2_instr_pc"],
        }
        if mode is not None:
            state["attribute_transactions"].append(
                {
                    **accepted_attributes,
                    "s2_ftq_flag": s["s2_ftq_flag"],
                    "s2_ftq_value": s["s2_ftq_value"],
                    "s2_instr_pc": s["s2_instr_pc"],
                    "tl_a_addr": None,
                    "tl_a_fire": False,
                    "response_seen": False,
                }
            )

    stalled = state["stalled_a"]
    if (
        stalled is not None
        and s.get("wfi_req") == 1
        and not tl_a_valid
        and state["wfi_retracted_a"] is None
    ):
        state["wfi_retracted_a"] = dict(stalled)

    wfi_retracted = state["wfi_retracted_a"]
    if (
        wfi_retracted is not None
        and s.get("wfi_req") == 0
        and tl_a_valid
        and s["tl_a_addr"] is not None
        and wfi_retracted.get("addr") == s["tl_a_addr"]
    ):
        _mark(
            recorder,
            9,
            cycle,
            {
                **evidence,
                "wfi_retracted_addr": wfi_retracted.get("addr"),
                "wfi_recovery_addr": s["tl_a_addr"],
            },
        )
        state["wfi_retracted_a"] = None

    if tl_a_valid and s["tl_a_ready"] == 0:
        current = {
            "addr": s["tl_a_addr"],
            "mem_back_type_mm": s["tl_a_mem_back_type_mm"],
            "mem_page_type_nc": s["tl_a_mem_page_type_nc"],
        }
        if stalled is None:
            state["stalled_a"] = current
        else:
            if (
                None not in (stalled["addr"], current["addr"])
                and current["addr"] == stalled["addr"]
            ):
                _mark(recorder, 1, cycle, evidence)
            if (
                None not in (stalled["mem_back_type_mm"], current["mem_back_type_mm"])
                and current["mem_back_type_mm"] == stalled["mem_back_type_mm"]
            ):
                _mark(recorder, 2, cycle, evidence)
            if (
                None not in (stalled["mem_page_type_nc"], current["mem_page_type_nc"])
                and current["mem_page_type_nc"] == stalled["mem_page_type_nc"]
            ):
                _mark(recorder, 3, cycle, evidence)
    elif tl_a_fire or not tl_a_valid:
        state["stalled_a"] = None

    if tl_a_fire and not entry_resending:
        accepted_request = state["latest_accepted_request"]
        if accepted_request is not None:
            state["active_instruncache_request"] = {
                **accepted_request,
                "tl_a_addr": s["tl_a_addr"],
                "d_seen": False,
            }

    active_request = state["active_instruncache_request"]
    if (
        s["backend_redirect"] == 1
        and active_request is not None
        and not active_request["d_seen"]
        and state["redirected_wait_d_pending"] is None
    ):
        state["redirected_wait_d_pending"] = {
            **active_request,
            "saw_d_after_redirect": False,
            "saw_instr_response_after_redirect": False,
            "old_identity_delivered": False,
        }

    if (
        state["previous_tl_a_fire"]
        and entry_state == _ENTRY_REFILL_RESP
        and not tl_a_valid
    ):
        _mark(recorder, 4, cycle, evidence)

    if tl_d_valid:
        if active_request is not None:
            active_request["d_seen"] = True
        redirected_wait_d = state["redirected_wait_d_pending"]
        if redirected_wait_d is not None:
            redirected_wait_d["saw_d_after_redirect"] = True
        state["d_response_pending"] = {
            "data": s["tl_d_data"],
            "corrupt": s["tl_d_corrupt"],
            "denied": s["tl_d_denied"],
            "entry_addr": entry_addr,
            "resending": entry_resending,
            "page_tail": page_tail,
            "beat_tail": beat_tail,
            "expected_data": _selected_word(s["tl_d_data"], entry_addr),
        }
        if (
            page_tail
            and not entry_resending
            and response_is_rvi
            and s["tl_d_corrupt"] == 1
        ):
            state["first_page_tl_fault_pending"] = {
                "entry_addr": entry_addr,
                "expected_exception": 3 if s["tl_d_denied"] == 1 else 5,
                "s2_pc": s["s2_pc"],
                "s2_instr_pc": s["s2_instr_pc"],
                "s2_ftq_flag": s["s2_ftq_flag"],
                "s2_ftq_value": s["s2_ftq_value"],
                "saw_instr_response": False,
                "old_path_external_request": False,
            }

    pending_d = state["d_response_pending"]
    if s["instr_resp_valid"] == 1 and pending_d is not None:
        redirected_wait_d = state["redirected_wait_d_pending"]
        if redirected_wait_d is not None and redirected_wait_d["saw_d_after_redirect"]:
            redirected_wait_d["saw_instr_response_after_redirect"] = True
        response_data_matches = (
            s["instr_resp_data"] is not None
            and pending_d["expected_data"] is not None
            and int(s["instr_resp_data"]) == int(pending_d["expected_data"])
        )
        if response_data_matches:
            _mark(
                recorder, 5, cycle, {**evidence, "response_data": s["instr_resp_data"]}
            )
        if pending_d["denied"] == 1 and s["instr_resp_denied"] == 1:
            _mark(recorder, 6, cycle, evidence)
        if pending_d["corrupt"] == 1 and s["instr_resp_corrupt"] == 1:
            _mark(recorder, 7, cycle, evidence)
        if s["instr_resp_need_resend"] == 1:
            _mark(recorder, 8, cycle, evidence)
        if (
            pending_d["beat_tail"]
            and not pending_d["resending"]
            and pending_d["corrupt"] == 1
            and pending_d["denied"] == 0
            and s["instr_resp_corrupt"] == 1
            and s["instr_resp_denied"] == 0
            and not entry_resending
            and s["instr_resp_need_resend"] == 0
        ):
            _mark(recorder, 15, cycle, evidence)
        if (
            pending_d["beat_tail"]
            and not pending_d["resending"]
            and pending_d["corrupt"] == 1
            and pending_d["denied"] == 1
            and s["instr_resp_corrupt"] == 1
            and s["instr_resp_denied"] == 1
            and not entry_resending
            and s["instr_resp_need_resend"] == 0
        ):
            _mark(recorder, 16, cycle, evidence)
        if (
            pending_d["resending"]
            and pending_d["corrupt"] == 1
            and s["instr_resp_corrupt"] == 1
            and s["instr_resp_denied"] == pending_d["denied"]
        ):
            _mark(recorder, 20 if pending_d["denied"] == 1 else 19, cycle, evidence)
        cross_8b = state["cross_8b_pending"]
        if (
            pending_d["resending"]
            and pending_d["corrupt"] == 0
            and pending_d["denied"] == 0
            and cross_8b is not None
            and cross_8b.get("expected_data") is not None
            and s["instr_resp_data"] is not None
            and int(s["instr_resp_data"]) == int(cross_8b["expected_data"])
        ):
            _mark(
                recorder, 18, cycle, {**evidence, "stitched_data": s["instr_resp_data"]}
            )
            if not entry_resending and s["instr_resp_need_resend"] == 0:
                _mark(
                    recorder,
                    21,
                    cycle,
                    {
                        **evidence,
                        "single_instr_response": True,
                        "resending_cleared": True,
                    },
                )
        if (
            not pending_d["beat_tail"]
            and not pending_d["resending"]
            and pending_d["corrupt"] == 0
            and pending_d["denied"] == 0
            and _response_is_rvi(pending_d["data"], pending_d["entry_addr"])
            and s["instr_resp_need_resend"] == 0
        ):
            offset = int(pending_d["entry_addr"]) & 0x3
            if offset in {0, 1, 2}:
                state["non_cross_rvi_offsets"].add(offset)
            if state["non_cross_rvi_offsets"] == {0, 1, 2}:
                _mark(
                    recorder,
                    13,
                    cycle,
                    {**evidence, "observed_offsets": [0, 2, 4]},
                )
        if (
            pending_d["beat_tail"]
            and not pending_d["resending"]
            and pending_d["corrupt"] == 0
            and pending_d["denied"] == 0
            and _response_is_rvc(pending_d["data"], pending_d["entry_addr"])
            and s["instr_resp_need_resend"] == 0
        ):
            _mark(recorder, 14, cycle, evidence)
        if pending_d["page_tail"] and s["instr_resp_need_resend"] == 1:
            state["cross_page_pending"] = {
                "entry_addr": pending_d["entry_addr"],
                "data": s["instr_resp_data"],
                "saw_internal_resend": False,
                "saw_half_redirect": False,
                "saw_recovery_request": False,
                "redirect_half_pc": None,
                "redirect_half_data": None,
                "recovery_entry_addr": None,
                "flush_pending": None,
            }
            _mark(recorder, 22, cycle, evidence)
        if (
            pending_d["page_tail"]
            and s["instr_resp_need_resend"] == 0
            and pending_d["corrupt"] == 0
            and pending_d["denied"] == 0
            and _response_is_rvc(pending_d["data"], pending_d["entry_addr"])
        ):
            _mark(recorder, 24, cycle, evidence)
        if (
            pending_d["page_tail"]
            and pending_d["corrupt"] == 1
            and s["instr_resp_corrupt"] == 1
            and s["instr_resp_denied"] == pending_d["denied"]
            and s["instr_resp_need_resend"] == 0
        ):
            _mark(recorder, 25, cycle, evidence)
        first_page_fault = state["first_page_tl_fault_pending"]
        if (
            first_page_fault is not None
            and pending_d["entry_addr"] == first_page_fault["entry_addr"]
            and s["instr_resp_corrupt"] == 1
            and s["instr_resp_denied"] == pending_d["denied"]
            and s["instr_resp_need_resend"] == 0
        ):
            first_page_fault["saw_instr_response"] = True
        state["d_response_pending"] = None
        state["active_instruncache_request"] = None

    redirected_wait_d = state["redirected_wait_d_pending"]
    if (
        redirected_wait_d is not None
        and s["to_valid"] == 1
        and s["to_ready"] == 1
        and single_delivery
    ):
        old_identity = (
            redirected_wait_d["s2_ftq_flag"],
            redirected_wait_d["s2_ftq_value"],
            redirected_wait_d["s2_instr_pc"],
        )
        delivered_identity = (s["to_ftq_flag"], s["to_ftq_value"], s["to_pc"])
        identity_observable = None not in (*old_identity, *delivered_identity)
        if identity_observable and delivered_identity == old_identity:
            redirected_wait_d["old_identity_delivered"] = True
        elif (
            identity_observable
            and redirected_wait_d["saw_instr_response_after_redirect"]
            and not redirected_wait_d["old_identity_delivered"]
            and delivered_identity != old_identity
            and s["to_exception"] == 0
        ):
            _mark(
                recorder,
                10,
                cycle,
                {
                    **evidence,
                    "redirect_before_d": True,
                    "d_completed_after_redirect": True,
                    "instr_response_completed_after_redirect": True,
                    "old_identity_delivery_suppressed": True,
                    "old_identity": old_identity,
                    "recovery_identity": delivered_identity,
                },
            )
            state["redirected_wait_d_pending"] = None

    first_page_fault = state["first_page_tl_fault_pending"]
    if first_page_fault is not None:
        if tl_a_valid or s["to_uncache_valid"] == 1:
            first_page_fault["old_path_external_request"] = True
        if s["to_valid"] == 1 and s["to_ready"] == 1:
            expected_end_offset = (
                None
                if None
                in (first_page_fault["s2_pc"], first_page_fault["s2_instr_pc"])
                else int(first_page_fault["s2_instr_pc"])
                - int(first_page_fault["s2_pc"])
                + 1
            )
            expected_foldpc = (
                None
                if first_page_fault["s2_instr_pc"] is None
                else fold_pc(int(first_page_fault["s2_instr_pc"]) << 1)
            )
            identity_matches = (
                None
                not in (
                    expected_end_offset,
                    expected_foldpc,
                    first_page_fault["s2_instr_pc"],
                    first_page_fault["s2_ftq_flag"],
                    first_page_fault["s2_ftq_value"],
                    s["to_pc"],
                    s["to_ftq_flag"],
                    s["to_ftq_value"],
                    s["to_ftq_offset"],
                    s["to_foldpc"],
                )
                and int(s["to_pc"]) == int(first_page_fault["s2_instr_pc"])
                and s["to_ftq_flag"] == first_page_fault["s2_ftq_flag"]
                and s["to_ftq_value"] == first_page_fault["s2_ftq_value"]
                and int(s["to_ftq_offset"]) == expected_end_offset
                and int(s["to_foldpc"]) == expected_foldpc
            )
            if (
                first_page_fault["saw_instr_response"]
                and not first_page_fault["old_path_external_request"]
                and identity_matches
                and single_delivery
                and s["to_exception"] == first_page_fault["expected_exception"]
                and s["to_exception"] != 4
                and s["to_exception_cross_page"] == 0
                and s["prev_end_half"] == 0
            ):
                _mark(
                    recorder,
                    35,
                    cycle,
                    {
                        **evidence,
                        "first_page_tl_fault": True,
                        "expected_exception": first_page_fault[
                            "expected_exception"
                        ],
                        "need_resend_suppressed": True,
                        "no_second_page_request": True,
                        "illegal_instruction": False,
                        "ftq_pc_identity_matches": True,
                    },
                )
            state["first_page_tl_fault_pending"] = None

    if (
        clean_d
        and beat_tail
        and not page_tail
        and not entry_resending
        and response_is_rvi
    ):
        identity_source = active_request or state["latest_accepted_request"]
        old_identity = None
        if identity_source is not None:
            candidate_identity = (
                identity_source.get("s2_ftq_flag"),
                identity_source.get("s2_ftq_value"),
                identity_source.get("s2_instr_pc"),
            )
            if None not in candidate_identity:
                old_identity = candidate_identity
        state["cross_8b_pending"] = {
            "entry_addr": entry_addr,
            "first_half": _response_half(s["tl_d_data"], entry_addr),
            "second_a": False,
            "second_a_fire": False,
            "second_d": False,
            "expected_data": None,
            "old_identity": old_identity,
        }

    cross_8b = state["cross_8b_pending"]
    if cross_8b is not None:
        if entry_resending and tl_a_valid:
            expected_addr = ((int(cross_8b["entry_addr"]) >> 2) + 1) << 3
            if s["tl_a_addr"] is not None and int(s["tl_a_addr"]) == expected_addr:
                cross_8b["second_a"] = True
                cross_8b["second_a_fire"] |= tl_a_fire
                _mark(
                    recorder,
                    12,
                    cycle,
                    {**evidence, "expected_second_addr": expected_addr},
                )
                _mark(
                    recorder,
                    17,
                    cycle,
                    {**evidence, "expected_second_addr": expected_addr},
                )
        if tl_d_valid and entry_resending:
            cross_8b["second_d"] = True
            if clean_d:
                second_half = int(s["tl_d_data"]) & 0xFFFF
                cross_8b["expected_data"] = int(cross_8b["first_half"]) | (
                    second_half << 16
                )
        if cross_8b["second_d"] and not entry_resending and s["instr_resp_valid"] == 1:
            state["cross_8b_pending"] = None

    redirect_kind = None
    if s["backend_redirect"] == 1:
        redirect_kind = "backend"
    elif s["checker_redirect"] == 1 and s["wb_redirect"] == 1:
        redirect_kind = "checker"
    if (
        redirect_kind is not None
        and state["redirected_resend_pending"] is None
        and cross_8b is not None
        and cross_8b["second_a_fire"]
        and not cross_8b["second_d"]
        and cross_8b["old_identity"] is not None
        and entry_resending
        and entry_state == _ENTRY_REFILL_RESP
    ):
        state["redirected_resend_pending"] = {
            "redirect_cycle": int(cycle),
            "redirect_kind": redirect_kind,
            "old_identity": cross_8b["old_identity"],
            "first_entry_addr": cross_8b["entry_addr"],
            "expected_second_addr": ((int(cross_8b["entry_addr"]) >> 2) + 1)
            << 3,
            "saw_second_a_fire": True,
            "saw_second_d": False,
            "saw_instr_response": False,
            "resending_cleared": False,
            "expected_data": None,
        }

    redirected_resend = state["redirected_resend_pending"]
    if redirected_resend is not None:
        elapsed = int(cycle) - int(redirected_resend["redirect_cycle"])
        delivered_identity = None
        if (
            s["to_valid"] == 1
            and s["to_ready"] == 1
            and single_delivery
            and None not in (s["to_ftq_flag"], s["to_ftq_value"], s["to_pc"])
        ):
            delivered_identity = (
                s["to_ftq_flag"],
                s["to_ftq_value"],
                s["to_pc"],
            )
        if delivered_identity == redirected_resend["old_identity"]:
            _record_risk(
                recorder,
                "ifu_instruncache_redirected_resend_old_identity_leak",
                cycle,
                {
                    **evidence,
                    "redirect_kind": redirected_resend["redirect_kind"],
                    "redirect_cycle": redirected_resend["redirect_cycle"],
                    "old_identity": redirected_resend["old_identity"],
                },
            )
            state["redirected_resend_pending"] = None
        elif elapsed > 0:
            if tl_d_valid and entry_resending:
                if clean_d:
                    redirected_resend["saw_second_d"] = True
                    if cross_8b is not None:
                        redirected_resend["expected_data"] = cross_8b.get(
                            "expected_data"
                        )
                else:
                    _record_risk(
                        recorder,
                        "ifu_instruncache_redirected_resend_second_d_fault",
                        cycle,
                        {
                            **evidence,
                            "redirect_kind": redirected_resend["redirect_kind"],
                            "redirect_cycle": redirected_resend["redirect_cycle"],
                        },
                    )
                    state["redirected_resend_pending"] = None
            if (
                state["redirected_resend_pending"] is not None
                and redirected_resend["saw_second_d"]
                and s["instr_resp_valid"] == 1
            ):
                expected_data = redirected_resend["expected_data"]
                response_matches = (
                    expected_data is not None
                    and s["instr_resp_data"] is not None
                    and int(s["instr_resp_data"]) == int(expected_data)
                )
                if (
                    response_matches
                    and s["instr_resp_corrupt"] == 0
                    and s["instr_resp_denied"] == 0
                    and s["instr_resp_need_resend"] == 0
                    and not entry_resending
                ):
                    redirected_resend["saw_instr_response"] = True
                    redirected_resend["resending_cleared"] = True
            if (
                state["redirected_resend_pending"] is not None
                and redirected_resend["saw_second_d"]
                and redirected_resend["saw_instr_response"]
                and redirected_resend["resending_cleared"]
                and delivered_identity is not None
                and delivered_identity != redirected_resend["old_identity"]
                and s["to_exception"] == 0
            ):
                _mark(
                    recorder,
                    11,
                    cycle,
                    {
                        **evidence,
                        "redirect_kind": redirected_resend["redirect_kind"],
                        "redirect_cycle": redirected_resend["redirect_cycle"],
                        "first_entry_addr": redirected_resend["first_entry_addr"],
                        "expected_second_addr": redirected_resend[
                            "expected_second_addr"
                        ],
                        "second_a_fire_before_redirect": True,
                        "second_d_after_redirect": True,
                        "instr_response_after_redirect": True,
                        "resending_cleared": True,
                        "old_identity_delivery_suppressed": True,
                        "old_identity": redirected_resend["old_identity"],
                        "recovery_identity": delivered_identity,
                    },
                )
                state["redirected_resend_pending"] = None
        if (
            state["redirected_resend_pending"] is not None
            and elapsed > _REDIRECTED_RESEND_TIMEOUT_CYCLES
        ):
            _record_risk(
                recorder,
                "ifu_instruncache_redirected_resend_timeout",
                cycle,
                {
                    "redirect_kind": redirected_resend["redirect_kind"],
                    "redirect_cycle": redirected_resend["redirect_cycle"],
                    "old_identity": redirected_resend["old_identity"],
                    "saw_second_d": redirected_resend["saw_second_d"],
                    "saw_instr_response": redirected_resend["saw_instr_response"],
                },
            )
            state["redirected_resend_pending"] = None

    cross_page = state["cross_page_pending"]
    if cross_page is not None:
        if entry_resending or (tl_a_valid and entry_state == _ENTRY_REFILL_REQ):
            cross_page["saw_internal_resend"] = True
        if entry_state == _ENTRY_IDLE and not cross_page["saw_internal_resend"]:
            _mark(recorder, 23, cycle, evidence)
        if (
            s["uncache_redirect"] == 1
            and s["resp_need_resend"] == 1
            and s["uncache_pc"] is not None
            and s["resp_data"] is not None
        ):
            cross_page["saw_half_redirect"] = True
            cross_page["redirect_half_pc"] = int(s["uncache_pc"])
            cross_page["redirect_half_data"] = int(s["resp_data"]) & 0xFFFF
        if (
            cross_page["saw_half_redirect"]
            and s["prev_end_half"] == 1
            and s["prev_half_data"] is not None
            and s["prev_half_pc"] is not None
            and int(s["prev_half_data"]) == cross_page["redirect_half_data"]
            and int(s["prev_half_pc"]) == cross_page["redirect_half_pc"]
        ):
            _mark(
                recorder,
                26,
                cycle,
                {
                    **evidence,
                    "redirect_half_pc": cross_page["redirect_half_pc"],
                    "redirect_half_data": cross_page["redirect_half_data"],
                },
            )
        if (
            cross_page["saw_half_redirect"]
            and s["prev_end_half"] == 1
            and s["to_uncache_valid"] == 1
            and s["to_uncache_ready"] == 1
            and s["prev_half_data"] is not None
            and s["prev_half_pc"] is not None
            and int(s["prev_half_data"]) == cross_page["redirect_half_data"]
            and int(s["prev_half_pc"]) == cross_page["redirect_half_pc"]
        ):
            cross_page["saw_recovery_request"] = True
            cross_page["recovery_entry_addr"] = s["to_uncache_addr"]
            _mark(recorder, 27, cycle, evidence)
        if (
            s["backend_redirect"] == 1
            and cross_page["saw_half_redirect"]
            and cross_page["saw_recovery_request"]
            and entry_state == _ENTRY_REFILL_RESP
            and cross_page["flush_pending"] is None
        ):
            cross_page["flush_pending"] = {
                "old_pc": cross_page["redirect_half_pc"],
                "old_entry_addr": (
                    entry_addr
                    if entry_addr is not None
                    else cross_page["recovery_entry_addr"]
                ),
                "saw_clear": False,
                "old_response_complete": False,
                "old_delivery": False,
            }
        flush_pending = cross_page["flush_pending"]
        if flush_pending is not None:
            if (
                s["to_valid"] == 1
                and s["to_ready"] == 1
                and s["to_pc"] is not None
                and int(s["to_pc"]) == flush_pending["old_pc"]
            ):
                flush_pending["old_delivery"] = True
            if s["prev_end_half"] == 0 and s["s2_valid"] != 1:
                flush_pending["saw_clear"] = True
            if (
                s["instr_resp_valid"] == 1
                and entry_addr is not None
                and flush_pending["old_entry_addr"] is not None
                and int(entry_addr) == int(flush_pending["old_entry_addr"])
            ):
                flush_pending["old_response_complete"] = True
            if (
                flush_pending["saw_clear"]
                and flush_pending["old_response_complete"]
                and not flush_pending["old_delivery"]
            ):
                _mark(
                    recorder,
                    29,
                    cycle,
                    {
                        **evidence,
                        "old_pc": flush_pending["old_pc"],
                        "old_entry_addr": flush_pending["old_entry_addr"],
                        "half_state_cleared": True,
                        "old_response_completed": True,
                        "old_delivery": False,
                    },
                )
                state["cross_page_pending"] = None
                cross_page = None
        if cross_page is not None and (
            cross_page["saw_recovery_request"]
            and s["to_valid"] == 1
            and s["to_ready"] == 1
            and single_delivery
            and s["to_exception"] in {0, None}
            and s["to_pc"] is not None
            and int(s["to_pc"]) == cross_page["redirect_half_pc"]
            and s["s2_uncache_data"] is not None
            and (int(s["s2_uncache_data"]) & 0x3) == 0x3
            and (int(s["s2_uncache_data"]) & 0xFFFF)
            == cross_page["redirect_half_data"]
        ):
            _mark(recorder, 28, cycle, evidence)
            state["cross_page_pending"] = None
        if cross_page is not None and (
            s["to_valid"] == 1
            and s["to_ready"] == 1
            and s["to_exception_cross_page"] == 1
            and s["to_exception"] in {1, 2, 3}
            and s["prev_end_half"] == 1
        ):
            expected_foldpc = fold_pc(int(cross_page["redirect_half_pc"]) << 1)
            identity_matches = (
                None
                not in (
                    s["s2_ftq_flag"],
                    s["s2_ftq_value"],
                    s["to_ftq_flag"],
                    s["to_ftq_value"],
                    s["to_ftq_offset"],
                    s["to_foldpc"],
                )
                and s["to_ftq_flag"] == s["s2_ftq_flag"]
                and s["to_ftq_value"] == s["s2_ftq_value"]
                and int(s["to_ftq_offset"]) == 0
                and int(s["to_foldpc"]) == expected_foldpc
                and s["to_uncache_valid"] != 1
                and s["tl_a_valid"] != 1
                and single_delivery
            )
            if identity_matches:
                exception_type = int(s["to_exception"])
                state["second_page_exception_types"].add(exception_type)
                exception_evidence = {
                    **evidence,
                    "original_pc": int(cross_page["redirect_half_pc"]) << 1,
                    "expected_foldpc": expected_foldpc,
                    "exception_type": exception_type,
                    "cross_page_fix": True,
                    "ftq_identity_matches": True,
                    "no_second_page_instruncache_request": True,
                    "no_second_page_tl_a_request": True,
                }
                if exception_type == 3:
                    _mark(recorder, 33, cycle, exception_evidence)
                if state["second_page_exception_types"] == {1, 2, 3}:
                    _mark(
                        recorder,
                        34,
                        cycle,
                        {
                            **exception_evidence,
                            "observed_exception_types": [1, 2, 3],
                        },
                    )
            state["cross_page_pending"] = None

    first_page_iaf = (
        s["s2_valid"] == 1
        and s["s2_req_uncache"] == 1
        and s["s2_use_uncache"] == 0
        and s["s2_exception"] == 3
        and s["s2_instr_pc"] is not None
        and (int(s["s2_instr_pc"]) & 0x7FF) == 0x7FF
        and s["to_valid"] == 1
        and s["to_ready"] == 1
        and single_delivery
        and s["to_exception"] == 3
        and s["prev_end_half"] == 0
        and s["to_uncache_valid"] != 1
        and s["tl_a_valid"] != 1
    )
    if first_page_iaf:
        first_page_pc = int(s["s2_instr_pc"]) << 1
        expected_foldpc = fold_pc(first_page_pc)
        expected_ftq_offset = (
            None
            if s["s2_pc"] is None
            else int(s["s2_instr_pc"]) - int(s["s2_pc"])
        )
        identity_matches = (
            None
            not in (
                expected_ftq_offset,
                s["s2_ftq_flag"],
                s["s2_ftq_value"],
                s["to_ftq_flag"],
                s["to_ftq_value"],
                s["to_ftq_offset"],
                s["to_foldpc"],
            )
            and s["to_ftq_flag"] == s["s2_ftq_flag"]
            and s["to_ftq_value"] == s["s2_ftq_value"]
            and int(s["to_ftq_offset"]) == expected_ftq_offset
            and int(s["to_foldpc"]) == expected_foldpc
        )
        _mark(
            recorder,
            30,
            cycle,
            {
                **evidence,
                "first_page_pc": first_page_pc,
                "no_instruncache_request": True,
                "no_tl_a_request": True,
                "single_exception_delivery": True,
            },
        )
        if identity_matches:
            _mark(
                recorder,
                31,
                cycle,
                {
                    **evidence,
                    "first_page_pc": first_page_pc,
                    "expected_foldpc": expected_foldpc,
                    "expected_ftq_offset": expected_ftq_offset,
                    "ftq_identity_matches": True,
                },
            )
        if s["to_exception_cross_page"] == 0:
            _mark(
                recorder,
                32,
                cycle,
                {
                    **evidence,
                    "first_page_pc": first_page_pc,
                    "mcause": "instruction_access_fault",
                    "second_page_request_suppressed": True,
                    "illegal_instruction": False,
                },
            )

    if tl_a_valid:
        accepted_attributes = state["accepted_request_attributes"]
        attr_pairs = (
            (
                36,
                "mem_back_type_mm",
                None
                if accepted_attributes is None
                else accepted_attributes["mem_back_type_mm"],
                s["tl_a_mem_back_type_mm"],
            ),
            (
                37,
                "mem_page_type_nc",
                None
                if accepted_attributes is None
                else accepted_attributes["mem_page_type_nc"],
                s["tl_a_mem_page_type_nc"],
            ),
        )
        for index, name, expected, observed in attr_pairs:
            if (
                expected is not None
                and observed is not None
                and int(expected) == int(observed)
            ):
                _mark(
                    recorder,
                    index,
                    cycle,
                    {
                        **evidence,
                        "attribute": name,
                        "accepted_request_attributes": dict(accepted_attributes),
                    },
                )
        expected_mem_back_type_mm = attr_pairs[0][2]
        expected_mem_page_type_nc = attr_pairs[1][2]
        if expected_mem_page_type_nc == 1 and expected_mem_back_type_mm == 1:
            state["attribute_modes"].add("nc")
        if expected_mem_page_type_nc == 0 and expected_mem_back_type_mm == 0:
            state["attribute_modes"].add("mmio")

    attribute_transactions = state["attribute_transactions"]
    if tl_a_fire and not entry_resending:
        pending_a = next(
            (
                transaction
                for transaction in attribute_transactions
                if not transaction["tl_a_fire"]
            ),
            None,
        )
        if pending_a is not None:
            pending_a["tl_a_fire"] = True
            pending_a["tl_a_addr"] = s["tl_a_addr"]
    if s["instr_resp_valid"] == 1:
        pending_response = next(
            (
                transaction
                for transaction in attribute_transactions
                if transaction["tl_a_fire"] and not transaction["response_seen"]
            ),
            None,
        )
        if pending_response is not None:
            pending_response["response_seen"] = True
    if s["to_valid"] == 1 and s["to_ready"] == 1 and single_delivery:
        completed = next(
            (
                transaction
                for transaction in attribute_transactions
                if transaction["response_seen"]
                and None
                not in (
                    transaction["s2_ftq_flag"],
                    transaction["s2_ftq_value"],
                    transaction["s2_instr_pc"],
                    s["to_ftq_flag"],
                    s["to_ftq_value"],
                    s["to_pc"],
                )
                and transaction["s2_ftq_flag"] == s["to_ftq_flag"]
                and transaction["s2_ftq_value"] == s["to_ftq_value"]
                and transaction["s2_instr_pc"] == s["to_pc"]
                and s["to_exception"] in {0, None}
            ),
            None,
        )
        if completed is not None:
            completed_record = dict(completed)
            completed_record["to_ftq_flag"] = s["to_ftq_flag"]
            completed_record["to_ftq_value"] = s["to_ftq_value"]
            completed_record["to_pc"] = s["to_pc"]
            state["completed_attribute_transactions"].append(completed_record)
            attribute_transactions.remove(completed)
            recent = state["completed_attribute_transactions"][-2:]
            if (
                len(recent) == 2
                and recent[0]["mode"] != recent[1]["mode"]
                and {transaction["mode"] for transaction in recent}
                == {"mmio", "nc"}
            ):
                _mark(
                    recorder,
                    38,
                    cycle,
                    {
                        **evidence,
                        "ordered_completed_transactions": recent,
                        "distinct_ifu_identities": True,
                        "single_instruncache_transaction_per_mode": True,
                    },
                )

    if s["backend_redirect"] == 1 or s["ifu_flush"] == 1:
        state["attribute_transactions"].clear()
        state["first_page_tl_fault_pending"] = None
    if s["ifu_flush"] == 1:
        state["active_instruncache_request"] = None

    state["previous_entry_state"] = (
        _ENTRY_IDLE if entry_state is None else int(entry_state)
    )
    state["previous_entry_resending"] = entry_resending
    state["previous_tl_a_fire"] = tl_a_fire


__all__ = [
    "INSTR_UNCACHE_OWNER_COVERPOINT",
    "INSTR_UNCACHE_OWNER_COVERPOINTS",
    "INSTR_UNCACHE_OWNER_GROUP",
    "INSTR_UNCACHE_OWNER_LEAF_COUNT",
    "INSTR_UNCACHE_OWNER_SAMPLER_BIN_KEYS",
    "initialize_instr_uncache_owner_coverage_state",
    "reset_instr_uncache_owner_coverage_state",
    "sample_instr_uncache_owner_coverage",
]

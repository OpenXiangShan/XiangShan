from __future__ import annotations

from typing import Any, Optional


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


def initialize_instr_uncache_owner_coverage_state(recorder) -> None:
    recorder._ifu_instr_uncache_owner_state = {
        "previous_entry_state": _ENTRY_IDLE,
        "previous_entry_resending": False,
        "previous_tl_a_fire": False,
        "stalled_a": None,
        "d_response_pending": None,
        "cross_8b_pending": None,
        "cross_page_pending": None,
        "non_cross_rvi_offsets": set(),
        "attribute_modes": set(),
        "accepted_request_attributes": None,
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
        state["accepted_request_attributes"] = {
            "mem_back_type_mm": int(s["req_is_mmio"] == 0),
            "mem_page_type_nc": int(s["req_pbmt"] == 1),
            "is_mmio": int(s["req_is_mmio"]),
            "pbmt": int(s["req_pbmt"]),
        }

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

    if (
        state["previous_tl_a_fire"]
        and entry_state == _ENTRY_REFILL_RESP
        and not tl_a_valid
    ):
        _mark(recorder, 4, cycle, evidence)

    if tl_d_valid:
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

    pending_d = state["d_response_pending"]
    if s["instr_resp_valid"] == 1 and pending_d is not None:
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
            and not entry_resending
            and s["instr_resp_need_resend"] == 0
        ):
            _mark(recorder, 15, cycle, evidence)
        if (
            pending_d["beat_tail"]
            and not pending_d["resending"]
            and pending_d["corrupt"] == 1
            and pending_d["denied"] == 1
            and not entry_resending
            and s["instr_resp_need_resend"] == 0
        ):
            _mark(recorder, 16, cycle, evidence)
        if pending_d["resending"] and pending_d["corrupt"] == 1:
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
            and (pending_d["corrupt"] == 1 or pending_d["denied"] == 1)
            and s["instr_resp_need_resend"] == 0
        ):
            _mark(recorder, 25, cycle, evidence)
        state["d_response_pending"] = None

    if (
        clean_d
        and beat_tail
        and not page_tail
        and not entry_resending
        and response_is_rvi
    ):
        state["cross_8b_pending"] = {
            "entry_addr": entry_addr,
            "first_half": _response_half(s["tl_d_data"], entry_addr),
            "second_a": False,
            "second_d": False,
            "expected_data": None,
        }

    cross_8b = state["cross_8b_pending"]
    if cross_8b is not None:
        if entry_resending and tl_a_valid:
            expected_addr = ((int(cross_8b["entry_addr"]) >> 2) + 1) << 3
            if s["tl_a_addr"] is not None and int(s["tl_a_addr"]) == expected_addr:
                cross_8b["second_a"] = True
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

    cross_page = state["cross_page_pending"]
    if cross_page is not None:
        if entry_resending or (tl_a_valid and entry_state == _ENTRY_REFILL_REQ):
            cross_page["saw_internal_resend"] = True
        if entry_state == _ENTRY_IDLE and not cross_page["saw_internal_resend"]:
            _mark(recorder, 23, cycle, evidence)
        if (
            s["uncache_redirect"] == 1
            and s["resp_need_resend"] == 1
            and s["prev_end_half"] == 1
        ):
            _mark(recorder, 26, cycle, evidence)
        if (
            s["prev_end_half"] == 1
            and s["to_uncache_valid"] == 1
            and s["to_uncache_ready"] == 1
            and s["prev_half_data"] is not None
            and s["prev_half_pc"] is not None
        ):
            _mark(recorder, 27, cycle, evidence)
        if (
            s["to_valid"] == 1
            and s["to_ready"] == 1
            and single_delivery
            and s["to_exception"] in {0, None}
            and s["prev_end_half"] == 1
            and s["to_is_rvc"] is not None
            and int(s["to_is_rvc"]) & int(s["to_enq"]) == 0
        ):
            _mark(recorder, 28, cycle, evidence)
            state["cross_page_pending"] = None
        if (
            s["to_valid"] == 1
            and s["to_ready"] == 1
            and s["to_exception_cross_page"] == 1
            and s["to_exception"] in {1, 2, 3}
            and s["prev_end_half"] == 1
        ):
            _mark(recorder, 35, cycle, evidence)

    first_page_iaf = (
        s["s2_valid"] == 1
        and s["s2_req_uncache"] == 1
        and s["s2_use_uncache"] == 0
        and s["s2_exception"] == 3
        and s["s2_instr_pc"] is not None
        and (int(s["s2_instr_pc"]) & 0x7FF) == 0x7FF
        and s["to_valid"] == 1
        and s["to_ready"] == 1
        and s["to_exception"] == 3
        and s["to_uncache_valid"] != 1
        and s["tl_a_valid"] != 1
    )
    if first_page_iaf:
        _mark(recorder, 30, cycle, {**evidence, "no_second_page_request": True})

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

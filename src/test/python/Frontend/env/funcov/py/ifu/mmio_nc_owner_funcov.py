from __future__ import annotations

from typing import Any, Optional

from .instr_uncache_owner_funcov import (
    initialize_instr_uncache_owner_coverage_state,
    sample_instr_uncache_owner_coverage,
)
from .owner_v3_funcov import mark_owner_v3_checked


MMIO_OWNER_GROUP = "ifu_mmio_owner_v3"
NC_OWNER_GROUP = "ifu_nc_owner_v3"
MMIO_NC_OWNER_COVERPOINT = "sv_equivalent_leaf"
MMIO_OWNER_LEAF_COUNT = 39
NC_OWNER_LEAF_COUNT = 39
MMIO_NC_OWNER_COVERPOINTS = {
    MMIO_OWNER_GROUP: MMIO_NC_OWNER_COVERPOINT,
    NC_OWNER_GROUP: MMIO_NC_OWNER_COVERPOINT,
}
MMIO_NC_OWNER_SAMPLER_BIN_KEYS = frozenset(
    {
        *((MMIO_OWNER_GROUP, f"mmio_leaf_{index:03d}") for index in range(1, 40)),
        *((NC_OWNER_GROUP, f"nc_leaf_{index:03d}") for index in range(1, 40)),
    }
)

_IDLE = 0
_WAIT_LAST_COMMIT = 1
_SEND_REQ = 2
_WAIT_RESP = 3
_ENTRY_REFILL_RESP = 2
_PBMT_NC = 1
_PBMT_IO = 2

_IFU_PREFIXES = (
    "Frontend_top.Frontend.inner_ifu.__Vtogcov__",
    "Frontend_top.Frontend.inner_ifu.",
    "Frontend_top.Frontend._inner_ifu_",
)
_UNCACHE_PREFIXES = (
    "uncacheUnit.",
    "Frontend_top.Frontend.inner_ifu.uncacheUnit.__Vtogcov__",
    "Frontend_top.Frontend.inner_ifu.uncacheUnit.",
    "Frontend_top.Frontend.inner_ifu._uncacheUnit_",
)
_INSTR_UNCACHE_PREFIXES = (
    "Frontend_top.Frontend.inner_instrUncache.__Vtogcov__",
    "Frontend_top.Frontend.inner_instrUncache.",
    "Frontend_top.Frontend._inner_instrUncache_",
)


def _read(recorder, dut, *names: str) -> Optional[int]:
    return recorder._read_first_dut_signal(dut, tuple(str(name) for name in names))


def _read_ifu(recorder, dut, stem: str) -> Optional[int]:
    return _read(recorder, dut, *(prefix + str(stem) for prefix in _IFU_PREFIXES))


def _read_uncache(recorder, dut, stem: str) -> Optional[int]:
    return _read(recorder, dut, *(prefix + str(stem) for prefix in _UNCACHE_PREFIXES))


def _read_instr_uncache(recorder, dut, stem: str) -> Optional[int]:
    return _read(
        recorder, dut, *(prefix + str(stem) for prefix in _INSTR_UNCACHE_PREFIXES)
    )


def _mark(
    recorder, group: str, index: int, cycle: int, evidence: dict[str, Any]
) -> None:
    prefix = "mmio" if group == MMIO_OWNER_GROUP else "nc"
    recorder.mark(
        group,
        f"{prefix}_leaf_{int(index):03d}",
        cycle,
        {"event": "ifu_mmio_nc_sv_equivalent", **evidence},
    )


def initialize_mmio_nc_owner_coverage_state(recorder) -> None:
    recorder._ifu_mmio_nc_owner_state = {
        "previous_uncache_state": _IDLE,
        "previous_tl_a_fire": False,
        "previous_backend_can_accept": False,
        "stalled_a": None,
        "mmio_active": False,
        "mmio_d_response_pending": None,
        "mmio_last_delivery": None,
        "mmio_cross_page_pending": False,
        "mmio_cross_8b_pending": False,
        "mmio_seen": set(),
        "nc_active": False,
        "nc_last_delivery": None,
        "nc_cross_page_pending": False,
        "nc_cross_8b_pending": False,
        "nc_redirect_pending": False,
        "nc_checker_redirect_pending": None,
        "nc_to_mmio_pending": False,
        "nc_seen": set(),
        "previous_path": None,
        "path_transition_observations": {},
    }
    initialize_instr_uncache_owner_coverage_state(recorder)


def reset_mmio_nc_owner_coverage_state(recorder) -> None:
    initialize_mmio_nc_owner_coverage_state(recorder)


def _snapshot(recorder, dut) -> dict[str, Optional[int]]:
    enq = _read_ifu(recorder, dut, "io_toIBuffer_bits_enqEnable")
    is_rvc_mask = 0
    is_rvc_available = True
    for slot in range(36):
        value = _read_ifu(recorder, dut, f"io_toIBuffer_bits_isRvc_{slot}")
        if value is None:
            is_rvc_available = False
            break
        is_rvc_mask |= (int(value) & 1) << slot

    active_slot = None
    active_to_pc = None
    active_to_ftq_flag = None
    active_to_ftq_value = None
    active_to_ftq_offset = None
    active_to_foldpc = None
    if enq is not None and int(enq) != 0:
        active_slot = (int(enq) & -int(enq)).bit_length() - 1
        active_to_pc = _read_ifu(
            recorder, dut, f"io_toIBuffer_bits_pc_{active_slot}_addr"
        )
        active_to_ftq_flag = _read_ifu(
            recorder, dut, f"io_toIBuffer_bits_ftqPtr_{active_slot}_flag"
        )
        active_to_ftq_value = _read_ifu(
            recorder, dut, f"io_toIBuffer_bits_ftqPtr_{active_slot}_value"
        )
        active_to_ftq_offset = _read_ifu(
            recorder, dut, f"io_toIBuffer_bits_instrEndOffset_{active_slot}_offset"
        )
        active_to_foldpc = _read_ifu(
            recorder, dut, f"io_toIBuffer_bits_foldpc_{active_slot}"
        )
    s2_align_shift = _read_ifu(recorder, dut, "s2_alignShiftNum")
    s2_instr_pc = (
        None
        if s2_align_shift is None
        else _read_ifu(
            recorder,
            dut,
            f"s2_alignedInstrPcVec_{int(s2_align_shift)}_addr",
        )
    )
    entry_state = _read(
        recorder,
        dut,
        "inner_instrUncache.entries_0.state",
        "Frontend_top.Frontend.inner_instrUncache.entries_0.state",
    )
    to_uncache_ready = _read(
        recorder,
        dut,
        "inner_ifu.io_toUncache_req_ready",
        "inner_instrUncache.entries_0.io_req_ready",
        "Frontend_top.Frontend.inner_instrUncache.entries_0.io_req_ready",
        "Frontend_top.Frontend.inner_ifu.io_toUncache_req_ready",
        *(prefix + "io_toUncache_req_ready" for prefix in _UNCACHE_PREFIXES),
    )
    # The Verilator offset exposes req_ready as a projection of the entry FSM,
    # so derive the same contract when the projection handle is unavailable.
    if to_uncache_ready is None and entry_state is not None:
        to_uncache_ready = int(entry_state == _IDLE)
    to_uncache_addr = _read(
        recorder,
        dut,
        "inner_ifu.io_toUncache_req_bits_addr_addr",
        "inner_instrUncache.entries_0.io_req_bits_addr_addr",
        "Frontend_top.Frontend.inner_ifu.io_toUncache_req_bits_addr_addr",
        *(prefix + "io_toUncache_req_bits_addr_addr" for prefix in _UNCACHE_PREFIXES),
        "Frontend_top.Frontend.inner_instrUncache.entries_0.io_req_bits_addr_addr",
    )
    if to_uncache_addr is None:
        to_uncache_addr = _read_uncache(recorder, dut, "uncachePAddr_addr")

    return {
        "s1_valid": _read_ifu(recorder, dut, "s1_valid"),
        "s1_flush": _read_ifu(recorder, dut, "s1_flush"),
        "s1_req_uncache": _read_ifu(recorder, dut, "s1_reqIsUncache"),
        "s1_pmp_mmio": _read_ifu(recorder, dut, "s1_icacheMetaIn_0_pmpMmio"),
        "s1_pbmt": _read_ifu(recorder, dut, "s1_icacheMetaIn_0_itlbPbmt"),
        "s1_paddr": _read_ifu(recorder, dut, "s1_icacheMetaIn_0_pAddr_addr"),
        "s1_pc": _read_ifu(recorder, dut, "s1_fetchBlock_0_startVAddr_addr"),
        "s1_ftq_flag": _read_ifu(recorder, dut, "s1_fetchBlock_0_ftqIdx_flag"),
        "s1_ftq_value": _read_ifu(recorder, dut, "s1_fetchBlock_0_ftqIdx_value"),
        "s2_valid": _read_ifu(recorder, dut, "s2_valid_valid"),
        "s2_req_uncache": _read_ifu(recorder, dut, "s2_reqIsUncache"),
        "s2_use_uncache": _read_ifu(recorder, dut, "s2_useUncacheFetch"),
        "s2_pmp_mmio": _read_ifu(recorder, dut, "s2_icacheMeta_0_pmpMmio"),
        "s2_pbmt": _read_ifu(recorder, dut, "s2_icacheMeta_0_itlbPbmt"),
        "s2_paddr": _read_ifu(recorder, dut, "s2_icacheMeta_0_pAddr_addr"),
        "s2_exception": _read_ifu(recorder, dut, "s2_icacheMeta_0_exception_value"),
        "s2_pc": _read_ifu(recorder, dut, "s2_fetchBlock_0_startVAddr_addr"),
        "s2_ftq_flag": _read_ifu(recorder, dut, "s2_fetchBlock_0_ftqIdx_flag"),
        "s2_ftq_value": _read_ifu(recorder, dut, "s2_fetchBlock_0_ftqIdx_value"),
        "s2_instr_pc": s2_instr_pc,
        "s2_uncache_data": _read_ifu(recorder, dut, "s2_uncacheData"),
        "is_first": _read_ifu(recorder, dut, "isFirstInstr"),
        "req_valid": _read_uncache(recorder, dut, "io_req_valid"),
        "req_ready": _read_uncache(recorder, dut, "io_req_ready"),
        "req_is_mmio": _read(
            recorder,
            dut,
            *(prefix + "io_req_bits_isMmio" for prefix in _UNCACHE_PREFIXES),
            *(prefix + "s2_icacheMeta_0_pmpMmio" for prefix in _IFU_PREFIXES),
        ),
        "req_pbmt": _read(
            recorder,
            dut,
            *(prefix + "io_req_bits_pbmt" for prefix in _UNCACHE_PREFIXES),
            *(prefix + "s2_icacheMeta_0_itlbPbmt" for prefix in _IFU_PREFIXES),
        ),
        "uncache_state": _read_uncache(recorder, dut, "uncacheState"),
        "ifu_stall": _read_uncache(recorder, dut, "io_ifuStall"),
        "to_uncache_valid": _read(
            recorder,
            dut,
            "inner_ifu.io_toUncache_req_valid",
            "inner_instrUncache.entries_0.io_req_valid",
            "Frontend_top.Frontend.inner_instrUncache.entries_0.io_req_valid",
            "Frontend_top.Frontend.inner_ifu.io_toUncache_req_valid",
            *(
                prefix + "io_toUncache_req_valid"
                for prefix in _UNCACHE_PREFIXES
            ),
        ),
        "to_uncache_ready": to_uncache_ready,
        "to_uncache_addr": to_uncache_addr,
        "empty_after": _read_uncache(recorder, dut, "io_emptyAfter"),
        "uncache_busy": _read_ifu(recorder, dut, "uncacheBusy"),
        "ibuffer_ready": _read_ifu(recorder, dut, "io_toIBuffer_ready"),
        "ibuffer_empty": _read(
            recorder,
            dut,
            "inner_ibuffer.io_empty",
            "Frontend_top.Frontend.inner_ibuffer.io_empty",
            "Frontend_top.Frontend._inner_ibuffer_io_empty",
        ),
        "backend_empty": _read(
            recorder,
            dut,
            "Frontend_top.io_backend_backendEmpty",
            "Frontend_top.Frontend.io_backend_backendEmpty",
        ),
        "backend_commit": _read(
            recorder,
            dut,
            "Frontend_top.io_backend_toFtq_commit_valid",
        ),
        "backend_accept": _read(
            recorder,
            dut,
            "Frontend_top.io_backend_toIBuf_decodeCanAccept",
        ),
        "tl_a_valid": _read(
            recorder, dut, "auto_inner_instrUncache_client_out_a_valid"
        ),
        "tl_a_ready": _read(
            recorder, dut, "auto_inner_instrUncache_client_out_a_ready"
        ),
        "tl_a_addr": _read(
            recorder, dut, "auto_inner_instrUncache_client_out_a_bits_address"
        ),
        "tl_d_valid": _read(
            recorder, dut, "auto_inner_instrUncache_client_out_d_valid"
        ),
        "tl_d_data": _read(
            recorder, dut, "auto_inner_instrUncache_client_out_d_bits_data"
        ),
        "tl_d_corrupt": _read(
            recorder, dut, "auto_inner_instrUncache_client_out_d_bits_corrupt"
        ),
        "tl_d_denied": _read(
            recorder, dut, "auto_inner_instrUncache_client_out_d_bits_denied"
        ),
        "instr_resp_valid": _read_instr_uncache(recorder, dut, "io_toIfu_resp_valid"),
        "instr_resp_data": _read_instr_uncache(
            recorder, dut, "io_toIfu_resp_bits_data"
        ),
        "instr_resp_corrupt": _read_instr_uncache(
            recorder, dut, "io_toIfu_resp_bits_corrupt"
        ),
        "instr_resp_denied": _read_instr_uncache(
            recorder, dut, "io_toIfu_resp_bits_denied"
        ),
        "instr_resp_need_resend": _read_instr_uncache(
            recorder, dut, "io_toIfu_resp_bits_needResend"
        ),
        "entry_state": entry_state,
        "entry_resending": _read(
            recorder,
            dut,
            "Frontend_top.Frontend.inner_instrUncache.entries_0.resending",
        ),
        "entry_req_addr": _read(
            recorder,
            dut,
            "Frontend_top.Frontend.inner_instrUncache.entries_0.reqReg_addr_addr",
        ),
        "entry_mem_back_type_mm": _read(
            recorder,
            dut,
            "Frontend_top.Frontend.inner_instrUncache.entries_0.reqReg_memBackTypeMM",
            "Frontend_top.Frontend.inner_instrUncache.entries_0.io_req_bits_memBackTypeMM",
        ),
        "entry_mem_page_type_nc": _read(
            recorder,
            dut,
            "Frontend_top.Frontend.inner_instrUncache.entries_0.reqReg_memPageTypeNC",
            "Frontend_top.Frontend.inner_instrUncache.entries_0.io_req_bits_memPageTypeNC",
        ),
        "tl_a_mem_back_type_mm": _read(
            recorder,
            dut,
            "auto_inner_instrUncache_client_out_a_bits_user_memBackType_MM",
            "auto_inner_instrUncache_client_out_a_bits_user_MemBackTypeMM",
            "auto_inner_instrUncache_client_out_a_bits_user_memBackTypeMM",
        ),
        "tl_a_mem_page_type_nc": _read(
            recorder,
            dut,
            "auto_inner_instrUncache_client_out_a_bits_user_memPageType_NC",
            "auto_inner_instrUncache_client_out_a_bits_user_MemPageTypeNC",
            "auto_inner_instrUncache_client_out_a_bits_user_memPageTypeNC",
        ),
        "resp_valid": _read_uncache(recorder, dut, "io_resp_valid"),
        "resp_data": _read_uncache(recorder, dut, "io_resp_bits_uncacheData"),
        "resp_exception": _read_uncache(recorder, dut, "io_resp_bits_exception_value"),
        "resp_need_resend": _read_uncache(recorder, dut, "io_resp_bits_needResend"),
        "to_valid": _read_ifu(recorder, dut, "io_toIBuffer_valid"),
        "to_ready": _read_ifu(recorder, dut, "io_toIBuffer_ready"),
        "to_enq": enq,
        "to_pc": active_to_pc,
        "to_ftq_flag": active_to_ftq_flag,
        "to_ftq_value": active_to_ftq_value,
        "to_ftq_offset": active_to_ftq_offset,
        "to_foldpc": active_to_foldpc,
        "to_is_rvc": is_rvc_mask if is_rvc_available else None,
        "to_exception": _read_ifu(
            recorder, dut, "io_toIBuffer_bits_exceptionType_value"
        ),
        "to_exception_cross_page": _read_ifu(
            recorder, dut, "io_toIBuffer_bits_exceptionCrossPage"
        ),
        "backend_redirect": _read(
            recorder,
            dut,
            "Frontend_top.Frontend.inner_ftq.backendRedirect_valid",
            "TOP.Frontend_top.Frontend.inner_ftq.backendRedirect_valid",
            "Frontend_top.io_backend_toFtq_redirect_valid",
            "io_backend_toFtq_redirect_valid",
        ),
        "ifu_flush": _read_ifu(recorder, dut, "s2_flush"),
        "s2_wb_not_flush": _read_ifu(recorder, dut, "s2_wbNotFlush"),
        "uncache_redirect": _read_ifu(recorder, dut, "uncacheRedirect_valid"),
        "wb_redirect": _read_ifu(recorder, dut, "io_toFtq_wbRedirect_valid"),
        "checker_redirect": _read_ifu(recorder, dut, "wbRedirect_valid"),
        "wb_path_valid": _read_ifu(recorder, dut, "wbValid"),
        "wb_ftq_flag": _read_ifu(recorder, dut, "wbAlignFetchBlock_0_ftqIdx_flag"),
        "wb_ftq_value": _read_ifu(recorder, dut, "wbAlignFetchBlock_0_ftqIdx_value"),
        "wb_pc": _read_ifu(recorder, dut, "wbAlignFetchBlock_0_startVAddr_addr"),
        "branch_type": _read_ifu(recorder, dut, "brAttribute_branchType"),
        "prev_end_half": _read_ifu(
            recorder, dut, "s2_prevEndIsHalfRviInfo_valid"
        ),
        "prev_half_data": _read_ifu(
            recorder, dut, "s2_prevEndIsHalfRviInfo_bits_data"
        ),
        "prev_half_pc": _read_ifu(
            recorder, dut, "s2_prevEndIsHalfRviInfo_bits_pc_addr"
        ),
        "uncache_pc": _read_ifu(recorder, dut, "uncachePc_addr"),
        "wfi_safe": _read(recorder, dut, "Frontend_top.io_backend_wfi_wfiSafe"),
        "wfi_req": _read(
            recorder,
            dut,
            "Frontend_top.io_backend_wfi_wfiReq",
            "Frontend_top.Frontend.io_backend_wfi_wfiReq",
            "io_backend_wfi_wfiReq",
        ),
        "waylookup_valid": _read(
            recorder,
            dut,
            "inner_icache.wayLookup.io_toMainPipe_valid",
            "Frontend_top.Frontend.inner_icache.wayLookup.io_toMainPipe_valid",
        ),
        "waymask_0": _read(
            recorder,
            dut,
            "inner_icache.wayLookup.io_toMainPipe_bits_wayLookupInfo_0_bits_entry_waymask_0",
        ),
        "waymask_1": _read(
            recorder,
            dut,
            "inner_icache.wayLookup.io_toMainPipe_bits_wayLookupInfo_0_bits_entry_waymask_1",
        ),
    }


def _sample_mmio(
    recorder, cycle: int, s: dict[str, Optional[int]], state: dict
) -> None:
    mmio_candidate = (
        s["s2_valid"] == 1
        and s["s2_req_uncache"] == 1
        and (s["s2_pmp_mmio"] == 1 or s["s2_pbmt"] != _PBMT_NC)
    )
    nc_candidate = (
        s["s2_valid"] == 1 and s["s2_pmp_mmio"] == 0 and s["s2_pbmt"] == _PBMT_NC
    )
    single_delivery = s["to_enq"] is not None and int(s["to_enq"]).bit_count() == 1
    delivery_data = (
        s["s2_uncache_data"]
        if s["s2_uncache_data"] is not None
        else s["resp_data"]
    )
    delivered_is_rvc = (
        single_delivery
        and delivery_data is not None
        and (int(delivery_data) & 0x3) != 0x3
    )
    if mmio_candidate:
        state["mmio_active"] = True
    mmio_active = bool(state["mmio_active"])
    mmio_delivery = (
        mmio_active and s["to_valid"] == 1 and s["to_ready"] == 1
    )
    state_value = s["uncache_state"]
    ifu_stall = s["ifu_stall"]
    if ifu_stall is None and s["ibuffer_ready"] is not None:
        ifu_stall = int(s["ibuffer_ready"] == 0)
    to_uncache_valid = s["to_uncache_valid"]
    empty_after = s["empty_after"]
    if (
        empty_after is None
        and s["backend_empty"] is not None
        and s["ibuffer_empty"] is not None
    ):
        empty_after = int(s["backend_empty"] == 1 and s["ibuffer_empty"] == 1)
    entry_wait_resp = s["entry_state"] == _ENTRY_REFILL_RESP
    page_tail = (
        s["entry_req_addr"] is not None
        and (int(s["entry_req_addr"]) & 0x7FF) == 0x7FF
    )
    beat_tail = (
        s["entry_req_addr"] is not None
        and (int(s["entry_req_addr"]) & 0x3) == 0x3
    )
    response_half = None
    if s["tl_d_data"] is not None and s["entry_req_addr"] is not None:
        shift = (int(s["entry_req_addr"]) & 0x3) * 16
        response_half = (int(s["tl_d_data"]) >> shift) & 0xFFFF
    response_is_rvi = response_half is not None and (response_half & 0x3) == 0x3
    pending = mmio_candidate or mmio_active
    cancel_pending_mmio = (
        mmio_active
        and state_value in {_WAIT_LAST_COMMIT, _WAIT_RESP}
        and (s["backend_redirect"] == 1 or s["ifu_flush"] == 1)
        and not mmio_delivery
    )
    evidence = {
        "s2_pc": s["s2_pc"],
        "state": state_value,
        "entry_state": s["entry_state"],
        "tl_a_addr": s["tl_a_addr"],
        "uncache_pc": s["uncache_pc"],
        "resp_data": s["resp_data"],
        "s2_uncache_data": s["s2_uncache_data"],
        "resp_valid": s["resp_valid"],
        "ifu_stall": ifu_stall,
        "to_uncache_valid": to_uncache_valid,
        "backend_redirect": s["backend_redirect"],
        "uncache_redirect": s["uncache_redirect"],
        "wb_redirect": s["wb_redirect"],
    }

    conditions = {
        1: mmio_candidate and s["ifu_flush"] == 0 and s["backend_redirect"] == 0,
        2: s["s2_valid"] == 1 and s["s2_pmp_mmio"] == 1 and s["s2_pbmt"] != _PBMT_NC,
        3: s["s2_valid"] == 1 and s["s2_pmp_mmio"] == 1 and s["s2_pbmt"] == _PBMT_NC,
        4: nc_candidate,
        5: mmio_candidate and s["s2_exception"] not in {None, 0},
        6: mmio_delivery and single_delivery,
        7: s["resp_valid"] == 1 and mmio_delivery and single_delivery,
        9: state_value == _IDLE and s["req_valid"] == 0,
        10: state["previous_uncache_state"] == _IDLE
        and state_value == _WAIT_LAST_COMMIT
        and mmio_active,
        11: state["previous_uncache_state"] == _IDLE
        and state_value == _SEND_REQ
        and nc_candidate,
        12: mmio_active
        and state["previous_uncache_state"] == _WAIT_LAST_COMMIT
        and state_value == _SEND_REQ
        and s["is_first"] == 1,
        13: mmio_active
        and state_value == _WAIT_LAST_COMMIT
        and s["is_first"] == 0
        and empty_after == 0,
        14: mmio_active
        and state_value == _WAIT_LAST_COMMIT
        and to_uncache_valid == 0,
        15: cancel_pending_mmio,
        16: mmio_active
        and state_value == _SEND_REQ
        and ifu_stall == 0
        and to_uncache_valid == 1,
        17: mmio_active
        and state_value == _SEND_REQ
        and ifu_stall == 1
        and to_uncache_valid == 0,
        18: mmio_active
        and state["previous_uncache_state"] == _SEND_REQ
        and state_value == _WAIT_RESP
        and s["req_ready"] == 0,
        19: mmio_active
        and state_value == _WAIT_RESP
        and s["instr_resp_valid"] != 1,
        21: mmio_active
        and (
            (
                s["resp_valid"] == 1
                and (s["ifu_flush"] == 1 or s["backend_redirect"] == 1)
            )
            or (s["backend_redirect"] == 1 and state_value == _WAIT_RESP)
        )
        and not mmio_delivery,
        22: mmio_active
        and page_tail
        and s["resp_valid"] == 1
        and s["resp_need_resend"] == 1
        and s["uncache_redirect"] == 1,
        23: mmio_active
        and s["uncache_redirect"] == 1
        and s["resp_need_resend"] == 1
        and s["uncache_pc"] is not None,
        24: mmio_active
        and s["uncache_redirect"] == 1
        and s["resp_need_resend"] == 1
        and s["resp_data"] is not None,
        25: mmio_active
        and s["prev_end_half"] == 1
        and s["req_valid"] == 1
        and state_value != _WAIT_LAST_COMMIT,
        27: mmio_active
        and page_tail
        and s["resp_valid"] == 1
        and s["resp_data"] is not None
        and (int(s["resp_data"]) & 0x3) != 0x3
        and s["resp_need_resend"] == 0,
        28: mmio_active
        and page_tail
        and s["resp_valid"] == 1
        and s["resp_data"] is not None
        and (int(s["resp_data"]) & 0x3) != 0x3
        and s["prev_end_half"] == 0,
        29: s["s2_valid"] == 1
        and s["s2_req_uncache"] == 1
        and s["s2_pmp_mmio"] == 1
        and s["s2_exception"] == 3
        and s["s2_use_uncache"] == 0,
        35: mmio_active
        and s["wb_path_valid"] == 1
        and s["wb_redirect"] == 1
        and s["s2_req_uncache"] == 1,
        36: mmio_delivery and s["wb_redirect"] == 1 and s["wb_path_valid"] == 0,
        37: mmio_active
        and s["backend_redirect"] == 1
        and s["resp_valid"] == 1
        and s["wb_redirect"] == 0,
        39: pending and s["wfi_safe"] == 0,
    }
    for index, condition in conditions.items():
        if condition:
            _mark(recorder, MMIO_OWNER_GROUP, index, cycle, evidence)

    seen = state["mmio_seen"]
    if mmio_candidate and s["s2_use_uncache"] == 1 and s["req_valid"] == 1:
        seen.add("mmio_gate")
    if nc_candidate and s["s2_use_uncache"] == 1 and s["req_valid"] == 1:
        seen.add("nc_gate")
    if {"mmio_gate", "nc_gate"}.issubset(seen):
        _mark(recorder, MMIO_OWNER_GROUP, 8, cycle, {**evidence, "seen": sorted(seen)})

    if mmio_active and s["instr_resp_valid"] == 1:
        state["mmio_d_response_pending"] = (
            "denied"
            if s["instr_resp_denied"] == 1
            else "corrupt" if s["instr_resp_corrupt"] == 1 else "clean"
        )
    if s["resp_valid"] == 1 and state["mmio_d_response_pending"] is not None:
        _mark(
            recorder,
            MMIO_OWNER_GROUP,
            20,
            cycle,
            {
                **evidence,
                "response_kind": state["mmio_d_response_pending"],
                "need_resend": s["resp_need_resend"],
            },
        )
        state["mmio_d_response_pending"] = None

    if (
        mmio_active
        and
        beat_tail
        and entry_wait_resp
        and s["entry_resending"] == 0
        and s["tl_d_valid"] == 1
        and response_is_rvi
        and s["tl_d_corrupt"] == 0
        and s["tl_d_denied"] == 0
    ):
        state["mmio_cross_8b_pending"] = True
        seen.add("cross_8b_first")
    if (
        state["mmio_cross_8b_pending"]
        and s["entry_resending"] == 1
        and s["tl_a_valid"] == 1
    ):
        seen.add("cross_8b_second_a")
    if (
        state["mmio_cross_8b_pending"]
        and s["entry_resending"] == 1
        and entry_wait_resp
        and s["tl_d_valid"] == 1
        and s["tl_d_corrupt"] == 0
        and s["tl_d_denied"] == 0
    ):
        seen.add("cross_8b_second_d")
    if (
        state["mmio_cross_8b_pending"]
        and s["resp_valid"] == 1
        and s["resp_need_resend"] == 0
        and mmio_delivery
        and not delivered_is_rvc
    ):
        seen.add("cross_8b_delivery")
    if {"cross_8b_first", "cross_8b_second_d", "cross_8b_delivery"}.issubset(seen):
        _mark(recorder, MMIO_OWNER_GROUP, 33, cycle, {**evidence, "seen": sorted(seen)})

    if (
        mmio_active
        and page_tail
        and s["resp_valid"] == 1
        and s["resp_need_resend"] == 1
    ):
        state["mmio_cross_page_pending"] = True
    if (
        state["mmio_cross_page_pending"]
        and mmio_delivery
        and not delivered_is_rvc
        and single_delivery
    ):
        _mark(recorder, MMIO_OWNER_GROUP, 26, cycle, evidence)
    if (
        state["mmio_cross_page_pending"]
        and s["to_valid"] == 1
        and s["to_exception_cross_page"] == 1
        and s["to_exception"] in {1, 2, 3}
    ):
        _mark(
            recorder,
            MMIO_OWNER_GROUP,
            30,
            cycle,
            {**evidence, "exception_kind": s["to_exception"]},
        )

    last_delivery = state["mmio_last_delivery"]
    if (
        mmio_delivery
        and single_delivery
        and last_delivery is not None
        and s["uncache_pc"] is not None
    ):
        expected_step = 1 if last_delivery["is_rvc"] else 2
        if int(s["uncache_pc"]) == int(last_delivery["pc"]) + expected_step:
            if delivered_is_rvc:
                _mark(recorder, MMIO_OWNER_GROUP, 31, cycle, evidence)
            else:
                _mark(recorder, MMIO_OWNER_GROUP, 32, cycle, evidence)
    if mmio_delivery and s["branch_type"] in {1, 2, 3}:
        cfi_kind = {1: "branch", 2: "jal_call", 3: "jalr_ret"}[int(s["branch_type"])]
        _mark(
            recorder,
            MMIO_OWNER_GROUP,
            34,
            cycle,
            {**evidence, "cfi_kind": cfi_kind},
        )
    if mmio_delivery and s["resp_exception"] in {3, 5}:
        _mark(
            recorder,
            MMIO_OWNER_GROUP,
            38,
            cycle,
            {
                **evidence,
                "error_kind": "denied" if s["resp_exception"] == 3 else "corrupt",
            },
        )

    if mmio_delivery and s["uncache_pc"] is not None:
        state["mmio_last_delivery"] = {
            "pc": int(s["uncache_pc"]),
            "is_rvc": bool(delivered_is_rvc),
        }
    if s["ifu_flush"] == 1 or s["backend_redirect"] == 1:
        state["mmio_active"] = False
        state["mmio_d_response_pending"] = None
        state["mmio_last_delivery"] = None
        state["mmio_cross_page_pending"] = False
        state["mmio_cross_8b_pending"] = False
    elif state["mmio_cross_page_pending"] and mmio_delivery:
        state["mmio_cross_page_pending"] = False
    if mmio_delivery and not mmio_candidate:
        state["mmio_active"] = False
    if state["mmio_cross_8b_pending"] and s["resp_valid"] == 1:
        state["mmio_cross_8b_pending"] = False


def _sample_nc(recorder, cycle: int, s: dict[str, Optional[int]], state: dict) -> None:
    nc_candidate = (
        s["s2_valid"] == 1
        and s["s2_req_uncache"] == 1
        and s["s2_pmp_mmio"] == 0
        and s["s2_pbmt"] == _PBMT_NC
    )
    mmio_candidate = s["s2_valid"] == 1 and (
        s["s2_pmp_mmio"] == 1 or s["s2_pbmt"] == _PBMT_IO
    )
    req_fire = s["req_valid"] == 1 and s["req_ready"] == 1
    nc_accept = nc_candidate and s["s2_use_uncache"] == 1 and req_fire
    nc_active = bool(state["nc_active"])
    single_delivery = s["to_enq"] is not None and int(s["to_enq"]).bit_count() == 1
    delivery_data = (
        s["s2_uncache_data"]
        if s["s2_uncache_data"] is not None
        else s["resp_data"]
    )
    delivered_is_rvc = (
        single_delivery
        and delivery_data is not None
        and (int(delivery_data) & 0x3) != 0x3
    )
    nc_delivery = nc_active and s["to_valid"] == 1 and s["to_ready"] == 1
    clean_delivery = nc_delivery and s["resp_exception"] == 0
    pending = nc_candidate or nc_active
    ifu_stall = s["ifu_stall"]
    if ifu_stall is None and s["ibuffer_ready"] is not None:
        ifu_stall = int(s["ibuffer_ready"] == 0)
    to_uncache_valid = s["to_uncache_valid"]
    to_uncache_fire = (
        s["to_uncache_valid"] == 1 and s["to_uncache_ready"] == 1
    )
    entry_wait_resp = s["entry_state"] == _ENTRY_REFILL_RESP
    tl_a_fire = s["tl_a_valid"] == 1 and s["tl_a_ready"] == 1
    page_tail = (
        s["entry_req_addr"] is not None
        and (int(s["entry_req_addr"]) & 0x7FF) == 0x7FF
    )
    s2_page_tail = (
        s["s2_instr_pc"] is not None
        and (int(s["s2_instr_pc"]) & 0x7FF) == 0x7FF
    )
    beat_tail = (
        s["entry_req_addr"] is not None
        and (int(s["entry_req_addr"]) & 0x3) == 0x3
    )
    response_half = None
    if s["tl_d_data"] is not None and s["entry_req_addr"] is not None:
        response_half = (
            int(s["tl_d_data"]) >> ((int(s["entry_req_addr"]) & 0x3) * 16)
        ) & 0xFFFF
    response_is_rvi = response_half is not None and (response_half & 0x3) == 0x3
    response_is_rvc = response_half is not None and (response_half & 0x3) != 0x3
    evidence = {
        "s2_pc": s["s2_pc"],
        "s2_instr_pc": s["s2_instr_pc"],
        "state": s["uncache_state"],
        "entry_state": s["entry_state"],
        "tl_a_addr": s["tl_a_addr"],
        "uncache_pc": s["uncache_pc"],
        "resp_data": s["resp_data"],
        "s2_uncache_data": s["s2_uncache_data"],
        "to_pc": s["to_pc"],
        "ifu_stall": ifu_stall,
        "to_uncache_valid": to_uncache_valid,
        "checker_redirect": s["checker_redirect"],
        "wb_redirect": s["wb_redirect"],
    }
    seen = state["nc_seen"]

    if nc_accept:
        _mark(recorder, NC_OWNER_GROUP, 1, cycle, evidence)
        mark_owner_v3_checked(
            recorder,
            "BIN-959",
            cycle,
            {
                **evidence,
                "pbmt": _PBMT_NC,
                "pmp_mmio": 0,
                "uncache_selected": True,
                "request_fired": True,
            },
            producer="ifu_nc_attribute_sampler",
        )
        waymask_known = (
            s["waylookup_valid"] == 1
            and s["waymask_0"] is not None
            and s["waymask_1"] is not None
        )
        if waymask_known:
            seen.add(
                "waymask_hit"
                if int(s["waymask_0"]) or int(s["waymask_1"])
                else "waymask_miss"
            )
        _mark(
            recorder,
            NC_OWNER_GROUP,
            3,
            cycle,
            {
                **evidence,
                "uncache_selected": True,
                "waymask_known": waymask_known,
            },
        )
    conditions = {
        2: (
            nc_candidate and s["s2_exception"] not in {None, 0}
        ) or (
            nc_delivery and s["to_exception"] in {1, 2, 3, 5}
        ),
        4: nc_active
        and state["previous_uncache_state"] == _IDLE
        and s["uncache_state"] == _SEND_REQ,
        5: nc_active
        and s["uncache_state"] == _SEND_REQ
        and (s["backend_empty"] == 0 or s["ibuffer_empty"] == 0),
        6: nc_active
        and s["uncache_state"] != _IDLE
        and s["s2_valid"] == 1
        and s["s2_use_uncache"] == 1
        and s["req_valid"] == 0,
        7: nc_active
        and s["uncache_state"] == _SEND_REQ
        and ifu_stall == 0
        and to_uncache_valid == 1,
        8: nc_active
        and s["uncache_state"] == _SEND_REQ
        and ifu_stall == 1
        and to_uncache_valid == 0,
        10: nc_active
        and state["previous_uncache_state"] == _SEND_REQ
        and s["uncache_state"] == _WAIT_RESP
        and s["req_ready"] == 0,
        14: nc_active
        and (
            (
                (s["ifu_flush"] == 1 or s["backend_redirect"] == 1)
                and s["resp_valid"] == 1
            )
            or (s["backend_redirect"] == 1 and s["uncache_state"] == _WAIT_RESP)
        ),
        15: pending and s["wfi_safe"] == 0,
        18: nc_delivery and s["backend_accept"] == 0,
        20: nc_delivery and s["branch_type"] == 0,
        21: nc_delivery and s["branch_type"] == 1,
        22: nc_delivery and s["branch_type"] in {2, 3},
        24: nc_active
        and beat_tail
        and entry_wait_resp
        and s["entry_resending"] == 0
        and s["tl_d_valid"] == 1
        and response_is_rvc
        and s["tl_d_corrupt"] == 0
        and s["tl_d_denied"] == 0,
        28: nc_active
        and page_tail
        and s["resp_valid"] == 1
        and s["resp_data"] is not None
        and (int(s["resp_data"]) & 0x3) != 0x3
        and s["resp_need_resend"] == 0,
        32: nc_candidate
        and s["s2_use_uncache"] == 0
        and s2_page_tail
        and s["s2_exception"] == 3
        and s["to_valid"] == 1
        and s["to_ready"] == 1
        and s["to_exception"] == 3,
    }
    for index, condition in conditions.items():
        if condition:
            _mark(recorder, NC_OWNER_GROUP, index, cycle, evidence)
    if conditions[8]:
        _mark(
            recorder,
            MMIO_OWNER_GROUP,
            17,
            cycle,
            {
                **evidence,
                "canonical_protocol_bin": "BIN-1032",
                "witness_path": "pbmt_nc",
            },
        )

    stalled = state["stalled_a"]
    if nc_active and s["tl_a_valid"] == 1 and s["tl_a_ready"] == 0:
        if stalled is None and s["tl_a_addr"] is not None:
            state["stalled_a"] = int(s["tl_a_addr"])
        elif stalled is not None and s["tl_a_addr"] == stalled:
            seen.add("a_stable")
    if stalled is not None and tl_a_fire and s["tl_a_addr"] == stalled:
        seen.add("a_release")
        state["stalled_a"] = None
    if {"a_stable", "a_release"}.issubset(seen):
        _mark(recorder, NC_OWNER_GROUP, 9, cycle, {**evidence, "seen": sorted(seen)})

    if nc_active and s["instr_resp_valid"] == 1:
        seen.add(
            "d_denied"
            if s["instr_resp_denied"] == 1
            else "d_corrupt" if s["instr_resp_corrupt"] == 1 else "d_clean"
        )
    if nc_active and s["resp_valid"] == 1 and s["uncache_state"] == _IDLE:
        seen.add("response_idle")
    response_kinds = {"d_clean", "d_corrupt", "d_denied"} & seen
    if response_kinds and "response_idle" in seen:
        _mark(
            recorder,
            NC_OWNER_GROUP,
            11,
            cycle,
            {**evidence, "response_kinds": sorted(response_kinds)},
        )

    if pending and s["backend_redirect"] == 1:
        state["nc_redirect_pending"] = True
    if nc_active and s["uncache_state"] == _WAIT_RESP and s["backend_redirect"] == 1:
        seen.add("redirect_wait_resp")

    wb_identity = (s["wb_ftq_flag"], s["wb_ftq_value"])
    s1_identity = (s["s1_ftq_flag"], s["s1_ftq_value"])
    s2_identity = (s["s2_ftq_flag"], s["s2_ftq_value"])
    wb_identity_known = None not in wb_identity
    s1_identity_known = None not in s1_identity
    s2_identity_known = None not in s2_identity
    older_checker_redirect = (
        s["checker_redirect"] == 1
        and s["wb_path_valid"] == 1
        and s["wb_redirect"] == 1
        and s["ifu_flush"] == 1
        and s["backend_redirect"] != 1
        and wb_identity_known
    )
    younger_nc_in_s1 = (
        older_checker_redirect
        and s["s1_valid"] == 1
        and s["s1_flush"] == 1
        and s["s1_req_uncache"] == 1
        and s["s1_pmp_mmio"] == 0
        and s["s1_pbmt"] == _PBMT_NC
        and s1_identity_known
        and s1_identity != wb_identity
    )
    younger_nc_in_s2 = (
        older_checker_redirect
        and nc_candidate
        and s["s2_wb_not_flush"] != 1
        and s2_identity_known
        and s2_identity != wb_identity
    )
    internal_req_races_flush = younger_nc_in_s2 and req_fire
    # A checker redirect can remain asserted for more than one sampled cycle.
    # Keep the first overlap context intact so the following cycle's same NC
    # request is not mistaken for a distinct recovery transaction.
    if (
        older_checker_redirect
        and (younger_nc_in_s1 or younger_nc_in_s2)
        and state["nc_checker_redirect_pending"] is None
    ):
        use_s2_identity = bool(younger_nc_in_s2)
        old_identity = s2_identity if use_s2_identity else s1_identity
        old_pc = s["s2_pc"] if use_s2_identity else s["s1_pc"]
        old_paddr = s["s2_paddr"] if use_s2_identity else s["s1_paddr"]
        state["nc_checker_redirect_pending"] = {
            "redirect_cycle": int(cycle),
            "checker_pc": s["wb_pc"],
            "checker_ftq": wb_identity,
            "old_ftq": old_identity,
            "old_pc": old_pc,
            "old_paddr": old_paddr,
            "younger_nc_present_in_s1": bool(younger_nc_in_s1),
            "younger_nc_present_in_s2": bool(younger_nc_in_s2),
            "younger_nc_internal_req_races_flush": bool(internal_req_races_flush),
            "old_nc_no_instruncache_request": not to_uncache_fire,
            "old_nc_no_tl_a_fire": not tl_a_fire,
            "old_nc_no_ibuffer_delivery": not (
                s["to_valid"] == 1 and s["to_ready"] == 1
            ),
            "old_nc_no_response": not (
                s["instr_resp_valid"] == 1 or s["resp_valid"] == 1
            ),
            "recovery": None,
            "failure_reasons": [],
        }

    cacheable_delivery = (
        not nc_active
        and s["s2_valid"] == 1
        and s["s2_req_uncache"] == 0
        and s["to_valid"] == 1
        and s["to_ready"] == 1
    )
    mmio_accept = mmio_candidate and req_fire
    current_path = (
        "nc"
        if nc_accept
        else "mmio" if mmio_accept else "cacheable" if cacheable_delivery else None
    )
    checker_pending = state["nc_checker_redirect_pending"]
    if (
        checker_pending is not None
        and int(cycle) > checker_pending["redirect_cycle"]
        # Keep the overlap context quiescent while the same redirect remains
        # asserted; the request visible in this cycle is still the flushed B.
        and not older_checker_redirect
    ):
        failures = checker_pending["failure_reasons"]
        old_ftq = tuple(checker_pending["old_ftq"])
        delivered_ftq = (s["to_ftq_flag"], s["to_ftq_value"])
        delivered_pc_matches_old = (
            checker_pending["old_pc"] is None
            or s["to_pc"] is None
            or int(s["to_pc"]) == int(checker_pending["old_pc"])
        )
        old_ibuffer_delivery = (
            s["to_valid"] == 1
            and s["to_ready"] == 1
            and None not in delivered_ftq
            and delivered_ftq == old_ftq
            and delivered_pc_matches_old
        )
        recovery = checker_pending["recovery"]
        if recovery is None:
            if to_uncache_fire:
                checker_pending["old_nc_no_instruncache_request"] = False
                failures.append("old_nc_instruncache_request")
            if tl_a_fire:
                checker_pending["old_nc_no_tl_a_fire"] = False
                failures.append("old_nc_tl_a_fire")
            if s["instr_resp_valid"] == 1 or s["resp_valid"] == 1:
                checker_pending["old_nc_no_response"] = False
                failures.append("old_nc_response")
            if old_ibuffer_delivery:
                checker_pending["old_nc_no_ibuffer_delivery"] = False
                failures.append("old_nc_ibuffer_delivery")
            if nc_accept:
                recovery_identity_known = (
                    s2_identity_known
                    and checker_pending["old_pc"] is not None
                    and checker_pending["old_paddr"] is not None
                    and s["s2_pc"] is not None
                    and s["s2_paddr"] is not None
                )
                ftq_changed = recovery_identity_known and s2_identity != old_ftq
                pc_changed = (
                    checker_pending["old_pc"] is not None
                    and s["s2_pc"] is not None
                    and int(s["s2_pc"]) != int(checker_pending["old_pc"])
                )
                paddr_changed = (
                    checker_pending["old_paddr"] is not None
                    and s["s2_paddr"] is not None
                    and int(s["s2_paddr"]) != int(checker_pending["old_paddr"])
                )
                identity_changes = [
                    name
                    for name, changed in (
                        ("ftq", ftq_changed),
                        ("pc", pc_changed),
                        ("paddr", paddr_changed),
                    )
                    if changed
                ]
                if recovery_identity_known and identity_changes:
                    checker_pending["recovery"] = {
                        "accept_cycle": int(cycle),
                        "ftq": s2_identity,
                        "pc": s["s2_pc"],
                        "paddr": s["s2_paddr"],
                        "identity_changes": identity_changes,
                        "to_instruncache_request": False,
                        "tl_a_fire": False,
                        "response": False,
                        "ibuffer_delivery": False,
                    }
                else:
                    failures.append("recovery_nc_identity_not_distinct")
        else:
            if old_ibuffer_delivery:
                checker_pending["old_nc_no_ibuffer_delivery"] = False
                failures.append("old_nc_ibuffer_delivery")
            recovery_paddr = recovery["paddr"]
            if to_uncache_fire and not recovery["to_instruncache_request"]:
                to_uncache_matches = (
                    recovery_paddr is not None
                    and s["to_uncache_addr"] is not None
                    and int(s["to_uncache_addr"]) == int(recovery_paddr)
                )
                recovery["to_instruncache_request"] |= bool(to_uncache_matches)
                if not to_uncache_matches:
                    failures.append("instruncache_request_identity_mismatch")
            if tl_a_fire and not recovery["tl_a_fire"]:
                tl_matches = (
                    recovery_paddr is not None
                    and s["tl_a_addr"] is not None
                    and int(s["tl_a_addr"])
                    == ((int(recovery_paddr) << 1) & ~0x7)
                )
                recovery["tl_a_fire"] |= bool(tl_matches)
                if not tl_matches:
                    failures.append("tl_a_request_identity_mismatch")
            if s["instr_resp_valid"] == 1 or s["resp_valid"] == 1:
                recovery["response"] = True
            recovery_delivery = (
                s["to_valid"] == 1
                and s["to_ready"] == 1
                and None not in delivered_ftq
                and delivered_ftq == tuple(recovery["ftq"])
                and s["to_pc"] is not None
                and recovery["pc"] is not None
                and int(s["to_pc"]) == int(recovery["pc"])
            )
            recovery["ibuffer_delivery"] |= bool(recovery_delivery)
            recovery_complete = all(
                recovery[name]
                for name in (
                    "to_instruncache_request",
                    "tl_a_fire",
                    "response",
                    "ibuffer_delivery",
                )
            )
            strong_overlap = checker_pending[
                "younger_nc_internal_req_races_flush"
            ]
            if recovery_complete and strong_overlap and not failures:
                _mark(
                    recorder,
                    NC_OWNER_GROUP,
                    13,
                    cycle,
                    {
                        **evidence,
                        "older_cacheable_checker_redirect": True,
                        "redirect_cycle": checker_pending["redirect_cycle"],
                        "checker_pc": checker_pending["checker_pc"],
                        "checker_ftq": list(checker_pending["checker_ftq"]),
                        "old_nc_ftq": list(old_ftq),
                        "old_nc_pc": checker_pending["old_pc"],
                        "old_nc_paddr": checker_pending["old_paddr"],
                        "younger_nc_present_in_s1": checker_pending[
                            "younger_nc_present_in_s1"
                        ],
                        "younger_nc_present_in_s2": checker_pending[
                            "younger_nc_present_in_s2"
                        ],
                        "younger_nc_internal_req_races_flush": True,
                        "old_nc_no_instruncache_request": True,
                        "old_nc_no_tl_a_fire": True,
                        "old_nc_no_ibuffer_delivery": True,
                        "old_nc_no_response": True,
                        "recovery_nc_new_identity": True,
                        "recovery_nc_ftq": list(recovery["ftq"]),
                        "recovery_nc_pc": recovery["pc"],
                        "recovery_nc_paddr": recovery["paddr"],
                        "recovery_nc_identity_changes": recovery[
                            "identity_changes"
                        ],
                        "recovery_nc_request_and_delivery": True,
                        "stage_position_cross": "checker_redirect_x_internal_req_fire",
                        "flush_side_effect_cross": "flush_x_no_old_external_request",
                        "recovery_identity_cross": "recovery_x_new_ftq_pc_or_address",
                    },
                )
                state["nc_checker_redirect_pending"] = None
        if (
            state["nc_checker_redirect_pending"] is not None
            and int(cycle) - checker_pending["redirect_cycle"] > 8192
        ):
            state["nc_checker_redirect_pending"] = None
    previous_path = state["previous_path"]
    if current_path is not None and previous_path is not None:
        transition = f"{previous_path}_to_{current_path}"
        seen.add(transition)
        if transition in {
            "cacheable_to_nc",
            "cacheable_to_mmio",
            "nc_to_cacheable",
            "nc_to_mmio",
        }:
            state["path_transition_observations"][transition] = {
                "cycle": int(cycle),
                "prev_end_half": s["prev_end_half"],
                "prev_half_pc": s["prev_half_pc"],
                "prev_half_data": s["prev_half_data"],
                "pmp_mmio": s["s2_pmp_mmio"],
                "pbmt": s["s2_pbmt"],
            }
        if transition == "nc_to_cacheable":
            for index in (34, 37):
                _mark(
                    recorder,
                    NC_OWNER_GROUP,
                    index,
                    cycle,
                    {**evidence, "transition": transition},
                )
        elif transition == "nc_to_mmio":
            state["nc_to_mmio_pending"] = True
        elif transition == "cacheable_to_nc":
            _mark(
                recorder,
                NC_OWNER_GROUP,
                36,
                cycle,
                {**evidence, "transition": transition},
            )
    if state["nc_redirect_pending"] and current_path is not None:
        for index in (12, 39):
            _mark(
                recorder,
                NC_OWNER_GROUP,
                index,
                cycle,
                {**evidence, "new_path": current_path},
            )
        state["nc_redirect_pending"] = False
    if state["nc_to_mmio_pending"] and s["uncache_state"] == _WAIT_LAST_COMMIT:
        for index in (35, 38):
            _mark(recorder, NC_OWNER_GROUP, index, cycle, evidence)
        state["nc_to_mmio_pending"] = False
    required_path_transitions = {
        "cacheable_to_nc",
        "cacheable_to_mmio",
        "nc_to_cacheable",
        "nc_to_mmio",
    }
    path_transitions = required_path_transitions & seen
    if path_transitions:
        if path_transitions == required_path_transitions:
            mark_owner_v3_checked(
                recorder,
                "BIN-957",
                cycle,
                {
                    **evidence,
                    "event": "sequential_fetch_path_transition",
                    "path_transitions": sorted(path_transitions),
                    "transition_observations": state[
                        "path_transition_observations"
                    ],
                    "mixed_dual_block_response": False,
                },
                producer="ifu_sequential_path_transition_sampler",
            )
        _mark(
            recorder,
            NC_OWNER_GROUP,
            33,
            cycle,
            {**evidence, "path_transitions": sorted(path_transitions)},
        )

    last_delivery = state["nc_last_delivery"]
    if clean_delivery and single_delivery:
        seen.add("delivery_rvc" if delivered_is_rvc else "delivery_rvi")
        _mark(
            recorder,
            NC_OWNER_GROUP,
            19,
            cycle,
            {**evidence, "delivery_type": "rvc" if delivered_is_rvc else "rvi"},
        )
        if last_delivery is not None and s["uncache_pc"] is not None:
            expected_step = 1 if last_delivery["is_rvc"] else 2
            if int(s["uncache_pc"]) == int(last_delivery["pc"]) + expected_step:
                _mark(
                    recorder,
                    NC_OWNER_GROUP,
                    16 if delivered_is_rvc else 17,
                    cycle,
                    evidence,
                )
        if s["uncache_pc"] is not None:
            state["nc_last_delivery"] = {
                "pc": int(s["uncache_pc"]),
                "is_rvc": bool(delivered_is_rvc),
            }
    if (
        nc_active
        and beat_tail
        and entry_wait_resp
        and s["entry_resending"] == 0
        and s["tl_d_valid"] == 1
        and response_is_rvi
        and s["tl_d_corrupt"] == 0
        and s["tl_d_denied"] == 0
    ):
        state["nc_cross_8b_pending"] = True
        seen.add("cross_8b_first")
    if (
        state["nc_cross_8b_pending"]
        and s["entry_resending"] == 1
        and s["tl_a_valid"] == 1
    ):
        seen.add("cross_8b_second_a")
        _mark(recorder, NC_OWNER_GROUP, 23, cycle, {**evidence, "seen": sorted(seen)})
    if (
        state["nc_cross_8b_pending"]
        and s["resp_valid"] == 1
        and s["resp_need_resend"] == 0
        and nc_delivery
        and not delivered_is_rvc
        and single_delivery
    ):
        _mark(recorder, NC_OWNER_GROUP, 25, cycle, evidence)
        state["nc_cross_8b_pending"] = False

    if nc_active and page_tail and s["resp_valid"] == 1 and s["resp_need_resend"] == 1:
        state["nc_cross_page_pending"] = True
        seen.add("cross_page_resend")
    if (
        state["nc_cross_page_pending"]
        and s["uncache_redirect"] == 1
        and s["resp_need_resend"] == 1
        and s["uncache_pc"] is not None
        and s["resp_data"] is not None
    ):
        seen.add("cross_page_half_state")
    if {"cross_page_resend", "cross_page_half_state"}.issubset(seen):
        _mark(recorder, NC_OWNER_GROUP, 26, cycle, {**evidence, "seen": sorted(seen)})
    if (
        state["nc_cross_page_pending"]
        and nc_delivery
        and not delivered_is_rvc
        and single_delivery
    ):
        for index in (27, 29):
            _mark(recorder, NC_OWNER_GROUP, index, cycle, evidence)
        state["nc_cross_page_pending"] = False
    if (
        state["nc_cross_page_pending"]
        and s["to_valid"] == 1
        and s["to_exception_cross_page"] == 1
        and s["to_exception"] in {1, 2, 3}
    ):
        _mark(
            recorder,
            NC_OWNER_GROUP,
            31,
            cycle,
            {**evidence, "exception_kind": s["to_exception"]},
        )
    if conditions[32]:
        ftq_identity_known = all(
            s[name] is not None
            for name in (
                "s2_ftq_flag",
                "s2_ftq_value",
                "to_ftq_flag",
                "to_ftq_value",
                "to_ftq_offset",
            )
        )
        ftq_identity_matches = (
            ftq_identity_known
            and s["to_ftq_flag"] == s["s2_ftq_flag"]
            and s["to_ftq_value"] == s["s2_ftq_value"]
        )
        no_uncache_request = (
            s["req_valid"] != 1
            and s["to_uncache_valid"] != 1
            and s["tl_a_valid"] != 1
        )
        if ftq_identity_matches and no_uncache_request:
            _mark(
                recorder,
                NC_OWNER_GROUP,
                30,
                cycle,
                {
                    **evidence,
                    "functional_exception_identity_checked": True,
                    "ftq_ptr": [s["to_ftq_flag"], s["to_ftq_value"]],
                    "ftq_offset": s["to_ftq_offset"],
                    "debug_pc": s["to_pc"],
                    "debug_pc_matches_nc_va": s["to_pc"] == s["s2_instr_pc"],
                    "cfvec_pc_functional_requirement": False,
                    "old_uncache_request_suppressed": True,
                },
            )

    if s["ifu_flush"] == 1:
        state["nc_active"] = False
        state["nc_last_delivery"] = None
        state["nc_cross_8b_pending"] = False
        state["nc_cross_page_pending"] = False
    elif nc_accept:
        state["nc_active"] = True
    elif s["resp_valid"] == 1:
        state["nc_active"] = False
    if s["backend_redirect"] == 1:
        state["nc_last_delivery"] = None
        state["nc_checker_redirect_pending"] = None
    if current_path is not None:
        state["previous_path"] = current_path


def sample_mmio_nc_owner_coverage(recorder, env, cycle: int) -> None:
    dut = getattr(env, "dut", None)
    if dut is None:
        return
    state = getattr(recorder, "_ifu_mmio_nc_owner_state", None)
    if not isinstance(state, dict):
        initialize_mmio_nc_owner_coverage_state(recorder)
        state = recorder._ifu_mmio_nc_owner_state
    snapshot = _snapshot(recorder, dut)
    _sample_mmio(recorder, int(cycle), snapshot, state)
    _sample_nc(recorder, int(cycle), snapshot, state)
    sample_instr_uncache_owner_coverage(recorder, int(cycle), snapshot)
    state["previous_uncache_state"] = (
        _IDLE if snapshot["uncache_state"] is None else int(snapshot["uncache_state"])
    )
    state["previous_tl_a_fire"] = bool(
        snapshot["tl_a_valid"] == 1 and snapshot["tl_a_ready"] == 1
    )
    state["previous_backend_can_accept"] = bool(snapshot["backend_accept"] == 1)


__all__ = [
    "MMIO_NC_OWNER_COVERPOINT",
    "MMIO_NC_OWNER_COVERPOINTS",
    "MMIO_NC_OWNER_SAMPLER_BIN_KEYS",
    "MMIO_OWNER_GROUP",
    "MMIO_OWNER_LEAF_COUNT",
    "NC_OWNER_GROUP",
    "NC_OWNER_LEAF_COUNT",
    "initialize_mmio_nc_owner_coverage_state",
    "reset_mmio_nc_owner_coverage_state",
    "sample_mmio_nc_owner_coverage",
]

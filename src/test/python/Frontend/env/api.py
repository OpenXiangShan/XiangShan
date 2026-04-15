from __future__ import annotations

import logging
import os
from typing import Sequence

from .env_config import DEFAULT_ENV_CONFIG
from .logging_utils import configure_env_logging, parse_log_level
from .nemu_trace_pipeline import generate_nemu_trace_from_bin
from .request_apis import (
    check_pc_sequence,
    inject_redirect,
    load_golden_trace,
    load_program,
    load_program_file,
    normalize_bp_ctrl_config,
    normalize_commit_target,
    normalize_golden_trace_source,
    normalize_pc_sequence_expectation,
    normalize_program_image,
    normalize_redirect_txn,
    run_until_commit,
    run_until_golden_trace_complete,
)
from .signal_utils import get_sig


logger = logging.getLogger("env.api")


def _env_config(env):
    return getattr(env, "config", DEFAULT_ENV_CONFIG)


def _read_positive_int_env(name: str) -> int:
    raw = str(os.getenv(name, "")).strip()
    if not raw:
        return 0
    try:
        value = int(raw)
    except ValueError:
        logger.warning("ignore invalid %s=%r", str(name), raw)
        return 0
    if value <= 0:
        return 0
    return value


def _read_progress_interval_from_env() -> int:
    return _read_positive_int_env("TB_TRACE_PROGRESS_INTERVAL")


def _read_stall_snapshot_interval_from_env() -> int:
    return _read_positive_int_env("TB_TRACE_STALL_SNAPSHOT_INTERVAL")


def _format_optional_pc(value) -> str:
    if value is None:
        return "none"
    return f"0x{int(value):x}"


def _get_current_golden_pc(trace, backend_model):
    getter = getattr(backend_model, "current_golden_pc", None)
    if callable(getter):
        return getter()
    cur = trace.peek()
    if cur is None:
        return None
    return int(cur.pc)


def _read_sig(dut, name: str, default=None):
    if dut is None:
        return default
    if default is None:
        return get_sig(dut, name, 0)
    return get_sig(dut, name, default)


def _read_addr_sig(dut, name: str) -> int:
    return int(_read_sig(dut, name, 0)) << 1


def _read_indexed_bits(dut, prefix: str, count: int) -> tuple[list[int], int]:
    bits: list[int] = []
    mask = 0
    for idx in range(int(count)):
        bit = 1 if int(_read_sig(dut, f"{prefix}{idx}", 0)) else 0
        bits.append(bit)
        if bit:
            mask |= 1 << idx
    return bits, mask


def api_Frontend_capture_frontend_stall_snapshot(env) -> dict:
    """Capture a read-only snapshot of key DUT frontend boundary signals."""
    trace = getattr(env.backend_model, "golden_trace", None)
    dut = getattr(env, "dut", None)
    monitor = getattr(env, "monitor", None)
    redirect_head = None
    if monitor is not None and hasattr(monitor, "oldest_unsynced_redirect"):
        redirect_head = monitor.oldest_unsynced_redirect()
    backend_model = getattr(env, "backend_model", None)

    def _encode_ftq_entry(entry):
        if entry is None:
            return None
        return {
            "ftq_flag": int(getattr(entry, "ftq_flag", 0)),
            "ftq_value": int(getattr(entry, "ftq_value", 0)),
            "total_cfi": int(getattr(entry, "total_cfi", 0)),
            "resolved_cfi": int(getattr(entry, "resolved_cfi", 0)),
            "dispatch_complete": int(bool(getattr(entry, "dispatch_complete", False))),
            "has_redirect": int(bool(getattr(entry, "has_redirect", False))),
            "commit_ready_cycle": int(getattr(entry, "commit_ready_cycle", 0)),
        }

    pending_level0_target_ftq = getattr(backend_model, "_pending_level0_target_ftq", None)
    recent_redirect = None
    if backend_model is not None:
        for event in reversed(list(getattr(backend_model, "last_events", []))):
            if str(event.get("kind", "")) != "redirect":
                continue
            recent_redirect = {
                "cycle": int(event.get("cycle", 0)),
                "target_pc": int(event.get("target_pc", 0)),
                "reason": str(event.get("reason", "")),
            }
            break

    def _read_ftq_entry(idx: int | None):
        if idx is None or idx < 0:
            return None
        return {
            "idx": int(idx),
            "start_pc": _read_addr_sig(dut, f"Frontend_top.Frontend.inner_ftq.entryQueue_{idx}_startPc_addr"),
            "taken_valid": _read_sig(dut, f"Frontend_top.Frontend.inner_ftq.entryQueue_{idx}_takenCfiOffset_valid", 0),
            "taken_bits": _read_sig(dut, f"Frontend_top.Frontend.inner_ftq.entryQueue_{idx}_takenCfiOffset_bits", 0),
        }

    cfvec = []
    for slot in range(8):
        if _read_sig(dut, f"io_backend_cfVec_{slot}_valid", 0) != 1:
            continue
        cfvec.append(
            {
                "slot": int(slot),
                "pc": _read_sig(dut, f"io_backend_cfVec_{slot}_bits_pc", 0),
                "instr": _read_sig(dut, f"io_backend_cfVec_{slot}_bits_instr", 0),
                "ftq_flag": _read_sig(dut, f"io_backend_cfVec_{slot}_bits_ftqPtr_flag", 0),
                "ftq_value": _read_sig(dut, f"io_backend_cfVec_{slot}_bits_ftqPtr_value", 0),
                "ftq_offset": _read_sig(dut, f"io_backend_cfVec_{slot}_bits_ftqOffset", 0),
                "is_last": _read_sig(dut, f"io_backend_cfVec_{slot}_bits_isLastInFtqEntry", 0),
            }
        )

    stall_reason = [_read_sig(dut, f"io_backend_stallReason_reason_{idx}", 0) for idx in range(8)]
    s3_instr_valid_bits, s3_instr_valid_mask = _read_indexed_bits(
        dut,
        "Frontend_top.Frontend.inner_ifu.s3_alignInstrValid_",
        12,
    )
    s3_invalid_taken_bits, s3_invalid_taken_mask = _read_indexed_bits(
        dut,
        "Frontend_top.Frontend.inner_ifu.s3_alignInvalidTaken_",
        12,
    )
    s3_select_block_bits, s3_select_block_mask = _read_indexed_bits(
        dut,
        "Frontend_top.Frontend.inner_ifu.s3_alignCompactInfo_selectBlock_",
        12,
    )
    fixed_two_fetch_bits, fixed_two_fetch_mask = _read_indexed_bits(
        dut,
        "Frontend_top.Frontend.inner_ifu.predChecker.io_resp_stage1Out_fixedTwoFetchRange_",
        12,
    )
    ifu_ptr_flag = _read_sig(dut, "Frontend_top.Frontend.inner_ftq.ifuPtr_ptrs_0_flag", 0)
    ifu_ptr_value = _read_sig(dut, "Frontend_top.Frontend.inner_ftq.ifuPtr_ptrs_0_value", 0)
    bpu_ptr_flag = _read_sig(dut, "Frontend_top.Frontend.inner_ftq.bpuPtr_ptrs_0_flag", 0)
    bpu_ptr_value = _read_sig(dut, "Frontend_top.Frontend.inner_ftq.bpuPtr_ptrs_0_value", 0)
    ibuffer_lane_pcs = [
        _read_addr_sig(dut, f"Frontend_top.Frontend._inner_ifu_io_toIBuffer_bits_pc_{idx}_addr")
        for idx in range(4)
    ]
    return {
        "cursor": (None if trace is None else int(trace.cursor)),
        "total_entries": (0 if trace is None else int(len(trace.entries))),
        "golden_pc": _get_current_golden_pc(trace, env.backend_model) if trace is not None else None,
        "golden_wait_pc": getattr(env.backend_model, "_golden_wait_pc", None),
        "backend_can_accept": _read_sig(dut, "io_backend_canAccept", 0),
        "from_ftq": {
            "wen": _read_sig(dut, "io_backend_fromFtq_wen", 0),
            "ftq_idx": _read_sig(dut, "io_backend_fromFtq_ftqIdx", 0),
            "start_pc_addr": _read_sig(dut, "io_backend_fromFtq_startPc_addr", 0),
        },
        "from_ifu_gpaddr": {
            "wen": _read_sig(dut, "io_backend_fromIfu_gpAddrMem_wen", 0),
            "waddr": _read_sig(dut, "io_backend_fromIfu_gpAddrMem_waddr", 0),
            "gpaddr": _read_sig(dut, "io_backend_fromIfu_gpAddrMem_wdata_gpaddr", 0),
            "is_for_vs_nonleaf_pte": _read_sig(dut, "io_backend_fromIfu_gpAddrMem_wdata_isForVSnonLeafPTE", 0),
        },
        "backend_state": {
            "commit_ptr": {
                "flag": int(getattr(backend_model, "commit_ptr_flag", 0)),
                "value": int(getattr(backend_model, "commit_ptr_value", 0)),
            },
            "pending_level0_target_ftq": (
                None
                if pending_level0_target_ftq is None
                else {
                    "flag": int(pending_level0_target_ftq[0]),
                    "value": int(pending_level0_target_ftq[1]),
                }
            ),
            "current_ftq_entry": _encode_ftq_entry(getattr(backend_model, "_current_ftq_entry", None)),
            "ftq_entries": [
                _encode_ftq_entry(entry) for entry in list(getattr(backend_model, "ftq_entries", []))[:4]
            ],
            "recent_redirect": recent_redirect,
        },
        "redirect_drive": {
            "valid": _read_sig(dut, "io_backend_toFtq_redirect_valid", 0),
            "pc": _read_sig(dut, "io_backend_toFtq_redirect_bits_pc", 0),
            "target": _read_sig(dut, "io_backend_toFtq_redirect_bits_target", 0),
            "level": _read_sig(dut, "io_backend_toFtq_redirect_bits_level", 0),
            "debug_is_ctrl": _read_sig(dut, "io_backend_toFtq_redirect_bits_debugIsCtrl", 0),
            "debug_is_mem_vio": _read_sig(dut, "io_backend_toFtq_redirect_bits_debugIsMemVio", 0),
        },
        "monitor_recent_dut_redirect": getattr(monitor, "last_dut_redirect", None),
        "stall_reason": stall_reason,
        "wfi": {
            "req": _read_sig(dut, "io_backend_wfi_wfiReq", 0),
            "safe": _read_sig(dut, "io_backend_wfi_wfiSafe", 0),
        },
        "icache_req": {
            "valid": _read_sig(dut, "auto_inner_icache_client_out_a_valid", 0),
            "ready": _read_sig(dut, "auto_inner_icache_client_out_a_ready", 0),
            "address": _read_sig(dut, "auto_inner_icache_client_out_a_bits_address", 0),
            "source": _read_sig(dut, "auto_inner_icache_client_out_a_bits_source", 0),
        },
        "icache_resp": {
            "valid": _read_sig(dut, "auto_inner_icache_client_out_d_valid", 0),
            "source": _read_sig(dut, "auto_inner_icache_client_out_d_bits_source", 0),
            "opcode": _read_sig(dut, "auto_inner_icache_client_out_d_bits_opcode", 0),
        },
        "uncache_req": {
            "valid": _read_sig(dut, "auto_inner_instrUncache_client_out_a_valid", 0),
            "ready": _read_sig(dut, "auto_inner_instrUncache_client_out_a_ready", 0),
            "address": _read_sig(dut, "auto_inner_instrUncache_client_out_a_bits_address", 0),
        },
        "uncache_resp": {
            "valid": _read_sig(dut, "auto_inner_instrUncache_client_out_d_valid", 0),
            "source": _read_sig(dut, "auto_inner_instrUncache_client_out_d_bits_source", 0),
        },
        "monitor_redirect": {
            "wait_sync_after_redirect": int(bool(getattr(monitor, "wait_sync_after_redirect", False))),
            "expected_pc": getattr(monitor, "expected_pc", None),
            "redirect_grace": int(getattr(monitor, "redirect_grace", 0)),
            "redirect_sync_deadline": int(getattr(monitor, "redirect_sync_deadline", 0)),
            "queue_len": int(len(getattr(monitor, "_redirect_queue", []))) if monitor is not None else 0,
            "head": (
                None
                if redirect_head is None
                else {
                    "seq": int(getattr(redirect_head, "seq", 0)),
                    "target_pc": int(getattr(redirect_head, "target_pc", 0)),
                    "reason": str(getattr(redirect_head, "reason", "")),
                    "deadline": int(getattr(redirect_head, "deadline", 0)),
                }
            ),
        },
        "ftq_runtime": {
            "ifu_ptr": {"flag": int(ifu_ptr_flag), "value": int(ifu_ptr_value)},
            "bpu_ptr": {"flag": int(bpu_ptr_flag), "value": int(bpu_ptr_value)},
            "prediction_ptr_value": _read_sig(dut, "Frontend_top.Frontend.inner_ftq.predictionPtr_value", 0),
            "ifu_entry": _read_ftq_entry(int(ifu_ptr_value)),
            "bpu_entry": _read_ftq_entry(int(bpu_ptr_value)),
            "backend_redirect_valid": _read_sig(dut, "Frontend_top.Frontend.inner_ftq.backendRedirect_valid", 0),
            "redirect_valid": _read_sig(dut, "Frontend_top.Frontend.inner_ftq.redirect_valid", 0),
            "redirect_reg_valid": _read_sig(dut, "Frontend_top.Frontend.inner_ftq.redirectReg_valid", 0),
            "redirect_next_valid": _read_sig(dut, "Frontend_top.Frontend.inner_ftq.redirectNext_valid", 0),
        },
        "ifu_runtime": {
            "from_ftq_req_ready": _read_sig(dut, "Frontend_top.Frontend.inner_ifu.io_fromFtq_req_ready", 0),
            "from_ftq_taken_valid": _read_sig(
                dut,
                "Frontend_top.Frontend.inner_ifu.io_fromFtq_req_bits_fetch_0_takenCfiOffset_valid",
                0,
            ),
            "to_ibuffer_valid": _read_sig(dut, "Frontend_top.Frontend.inner_ifu.io_toIBuffer_valid", 0),
            "s0_fire": _read_sig(dut, "Frontend_top.Frontend.inner_ifu.s0_fire", 0),
            "s1_fire": _read_sig(dut, "Frontend_top.Frontend.inner_ifu.s1_fire", 0),
            "s2_fire": _read_sig(dut, "Frontend_top.Frontend.inner_ifu.s2_fire", 0),
            "s1_ready": _read_sig(dut, "Frontend_top.Frontend.inner_ifu.s1_ready", 0),
            "s2_ready": _read_sig(dut, "Frontend_top.Frontend.inner_ifu.s2_ready", 0),
            "s3_ready": _read_sig(dut, "Frontend_top.Frontend.inner_ifu.s3_ready", 0),
            "s1_valid": _read_sig(dut, "Frontend_top.Frontend.inner_ifu.s1_valid", 0),
            "s2_valid": _read_sig(dut, "Frontend_top.Frontend.inner_ifu.s2_valid", 0),
            "s3_valid": _read_sig(dut, "Frontend_top.Frontend.inner_ifu.s3_valid_valid", 0),
            "s1_start_pc": _read_addr_sig(dut, "Frontend_top.Frontend.inner_ifu.s1_fetchBlock_0_startVAddr_addr"),
            "s2_start_pc": _read_addr_sig(dut, "Frontend_top.Frontend.inner_ifu.s2_fetchBlock_0_startVAddr_addr"),
            "s3_start_pc": _read_addr_sig(dut, "Frontend_top.Frontend.inner_ifu.s3_alignFetchBlock_0_startVAddr_addr"),
            "s3_target_pc": _read_addr_sig(dut, "Frontend_top.Frontend.inner_ifu.s3_alignFetchBlock_0_target_addr"),
            "s3_ftq": {
                "flag": _read_sig(dut, "Frontend_top.Frontend.inner_ifu.s3_alignFetchBlock_0_ftqIdx_flag", 0),
                "value": _read_sig(dut, "Frontend_top.Frontend.inner_ifu.s3_alignFetchBlock_0_ftqIdx_value", 0),
            },
            "to_ibuffer_enq_enable_mask": _read_sig(
                dut,
                "Frontend_top.Frontend.inner_ifu.io_toIBuffer_bits_enqEnable_0",
                0,
            ),
            "s3_instr_valid_bits": s3_instr_valid_bits,
            "s3_instr_valid_mask": s3_instr_valid_mask,
            "s3_invalid_taken_bits": s3_invalid_taken_bits,
            "s3_invalid_taken_mask": s3_invalid_taken_mask,
            "s3_select_block_bits": s3_select_block_bits,
            "s3_select_block_mask": s3_select_block_mask,
            "fixed_two_fetch_range_bits": fixed_two_fetch_bits,
            "fixed_two_fetch_range_mask": fixed_two_fetch_mask,
            "ibuffer_lane_pcs": ibuffer_lane_pcs,
        },
        "cfvec_valid_count": len(cfvec),
        "cfvec": cfvec,
    }


def _format_stall_snapshot(snapshot: dict) -> str:
    from_ftq = snapshot["from_ftq"]
    from_ifu_gpaddr = snapshot["from_ifu_gpaddr"]
    backend_state = snapshot["backend_state"]
    redirect_drive = snapshot["redirect_drive"]
    icache_req = snapshot["icache_req"]
    icache_resp = snapshot["icache_resp"]
    uncache_req = snapshot["uncache_req"]
    uncache_resp = snapshot["uncache_resp"]
    monitor_redirect = snapshot["monitor_redirect"]
    monitor_recent_dut_redirect = snapshot["monitor_recent_dut_redirect"]
    ftq_runtime = snapshot["ftq_runtime"]
    ifu_runtime = snapshot["ifu_runtime"]
    redirect_head = monitor_redirect["head"]
    current_ftq_entry = backend_state["current_ftq_entry"]
    recent_redirect = backend_state["recent_redirect"]

    def _format_ftq_entry(entry):
        if entry is None:
            return "none"
        return "({flag},{value},cfi={resolved}/{total},redirect={redirect})".format(
            flag=int(entry["ftq_flag"]),
            value=int(entry["ftq_value"]),
            resolved=int(entry["resolved_cfi"]),
            total=int(entry["total_cfi"]),
            redirect=int(entry["has_redirect"]),
        )

    def _format_runtime_ftq_entry(entry):
        if entry is None:
            return "none"
        return "({idx},start={start},taken={taken_valid}/{taken_bits})".format(
            idx=int(entry["idx"]),
            start=_format_optional_pc(entry["start_pc"]),
            taken_valid=int(entry["taken_valid"]),
            taken_bits=int(entry["taken_bits"]),
        )

    def _format_recent_dut_redirect(entry):
        if entry is None:
            return "none"
        return (
            "(cycle={cycle},pc={pc},target={target},taken={taken},level={level},ctrl={ctrl},mem_vio={mem_vio})"
        ).format(
            cycle=int(entry.get("cycle", 0)),
            pc=_format_optional_pc(entry.get("pc")),
            target=_format_optional_pc(entry.get("target_pc")),
            taken=int(entry.get("taken", 0)),
            level=int(entry.get("level", 0)),
            ctrl=int(entry.get("debug_is_ctrl", 0)),
            mem_vio=int(entry.get("debug_is_mem_vio", 0)),
        )

    return (
        "backend_can_accept={backend_can_accept} "
        "golden_pc={golden_pc} golden_wait_pc={golden_wait_pc} "
        "from_ftq_wen={from_ftq_wen} from_ftq_idx={from_ftq_idx} from_ftq_start={from_ftq_start} "
        "from_ifu_gpaddr=({from_ifu_wen},{from_ifu_waddr},0x{from_ifu_gpaddr:x},vs_nonleaf={from_ifu_vs_nonleaf}) "
        "commit_ptr=({commit_ptr_flag},{commit_ptr_value}) "
        "pending_level0_target_ftq={pending_level0_target_ftq} "
        "current_ftq_entry={current_ftq_entry} "
        "ftq_entries_head={ftq_entries_head} "
        "recent_redirect_target={recent_redirect_target} "
        "stall_reason={stall_reason} "
        "icache_req=({icache_req_valid},{icache_req_ready},0x{icache_req_addr:x},src={icache_req_source}) "
        "icache_resp=({icache_resp_valid},src={icache_resp_source},opc={icache_resp_opcode}) "
        "uncache_req=({uncache_req_valid},{uncache_req_ready},0x{uncache_req_addr:x}) "
        "uncache_resp=({uncache_resp_valid},src={uncache_resp_source}) "
        "wfi=({wfi_req},{wfi_safe}) "
        "monitor_wait_sync={monitor_wait_sync} monitor_expected_pc={monitor_expected_pc} "
        "redirect_grace={redirect_grace} redirect_queue_len={redirect_queue_len} "
        "redirect_head_target={redirect_head_target} redirect_head_deadline={redirect_head_deadline} "
        "redirect_drive=({redirect_drive_valid},pc={redirect_drive_pc},target={redirect_drive_target},"
        "level={redirect_drive_level},ctrl={redirect_drive_ctrl},mem_vio={redirect_drive_mem_vio}) "
        "recent_dut_redirect={recent_dut_redirect} "
        "ftq_runtime=(ifu=({ifu_ptr_flag},{ifu_ptr_value}) bpu=({bpu_ptr_flag},{bpu_ptr_value}) "
        "pred={prediction_ptr_value} ifu_entry={ifu_entry} bpu_entry={bpu_entry} "
        "redirects=[{ftq_backend_redirect},{ftq_redirect},{ftq_redirect_reg},{ftq_redirect_next}]) "
        "ifu_runtime=(req_ready={ifu_req_ready} taken_valid={ifu_req_taken_valid} to_ibuf_valid={ifu_to_ibuf_valid} "
        "fire=[{s0_fire},{s1_fire},{s2_fire}] ready=[{s1_ready},{s2_ready},{s3_ready}] "
        "valid=[{s1_valid},{s2_valid},{s3_valid}] s1={ifu_s1_start} s2={ifu_s2_start} "
        "s3={ifu_s3_start} s3_target={ifu_s3_target} s3_ftq=({ifu_s3_ftq_flag},{ifu_s3_ftq_value}) "
        "enq=0x{ifu_enq_mask:x} s3_valid_mask=0x{ifu_s3_valid_mask:x} "
        "invalid_taken_mask=0x{ifu_s3_invalid_taken_mask:x} select_block_mask=0x{ifu_s3_select_block_mask:x} "
        "fixed_range_mask=0x{ifu_fixed_range_mask:x} ibuf_pcs={ibuf_pcs}) "
        "cfvec_valid_count={cfvec_valid_count}"
    ).format(
        backend_can_accept=int(snapshot["backend_can_accept"]),
        golden_pc=_format_optional_pc(snapshot["golden_pc"]),
        golden_wait_pc=_format_optional_pc(snapshot["golden_wait_pc"]),
        from_ftq_wen=int(from_ftq["wen"]),
        from_ftq_idx=int(from_ftq["ftq_idx"]),
        from_ftq_start=_format_optional_pc(from_ftq["start_pc_addr"] << 1),
        from_ifu_wen=int(from_ifu_gpaddr["wen"]),
        from_ifu_waddr=int(from_ifu_gpaddr["waddr"]),
        from_ifu_gpaddr=int(from_ifu_gpaddr["gpaddr"]),
        from_ifu_vs_nonleaf=int(from_ifu_gpaddr["is_for_vs_nonleaf_pte"]),
        commit_ptr_flag=int(backend_state["commit_ptr"]["flag"]),
        commit_ptr_value=int(backend_state["commit_ptr"]["value"]),
        pending_level0_target_ftq=(
            "none"
            if backend_state["pending_level0_target_ftq"] is None
            else "({flag},{value})".format(
                flag=int(backend_state["pending_level0_target_ftq"]["flag"]),
                value=int(backend_state["pending_level0_target_ftq"]["value"]),
            )
        ),
        current_ftq_entry=_format_ftq_entry(current_ftq_entry),
        ftq_entries_head="[" + ",".join(_format_ftq_entry(entry) for entry in backend_state["ftq_entries"]) + "]",
        recent_redirect_target=(
            _format_optional_pc(recent_redirect["target_pc"]) if recent_redirect is not None else "none"
        ),
        stall_reason="[" + ",".join(str(int(x)) for x in snapshot["stall_reason"]) + "]",
        icache_req_valid=int(icache_req["valid"]),
        icache_req_ready=int(icache_req["ready"]),
        icache_req_addr=int(icache_req["address"]),
        icache_req_source=int(icache_req["source"]),
        icache_resp_valid=int(icache_resp["valid"]),
        icache_resp_source=int(icache_resp["source"]),
        icache_resp_opcode=int(icache_resp["opcode"]),
        uncache_req_valid=int(uncache_req["valid"]),
        uncache_req_ready=int(uncache_req["ready"]),
        uncache_req_addr=int(uncache_req["address"]),
        uncache_resp_valid=int(uncache_resp["valid"]),
        uncache_resp_source=int(uncache_resp["source"]),
        wfi_req=int(snapshot["wfi"]["req"]),
        wfi_safe=int(snapshot["wfi"]["safe"]),
        monitor_wait_sync=int(monitor_redirect["wait_sync_after_redirect"]),
        monitor_expected_pc=_format_optional_pc(monitor_redirect["expected_pc"]),
        redirect_grace=int(monitor_redirect["redirect_grace"]),
        redirect_queue_len=int(monitor_redirect["queue_len"]),
        redirect_head_target=(
            _format_optional_pc(redirect_head["target_pc"]) if redirect_head is not None else "none"
        ),
        redirect_head_deadline=(int(redirect_head["deadline"]) if redirect_head is not None else 0),
        redirect_drive_valid=int(redirect_drive["valid"]),
        redirect_drive_pc=_format_optional_pc(redirect_drive["pc"]),
        redirect_drive_target=_format_optional_pc(redirect_drive["target"]),
        redirect_drive_level=int(redirect_drive["level"]),
        redirect_drive_ctrl=int(redirect_drive["debug_is_ctrl"]),
        redirect_drive_mem_vio=int(redirect_drive["debug_is_mem_vio"]),
        recent_dut_redirect=_format_recent_dut_redirect(monitor_recent_dut_redirect),
        ifu_ptr_flag=int(ftq_runtime["ifu_ptr"]["flag"]),
        ifu_ptr_value=int(ftq_runtime["ifu_ptr"]["value"]),
        bpu_ptr_flag=int(ftq_runtime["bpu_ptr"]["flag"]),
        bpu_ptr_value=int(ftq_runtime["bpu_ptr"]["value"]),
        prediction_ptr_value=int(ftq_runtime["prediction_ptr_value"]),
        ifu_entry=_format_runtime_ftq_entry(ftq_runtime["ifu_entry"]),
        bpu_entry=_format_runtime_ftq_entry(ftq_runtime["bpu_entry"]),
        ftq_backend_redirect=int(ftq_runtime["backend_redirect_valid"]),
        ftq_redirect=int(ftq_runtime["redirect_valid"]),
        ftq_redirect_reg=int(ftq_runtime["redirect_reg_valid"]),
        ftq_redirect_next=int(ftq_runtime["redirect_next_valid"]),
        ifu_req_ready=int(ifu_runtime["from_ftq_req_ready"]),
        ifu_req_taken_valid=int(ifu_runtime["from_ftq_taken_valid"]),
        ifu_to_ibuf_valid=int(ifu_runtime["to_ibuffer_valid"]),
        s0_fire=int(ifu_runtime["s0_fire"]),
        s1_fire=int(ifu_runtime["s1_fire"]),
        s2_fire=int(ifu_runtime["s2_fire"]),
        s1_ready=int(ifu_runtime["s1_ready"]),
        s2_ready=int(ifu_runtime["s2_ready"]),
        s3_ready=int(ifu_runtime["s3_ready"]),
        s1_valid=int(ifu_runtime["s1_valid"]),
        s2_valid=int(ifu_runtime["s2_valid"]),
        s3_valid=int(ifu_runtime["s3_valid"]),
        ifu_s1_start=_format_optional_pc(ifu_runtime["s1_start_pc"]),
        ifu_s2_start=_format_optional_pc(ifu_runtime["s2_start_pc"]),
        ifu_s3_start=_format_optional_pc(ifu_runtime["s3_start_pc"]),
        ifu_s3_target=_format_optional_pc(ifu_runtime["s3_target_pc"]),
        ifu_s3_ftq_flag=int(ifu_runtime["s3_ftq"]["flag"]),
        ifu_s3_ftq_value=int(ifu_runtime["s3_ftq"]["value"]),
        ifu_enq_mask=int(ifu_runtime["to_ibuffer_enq_enable_mask"]),
        ifu_s3_valid_mask=int(ifu_runtime["s3_instr_valid_mask"]),
        ifu_s3_invalid_taken_mask=int(ifu_runtime["s3_invalid_taken_mask"]),
        ifu_s3_select_block_mask=int(ifu_runtime["s3_select_block_mask"]),
        ifu_fixed_range_mask=int(ifu_runtime["fixed_two_fetch_range_mask"]),
        ibuf_pcs="[" + ",".join(_format_optional_pc(pc) for pc in ifu_runtime["ibuffer_lane_pcs"]) + "]",
        cfvec_valid_count=int(snapshot["cfvec_valid_count"]),
    )


def api_Frontend_load_program(env, bin_data, base_addr, max_cycles=1000):
    """Load a program image into the env memory model."""
    configure_env_logging()
    image = normalize_program_image(bin_data=bin_data, base_addr=base_addr)
    written = int(load_program(env, image, step_cycles=1))
    logger.info("api load program: base=0x%x size=%d", image.base_addr, len(image.payload))
    return written


def api_Frontend_load_program_file(env, path, base_addr, max_cycles=1000):
    """Load a program image from a binary file into the env memory model."""
    configure_env_logging()
    size = int(load_program_file(env, path, base_addr, step_cycles=1))
    logger.info("api load program file: path=%s base=0x%x size=%d", str(path), int(base_addr), size)
    return size


def api_Frontend_load_golden_trace(env, path, max_cycles=0) -> int:
    """Load a golden trace file and attach it to backend model comparison."""
    configure_env_logging()
    source = normalize_golden_trace_source(path)
    count = int(load_golden_trace(env, source, step_cycles=max_cycles))
    logger.info("api load golden trace: path=%s entries=%d", source.path, count)
    return count


def api_Frontend_prepare_program_and_nemu_trace(
    env,
    bin_path,
    base_addr,
    trace_output_path,
    nemu_exec_path=None,
    nemu_log_path=None,
    nemu_max_instr=0,
    trace_limit=0,
    load_trace=1,
    max_cycles=0,
) -> dict:
    """Load real bin into DUT env, run NEMU to build trace json/jsonl, optionally load trace."""
    configure_env_logging()
    env_config = _env_config(env)
    nemu_image_base_addr = int(env_config.trace.nemu_image_base_addr)
    if int(base_addr) != nemu_image_base_addr:
        raise ValueError(
            "api_Frontend_prepare_program_and_nemu_trace requires "
            f"base_addr=0x{nemu_image_base_addr:x} to match the current NEMU image base, "
            f"got 0x{int(base_addr):x}"
        )
    bin_size = int(api_Frontend_load_program_file(env, str(bin_path), int(base_addr), max_cycles=0))
    trace_out = generate_nemu_trace_from_bin(
        bin_path=str(bin_path),
        trace_output_path=str(trace_output_path),
        nemu_exec_path=(None if not nemu_exec_path else str(nemu_exec_path)),
        nemu_log_path=(None if not nemu_log_path else str(nemu_log_path)),
        nemu_max_instr=int(nemu_max_instr),
        trace_limit=int(trace_limit),
    )
    trace_entries = int(trace_out["trace_entries"])

    loaded_trace_entries = 0
    if int(load_trace):
        loaded_trace_entries = int(load_golden_trace(env, normalize_golden_trace_source(trace_output_path), step_cycles=0))

    if int(max_cycles) > 0:
        env.step(int(max_cycles))

    out = {
        "bin_path": str(bin_path),
        "bin_size": int(bin_size),
        "base_addr": int(base_addr),
        "nemu_exec_path": str(trace_out["nemu_exec_path"]),
        "nemu_log_path": str(trace_out["nemu_log_path"]),
        "trace_output_path": str(trace_out["trace_output_path"]),
        "trace_entries": int(trace_entries),
        "loaded_trace_entries": int(loaded_trace_entries),
    }
    logger.info(
        "api prepare program and nemu trace: bin=%s base=0x%x bin_size=%d trace_entries=%d loaded_trace=%d",
        str(bin_path),
        int(base_addr),
        int(bin_size),
        int(trace_entries),
        int(loaded_trace_entries),
    )
    return out


def api_Frontend_run_until_commit(env, target_count, max_cycles=10000) -> int:
    """Run simulation until target commits are observed."""
    configure_env_logging()
    target = normalize_commit_target(target_count=target_count, max_cycles=max_cycles)
    if target.target_count < 0:
        raise ValueError("target_count must be >= 0")
    got = int(run_until_commit(env, target))
    logger.info("api run until commit: target=%d got=%d max_cycles=%d", target.target_count, got, target.max_cycles)
    return got


def api_Frontend_run_until_golden_complete(env, max_cycles=10000) -> bool:
    """Run simulation until the attached golden trace is fully consumed and backend state is quiescent."""
    configure_env_logging()
    return bool(
        run_until_golden_trace_complete(
            env,
            max_cycles=int(max_cycles),
            progress_interval=_read_progress_interval_from_env(),
            stall_snapshot_interval=_read_stall_snapshot_interval_from_env(),
            logger=logger,
            current_golden_pc_getter=_get_current_golden_pc,
            format_optional_pc=_format_optional_pc,
            stall_snapshot_capture=api_Frontend_capture_frontend_stall_snapshot,
            stall_snapshot_formatter=_format_stall_snapshot,
        )
    )


def api_Frontend_inject_redirect(env, target_pc, reason, max_cycles=1000) -> bool:
    """Inject a redirect event and wait until monitor sees target PC."""
    configure_env_logging()
    txn = normalize_redirect_txn(target_pc=target_pc, reason=reason, max_cycles=max_cycles)
    logger.info(
        "api inject redirect: target=0x%x reason=%s max_cycles=%d",
        txn.target_pc,
        txn.reason,
        txn.max_cycles,
    )
    ok = inject_redirect(env, txn)
    if ok:
        logger.info("api inject redirect success: target=0x%x", txn.target_pc)
    else:
        logger.warning("api inject redirect timeout: target=0x%x", txn.target_pc)
    return ok


def api_Frontend_check_pc_sequence(env, expected_pcs: Sequence[int], max_cycles=5000) -> bool:
    """Check if expected PC sequence appears in monitor observations in order."""
    configure_env_logging()
    expectation = normalize_pc_sequence_expectation(expected_pcs=expected_pcs, max_cycles=max_cycles)
    ok = check_pc_sequence(env, expectation)
    if ok:
        logger.info("api check pc sequence success: count=%d", len(expectation.expected_pcs))
    else:
        logger.warning("api check pc sequence timeout: expected=%d", len(expectation.expected_pcs))
    return ok


def api_Frontend_get_branch_stats(env) -> dict:
    """Return branch stats from branch checker."""
    return env.branch_checker.get_stats()


def api_Frontend_set_log_level(env, level, max_cycles=0) -> int:
    """Set testbench env logging level, e.g. DEBUG/INFO/WARNING/ERROR."""
    value = parse_log_level(str(level))
    configure_env_logging(str(level))
    if hasattr(env, "set_log_level"):
        env.set_log_level(str(level))
    if int(max_cycles) > 0:
        env.step(int(max_cycles))
    logger.info("api set log level: %s", str(level).upper())
    return int(value)


def api_Frontend_set_bp_ctrl_enable(
    env,
    ubtb_enable=1,
    abtb_enable=1,
    mbtb_enable=1,
    tage_enable=1,
    sc_enable=1,
    ittage_enable=1,
    max_cycles=0,
) -> dict:
    """Set io_csrCtrl_bp_ctrl_*Enable bits."""
    configure_env_logging()
    bp_cfg = normalize_bp_ctrl_config(
        ubtb_enable=ubtb_enable,
        abtb_enable=abtb_enable,
        mbtb_enable=mbtb_enable,
        tage_enable=tage_enable,
        sc_enable=sc_enable,
        ittage_enable=ittage_enable,
    )
    cfg = env.set_bp_ctrl_enable(
        ubtb_enable=bp_cfg.ubtb_enable,
        abtb_enable=bp_cfg.abtb_enable,
        mbtb_enable=bp_cfg.mbtb_enable,
        tage_enable=bp_cfg.tage_enable,
        sc_enable=bp_cfg.sc_enable,
        ittage_enable=bp_cfg.ittage_enable,
    )
    if int(max_cycles) > 0:
        env.step(int(max_cycles))
    logger.info("api set bp ctrl enable: %s", cfg)
    return cfg


def api_Frontend_enable_fst_dump(env, fst_path=None, max_cycles=1):
    """Enable FST waveform dump and optionally set waveform path."""
    path = env.enable_fst_dump(fst_path)
    if int(max_cycles) > 0:
        env.step(1)
    return path


def api_Frontend_pause_fst_dump(env, max_cycles=1) -> bool:
    """Pause FST waveform dump."""
    ok = env.pause_waveform_dump()
    if int(max_cycles) > 0:
        env.step(1)
    return ok


def api_Frontend_flush_fst_dump(env, max_cycles=1) -> bool:
    """Flush FST waveform file buffers to disk."""
    ok = env.flush_waveform()
    if int(max_cycles) > 0:
        env.step(1)
    return ok

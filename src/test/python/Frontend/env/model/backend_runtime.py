from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Callable, Optional

from ..core.transactions import FtqIdxAheadTxn


@dataclass(frozen=True)
class CfVecSlotSnapshot:
    slot: int
    valid: bool = False
    foldpc: int = 0
    instr: int = 0
    is_rvc: bool = False
    pred_taken: bool = False
    fixed_taken: bool = False
    ftq_flag: int = 0
    ftq_value: int = 0
    ftq_offset: int = 0
    exception_bits: int = 0
    cross_page_ipf_fix: bool = False
    vtype_illegal: bool = True
    vtype_vma: bool = False
    vtype_vta: bool = False
    vtype_vsew: int = 0
    vtype_vlmul: int = 0
    specvtype_illegal: bool = True
    specvtype_vma: bool = False
    specvtype_vta: bool = False
    specvtype_vsew: int = 0
    specvtype_vlmul: int = 0


@dataclass(frozen=True)
class CfVecCycleSnapshot:
    cycle: int
    slots: tuple[CfVecSlotSnapshot, ...]


def capture_cfvec_snapshot(cycle: int, read_slot: Callable[[str, int], int]) -> CfVecCycleSnapshot:
    exception_signals = (
        (1, "cfvec_exception_vec_1"),
        (2, "cfvec_exception_vec_2"),
        (12, "cfvec_exception_vec_12"),
        (19, "cfvec_exception_vec_19"),
        (20, "cfvec_exception_vec_20"),
    )
    slots = []
    for slot in range(8):
        if not read_slot("cfvec_valid", slot):
            slots.append(CfVecSlotSnapshot(slot=slot))
            continue
        exception_bits = sum(
            (1 << bit) for bit, name in exception_signals if read_slot(name, slot)
        )
        pred_taken = bool(read_slot("cfvec_pred_taken", slot))
        slots.append(CfVecSlotSnapshot(
            slot=slot,
            valid=True,
            foldpc=read_slot("cfvec_foldpc", slot),
            instr=read_slot("cfvec_instr", slot),
            is_rvc=bool(read_slot("cfvec_is_rvc", slot)),
            pred_taken=pred_taken,
            fixed_taken=pred_taken,
            ftq_flag=read_slot("cfvec_ftq_ptr_flag", slot),
            ftq_value=read_slot("cfvec_ftq_ptr_value", slot),
            ftq_offset=read_slot("cfvec_ftq_offset", slot),
            exception_bits=exception_bits,
            cross_page_ipf_fix=bool(read_slot("cfvec_cross_page_ipf_fix", slot)),
            vtype_illegal=bool(read_slot("cfvec_vtype_illegal", slot)),
            vtype_vma=bool(read_slot("cfvec_vtype_vma", slot)),
            vtype_vta=bool(read_slot("cfvec_vtype_vta", slot)),
            vtype_vsew=read_slot("cfvec_vtype_vsew", slot),
            vtype_vlmul=read_slot("cfvec_vtype_vlmul", slot),
            specvtype_illegal=bool(read_slot("cfvec_specvtype_illegal", slot)),
            specvtype_vma=bool(read_slot("cfvec_specvtype_vma", slot)),
            specvtype_vta=bool(read_slot("cfvec_specvtype_vta", slot)),
            specvtype_vsew=read_slot("cfvec_specvtype_vsew", slot),
            specvtype_vlmul=read_slot("cfvec_specvtype_vlmul", slot),
        ))
    return CfVecCycleSnapshot(cycle=int(cycle), slots=tuple(slots))


@dataclass(frozen=True)
class BackendObservationSnapshot:
    from_ftq_wen: int = 0
    from_ftq_ftq_idx: int = 0
    from_ftq_start_pc_addr: int = 0
    ibuf_full: int = 0


@dataclass(frozen=True)
class BackendCycleActions:
    can_accept: int = 1
    backend_empty: int = 1
    wfi_req: int = 0
    commit_entry: Optional[Any] = None
    resolve_entries: tuple = ()
    call_ret_commit_group: tuple = ()
    ftq_idx_ahead: Optional[FtqIdxAheadTxn] = None
    redirect_payload: Optional[dict] = None
    vtype_commit: Optional[tuple[int, bool]] = None


__all__ = [
    "BackendCycleActions",
    "BackendObservationSnapshot",
    "CfVecCycleSnapshot",
    "CfVecSlotSnapshot",
    "capture_cfvec_snapshot",
]

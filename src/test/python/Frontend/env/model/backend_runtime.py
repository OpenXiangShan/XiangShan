from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Optional

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
    is_last: bool = False
    exception_bits: int = 0


@dataclass(frozen=True)
class CfVecCycleSnapshot:
    cycle: int
    slots: tuple[CfVecSlotSnapshot, ...]


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


__all__ = [
    "BackendCycleActions",
    "BackendObservationSnapshot",
    "CfVecCycleSnapshot",
    "CfVecSlotSnapshot",
]

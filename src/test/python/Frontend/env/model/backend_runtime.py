from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Optional

from ..core.transactions import FtqIdxAheadTxn


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


__all__ = ["BackendCycleActions", "BackendObservationSnapshot"]

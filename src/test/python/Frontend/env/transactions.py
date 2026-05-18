from __future__ import annotations

from dataclasses import dataclass
from typing import Optional, Sequence


@dataclass(frozen=True)
class ProgramImage:
    payload: bytes
    base_addr: int


@dataclass(frozen=True)
class GoldenTraceSource:
    path: str
    start_index: int = 0


@dataclass(frozen=True)
class RedirectTxn:
    target_pc: int
    reason: str
    max_cycles: int = 1000


@dataclass(frozen=True)
class CommitTarget:
    target_count: int
    max_cycles: int = 10000


@dataclass(frozen=True)
class PcSequenceExpectation:
    expected_pcs: Sequence[int]
    max_cycles: int = 5000


@dataclass(frozen=True)
class BpCtrlConfig:
    ubtb_enable: int = 1
    abtb_enable: int = 1
    mbtb_enable: int = 1
    tage_enable: int = 1
    sc_enable: int = 1
    ittage_enable: int = 1


@dataclass(frozen=True)
class IFUFetchTransaction:
    """Canonical IFU fetch observation used by the UVM-like pilot path.

    This is intentionally smaller than the historical SystemVerilog IFU xactions:
    it captures the common fetch item needed by the first Python bare-mode sample
    without forcing current agents/monitors into a full UVM hierarchy.
    """

    cycle: int
    pc: int
    instr: int
    is_rvc: bool
    valid: bool = True
    exception: Optional[str] = None
    fetch_path: str = "unknown"
    slot: int = 0

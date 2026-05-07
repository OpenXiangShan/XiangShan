from __future__ import annotations

from dataclasses import dataclass
from typing import Sequence


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

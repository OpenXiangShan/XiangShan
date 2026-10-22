from __future__ import annotations

from dataclasses import dataclass

from memblock_toffee.timing import CycleToken


def flush_cycle_transaction_id(cycle: int) -> str:
    return f"flush-cycle-{cycle}"


@dataclass(frozen=True, slots=True)
class FlushCommand:
    transaction_id: str
    enable: bool
    done: bool


@dataclass(frozen=True, slots=True)
class FlushExpectation:
    transaction_id: str
    pre_outer_enable: bool
    pre_backend_done: bool
    post_outer_enable: bool
    post_backend_done: bool


@dataclass(frozen=True, slots=True)
class FlushPreSample:
    token: CycleToken
    outer_enable: bool
    backend_done: bool


@dataclass(frozen=True, slots=True)
class FlushObservation:
    transaction_id: str
    pre_outer_enable: bool
    pre_backend_done: bool
    post_outer_enable: bool
    post_backend_done: bool
    cycle: int

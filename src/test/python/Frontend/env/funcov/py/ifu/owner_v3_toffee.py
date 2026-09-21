from __future__ import annotations

from .owner_v3_funcov import (
    OWNER_V3_COVERPOINTS,
    OWNER_V3_SAMPLER_BIN_KEYS,
    derive_owner_v3_from_source,
    handle_owner_v3_event,
)
from ...native_toffee import NativeDomainToffeeCoverage


class OwnerV3ToffeeCoverage(NativeDomainToffeeCoverage):
    """Native Toffee CovGroup sampling for owner V3 checked/source events."""

    def __init__(self, runtime, *, sink=None, audit_recorder=None) -> None:
        super().__init__(
            runtime,
            keys=OWNER_V3_SAMPLER_BIN_KEYS,
            coverpoints=OWNER_V3_COVERPOINTS,
            evaluate=None,
            sink=sink,
            audit_recorder=audit_recorder,
        )

    def handle_event(self, event) -> bool:
        return handle_owner_v3_event(self, event)

    def hit_count_by_bin_id(self, bin_id: str) -> int:
        if self._sink is None:
            return 0
        return self._sink.hit_count_by_bin_id(bin_id)

    def derive_from_source(self, bin_id, cycle, evidence) -> None:
        derive_owner_v3_from_source(self, bin_id, cycle, evidence)


__all__ = ["OwnerV3ToffeeCoverage"]

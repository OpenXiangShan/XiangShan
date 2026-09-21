from __future__ import annotations

from .mmio_v3_funcov import (
    MMIO_V3_COVERPOINTS,
    MMIO_V3_SAMPLER_BIN_KEYS,
    evaluate_mmio_v3_coverage,
    handle_mmio_v3_checked_event,
    initialize_mmio_v3_coverage_state,
)
from ...native_toffee import NativeDomainToffeeCoverage


class MmioV3ToffeeCoverage(NativeDomainToffeeCoverage):
    """Native Toffee CovGroup sampling for IFU MMIO V3."""

    def __init__(self, runtime, *, sink=None, audit_recorder=None) -> None:
        super().__init__(
            runtime,
            keys=MMIO_V3_SAMPLER_BIN_KEYS,
            coverpoints=MMIO_V3_COVERPOINTS,
            evaluate=evaluate_mmio_v3_coverage,
            reset=initialize_mmio_v3_coverage_state,
            sink=sink,
            audit_recorder=audit_recorder,
        )

    def handle_event(self, event) -> bool:
        return handle_mmio_v3_checked_event(self, event)


__all__ = ["MmioV3ToffeeCoverage"]

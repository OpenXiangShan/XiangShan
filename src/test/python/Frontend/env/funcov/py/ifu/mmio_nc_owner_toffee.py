from __future__ import annotations

from .instr_uncache_owner_funcov import (
    INSTR_UNCACHE_OWNER_COVERPOINTS,
    INSTR_UNCACHE_OWNER_SAMPLER_BIN_KEYS,
)
from .mmio_nc_owner_funcov import (
    MMIO_NC_OWNER_COVERPOINTS,
    MMIO_NC_OWNER_SAMPLER_BIN_KEYS,
    evaluate_mmio_nc_owner_coverage,
    reset_mmio_nc_owner_coverage_state,
)
from ...native_toffee import NativeDomainToffeeCoverage


class MmioNcOwnerToffeeCoverage(NativeDomainToffeeCoverage):
    """Native Toffee CovGroup sampling for MMIO/NC + InstrUncache owner bins."""

    def __init__(self, runtime, *, sink=None, audit_recorder=None) -> None:
        coverpoints = {**MMIO_NC_OWNER_COVERPOINTS, **INSTR_UNCACHE_OWNER_COVERPOINTS}
        keys = MMIO_NC_OWNER_SAMPLER_BIN_KEYS | INSTR_UNCACHE_OWNER_SAMPLER_BIN_KEYS
        super().__init__(
            runtime,
            keys=keys,
            coverpoints=coverpoints,
            evaluate=evaluate_mmio_nc_owner_coverage,
            reset=reset_mmio_nc_owner_coverage_state,
            sink=sink,
            audit_recorder=audit_recorder,
        )


__all__ = ["MmioNcOwnerToffeeCoverage"]

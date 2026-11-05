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

    def __init__(self, runtime, *, sink=None) -> None:
        coverpoints = {**MMIO_NC_OWNER_COVERPOINTS, **INSTR_UNCACHE_OWNER_COVERPOINTS}
        keys = MMIO_NC_OWNER_SAMPLER_BIN_KEYS | INSTR_UNCACHE_OWNER_SAMPLER_BIN_KEYS
        super().__init__(
            runtime,
            keys=keys,
            coverpoints=coverpoints,
            evaluate=evaluate_mmio_nc_owner_coverage,
            reset=reset_mmio_nc_owner_coverage_state,
            sink=sink,
        )


    # These pending-protocol diagnostics are read through the hub/sink by DUT
    # tests. Share only their explicit state fields, so hub reset replaces the
    # same dictionaries the native evaluator uses on its next callback.
    @property
    def _ifu_instr_uncache_owner_state(self):
        return self._runtime._ifu_instr_uncache_owner_state

    @_ifu_instr_uncache_owner_state.setter
    def _ifu_instr_uncache_owner_state(self, value):
        self._runtime._ifu_instr_uncache_owner_state = value

    @property
    def _ifu_mmio_nc_owner_state(self):
        return self._runtime._ifu_mmio_nc_owner_state

    @_ifu_mmio_nc_owner_state.setter
    def _ifu_mmio_nc_owner_state(self, value):
        self._runtime._ifu_mmio_nc_owner_state = value


__all__ = ["MmioNcOwnerToffeeCoverage"]

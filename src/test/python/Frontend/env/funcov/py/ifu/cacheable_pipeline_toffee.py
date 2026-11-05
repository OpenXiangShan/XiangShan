from __future__ import annotations

from .cacheable_pipeline_funcov import (
    IFU_CACHEABLE_PIPELINE_COVERPOINTS,
    IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS,
    evaluate_ifu_cacheable_pipeline_coverage,
    reset_ifu_cacheable_pipeline_state,
)
from ...native_toffee import NativeDomainToffeeCoverage


class IfuCacheablePipelineToffeeCoverage(NativeDomainToffeeCoverage):
    """Native Toffee CovGroup sampling for IFU cacheable pipeline."""

    def __init__(self, runtime, *, sink=None) -> None:
        super().__init__(
            runtime,
            keys=IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS,
            coverpoints=IFU_CACHEABLE_PIPELINE_COVERPOINTS,
            evaluate=evaluate_ifu_cacheable_pipeline_coverage,
            reset=reset_ifu_cacheable_pipeline_state,
            sink=sink,
        )


    # The cfVec domain consumes these validated S0->S1 transfer receipts.
    # Keep this bounded, resettable cross-domain proof store on the hub;
    # pending pipeline state remains private to the native producer model.
    @property
    def _ifu_cacheable_verified_windows(self):
        return self._runtime._ifu_cacheable_verified_windows

    @_ifu_cacheable_verified_windows.setter
    def _ifu_cacheable_verified_windows(self, value):
        self._runtime._ifu_cacheable_verified_windows = value


__all__ = ["IfuCacheablePipelineToffeeCoverage"]

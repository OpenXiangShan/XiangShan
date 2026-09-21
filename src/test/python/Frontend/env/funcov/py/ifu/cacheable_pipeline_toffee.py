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

    def __init__(self, runtime, *, sink=None, audit_recorder=None) -> None:
        super().__init__(
            runtime,
            keys=IFU_CACHEABLE_PIPELINE_SAMPLER_BIN_KEYS,
            coverpoints=IFU_CACHEABLE_PIPELINE_COVERPOINTS,
            evaluate=evaluate_ifu_cacheable_pipeline_coverage,
            reset=reset_ifu_cacheable_pipeline_state,
            sink=sink,
            audit_recorder=audit_recorder,
        )


__all__ = ["IfuCacheablePipelineToffeeCoverage"]

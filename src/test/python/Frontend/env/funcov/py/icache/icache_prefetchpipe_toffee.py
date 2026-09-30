from __future__ import annotations

from .icache_prefetchpipe_funcov import (
    ICACHE_PREFETCHPIPE_COVERPOINTS,
    ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS,
    evaluate_icache_prefetchpipe_coverage,
    reset_icache_prefetchpipe_coverage_state,
)
from ...native_toffee import NativeCycleToffeeCoverage


class ICachePrefetchpipeToffeeCoverage(NativeCycleToffeeCoverage):
    """Native Toffee CovGroup sampling for the ICache PrefetchPipe domain."""

    def __init__(self, runtime, *, sink=None, audit_recorder=None) -> None:
        super().__init__(
            runtime,
            keys=ICACHE_PREFETCHPIPE_SAMPLER_BIN_KEYS,
            coverpoints=ICACHE_PREFETCHPIPE_COVERPOINTS,
            evaluate=evaluate_icache_prefetchpipe_coverage,
            reset=reset_icache_prefetchpipe_coverage_state,
            sink=sink,
            audit_recorder=audit_recorder,
            flag_key=lambda key: key[1],
        )


__all__ = ["ICachePrefetchpipeToffeeCoverage"]

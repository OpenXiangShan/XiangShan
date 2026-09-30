from __future__ import annotations

from .icache_waylookup_funcov import (
    ICACHE_WAYLOOKUP_COVERPOINTS,
    ICACHE_WAYLOOKUP_SAMPLER_BIN_KEYS,
    evaluate_icache_waylookup_coverage,
    reset_icache_waylookup_coverage_state,
)
from ...native_toffee import NativeCycleToffeeCoverage


class ICacheWaylookupToffeeCoverage(NativeCycleToffeeCoverage):
    """Native Toffee CovGroup sampling for the ICache WayLookup domain."""

    def __init__(self, runtime, *, sink=None, audit_recorder=None) -> None:
        super().__init__(
            runtime,
            keys=ICACHE_WAYLOOKUP_SAMPLER_BIN_KEYS,
            coverpoints=ICACHE_WAYLOOKUP_COVERPOINTS,
            evaluate=evaluate_icache_waylookup_coverage,
            reset=reset_icache_waylookup_coverage_state,
            sink=sink,
            audit_recorder=audit_recorder,
            flag_key=lambda key: key[1],
        )


__all__ = ["ICacheWaylookupToffeeCoverage"]

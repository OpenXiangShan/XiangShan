from __future__ import annotations

from .icache_hitmiss_funcov import (
    ICACHE_HITMISS_COVERPOINTS,
    ICACHE_HITMISS_SAMPLER_BIN_KEYS,
    evaluate_icache_hitmiss_coverage,
    reset_icache_hitmiss_coverage_state,
)
from ...native_toffee import NativeCycleToffeeCoverage


class ICacheHitMissToffeeCoverage(NativeCycleToffeeCoverage):
    """Native Toffee CovGroup sampling for the ICache hit/miss domain."""

    def __init__(self, runtime, *, sink=None, audit_recorder=None) -> None:
        super().__init__(
            runtime,
            keys=ICACHE_HITMISS_SAMPLER_BIN_KEYS,
            coverpoints=ICACHE_HITMISS_COVERPOINTS,
            evaluate=evaluate_icache_hitmiss_coverage,
            reset=reset_icache_hitmiss_coverage_state,
            sink=sink,
            audit_recorder=audit_recorder,
            flag_key=lambda key: key[1],
        )


__all__ = ["ICacheHitMissToffeeCoverage"]

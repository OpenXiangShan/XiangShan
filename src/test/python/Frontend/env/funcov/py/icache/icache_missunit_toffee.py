from __future__ import annotations

from .icache_missunit_funcov import (
    ICACHE_MISSUNIT_COVERPOINTS,
    ICACHE_MISSUNIT_SAMPLER_BIN_KEYS,
    evaluate_icache_missunit_coverage,
    reset_icache_missunit_coverage_state,
)
from ...native_toffee import NativeCycleToffeeCoverage


class ICacheMissunitToffeeCoverage(NativeCycleToffeeCoverage):
    """Native Toffee CovGroup sampling for the ICache MissUnit domain."""

    def __init__(self, runtime, *, sink=None, audit_recorder=None) -> None:
        super().__init__(
            runtime,
            keys=ICACHE_MISSUNIT_SAMPLER_BIN_KEYS,
            coverpoints=ICACHE_MISSUNIT_COVERPOINTS,
            evaluate=evaluate_icache_missunit_coverage,
            reset=reset_icache_missunit_coverage_state,
            sink=sink,
            audit_recorder=audit_recorder,
            flag_key=lambda key: key[1],
        )


__all__ = ["ICacheMissunitToffeeCoverage"]

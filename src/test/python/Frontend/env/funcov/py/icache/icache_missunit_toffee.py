from __future__ import annotations

from .icache_missunit_funcov import (
    ICACHE_MISSUNIT_COVERPOINTS,
    ICACHE_MISSUNIT_SAMPLER_BIN_KEYS,
    ICACHE_REFILL_ISOLATION_BINS,
    ICACHE_REFILL_ISOLATION_POINT,
    evaluate_icache_missunit_coverage,
    reset_icache_missunit_coverage_state,
)
from ...native_toffee import NativeCycleToffeeCoverage


class ICacheMissunitToffeeCoverage(NativeCycleToffeeCoverage):
    """Native Toffee CovGroup sampling for the ICache MissUnit domain."""

    def __init__(self, runtime, *, sink=None) -> None:
        super().__init__(
            runtime,
            keys=ICACHE_MISSUNIT_SAMPLER_BIN_KEYS,
            coverpoints=ICACHE_MISSUNIT_COVERPOINTS,
            evaluate=evaluate_icache_missunit_coverage,
            reset=reset_icache_missunit_coverage_state,
            sink=sink,
            flag_key=lambda key: key[1],
        )

    def _build_native_groups(self) -> None:
        for name in ICACHE_REFILL_ISOLATION_BINS.values():
            self._coverpoints_by_key["icache_missunit_refill", name] = ICACHE_REFILL_ISOLATION_POINT
        super()._build_native_groups()


__all__ = ["ICacheMissunitToffeeCoverage"]

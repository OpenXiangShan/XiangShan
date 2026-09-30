from __future__ import annotations

from .icache_mainpipe_funcov import (
    ICACHE_MAINPIPE_COVERPOINTS,
    ICACHE_MAINPIPE_SAMPLER_BIN_KEYS,
    evaluate_icache_mainpipe_coverage,
    reset_icache_mainpipe_coverage_state,
)
from ...native_toffee import NativeCycleToffeeCoverage


class ICacheMainpipeToffeeCoverage(NativeCycleToffeeCoverage):
    """Native Toffee CovGroup sampling for the ICache MainPipe domain."""

    def __init__(self, runtime, *, sink=None, audit_recorder=None) -> None:
        super().__init__(
            runtime,
            keys=ICACHE_MAINPIPE_SAMPLER_BIN_KEYS,
            coverpoints=ICACHE_MAINPIPE_COVERPOINTS,
            evaluate=evaluate_icache_mainpipe_coverage,
            reset=reset_icache_mainpipe_coverage_state,
            sink=sink,
            audit_recorder=audit_recorder,
            flag_key=lambda key: key[1],
        )


__all__ = ["ICacheMainpipeToffeeCoverage"]

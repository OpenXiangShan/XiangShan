from __future__ import annotations

from .two_fetch_funcov import (
    TWO_FETCH_COVERPOINTS,
    TWO_FETCH_SAMPLER_BIN_KEYS,
    evaluate_two_fetch_coverage,
    reset_ftq_coverage_state,
)
from ...native_toffee import NativeDomainToffeeCoverage


class TwoFetchToffeeCoverage(NativeDomainToffeeCoverage):
    """Native Toffee CovGroup sampling for the FTQ two-fetch domain."""

    def __init__(self, runtime, *, sink=None, audit_recorder=None) -> None:
        super().__init__(
            runtime,
            keys=TWO_FETCH_SAMPLER_BIN_KEYS,
            coverpoints=TWO_FETCH_COVERPOINTS,
            evaluate=evaluate_two_fetch_coverage,
            reset=reset_ftq_coverage_state,
            sink=sink,
            audit_recorder=audit_recorder,
        )


__all__ = ["TwoFetchToffeeCoverage"]

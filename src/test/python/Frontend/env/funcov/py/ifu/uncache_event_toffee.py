from __future__ import annotations

from ...native_toffee import NativeDomainToffeeCoverage
from ...runtime_context import UNCACHE_EVENT_SAMPLER_BIN_KEYS


UNCACHE_EVENT_COVERPOINTS = {
    "uncache_ordering": "mmio_commit_gate",
    "uncache_path_switch": "redirect_recovery",
    "fetch_path_switch": "redirect_recovery",
}


def evaluate_uncache_event_coverage(runtime, env, cycle: int):
    from ...native_toffee import FlagMarkTarget

    flags = {key: False for key in UNCACHE_EVENT_SAMPLER_BIN_KEYS}
    evidence: dict = {}
    target = FlagMarkTarget(flags=flags, evidence=evidence)
    runtime._sample_uncache_cycle_state(
        env.dut, int(cycle), env, mark_target=target
    )
    return flags, evidence


class UncacheEventToffeeCoverage(NativeDomainToffeeCoverage):
    """Native Toffee CovGroup sampling for uncache path and ordering events."""

    def __init__(self, runtime, *, sink=None, audit_recorder=None) -> None:
        super().__init__(
            runtime,
            keys=UNCACHE_EVENT_SAMPLER_BIN_KEYS,
            coverpoints=UNCACHE_EVENT_COVERPOINTS,
            evaluate=evaluate_uncache_event_coverage,
            sink=sink,
            audit_recorder=audit_recorder,
        )

__all__ = [
    "UNCACHE_EVENT_COVERPOINTS",
    "UNCACHE_EVENT_SAMPLER_BIN_KEYS",
    "UncacheEventToffeeCoverage",
    "evaluate_uncache_event_coverage",
]

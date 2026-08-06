"""FTQ/ICache WayLookup result and fallback groups."""

from .two_fetch_funcov import TWO_FETCH_COVERPOINTS, TWO_FETCH_SAMPLER_BIN_KEYS, sample_two_fetch_coverage

COVERAGE_GROUPS = {
    name: TWO_FETCH_COVERPOINTS[name]
    for name in ("two_fetch_waylookup_result", "two_fetch_waylookup_block_reason")
}
SAMPLER_BIN_KEYS = frozenset(key for key in TWO_FETCH_SAMPLER_BIN_KEYS if key[0] in COVERAGE_GROUPS)

def sample_waylookup_coverage(recorder, env, cycle: int) -> None:
    sample_two_fetch_coverage(recorder, env, cycle, COVERAGE_GROUPS)

__all__ = ["COVERAGE_GROUPS", "SAMPLER_BIN_KEYS", "sample_waylookup_coverage"]

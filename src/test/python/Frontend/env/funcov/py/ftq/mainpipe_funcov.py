"""MainPipe dual-fetch hit pattern and completion groups."""

from .two_fetch_funcov import TWO_FETCH_COVERPOINTS, TWO_FETCH_SAMPLER_BIN_KEYS, sample_two_fetch_coverage

COVERAGE_GROUPS = {
    name: TWO_FETCH_COVERPOINTS[name]
    for name in ("two_fetch_mainpipe_hit_pattern", "two_fetch_mainpipe_completion")
}
SAMPLER_BIN_KEYS = frozenset(key for key in TWO_FETCH_SAMPLER_BIN_KEYS if key[0] in COVERAGE_GROUPS)

def sample_mainpipe_coverage(recorder, env, cycle: int) -> None:
    sample_two_fetch_coverage(recorder, env, cycle, COVERAGE_GROUPS)

__all__ = ["COVERAGE_GROUPS", "SAMPLER_BIN_KEYS", "sample_mainpipe_coverage"]

"""Unit tests for shared directed ICache test helpers."""

from tests.py.icache.directed_dut.support import wait_coverage_hits


class _Coverage:
    def __init__(self, hits):
        self.hits = set(hits)

    def key_hit(self, group, bin_name):
        return (group, bin_name) in self.hits


class _Env:
    current_cycle = 0

    def __init__(self, hits):
        self.functional_coverage = _Coverage(hits)

    def step(self, cycles):
        self.current_cycle += int(cycles)


def test_wait_coverage_hits_handles_multiple_already_hit_targets():
    env = _Env({("group_a", "bin_a"), ("group_b", "bin_b")})

    wait_coverage_hits(
        env,
        (("group_a", "bin_a"), ("group_b", "bin_b")),
        max_cycles=1,
    )

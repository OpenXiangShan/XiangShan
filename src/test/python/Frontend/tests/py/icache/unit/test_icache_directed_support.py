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
    assert env.current_cycle == 0


class _StepwiseCoverage:
    def __init__(self, schedule):
        self.schedule = schedule
        self.cycle = 0

    def key_hit(self, group, bin_name):
        return self.cycle in self.schedule.get((group, bin_name), ())


class _StepwiseEnv:
    def __init__(self, schedule):
        self.functional_coverage = _StepwiseCoverage(schedule)
        self.current_cycle = 0

    def step(self, cycles):
        self.current_cycle += int(cycles)
        self.functional_coverage.cycle = self.current_cycle


def test_wait_coverage_hits_waits_until_each_target_is_sampled():
    env = _StepwiseEnv({
        ("group_a", "bin_a"): {1},
        ("group_b", "bin_b"): {2},
    })

    wait_coverage_hits(
        env,
        (("group_a", "bin_a"), ("group_b", "bin_b")),
        max_cycles=4,
    )

    assert env.current_cycle == 2


def test_wait_coverage_hits_counts_a_hit_sampled_by_the_final_step():
    env = _StepwiseEnv({("group_a", "bin_a"): {1}})

    wait_coverage_hits(
        env,
        (("group_a", "bin_a"),),
        max_cycles=1,
    )

    assert env.current_cycle == 1


def test_wait_coverage_hits_reports_only_the_targets_still_missing():
    env = _StepwiseEnv({("group_a", "bin_a"): {1}})

    try:
        wait_coverage_hits(
            env,
            (("group_a", "bin_a"), ("group_b", "bin_b")),
            max_cycles=1,
        )
    except AssertionError as error:
        detail = error.args[0]
    else:
        raise AssertionError("wait should have timed out")

    assert detail["missing"] == ["group_b.bin_b"]
    assert detail["max_cycles"] == 1
    assert env.current_cycle == 1

"""Unit tests for shared directed ICache test helpers."""

import hashlib
from types import SimpleNamespace

import pytest

from env.core.frontend_env import FrontendEnv
from env.runtime.dut_factory import FakeDUTFrontend
from tests.py.icache.directed_dut.support import (
    _build_bpu_training_program,
    _warm_bpu_training_stream,
    initialize_bpu_s3_stream,
    wait_coverage_hits,
)


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


def test_bpu_training_program_preserves_the_original_instruction_bytes():
    payload = _build_bpu_training_program()

    assert len(payload) == 512
    # Golden digest of the original eight-block RVC stream, before relocation.
    assert hashlib.sha256(payload).hexdigest() == (
        "11913abef48b491a5e95ecc87adb40c82ea6d0dc73f9750ac50a6fd4ed140961"
    )


class _BpuTrainingCoverage:
    def __init__(self, env):
        self.env = env

    def key_hit(self, group, bin_name):
        assert (group, bin_name) in {
            ("ifu_cfi_decode_type", "jal"),
            ("two_fetch_ftq_eligibility", "eligible_dual"),
            ("two_fetch_delivery", "dual_fire"),
        }
        return (
            self.env.current_cycle >= self.env.ready_cycle
            and bin_name != self.env.missing
        )

    def _read_first_dut_signal(self, dut, names):
        assert dut is self.env.dut
        suffixes = {
            "ftq_valid": "io_fromFtq_toMainPipe_valid",
            "ftq_ready": "io_fromFtq_toMainPipe_ready",
            "ftq_req1_valid": "io_fromFtq_toMainPipe_bits_req_1_valid",
        }
        for key, suffix in suffixes.items():
            if any(name.endswith(suffix) for name in names):
                if self.env.missing == "unavailable_" + key:
                    return None
                return int(
                    self.env.current_cycle >= self.env.ready_cycle
                    and key != self.env.missing
                )
        raise AssertionError({"unexpected_signals": names})


class _BpuTrainingEnv:
    def __init__(self, *, ready_cycle=1, missing=None):
        self.current_cycle = 0
        self.ready_cycle = ready_cycle
        self.missing = missing
        self.dut = object()
        self.steps = []
        self.functional_coverage = _BpuTrainingCoverage(self)
        self.backend_model = SimpleNamespace(
            get_stats=lambda: {
                "commit_count": int(
                    self.current_cycle >= self.ready_cycle and self.missing != "commit"
                ),
            }
        )
        self.icache_agent = SimpleNamespace(
            get_stats=lambda: {
                "req_count": (
                    4
                    if self.current_cycle >= self.ready_cycle and self.missing != "requests"
                    else 3
                ),
            }
        )
        self.monitor = SimpleNamespace(get_errors=lambda: ["retained checker error"])

    def step(self, cycles):
        self.steps.append(cycles)
        self.current_cycle += int(cycles)


def test_bpu_warmup_accepts_the_final_cycle_and_always_steps_before_sampling():
    env = _BpuTrainingEnv(ready_cycle=2)

    result = _warm_bpu_training_stream(env, max_cycles=2)

    assert result == {
        "cycle": 2,
        "icache": {"req_count": 4},
        "backend": {"commit_count": 1},
    }
    assert env.steps == [1, 1]


@pytest.mark.parametrize(
    "missing",
    [
        "commit", "requests", "jal", "eligible_dual", "dual_fire",
        "ftq_valid", "ftq_ready", "ftq_req1_valid",
    ],
)
def test_bpu_warmup_requires_all_training_conditions(missing):
    env = _BpuTrainingEnv(missing=missing)

    with pytest.raises(AssertionError) as raised:
        _warm_bpu_training_stream(env, max_cycles=2)

    assert env.steps == [1, 1]
    assert raised.value.args[0] == {
        "reason": "short-block loop did not reach dual-eligible frontend execution",
        "max_cycles": 2,
        "icache": env.icache_agent.get_stats(),
        "backend": env.backend_model.get_stats(),
        "monitor_errors": ["retained checker error"],
    }


def test_bpu_warmup_preserves_the_environment_cycle_limit(monkeypatch):
    monkeypatch.setenv("TB_TWO_FETCH_TRAIN_MAX_CYCLES", "2")
    env = _BpuTrainingEnv(ready_cycle=3)

    with pytest.raises(AssertionError) as raised:
        _warm_bpu_training_stream(env)

    assert env.current_cycle == 2
    assert raised.value.args[0]["max_cycles"] == 2


def test_bpu_warmup_reports_an_unavailable_signal_without_further_steps():
    env = _BpuTrainingEnv(missing="unavailable_ftq_valid")

    with pytest.raises(AssertionError) as raised:
        _warm_bpu_training_stream(env, max_cycles=2)

    assert env.steps == [1]
    assert raised.value.args[0]["label"] == "ftq_valid"
    assert raised.value.args[0]["reason"] == (
        "required directed-flow observation is unavailable"
    )


def test_bpu_stream_loads_before_reset_and_configures_icache_after_warmup(monkeypatch):
    env = FrontendEnv(FakeDUTFrontend())
    env.ready_cycle = 24  # Twenty reset cycles, one release cycle, three warmup cycles.
    env.missing = None
    env.functional_coverage = _BpuTrainingCoverage(env)
    monkeypatch.setattr(
        env.backend_model, "get_stats",
        lambda: {"commit_count": int(env.current_cycle >= 24)},
    )
    monkeypatch.setattr(
        env.icache_agent, "get_stats",
        lambda: {"req_count": 4 if env.current_cycle >= 24 else 0},
    )
    samples = []
    env.register_cycle_observer(
        lambda _cycle, active: samples.append(
            (
                int(active.dut.reset.value),
                active.memory.read_u16(0x80000000),
                active.icache_agent.miss_latency,
            )
        )
    )

    initialize_bpu_s3_stream(env, seed=0x6617)

    assert env.current_cycle == 24
    assert [reset for reset, _, _ in samples] == [1] * 20 + [0] * 4
    assert all(word == 0x0001 and latency == 20 for _, word, latency in samples)
    assert env.init_result.completed and env.init_result.end_cycle == 21
    assert env.monitor.expected_pc == 0x80000000
    assert env.icache_agent.hit_latency == 1
    assert env.icache_agent.miss_latency == 32
    assert env.icache_agent.miss_rate == 0.0

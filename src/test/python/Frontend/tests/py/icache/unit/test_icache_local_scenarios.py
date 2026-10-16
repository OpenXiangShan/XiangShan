"""Behavioral contracts for locally owned directed ICache scenarios."""

import hashlib
import os
from pathlib import Path
import subprocess
import sys
from types import SimpleNamespace

import pytest

from env.core.frontend_env import FrontendEnv
from env.runtime.dut_factory import FakeDUTFrontend
from env.runtime.frontend_init import (
    LoadProgram,
    ProgramImage,
    initialize_frontend,
    run_initialization_steps,
)
from env.support.env_config import EnvConfig
from tests.py.icache.directed_dut import (
    scenarios,
    support,
    programs,
)

from tests.py.icache.directed_dut import (
    test_icache_mainpipe_partial_closure_dut as protection_cases,
)
from tests.py.icache.directed_dut import (
    test_icache_prefetchpipe_closure_dut as prefetch_cases,
)


def test_directed_modules_import_when_external_testcase_modules_are_unavailable():
    frontend_root = Path(__file__).resolve().parents[4]
    script = """
import importlib
from pathlib import Path
import sys

class BlockExternalTestcases:
    def find_spec(self, fullname, path=None, target=None):
        if fullname.startswith(('tests.py.jiabowen', 'tests.py.zhaoxinran')):
            raise ImportError('external testcase dependency: ' + fullname)
        return None

sys.meta_path.insert(0, BlockExternalTestcases())
for path in sorted(Path('tests/py/icache/directed_dut').glob('*.py')):
    importlib.import_module('tests.py.icache.directed_dut.' + path.stem)
"""
    result = subprocess.run(
        [sys.executable, "-c", script],
        cwd=frontend_root,
        env={**os.environ, "PYTHONDONTWRITEBYTECODE": "1"},
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert result.returncode == 0, result.stdout + result.stderr


def test_dual_fetch_program_preserves_the_original_bytes_and_differs_from_bpu():
    payload = programs.build_dual_fetch_program()
    assert len(payload) == 288
    assert hashlib.sha256(payload).hexdigest() == (
        "a915b19e1a8e6170a95220adbf5dd9d12fa819603f727879c6358e2922f8094d"
    )
    assert payload != programs.build_bpu_training_program()


class _RecordingEnv:
    def __init__(self):
        self.current_cycle = 0
        self.config = EnvConfig()
        self.page_table = SimpleNamespace(mode="bare")
        self.backend_model = SimpleNamespace()
        self.branch_checker = SimpleNamespace(
            get_stats=lambda: {"by_type": {"jump": 20, "branch": 20}}
        )
        self.monitor = SimpleNamespace(get_errors=lambda: [])
        self.trace = []

    def load_program(self, data, base_addr):
        self.trace.append(("load", int(base_addr), bytes(data)))

    def step(self, cycles):
        self.current_cycle += int(cycles)
        self.trace.append(("step", int(cycles)))

    def _emit_event(self, name, payload, **kwargs):
        self.trace.append(("event", name, payload))


def test_nop_loaders_preserve_their_distinct_cycle_counts():
    env = _RecordingEnv()
    support.load_nop_program(env, words=4, base_addr=0x80040000, step_cycles=0)
    assert env.trace == [("load", 0x80040000, b"\x13\0\0\0" * 4)]
    assert env.current_cycle == 0

    support.load_nop_program(env, words=4)
    assert env.trace[-2:] == [
        ("load", 0x80000000, b"\x13\0\0\0" * 4),
        ("step", 1),
    ]
    assert env.current_cycle == 1


def test_dual_fetch_loader_does_not_advance_or_reset_the_stream():
    env = _RecordingEnv()
    run_initialization_steps(
        env,
        (
            LoadProgram(
                ProgramImage(
                    programs.build_dual_fetch_program(), programs.DUAL_FETCH_BASE
                )
            ),
        ),
    )
    assert env.trace == [("load", 0x80060000, programs.build_dual_fetch_program())]
    assert env.current_cycle == 0


def test_cacheable_startup_loads_and_configures_before_reset():
    env = FrontendEnv(FakeDUTFrontend())
    for aliases in support._MAINPIPE_SIGNALS.values():
        setattr(env.dut, aliases[0], SimpleNamespace(value=0))
    observed = []
    env.register_cycle_observer(
        lambda _, active: observed.append(
            (active.memory.read_u32(0x80040000), active.icache_agent.miss_latency)
        )
    )

    samples = support.register_mainpipe_observer(env)
    initialize_frontend(
        env,
        support.make_cacheable_init_config(
            env.config, programs.CACHEABLE_BASE, latency=7
        ),
    )

    assert env.current_cycle == 21
    assert observed == [(0x13, 7)] * 21
    assert len(samples) == 21
    assert env.monitor.expected_pc == 0x80040000
    assert env.init_result.completed


def _dual_sample(cycle, *, misses=(0, 0), fire=0, raw=0, registered=0):
    sample = {
        "cycle": cycle,
        "s1_valid": 1,
        "s1_req1_valid": 1,
        "s1_exception": 0,
        "s1_mmio": 0,
        "s1_flush": 0,
        "s1_fire": fire,
        "s1_ftq0_flag": 0,
        "s1_ftq0_value": 1,
        "s1_ftq1_flag": 0,
        "s1_ftq1_value": 2,
        "s1_req0_addr": 0x80060000 >> 1,
        "s1_req1_addr": 0x80060040 >> 1,
    }
    for index in range(4):
        sample[f"should_fetch_{index}"] = misses[index // 2] if index % 2 == 0 else 0
        sample[f"mshr_valid_{index}"] = raw
        sample[f"mshr_valid_reg_{index}"] = registered
    return sample


@pytest.mark.parametrize(
    "misses,pattern",
    [
        ((0, 0), "hit_hit"),
        ((0, 1), "hit_miss"),
        ((1, 0), "miss_hit"),
        ((1, 1), "miss_miss"),
    ],
)
def test_dual_transactions_preserve_patterns_addresses_and_registered_evidence(
    misses, pattern
):
    samples = [
        _dual_sample(0, misses=misses, raw=1),
        _dual_sample(1, misses=misses, registered=1),
        _dual_sample(2, misses=misses, fire=1),
        _dual_sample(3, misses=misses, fire=1),
    ]
    transactions = support.completed_dual_transactions(samples)
    assert len(transactions) == 1
    transaction = transactions[0]
    assert transaction["pattern"] == pattern
    assert transaction["req0_addr"] == 0x80060000
    assert transaction["req1_addr"] == 0x80060040
    assert transaction["fire_cycle"] == 2
    assert transaction["registered_exact"] is True


def test_dual_transactions_reject_late_registered_evidence():
    transactions = support.completed_dual_transactions(
        [
            _dual_sample(0, misses=(1, 1), raw=1),
            _dual_sample(2, misses=(1, 1), registered=1),
            _dual_sample(3, misses=(1, 1), fire=1),
        ]
    )
    assert transactions[0]["registered_exact"] is False


def test_mainpipe_wait_accepts_a_condition_produced_by_the_final_step():
    env = _RecordingEnv()
    assert support.run_until(env, lambda: env.current_cycle == 2, max_cycles=2)
    assert env.trace == [("step", 1), ("step", 1)]


class _CommitWait:
    def __init__(self, target):
        self.target = target

    def run(self, env):
        env.trace.append(
            ("commit_wait", self.target.target_count, self.target.max_cycles)
        )
        return self.target.target_count


@pytest.mark.parametrize(
    "seed,scenario,program_sha256,target_commits,max_cycles,resolve_delay,derived_seed",
    [
        (
            0,
            "run_multi_branch_random_positions",
            "e3ca5bde944e5f36fdd834016e23c232e6d1251dcd763be3dc13c5bf5044ab9c",
            72,
            5000,
            (1, 10),
            17892110306549710766,
        ),
        (
            0,
            "run_large_loop_multi_segment",
            "119cb65155d8c687136cf5e652cfbb20c6d0dd78030ad0eb1ee12edd5eb7c010",
            347,
            30000,
            (1, 13),
            6126520146403644378,
        ),
        (
            1,
            "run_multi_branch_random_positions",
            "6d7a8066c5674614ed4afecf9a40e03c9978d0d79e26063eec5eafe1c92b3430",
            33,
            5000,
            (2, 10),
            14142776992509333658,
        ),
        (
            1,
            "run_large_loop_multi_segment",
            "6baa6074ae4bcb5feeff7edef7efb590d4a32dca62c91c3c141932121504e0ed",
            344,
            30000,
            (1, 4),
            8057941743848588297,
        ),
        (
            0x6605,
            "run_multi_branch_random_positions",
            "e8de737625674fa1ff25b12358f651007cca6b28cb6e86bd3168367a5afbcee0",
            24,
            5000,
            (1, 8),
            6155237503232082560,
        ),
        (
            0x6605,
            "run_large_loop_multi_segment",
            "df0a1dc4d023f5e426508e9e24a13b7ecc6bbe5b6c98ab4adcf2c00b736c281d",
            429,
            30000,
            (2, 5),
            10716707941814910885,
        ),
    ],
)
def test_random_branch_scenarios_preserve_programs_timing_and_replay(
    monkeypatch,
    seed,
    scenario,
    program_sha256,
    target_commits,
    max_cycles,
    resolve_delay,
    derived_seed,
):
    monkeypatch.setenv("TB_SEED", str(seed))
    owner = prefetch_cases if scenario == "run_large_loop_multi_segment" else scenarios
    monkeypatch.setattr(owner, "RunUntilCommitSequence", _CommitWait)
    env = _RecordingEnv()
    getattr(owner, scenario)(env)

    load = next(event for event in env.trace if event[0] == "load")
    assert load[1] == 0x80000000
    assert hashlib.sha256(load[2]).hexdigest() == program_sha256
    assert ("commit_wait", target_commits, max_cycles) in env.trace
    assert env.current_cycle == 1
    assert (
        env.backend_model.resolve_min_delay,
        env.backend_model.resolve_max_delay,
    ) == resolve_delay
    record = env.random_scenario_records[0]
    assert record["base_seed"] == seed
    assert record["seed"] == derived_seed
    assert record["parameters"]["program_sha256"] == program_sha256


class _StopScenario(Exception):
    pass


def _stop_translation_preparation(step, env):
    raise _StopScenario(step.scenario)


@pytest.mark.parametrize(
    "scenario_name", ["run_pmp_execute_denied", "run_cross_page_pma_path"]
)
def test_protection_scenarios_preserve_permissions_and_page_boundaries(
    monkeypatch, scenario_name
):
    monkeypatch.setattr(
        protection_cases, "initialize_frontend", lambda *args, **kwargs: None
    )
    monkeypatch.setattr(
        protection_cases,
        "_randomize_translation_timing",
        lambda env, scenario: scenario,
    )
    monkeypatch.setattr(
        protection_cases.PrepareTranslationScenario, "run", _stop_translation_preparation
    )
    with pytest.raises(_StopScenario) as raised:
        getattr(protection_cases, scenario_name)(SimpleNamespace(config=EnvConfig()))
    scenario = raised.value.args[0]
    if scenario_name == "run_pmp_execute_denied":
        assert (scenario.va, scenario.pa) == (0x80000000, 0x80001000)
        assert scenario.expected_result == "access_fault"
        assert not scenario.pmp_entries[0].config.execute
        assert scenario.pma_entries[0].config.cacheable
    else:
        assert (scenario.va, scenario.pa) == (0x80000FFE, 0x80001FFE)
        assert scenario.page_count == 2
        assert [entry.addr for entry in scenario.pma_entries] == [
            0x80001000,
            0x80002000,
        ]
        assert [entry.config.cacheable for entry in scenario.pma_entries] == [
            True,
            False,
        ]


def test_sector_lane_scenario_preserves_reuse_without_another_ptw(monkeypatch):
    monkeypatch.setattr(
        protection_cases, "initialize_frontend", lambda *args, **kwargs: None
    )
    monkeypatch.setattr(
        protection_cases,
        "_randomize_translation_timing",
        lambda env, scenario: scenario,
    )

    class SequenceProbe:
        def __init__(self, actions):
            self.actions = actions

        def run(self, env):
            raise _StopScenario(self.actions)

    monkeypatch.setattr(protection_cases, "TranslationScenarioSequence", SequenceProbe)
    with pytest.raises(_StopScenario) as raised:
        protection_cases.run_sector_lane_refill(SimpleNamespace(config=EnvConfig()))
    first, second = raised.value.args[0]
    assert first.page_indexes == (0,)
    assert first.scenario.page_count == 2
    assert second.page_indexes == (1,)
    assert second.reuse_previous is True and second.expect_ptw is False


def test_pbmt_redirect_scenario_configures_agents_before_starting(monkeypatch):
    calls = []
    monkeypatch.setattr(
        protection_cases,
        "_prepare_sv39_dual_nc_cacheable_stream",
        lambda env: ([(0x80000000, 1, True)], []),
    )

    def stop_at_startup(env, config):
        for step in config.before_reset:
            step.run(env)
        calls.append(("startup", {"reset_vector": config.reset_vector}))
        raise _StopScenario

    monkeypatch.setattr(protection_cases, "initialize_frontend", stop_at_startup)
    env = SimpleNamespace(
        config=EnvConfig(),
        current_cycle=0,
        icache_agent=SimpleNamespace(
            configure=lambda **kwargs: calls.append(("icache", kwargs))
        ),
        uncache_agent=SimpleNamespace(
            configure=lambda **kwargs: calls.append(("uncache", kwargs))
        ),
    )
    with pytest.raises(_StopScenario):
        protection_cases.run_cacheable_pending_redirect_to_pbmt_nc(env)
    assert calls == [
        ("uncache", {"latency": 16, "mmio_latency": 16}),
        (
            "icache",
            {"hit_latency": 64, "miss_latency": 64, "miss_rate": 0.0, "seed": 4},
        ),
        ("startup", {"reset_vector": 0x80002000}),
    ]

"""Offline checks against the current public APIs, without compiled DUT imports."""
from dataclasses import replace
from types import SimpleNamespace as NS
import os
from pathlib import Path
import subprocess
import sys

import pytest

from env.core.frontend_env import FrontendEnv
from env.funcov.recorder import FrontendFuncovSampleHub
from env.runtime.dut_factory import FakeDUTFrontend
from env.sequences import TranslationScenarioBuilder

from .scenario_coordinator import BIN951_IFU_SIGNAL_NAMES, ScenarioCoordinator, TimingPlan
from .random_case_test_support import _FakeCoordinatorEnv, _coordinator_program_map, _fake_bin951_dut
from .test_translation_attributes import attribute_plan_from_dict, TRANSLATION_SCHEMA_VERSION
from .translation_timing import build_translation_scenario


def test_native_snapshot_begins_after_backend_drive_before_random_observers(monkeypatch):
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    order = []
    env.functional_coverage = NS(
        native_toffee_runtime_enabled=True,
        begin_cycle_snapshot=lambda cycle: order.append(("snapshot", cycle)),
    )
    monkeypatch.setattr(env, "_begin_backend_cycle", lambda c: None)
    monkeypatch.setattr(env, "_drive_backend_cycle", lambda c: order.append(("drive", c)))
    monkeypatch.setattr(env.flow_checker, "on_clock_edge", lambda c: None)
    monkeypatch.setattr(env.translation_checker, "on_clock_edge", lambda c: None)
    env.register_cycle_observer(lambda c, e: order.append(("random", c)))
    env.dut.reset.value = 0
    env._on_clock_edge(7)
    assert order == [("drive", 7), ("snapshot", 7), ("random", 7)]


def test_campaign_cli_help_imports_without_external_pythonpath(tmp_path):
    script = Path(__file__).with_name("run_random_campaign.py")
    environment = dict(os.environ)
    environment.pop("PYTHONPATH", None)
    result = subprocess.run([sys.executable, str(script), "--help"], cwd=tmp_path, env=environment, text=True, capture_output=True, timeout=10)
    assert result.returncode == 0, result.stderr
    assert "--master-seed" in result.stdout


def test_random_state_uses_the_same_native_read_once_snapshot():
    env = _FakeCoordinatorEnv(None)
    env.dut = _fake_bin951_dut()
    recorder = FrontendFuncovSampleHub.__new__(FrontendFuncovSampleHub)
    recorder.native_toffee_runtime_enabled = True
    recorder._cycle_snapshot_cycle = None
    recorder._dut_signal_cache = {}
    recorder._missing_dut_signals = set()
    recorder._registered_internal_signals = set()
    env.functional_coverage = recorder
    recorder.begin_cycle_snapshot(7)
    name = BIN951_IFU_SIGNAL_NAMES["s1_half_data"]
    getattr(env.dut, name).value = 0x13
    assert recorder._try_read_dut_signal(env.dut, name) == 0x13
    getattr(env.dut, name).value = 0x63
    coordinator = ScenarioCoordinator(scenario_id="snapshot", timing_plan=TimingPlan.from_seed(7, "ibuffer_pressure_cross"), program_map=_coordinator_program_map())
    coordinator.observe_cycle(7, env)
    assert coordinator._bin951_history[-1]["s1_half_data"] == 0x13
    with pytest.raises(RuntimeError, match="current post-drive native snapshot"):
        coordinator.observe_cycle(8, env)


def test_hardware_reset_discards_random_pending_proof_and_agent_responses():
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    coordinator = ScenarioCoordinator(scenario_id="reset", timing_plan=TimingPlan.from_seed(7, "ibuffer_pressure_cross"), program_map=_coordinator_program_map())
    coordinator.install(env)
    coordinator._start_backpressure(4, {"type": "backend.redirect_queued", "payload": {}})
    coordinator._bin951_history.append({"s1_half_valid": 1})
    coordinator._bin951_redirect_edges[4] = {"valid": 1}
    coordinator._bin951_active = {"redirect_cycle": 4}
    env.ptw_agent.pending.append(object())
    env.uncache_agent.pending.append(object())
    env.dut.reset.value = 1
    env._on_clock_edge(5)
    assert not env.ptw_agent.pending and env.ptw_agent.active_resp is None
    assert not env.uncache_agent.pending
    assert coordinator._bin951_history == [] and coordinator._bin951_redirect_edges == {}
    assert coordinator._bin951_active is None
    assert coordinator._actions[0]["status"] == "FAIL"
    assert coordinator._bin951_events[-1]["status"] == "UNCONFIRMED"
    assert not coordinator.realized_events()


def mixed_pbmt_scenario():
    plan = attribute_plan_from_dict({"schema_version": 1, "profile": "mixed", "pbmt": 0, "pbmte": 1, "pma_pattern": [1], "pbmt_pattern": [0, 1, 2]})
    spec = NS(scenario_id="mixed", ordinal=0, parameters={"translation": {"schema_version": TRANSLATION_SCHEMA_VERSION, "randomizer_seed": 7, "ordinal": 0, "attribute_plan": plan.to_dict()}})
    return build_translation_scenario(spec, bytes(3 * 4096))


def test_per_page_pbmt_is_installed_in_real_mapping_and_expected_outcomes():
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    scenario = mixed_pbmt_scenario()
    state = TranslationScenarioBuilder(env).build(scenario)
    assert [outcome["stage1_pbmt"] for outcome in state.expected_page_outcomes] == [0, 1, 2]
    assert [outcome["expected_path"] for outcome in state.expected_page_outcomes] == ["cacheable", "uncache", "uncache"]
    assert all(outcome["ok"] for outcome in state.expected_page_outcomes)


@pytest.mark.parametrize("overrides, message", [
    ({"s1_page_pbmt": (0, 1)}, "exactly one PBMT"),
    ({"s1_page_pbmt": (0, 1, 3)}, "values must be"),
    ({"mode": "bare"}, "requires non-bare"),
])
def test_invalid_pbmt_contract_fails_before_mapping(overrides, message):
    env = FrontendEnv(FakeDUTFrontend(), register_callbacks=False)
    scenario = replace(mixed_pbmt_scenario(), **overrides)
    if scenario.mode == "bare":
        scenario = replace(scenario, pa=scenario.va)
    with pytest.raises(ValueError, match=message):
        TranslationScenarioBuilder(env).validate(scenario)

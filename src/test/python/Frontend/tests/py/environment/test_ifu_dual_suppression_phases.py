"""Check real model-derived phase expectations and exact response attribution."""

from types import SimpleNamespace

import pytest

from env.checkers.translation_scenario_checker import TranslationScenarioChecker
from env.agents.ptw_agent import PTWAgent
from env.model import PageTableModel
from tests.py.jiabowen import test_ifu_nc_dual_suppression_v3_dut as case
from tests.py.jiabowen import test_ifu_cacheable_mixed_window_v3_dut as pma_case


class _PhaseEnvironment:
    def __init__(self, pbmt):
        self.page_table = PageTableModel()
        self.page_table.set_mode("sv39")
        self.page_table.map_page(case._BASE >> 12, case.uncache._NORMAL_PHYS_BASE >> 12,
                                 v=1, r=1, x=1, pbmt=pbmt)
        self.translation_epoch = 7
        self.ptw_agent = PTWAgent(self.page_table)
        self.current_cycle = 10
        self.monitor = SimpleNamespace(redirect_count=0)
        self.icache_agent = SimpleNamespace(request_records=[])
        self.uncache_agent = SimpleNamespace(request_addrs=[])
        self.translation_oracle = TranslationScenarioChecker()
        self.translation_oracle.bind_env(self)
        self.states = []

    def arm_translation_scenario(self, state):
        assert state.translation_epoch == self.translation_epoch
        self.states.append(state)
        return self.translation_oracle.arm(state, translation_epoch=self.translation_epoch)

    def load_program(self, *_args, **_kwargs):
        raise AssertionError("phase expectation must never reload live stimulus")

    def update_translation_context(self, *_args, **_kwargs):
        raise AssertionError("phase expectation must never change DUT context")


def _arm(env, *, pbmt, cacheable, label):
    return case._arm_dual_suppression_phase(
        env, case._second_block_taken_loop(), pbmt=pbmt,
        pma_cacheable=cacheable, label=label,
    )


def _translation_completed(env):
    state = env.states[-1]
    identity = {key: state.expected_ptw_request[key] for key in ("vpn", "s2xlate", "get_gpa")}
    env.translation_oracle.observe_ptw_request(11, **identity)
    env.translation_oracle.observe_ptw_response(12, **identity, response={})


@pytest.mark.parametrize("pbmt,cacheable,path,nc", [
    (0, True, "icache", None), (1, True, "uncache", 1), (0, False, "uncache", 0),
])
def test_phase_uses_actual_mapping_and_independent_declared_pma(pbmt, cacheable, path, nc):
    env = _PhaseEnvironment(pbmt)
    state = _arm(env, pbmt=pbmt, cacheable=cacheable, label="unit")
    assert state.scenario.payload == bytes.fromhex("0100")
    assert state.scenario.va == case._BASE + 0x3E
    assert state.expected_outcome["stage1_pbmt"] == pbmt
    assert state.expected_outcome["permission"]["pma_mmio"] == int(not cacheable)
    assert state.expected_outcome["fetch_path"] == ("uncache" if pbmt else "pma")
    _translation_completed(env)
    pa = case.uncache._NORMAL_PHYS_BASE + (0 if path == "icache" else 0x38)
    env.translation_oracle.observe_fetch_request(13, path=path, pa=pa, mem_page_type_nc=nc)
    assert env.translation_oracle.assert_complete()["error_count"] == 0


def test_pma_phase_rearm_retains_errors_and_uses_new_epoch_without_new_program():
    env = _PhaseEnvironment(0)
    first = _arm(env, pbmt=0, cacheable=True, label="warmup")
    _translation_completed(env)
    env.translation_oracle.observe_fetch_request(
        13, path="icache", pa=case.uncache._NORMAL_PHYS_BASE)
    assert env.translation_oracle.assert_complete()["error_count"] == 0
    records = list(env.translation_oracle.records)
    env.translation_epoch += 1
    second = _arm(env, pbmt=0, cacheable=False, label="pma")
    assert second.translation_epoch == first.translation_epoch + 1
    assert second.expected_outcome["stage1_pbmt"] == first.expected_outcome["stage1_pbmt"] == 0
    assert second.scenario.expected_path == "uncache"
    assert env.translation_oracle.records[:len(records)] == records
    assert env.translation_oracle.active["fetch_observation_ready"] is True
    _translation_completed(env)
    env.translation_oracle.observe_fetch_request(
        13, path="uncache", pa=case.uncache._NORMAL_PHYS_BASE + 0x38, mem_page_type_nc=0)
    assert env.translation_oracle.assert_complete()["error_count"] == 0


@pytest.mark.parametrize("fault", ["nc_borrow", "cached_path", "wrong_pa", "no_ptw", "old_error"])
def test_pma_phase_oracle_rejects_wrong_memory_type_or_incomplete_history(fault):
    env = _PhaseEnvironment(0)
    if fault == "old_error":
        env.translation_oracle.errors.append({"reason": "retained-warmup-failure"})
    _arm(env, pbmt=0, cacheable=False, label="pma")
    if fault != "no_ptw":
        _translation_completed(env)
    env.translation_oracle.observe_fetch_request(
        13, path="icache" if fault == "cached_path" else "uncache",
        pa=case.uncache._NORMAL_PHYS_BASE + (0x40 if fault == "wrong_pa" else 0x38),
        mem_page_type_nc=int(fault == "nc_borrow"),
    )
    with pytest.raises(AssertionError):
        env.translation_oracle.assert_complete()


def test_phase_expectation_rejects_mapping_pbmt_disagreement():
    env = _PhaseEnvironment(1)
    with pytest.raises(AssertionError):
        _arm(env, pbmt=0, cacheable=False, label="pma")
    assert not env.states


@pytest.mark.parametrize("wrong", [None, "valid0", "valid1", "pbmt", "pmp_mmio",
                                  "exception", "flag", "index", "start"])
def test_pma_response_requires_pbmt0_pmp1_and_exact_input_identity(wrong):
    request = dict(first_ftq_flag=0, first_ftq_value=27, first_start=0x40000030)
    response = dict(valid0=1, valid1=0, pbmt=0, pmp_mmio=1,
                    exception=0, flag=0, index=27, start=0x40000030)
    if wrong:
        response[wrong] ^= 1
        with pytest.raises(AssertionError):
            case._check_single_nc_response(request, response, pbmt=0, pmp_mmio=1)
    else:
        case._check_single_nc_response(request, response, pbmt=0, pmp_mmio=1)


def test_pma_wrapper_selects_only_shared_pma_mode(monkeypatch):
    calls = []
    monkeypatch.setattr(pma_case, "_run_dual_suppression_case", lambda *args, **kw: calls.append((args, kw)))
    env = object()
    pma_case.test_two_fetch_mmio_window_suppresses_second_icache_response(env)
    assert calls == [((env,), {"warm_cacheable": True, "attribute_kind": "pma"})]

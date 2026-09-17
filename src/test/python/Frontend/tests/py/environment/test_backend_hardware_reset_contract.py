"""Hardware reset starts a new FTQ epoch without erasing diagnostic history."""

from types import SimpleNamespace

import pytest

from env.core.backend_model import BackendModel
from env.core.frontend_env import FrontendEnv
from env.model.backend_runtime import BackendObservationSnapshot
from env.model.backend_state import BackendEvent, FtqEntry
from env.model.golden_trace import GoldenTrace, TraceEntry
from env.runtime.dut_factory import FakeDUTFrontend
from tests.py.environment.test_backend_model_unit import _ObserveIf, _queue_instr


def _dirty_model():
    model = BackendModel(random_seed=954, resolve_min_delay=6, resolve_max_delay=9)
    model.commit_count = 21
    model.commit_ptr_flag, model.commit_ptr_value = 0, 21
    model._ftq_start_pc_by_value[21] = 0x80200F00
    model._ftq_start_pc_cache[21] = 0x80200F00
    model._ftq_group_pc_history[(0, 21)] = [(0x80200F00, False)]
    model._pc_group_occurrences[0x80200F00] = [(0, 21, 0)]
    model.ftq_entries.append(FtqEntry(0, 21))
    model._current_ftq_entry = FtqEntry(0, 22)
    model._current_ftq_seen_packets.add((0, 22, 1, 0x13, 0))
    model._pending_resolves.append(object())
    model.pending_events.append(BackendEvent("redirect", 10, {"target_pc": 0x80002000}))
    model._cfvec_queue.append(_queue_instr(0x80200F00, 0, 21))
    model._commit_queue.append(0)
    model._pending_queue_resolve_indices.append(0)
    model._pending_queue_call_ret_commit_indices.append(0)
    model._scheduled_queue_call_ret_commit_groups.append((10, [object()]))
    model._visible_queue_call_ret_commit_group.append(object())
    model._pending_level0_target_ftq = (0, 22)
    model._pending_level0_target_pc = 0x80003000
    model._last_correct_cfi_context = {"ftq_flag": 0, "ftq_value": 21}
    model._last_committed_correct_cfi_context = dict(model._last_correct_cfi_context)
    model._planned_commit_apply = {"old": True}
    model._recovery_commit_block_ftq = (0, 21)
    model._last_driven_redirect_signature = (0, 21, 1, 2, 3, 4)
    model._skip_cfvec_until_cycle = 999
    model._last_observation = BackendObservationSnapshot(from_ftq_wen=1, from_ftq_ftq_idx=21)
    model._set_active_wrong_path_episode(origin_index=0, target_pc=0x80003000, redirect_context={})
    model.last_events.append({"kind": "pre_reset_diagnostic"})
    return model


def test_reset_clears_all_transaction_state_preserving_bindings_config_and_statistics():
    model = _dirty_model()
    marker = object()
    model.dut = model.env = model.monitor = marker
    rng_state = model._rng.getstate()
    model.on_hardware_reset(100)
    assert model.commit_ptr_flag == model.commit_ptr_value == model.commit_count == 0
    assert model.get_stats()["commit_count"] == 21
    assert model.get_stats()["epoch_commit_count"] == 0
    assert model.get_stats()["hardware_reset_count"] == 1
    assert model.pending_work_count() == 0
    assert model.backend_empty_for_dut() == 1
    for name in ("_ftq_start_pc_by_value", "_ftq_start_pc_cache", "_ftq_group_pc_history",
                 "_pc_group_occurrences", "_cfvec_queue", "_commit_queue",
                 "_current_ftq_seen_packets", "_pending_queue_resolve_indices"):
        assert not getattr(model, name), name
    for name in ("_pending_level0_target_pc", "_last_correct_cfi_context",
                 "_last_committed_correct_cfi_context", "_planned_commit_apply",
                 "_recovery_commit_block_ftq", "_last_driven_redirect_signature",
                 "_skip_cfvec_until_cycle"):
        assert getattr(model, name) is None, name
    assert model.current_frontend_observation() == BackendObservationSnapshot()
    assert model.dut is model.env is model.monitor is marker
    assert model.resolve_min_delay == 6 and model.resolve_max_delay == 9
    assert model._rng.getstate() == rng_state
    assert model.last_events[0] == {"kind": "pre_reset_diagnostic"}
    model._apply_backend_state()
    assert model.pending_work_count() == 0 and model.commit_ptr_value == 0


def test_reset_is_one_epoch_per_assertion_and_first_ftq_zero_is_not_stale():
    model = _dirty_model()
    model.on_hardware_reset(100)
    model.on_hardware_reset(101)
    assert model.get_stats()["hardware_reset_count"] == 1
    assert model.get_stats()["commit_count"] == 21
    model.begin_cycle(102)
    assert not model._ftq_ptr_is_stale_relative_to_commit(0, 0)
    model.commit_count = 1
    assert model._ftq_ptr_is_stale_relative_to_commit(0, 0)
    model.on_hardware_reset(103)
    assert model.get_stats()["commit_count"] == 22
    assert model.get_stats()["hardware_reset_count"] == 2


@pytest.mark.parametrize("name,dirty,clean", [
    ("_reuse_commit_ptr_once", True, False),
    ("ibuf_full_streak", 30, 0),
    ("_current_ftq_max_offset", 31, -1),
    ("_current_ftq_observed_pending_target_pc", True, False),
    ("_cycle_start_golden_pc", 0x80001234, None),
    ("_cycle_start_golden_cursor", 200, None),
    ("_recovery_commit_block_cycle", 99, -1),
    ("_last_driven_redirect_cycle", 99, None),
])
def test_reset_clears_auxiliary_epoch_state(name, dirty, clean):
    model = BackendModel()
    setattr(model, name, dirty)
    model.on_hardware_reset(100)
    assert getattr(model, name) == clean


def test_new_epoch_stale_context_check_remains_strict():
    model = _dirty_model()
    model.on_hardware_reset(100)
    model.begin_cycle(101)
    model.commit_count = 1
    with pytest.raises(AssertionError, match="selected stale ftq context"):
        model._assert_redirect_drive_ftq_not_stale(
            payload_ftq_flag=0, payload_ftq_value=0,
            drive_ftq_flag=0, drive_ftq_value=0, target_pc=0x80004000, reason="new-epoch-negative",
        )


def test_standalone_backend_callback_also_gates_reset(monkeypatch):
    dut = FakeDUTFrontend()
    env = FrontendEnv(dut, register_callbacks=False)
    model = env.backend_model
    model.bind(dut)  # Standalone callback API binds a DUT, not just env interfaces.
    model.commit_ptr_value, model.commit_count = 21, 21
    for name in ("begin_cycle", "consume_backend_observation", "plan_cycle_actions"):
        monkeypatch.setattr(model, name, lambda *_args: pytest.fail("standalone observed/planned under reset"))
    dut.reset.value = 1
    env.backend_agent._drive_if.commit_valid.value = 1
    model.on_clock_edge(100)
    assert model.commit_ptr_value == 0
    assert env.backend_agent._drive_if.commit_valid.value == 0
    assert model.get_stats()["commit_count"] == 21


def test_old_pc_mapping_unavailable_after_reset_then_new_mapping_is_required():
    model = _dirty_model()
    model.observe_if = _ObserveIf()
    model.on_hardware_reset(100)
    with pytest.raises(AssertionError, match="without an observed start PC"):
        model.observed_cfvec_pc(0)
    model.consume_backend_observation(BackendObservationSnapshot(
        from_ftq_wen=1, from_ftq_ftq_idx=0, from_ftq_start_pc_addr=0x80004000 >> 1,
    ))
    assert model._ftq_start_pc_by_value == {0: 0x80004000}


@pytest.mark.parametrize("cursor", [0, 1, 2])
def test_reset_never_detaches_advances_or_rewinds_golden_trace(cursor):
    model = _dirty_model()
    trace = GoldenTrace([TraceEntry(i, 0x80000000 + i * 4, 0x13, 4) for i in range(2)])
    model.set_golden_trace(trace, start_cursor=cursor)
    model.set_explicit_injection_enabled(False, "trace run")
    model.on_hardware_reset(100)
    assert model.golden_trace is trace and trace.cursor == cursor
    assert model._explicit_injection_enabled is False
    with pytest.raises(AssertionError, match="trace run"):
        model._assert_explicit_injection_allowed("redirect")


def test_environment_reset_does_not_observe_or_plan_backend_or_clear_errors(monkeypatch):
    dut = FakeDUTFrontend()
    env = FrontendEnv(dut, register_callbacks=False)
    env.backend_model.commit_count = 21
    env.backend_model.commit_ptr_value = 21
    env.backend_model._ftq_start_pc_by_value[21] = 0x80200F00
    sentinel_error = {"kind": "pre_reset_error"}
    env.monitor.errors.append(sentinel_error)
    env.monitor.observations.append(SimpleNamespace(pc=0x80200F00))
    env.monitor.wait_sync_after_redirect = True
    env.monitor.redirect_grace = 2
    env.monitor._skip_cfvec_until_cycle = 999
    env.translation_oracle.errors.append({"kind": "pre_reset_translation_error"})
    active = {"existing": "must not silently accept/disarm"}
    env.translation_oracle.active = active
    called = []
    for obj, name in ((env, "_begin_backend_cycle"), (env, "_drive_backend_cycle"),
                      (env.monitor, "on_clock_edge"), (env.translation_oracle, "on_clock_edge")):
        monkeypatch.setattr(obj, name, lambda *_args, **_kwargs: pytest.fail("backend observed/planned under reset"))
    for obj in (env.ptw_agent, env.uncache_agent, env.ptw_full_ppn_checker, env.ptw_resp_input_checker):
        monkeypatch.setattr(obj, "on_clock_edge", lambda _cycle: None)
    env.register_cycle_observer(lambda cycle, _env: called.append(cycle))
    for stem in ("commit_valid", "redirect_valid", "ftq_idx_ahead_valid"):
        getattr(env.backend_agent._drive_if, stem).value = 1
    for stem in ("resolve_valid", "call_ret_commit_valid"):
        for signal in getattr(env.backend_agent._drive_if, stem):
            signal.value = 1
    dut.reset.value = 1
    dut.io_reset_vector_addr.value = 0x80004000 >> 1
    env._on_clock_edge(100)
    env._on_clock_edge(101)
    assert env.backend_model.commit_ptr_value == 0
    assert env.backend_model.get_stats()["commit_count"] == 21
    assert env.backend_model.get_stats()["hardware_reset_count"] == 1
    assert env.monitor.get_errors() == [sentinel_error]
    assert len(env.monitor.observations) == 1
    assert env.monitor.expected_pc == 0x80004000
    assert not env.monitor.wait_sync_after_redirect and env.monitor.redirect_grace == 0
    assert env.monitor._skip_cfvec_until_cycle is None
    assert env.translation_oracle.active is active
    assert len(env.translation_oracle.errors) == 1
    assert called == [100, 101]
    for stem in ("commit_valid", "redirect_valid", "ftq_idx_ahead_valid"):
        assert getattr(env.backend_agent._drive_if, stem).value == 0
    for stem in ("resolve_valid", "call_ret_commit_valid"):
        assert all(signal.value == 0 for signal in getattr(env.backend_agent._drive_if, stem))


@pytest.mark.parametrize("patch", [
    {"redirect": 1}, {"resolves": [0, 1, 0]},
    {"call_ret_payloads": [{"ras_action": 1, "ftq_start_pc": 0x80010000}]},
    {"call_ret_payloads": [{"ras_action": 0, "ftq_start_pc": 0x80000000}]},
    {"call_ret_payloads": [{"ras_action": 0, "ftq_start_pc": None}]},
    {"commit_start_pc": 0x80000000}, {"commit_start_pc": None},
])
def test_reset_dut_checker_rejects_old_or_unidentified_actions(patch):
    from tests.py.jiabowen.test_backend_reset_epoch_v3_dut import _assert_no_old_actions

    record = {"redirect": 0, "resolves": [0, 0, 0], "commit": 1,
              "commit_start_pc": 0x80010000,
              "call_ret_payloads": [{"ras_action": 0, "ftq_start_pc": 0x80010000}]}
    _assert_no_old_actions(record, 0x80010000)
    with pytest.raises(AssertionError):
        _assert_no_old_actions({**record, **patch}, 0x80010000)

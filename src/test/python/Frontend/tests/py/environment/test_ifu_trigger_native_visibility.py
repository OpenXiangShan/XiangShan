"""Check native trigger diagnostics through the actual model/hub/sink path."""

from tests.py.environment.test_ifu_metadata_native_visibility import _runtime
from tests.py.jiabowen.test_ifu_compact_functional_coverage import (
    _TRIGGER_PREFIX, _set_frontend_trigger_config,
)


def test_native_trigger_state_and_missing_capability_are_visible_on_hub(tmp_path):
    hub, _env, dut, model = _runtime(tmp_path)
    for slot in range(4):
        _set_frontend_trigger_config(dut, slot)
    delattr(dut, _TRIGGER_PREFIX + "tdataVec_0_timing")

    dut.set("reset", 0)
    hub.begin_cycle_snapshot(10)
    model.on_cycle(10)
    state = model._ifu_frontend_trigger_state
    gap = model._ifu_frontend_trigger_config_gap
    assert isinstance(state, dict) and gap["active"] is True
    assert gap["missing"] == ["tdataVec_0_timing"]
    assert gap["affected_bin_ids"] == ["BIN-1000"]
    for obj in (hub, hub.toffee_sink):
        assert obj._ifu_frontend_trigger_state is state
        assert obj._ifu_frontend_trigger_config_gap is gap
        assert obj._ifu_frontend_trigger_signal_paths is model._ifu_frontend_trigger_signal_paths
    assert hub.toffee_sink.hit_count_by_bin_id("BIN-1000") == 0

    # Follow the real fixture's hub/model ordering across reset. No held proof
    # or update sampled before reset can join a transaction after reset.
    state["held_trigger"] = {"cycle": 10, "lanes": []}
    state["verified_updates"]["before-reset"] = {"cycle": 10}
    dut.set("reset", 1)
    hub.begin_cycle_snapshot(11)
    hub.on_cycle(11, hub.env)
    model.on_cycle(11)
    assert hub.toffee_sink._ifu_frontend_trigger_state is None
    assert hub.toffee_sink._ifu_frontend_trigger_config_gap is gap
    dut.set("reset", 0)
    hub.begin_cycle_snapshot(12)
    model.on_cycle(12)
    fresh = hub.toffee_sink._ifu_frontend_trigger_state
    assert fresh is not state and fresh["held_trigger"] is None
    assert fresh["verified_updates"] == {}
    model._unrelated_native_state = "model-only"
    assert not hasattr(hub, "_unrelated_native_state")

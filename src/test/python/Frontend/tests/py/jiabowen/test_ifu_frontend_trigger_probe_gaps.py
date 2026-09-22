"""Missing Trigger config must be explicit and cannot join stale samples."""

import pytest

from env.funcov.py.ifu.compact_funcov import _sample_frontend_trigger, _frontend_trigger_state
from tests.py.jiabowen.test_ifu_compact_functional_coverage import (
    _make_recorder, _set_frontend_trigger_config, _TRIGGER_PREFIX,
)


@pytest.mark.parametrize("field", ["matchType", "select", "timing", "action", "chain", "tdata2"])
@pytest.mark.parametrize("slot", range(4))
@pytest.mark.parametrize("unavailable", ["absent", "unreadable"])
def test_trigger_missing_config_is_visible_and_discards_pending_history(tmp_path, field, slot, unavailable):
    recorder, _env, dut, _memory = _make_recorder(tmp_path)
    for index in range(4):
        _set_frontend_trigger_config(dut, index)
    state = _frontend_trigger_state(recorder)
    state["last_configs"] = ((0, 0, 0, 0, 0, 0),) * 4
    state["verified_updates"][(0, 0, 0, 0, 0, 0, 0)] = {"cycle": 1}
    state["held_trigger"] = {"cycle": 1, "lanes": []}
    state["chain_samples"].add("chain_pass")
    state["s2_transaction_baselines"][0x80000000] = {"instr": 1}
    stem = f"tdataVec_{slot}_{field}"
    if unavailable == "absent":
        delattr(dut, _TRIGGER_PREFIX + stem)
    else:
        getattr(dut, _TRIGGER_PREFIX + stem).value = None
    _sample_frontend_trigger(recorder, dut, 2)
    _sample_frontend_trigger(recorder, dut, 3)
    assert not any(recorder.toffee_sink.hit_counts().values())
    if field != "timing":
        assert recorder._ifu_frontend_trigger_state is None
    else:
        assert not recorder._ifu_frontend_trigger_state["chain_samples"]
    gaps = [e for e in recorder.risk_observations
            if e.get("event") == "frontend_trigger_config_probe_missing"]
    assert len(gaps) == 1
    assert gaps[0]["missing"] == [stem]
    assert gaps[0]["coverage_promotion"] == "none_for_affected_bins"
    if field == "timing":
        assert gaps[0]["affected_bin_ids"] == ["BIN-1000"]
    assert _TRIGGER_PREFIX + stem in gaps[0]["attempted_paths"][stem]
    # The artifact must retain the capability gap even after risk-tail eviction.
    for cycle in range(128):
        recorder.risk_observations.append({"event": "unrelated_risk", "cycle": cycle})
    persistent = recorder._ifu_frontend_trigger_config_gap
    assert persistent["missing"] == [stem] and persistent["active"] is True
    assert persistent["first_cycle"] == 2 and persistent["last_cycle"] == 3
    assert persistent["rejected_samples"] == 2
    _set_frontend_trigger_config(dut, slot)
    _sample_frontend_trigger(recorder, dut, 4)
    restored = _frontend_trigger_state(recorder)
    assert restored["verified_updates"] == {}
    assert restored["held_trigger"] is None
    assert not restored["chain_samples"] and not restored["s2_transaction_baselines"]
    assert not any(recorder.toffee_sink.hit_counts().values())
    # Absent handles are cached for the fixed DUT ABI; adding a fake attribute
    # cannot change that ABI. A transiently unreadable existing handle can
    # recover, but only with fresh temporal state.
    assert recorder._ifu_frontend_trigger_config_gap["active"] is (unavailable == "absent")

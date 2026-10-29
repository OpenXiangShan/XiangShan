"""Exercise the actual native evaluate/recorder/hub/sink metadata path offline."""

import pytest

from env.funcov.native_toffee import EvaluateFlagRecorder
from env.funcov.toffee_bridge import ToffeeCoverageSink
from env.funcov.toffee_runtime import create_toffee_runtime
from env.funcov.runtime_context import default_pilot_csv_path
from tests.py.jiabowen.test_ifu_compact_functional_coverage import _PREFIX, _make_recorder
from tests.py.jiabowen.test_ifu_exception_metadata_funcov import _component


_GROUP, _BIN = "ifu_v3_pipeline_owner_model", "owner_leaf_056"


def _runtime(tmp_path):
    hub, env, dut, _ = _make_recorder(tmp_path)
    runtime = create_toffee_runtime(hub, hub.toffee_sink)
    model = next(m for m in runtime.cycle_models if type(m).__name__ == "IfuCfvecToffeeCoverage")
    return hub, env, dut, model


def _cycle(hub, dut, model, cycle):
    dut.set("reset", 0)
    hub.begin_cycle_snapshot(cycle)
    model.on_cycle(cycle)


def test_existing_attribute_collection_and_unattached_sink_semantics(tmp_path):
    hub, _env, _dut, model = _runtime(tmp_path)
    sink = hub.toffee_sink
    wrapped = EvaluateFlagRecorder(model, {}, {})
    wrapped._unrelated_native_state = "native-only"
    assert model._unrelated_native_state == "native-only"
    assert not hasattr(hub, "_unrelated_native_state")
    assert not hasattr(sink, "_unrelated_native_state")
    assert not hasattr(hub, "_raw_dict")  # Not a collection API in this tree.
    unattached = ToffeeCoverageSink.from_registry(default_pilot_csv_path())
    with pytest.raises(AttributeError):
        _ = unattached._ifu_exception_metadata_witnesses


def test_native_satp_witness_is_visible_without_cfvec_or_phase_delay(tmp_path):
    hub, _env, dut, model = _runtime(tmp_path)
    _component(dut, "satp")
    _cycle(hub, dut, model, 10)
    for source in (model, hub, hub.toffee_sink):
        assert source._ifu_exception_metadata_checks == {"satp_flush"}
        assert source._ifu_exception_metadata_witnesses["satp_flush"]["cycle"] == 10
    assert model._ifu_exception_metadata_witnesses is hub._ifu_exception_metadata_witnesses
    assert hub.toffee_sink._ifu_exception_metadata_witnesses is hub._ifu_exception_metadata_witnesses
    assert "_ifu_exception_metadata_witnesses" not in model.__dict__
    assert not hub.key_hit(_GROUP, _BIN)  # A component alone is never the whole bin.


def test_all_five_complete_witnesses_and_native_owner_hit_stay_same_run(tmp_path):
    hub, _env, dut, model = _runtime(tmp_path)
    for cycle, name in zip((10, 12, 14, 16), ("satp", "backend", "cross", "gpf")):
        _component(dut, name)
        _cycle(hub, dut, model, cycle)
        assert hub.key_hit(_GROUP, _BIN) == (name == "gpf")
        if name != "gpf":
            dut.set("reset", 1)
            hub.begin_cycle_snapshot(cycle + 1)
            hub.on_cycle(cycle + 1, hub.env)
            model.on_cycle(cycle + 1)
    expected = {"satp_flush", "backend_exception", "cross_page", "gpaddr", "vs_nonleaf_pte"}
    assert hub.toffee_sink._ifu_exception_metadata_checks == expected
    assert {w["cycle"] for w in hub.toffee_sink._ifu_exception_metadata_witnesses.values()} == {10, 12, 14, 16}
    other, _env2, _dut2, model2 = _runtime(tmp_path / "independent")
    assert not getattr(other, "_ifu_exception_metadata_checks", set())
    assert not getattr(model2, "_ifu_exception_metadata_witnesses", {})
    assert not other.key_hit(_GROUP, _BIN)


@pytest.mark.parametrize("mode", ["absent", "unreadable"])
def test_missing_probe_gap_visible_and_recovery_requires_new_complete_cycle(tmp_path, mode):
    hub, _env, dut, model = _runtime(tmp_path)
    _component(dut, "satp")
    stem = "io_toIBuffer_bits_hasSatpFlush"
    if mode == "absent":
        delattr(dut, _PREFIX + stem)
    else:
        getattr(dut, _PREFIX + stem).value = None
    _cycle(hub, dut, model, 10)
    assert not getattr(hub.toffee_sink, "_ifu_exception_metadata_checks", set())
    assert ("satp_flush", stem) in hub.toffee_sink._ifu_exception_metadata_gaps
    assert not hub.key_hit(_GROUP, _BIN)
    if mode == "unreadable":
        getattr(dut, _PREFIX + stem).value = 1
        _cycle(hub, dut, model, 11)
        assert hub.toffee_sink._ifu_exception_metadata_witnesses["satp_flush"]["cycle"] == 11
    # Absent paths are cached as absent by the existing collection API. Do
    # not bypass that cache or turn missing inventory into a synthetic value.


@pytest.mark.parametrize("stem,value", [
    ("io_toIBuffer_valid", 0), ("io_toIBuffer_ready", 0), ("io_toIBuffer_bits_enqEnable", 0),
    ("s2_icacheMeta_0_hasSatpFlush", 0),
])
def test_non_delivery_or_mismatched_metadata_does_not_publish_witness(tmp_path, stem, value):
    hub, _env, dut, model = _runtime(tmp_path)
    _component(dut, "satp")
    dut.set(_PREFIX + stem, value)
    _cycle(hub, dut, model, 10)
    assert not getattr(hub.toffee_sink, "_ifu_exception_metadata_checks", set())
    assert not hub.key_hit(_GROUP, _BIN)


def test_mismatched_gpa_fragments_never_join_in_native_path(tmp_path):
    hub, _env, dut, model = _runtime(tmp_path)
    _component(dut, "gpf")
    dut.set(_PREFIX + "io_toBackend_gpAddrMem_wdata_gpaddr", 0xDEAD0000)
    _cycle(hub, dut, model, 10)
    dut.set(_PREFIX + "io_toBackend_gpAddrMem_wdata_gpaddr", 0x80601000)
    dut.set(_PREFIX + "io_toBackend_gpAddrMem_waddr", 8)
    _cycle(hub, dut, model, 11)
    assert not getattr(hub.toffee_sink, "_ifu_exception_metadata_witnesses", {})
    assert not hub.key_hit(_GROUP, _BIN)


def test_completed_witness_and_missing_gap_survive_risk_tail_truncation(tmp_path):
    hub, _env, dut, model = _runtime(tmp_path)
    _component(dut, "satp")
    _cycle(hub, dut, model, 10)
    _component(dut, "gpf")
    getattr(dut, _PREFIX + "io_toBackend_gpAddrMem_wdata_gpaddr").value = None
    _cycle(hub, dut, model, 11)
    for i in range(160):
        hub.risk_observations.append({"unrelated": i})
    assert set(hub.toffee_sink._ifu_exception_metadata_witnesses) == {"satp_flush"}
    assert ("gpaddr", "gpAddrMem.wdata_gpaddr") in hub.toffee_sink._ifu_exception_metadata_gaps
    assert not any("event" in event for event in hub.risk_observations)
    assert not hub.key_hit(_GROUP, _BIN)


def test_observer_before_native_callback_sees_prior_state_then_published_state(tmp_path):
    hub, _env, dut, model = _runtime(tmp_path)
    _component(dut, "satp")
    hub.begin_cycle_snapshot(10)
    # FrontendEnv cycle observers run before the native coverage callback.
    assert not getattr(hub.toffee_sink, "_ifu_exception_metadata_checks", set())
    model.on_cycle(10)
    assert hub.toffee_sink._ifu_exception_metadata_witnesses["satp_flush"]["cycle"] == 10


def test_observer_and_native_evaluator_share_read_once_values(tmp_path):
    hub, _env, dut, model = _runtime(tmp_path)
    _component(dut, "satp")
    hub.begin_cycle_snapshot(10)
    path = _PREFIX + "io_toIBuffer_bits_hasSatpFlush"
    assert hub.toffee_sink._try_read_dut_signal(dut, path) == 1
    getattr(dut, path).value = 0
    model.on_cycle(10)
    assert hub.toffee_sink._ifu_exception_metadata_witnesses["satp_flush"]["cycle"] == 10
    hub.begin_cycle_snapshot(11)
    assert hub.toffee_sink._try_read_dut_signal(dut, path) == 0

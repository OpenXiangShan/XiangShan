from types import SimpleNamespace

import pytest

from env.support.ifu_native_aliases import (
    IFU_ENQUEUE_PATHS, LINE0_TL_EXCEPTION_PATHS, LINE0_PRIOR_EXCEPTION_PATHS,
    read_last_in_ftq, read_line0_tl_cause,
)
from env.funcov.py.ifu.compact_funcov import _read_ifu_output_slot
from env.funcov.py.ifu.cacheable_pipeline_funcov import (
    _capture_line0_tl_fault_source, _read_late_fault_signal_with_path,
)

_IFU = "Frontend_top.Frontend.inner_ifu."
_MAIN = "Frontend_top.Frontend.inner_icache.mainPipe."


@pytest.mark.parametrize("encoded", [0, 3, 5])
def test_tl_projection_uses_registered_line_zero_not_live_or_merged_fault(encoded):
    values = {
        LINE0_TL_EXCEPTION_PATHS[0]: encoded,
        LINE0_PRIOR_EXCEPTION_PATHS[0]: 0,
        _MAIN + "s1_tlException_reg_1_value": 5 if encoded != 5 else 3,
        _MAIN + "io_missResp_bits_corrupt": 1,
        _MAIN + "io_missResp_bits_denied": 1,
        _MAIN + "io_toIfu_req_bits_info_0_icacheMeta_exception_value": 3,
    }
    projection = read_line0_tl_cause(values.get)
    assert projection.value == encoded
    assert projection.source_values == {LINE0_TL_EXCEPTION_PATHS[0]: encoded, LINE0_PRIOR_EXCEPTION_PATHS[0]: 0}


@pytest.mark.parametrize("bad", [None, 1, 2, 4, 6, -1])
def test_tl_missing_or_invalid_encoded_probe_cannot_use_any_other_signal(bad):
    values = {
        _MAIN + "s1_tlCorrupt_r": 1,
        _MAIN + "s1_tlDenied_r": 1,
        _MAIN + "s1_tlException_reg_1_value": 3,
        _MAIN + "__Vtogcov__s1_tlException_reg_value": 3,
        _MAIN + "io_missResp_bits_denied": 1,
    }
    if bad is not None:
        values[LINE0_TL_EXCEPTION_PATHS[0]] = bad
    assert read_line0_tl_cause(values.get) is None
    recorder = SimpleNamespace(env=SimpleNamespace(dut=object()),
                              _try_read_dut_signal=lambda _dut, name: values.get(name))
    # _dut reads recorder._env in the real context; both attributes are explicit.
    recorder._env = recorder.env
    for key in ("line0_tl_corrupt", "line0_tl_denied"):
        assert _read_late_fault_signal_with_path(recorder, key) == (None, None)


@pytest.mark.parametrize("encoded,expected", [(3, "tl_denied"), (5, "tl_corrupt")])
def test_tl_source_capture_binds_native_cause_to_accepted_ftq_transaction(tmp_path, encoded, expected):
    from tests.py.jiabowen.test_ifu_cacheable_pipeline_functional_coverage import _make_recorder, _set_request, _ICACHE_PREFIX
    recorder, env, dut = _make_recorder(tmp_path)
    _set_request(dut, ftq0=(0, 13))
    dut.set(_ICACHE_PREFIX + "__Vtogcov__io_toIfu_req_bits_info_0_icacheMeta_exception_value", encoded)
    dut.set(LINE0_TL_EXCEPTION_PATHS[0], encoded)
    dut.set(LINE0_PRIOR_EXCEPTION_PATHS[0], 0)
    _capture_line0_tl_fault_source(recorder, 1)
    pending = recorder._ifu_late_fault_source_pending
    assert pending["ftq_identity"] == (0, 13)
    assert pending["fault_source"] == expected
    assert pending["source_projection"]["source_values"] == {LINE0_TL_EXCEPTION_PATHS[0]: encoded, LINE0_PRIOR_EXCEPTION_PATHS[0]: 0}
    assert pending["source_projection"]["normalized_cause_only"] is True


@pytest.mark.parametrize("enq,left,right,expected", [
    (0b01, 0, 0, 1), (0b11, 0, 0, 0), (0b11, 0, 1, 1),
    (0b11, 1, 0, 1), (0b11, 1, 1, 0), (0, 0, 0, 0),
])
@pytest.mark.parametrize("slot", [0, 34])
def test_last_entry_projects_current_window_boundary_not_just_enqueue(enq, left, right, expected, slot):
    values = {
        IFU_ENQUEUE_PATHS[2]: enq << slot,
        _IFU + f"select_{slot}": left,
        _IFU + f"select_{slot+1}": right,
    }
    projection = read_last_in_ftq(values.get, slot)
    assert projection.value == expected
    assert projection.source_values == values


@pytest.mark.parametrize("missing", [IFU_ENQUEUE_PATHS[2], _IFU + "select_0", _IFU + "select_1"])
def test_last_entry_requires_every_native_parent_even_if_toggle_output_exists(missing):
    values = {IFU_ENQUEUE_PATHS[2]: 3, _IFU + "select_0": 0, _IFU + "select_1": 1,
              _IFU + "__Vtogcov__io_toIBuffer_bits_isLastInFtqEntry_0": 1}
    del values[missing]
    assert read_last_in_ftq(values.get, 0) is None


@pytest.mark.parametrize("enq,expected", [(0, 0), (1 << 35, 1)])
def test_last_interface_lane_requires_enqueue_and_no_nonexistent_lane36(enq, expected):
    projection = read_last_in_ftq({IFU_ENQUEUE_PATHS[2]: enq}.get, 35)
    assert projection.value == expected
    assert len(projection.source_values) == 1


def test_native_direct_last_output_keeps_its_value_and_does_not_use_toggle():
    values = {_IFU + "io_toIBuffer_bits_isLastInFtqEntry_0": 0,
              _IFU + "__Vtogcov__io_toIBuffer_bits_isLastInFtqEntry_0": 1}
    assert read_last_in_ftq(values.get, 0).value == 0


def test_projection_uses_recorder_snapshot_once_per_cycle_and_refreshes_next(tmp_path):
    from tests.py.jiabowen.test_ifu_cacheable_pipeline_functional_coverage import _make_recorder
    recorder, env, dut = _make_recorder(tmp_path)
    dut.set(IFU_ENQUEUE_PATHS[0], 1)
    dut.set(_IFU + "select_0", 0)
    dut.set(_IFU + "select_1", 0)
    recorder.begin_cycle_snapshot(1)
    assert _read_ifu_output_slot(recorder, dut, "isLastInFtqEntry", 0) == 1
    dut.set(IFU_ENQUEUE_PATHS[0], 3)
    assert _read_ifu_output_slot(recorder, dut, "isLastInFtqEntry", 0) == 1
    assert recorder._ifu_last_in_ftq_projections[0]["snapshot_cycle"] == 1
    recorder.begin_cycle_snapshot(2)
    assert _read_ifu_output_slot(recorder, dut, "isLastInFtqEntry", 0) == 0


@pytest.mark.parametrize("prior", [None, 1, 2, 3, 4, 5])
def test_same_merged_code_cannot_attribute_tl_fault_if_priority_selector_missing_or_nonzero(prior):
    values = {LINE0_TL_EXCEPTION_PATHS[0]: 3,
              _MAIN + "io_toIfu_req_bits_info_0_icacheMeta_exception_value": 3}
    if prior is not None:
        values[LINE0_PRIOR_EXCEPTION_PATHS[0]] = prior
    assert read_line0_tl_cause(values.get) is None


def test_missing_parent_removes_old_projection_witness():
    values = {IFU_ENQUEUE_PATHS[2]: 1, _IFU + "select_0": 0, _IFU + "select_1": 0}
    recorder = SimpleNamespace(_try_read_dut_signal=lambda _dut, name: values.get(name))
    assert _read_ifu_output_slot(recorder, object(), "isLastInFtqEntry", 0) == 1
    assert 0 in recorder._ifu_last_in_ftq_projections
    del values[_IFU + "select_1"]
    assert _read_ifu_output_slot(recorder, object(), "isLastInFtqEntry", 0) is None
    assert 0 not in recorder._ifu_last_in_ftq_projections

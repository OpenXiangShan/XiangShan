"""Exercise PredChecker testcase readers with native-only, missing and conflicting probes."""
from types import SimpleNamespace

import pytest

from tests.py.jiabowen import test_ifu_predchecker_v3_dut as testcase


NATIVE = 'Frontend_top.Frontend.inner_ifu.'
SHADOW = NATIVE + '__Vtogcov__'


class Recorder:
    def __init__(self, values):
        self.env = SimpleNamespace(dut=object())
        self.values = values
        self.paths = []

    def _read_first_dut_signal(self, dut, paths):
        for path in paths:
            self.paths.append(path)
            value = self.values.get(path)
            if value is not None:
                return value
        return None


@pytest.mark.parametrize('field,value', [('isRvc', 0), ('endOffset', 31)])
def test_actual_slot19_readers_use_native_not_toggle_storage(field, value):
    tail = f's2_alignedInstrVec_19_{field}'
    reader = Recorder({NATIVE + tail: value, SHADOW + tail: 1 - value})
    assert testcase._read_optional(reader, SHADOW + tail) == value
    assert testcase._read_required(reader, SHADOW + tail) == value
    assert all('__Vtogcov__' not in name for name in reader.paths)


@pytest.mark.parametrize('field', ['isRvc', 'endOffset'])
def test_actual_slot19_missing_native_never_becomes_zero_or_shadow_value(field):
    tail = f's2_alignedInstrVec_19_{field}'
    reader = Recorder({SHADOW + tail: 0})
    assert testcase._read_optional(reader, SHADOW + tail) is None
    with pytest.raises(AssertionError, match='required V3 PredChecker signal is unavailable'):
        testcase._read_required(reader, SHADOW + tail)


def test_actual_reader_accepts_native_module_alias():
    reader = Recorder({'inner_ifu.s2_alignedInstrVec_19_endOffset': 31})
    assert testcase._read_required(reader, SHADOW + 's2_alignedInstrVec_19_endOffset') == 31
    assert all('__Vtogcov__' not in name for name in reader.paths)


@pytest.mark.parametrize('native_value,shadow_value', [(0, 1), (1, 0)])
def test_actual_direct_reader_uses_current_checker_redirect(native_value, shadow_value):
    suffix = 'predChecker.io_resp_stage2Out_checkerRedirect_valid'
    native = NATIVE + suffix
    shadow = NATIVE + 'predChecker.__Vtogcov__io_resp_stage2Out_checkerRedirect_valid'
    dut = SimpleNamespace(**{native: SimpleNamespace(value=native_value), shadow: SimpleNamespace(value=shadow_value)})
    assert testcase._read_direct(SimpleNamespace(dut=dut), shadow) == native_value


def test_actual_direct_reader_rejects_shadow_only_checker_redirect():
    shadow = NATIVE + 'predChecker.__Vtogcov__io_resp_stage2Out_checkerRedirect_valid'
    dut = SimpleNamespace(**{shadow: SimpleNamespace(value=1)})
    assert testcase._read_direct(SimpleNamespace(dut=dut), shadow) is None


def test_predicted_taken_gate_requires_real_taken_and_exact_full_pc():
    values = {NATIVE + f's2_alignedInstrVec_{slot}_valid': 0 for slot in range(testcase._OBSERVABLE_S2_SLOTS)}
    stem = NATIVE + 's2_alignedInstrVec_19_'
    values.update({stem + 'valid': 1, stem + 'isPredTaken': 0, NATIVE + 's2_alignedInstrPcVec_19_addr': 0x8000005e >> 1})
    reader = Recorder(values)
    assert not testcase._s2_has_predicted_taken_at(reader, 0x8000005e)
    values[stem + 'isPredTaken'] = 1
    assert testcase._s2_has_predicted_taken_at(reader, 0x8000005e)
    assert not testcase._s2_has_predicted_taken_at(reader, 0x8000005c)
    del values[stem + 'isPredTaken']
    with pytest.raises(AssertionError, match='required V3 PredChecker signal is unavailable'):
        testcase._s2_has_predicted_taken_at(reader, 0x8000005e)


@pytest.mark.parametrize('condition,expected', [
    ('matched', True), ('ftq_mismatch', False), ('offset_mismatch', False),
    ('data_mismatch', False), ('s1_flush', False), ('not_taken', False),
])
def test_actual_native_pipeline_derives_bin899_only_from_matched_taken_transfer(tmp_path, condition, expected):
    from env.funcov.toffee_runtime import create_toffee_runtime
    from tests.py.jiabowen.test_ifu_cacheable_pipeline_functional_coverage import (
        _make_recorder, _set_request, _set_s1, _idle_request, _SIGNALS,
    )
    hub, env, dut = _make_recorder(tmp_path)
    blocks = _set_request(dut, taken0=(int(condition != 'not_taken'), 7))
    _set_s1(dut, blocks, valid=0, fire=0)
    runtime = create_toffee_runtime(hub, hub.toffee_sink)
    model = next(item for item in runtime.cycle_models if type(item).__name__ == 'IfuCacheablePipelineToffeeCoverage')
    dut.set('reset', 1)
    hub.begin_cycle_snapshot(0)
    model.on_cycle(0)
    dut.set('reset', 0)
    hub.begin_cycle_snapshot(1)
    model.on_cycle(1)
    if condition == 'ftq_mismatch': blocks[0]['ftqIdx_value'] += 1
    if condition == 'offset_mismatch': blocks[0]['takenCfiOffset_bits'] += 1
    if condition == 'data_mismatch': blocks[0]['data'] ^= 1 << 400
    _idle_request(dut)
    _set_s1(dut, blocks)
    dut.set(_SIGNALS['s1_flush'][0], int(condition == 's1_flush'))
    hub.begin_cycle_snapshot(2)
    model.on_cycle(2)
    assert hub.key_hit('ifu_cacheable_metadata', 'taken_offset_preserved') is expected
    assert hub.key_hit('ifu_v3_pipeline_owner_model', 'owner_leaf_001') is expected
    assert bool(hub.hit_count_by_bin_id('BIN-899')) is expected

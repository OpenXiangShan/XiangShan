"""Exercise the actual BIN904 testcase readers without a compiled DUT."""
from types import SimpleNamespace

import pytest

from env.funcov.py.ifu.cacheable_pipeline_funcov import _UPSTREAM_SIGNALS, _native_signal_names
from tests.py.jiabowen.test_ifu_cacheable_mixed_window_v3_dut import _upstream_snapshot
from tests.py.jiabowen.test_ifu_nc_dual_suppression_v3_dut import _candidate_paths, _read_native_paths


class Recorder:
    def __init__(self, values):
        self.env = SimpleNamespace(dut=object())
        self.values = values
        self.read_paths = []

    def _try_read_dut_signal(self, dut, path):
        self.read_paths.append(path)
        return self.values.get(path)

    def _read_first_dut_signal(self, dut, paths):
        for path in paths:
            value = self._try_read_dut_signal(dut, path)
            if value is not None:
                return value
        return None


def test_mixed_window_actual_snapshot_ignores_contradicting_toggle_storage():
    values, expected = {}, {}
    for index, (key, candidates) in enumerate(_UPSTREAM_SIGNALS.items()):
        expected[key] = index % 2
        native = _native_signal_names(candidates)[0]
        values[native] = expected[key]
        shadow = native.replace('inner_icache.', 'inner_icache.__Vtogcov__')
        if shadow == native: shadow = native.replace('Frontend.', 'Frontend.__Vtogcov__', 1)
        values[shadow] = 1 - expected[key]
    recorder = Recorder(values)
    assert _upstream_snapshot(recorder) == expected
    assert all('__Vtogcov__' not in path for path in recorder.read_paths)


def test_mixed_window_actual_snapshot_rejects_shadow_only_missing_native():
    values = {}
    for key, candidates in _UPSTREAM_SIGNALS.items():
        values[_native_signal_names(candidates)[0]] = 0
    missing = 'second_requested'
    for path in _native_signal_names(_UPSTREAM_SIGNALS[missing]): values.pop(path, None)
    for path in _UPSTREAM_SIGNALS[missing]:
        values[path if '__Vtogcov__' in path else path.replace('inner_icache.', 'inner_icache.__Vtogcov__')] = 1
    with pytest.raises(AssertionError, match='missing_native_upstream_probe'):
        _upstream_snapshot(Recorder(values))


@pytest.mark.parametrize('candidates', [
    ('Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_ready',),
    ('Frontend_top.Frontend.inner_icache.mainPipe.s0_fire',),
    ('Frontend_top.Frontend.inner_ifu.__Vtogcov__s0_fetchBlock_0_valid',),
])
def test_trained_nc_actual_consumer_uses_native_when_shadow_disagrees(candidates):
    native = _candidate_paths(candidates)
    assert native and all('__Vtogcov__' not in p for p in native)
    values = {native[-1]: 1}
    for p in candidates:
        shadow = p if '__Vtogcov__' in p else p.replace('.mainPipe.', '.mainPipe.__Vtogcov__')
        if shadow not in native: values[shadow] = 0
    recorder = Recorder(values)
    value, paths = _read_native_paths(recorder, candidates)
    assert value == 1 and set(paths) == {native[-1]}
    assert all('__Vtogcov__' not in p for p in recorder.read_paths)


@pytest.mark.parametrize('candidates', [
    ('Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_ready',),
    ('Frontend_top.Frontend.inner_icache.mainPipe.s0_fire',),
])
def test_trained_nc_actual_consumer_never_accepts_shadow_only(candidates):
    shadows = {p if '__Vtogcov__' in p else p.replace('.mainPipe.', '.mainPipe.__Vtogcov__'): 1 for p in candidates}
    with pytest.raises(AssertionError, match='missing_native_probe'):
        _read_native_paths(Recorder(shadows), candidates)


def test_trained_nc_actual_consumer_still_rejects_semantic_alias_disagreement():
    candidates = ('Frontend_top.Frontend.inner_icache.mainPipe.s0_fire',)
    paths = _candidate_paths(candidates)
    assert len(paths) == 2
    with pytest.raises(AssertionError, match='native_alias_disagreement'):
        _read_native_paths(Recorder({paths[0]: 1, paths[1]: 0}), candidates)

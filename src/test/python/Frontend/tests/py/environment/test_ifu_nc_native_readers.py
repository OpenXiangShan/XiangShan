"""Exercise the trained-NC native reader without a compiled DUT."""

from types import SimpleNamespace

import pytest

from env.funcov.py.ifu.cacheable_pipeline_funcov import _native_signal_names
from tests.py.jiabowen.test_ifu_nc_dual_suppression_v3_dut import (
    _candidate_paths,
    _read_native_paths,
)


class Recorder:
    def __init__(self, values):
        self.env = SimpleNamespace(dut=object())
        self.values = values
        self.read_paths = []

    def _try_read_dut_signal(self, dut, path):
        del dut
        self.read_paths.append(path)
        return self.values.get(path)


_CASES = {
    "req_ready": (
        "Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_req_ready",
        "inner_icache.io_toIfu_req_ready",
    ),
    "s0_fire": (
        "Frontend_top.Frontend.inner_icache.mainPipe.s0_fire",
        (
            "Frontend_top.Frontend.inner_icache.mainPipe.s0_fire",
            "inner_icache.mainPipe.s0_fire",
        ),
    ),
    "s0_valid": (
        "Frontend_top.Frontend.inner_ifu.__Vtogcov__s0_fetchBlock_0_valid",
        "inner_ifu.s0_fetchBlock_0_valid",
    ),
}


@pytest.mark.parametrize("case", _CASES)
def test_trained_nc_consumer_uses_2958_native_when_shadow_disagrees(case):
    original, native = _CASES[case]
    native_paths = native if isinstance(native, tuple) else (native,)
    values = {path: 1 for path in native_paths}
    values.setdefault(original, 0)
    recorder = Recorder(values)

    value, paths = _read_native_paths(recorder, (original,))
    assert value == 1
    assert set(paths) == set(native_paths)
    assert all("__Vtogcov__" not in path for path in recorder.read_paths)


@pytest.mark.parametrize("case", ("req_ready", "s0_fire"))
def test_trained_nc_consumer_never_accepts_shadow_only(case):
    original, _ = _CASES[case]
    shadow = original.replace(
        ".inner_icache.", ".inner_icache.__Vtogcov__", 1
    )
    if shadow == original:
        shadow = original.replace(
            ".inner_ifu.", ".inner_ifu.__Vtogcov__", 1
        )

    with pytest.raises(AssertionError, match="missing_native_probe"):
        _read_native_paths(Recorder({shadow: 1}), (original,))


def test_trained_nc_consumer_rejects_semantic_alias_disagreement():
    original = "Frontend_top.Frontend.inner_icache.mainPipe.s0_fire"
    paths = _candidate_paths((original,))
    assert len(paths) == 2

    with pytest.raises(AssertionError, match="native_alias_disagreement"):
        _read_native_paths(Recorder({paths[0]: 1, paths[1]: 0}), (original,))


def test_trained_nc_candidates_do_not_include_toggle_storage():
    for original, _ in _CASES.values():
        assert all(
            "__Vtogcov__" not in path
            for path in _native_signal_names((original,))
        )

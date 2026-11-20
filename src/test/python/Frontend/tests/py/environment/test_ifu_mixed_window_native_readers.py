"""Exercise the mixed-window upstream reader without a compiled DUT."""

from types import SimpleNamespace

import pytest

from env.funcov.py.ifu.cacheable_pipeline_funcov import _UPSTREAM_SIGNALS
from tests.py.jiabowen.test_ifu_cacheable_mixed_window_v3_dut import _upstream_snapshot


_NATIVE_2958 = {
    "mainpipe_fire": "Frontend_top.Frontend.inner_icache.mainPipe.s0_fire",
    "second_requested": "inner_icache.io_fromFtq_toMainPipe_bits_req_1_valid",
    "second_waylookup_valid": (
        "Frontend_top.Frontend.inner_icache.mainPipe."
        "io_fromWayLookup_bits_wayLookupInfo_1_valid"
    ),
    "first_mmio": (
        "Frontend_top.Frontend.inner_icache.mainPipe."
        "io_fromWayLookup_bits_wayLookupInfo_0_bits_entry_isMmio"
    ),
    "second_mmio": (
        "Frontend_top.Frontend.inner_icache.mainPipe."
        "io_fromWayLookup_bits_wayLookupInfo_1_bits_entry_isMmio"
    ),
    "first_itlb_exception": (
        "inner_icache.mainPipe.io_fromWayLookup_bits_wayLookupInfo_0_bits_"
        "exceptionEntry_itlbException_value"
    ),
    "second_itlb_exception": (
        "inner_icache.mainPipe.io_fromWayLookup_bits_wayLookupInfo_1_bits_"
        "exceptionEntry_itlbException_value"
    ),
    "first_ftq_flag": "inner_icache.io_fromFtq_toMainPipe_bits_req_0_ftqIdx_flag",
    "first_ftq_value": "inner_icache.io_fromFtq_toMainPipe_bits_req_0_ftqIdx_value",
    "real_two_fetch": "inner_icache.io_toFtq_fromMainPipe_realTwoFetchValid",
}


class Recorder:
    def __init__(self, values):
        self.env = SimpleNamespace(dut=object())
        self.values = values
        self.read_paths = []

    def _read_first_dut_signal(self, dut, paths):
        del dut
        for path in paths:
            self.read_paths.append(path)
            if path in self.values:
                return self.values[path]
        return None


def test_mixed_window_reader_uses_2958_native_paths_and_ignores_toggle_storage():
    expected = {key: index % 2 for index, key in enumerate(_UPSTREAM_SIGNALS)}
    values = {_NATIVE_2958[key]: value for key, value in expected.items()}
    for key in _UPSTREAM_SIGNALS:
        shadow = _NATIVE_2958[key].replace(
            "inner_icache.", "inner_icache.__Vtogcov__", 1
        )
        values[shadow] = 1 - expected[key]
    recorder = Recorder(values)

    assert _upstream_snapshot(recorder) == expected
    assert recorder.read_paths
    assert all("__Vtogcov__" not in path for path in recorder.read_paths)


def test_mixed_window_reader_rejects_shadow_only_missing_native():
    missing = "second_requested"
    values = {_NATIVE_2958[key]: 0 for key in _UPSTREAM_SIGNALS if key != missing}
    values[
        "inner_icache.__Vtogcov__io_fromFtq_toMainPipe_bits_req_1_valid"
    ] = 1

    with pytest.raises(AssertionError, match="missing_native_upstream_probe"):
        _upstream_snapshot(Recorder(values))

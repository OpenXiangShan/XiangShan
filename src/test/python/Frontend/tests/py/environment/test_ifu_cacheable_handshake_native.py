"""The cacheable IFU handshake comes from current MainPipe port values."""
from types import SimpleNamespace

import pytest
from env.funcov.py.ifu.cacheable_pipeline_funcov import _SIGNALS, _read_signal


@pytest.mark.parametrize("key", ["req_valid", "req_ready"])
def test_handshake_reads_native_mainpipe_and_rejects_toggle_only(key):
    native = f"Frontend_top.Frontend.inner_icache.mainPipe.io_toIfu_{key}"
    shadow = f"Frontend_top.Frontend.inner_icache.__Vtogcov__io_toIfu_{key}"
    values = {native: 0, shadow: 1}
    def read(_dut, paths):
        return next((values[p] for p in paths if p in values), None)
    recorder = SimpleNamespace(env=SimpleNamespace(dut=object()), _read_first_dut_signal=read)
    assert _SIGNALS[key] == (native,)
    assert _read_signal(recorder, key) == 0
    del values[native]
    assert _read_signal(recorder, key) is None

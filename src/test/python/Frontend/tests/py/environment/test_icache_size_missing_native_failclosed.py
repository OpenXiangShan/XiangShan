from types import SimpleNamespace
import pytest
from env.funcov.py.ifu.cacheable_pipeline_funcov import _read_req_field


@pytest.mark.parametrize("index,offset,poison", [(0,None,0),(0,-1,3),(0,32,32),(1,None,15)])
def test_missing_or_invalid_native_offset_cannot_use_short_size_alias(index, offset, poison):
    path = "Frontend_top.Frontend.inner_icache.mainPipe." + f"s1_maybeRvcAlignInfo_takenCfiOffset_{index}"
    def poison_reader(_dut, _names):
        raise AssertionError("size must not consult the unreliable short alias")
    recorder = SimpleNamespace(env=SimpleNamespace(dut=object()),
        _try_read_dut_signal=lambda _dut, name: offset if name == path else None,
        _read_first_dut_signal=poison_reader)
    assert _read_req_field(recorder,index,"size") is None


@pytest.mark.parametrize("index,offset,expected", [(0,0,1),(0,14,15),(0,31,32),(1,31,32)])
def test_size_boundary_uses_full_native_width(index, offset, expected):
    path = "Frontend_top.Frontend.inner_icache.mainPipe." + f"s1_maybeRvcAlignInfo_takenCfiOffset_{index}"
    recorder = SimpleNamespace(env=SimpleNamespace(dut=object()),
        _try_read_dut_signal=lambda _dut, name: offset if name == path else None,
        _read_first_dut_signal=lambda _dut, _names: 0)
    assert _read_req_field(recorder,index,"size") == expected

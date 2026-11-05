"""Preserve the actual six-bit size; a wrap or toggle alias is not evidence."""
from types import SimpleNamespace

import pytest

from env.support.ifu_native_aliases import read_icache_fetch_block_size
from env.funcov.py.ifu.cacheable_pipeline_funcov import _read_req_field


_PREFIX = "Frontend_top.Frontend.inner_icache.mainPipe."


@pytest.mark.parametrize("index,offset,size", [(0, 0, 1), (0, 14, 15), (0, 31, 32), (1, 0, 1), (1, 31, 32)])
def test_six_bit_size_matches_emitted_rtl_without_truncation(index, offset, size):
    path = _PREFIX + f"s1_maybeRvcAlignInfo_takenCfiOffset_{index}"
    projection = read_icache_fetch_block_size({path: offset}.get, index)
    assert projection.value == size
    assert projection.source_values == {path: offset}


@pytest.mark.parametrize("value", [None, -1, 32])
def test_missing_or_out_of_range_source_is_not_synthesized(value):
    assert read_icache_fetch_block_size(lambda _path: value, 0) is None


def test_shadow_offset_alone_is_unavailable():
    shadow = _PREFIX + "__Vtogcov__s1_maybeRvcAlignInfo_takenCfiOffset_0"
    assert read_icache_fetch_block_size({shadow: 31}.get, 0) is None


@pytest.mark.parametrize("index", [-1, 2])
def test_nonexistent_fetch_block_is_rejected(index):
    with pytest.raises(ValueError):
        read_icache_fetch_block_size(lambda _: 0, index)


@pytest.mark.parametrize("offset,wrong_alias", [(14, 3), (31, 0)])
def test_actual_reader_prefers_full_native_projection_to_bad_alias(offset, wrong_alias):
    path = _PREFIX + "s1_maybeRvcAlignInfo_takenCfiOffset_0"
    recorder = SimpleNamespace(
        env=SimpleNamespace(dut=object()),
        _try_read_dut_signal=lambda _dut, name: offset if name == path else None,
        _read_first_dut_signal=lambda _dut, _names: wrong_alias,
    )
    assert _read_req_field(recorder, 0, "size") == offset + 1


def test_contract_comes_from_current_generated_six_bit_equation():
    from env.runtime.pylib import frontend_build_root_path
    rtl = (frontend_build_root_path() / "rtl/ICacheMainPipe.sv").read_text()
    for index in (0, 1):
        assert f"output [5:0]   io_toIfu_req_bits_info_{index}_size" in rtl
        assert f"6'({{1'h0, s1_maybeRvcAlignInfo_takenCfiOffset_{index}}} + 6'h1)" in rtl

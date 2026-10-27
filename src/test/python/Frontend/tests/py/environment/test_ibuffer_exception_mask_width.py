"""Full-width IFU interface reads must preserve the last enqueue lane."""
from types import SimpleNamespace

import pytest

from env.funcov.py.ibuffer.sampler import sample_ibuffer_contract
from env.funcov.py.ifu.compact_funcov import (
    _read_exception_mask_bit,
    _read_ifu_output_mask,
)
from env.funcov.recorder import FrontendFuncovSampleHub


_PREFIX = "Frontend_top.Frontend.inner_ifu."


def _recorder(values):
    return SimpleNamespace(
        _read_first_dut_signal=lambda _dut, names: next(
            (values[name] for name in names if name in values), None
        ),
        risk_observations=[],
    )


@pytest.mark.parametrize("sampler", [
    sample_ibuffer_contract,
    FrontendFuncovSampleHub._sample_ibuffer_contract,
])
@pytest.mark.parametrize("enqueued", [False, True])
def test_last_lane_exception_is_preserved_and_checked_against_full_enqueue(sampler, enqueued):
    high_bit = 1 << 35
    values = {
        "Frontend_top.Frontend._inner_ifu_io_toIBuffer_valid": 1,
        _PREFIX + "io_toIBuffer_bits_exceptionMask": high_bit,
        _PREFIX + "io_toIBuffer_bits_enqEnable": high_bit if enqueued else 1,
        # A scalar-looking legacy alias must not hide the full interface bus.
        _PREFIX + "io_toIBuffer_bits_enqEnable_0": 1,
    }
    recorder = _recorder(values)

    sampler(recorder, object(), 7)

    observation, = recorder.risk_observations
    assert observation["exception_mask"] == high_bit
    assert observation["enq_enable"] == (high_bit if enqueued else 1)
    assert observation["mask_without_enq"] == (0 if enqueued else high_bit)
    assert observation["aligned"] is enqueued


def test_compact_exception_reader_keeps_last_lane_and_missing_signal():
    recorder = _recorder({_PREFIX + "io_toIBuffer_bits_exceptionMask": (1 << 35) | 1})
    assert _read_ifu_output_mask(recorder, object(), "exceptionMask") == (1 << 35) | 1
    assert _read_exception_mask_bit(recorder, object(), 35) == 1
    assert _read_exception_mask_bit(recorder, object(), 34) == 0
    missing = _recorder({})
    assert _read_ifu_output_mask(missing, object(), "exceptionMask") is None


def test_vector_mask_reader_includes_last_interface_lane_and_requires_it():
    values = {_PREFIX + f"io_toIBuffer_bits_triggered_{i}": int(i == 35) for i in range(36)}
    recorder = _recorder(values)
    assert _read_ifu_output_mask(recorder, object(), "triggered") == 1 << 35
    del values[_PREFIX + "io_toIBuffer_bits_triggered_35"]
    assert _read_ifu_output_mask(recorder, object(), "triggered") is None


@pytest.mark.parametrize("sampler", [sample_ibuffer_contract, FrontendFuncovSampleHub._sample_ibuffer_contract])
@pytest.mark.parametrize("missing", ["io_toIBuffer_bits_exceptionMask", "io_toIBuffer_bits_enqEnable"])
def test_missing_packed_interface_never_becomes_alignment_observation(sampler, missing):
    values = {
        "Frontend_top.Frontend._inner_ifu_io_toIBuffer_valid": 1,
        _PREFIX + "io_toIBuffer_bits_exceptionMask": 1 << 35,
        _PREFIX + "io_toIBuffer_bits_enqEnable": 1 << 35,
    }
    del values[_PREFIX + missing]
    recorder = _recorder(values)
    sampler(recorder, object(), 9)
    assert recorder.risk_observations == []


def test_packed_adapter_does_not_relax_bin836_fixed_taken_contract(tmp_path):
    from tests.py.jiabowen.test_ifu_compact_functional_coverage import _make_recorder, _set_ifu_output
    from env.funcov.py.ifu.cfvec_funcov import sample_cfvec_coverage
    recorder, env, dut, memory = _make_recorder(tmp_path)
    memory.write32(0x80000000, 0x13)
    _set_ifu_output(dut, [(0, 0x80000000, 0x13, 0, 1, 0, 1, 0)])
    sample_cfvec_coverage(recorder, env, 1)
    assert not recorder.key_hit("ifu_ibuffer_output", "taken_end_metadata")

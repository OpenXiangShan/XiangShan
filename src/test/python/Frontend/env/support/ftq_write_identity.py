"""Resolve the native FTQ prediction pointer selected for a backend PC write."""
from __future__ import annotations

from .signal_utils import _lookup_pin

_POINTER_INPUTS = {
    's3_override': 'inner_ftq.io_fromBpu_prediction_bits_s3Override',
    's2_override': 'inner_ftq.io_fromBpu_prediction_bits_s2Override',
    's3_flag': 'inner_ftq.io_fromBpu_s3FtqPtr_flag',
    's3_value': 'inner_ftq.io_fromBpu_s3FtqPtr_value',
    's2_flag': 'inner_ftq.io_fromBpu_s2FtqPtr_flag',
    's2_value': 'inner_ftq.io_fromBpu_s2FtqPtr_value',
    'bpu_flag': 'inner_ftq.bpuPtr_ptrs_0_flag',
    'bpu_value': 'inner_ftq.bpuPtr_ptrs_0_value',
}


def ftq_write_flag(reader, index: int) -> int | None:
    """Mirror Ftq.predictionPtr; never guess a generation for missing probes."""
    def read(key, bits):
        value = reader(_POINTER_INPUTS[key])
        return value if type(value) is int and 0 <= value < (1 << bits) else None

    s3 = read('s3_override', 1)
    s2 = read('s2_override', 1)
    if s3 is None or s2 is None:
        return None
    source = 's3' if s3 else ('s2' if s2 else 'bpu')
    flag, value = read(source + '_flag', 1), read(source + '_value', 6)
    return flag if flag is not None and value == index else None


def read_ftq_write_flag(dut, index: int) -> int | None:
    if getattr(dut, "_is_fake_frontend_dut", False):
        return None

    def read(name):
        try:
            pin = _lookup_pin(dut, name)
            value = getattr(pin, 'value', None)
            return None if value is None else int(value)
        except (AttributeError, TypeError, ValueError):
            return None
    return ftq_write_flag(read, index)

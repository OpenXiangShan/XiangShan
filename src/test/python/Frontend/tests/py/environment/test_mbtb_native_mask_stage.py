import pytest
from env.support.bpu_v3_contract import (
    MBTB_WRITE_BUFFER_SIGNAL_GROUPS as groups,
    BpuV3SignalUnavailable, sample_mbtb_write_buffer_events,
)

def values():
    return {v[0]: 0 for k, v in groups.items() if k.endswith(("_write_valid", "_hit_mask", "_dirty_mask"))}

def put(v, key, value):
    v[groups[key][0]] = value

@pytest.mark.parametrize('port', range(4))
@pytest.mark.parametrize('bit', range(16))
def test_each_port_and_row_entry(port, bit):
    v = values()
    prefix = f'mbtb_a0_i0_p{port}_'
    row, entry = divmod(bit, 4)
    put(v, prefix + 'write_valid', 1)
    put(v, prefix + 'hit_mask', 1 << bit)
    fields = ('setIdx', 'entry_tag', 'entry_attribute_branchType', 'entry_attribute_rasAction',
              'entry_position', 'entry_targetCarry_value', 'entry_targetLowerBits')
    for field in fields:
        put(v, prefix + 'input_' + field, 0)
        put(v, f'mbtb_a0_i0_r{row}_e{entry}_' + field, 0)
    put(v, f'mbtb_a0_i0_r{row}_e{entry}_dirty', 0)
    (event,) = sample_mbtb_write_buffer_events(v.get)
    assert (event.port, event.row, event.entry) == (port, row, entry)

@pytest.mark.parametrize('valid,hit,dirty', [(2, 0, 0), (0, 1, 0), (1, 3, 0), (1, 1 << 16, 0), (1, 0, -1)])
def test_invalid_masks_fail_closed(valid, hit, dirty):
    v = values()
    for field, val in [('write_valid', valid), ('hit_mask', hit), ('dirty_mask', dirty)]:
        put(v, 'mbtb_a0_i0_p0_' + field, val)
    with pytest.raises(AssertionError):
        sample_mbtb_write_buffer_events(v.get)

def test_absent_native_mask_fails_even_if_toggle_present():
    v = values()
    key = 'mbtb_a0_i0_p0_hit_mask'
    native = groups[key][0]
    del v[native]
    v[native.replace('entryWriteBuffer.', 'entryWriteBuffer.__Vtogcov__')] = 0
    with pytest.raises(BpuV3SignalUnavailable):
        sample_mbtb_write_buffer_events(v.get)

def test_dirty_hit_is_not_reported_as_written():
    v = values()
    put(v, 'mbtb_a0_i0_p0_write_valid', 1)
    put(v, 'mbtb_a0_i0_p0_hit_mask', 1 << 7)
    put(v, 'mbtb_a0_i0_p0_dirty_mask', 1 << 7)
    assert not sample_mbtb_write_buffer_events(v.get)

def test_mismatched_dirty_bit_rejected():
    v = values()
    put(v, 'mbtb_a0_i0_p0_write_valid', 1)
    put(v, 'mbtb_a0_i0_p0_hit_mask', 1)
    fields = ('setIdx', 'entry_tag', 'entry_attribute_branchType', 'entry_attribute_rasAction', 'entry_position', 'entry_targetCarry_value', 'entry_targetLowerBits')
    for field in fields:
        put(v, 'mbtb_a0_i0_p0_input_' + field, 0)
        put(v, 'mbtb_a0_i0_r0_e0_' + field, 0)
    put(v, 'mbtb_a0_i0_r0_e0_dirty', 1)
    with pytest.raises(AssertionError, match='disagrees'):
        sample_mbtb_write_buffer_events(v.get)


def test_simultaneous_ports_preserve_independent_events():
    v = values()
    fields = ('setIdx', 'entry_tag', 'entry_attribute_branchType', 'entry_attribute_rasAction',
              'entry_position', 'entry_targetCarry_value', 'entry_targetLowerBits')
    for port in (0, 2):
        prefix = f'mbtb_a0_i0_p{port}_'
        put(v, prefix + 'write_valid', 1)
        put(v, prefix + 'hit_mask', 1 << 9)
        for field in fields:
            put(v, prefix + 'input_' + field, 0)
    for field in fields:
        put(v, 'mbtb_a0_i0_r2_e1_' + field, 0)
    put(v, 'mbtb_a0_i0_r2_e1_dirty', 0)
    events = sample_mbtb_write_buffer_events(v.get)
    assert [(event.port, event.row, event.entry) for event in events] == [(0, 2, 1), (2, 2, 1)]

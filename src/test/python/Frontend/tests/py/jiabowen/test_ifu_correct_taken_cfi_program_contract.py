"""Independent RV64I execution check for the BIN-932 program/trace pair."""

from dataclasses import replace

import pytest

from tests.py.jiabowen.test_ifu_correct_taken_cfi_kinds_v3_dut import (
    _BASE, _architectural_trace, _program,
)


def _signed(value, bits):
    return value - (1 << bits) if value & (1 << (bits - 1)) else value


def _check_trace(payload, entries):
    registers, pc = [0] * 32, _BASE
    for entry in entries:
        assert entry.pc == pc
        offset = pc - _BASE
        assert 0 <= offset <= len(payload) - 4
        instruction = int.from_bytes(payload[offset:offset + 4], "little")
        assert instruction == entry.instr and entry.size == 4
        opcode = instruction & 127
        rd, rs1 = (instruction >> 7) & 31, (instruction >> 15) & 31
        next_pc = pc + 4
        if opcode == 0x17:
            registers[rd] = pc + _signed(instruction & 0xFFFFF000, 32)
        elif opcode == 0x13:
            assert (instruction >> 12) & 7 == 0
            registers[rd] = registers[rs1] + _signed(instruction >> 20, 12)
        elif opcode == 0x6F:
            imm = ((instruction >> 31) << 20 | ((instruction >> 12) & 255) << 12
                   | ((instruction >> 20) & 1) << 11 | ((instruction >> 21) & 1023) << 1)
            next_pc = pc + _signed(imm, 21)
            registers[rd] = pc + 4
        elif opcode == 0x67:
            assert (instruction >> 12) & 7 == 0
            next_pc = (registers[rs1] + _signed(instruction >> 20, 12)) & ~1
            registers[rd] = pc + 4
        else:
            raise AssertionError(f"unexpected opcode {opcode:x}")
        if opcode in (0x6F, 0x67):
            assert entry.taken and entry.target_pc == next_pc
        else:
            assert not entry.taken and entry.target_pc is None
        registers[0] = 0
        pc = next_pc


def test_bin932_program_has_repeated_isa_correct_direct_indirect_call_ret():
    payload, _ = _program()
    trace = _architectural_trace(32)
    _check_trace(payload, trace.entries)
    assert len(trace.entries) == 3 + 5 * 32
    assert sum(e.kind == "ret" for e in trace.entries) == 32
    assert sum(e.kind == "jump_indirect" for e in trace.entries) == 32


@pytest.mark.parametrize("index,target", [(4, 0), (4, _BASE + 0x144), (6, _BASE + 0x300)])
def test_bin932_oracle_rejects_wrong_indirect_and_return_targets(index, target):
    payload, _ = _program()
    trace = _architectural_trace(2)
    trace.entries[index] = replace(trace.entries[index], target_pc=target)
    with pytest.raises(AssertionError):
        _check_trace(payload, trace.entries)


def test_bin932_oracle_rejects_return_followed_by_accidental_self_loop():
    payload, _ = _program()
    damaged = bytearray(payload)
    damaged[0x304:0x308] = (0x6F).to_bytes(4, "little")
    with pytest.raises(AssertionError):
        _check_trace(bytes(damaged), _architectural_trace(2).entries)

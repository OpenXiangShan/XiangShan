"""Decode executable bytes independently and reject unbound MBTB evidence."""
from dataclasses import replace
import json

import pytest

from env.support.bpu_v3_contract import BpuV3SignalUnavailable, MbtbWriteEntry
from tests.py.support.mbtb_write_program import build_mbtb_write_compare_program
from tests.py.support.mbtb_training_request import (
    TRAINING_REQUEST_FIELDS, sample_mbtb_training_request,
)


def _execute(payload, trace, base):
    """RV64 subset interpreter; targets arise from register arithmetic."""
    registers, pc, source_jumps = [0] * 32, base, []
    mask = (1 << 64) - 1

    def signed(value, bits):
        return value - (1 << bits) if value & (1 << (bits - 1)) else value

    for oracle in trace.entries:
        assert oracle.pc == pc and 0 <= pc - base <= len(payload) - 4
        instruction = int.from_bytes(payload[pc - base:pc - base + 4], "little")
        assert oracle.instr == instruction and oracle.size == 4
        opcode = instruction & 127
        rd, rs1, rs2 = (instruction >> 7) & 31, (instruction >> 15) & 31, (instruction >> 20) & 31
        next_pc, taken, kind = pc + 4, False, "normal"
        if opcode == 0x17:
            registers[rd] = (pc + signed(instruction & 0xfffff000, 32)) & mask
        elif opcode == 0x13:
            assert (instruction >> 12) & 7 == 0
            registers[rd] = (registers[rs1] + signed(instruction >> 20, 12)) & mask
        elif opcode == 0x67:
            assert (instruction >> 12) & 7 == 0
            next_pc = (registers[rs1] + signed(instruction >> 20, 12)) & ~1
            registers[rd] = pc + 4
            taken, kind = True, "jump_indirect"
            if pc == base + 0x40:
                assert rd == 0 and rs1 == 6 and instruction >> 20 == 0
                source_jumps.append((pc, next_pc))
        elif opcode == 0x63:
            assert (instruction >> 12) & 7 == 1
            offset = (((instruction >> 31) & 1) << 12) | (((instruction >> 7) & 1) << 11)
            offset |= (((instruction >> 25) & 63) << 5) | (((instruction >> 8) & 15) << 1)
            taken, kind = registers[rs1] != registers[rs2], "branch"
            next_pc = pc + signed(offset, 13) if taken else pc + 4
        elif opcode == 0x6f:
            offset = (((instruction >> 31) & 1) << 20) | (((instruction >> 12) & 255) << 12)
            offset |= (((instruction >> 20) & 1) << 11) | (((instruction >> 21) & 1023) << 1)
            next_pc, taken, kind = pc + signed(offset, 21), True, "jump"
            registers[rd] = pc + 4
        else:
            raise AssertionError(f"unsupported instruction {instruction:#x}")
        assert oracle.kind == kind and oracle.taken == taken
        assert oracle.target_pc == (next_pc if kind != "normal" else None)
        registers[0] = 0
        pc = next_pc
    return source_jumps


@pytest.mark.parametrize("base,stable,changed", [
    (0x80000000, 3, 2), (0x80000000, 7, 5), (0x80000000, 16, 16),
    (0x80200000, 3, 2), (0x80200000, 31, 4),
])
def test_every_oracle_entry_follows_executable_rv64_bytes(base, stable, changed):
    payload, trace, facts = build_mbtb_write_compare_program(
        base, stable_iterations=stable, changed_iterations=changed)
    jumps = _execute(payload, trace, base)
    assert jumps == ([(facts["source_pc"], facts["initial_target"])] * stable
                     + [(facts["source_pc"], facts["changed_target"])] * changed)
    assert trace.entries[facts["switch_trace_index"]].target_pc == facts["changed_target"]
    # Full target differs from MainBtb's one-region carry reconstruction for
    # every repetition; equality here is never an optimistic branch oracle.
    for target, predicted in ((facts["initial_target"], facts["mbtb_predicted_initial_target"]),
                              (facts["changed_target"], facts["mbtb_predicted_changed_target"])):
        source_region = facts["source_pc"] >> 21
        assert target >> 21 == source_region + 2
        reconstructed = ((source_region + 1) << 21) | (target & ((1 << 21) - 1))
        assert reconstructed == predicted and reconstructed != target
    assert facts["position"] == 1  # last halfword of source's 32-bit JALR
    assert (facts["align_bank"], facts["internal_bank"], facts["set_idx"]) == (0, 1, 0)


def test_oracle_cannot_claim_a_target_that_register_execution_does_not_take():
    payload, trace, facts = build_mbtb_write_compare_program(stable_iterations=3, changed_iterations=2)
    index = facts["switch_trace_index"]
    trace.entries[index] = replace(trace.entries[index], target_pc=facts["initial_target"])
    with pytest.raises(AssertionError):
        _execute(payload, trace, 0x80000000)


def test_nop_cannot_masquerade_as_the_registered_other_indirect():
    payload, trace, facts = build_mbtb_write_compare_program(stable_iterations=3, changed_iterations=2)
    data = bytearray(payload)
    data[0x40:0x44] = (0x13).to_bytes(4, "little")
    trace.entries = [replace(item, instr=0x13) if item.pc == facts["source_pc"] else item
                     for item in trace.entries]
    with pytest.raises(AssertionError):
        _execute(bytes(data), trace, 0x80000000)


def _native_request(facts):
    root = "Frontend_top.Frontend.inner_bpu.mbtb.alignBanks_0."
    fields = dict(valid=1, need_write=1, mispredict_valid=1,
                  start_pc=facts["source_pc"] >> 1, position=facts["position"],
                  target=facts["initial_target"] >> 1, branch_type=3, ras_action=0)
    return {root + TRAINING_REQUEST_FIELDS[key]: value for key, value in fields.items()}


def _entry(facts):
    return MbtbWriteEntry(facts["set_idx"], facts["tag"], 3, 0, facts["position"],
                         1, (facts["initial_target"] >> 1) & ((1 << 20) - 1))


def test_native_request_preserves_full_target_and_handshake_source_evidence():
    _, _, facts = build_mbtb_write_compare_program()
    values = _native_request(facts)
    sample = sample_mbtb_training_request(values.get, 0)
    sample.require_source(facts, _entry(facts))
    assert sample.target == facts["initial_target"] and sample.start_pc == facts["source_pc"]
    assert sample.source_values == values
    assert all("__Vtogcov__" not in path for path in sample.signal_paths.values())


@pytest.mark.parametrize("field", list(TRAINING_REQUEST_FIELDS))
def test_missing_native_request_cannot_fall_back_to_toggle_or_default(field):
    _, _, facts = build_mbtb_write_compare_program()
    values = _native_request(facts)
    suffix = TRAINING_REQUEST_FIELDS[field]
    old = values.pop("Frontend_top.Frontend.inner_bpu.mbtb.alignBanks_0." + suffix)
    values["Frontend_top.Frontend.inner_bpu.mbtb.alignBanks_0.__Vtogcov__" + suffix] = old
    with pytest.raises(BpuV3SignalUnavailable):
        sample_mbtb_training_request(values.get, 0)


@pytest.mark.parametrize("field,value", [
    ("valid", 0), ("need_write", 0), ("mispredict_valid", 0),
    ("start_pc", 0x40000030), ("position", 2),
    ("target", 0x40100800), ("branch_type", 2), ("ras_action", 1),
])
def test_target_alias_identity_and_inactive_request_never_qualify(field, value):
    _, _, facts = build_mbtb_write_compare_program()
    values = _native_request(facts)
    values["Frontend_top.Frontend.inner_bpu.mbtb.alignBanks_0." + TRAINING_REQUEST_FIELDS[field]] = value
    sample = sample_mbtb_training_request(values.get, 0)
    with pytest.raises(AssertionError):
        sample.require_source(facts, _entry(facts))


def test_all_training_parents_exist_as_native_frozen_inventory_probes():
    from env.runtime.pylib import frontend_pylib_path
    names = set()
    for line in (frontend_pylib_path() / "Frontend/Frontend_offset.yaml").read_text().splitlines():
        if line.startswith("  - name: "):
            value = line[10:]
            names.add(json.loads(value) if value.startswith('"') else value)
    root = "Frontend_top.Frontend.inner_bpu.mbtb.alignBanks_0."
    assert {root + suffix for suffix in TRAINING_REQUEST_FIELDS.values()} <= names


def test_other_align_bank_cannot_silently_inherit_this_fixture_contract():
    with pytest.raises(AssertionError, match="alignBank0"):
        sample_mbtb_training_request(lambda _: 0, 1)

"""Offline native MBTB projections and registered S3 target contracts."""
from dataclasses import replace
from pathlib import Path
import json

import pytest

from env.support.bpu_v3_contract import (
    BPU_TARGET_DIFF_SIGNAL_GROUPS, MBTB_WRITE_BUFFER_SIGNAL_GROUPS,
    BpuV3SignalUnavailable, sample_bpu_target_diff_cycle, sample_mbtb_write_buffer_events,
)
from tests.py.support.bpu_target_program import build_bpu_target_change_program


_BASE = "Frontend_top.Frontend.inner_bpu.mbtb.alignBanks_0.internalBanks_0.entryWriteBuffer."


def _mbtb_values(port=0, row=2, entry=1):
    groups = MBTB_WRITE_BUFFER_SIGNAL_GROUPS
    values = {}
    for bank in range(2):
        for internal in range(4):
            prefix = f"Frontend_top.Frontend.inner_bpu.mbtb.alignBanks_{bank}.internalBanks_{internal}.entryWriteBuffer."
            values[prefix + "dirtyUInt"] = 0
            for p in range(4):
                values[groups[f"mbtb_a{bank}_i{internal}_p{p}_write_valid"][0]] = 0
                values[prefix + "hitMaskUInt" + ("" if p == 0 else f"_{p}")] = 0
    values[groups[f"mbtb_a0_i0_p{port}_write_valid"][0]] = 1
    values[_BASE + "hitMaskUInt" + ("" if port == 0 else f"_{port}")] = 1 << (row * 4 + entry)
    fields = {"setIdx": 17, "entry_tag": 0x8123, "entry_attribute_branchType": 3,
              "entry_attribute_rasAction": 0, "entry_position": 5,
              "entry_targetCarry_value": 1, "entry_targetLowerBits": 0x76123}
    for field, value in fields.items():
        values[groups[f"mbtb_a0_i0_p{port}_input_{field}"][0]] = value
        values[groups[f"mbtb_a0_i0_r{row}_e{entry}_{field}"][0]] = value
    values[groups[f"mbtb_a0_i0_r{row}_e{entry}_dirty"][0]] = 0
    return values


@pytest.mark.parametrize("port", range(4))
@pytest.mark.parametrize("row,entry", [(0, 0), (1, 3), (2, 1), (3, 2)])
def test_native_parent_projection_keeps_row_entry_and_evidence(port, row, entry):
    values = _mbtb_values(port, row, entry)
    event, = sample_mbtb_write_buffer_events(values.get)
    assert (event.port, event.row, event.entry) == (port, row, entry)
    assert event.identity_matches and not event.semantic_changed and event.dirty == 0
    hit_path = _BASE + "hitMaskUInt" + ("" if port == 0 else f"_{port}")
    assert event.source_values[hit_path] == 1 << (row * 4 + entry)
    assert event.source_values[_BASE + "dirtyUInt"] == 0
    assert all("__Vtogcov__" not in path for path in event.signal_paths.values())


@pytest.mark.parametrize("missing", ["hitMaskUInt", "dirtyUInt"])
def test_missing_native_projection_parent_cannot_use_toggle_or_default(missing):
    values = _mbtb_values()
    del values[_BASE + missing]
    values[_BASE + "__Vtogcov__" + missing] = 0
    values[_BASE + "__Vtogcov__hitWritten"] = 1
    with pytest.raises(BpuV3SignalUnavailable):
        sample_mbtb_write_buffer_events(values.get)


@pytest.mark.parametrize("hit", [3, (1 << 3) | (1 << 13), 1 << 16, -1])
def test_multihit_or_wrong_width_never_invents_a_selected_entry(hit):
    values = _mbtb_values()
    values[_BASE + "hitMaskUInt"] = hit
    with pytest.raises(AssertionError):
        sample_mbtb_write_buffer_events(values.get)


def test_dirty_native_match_cannot_be_reclassified_as_clean_hit():
    values = _mbtb_values()
    values[_BASE + "dirtyUInt"] = 1 << 9
    assert sample_mbtb_write_buffer_events(values.get) == ()


def _target(**overrides):
    fields = {key: 0 for key in BPU_TARGET_DIFF_SIGNAL_GROUPS}
    fields.update(s3_valid=1, s3_taken=1, s2_taken=1, s3_first_9=1, s2_first_9=1,
                  s3_branch_type=3, ras_enable=1, s1_target=0x40000800,
                  s2_target=0x40000802, s3_target=0x40000800, s3_start_pc=0x40000020)
    fields.update(overrides)
    values = {BPU_TARGET_DIFF_SIGNAL_GROUPS[key][0]: value for key, value in fields.items()}
    # A live S2 transaction is unrelated to the registered transaction in S3.
    values["Frontend_top.Frontend.inner_bpu.s2_s1Prediction_target_addr"] = 0xdead
    return sample_bpu_target_diff_cycle(values.get)


def test_target_sampler_uses_registered_transaction_and_tail_slot_identity():
    sample = _target()
    assert sample.target_only_candidate and not sample.target_diff
    assert sample.s3_first_taken_mask == sample.s2_first_taken_mask == 1 << 9
    assert sample.s3_start_pc == 0x80000040
    assert not _target(s2_first_9=0, s2_first_8=1).target_only_candidate


def test_s2_override_suppresses_btb_target_diff_but_not_full_ras_ittage_comparison():
    assert not _target(s2_override=1, s3_target=0x40000802).target_diff
    assert _target(s2_override=0, s3_target=0x40000802).target_diff
    for mode in ({"ittage_hit": 1}, {"s3_ras_action": 1}):
        assert not _target(s2_override=1, s3_target=0x40000802, **mode).target_diff
        assert _target(s2_override=1, s3_target=0x40000800, **mode).target_diff
        assert not _target(s2_override=0, s3_target=0x40000800, **mode).target_diff
    assert _target(s3_ras_action=1, ras_enable=0).target_source == "btb"


@pytest.mark.parametrize("missing", ["s1_target", "s2_target", "s2_override", "s2_first_9", "s3_first_9"])
def test_target_contract_requires_every_registered_native_parent(missing):
    fields = {paths[0]: 0 for paths in BPU_TARGET_DIFF_SIGNAL_GROUPS.values()}
    path = BPU_TARGET_DIFF_SIGNAL_GROUPS[missing][0]
    del fields[path]
    fields[path.replace("inner_bpu.", "inner_bpu.__Vtogcov__")] = 0
    with pytest.raises(BpuV3SignalUnavailable):
        sample_bpu_target_diff_cycle(fields.get)


def test_first_taken_multihot_cannot_qualify_target_only_evidence():
    with pytest.raises(AssertionError, match="one-hot"):
        _target(s3_first_0=1)


def test_all_target_and_mbtb_projection_parent_probes_exist_in_frozen_inventory():
    from env.runtime.pylib import frontend_pylib_path
    names = set()
    for line in (frontend_pylib_path() / "Frontend/Frontend_offset.yaml").read_text().splitlines():
        if line.startswith("  - name: "):
            value = line[10:]
            names.add(json.loads(value) if value.startswith('"') else value)
    assert {paths[0] for paths in BPU_TARGET_DIFF_SIGNAL_GROUPS.values()} <= names
    for bank in range(2):
        for internal in range(4):
            prefix = f"Frontend_top.Frontend.inner_bpu.mbtb.alignBanks_{bank}.internalBanks_{internal}.entryWriteBuffer."
            assert prefix + "dirtyUInt" in names
            for port in range(4):
                assert prefix + "hitMaskUInt" + ("" if port == 0 else f"_{port}") in names


def _execute_trace(payload, trace, base):
    """Independent subset RV64 decoder: execute bytes and compare the oracle."""
    regs = [0] * 32
    pc = base
    jalr_targets = []
    mask = (1 << 64) - 1
    sext = lambda value, bits: value - (1 << bits) if value & (1 << (bits - 1)) else value
    for expected in trace.entries:
        assert expected.pc == pc
        word = int.from_bytes(payload[pc - base:pc - base + 4], "little")
        assert expected.instr == word and expected.size == 4
        opcode = word & 127
        rd, rs1, rs2 = (word >> 7) & 31, (word >> 15) & 31, (word >> 20) & 31
        next_pc, taken = pc + 4, False
        if opcode == 0x17:
            regs[rd] = (pc + sext(word & 0xfffff000, 32)) & mask
        elif opcode == 0x13:
            regs[rd] = (regs[rs1] + sext(word >> 20, 12)) & mask
        elif opcode == 0x67:
            next_pc, taken = (regs[rs1] + sext(word >> 20, 12)) & ~1, True
            jalr_targets.append(next_pc)
        elif opcode == 0x63:
            immediate = (((word >> 31) & 1) << 12) | (((word >> 7) & 1) << 11) | (((word >> 25) & 63) << 5) | (((word >> 8) & 15) << 1)
            taken = regs[rs1] != regs[rs2]
            next_pc = pc + sext(immediate, 13) if taken else pc + 4
        elif opcode == 0x6f:
            immediate = (((word >> 31) & 1) << 20) | (((word >> 12) & 255) << 12) | (((word >> 20) & 1) << 11) | (((word >> 21) & 1023) << 1)
            next_pc, taken = pc + sext(immediate, 21), True
        else:
            raise AssertionError(f"unexpected opcode {opcode:#x}")
        if expected.kind != "normal":
            assert expected.taken == taken and expected.target_pc == next_pc
        regs[0] = 0
        pc = next_pc
    return jalr_targets


def test_real_jalr_program_oracle_follows_instruction_execution_without_observed_target():
    payload, trace, program = build_bpu_target_change_program(stable_iterations=4, changed_iterations=3)
    assert _execute_trace(payload, trace, 0x80000000) == [program["initial_target"]] * 4 + [program["changed_target"]] * 3
    index = program["switch_trace_index"]
    trace.entries[index] = replace(trace.entries[index], target_pc=program["initial_target"])
    with pytest.raises(AssertionError):
        _execute_trace(payload, trace, 0x80000000)


def test_projection_and_target_equations_match_frozen_source_and_emitted_rtl():
    from env.runtime.pylib import frontend_build_root_path
    root = frontend_build_root_path().parent
    buffer = (root / "src/main/scala/xiangshan/frontend/bpu/WriteBuffer.scala").read_text()
    assert "val hitMaskUInt  = VecInit(hitMask.flatten).asUInt" in buffer
    assert "val hitNotWritten = (hitMaskUInt & dirtyUInt).orR" in buffer
    assert "val hitWritten    = (hitMaskUInt & ~dirtyUInt).orR" in buffer
    assert "val hitRowIdxVec = VecInit(hitMask.map(OHToUInt(_)))" in buffer
    emitted = (root / "build-frontend/rtl/WriteBuffer_4.sv").read_text()
    assert "wire [15:0]      hitMaskUInt =" in emitted
    assert "wire [15:0]      dirtyUInt =" in emitted
    assert "_hitWritten_T_1 = hitMaskUInt & ~dirtyUInt" in emitted
    bpu = (root / "src/main/scala/xiangshan/frontend/bpu/Bpu.scala").read_text()
    assert "private val s3_s1Prediction   = RegEnable(s2_s1Prediction, s2_fire)" in bpu
    assert "private val s3_s2Override     = RegEnable(s2_override, s2_fire)" in bpu
    assert "val mbtbTargetDiff = !s3_s2Override && (entry.bits.targetLower =/= s3_s1Prediction.targetLower)" in bpu
    assert "val takenDiff     = !s2_s1Prediction.taken" in bpu
    assert "val firstTakenBranchDiff = !s3_s2FirstTakenBranchOH(i)" in bpu
    bpu_rtl = (root / "build-frontend/rtl/Bpu.sv").read_text()
    assert "s3_mbtbResult_0_bits_target_addr[21:0] != s3_s1Prediction_target_addr[21:0]" in bpu_rtl

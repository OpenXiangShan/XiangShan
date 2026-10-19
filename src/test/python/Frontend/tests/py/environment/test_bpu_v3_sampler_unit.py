from dataclasses import replace

import pytest

from env.support.bpu_v3_contract import (
    BPU_TARGET_DIFF_SIGNAL_GROUPS,
    BPU_V3_SIGNAL_GROUPS,
    BTB_TARGET_COMPARE_WIDTH,
    MBTB_WRITE_BUFFER_SIGNAL_GROUPS,
    PREFETCH_DEPTH,
    PREFETCH_DEPTH_SIGNAL_GROUPS,
    BpuTargetDiffCycle,
    BpuV3SignalUnavailable,
    MbtbWriteEntry,
    read_mbtb_write_buffer_dirty,
    sample_bpu_target_diff_cycle,
    sample_bpu_v3_cycle,
    sample_mbtb_write_buffer_events,
    sample_prefetch_depth_cycle,
)


def _target_diff_cycle(**overrides: int) -> BpuTargetDiffCycle:
    values = dict(
        s3_valid=1, s3_taken=1, s1_taken=1,
        s1_cfi_position=3, s1_branch_type=2, s1_ras_action=0, s1_target=0,
        s3_cfi_position=3, s3_branch_type=2, s3_ras_action=0, s3_target=0,
        ittage_hit=0, s3_override=0, signal_paths={},
    )
    values.update(overrides)
    return BpuTargetDiffCycle(**values)


def test_bpu_sampler_requires_override_probe_and_records_selected_alias() -> None:
    values = {candidates[-1]: 0 for candidates in BPU_V3_SIGNAL_GROUPS.values()}
    values[BPU_V3_SIGNAL_GROUPS["s3_valid"][-1]] = 1
    override = BPU_V3_SIGNAL_GROUPS["s3_override"][-1]

    with pytest.raises(BpuV3SignalUnavailable) as exc_info:
        sample_bpu_v3_cycle({key: value for key, value in values.items() if key != override}.get)
    assert exc_info.value.key == "s3_override"

    sample = sample_bpu_v3_cycle(values.get)
    assert sample.is_all_not_taken_candidate
    assert sample.signal_paths["s3_override"] == override


def test_btb_target_diff_ignores_high_bits_but_ittage_and_ras_do_not() -> None:
    high_bit = 1 << BTB_TARGET_COMPARE_WIDTH
    btb = _target_diff_cycle(s3_target=high_bit)
    ittage = _target_diff_cycle(
        s1_branch_type=3, s3_branch_type=3, ittage_hit=1, s3_target=high_bit,
    )
    ras = replace(ittage, s1_ras_action=1, s3_ras_action=1)
    pop_and_push = replace(ittage, s1_ras_action=3, s3_ras_action=3)

    assert btb.target_only_candidate and btb.btb_high_only_diff
    assert not btb.target_diff
    assert _target_diff_cycle(s3_target=1).target_diff
    assert ittage.target_source == "ittage" and ittage.target_diff
    assert ras.target_source == "ras" and ras.target_diff
    assert pop_and_push.target_source == "btb" and not pop_and_push.target_diff


def test_target_diff_sampler_requires_selected_target_probe() -> None:
    values = {candidates[0]: 0 for candidates in BPU_TARGET_DIFF_SIGNAL_GROUPS.values()}
    values.pop(BPU_TARGET_DIFF_SIGNAL_GROUPS["s3_target"][0])

    with pytest.raises(BpuV3SignalUnavailable) as exc_info:
        sample_bpu_target_diff_cycle(values.get)
    assert exc_info.value.key == "s3_target"


def test_mbtb_compare_bits_exclude_identity_and_include_prediction_fields() -> None:
    stored = MbtbWriteEntry(3, 0x1234, 2, 0, 7, 1, 0x34567)
    identity_only = replace(stored, set_idx=4, tag=0x5678, position=9)

    assert identity_only.identity != stored.identity
    assert identity_only.compare_bits == stored.compare_bits
    assert replace(stored, target_lower=stored.target_lower ^ 1).compare_bits != stored.compare_bits
    assert replace(stored, ras_action=2).compare_bits != stored.compare_bits


def test_mbtb_write_buffer_sampler_selects_hit_entry_and_dirty_probe() -> None:
    groups = MBTB_WRITE_BUFFER_SIGNAL_GROUPS
    values = {
        candidates[-1]: 0
        for key, candidates in groups.items()
        if key.endswith(("_write_valid", "_hit_written", "_hit_not_written"))
    }

    def set_value(key: str, value: int) -> None:
        values[groups[key][-1]] = value

    set_value("mbtb_a0_i0_p0_write_valid", 1)
    set_value("mbtb_a0_i0_p0_hit_written", 1)
    for row in range(4):
        set_value(f"mbtb_a0_i0_p0_hit_row_{row}", int(row == 2))
        set_value(f"mbtb_a0_i0_p0_hit_index_{row}", int(row == 2))
    fields = {
        "setIdx": 3, "entry_tag": 0x1234, "entry_attribute_branchType": 2,
        "entry_attribute_rasAction": 0, "entry_position": 7,
        "entry_targetCarry_value": 1, "entry_targetLowerBits": 0x34567,
    }
    for field, value in fields.items():
        set_value(f"mbtb_a0_i0_p0_input_{field}", value)
        set_value(f"mbtb_a0_i0_r2_e1_{field}", value)
    set_value("mbtb_a0_i0_r2_e1_dirty", 0)

    (event,) = sample_mbtb_write_buffer_events(values.get)
    dirty, path = read_mbtb_write_buffer_dirty(values.get, event)

    assert (event.align_bank, event.internal_bank, event.port, event.row, event.entry) == (0, 0, 0, 2, 1)
    assert event.identity_matches and not event.semantic_changed
    assert dirty == 0 and path == groups["mbtb_a0_i0_r2_e1_dirty"][-1]


def test_prefetch_sampler_requires_shared_ready_and_uses_it_for_dual_write() -> None:
    groups = PREFETCH_DEPTH_SIGNAL_GROUPS
    values = {candidates[-1]: 0 for candidates in groups.values()}
    ready = groups["shared_write_ready"][-1]
    values[groups["num_valid_entries"][-1]] = PREFETCH_DEPTH
    values[groups["write0_valid"][-1]] = 1
    values[groups["write1_valid"][-1]] = 1

    with pytest.raises(BpuV3SignalUnavailable) as exc_info:
        sample_prefetch_depth_cycle({key: value for key, value in values.items() if key != ready}.get)
    assert exc_info.value.key == "shared_write_ready"

    assert sample_prefetch_depth_cycle(values.get).full
    assert not sample_prefetch_depth_cycle(values.get).dual_write_fire
    values[ready] = 1
    assert sample_prefetch_depth_cycle(values.get).dual_write_fire

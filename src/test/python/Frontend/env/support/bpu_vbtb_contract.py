"""DefaultConfig #6587 observation contract; no observations imply a coverage HIT.

Result slots are physical align-bank order (four SRAM ways followed by VBTB).
SC prediction/meta has ten logical entries, independent of its eight storage ways.
"""
from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass

from .bpu_v3_contract import (
    BTB_RESULT_ENTRIES, MBTB_ALIGN_BANKS, MBTB_WAYS, SC_STORAGE_WAYS,
    VBTB_WAYS_PER_ALIGN_BANK, _read_required,
)

_BPU = "Frontend_top.Frontend.inner_bpu."


def _paths(base: str, suffix: str) -> tuple[str, ...]:
    return (base + suffix,)


def vbtb_slot(align_bank: int) -> int:
    if not 0 <= align_bank < MBTB_ALIGN_BANKS:
        raise ValueError("align bank outside DefaultConfig")
    return align_bank * (MBTB_WAYS + 1) + MBTB_WAYS


def _groups() -> dict[str, tuple[str, ...]]:
    groups = {
        "s2_fire": _paths(_BPU + "mbtb.", "io_stageCtrl_s2_fire"),
        "s3_fire": _paths(_BPU + "mbtb.", "io_stageCtrl_s3_fire"),
    }
    for slot in range(BTB_RESULT_ENTRIES):
        groups[f"taken_{slot}"] = _paths(_BPU, f"s3_takenMask_{slot}")
        groups[f"sc_used_{slot}"] = _paths(_BPU + "sc.", f"io_scUsed_{slot}")
        groups[f"sc_taken_{slot}"] = _paths(_BPU, f"s3_scTakenMask_{slot}")
        groups[f"sc_way_{slot}"] = _paths(_BPU + "sc.", f"s2_wayIdx_{slot}")
        for key, suffix in {
            "s3_jump": f"s3_isJumpVec_{slot}",
            "s3_cond": f"s3_isCondVec_{slot}",
            "s3_base_taken": f"s3_mbtbResult_{slot}_bits_taken",
            "s3_sc_used": f"s3_scUsed_{slot}",
            "s3_sc_taken": f"s3_scTakenMask_{slot}",
            "s3_tage_valid": f"s3_tagePrediction_takenVec_{slot}_valid",
            "s3_tage_taken": f"s3_tagePrediction_takenVec_{slot}_bits",
        }.items():
            groups[f"{key}_{slot}"] = _paths(_BPU, suffix)
    for bank in range(MBTB_ALIGN_BANKS):
        base = _BPU + f"mbtb.alignBanks_{bank}.victimBtb."
        fields = {
            "selected": "s3_selectOH",
            "s2_selected": "s2_selectOH",
            "prediction_valid": "s2_prediction_valid",
            "prediction_position": "s2_prediction_bits_cfiPosition",
            "prediction_target": "s2_prediction_bits_target_addr",
            "meta_hit": "io_read_resp_s2_meta_rawHit",
            "meta_position": "io_read_resp_s2_meta_position",
            "pred_valid": "replacer.io_predTouch_valid",
            "pred_way": "replacer.io_predTouch_bits",
            "train_valid": "replacer.io_trainTouch_valid",
            "train_way": "replacer.io_trainTouch_bits",
            "state": "replacer.state_reg",
            "victim": "_replacer_io_victimMask",
            "snapshot_write": "snapshotWriteEvicted",
            "snapshot_mask": "snapshotEntryWayMask",
            "train_req": "io_trainEntry_req_valid",
            "entry_mask": "io_trainEntry_req_bits_entryWayMask",
            "counter_mask": "io_trainEntry_req_bits_counterWayMask",
        }
        for key, suffix in fields.items():
            # Never substitute toggle coverage state for an unbound RTL probe.
            # Inventory aliases must point to native direct/projection sources.
            parent, _, leaf = suffix.rpartition(".")
            groups[f"a{bank}_{key}"] = _paths(base + (parent + "." if parent else ""), leaf)
        # MainBtb flattens predictions but retains bank/way axes in meta.
        for key, leaf in (("meta_hit", "rawHit"), ("meta_position", "position")):
            groups[f"a{bank}_{key}"] = _paths(
                _BPU + "mbtb.", f"io_meta_entries_{bank}_{MBTB_WAYS}_{leaf}")
        for way in range(VBTB_WAYS_PER_ALIGN_BANK):
            groups[f"a{bank}_branch_type_{way}"] = _paths(base, f"entries_{way}_entry_attribute_branchType")
    return groups


VBTB_SIGNAL_GROUPS = _groups()


def _one_hot_way(mask: int, *, allow_zero: bool = False) -> int | None:
    if not 0 <= mask < (1 << VBTB_WAYS_PER_ALIGN_BANK) or mask.bit_count() > 1:
        raise AssertionError(f"invalid VBTB one-hot mask: {mask:#x}")
    if not mask:
        if allow_zero:
            return None
        raise AssertionError("VBTB operation requires a selected way")
    return mask.bit_length() - 1


def _plru_touch(state: int, way: int, ways: int = VBTB_WAYS_PER_ALIGN_BANK) -> int:
    # utility.PseudoLRU stores the root at the MSB, right subtree above left.
    if ways == 1:
        return 0
    half = ways // 2
    bits = half - 1
    submask = (1 << bits) - 1
    left, right = state & submask, (state >> bits) & submask
    if way < half:
        left = _plru_touch(left, way, half)
        root = 1
    else:
        right = _plru_touch(right, way - half, half)
        root = 0
    return (root << (ways - 2)) | (right << bits) | left


def _plru_victim(state: int, ways: int = VBTB_WAYS_PER_ALIGN_BANK) -> int:
    if ways == 1:
        return 0
    half = ways // 2
    bits = half - 1
    root = (state >> (ways - 2)) & 1
    subtree = (state >> (bits if root else 0)) & ((1 << bits) - 1)
    return root * half + _plru_victim(subtree, half)


@dataclass(frozen=True)
class VbtbCycle:
    values: dict[str, int]
    signal_paths: dict[str, str]


def sample_vbtb_cycle(reader: Callable[[str], int | None]) -> VbtbCycle:
    values, paths = {}, {}
    for key, candidates in VBTB_SIGNAL_GROUPS.items():
        values[key], paths[key] = _read_required(reader, key, candidates, native_only=True)
    return VbtbCycle(values, paths)


def check_vbtb_cycle(sample: VbtbCycle) -> None:
    v = sample.values
    for slot in range(BTB_RESULT_ENTRIES):
        assert 0 <= v[f"sc_way_{slot}"] < SC_STORAGE_WAYS, "SC storage way out of range"
        if not v["s3_fire"]:
            continue
        conditional_taken = (v[f"s3_sc_taken_{slot}"] if v[f"s3_sc_used_{slot}"]
                             else v[f"s3_tage_taken_{slot}"] if v[f"s3_tage_valid_{slot}"]
                             else v[f"s3_base_taken_{slot}"])
        expected_taken = bool(v[f"s3_jump_{slot}"] or
                              (v[f"s3_cond_{slot}"] and conditional_taken))
        assert v[f"taken_{slot}"] == int(expected_taken), "SC/TAGE/base taken priority mismatch"
    for bank in range(MBTB_ALIGN_BANKS):
        a = f"a{bank}_"
        # RegEnable payloads have no reset; a consuming stage fire qualifies them.
        selected = (_one_hot_way(v[a + "selected"], allow_zero=True)
                    if v["s3_fire"] else None)
        if v["s2_fire"]:
            s2_selected = _one_hot_way(v[a + "s2_selected"], allow_zero=True)
            assert v[a + "prediction_valid"] == v[a + "meta_hit"] == int(s2_selected is not None)
            if s2_selected is not None:
                assert v[a + "prediction_position"] == v[a + "meta_position"], "VBTB meta identity mismatch"
        expected_pred = bool(v["s3_fire"] and v[f"taken_{vbtb_slot(bank)}"] and selected is not None)
        assert v[a + "pred_valid"] == int(expected_pred), "VBTB final taken feedback mismatch"
        if expected_pred:
            assert v[a + "pred_way"] == selected, "VBTB predTouch selected way mismatch"
        snapshot = v[a + "snapshot_write"] == 1
        train = v[a + "train_req"] == 1
        assert v[a + "train_valid"] == int(snapshot or train), "VBTB trainTouch valid mismatch"
        if snapshot:
            expected_way = _one_hot_way(v[a + "snapshot_mask"])
        elif train:
            mask = v[a + "entry_mask"]
            if mask:
                expected_way = _one_hot_way(mask)
            else:
                counter_mask = v[a + "counter_mask"]
                assert 0 <= counter_mask < (1 << VBTB_WAYS_PER_ALIGN_BANK)
                # PriorityEncoder(0) = 0 is defined by RTL, not a missing probe.
                expected_way = (counter_mask & -counter_mask).bit_length() - 1 if counter_mask else 0
        else:
            expected_way = None
        if expected_way is not None:
            assert v[a + "train_way"] == expected_way, "VBTB snapshot/training priority mismatch"
        state = v[a + "state"]
        assert 0 <= state < (1 << (VBTB_WAYS_PER_ALIGN_BANK - 1))
        branch_types = [v[a + f"branch_type_{w}"] for w in range(VBTB_WAYS_PER_ALIGN_BANK)]
        assert all(0 <= b < 4 for b in branch_types), "VBTB branch type out of range"
        # MainBtbEntry.valid = !attribute.isNone; BranchType.None encodes 0.
        invalid = sum(int(b == 0) << w for w, b in enumerate(branch_types))
        victim = (invalid & -invalid) if invalid else (1 << _plru_victim(state))
        assert v[a + "victim"] == victim, "VBTB invalid-first/PLRU victim mismatch"


def check_vbtb_transition(before: VbtbCycle, after: VbtbCycle, *, before_cycle: int,
                          after_cycle: int, reset: bool) -> None:
    """Caller supplies consecutive pre-edge snapshots; reset overrides both touches."""
    assert after_cycle == before_cycle + 1, "replacement proof requires consecutive cycles"
    for bank in range(MBTB_ALIGN_BANKS):
        a = f"a{bank}_"
        v = before.values
        state = v[a + "state"]
        for event in ("pred", "train"):
            if v[a + event + "_valid"]:
                way = v[a + event + "_way"]
                assert 0 <= way < VBTB_WAYS_PER_ALIGN_BANK
                state = _plru_touch(state, way)
        assert after.values[a + "state"] == (0 if reset else state), "VBTB replacement feedback mismatch"

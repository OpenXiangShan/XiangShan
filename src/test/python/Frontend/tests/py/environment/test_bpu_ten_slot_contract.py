from dataclasses import replace
from pathlib import Path
import pytest
from env.support.bpu_v3_contract import (
    BTB_RESULT_ENTRIES, BPU_V3_SIGNAL_GROUPS, BpuV3SignalUnavailable,
    SC_STORAGE_WAYS, sample_bpu_v3_cycle,
)
from env.support.bpu_vbtb_contract import (
    VBTB_SIGNAL_GROUPS, _plru_touch, _plru_victim,
    check_vbtb_cycle, check_vbtb_transition, sample_vbtb_cycle, vbtb_slot,
)

def _sample(**patches):
    values = {key: 0 for key in VBTB_SIGNAL_GROUPS}
    values.update(s2_fire=1, s3_fire=1)
    for bank in range(2):
        for way in range(16):
            values[f"a{bank}_branch_type_{way}"] = 1
        values[f"a{bank}_victim"] = 1
    values.update(patches)
    signals = {VBTB_SIGNAL_GROUPS[k][0]: v for k, v in values.items()}
    return sample_vbtb_cycle(signals.get)


@pytest.mark.parametrize("missing", ["taken_9", "sc_used_9", "a1_meta_hit", "a1_pred_way", "a0_state"])
def test_vbtb_missing_probe_fails_closed(missing):
    signals = {p[0]: 0 for k, p in VBTB_SIGNAL_GROUPS.items() if k != missing}
    with pytest.raises(BpuV3SignalUnavailable) as error:
        sample_vbtb_cycle(signals.get)
    assert error.value.key == missing


def test_vbtb_feedback_uses_physical_bank_last_slot_and_selected_way():
    assert (vbtb_slot(0), vbtb_slot(1)) == (4, 9)
    valid = _sample(s3_fire=1, taken_9=1, s3_jump_9=1, a1_selected=1 << 13,
                    a1_pred_valid=1, a1_pred_way=13)
    check_vbtb_cycle(valid)
    wrong = replace(valid, values={**valid.values, "taken_9": 0, "taken_8": 1})
    with pytest.raises(AssertionError, match="taken"):
        check_vbtb_cycle(wrong)


def test_vbtb_meta_must_follow_same_selected_s2_prediction():
    check_vbtb_cycle(_sample(a0_s2_selected=8, a0_prediction_valid=1,
                            a0_meta_hit=1, a0_prediction_position=11, a0_meta_position=11))
    with pytest.raises(AssertionError, match="meta identity"):
        check_vbtb_cycle(_sample(a0_s2_selected=8, a0_prediction_valid=1,
                                a0_meta_hit=1, a0_prediction_position=11, a0_meta_position=12))


def test_vbtb_snapshot_wins_train_touch_and_counter_priority_is_lowest_way():
    check_vbtb_cycle(_sample(a0_snapshot_write=1, a0_snapshot_mask=1 << 15,
                            a0_train_req=1, a0_entry_mask=2,
                            a0_train_valid=1, a0_train_way=15))
    check_vbtb_cycle(_sample(a0_train_req=1, a0_counter_mask=(1 << 12) | 8,
                            a0_train_valid=1, a0_train_way=3))
    with pytest.raises(AssertionError, match="priority"):
        check_vbtb_cycle(_sample(a0_snapshot_write=1, a0_snapshot_mask=1 << 15,
                                a0_train_req=1, a0_entry_mask=2,
                                a0_train_valid=1, a0_train_way=1))


def test_sc_ten_logical_results_keep_eight_storage_ways():
    check_vbtb_cycle(_sample(sc_way_9=7))
    with pytest.raises(AssertionError, match="SC storage way"):
        check_vbtb_cycle(_sample(sc_way_9=8))


def test_replacement_invalid_way_precedes_tree_victim():
    check_vbtb_cycle(_sample(a0_branch_type_12=0, a0_branch_type_7=0, a0_victim=1 << 7))
    with pytest.raises(AssertionError, match="invalid-first"):
        check_vbtb_cycle(_sample(a0_branch_type_12=0, a0_victim=1))


def test_plru_reference_known_four_way_vectors_and_full_sixteen_way_walk():
    assert _plru_touch(0, 0, 4) == 0b101
    assert _plru_touch(0b101, 3, 4) == 0b001
    assert _plru_victim(0b001, 4) == 1
    state = 0
    for way in range(16):
        state = _plru_touch(state, way)
        assert _plru_victim(state) != way
    assert state == 0  # ascending walk leaves way 0 least recent


def test_replacement_transition_applies_prediction_then_training_and_reset():
    before = _sample(s3_fire=1, taken_4=1, s3_jump_4=1, a0_selected=1,
                     a0_pred_valid=1, a0_pred_way=0,
                     a0_train_req=1, a0_entry_mask=1 << 8,
                     a0_train_valid=1, a0_train_way=8)
    check_vbtb_cycle(before)
    state = _plru_touch(_plru_touch(0, 0), 8)
    after = _sample(a0_state=state, a0_victim=1 << _plru_victim(state))
    check_vbtb_transition(before, after, before_cycle=5, after_cycle=6, reset=False)
    reversed_state = _plru_touch(_plru_touch(0, 8), 0)
    with pytest.raises(AssertionError, match="feedback"):
        check_vbtb_transition(before, _sample(a0_state=reversed_state),
                              before_cycle=5, after_cycle=6, reset=False)
    check_vbtb_transition(before, _sample(), before_cycle=5, after_cycle=6, reset=True)
    with pytest.raises(AssertionError, match="consecutive"):
        check_vbtb_transition(before, after, before_cycle=5, after_cycle=7, reset=False)


def test_vbtb_slot_sc_overrides_tage_and_base_and_uses_s3_not_s2():
    check_vbtb_cycle(_sample(s3_cond_9=1, s3_sc_used_9=1, s3_sc_taken_9=0,
                            s3_tage_valid_9=1, s3_tage_taken_9=1, s3_base_taken_9=1,
                            sc_taken_9=1))
    check_vbtb_cycle(_sample(s3_cond_4=1, s3_tage_valid_4=1, s3_tage_taken_4=1,
                            taken_4=1))
    with pytest.raises(AssertionError, match="taken priority"):
        check_vbtb_cycle(_sample(s3_cond_9=1, s3_sc_used_9=1, s3_sc_taken_9=0,
                                s3_tage_valid_9=1, s3_tage_taken_9=1, taken_9=1))


def test_touch_and_replacement_priority_match_source_rtl():
    root = Path(__file__).resolve().parents[7] / "src/main/scala/xiangshan/frontend/bpu/mbtb"
    vbtb = (root / "VictimBtb.scala").read_text()
    repl = (root / "VictimBtbReplacer.scala").read_text()
    assert "io.stageCtrl.s3_fire && io.s3_vbtbTaken && s3_selectOH.orR" in vbtb
    assert "snapshotWriteEvicted || trainEntry.req.valid" in vbtb
    assert "Mux(snapshotWriteEvicted, OHToUInt(snapshotEntryWayMask), trainEntryTouchWay)" in vbtb
    assert "replacer.access(Seq(io.predTouch, io.trainTouch))" in repl
    assert "PriorityEncoderOH(invalidMask)" in repl
    assert "UIntToOH(replacer.way)" in repl



@pytest.mark.parametrize("slot", [4, 8, 9])
def test_all_ten_results_are_required_and_tail_taken_is_not_lost(slot):
    values = {p[0]: 0 for p in BPU_V3_SIGNAL_GROUPS.values()}
    values[BPU_V3_SIGNAL_GROUPS["s3_valid"][0]] = 1
    values[BPU_V3_SIGNAL_GROUPS[f"s3_taken_mask_{slot}"][0]] = 1
    sample = sample_bpu_v3_cycle(values.get, require_native=True)
    assert len(sample.s3_taken_mask) == BTB_RESULT_ENTRIES == 10
    assert not sample.is_all_not_taken_candidate


@pytest.mark.parametrize("slot", [8, 9])
def test_new_strict_mode_rejects_missing_tail_even_with_toggle_alias(slot):
    values = {p[0]: 0 for p in BPU_V3_SIGNAL_GROUPS.values()}
    del values[BPU_V3_SIGNAL_GROUPS[f"s3_taken_mask_{slot}"][0]]
    values[BPU_V3_SIGNAL_GROUPS[f"s3_taken_mask_{slot}"][-1]] = 0
    with pytest.raises(BpuV3SignalUnavailable) as error:
        sample_bpu_v3_cycle(values.get, require_native=True)
    assert error.value.key == f"s3_taken_mask_{slot}"


def test_payload_checks_are_stage_qualified_but_feedback_remains_checked():
    check_vbtb_cycle(_sample(s2_fire=0, s3_fire=0,
                            a0_selected=0x29ff, a0_s2_selected=0xd856))
    with pytest.raises(AssertionError, match="one-hot"):
        check_vbtb_cycle(_sample(a0_selected=0x29ff))
    with pytest.raises(AssertionError, match="one-hot"):
        check_vbtb_cycle(_sample(s3_fire=0, a0_s2_selected=0xd856))
    with pytest.raises(AssertionError, match="final taken"):
        check_vbtb_cycle(_sample(s3_fire=0, a0_pred_valid=1))


def test_ten_result_dimensions_and_physical_vbtb_slots_follow_frozen_source():
    root = Path(__file__).resolve().parents[7] / "src/main/scala/xiangshan/frontend/bpu"
    read = lambda name: (root / name).read_text()
    assert "(bpuParameters.mbtbParameters.NumWay + 1) * NumBtbAlignBanks" in read("Parameters.scala")
    assert "VecInit(s2_predictions :+ s2_vbtbPrediction)" in read("mbtb/MainBtbAlignBank.scala")
    assert "i * (NumWay + 1) + NumWay" in read("mbtb/MainBtb.scala")
    assert "NumWays:             Int = 8" in read("sc/Parameters.scala")
    assert "for (i <- 0 until NumBtbResultEntries)" in read("sc/Sc.scala")
    assert (vbtb_slot(0), vbtb_slot(1)) == (4, 9)


def test_native_single_bit_result_cannot_accept_wrong_width_value():
    values = {paths[0]: 0 for paths in BPU_V3_SIGNAL_GROUPS.values()}
    values[BPU_V3_SIGNAL_GROUPS["s3_taken_mask_9"][0]] = 2
    with pytest.raises(AssertionError, match="single-bit contract"):
        sample_bpu_v3_cycle(values.get, require_native=True)


def test_inventory_resolver_only_accepts_exact_native_reduction_projection():
    from tools.audit_bpu_ten_slot_contract import resolve_native
    records = {
        "state": {"offset": "20", "rtl_width": "16"},
        "meta": {"kind": "projection", "source": "state", "rtl_width": "1",
                 "expr": "0U != (IData)(state)"},
        "__Vtogcov__state": {"offset": "40", "rtl_width": "16"},
    }
    assert resolve_native(records, ("meta",), 1)["projection"] == "nonzero"
    assert "missing_native_probe" in resolve_native(records, ("meta",), 16)
    assert "missing_native_probe" in resolve_native(records, ("__Vtogcov__state",), 16)
    for mutation in ({"expr": "__VdtypeVar[0]"}, {"source": "__Vtogcov__state"},
                     {"rtl_width": "16"}, {"kind": "expr"}):
        bad = {**records, "meta": {**records["meta"], **mutation}}
        assert "missing_native_probe" in resolve_native(bad, ("meta",), 1)
    missing = {k: v for k, v in records.items() if k != "state"}
    assert "missing_native_probe" in resolve_native(missing, ("meta",), 1)
    records["one"] = {"offset": "1", "rtl_width": "1"}
    records["arbitrary"] = {"kind": "expr", "source": "one", "rtl_width": "1", "expr": "one ^ 1"}
    records["wrong_width"] = {"kind": "direct", "source": "one", "rtl_width": "16"}
    assert "missing_native_probe" in resolve_native(records, ("arbitrary",), 1)
    assert "missing_native_probe" in resolve_native(records, ("wrong_width",), 1)


def test_directed_all_not_taken_consumer_explicitly_requires_native_contract():
    import ast
    path = Path(__file__).resolve().parents[1] / "jiabowen/test_bpu_v3_negative_dut.py"
    calls = [n for n in ast.walk(ast.parse(path.read_text()))
             if isinstance(n, ast.Call) and isinstance(n.func, ast.Name)
             and n.func.id == "sample_bpu_v3_cycle"]
    assert calls
    assert all(any(k.arg == "require_native" and isinstance(k.value, ast.Constant)
                   and k.value.value is True for k in n.keywords) for n in calls)

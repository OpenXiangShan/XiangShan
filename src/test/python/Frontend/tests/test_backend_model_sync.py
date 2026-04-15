from __future__ import annotations

from env.backend_model import BackendModel, FtqEntry, ResolveEntry
from env.model.backend_state import CommitInstruction, QueueInstr
from env.model.backend_runtime import BackendObservationSnapshot
from env.monitor import Observation
from env.trace import GoldenTrace, TraceEntry


class _Pin:
    def __init__(self, value=0):
        self.value = int(value)


class _DummyDut:
    def __init__(self):
        self._pins = {}

    def __getattr__(self, name):
        if name.startswith("_"):
            raise AttributeError(name)
        pin = self._pins.get(name)
        if pin is None:
            pin = _Pin(0)
            self._pins[name] = pin
        return pin


def _drive_cfvec_slot(
    dut,
    slot: int,
    *,
    valid: int,
    pc: int = 0,
    instr: int = 0,
    is_rvc: int = 0,
    pred_taken: int = 0,
    ftq_flag: int = 0,
    ftq_value: int = 0,
    ftq_offset: int = 0,
    is_last: int = 0,
) -> None:
    base = f"io_backend_cfVec_{int(slot)}_bits_"
    getattr(dut, f"io_backend_cfVec_{int(slot)}_valid").value = int(valid)
    getattr(dut, base + "pc").value = int(pc)
    getattr(dut, base + "instr").value = int(instr)
    getattr(dut, base + "isRvc").value = int(is_rvc)
    getattr(dut, base + "predTaken").value = int(pred_taken)
    getattr(dut, base + "ftqPtr_flag").value = int(ftq_flag)
    getattr(dut, base + "ftqPtr_value").value = int(ftq_value)
    getattr(dut, base + "ftqOffset").value = int(ftq_offset)
    getattr(dut, base + "isLastInFtqEntry").value = int(is_last)


def _clear_cfvec(dut) -> None:
    for slot in range(8):
        getattr(dut, f"io_backend_cfVec_{slot}_valid").value = 0


def test_indirect_redirect_is_dropped_after_current_golden_cfi_successor_is_observed():
    wrong_target = 0x80004852
    golden_target = 0x80004A9A

    dut = _DummyDut()
    bm = BackendModel(resolve_min_delay=0, resolve_max_delay=0)
    bm.bind(dut)
    trace = GoldenTrace(
        [
            TraceEntry(index=0, pc=0x80003AA0, instr=0x00008082, size=2, kind="jump_indirect", taken=True, target_pc=golden_target),
            TraceEntry(index=1, pc=golden_target, instr=0x00007782, size=2, kind="normal", taken=False, target_pc=None),
        ]
    )
    trace.cursor = 0
    bm.set_golden_trace(trace)

    class _Monitor:
        def __init__(self):
            self.observations = [
                Observation(cycle=921, slot=0, pc=0x80003AA0, instr=0x00008082, is_rvc=True, pred_taken=False),
                Observation(cycle=922, slot=0, pc=golden_target, instr=0x00007782, is_rvc=True, pred_taken=False),
            ]

        def notify_redirect(self, target_pc, reason="redirect"):
            return None

    bm.attach_monitor(_Monitor())
    bm.current_cycle = 925
    bm._pending_resolves.append(
        ResolveEntry(
            ready_cycle=925,
            inst_pc=0x80003AA0,
            pc=0x80003A84,
            target=wrong_target,
            taken=True,
            mispredict=True,
            ftq_flag=0,
            ftq_value=33,
            ftq_offset=14,
            branch_type=3,
            ras_action=1,
            queued_cycle=920,
            is_rvc=True,
        )
    )

    bm._drive_resolves()

    assert int(dut.io_backend_toFtq_resolve_0_valid.value) == 1
    assert int(dut.io_backend_toFtq_resolve_0_bits_target_addr.value) == (wrong_target >> 1)
    assert len(bm.pending_events) == 0
    assert len(bm._pending_resolves) == 0


def test_inject_redirect_uses_random_delay_range_from_backend_config():
    bm = BackendModel(redirect_min_delay=5, redirect_max_delay=8)
    bm.current_cycle = 100

    bm.inject_redirect(target_pc=0x80000080, reason="unit-test")

    assert len(bm.pending_events) == 1
    event = bm.pending_events[0]
    assert event.kind == "redirect"
    assert event.payload["target_pc"] == 0x80000080
    assert 105 <= int(event.ready_cycle) <= 108


def test_inject_redirect_clears_semantic_queue_state() -> None:
    bm = BackendModel()
    bm._semantic_queue.extend(
        [
            QueueInstr(cycle=0, slot=0, pc=0x80000000, instr=0x13, is_rvc=False, pred_taken=False, ftq_flag=0, ftq_value=1, ftq_offset=0, is_last_in_entry=False),
            QueueInstr(cycle=0, slot=1, pc=0x80000004, instr=0x6F, is_rvc=False, pred_taken=True, ftq_flag=0, ftq_value=1, ftq_offset=2, is_last_in_entry=True, is_cfi=True),
        ]
    )
    bm._pending_redirect_origin_index = 1
    bm._pending_queue_resolve_indices.extend([1])
    bm._pending_queue_call_ret_commit_indices.extend([0, 1])
    bm._scheduled_queue_call_ret_commit_groups.append((10, []))
    bm._visible_queue_call_ret_commit_group = [CommitInstruction(0, 0x80000000, 0x13, 0, 1, 0, queue_index=0)]
    bm._pending_resolves.append(
        ResolveEntry(
            ready_cycle=3,
            inst_pc=0x80000004,
            pc=0x80000000,
            target=0x80000010,
            taken=True,
            mispredict=True,
            ftq_flag=0,
            ftq_value=1,
            ftq_offset=2,
            branch_type=2,
            ras_action=0,
            queue_index=1,
        )
    )

    bm.inject_redirect(target_pc=0x80000100, reason="manual")

    assert list(bm._semantic_queue) == []
    assert bm._pending_redirect_origin_index is None
    assert list(bm._pending_queue_resolve_indices) == []
    assert list(bm._pending_queue_call_ret_commit_indices) == []
    assert list(bm._scheduled_queue_call_ret_commit_groups) == []
    assert bm._visible_queue_call_ret_commit_group == []
    assert list(bm._pending_resolves) == []


def test_last_packet_sets_commit_ready_cycle_in_commit_delay_range():
    dut = _DummyDut()
    bm = BackendModel(commit_min_delay=3, commit_max_delay=10)
    bm.bind(dut)
    bm.current_cycle = 100

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000000,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=1,
        ftq_offset=0,
        is_last=1,
    )

    bm._sample_cfvec()

    assert len(bm.ftq_entries) == 1
    assert 103 <= int(bm.ftq_entries[0].commit_ready_cycle) <= 110


def test_semantic_queue_scaffolding_mirrors_unique_cfvec_packets_in_order() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000000,
        instr=0x00000013,
        is_rvc=0,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=1,
        ftq_offset=0,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        1,
        valid=1,
        pc=0x80000004,
        instr=0x0000006F,
        is_rvc=0,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=1,
        ftq_offset=2,
        is_last=1,
    )

    bm.current_cycle = 10
    bm._sample_cfvec()

    semantic_queue = list(bm._semantic_queue)
    assert len(semantic_queue) == 2
    assert [(entry.slot, entry.pc, entry.ftq_offset) for entry in semantic_queue] == [
        (0, 0x80000000, 0),
        (1, 0x80000004, 2),
    ]
    assert semantic_queue[0].is_cfi is False
    assert semantic_queue[0].resolve_state == "not_needed"
    assert semantic_queue[1].is_cfi is True
    assert semantic_queue[1].resolve_state == "pending"


def test_semantic_queue_scaffolding_deduplicates_repeated_packet_within_same_ftq_entry() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000000,
        instr=0x00000013,
        is_rvc=0,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=3,
        ftq_offset=0,
        is_last=0,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()
    bm.current_cycle = 1
    bm._sample_cfvec()

    semantic_queue = list(bm._semantic_queue)
    assert len(semantic_queue) == 1
    assert semantic_queue[0].pc == 0x80000000
    assert semantic_queue[0].ftq_value == 3


def test_semantic_queue_marks_first_mismatch_as_wrong_path_start() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000000, instr=0x00000013, size=4),
                TraceEntry(index=1, pc=0x80000004, instr=0x00000013, size=4),
            ]
        )
        )

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000000,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=1,
        ftq_offset=0,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        1,
        valid=1,
        pc=0x80000010,
        instr=0x0000006F,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=1,
        ftq_offset=2,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    semantic_queue = list(bm._semantic_queue)
    assert len(semantic_queue) == 2
    assert semantic_queue[0].golden_match_state == "matched"
    assert semantic_queue[0].path_state == "correct"
    assert semantic_queue[1].golden_match_state == "mismatched"
    assert semantic_queue[1].path_state == "wrong"
    assert bm._pending_redirect_origin_index == 1


def test_semantic_queue_marks_younger_packets_wrong_after_redirect_origin() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000000, instr=0x00000013, size=4),
                TraceEntry(index=1, pc=0x80000004, instr=0x00000013, size=4),
            ]
        )
    )

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000000,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=2,
        ftq_offset=0,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        1,
        valid=1,
        pc=0x80000010,
        instr=0x0000006F,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=2,
        ftq_offset=2,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        2,
        valid=1,
        pc=0x80000020,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=2,
        ftq_offset=4,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    semantic_queue = list(bm._semantic_queue)
    assert [entry.path_state for entry in semantic_queue] == ["correct", "wrong", "wrong"]
    assert bm._pending_redirect_origin_index == 1


def test_semantic_queue_non_cfi_first_mismatch_starts_wrong_path() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000000, instr=0x00000063, size=4, kind="branch", taken=False, target_pc=0x80000004),
                TraceEntry(index=1, pc=0x80000004, instr=0x00000013, size=4),
            ]
        )
    )

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000000,
        instr=0x00000063,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=3,
        ftq_offset=0,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        1,
        valid=1,
        pc=0x80000010,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=3,
        ftq_offset=2,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        2,
        valid=1,
        pc=0x80000020,
        instr=0x0000006F,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=3,
        ftq_offset=4,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    semantic_queue = list(bm._semantic_queue)
    assert [entry.path_state for entry in semantic_queue] == ["correct", "wrong", "wrong"]
    assert [entry.golden_match_state for entry in semantic_queue] == ["matched", "mismatched", "mismatched"]
    assert bm._pending_redirect_origin_index == 1
    assert len(bm.pending_events) == 0
    assert len(bm._pending_resolves) == 2
    assert bm._pending_resolves[0].queue_index == 0
    assert bm._pending_resolves[0].mispredict is True
    assert bm._pending_resolves[0].target == 0x80000004


def test_semantic_queue_redirect_flush_prunes_wrong_path_suffix() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000000, instr=0x00000013, size=4),
                TraceEntry(index=1, pc=0x80000004, instr=0x00000013, size=4),
            ]
        )
    )

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000000,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=4,
        ftq_offset=0,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        1,
        valid=1,
        pc=0x80000010,
        instr=0x0000006F,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=4,
        ftq_offset=2,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        2,
        valid=1,
        pc=0x80000020,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=4,
        ftq_offset=4,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()
    assert len(bm._semantic_queue) == 3
    assert bm._pending_redirect_origin_index == 1

    payload = bm._plan_redirect_payload(
        {
            "target_pc": 0x80000004,
            "reason": "golden_resolve_redirect",
            "flush_on_drive": True,
            "pc": 0x80000010,
            "taken": 1,
            "ftq_flag": 0,
            "ftq_value": 4,
            "ftq_offset": 2,
            "branch_type": 2,
            "ras_action": 0,
            "level": 0,
        }
    )

    assert payload["target_pc"] == 0x80000004
    assert [(entry.pc, entry.path_state) for entry in bm._semantic_queue] == [(0x80000000, "correct")]
    assert bm._pending_redirect_origin_index is None


def test_semantic_queue_redirect_flush_prunes_wrong_path_pending_resolves() -> None:
    dut = _DummyDut()
    bm = BackendModel(resolve_min_delay=100, resolve_max_delay=100)
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000000, instr=0x00000013, size=4),
                TraceEntry(index=1, pc=0x80000004, instr=0x00000013, size=4),
            ]
        )
    )

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000000,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=16,
        ftq_offset=0,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        1,
        valid=1,
        pc=0x80000010,
        instr=0x0000006F,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=16,
        ftq_offset=2,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        2,
        valid=1,
        pc=0x80000020,
        instr=0x0000006F,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=16,
        ftq_offset=4,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    assert len(bm._pending_resolves) == 2

    bm._plan_redirect_payload(
        {
            "target_pc": 0x80000004,
            "reason": "golden_resolve_redirect",
            "flush_on_drive": True,
            "pc": 0x80000010,
            "taken": 1,
            "ftq_flag": 0,
            "ftq_value": 16,
            "ftq_offset": 2,
            "branch_type": 2,
            "ras_action": 0,
            "level": 0,
        }
    )

    assert len(bm._pending_resolves) == 0


def test_semantic_queue_transition_without_last_marks_surviving_prefix_closed() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000000,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=20,
        ftq_offset=0,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        1,
        valid=1,
        pc=0x80000004,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=20,
        ftq_offset=2,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        2,
        valid=1,
        pc=0x80000020,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=21,
        ftq_offset=0,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    semantic_queue = list(bm._semantic_queue)
    assert [(entry.ftq_value, entry.is_last_in_entry) for entry in semantic_queue] == [
        (20, False),
        (20, True),
        (21, True),
    ]


def test_semantic_queue_wait_break_marks_current_entry_closed() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000000, instr=0x0000006F, size=4, kind="jump", taken=True, target_pc=0x80000020),
                TraceEntry(index=1, pc=0x80000020, instr=0x00000013, size=4),
            ]
        )
    )

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000000,
        instr=0x0000006F,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=22,
        ftq_offset=0,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        1,
        valid=1,
        pc=0x80000004,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=22,
        ftq_offset=2,
        is_last=0,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    semantic_queue = list(bm._semantic_queue)
    assert len(semantic_queue) == 1
    assert semantic_queue[0].is_last_in_entry is True


def test_waiting_target_prefix_last_seals_current_ftq_entry() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000000, instr=0x00000013, size=4),
                TraceEntry(index=1, pc=0x80000020, instr=0x00000013, size=4),
            ]
        )
    )
    bm._golden_wait_pc = 0x80000020
    bm._pending_level0_target_ftq = (0, 30)

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000010,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=30,
        ftq_offset=0,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    assert bm._current_ftq_entry is None
    assert [(entry.ftq_flag, entry.ftq_value, entry.dispatch_complete, entry.observed_last_in_entry) for entry in bm.ftq_entries] == [
        (0, 30, True, True)
    ]
    assert list(bm._semantic_queue)[0].is_last_in_entry is True


def test_waiting_target_prefix_cfi_is_marked_resolve_skipped() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(GoldenTrace([TraceEntry(index=0, pc=0x80000020, instr=0x00000013, size=4)]))
    bm._golden_wait_pc = 0x80000020
    bm._pending_level0_target_ftq = (0, 31)

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000010,
        instr=0x0000006F,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=31,
        ftq_offset=0,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    semantic_queue = list(bm._semantic_queue)
    assert len(semantic_queue) == 1
    assert semantic_queue[0].is_cfi is True
    assert semantic_queue[0].path_state == "wrong"
    assert semantic_queue[0].resolve_state == "skipped"
    assert list(bm._pending_queue_resolve_indices) == []


def test_semantic_queue_commit_drops_stale_older_ftq_entries() -> None:
    bm = BackendModel()
    bm.set_golden_trace(GoldenTrace([]))
    bm._semantic_queue.extend(
        [
            QueueInstr(
                cycle=0,
                slot=0,
                pc=0x80000020,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=9,
                ftq_offset=0,
                is_last_in_entry=False,
                path_state="correct",
                rob_commit_state="committed",
                golden_match_state="matched",
            ),
            QueueInstr(
                cycle=0,
                slot=1,
                pc=0x80000024,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=9,
                ftq_offset=2,
                is_last_in_entry=True,
                path_state="correct",
                rob_commit_state="committed",
                golden_match_state="matched",
            ),
        ]
    )
    bm.ftq_entries.extend(
        [
            FtqEntry(ftq_flag=0, ftq_value=8, dispatch_complete=True, observed_last_in_entry=True, commit_ready_cycle=0),
            FtqEntry(ftq_flag=0, ftq_value=9, dispatch_complete=True, observed_last_in_entry=True, commit_ready_cycle=0),
        ]
    )
    bm.current_cycle = 1

    committed = bm._plan_commit_entry_for_cycle()

    assert committed is not None
    assert (committed.ftq_flag, committed.ftq_value) == (0, 9)
    assert len(bm._semantic_queue) == 0
    assert list(bm.ftq_entries) == []
    assert (bm.commit_ptr_flag, bm.commit_ptr_value) == (0, 9)


def test_semantic_queue_commit_uses_queue_head_when_ftq_entries_are_out_of_order() -> None:
    bm = BackendModel()
    bm.set_golden_trace(GoldenTrace([]))
    bm._semantic_queue.extend(
        [
            QueueInstr(
                cycle=0,
                slot=0,
                pc=0x80000020,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=12,
                ftq_offset=0,
                is_last_in_entry=False,
                path_state="correct",
                rob_commit_state="committed",
                golden_match_state="matched",
            ),
            QueueInstr(
                cycle=0,
                slot=1,
                pc=0x80000024,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=12,
                ftq_offset=2,
                is_last_in_entry=True,
                path_state="correct",
                rob_commit_state="committed",
                golden_match_state="matched",
            ),
        ]
    )
    bm.ftq_entries.extend(
        [
            FtqEntry(ftq_flag=0, ftq_value=13, dispatch_complete=False, observed_last_in_entry=False, commit_ready_cycle=0),
            FtqEntry(ftq_flag=0, ftq_value=12, dispatch_complete=False, observed_last_in_entry=False, commit_ready_cycle=0),
            FtqEntry(ftq_flag=0, ftq_value=14, dispatch_complete=True, observed_last_in_entry=True, commit_ready_cycle=0),
        ]
    )
    bm.current_cycle = 1

    committed = bm._plan_commit_entry_for_cycle()

    assert committed is not None
    assert (committed.ftq_flag, committed.ftq_value) == (0, 12)
    assert [(entry.ftq_flag, entry.ftq_value) for entry in bm.ftq_entries] == [(0, 14)]
    assert (bm.commit_ptr_flag, bm.commit_ptr_value) == (0, 12)


def test_semantic_queue_correct_path_cfi_emits_resolve_and_marks_emitted() -> None:
    dut = _DummyDut()
    bm = BackendModel(resolve_min_delay=0, resolve_max_delay=0)
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000000, instr=0x0000006F, size=4, kind="jump", taken=True, target_pc=0x80000010),
                TraceEntry(index=1, pc=0x80000010, instr=0x00000013, size=4),
            ]
        )
    )
    bm.resolve_min_delay = 0
    bm.resolve_max_delay = 0

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000000,
        instr=0x0000006F,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=5,
        ftq_offset=0,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()
    bm._drive_resolves()

    assert int(dut.io_backend_toFtq_resolve_0_valid.value) == 1
    assert bm._semantic_queue[0].resolve_state == "emitted"
    assert list(bm._pending_queue_resolve_indices) == []


def test_semantic_queue_wrong_path_cfi_resolve_is_skipped() -> None:
    dut = _DummyDut()
    bm = BackendModel(resolve_min_delay=0, resolve_max_delay=0)
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000000, instr=0x00000013, size=4),
                TraceEntry(index=1, pc=0x80000004, instr=0x00000013, size=4),
            ]
        )
    )
    bm.resolve_min_delay = 0
    bm.resolve_max_delay = 0

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000000,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=6,
        ftq_offset=0,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        1,
        valid=1,
        pc=0x80000010,
        instr=0x0000006F,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=6,
        ftq_offset=2,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        2,
        valid=1,
        pc=0x80000020,
        instr=0x0000006F,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=6,
        ftq_offset=4,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()
    bm._drive_resolves()

    assert int(dut.io_backend_toFtq_resolve_0_valid.value) == 0
    assert bm._semantic_queue[1].resolve_state == "skipped"
    assert bm._semantic_queue[2].resolve_state == "skipped"
    assert len(bm._pending_resolves) == 0
    assert list(bm._pending_queue_resolve_indices) == []


def test_resolve_extends_commit_ready_cycle_in_commit_delay_range():
    bm = BackendModel(commit_min_delay=3, commit_max_delay=10)
    bm.current_cycle = 200
    entry = FtqEntry(ftq_flag=0, ftq_value=7, total_cfi=1, resolved_cfi=0, dispatch_complete=True, commit_ready_cycle=0)
    bm.ftq_entries.append(entry)

    bm._sync_backend_state()
    bm._ftq_scoreboard.note_resolve(
        ResolveEntry(
            ready_cycle=200,
            inst_pc=0x80000000,
            pc=0x80000000,
            target=0x80000010,
            taken=True,
            mispredict=False,
            ftq_flag=0,
            ftq_value=7,
            ftq_offset=0,
            branch_type=0,
            ras_action=0,
        ),
        current_cycle=200,
        entry_flushes_itself=False,
    )
    bm._apply_backend_state()

    assert 203 <= int(entry.commit_ready_cycle) <= 210


def test_commit_does_not_rewind_into_duplicate_same_ftq_entry_after_commit():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.commit_count = 260
    bm.commit_ptr_flag = 1
    bm.commit_ptr_value = 49
    bm.current_cycle = 1237
    bm.ftq_entries.extend(
        [
            FtqEntry(ftq_flag=1, ftq_value=50, total_cfi=3, resolved_cfi=3, dispatch_complete=True, commit_ready_cycle=1236),
            FtqEntry(ftq_flag=1, ftq_value=50, total_cfi=3, resolved_cfi=3, dispatch_complete=True, commit_ready_cycle=1238),
        ]
    )

    bm._drive_commit()

    assert int(dut.io_backend_toFtq_commit_valid.value) == 1
    assert int(dut.io_backend_toFtq_commit_bits_flag.value) == 1
    assert int(dut.io_backend_toFtq_commit_bits_value.value) == 50
    assert bm.commit_ptr_flag == 1
    assert bm.commit_ptr_value == 50

    dut.io_backend_toFtq_commit_valid.value = 0
    bm.current_cycle = 1238
    bm._drive_commit()

    assert int(dut.io_backend_toFtq_commit_valid.value) == 0
    assert bm.commit_ptr_flag == 1
    assert bm.commit_ptr_value == 50


def test_commit_removes_stale_duplicate_entries_with_same_ftq_ptr():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.commit_count = 260
    bm.commit_ptr_flag = 1
    bm.commit_ptr_value = 49
    bm.current_cycle = 1237
    bm.ftq_entries.extend(
        [
            FtqEntry(ftq_flag=1, ftq_value=50, total_cfi=3, resolved_cfi=3, dispatch_complete=True, commit_ready_cycle=1236),
            FtqEntry(ftq_flag=1, ftq_value=50, total_cfi=3, resolved_cfi=3, dispatch_complete=True, commit_ready_cycle=1238),
        ]
    )

    bm._drive_commit()

    assert len(bm.ftq_entries) == 0
    assert bm.commit_ptr_flag == 1
    assert bm.commit_ptr_value == 50


def test_sealing_same_ftq_entry_twice_merges_instead_of_appending_duplicate():
    bm = BackendModel()
    bm._current_ftq_entry = FtqEntry(
        ftq_flag=1,
        ftq_value=50,
        total_cfi=3,
        resolved_cfi=0,
        dispatch_complete=False,
        has_redirect=False,
        commit_ready_cycle=1236,
    )
    bm.ftq_entries.append(
        FtqEntry(
            ftq_flag=1,
            ftq_value=50,
            total_cfi=3,
            resolved_cfi=3,
            dispatch_complete=True,
            has_redirect=False,
            commit_ready_cycle=1238,
        )
    )
    bm.current_cycle = 1237

    bm._seal_current_ftq_entry()

    assert len(bm.ftq_entries) == 1
    entry = bm.ftq_entries[0]
    assert (entry.ftq_flag, entry.ftq_value) == (1, 50)
    assert entry.dispatch_complete is True
    assert entry.commit_ready_cycle >= 1238


def test_level0_redirect_does_not_wait_for_next_target_slot_that_is_already_committed():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.commit_count = 247
    bm.commit_ptr_flag = 1
    bm.commit_ptr_value = 51

    bm._drive_redirect(
        {
            "target_pc": 0x80004A28,
            "reason": "golden_resolve_redirect",
            "flush_on_drive": True,
            "pc": 0x800049C2,
            "taken": 1,
            "ftq_flag": 1,
            "ftq_value": 50,
            "ftq_offset": 16,
            "branch_type": 1,
            "ras_action": 0,
            "level": 0,
        }
    )

    assert bm._pending_level0_target_ftq is None


def test_level0_redirect_does_not_wait_for_committed_next_target_slot_after_commit_ptr_rewinds():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.commit_count = 247
    bm.commit_ptr_flag = 1
    bm.commit_ptr_value = 51
    bm.ftq_entries.append(
        FtqEntry(ftq_flag=1, ftq_value=50, total_cfi=3, resolved_cfi=3, dispatch_complete=True, commit_ready_cycle=1243)
    )

    bm._drive_redirect(
        {
            "target_pc": 0x80004A28,
            "reason": "golden_resolve_redirect",
            "flush_on_drive": True,
            "pc": 0x800049C2,
            "taken": 1,
            "ftq_flag": 1,
            "ftq_value": 50,
            "ftq_offset": 16,
            "branch_type": 1,
            "ras_action": 0,
            "level": 0,
        }
    )

    assert (bm.commit_ptr_flag, bm.commit_ptr_value) == (1, 49)
    assert bm._pending_level0_target_ftq is None


def test_flush_after_redirect_keeps_partially_observed_next_target_current_entry():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.commit_count = 247
    bm.commit_ptr_flag = 1
    bm.commit_ptr_value = 49
    bm.ftq_entries.append(
        FtqEntry(ftq_flag=1, ftq_value=50, total_cfi=3, resolved_cfi=3, dispatch_complete=True, commit_ready_cycle=1243)
    )
    bm._current_ftq_entry = FtqEntry(ftq_flag=1, ftq_value=51)
    bm._ftq_group_pc_history[(1, 51)] = [
        (0x80004A00, False),
        (0x80004A02, False),
        (0x80004A04, False),
        (0x80004A06, False),
    ]

    bm._drive_redirect(
        {
            "target_pc": 0x80004A28,
            "reason": "golden_resolve_redirect",
            "flush_on_drive": True,
            "pc": 0x800049C2,
            "taken": 1,
            "ftq_flag": 1,
            "ftq_value": 50,
            "ftq_offset": 16,
            "branch_type": 1,
            "ras_action": 0,
            "level": 0,
        }
    )

    assert bm._current_ftq_entry is not None
    assert (bm._current_ftq_entry.ftq_flag, bm._current_ftq_entry.ftq_value) == (1, 51)


def test_golden_wait_pc_does_not_drop_partially_observed_target_entry_prefix():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(GoldenTrace([TraceEntry(index=0, pc=0x80004A28, instr=0x000C3783, size=4)]))
    bm._golden_wait_pc = 0x80004A28

    _drive_cfvec_slot(dut, 0, valid=1, pc=0x80004A00, instr=0x00000013, is_rvc=0, pred_taken=0, ftq_flag=1, ftq_value=51, ftq_offset=0, is_last=0)
    _drive_cfvec_slot(dut, 1, valid=1, pc=0x80004A02, instr=0x00000013, is_rvc=1, pred_taken=0, ftq_flag=1, ftq_value=51, ftq_offset=1, is_last=0)
    _drive_cfvec_slot(dut, 2, valid=1, pc=0x80004A04, instr=0x00000013, is_rvc=1, pred_taken=0, ftq_flag=1, ftq_value=51, ftq_offset=2, is_last=0)
    _drive_cfvec_slot(dut, 3, valid=1, pc=0x80004A06, instr=0x00000013, is_rvc=0, pred_taken=0, ftq_flag=1, ftq_value=51, ftq_offset=4, is_last=1)

    bm.current_cycle = 0
    bm._sample_cfvec()

    assert bm._current_ftq_entry is None
    assert [(entry.ftq_flag, entry.ftq_value, entry.dispatch_complete, entry.observed_last_in_entry) for entry in bm.ftq_entries] == [
        (1, 51, True, True)
    ]


def test_golden_wait_target_entry_suffix_does_not_reset_same_ftq_slot_state() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x800048FE, instr=0x06700713, size=4),
                TraceEntry(index=1, pc=0x80004902, instr=0x00068C93, size=4),
                TraceEntry(index=2, pc=0x80004904, instr=0x0AA76F63, size=4, kind="branch", taken=True, target_pc=0x800049C2),
                TraceEntry(index=3, pc=0x800049C2, instr=0xF975071B, size=4),
            ]
        )
    )

    _drive_cfvec_slot(dut, 0, valid=1, pc=0x800048FE, instr=0x06700713, is_rvc=0, pred_taken=0, ftq_flag=1, ftq_value=49, ftq_offset=1, is_last=0)
    _drive_cfvec_slot(dut, 1, valid=1, pc=0x80004902, instr=0x00068C93, is_rvc=0, pred_taken=0, ftq_flag=1, ftq_value=49, ftq_offset=2, is_last=0)
    _drive_cfvec_slot(dut, 2, valid=1, pc=0x80004904, instr=0x0AA76F63, is_rvc=0, pred_taken=0, ftq_flag=1, ftq_value=49, ftq_offset=4, is_last=0)

    bm.current_cycle = 0
    bm._sample_cfvec()

    assert bm._golden_wait_pc == 0x800049C2
    assert [(entry.pc, entry.path_state) for entry in bm._semantic_queue] == [
        (0x800048FE, "correct"),
        (0x80004902, "correct"),
        (0x80004904, "correct"),
    ]
    bm._pending_level0_target_ftq = (1, 49)

    _clear_cfvec(dut)
    _drive_cfvec_slot(dut, 0, valid=1, pc=0x8000491C, instr=0xF2E6E6E3, is_rvc=0, pred_taken=0, ftq_flag=1, ftq_value=49, ftq_offset=16, is_last=1)

    bm.current_cycle = 1
    bm._sample_cfvec()

    assert [(entry.pc, entry.path_state, entry.is_last_in_entry) for entry in bm._semantic_queue] == [
        (0x800048FE, "correct", False),
        (0x80004902, "correct", False),
        (0x80004904, "correct", True),
        (0x8000491C, "wrong", True),
    ]
    assert len(bm._pending_resolves) == 1
    assert bm._pending_resolves[0].inst_pc == 0x80004904
    assert bm.ftq_entries[0].total_cfi == 1


def test_golden_wait_pc_ignores_wrong_path_prefix_when_target_slot_is_known():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm._golden_wait_pc = 0x80004A28
    bm._pending_level0_target_ftq = (1, 51)

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80003E7A,
        instr=0x00000013,
        is_rvc=1,
        pred_taken=0,
        ftq_flag=1,
        ftq_value=52,
        ftq_offset=0,
        is_last=0,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    assert bm._current_ftq_entry is None
    assert bm._ftq_group_pc_history.get((1, 52)) is None


def test_golden_wait_pc_ignores_wrong_path_prefix_when_target_group_is_known_from_history():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm._golden_wait_pc = 0x80004A82
    bm._pc_group_occurrences[0x80004A82] = [(1, 59, 1)]

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80004848,
        instr=0x00000013,
        is_rvc=1,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=8,
        ftq_offset=0,
        is_last=0,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    assert bm._current_ftq_entry is None
    assert bm._ftq_group_pc_history.get((0, 8)) is None


def test_level0_redirect_keeps_waiting_if_next_target_slot_was_seen_only_on_wrong_path():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.commit_count = 64
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 35
    bm._golden_wait_pc = 0x800047FA
    bm._current_ftq_entry = FtqEntry(ftq_flag=0, ftq_value=36)

    bm._drive_redirect(
        {
            "target_pc": 0x800047FA,
            "reason": "golden_resolve_redirect",
            "flush_on_drive": True,
            "pc": 0x80004848,
            "taken": 1,
            "ftq_flag": 0,
            "ftq_value": 35,
            "ftq_offset": 4,
            "branch_type": 3,
            "ras_action": 1,
            "level": 0,
        }
    )

    assert bm._pending_level0_target_ftq == (0, 36)


def test_level0_redirect_does_not_clear_waiting_target_slot_on_ptr_match_without_target_pc():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm._golden_wait_pc = 0x800047FA
    bm._pending_level0_target_ftq = (0, 36)

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80004852,
        instr=0x00000013,
        is_rvc=0,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=36,
        ftq_offset=0,
        is_last=0,
    )
    bm.current_cycle = 0
    bm._sample_cfvec()

    assert bm._pending_level0_target_ftq == (0, 36)


def test_level0_redirect_keeps_waiting_when_next_target_group_has_only_partial_target_prefix():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.commit_count = 64
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 35
    bm._current_ftq_entry = FtqEntry(ftq_flag=0, ftq_value=36)
    bm._ftq_group_pc_history[(0, 36)] = [(0x800047FA, False)]

    bm._drive_redirect(
        {
            "target_pc": 0x800047FA,
            "reason": "golden_resolve_redirect",
            "flush_on_drive": True,
            "pc": 0x80004848,
            "taken": 1,
            "ftq_flag": 0,
            "ftq_value": 35,
            "ftq_offset": 4,
            "branch_type": 3,
            "ras_action": 1,
            "level": 0,
        }
    )

    assert bm._pending_level0_target_ftq == (0, 36)


def test_level0_redirect_keeps_waiting_when_next_target_slot_has_old_dispatch_complete_history():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.commit_count = 64
    bm.commit_ptr_flag = 1
    bm.commit_ptr_value = 58
    bm.ftq_entries.append(
        FtqEntry(ftq_flag=1, ftq_value=60, total_cfi=1, resolved_cfi=1, dispatch_complete=True, commit_ready_cycle=0)
    )
    bm._ftq_group_pc_history[(1, 60)] = [(0x80004A82, True)]

    bm._drive_redirect(
        {
            "target_pc": 0x80004A82,
            "reason": "golden_resolve_redirect",
            "flush_on_drive": True,
            "pc": 0x80004A80,
            "taken": 1,
            "ftq_flag": 1,
            "ftq_value": 59,
            "ftq_offset": 0,
            "branch_type": 1,
            "ras_action": 0,
            "level": 0,
        }
    )

    assert bm._pending_level0_target_ftq == (1, 60)


def test_target_ftq_prefix_matching_wait_pc_does_not_clear_wait_until_entry_closes():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm._golden_wait_pc = 0x80004A82
    bm._pending_level0_target_ftq = (0, 0)

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80004A82,
        instr=0x00000013,
        is_rvc=1,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=0,
        ftq_offset=0,
        is_last=0,
    )
    bm.current_cycle = 0
    bm._sample_cfvec()

    assert bm._pending_level0_target_ftq == (0, 0)

    _clear_cfvec(dut)
    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80004A84,
        instr=0x00000013,
        is_rvc=1,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=0,
        ftq_offset=1,
        is_last=1,
    )
    bm.current_cycle = 1
    bm._sample_cfvec()

    assert bm._pending_level0_target_ftq is None


def test_waiting_target_is_last_without_matching_wait_pc_does_not_clear_pending_slot():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm._golden_wait_pc = 0x80004A82
    bm._pending_level0_target_ftq = (1, 60)

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80004AC0,
        instr=0x00000013,
        is_rvc=0,
        pred_taken=0,
        ftq_flag=1,
        ftq_value=60,
        ftq_offset=0,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    assert bm._pending_level0_target_ftq == (1, 60)


def test_stale_cfvec_slot_older_than_commit_ptr_is_ignored() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.commit_count = 248
    bm.commit_ptr_flag = 1
    bm.commit_ptr_value = 58
    bm._golden_wait_pc = 0x80004D1C

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80004A82,
        instr=0x00000013,
        is_rvc=1,
        pred_taken=0,
        ftq_flag=1,
        ftq_value=54,
        ftq_offset=0,
        is_last=0,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    assert bm._current_ftq_entry is None
    assert list(bm.ftq_entries) == []
    assert bm._ftq_group_pc_history.get((1, 54)) is None


def test_simfrontend_redirect_override_does_not_reuse_internal_group_for_indirect_redirect():
    bm = BackendModel()
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 42
    bm._ftq_group_pc_history[(1, 54)] = [
        (0x80004852, False),
        (0x80004856, False),
        (0x8000485A, True),
        (0x8000485C, False),
        (0x800047FA, False),
    ]
    bm._pc_group_occurrences[0x800047FA] = [(1, 54, 4)]

    override = bm._simfrontend_redirect_drive_override(
        {
            "target_pc": 0x800047FA,
            "ftq_flag": 0,
            "ftq_value": 43,
            "branch_type": 3,
        }
    )

    assert override is None


def test_level0_redirect_override_reusing_observed_group_keeps_waiting_for_real_next_target_slot():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.commit_count = 64
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 0
    bm._ftq_group_pc_history[(1, 54)] = [
        (0x80004A80, False),
        (0x80004A82, False),
        (0x80004A84, False),
    ]
    bm._pc_group_occurrences[0x80004A82] = [(1, 54, 1)]

    payload = bm._plan_redirect_payload(
        {
            "target_pc": 0x80004A82,
            "reason": "golden_resolve_redirect",
            "flush_on_drive": True,
            "pc": 0x80004A9A,
            "taken": 1,
            "ftq_flag": 0,
            "ftq_value": 0,
            "ftq_offset": 8,
            "branch_type": 1,
            "ras_action": 0,
            "level": 0,
        }
    )

    assert bm._pending_level0_target_ftq == (0, 1)
    assert payload["pc"] == 0x80004A80
    assert payload["ftq_value"] == 54


def test_commit_clears_waiting_target_slot_when_it_reaches_pending_level0_target():
    bm = BackendModel()
    bm.commit_count = 64
    bm.commit_ptr_flag = 1
    bm.commit_ptr_value = 50
    bm._pending_level0_target_ftq = (1, 51)
    bm.ftq_entries.append(
        FtqEntry(ftq_flag=1, ftq_value=51, total_cfi=0, resolved_cfi=0, dispatch_complete=True, commit_ready_cycle=0)
    )

    head = bm._plan_commit_entry_for_cycle()

    assert head is not None
    assert (head.ftq_flag, head.ftq_value) == (1, 51)
    assert bm._pending_level0_target_ftq is None


def test_pending_level0_target_serializes_commit_before_wrap_target_entry() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(GoldenTrace([]))
    bm.current_cycle = 10
    bm.commit_count = 255
    bm.commit_ptr_flag = 1
    bm.commit_ptr_value = 59
    bm._pending_level0_target_ftq = (0, 0)
    bm.ftq_entries.extend(
        [
            FtqEntry(ftq_flag=1, ftq_value=60, total_cfi=1, resolved_cfi=1, dispatch_complete=True, commit_ready_cycle=0),
            FtqEntry(ftq_flag=1, ftq_value=61, total_cfi=1, resolved_cfi=1, dispatch_complete=True, commit_ready_cycle=0),
            FtqEntry(ftq_flag=1, ftq_value=62, total_cfi=1, resolved_cfi=1, dispatch_complete=True, commit_ready_cycle=0),
            FtqEntry(ftq_flag=1, ftq_value=63, total_cfi=1, resolved_cfi=1, dispatch_complete=True, commit_ready_cycle=0),
            FtqEntry(ftq_flag=0, ftq_value=0, total_cfi=1, resolved_cfi=1, dispatch_complete=False, commit_ready_cycle=0),
        ]
    )

    head = bm._plan_commit_entry_for_cycle()

    assert head is not None
    assert (head.ftq_flag, head.ftq_value) == (1, 60)
    assert (bm.commit_ptr_flag, bm.commit_ptr_value) == (1, 60)
    assert list((entry.ftq_flag, entry.ftq_value) for entry in bm.ftq_entries) == [
        (1, 61),
        (1, 62),
        (1, 63),
        (0, 0),
    ]
    assert bm._pending_level0_target_ftq == (0, 0)


def test_commit_prunes_older_wrap_entries_when_pointer_advances() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(GoldenTrace([]))
    bm.current_cycle = 10
    bm.commit_count = 251
    bm.commit_ptr_flag = 1
    bm.commit_ptr_value = 57
    bm.ftq_entries.extend(
        [
            FtqEntry(ftq_flag=1, ftq_value=54, total_cfi=1, resolved_cfi=1, dispatch_complete=True, commit_ready_cycle=0),
            FtqEntry(ftq_flag=1, ftq_value=55, total_cfi=1, resolved_cfi=1, dispatch_complete=True, commit_ready_cycle=0),
            FtqEntry(ftq_flag=1, ftq_value=56, total_cfi=1, resolved_cfi=1, dispatch_complete=True, commit_ready_cycle=0),
            FtqEntry(ftq_flag=1, ftq_value=57, total_cfi=1, resolved_cfi=1, dispatch_complete=True, commit_ready_cycle=0),
            FtqEntry(ftq_flag=1, ftq_value=58, total_cfi=1, resolved_cfi=1, dispatch_complete=True, commit_ready_cycle=0),
        ]
    )

    head = bm._plan_commit_entry_for_cycle()

    assert head is not None
    assert (head.ftq_flag, head.ftq_value) == (1, 58)
    assert (bm.commit_ptr_flag, bm.commit_ptr_value) == (1, 58)
    assert list((entry.ftq_flag, entry.ftq_value) for entry in bm.ftq_entries) == []


def test_same_ftq_slot_replay_resets_observed_group_start_pc() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80004A28,
        instr=0x00000013,
        is_rvc=0,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=39,
        ftq_offset=4,
        is_last=0,
    )
    bm.current_cycle = 0
    bm._sample_cfvec()

    _clear_cfvec(dut)
    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x800047FA,
        instr=0x00000013,
        is_rvc=0,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=39,
        ftq_offset=0,
        is_last=0,
    )
    bm.current_cycle = 1
    bm._sample_cfvec()

    assert bm._observed_ftq_start_pc(0, 39) == 0x800047FA
    assert bm._ftq_group_pc_history[(0, 39)] == [(0x800047FA, False)]


def test_backend_model_consumes_backend_observation_snapshot() -> None:
    bm = BackendModel()
    obs = BackendObservationSnapshot(
        from_ftq_wen=1,
        from_ftq_ftq_idx=7,
        from_ftq_start_pc_addr=0x40000123,
        ibuf_full=0,
    )

    bm.consume_backend_observation(obs)

    assert bm.current_frontend_observation().from_ftq_ftq_idx == 7


def test_callret_commit_becomes_visible_one_cycle_after_golden_trace_consumption() -> None:
    base = 0x80000000
    instr = 0x008000EF  # jal x1, 8

    dut = _DummyDut()
    bm = BackendModel(resolve_min_delay=0, resolve_max_delay=0)
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(
                    index=0,
                    pc=base,
                    instr=instr,
                    size=4,
                    kind="jump",
                    taken=True,
                    target_pc=base + 8,
                )
            ]
        )
    )

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=base,
        instr=instr,
        is_rvc=0,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=5,
        ftq_offset=0,
        is_last=1,
    )

    bm.on_clock_edge(0)

    assert int(dut.io_backend_toFtq_callRetCommit_0_valid.value) == 0

    _clear_cfvec(dut)
    bm.on_clock_edge(1)

    assert int(dut.io_backend_toFtq_callRetCommit_0_valid.value) == 1
    assert int(dut.io_backend_toFtq_callRetCommit_0_bits_rasAction.value) == 2
    assert int(dut.io_backend_toFtq_callRetCommit_0_bits_ftqPtr_value.value) == 5


def test_semantic_queue_callret_tracks_per_instruction_commit_projection() -> None:
    base = 0x80000100
    instr = 0x008000EF  # jal x1, 8

    dut = _DummyDut()
    bm = BackendModel(resolve_min_delay=0, resolve_max_delay=0)
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(
                    index=0,
                    pc=base,
                    instr=instr,
                    size=4,
                    kind="jump",
                    taken=True,
                    target_pc=base + 8,
                )
            ]
        )
    )

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=base,
        instr=instr,
        is_rvc=0,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=9,
        ftq_offset=0,
        is_last=1,
    )

    bm.on_clock_edge(0)

    assert bm._semantic_queue[0].rob_commit_state == "committed"
    assert bm._semantic_queue[0].call_ret_commit_state == "pending"
    assert list(bm._pending_queue_call_ret_commit_indices) == []
    scheduled_groups = list(bm._scheduled_queue_call_ret_commit_groups)
    assert len(scheduled_groups) == 1
    assert scheduled_groups[0][0] == 1
    assert len(scheduled_groups[0][1]) == 1
    assert scheduled_groups[0][1][0].queue_index == 0

    _clear_cfvec(dut)
    bm.on_clock_edge(1)

    assert bm._semantic_queue[0].call_ret_commit_state == "emitted"
    assert int(dut.io_backend_toFtq_callRetCommit_0_valid.value) == 1
    assert int(dut.io_backend_toFtq_callRetCommit_0_bits_rasAction.value) == 2
    assert int(dut.io_backend_toFtq_callRetCommit_0_bits_ftqPtr_value.value) == 9


def test_semantic_queue_commit_pops_only_queue_head_ftq_entry() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000200, instr=0x00000013, size=4),
                TraceEntry(index=1, pc=0x80000204, instr=0x00000013, size=4),
            ]
        )
    )

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000200,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=10,
        ftq_offset=0,
        is_last=1,
    )
    _drive_cfvec_slot(
        dut,
        1,
        valid=1,
        pc=0x80000204,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=11,
        ftq_offset=0,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()
    for entry in bm.ftq_entries:
        entry.commit_ready_cycle = 0
    bm.current_cycle = 10

    commit_entry = bm._plan_commit_entry_for_cycle()

    assert commit_entry is not None
    assert (commit_entry.ftq_flag, commit_entry.ftq_value) == (0, 10)
    assert [(entry.pc, entry.ftq_value) for entry in bm._semantic_queue] == [(0x80000204, 11)]
    assert [(entry.ftq_flag, entry.ftq_value) for entry in bm.ftq_entries] == [(0, 11)]
    assert (bm.commit_ptr_flag, bm.commit_ptr_value) == (0, 10)
    assert bm.commit_count == 1


def test_semantic_queue_commit_waits_for_correct_path_cfi_resolve() -> None:
    dut = _DummyDut()
    bm = BackendModel(resolve_min_delay=0, resolve_max_delay=0)
    bm.bind(dut)
    instr = 0x008000EF  # jal x1, 8
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000300, instr=instr, size=4, kind="jump", taken=True, target_pc=0x80000308),
            ]
        )
    )

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000300,
        instr=instr,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=12,
        ftq_offset=0,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()
    bm.ftq_entries[0].commit_ready_cycle = 0
    bm.current_cycle = 10

    assert bm._plan_commit_entry_for_cycle() is None

    bm._drive_resolves()
    bm.current_cycle = 20
    commit_entry = bm._plan_commit_entry_for_cycle()

    assert commit_entry is not None
    assert (commit_entry.ftq_flag, commit_entry.ftq_value) == (0, 12)
    assert list(bm._semantic_queue) == []




def test_golden_trace_zero_instruction_entry_does_not_wait_for_commit_visibility() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(GoldenTrace([]))
    bm.current_cycle = 10
    bm.ftq_entries.append(
        FtqEntry(
            ftq_flag=0,
            ftq_value=7,
            total_cfi=0,
            resolved_cfi=0,
            dispatch_complete=True,
            commit_ready_cycle=0,
        )
    )

    commit_entry = bm._plan_commit_entry_for_cycle()

    assert commit_entry is not None
    assert (commit_entry.ftq_flag, commit_entry.ftq_value) == (0, 7)


def test_backend_fault_redirect_requires_explicit_ftq_context() -> None:
    bm = BackendModel()

    import pytest

    with pytest.raises(AssertionError, match="backend-fault redirect"):
        bm._plan_redirect_payload(
            {
                "target_pc": 0x80000000,
                "reason": "backend_fault",
                "backend_ipf": 1,
            }
        )

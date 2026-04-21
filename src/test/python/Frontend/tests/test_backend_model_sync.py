from __future__ import annotations

from env.backend_model import BackendModel, FtqEntry, ResolveEntry
from env.model.backend_state import BackendEvent, CommitInstruction, QueueInstr
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


def test_explicit_injection_can_be_disabled_for_bin_mode() -> None:
    bm = BackendModel()
    bm.set_explicit_injection_enabled(False, reason="bin/program-file mode active")

    import pytest

    with pytest.raises(AssertionError, match="redirect injection is disabled"):
        bm.inject_redirect(target_pc=0x80000080, reason="manual")
    with pytest.raises(AssertionError, match="exception injection is disabled"):
        bm.inject_exception(cause=2, tval=0, pc=0x80000000)


def test_explicit_injection_reenabled_after_non_bin_program_load() -> None:
    bm = BackendModel()
    bm.set_explicit_injection_enabled(False, reason="bin/program-file mode active")
    bm.set_explicit_injection_enabled(True)

    bm.current_cycle = 100
    bm.inject_redirect(target_pc=0x80000080, reason="manual")
    assert len(bm.pending_events) == 1


def test_inject_redirect_clears_cfvec_queue_state() -> None:
    bm = BackendModel()
    bm._cfvec_queue.extend(
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

    assert list(bm._cfvec_queue) == []
    assert bm._pending_redirect_origin_index is None
    assert list(bm._pending_queue_resolve_indices) == []
    assert list(bm._pending_queue_call_ret_commit_indices) == []
    assert list(bm._scheduled_queue_call_ret_commit_groups) == []
    assert bm._visible_queue_call_ret_commit_group == []
    assert list(bm._pending_resolves) == []


def test_pending_work_count_includes_callret_related_queues() -> None:
    bm = BackendModel()
    assert bm.pending_work_count() == 0
    assert bm.has_pending_work() is False

    bm._pending_queue_call_ret_commit_indices.extend([0, 1])
    bm._scheduled_queue_call_ret_commit_groups.append(
        (
            10,
            [
                CommitInstruction(0, 0x80000000, 0x13, 0, 1, 0, queue_index=0, ftq_offset=0),
                CommitInstruction(1, 0x80000004, 0x13, 0, 1, 0, queue_index=1, ftq_offset=2),
            ],
        )
    )
    bm._visible_queue_call_ret_commit_group = [
        CommitInstruction(2, 0x80000008, 0x13, 0, 2, 0, queue_index=0, ftq_offset=0)
    ]

    assert bm.pending_work_count() == 5
    assert bm.has_pending_work() is True


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


def test_cfvec_queue_scaffolding_mirrors_unique_cfvec_packets_in_order() -> None:
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

    cfvec_queue = list(bm._cfvec_queue)
    assert len(cfvec_queue) == 2
    assert [(entry.slot, entry.pc, entry.ftq_offset) for entry in cfvec_queue] == [
        (0, 0x80000000, 0),
        (1, 0x80000004, 2),
    ]
    assert cfvec_queue[0].is_cfi is False
    assert cfvec_queue[0].resolve_state == "not_needed"
    assert cfvec_queue[1].is_cfi is True
    assert cfvec_queue[1].resolve_state == "pending"


def test_cfvec_queue_scaffolding_deduplicates_repeated_packet_within_same_ftq_entry() -> None:
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

    cfvec_queue = list(bm._cfvec_queue)
    assert len(cfvec_queue) == 1
    assert cfvec_queue[0].pc == 0x80000000
    assert cfvec_queue[0].ftq_value == 3


def test_cfvec_queue_marks_first_mismatch_as_wrong_path_start() -> None:
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

    cfvec_queue = list(bm._cfvec_queue)
    assert len(cfvec_queue) == 2
    assert cfvec_queue[0].golden_match_state == "matched"
    assert cfvec_queue[0].path_state == "correct"
    assert cfvec_queue[1].golden_match_state == "mismatched"
    assert cfvec_queue[1].path_state == "wrong"
    assert bm._pending_redirect_origin_index == 1


def test_cfvec_queue_marks_younger_packets_wrong_after_redirect_origin() -> None:
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
    bm.resolve_min_delay = 0
    bm.resolve_max_delay = 0

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

    cfvec_queue = list(bm._cfvec_queue)
    assert [entry.path_state for entry in cfvec_queue] == ["correct", "wrong", "wrong"]
    assert bm._pending_redirect_origin_index == 1


def test_cfvec_queue_non_cfi_first_mismatch_starts_wrong_path() -> None:
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
    bm.resolve_min_delay = 0
    bm.resolve_max_delay = 0

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

    cfvec_queue = list(bm._cfvec_queue)
    assert [entry.path_state for entry in cfvec_queue] == ["correct", "wrong", "wrong"]
    assert [entry.golden_match_state for entry in cfvec_queue] == ["matched", "mismatched", "mismatched"]
    assert bm._pending_redirect_origin_index == 1
    queued_reasons = [str(evt.payload.get("reason", "")) for evt in bm.pending_events]
    assert queued_reasons == ["golden_first_mismatch_redirect"]
    assert len(bm._pending_resolves) == 1
    assert bm._pending_resolves[0].queue_index == 0
    assert bm._pending_resolves[0].mispredict is True
    assert bm._pending_resolves[0].target == 0x80000004


def test_cfvec_queue_redirect_flush_prunes_wrong_path_suffix() -> None:
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
    bm.resolve_min_delay = 0
    bm.resolve_max_delay = 0

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
    assert len(bm._cfvec_queue) == 3
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
    assert [(entry.pc, entry.path_state) for entry in bm._cfvec_queue] == [(0x80000000, "correct")]
    assert bm._pending_redirect_origin_index is None


def test_first_mismatch_without_attributable_cfi_fails_fast() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000000, instr=0x00000013, size=4),
                TraceEntry(index=1, pc=0x80000004, instr=0x00000013, size=4),
                TraceEntry(index=2, pc=0x80000008, instr=0x00000013, size=4),
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
        ftq_value=5,
        ftq_offset=0,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        1,
        valid=1,
        pc=0x80000020,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=5,
        ftq_offset=2,
        is_last=1,
    )

    bm.current_cycle = 0
    import pytest

    with pytest.raises(AssertionError, match="first mismatch has no attributable CFI"):
        bm._sample_cfvec()


def test_first_mismatch_without_queue_cfi_uses_last_correct_cfi_context() -> None:
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
    bm._last_correct_cfi_context = {
        "pc": 0x80000010,
        "instr": 0x0000006F,
        "is_rvc": 0,
        "pred_taken": 1,
        "ftq_flag": 0,
        "ftq_value": 7,
        "ftq_offset": 2,
        "branch_type": 2,
        "ras_action": 0,
        "queue_index": None,
    }

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000020,
        instr=0x00000013,
        ftq_flag=0,
        ftq_value=9,
        ftq_offset=0,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    assert bm._pending_redirect_origin_index == 0
    queued_redirects = [evt for evt in bm.pending_events if evt.kind == "redirect"]
    assert len(queued_redirects) == 1
    payload = queued_redirects[0].payload
    assert payload["reason"] == "golden_first_mismatch_redirect"
    assert int(payload["pc"]) == 0x80000010
    assert int(payload["target_pc"]) == 0x80000000
    assert int(payload["taken"]) == 1


def test_cfvec_queue_redirect_flush_prunes_wrong_path_pending_resolves() -> None:
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
    bm.resolve_min_delay = 0
    bm.resolve_max_delay = 0

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

    assert len(bm._pending_resolves) == 1

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


def test_cfvec_queue_transition_without_last_marks_surviving_prefix_closed() -> None:
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

    cfvec_queue = list(bm._cfvec_queue)
    assert [(entry.ftq_value, entry.is_last_in_entry) for entry in cfvec_queue] == [
        (20, False),
        (20, True),
        (21, True),
    ]


def test_cfvec_queue_wait_break_marks_current_entry_closed() -> None:
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

    cfvec_queue = list(bm._cfvec_queue)
    assert len(cfvec_queue) == 2
    assert cfvec_queue[0].is_last_in_entry is True
    assert cfvec_queue[1].path_state == "wrong"


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
    assert list(bm._cfvec_queue)[0].is_last_in_entry is True


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

    cfvec_queue = list(bm._cfvec_queue)
    assert len(cfvec_queue) == 1
    assert cfvec_queue[0].is_cfi is True
    assert cfvec_queue[0].path_state == "wrong"
    assert cfvec_queue[0].resolve_state == "skipped"
    assert list(bm._pending_queue_resolve_indices) == []
    assert all(
        str(evt.payload.get("reason", "")) != "golden_first_mismatch_redirect"
        for evt in bm.pending_events
        if evt.kind == "redirect"
    )


def test_semantic_recovery_residual_prefix_does_not_queue_new_mismatch_redirect() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(GoldenTrace([TraceEntry(index=0, pc=0x80000020, instr=0x00000013, size=4)]))
    bm._cfvec_queue.append(
        QueueInstr(
            cycle=0,
            slot=0,
            pc=0x80000000,
            instr=0x00000013,
            is_rvc=False,
            pred_taken=False,
            ftq_flag=0,
            ftq_value=10,
            ftq_offset=0,
            is_last_in_entry=True,
            path_state="correct",
            golden_match_state="matched",
        )
    )
    bm._semantic_recovery_target_pc = 0x80000020
    bm._semantic_recovery_queue_start = len(bm._cfvec_queue)

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000010,
        instr=0x0000006F,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=11,
        ftq_offset=0,
        is_last=1,
    )

    bm.current_cycle = 1
    bm._sample_cfvec()

    assert bm._semantic_recovery_target_pc == 0x80000020
    assert len(bm._cfvec_queue) == 2
    assert bm._cfvec_queue[-1].path_state == "wrong"
    assert bm._cfvec_queue[-1].resolve_state == "skipped"
    assert all(
        str(evt.payload.get("reason", "")) != "golden_first_mismatch_redirect"
        for evt in bm.pending_events
        if evt.kind == "redirect"
    )


def test_post_redirect_prefix_before_target_does_not_queue_duplicate_redirect() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000052, instr=0x0000147D, size=2, kind="normal", taken=False, target_pc=None),
            ]
        )
    )
    bm._golden_wait_pc = 0x80000052
    bm._golden_wait_requires_redirect = True
    bm._post_redirect_target_pc = 0x80000052

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000054,
        instr=0x00001405,
        is_rvc=1,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=5,
        ftq_offset=0,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        1,
        valid=1,
        pc=0x80000058,
        instr=0x00038463,
        is_rvc=0,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=5,
        ftq_offset=1,
        is_last=0,
    )

    bm.current_cycle = 1
    bm._sample_cfvec()

    assert bm._post_redirect_target_pc == 0x80000052
    assert [entry.path_state for entry in bm._cfvec_queue] == ["wrong", "wrong"]
    assert all(
        str(evt.payload.get("reason", "")) != "golden_first_mismatch_redirect"
        for evt in bm.pending_events
        if evt.kind == "redirect"
    )


def test_observing_redirect_target_drops_stale_pending_same_target_redirects() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm._post_redirect_target_pc = 0x800000FA
    bm.pending_events.extend(
        [
            BackendEvent(
                kind="redirect",
                ready_cycle=10,
                payload={"target_pc": 0x800000FA, "reason": "golden_first_mismatch_redirect"},
            ),
            BackendEvent(
                kind="redirect",
                ready_cycle=11,
                payload={"target_pc": 0x800000FA, "reason": "golden_first_mismatch_redirect"},
            ),
        ]
    )

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x800000FA,
        instr=0x00001479,
        is_rvc=1,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=54,
        ftq_offset=0,
        is_last=0,
    )

    bm.current_cycle = 1
    bm._sample_cfvec()

    assert bm._post_redirect_target_pc is None
    assert len([evt for evt in bm.pending_events if evt.kind == "redirect"]) == 0


def test_stale_cfvec_packet_is_sampled_as_wrong_residual() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.commit_count = 1
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 4

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x800000f6,
        instr=0x00001415,
        is_rvc=1,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=3,
        ftq_offset=0,
        is_last=1,
    )

    bm.current_cycle = 1
    bm._sample_cfvec()

    assert len(bm._cfvec_queue) == 1
    entry = bm._cfvec_queue[0]
    assert int(entry.pc) == 0x800000f6
    assert entry.path_state == "wrong"
    assert entry.golden_match_state == "mismatched"


def test_cfvec_queue_commit_uses_queue_head_when_ftq_entries_are_out_of_order() -> None:
    bm = BackendModel()
    bm.set_golden_trace(GoldenTrace([]))
    bm._cfvec_queue.extend(
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


def test_cfvec_queue_correct_path_cfi_emits_resolve_and_marks_emitted() -> None:
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
    assert bm._cfvec_queue[0].resolve_state == "emitted"
    assert list(bm._pending_queue_resolve_indices) == []


def test_cfvec_queue_wrong_path_cfi_resolve_is_skipped() -> None:
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
    assert bm._cfvec_queue[1].resolve_state == "skipped"
    assert bm._cfvec_queue[2].resolve_state == "skipped"
    assert len(bm._pending_resolves) == 0
    assert list(bm._pending_queue_resolve_indices) == []


def test_cfvec_queue_first_mismatch_cfi_queues_first_mismatch_redirect() -> None:
    dut = _DummyDut()
    bm = BackendModel(resolve_min_delay=0, resolve_max_delay=0, redirect_min_delay=0, redirect_max_delay=0)
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
        ftq_value=7,
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
        ftq_value=7,
        ftq_offset=2,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    queued_redirects = [evt for evt in bm.pending_events if evt.kind == "redirect"]
    assert len(queued_redirects) >= 1
    reasons = [str(evt.payload.get("reason", "")) for evt in queued_redirects]
    assert "golden_first_mismatch_redirect" in reasons
    mismatch_redirect = next(evt for evt in queued_redirects if evt.payload.get("reason") == "golden_first_mismatch_redirect")
    assert int(mismatch_redirect.payload["target_pc"]) == 0x80000004
    assert int(mismatch_redirect.payload["pc"]) == 0x80000010


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


def test_semantic_commit_skips_stale_older_ftq_entry_not_present_in_queue() -> None:
    bm = BackendModel()
    bm.set_golden_trace(
        GoldenTrace([TraceEntry(index=0, pc=0x80004840, instr=0x00000013, size=4)])
    )
    bm.commit_count = 100
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 29
    bm.current_cycle = 200
    bm.ftq_entries.extend(
        [
            FtqEntry(ftq_flag=0, ftq_value=29, dispatch_complete=True, observed_last_in_entry=True, commit_ready_cycle=0),
            FtqEntry(ftq_flag=0, ftq_value=30, dispatch_complete=True, observed_last_in_entry=True, commit_ready_cycle=0),
        ]
    )
    bm._cfvec_queue.append(
        QueueInstr(
            cycle=150,
            slot=0,
            pc=0x80004840,
            instr=0x00000013,
            is_rvc=False,
            pred_taken=False,
            ftq_flag=0,
            ftq_value=30,
            ftq_offset=0,
            is_last_in_entry=True,
            path_state="correct",
            resolve_state="not_needed",
            rob_commit_state="committed",
            golden_match_state="matched",
            is_cfi=False,
        )
    )

    commit_entry = bm._plan_commit_entry_for_cycle()

    assert commit_entry is not None
    assert (commit_entry.ftq_flag, commit_entry.ftq_value) == (0, 30)
    assert (bm.commit_ptr_flag, bm.commit_ptr_value) == (0, 30)


def test_semantic_commit_infers_head_span_close_when_dispatch_complete_and_current_entry_absent() -> None:
    bm = BackendModel()
    bm.set_golden_trace(
        GoldenTrace([TraceEntry(index=0, pc=0x80004802, instr=0x00000013, size=4)])
    )
    bm.commit_count = 100
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 29
    bm.current_cycle = 200
    bm.ftq_entries.append(
        FtqEntry(ftq_flag=0, ftq_value=30, dispatch_complete=True, observed_last_in_entry=False, commit_ready_cycle=0)
    )
    bm._cfvec_queue.extend(
        [
            QueueInstr(
                cycle=150,
                slot=0,
                pc=0x80004802,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=30,
                ftq_offset=0,
                is_last_in_entry=False,
                path_state="correct",
                resolve_state="not_needed",
                rob_commit_state="committed",
                golden_match_state="matched",
                is_cfi=False,
            ),
            QueueInstr(
                cycle=150,
                slot=1,
                pc=0x80004804,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=30,
                ftq_offset=2,
                is_last_in_entry=False,
                path_state="correct",
                resolve_state="not_needed",
                rob_commit_state="committed",
                golden_match_state="matched",
                is_cfi=False,
            ),
        ]
    )

    commit_entry = bm._plan_commit_entry_for_cycle()

    assert commit_entry is not None
    assert (commit_entry.ftq_flag, commit_entry.ftq_value) == (0, 30)
    assert (bm.commit_ptr_flag, bm.commit_ptr_value) == (0, 30)
    assert len(bm._cfvec_queue) == 0


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
    bm._ftq_group_pc_history[(1, 51)] = [
        (0x80004A20, False),
        (0x80004A24, False),
        (0x80004A28, False),
    ]
    bm._pc_group_occurrences[0x80004A28] = [(1, 51, 2)]

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


def test_level0_redirect_flush_closes_surviving_current_ftq_span_when_current_entry_clears() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.commit_count = 100
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 20
    bm._current_ftq_entry = FtqEntry(ftq_flag=0, ftq_value=25)
    bm._cfvec_queue.extend(
        [
            QueueInstr(
                cycle=10,
                slot=0,
                pc=0x80004802,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=25,
                ftq_offset=0,
                is_last_in_entry=False,
                path_state="correct",
                resolve_state="not_needed",
                rob_commit_state="committed",
                golden_match_state="matched",
                is_cfi=False,
            ),
            QueueInstr(
                cycle=10,
                slot=1,
                pc=0x80004804,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=25,
                ftq_offset=2,
                is_last_in_entry=False,
                path_state="correct",
                resolve_state="not_needed",
                rob_commit_state="committed",
                golden_match_state="matched",
                is_cfi=False,
            ),
        ]
    )

    bm._drive_redirect(
        {
            "target_pc": 0x800047FA,
            "reason": "golden_resolve_redirect",
            "flush_on_drive": True,
            "pc": 0x800047F0,
            "taken": 1,
            "ftq_flag": 0,
            "ftq_value": 23,
            "ftq_offset": 4,
            "branch_type": 1,
            "ras_action": 0,
            "level": 0,
        }
    )

    assert bm._current_ftq_entry is None
    assert bm._cfvec_queue[-1].is_last_in_entry is True


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


def test_level0_redirect_flush_keeps_next_target_pending_resolve() -> None:
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.current_cycle = 100
    bm.commit_count = 32
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 10
    bm._pending_resolves.extend(
        [
            ResolveEntry(
                ready_cycle=130,
                inst_pc=0x80004F26,
                pc=0x80004F22,
                target=0x80004F32,
                taken=True,
                mispredict=False,
                ftq_flag=0,
                ftq_value=12,
                ftq_offset=2,
                branch_type=1,
                ras_action=0,
                queued_cycle=90,
                is_rvc=True,
                queue_index=6,
            ),
            ResolveEntry(
                ready_cycle=130,
                inst_pc=0x80004F3C,
                pc=0x80004F32,
                target=0x80004F2A,
                taken=True,
                mispredict=True,
                ftq_flag=0,
                ftq_value=13,
                ftq_offset=6,
                branch_type=1,
                ras_action=0,
                queued_cycle=90,
                is_rvc=True,
                queue_index=10,
            ),
        ]
    )
    bm.pending_events.extend(
        [
            BackendEvent(
                kind="redirect",
                ready_cycle=140,
                payload={
                    "target_pc": 0x80004F32,
                    "reason": "next-target-redirect",
                    "ftq_flag": 0,
                    "ftq_value": 12,
                    "ftq_offset": 2,
                    "queued_cycle": 90,
                },
            ),
            BackendEvent(
                kind="redirect",
                ready_cycle=140,
                payload={
                    "target_pc": 0x80004F2A,
                    "reason": "wrong-path-redirect",
                    "ftq_flag": 0,
                    "ftq_value": 13,
                    "ftq_offset": 6,
                    "queued_cycle": 90,
                },
            ),
            BackendEvent(
                kind="redirect",
                ready_cycle=140,
                payload={
                    "target_pc": 0x80004F32,
                    "reason": "golden_first_mismatch_redirect",
                    "ftq_flag": 0,
                    "ftq_value": 12,
                    "ftq_offset": 3,
                    "queued_cycle": 90,
                },
            ),
        ]
    )

    bm._drive_redirect(
        {
            "target_pc": 0x80004F22,
            "reason": "golden_resolve_redirect",
            "flush_on_drive": True,
            "pc": 0x8000313E,
            "taken": 1,
            "ftq_flag": 0,
            "ftq_value": 11,
            "ftq_offset": 8,
            "branch_type": 2,
            "ras_action": 2,
            "level": 0,
        }
    )

    assert bm._pending_level0_target_ftq == (0, 12)
    assert [(entry.ftq_flag, entry.ftq_value) for entry in bm._pending_resolves] == [(0, 12)]
    assert any(
        (
            int(evt.payload.get("ftq_flag", -1)),
            int(evt.payload.get("ftq_value", -1)),
        )
        == (0, 12)
        for evt in bm.pending_events
    )
    assert all("mismatch" not in str(evt.payload.get("reason", "")) for evt in bm.pending_events)
    assert all(
        (
            int(evt.payload.get("ftq_flag", -1)),
            int(evt.payload.get("ftq_value", -1)),
        )
        != (0, 13)
        for evt in bm.pending_events
    )


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
    assert [(entry.pc, entry.path_state) for entry in bm._cfvec_queue] == [
        (0x800048FE, "correct"),
        (0x80004902, "correct"),
        (0x80004904, "correct"),
    ]
    bm._pending_level0_target_ftq = (1, 49)

    _clear_cfvec(dut)
    _drive_cfvec_slot(dut, 0, valid=1, pc=0x8000491C, instr=0xF2E6E6E3, is_rvc=0, pred_taken=0, ftq_flag=1, ftq_value=49, ftq_offset=16, is_last=1)

    bm.current_cycle = 1
    bm._sample_cfvec()

    assert [(entry.pc, entry.path_state, entry.is_last_in_entry) for entry in bm._cfvec_queue] == [
        (0x800048FE, "correct", False),
        (0x80004902, "correct", False),
        (0x80004904, "correct", True),
        (0x8000491C, "wrong", True),
    ]
    assert len(bm._pending_resolves) == 1
    assert bm._pending_resolves[0].inst_pc == 0x80004904
    assert bm.ftq_entries[0].total_cfi == 1


def test_level0_redirect_keeps_waiting_if_next_target_slot_was_seen_only_on_wrong_path():
    dut = _DummyDut()
    bm = BackendModel()
    bm.bind(dut)
    bm.commit_count = 64
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 34
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
    bm.commit_ptr_value = 34
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


def test_stale_cfvec_slot_older_than_commit_ptr_is_tracked_as_recovery_residual() -> None:
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

    assert bm._current_ftq_entry is not None
    assert (bm._current_ftq_entry.ftq_flag, bm._current_ftq_entry.ftq_value) == (1, 54)
    assert list(bm.ftq_entries) == []
    assert [(entry.pc, entry.path_state) for entry in bm._cfvec_queue] == [(0x80004A82, "wrong")]
    assert bm._ftq_group_pc_history.get((1, 54)) == [(0x80004A82, True)]


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


def test_plan_redirect_payload_fails_fast_on_stale_drive_ftq_context():
    bm = BackendModel()
    bm.commit_count = 10
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 4

    import pytest

    with pytest.raises(AssertionError, match="redirect drive selected stale ftq context"):
        bm._plan_redirect_payload(
            {
                "target_pc": 0x80000028,
                "reason": "golden_first_mismatch_redirect",
                "flush_on_drive": True,
                "pc": 0x80000020,
                "taken": 1,
                "ftq_flag": 0,
                "ftq_value": 2,
                "ftq_offset": 0,
                "branch_type": 1,
                "ras_action": 0,
                "level": 0,
            }
        )


def test_mismatch_redirect_payload_keeps_original_drive_context() -> None:
    bm = BackendModel()
    bm.commit_count = 64
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 3
    bm._ftq_group_pc_history[(0, 5)] = [
        (0x80000044, False),
        (0x80000046, False),
        (0x8000004A, False),
    ]
    bm._pc_group_occurrences[0x80000044] = [(0, 5, 1)]

    payload = bm._plan_redirect_payload(
        {
            "target_pc": 0x80000044,
            "reason": "golden_first_mismatch_redirect",
            "flush_on_drive": True,
            "pc": 0x8000004A,
            "taken": 1,
            "ftq_flag": 0,
            "ftq_value": 4,
            "ftq_offset": 10,
            "branch_type": 1,
            "ras_action": 0,
            "level": 0,
        }
    )

    assert payload["pc"] == 0x8000004A
    assert payload["ftq_flag"] == 0
    assert payload["ftq_value"] == 4
    assert payload["ftq_offset"] == 10


def test_mismatch_redirect_uses_matched_golden_target_instead_of_drifted_cursor() -> None:
    bm = BackendModel()
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=132, pc=0x800000F2, instr=0x00039463, size=4, kind="branch", taken=True, target_pc=0x800000FA),
                TraceEntry(index=133, pc=0x800000FA, instr=0x00001479, size=2, kind="normal", taken=False, target_pc=None),
                TraceEntry(index=139, pc=0x80000116, instr=0x00001479, size=2, kind="normal", taken=False, target_pc=None),
            ]
        )
    )
    bm.golden_trace.cursor = 2
    bm._cfvec_queue.extend(
        [
            QueueInstr(
                cycle=0,
                slot=0,
                pc=0x800000EE,
                instr=0x0022F393,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=35,
                ftq_offset=2,
                is_last_in_entry=False,
                path_state="correct",
                golden_match_state="matched",
                golden_index=131,
            ),
            QueueInstr(
                cycle=0,
                slot=1,
                pc=0x800000F2,
                instr=0x00039463,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=35,
                ftq_offset=4,
                is_last_in_entry=False,
                path_state="correct",
                golden_match_state="matched",
                golden_index=132,
                golden_target_pc=0x800000FA,
                is_cfi=True,
            ),
            QueueInstr(
                cycle=0,
                slot=2,
                pc=0x800000F6,
                instr=0x00001415,
                is_rvc=True,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=35,
                ftq_offset=5,
                is_last_in_entry=True,
                path_state="unknown",
            ),
        ]
    )

    bm._cfvec_queue_note_mismatch(2)

    queued_redirects = [evt for evt in bm.pending_events if evt.kind == "redirect"]
    assert len(queued_redirects) == 1
    payload = queued_redirects[0].payload
    assert int(payload["pc"]) == 0x800000F2
    assert int(payload["target_pc"]) == 0x800000FA


def test_preceding_correct_cfi_search_does_not_cross_wrong_path_barrier() -> None:
    bm = BackendModel()
    bm._cfvec_queue.extend(
        [
            QueueInstr(
                cycle=0,
                slot=0,
                pc=0x80000112,
                instr=0x000E0463,
                is_rvc=False,
                pred_taken=True,
                ftq_flag=1,
                ftq_value=12,
                ftq_offset=3,
                is_last_in_entry=False,
                path_state="correct",
                is_cfi=True,
            ),
            QueueInstr(
                cycle=0,
                slot=1,
                pc=0x80000116,
                instr=0x00004505,
                is_rvc=True,
                pred_taken=False,
                ftq_flag=1,
                ftq_value=13,
                ftq_offset=0,
                is_last_in_entry=True,
                path_state="wrong",
            ),
            QueueInstr(
                cycle=0,
                slot=2,
                pc=0x800000f6,
                instr=0x00001415,
                is_rvc=True,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=35,
                ftq_offset=5,
                is_last_in_entry=False,
                path_state="correct",
            ),
        ]
    )

    assert bm._find_preceding_correct_path_cfi(2) is None
    assert bm._has_wrong_path_barrier_before(2) is True


def test_plan_redirect_payload_immediately_recovers_when_target_is_already_queued() -> None:
    bm = BackendModel()
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x8000001A, instr=0x0000147D, size=2, kind="normal", taken=False, target_pc=None),
                TraceEntry(index=1, pc=0x8000001C, instr=0x0012F393, size=4, kind="normal", taken=False, target_pc=None),
                TraceEntry(index=2, pc=0x80000020, instr=0x00038463, size=4, kind="branch", taken=True, target_pc=0x80000028),
            ]
        )
    )
    bm._cfvec_queue.extend(
        [
            QueueInstr(
                cycle=0,
                slot=0,
                pc=0x80000010,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=0,
                ftq_offset=0,
                is_last_in_entry=True,
                path_state="correct",
            ),
            QueueInstr(
                cycle=0,
                slot=1,
                pc=0x8000001A,
                instr=0x0000147D,
                is_rvc=True,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=1,
                ftq_offset=0,
                is_last_in_entry=False,
                path_state="correct",
            ),
            QueueInstr(
                cycle=0,
                slot=2,
                pc=0x8000001C,
                instr=0x0012F393,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=1,
                ftq_offset=1,
                is_last_in_entry=False,
                path_state="correct",
            ),
        ]
    )

    payload = bm._plan_redirect_payload(
        {
            "target_pc": 0x8000001A,
            "reason": "golden_first_mismatch_redirect",
            "flush_on_drive": True,
            "pc": 0x80000012,
            "taken": 1,
            "ftq_flag": 0,
            "ftq_value": 0,
            "ftq_offset": 6,
            "branch_type": 1,
            "ras_action": 0,
            "level": 0,
        }
    )

    assert payload["target_pc"] == 0x8000001A
    assert [int(entry.pc) for entry in bm._cfvec_queue] == [0x8000001A, 0x8000001C]
    assert bm._cfvec_queue[0].golden_match_state == "matched"
    assert bm._cfvec_queue[1].golden_match_state == "matched"
    assert bm._cfvec_queue[0].path_state == "correct"
    assert bm._cfvec_queue[1].path_state == "correct"
    assert bm._golden_wait_pc is None
    assert bm._golden_wait_requires_redirect is False
    assert bm._semantic_recovery_target_pc is None
    assert bm._semantic_recovery_queue_start is None


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


def test_level0_redirect_does_not_keep_waiting_when_target_pc_is_already_visible_on_correct_path() -> None:
    bm = BackendModel()
    bm.commit_count = 64
    bm.commit_ptr_flag = 1
    bm.commit_ptr_value = 63
    bm._cfvec_queue.extend(
        [
            QueueInstr(
                cycle=0,
                slot=0,
                pc=0x8000001A,
                instr=0x0000147D,
                is_rvc=True,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=1,
                ftq_offset=0,
                is_last_in_entry=False,
                path_state="correct",
            ),
            QueueInstr(
                cycle=0,
                slot=1,
                pc=0x8000001C,
                instr=0x000012F393,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=1,
                ftq_offset=1,
                is_last_in_entry=False,
                path_state="correct",
            ),
        ]
    )

    payload = bm._plan_redirect_payload(
        {
            "target_pc": 0x8000001A,
            "reason": "golden_first_mismatch_redirect",
            "flush_on_drive": True,
            "pc": 0x80000018,
            "taken": 0,
            "ftq_flag": 0,
            "ftq_value": 0,
            "ftq_offset": 6,
            "branch_type": 1,
            "ras_action": 0,
            "level": 0,
        }
    )

    assert payload["target_pc"] == 0x8000001A
    assert bm._pending_level0_target_ftq is None


def test_clear_stale_wait_states_drops_wrapped_pending_level0_target_when_target_absent() -> None:
    bm = BackendModel()
    bm.commit_count = 64
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 10
    bm._pending_level0_target_ftq = (1, 10)

    bm._clear_stale_wait_states()

    assert bm._pending_level0_target_ftq is None


def test_clear_stale_wait_states_clears_semantic_recovery_without_pending_redirect_or_wrong_path() -> None:
    bm = BackendModel()
    bm._semantic_recovery_target_pc = 0x80000080
    bm._semantic_recovery_queue_start = 7
    bm._pending_redirect_origin_index = None
    bm.pending_events.clear()
    bm._cfvec_queue.append(
        QueueInstr(
            cycle=0,
            slot=0,
            pc=0x80000080,
            instr=0x00000013,
            is_rvc=False,
            pred_taken=False,
            ftq_flag=0,
            ftq_value=1,
            ftq_offset=0,
            is_last_in_entry=True,
            path_state="correct",
        )
    )

    bm._clear_stale_wait_states()

    assert bm._semantic_recovery_target_pc is None
    assert bm._semantic_recovery_queue_start is None


def test_clear_stale_wait_states_clears_orphan_golden_wait_and_flushes_wrong_path_queue() -> None:
    bm = BackendModel()
    bm._golden_wait_pc = 0x80000080
    bm._golden_wait_requires_redirect = True
    bm._pending_level0_target_ftq = None
    bm._semantic_recovery_target_pc = None
    bm._pending_redirect_origin_index = None
    bm.pending_events.clear()
    bm.ftq_entries.clear()
    bm._current_ftq_entry = None
    bm._cfvec_queue.extend(
        [
            QueueInstr(
                cycle=0,
                slot=0,
                pc=0x80000040,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=2,
                ftq_offset=0,
                is_last_in_entry=False,
                path_state="wrong",
            ),
            QueueInstr(
                cycle=0,
                slot=1,
                pc=0x80000044,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=2,
                ftq_offset=2,
                is_last_in_entry=True,
                path_state="wrong",
            ),
        ]
    )

    bm._clear_stale_wait_states()

    assert bm._golden_wait_pc is None
    assert bm._golden_wait_requires_redirect is False
    assert len(bm._cfvec_queue) == 0


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
    assert bm.commit_count == 252
    assert list((entry.ftq_flag, entry.ftq_value) for entry in bm.ftq_entries) == []


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


def test_cfvec_queue_match_does_not_immediately_mark_instruction_committed() -> None:
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
    bm.resolve_min_delay = 0
    bm.resolve_max_delay = 0

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

    assert bm._cfvec_queue[0].golden_match_state == "matched"
    assert bm._cfvec_queue[0].path_state == "correct"
    assert bm._cfvec_queue[0].rob_commit_state == "pending"
    assert int(dut.io_backend_toFtq_callRetCommit_0_valid.value) == 0


def test_callret_commit_becomes_visible_one_cycle_after_simulated_instruction_commit() -> None:
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
    bm.resolve_min_delay = 3
    bm.resolve_max_delay = 3

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

    assert bm._cfvec_queue[0].rob_commit_state == "pending"
    assert int(dut.io_backend_toFtq_callRetCommit_0_valid.value) == 0

    _clear_cfvec(dut)
    bm.on_clock_edge(1)

    assert bm._cfvec_queue[0].rob_commit_state == "pending"
    assert int(dut.io_backend_toFtq_callRetCommit_0_valid.value) == 0

    bm.on_clock_edge(2)

    assert bm._cfvec_queue[0].rob_commit_state == "pending"
    assert int(dut.io_backend_toFtq_callRetCommit_0_valid.value) == 0

    bm.on_clock_edge(3)

    assert bm._cfvec_queue[0].rob_commit_state == "committed"
    assert bm._cfvec_queue[0].call_ret_commit_state == "pending"
    assert int(dut.io_backend_toFtq_callRetCommit_0_valid.value) == 0

    bm.on_clock_edge(4)

    assert int(dut.io_backend_toFtq_callRetCommit_0_valid.value) == 1
    assert int(dut.io_backend_toFtq_callRetCommit_0_bits_rasAction.value) == 2
    assert int(dut.io_backend_toFtq_callRetCommit_0_bits_ftqPtr_value.value) == 5


def test_cfvec_queue_callret_tracks_per_instruction_commit_frontier() -> None:
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
    bm.resolve_min_delay = 3
    bm.resolve_max_delay = 3

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

    assert bm._cfvec_queue[0].rob_commit_state == "pending"
    assert bm._cfvec_queue[0].call_ret_commit_state == "none"
    assert list(bm._pending_queue_call_ret_commit_indices) == []
    assert list(bm._scheduled_queue_call_ret_commit_groups) == []

    _clear_cfvec(dut)
    bm.on_clock_edge(1)

    assert bm._cfvec_queue[0].rob_commit_state == "pending"
    assert bm._cfvec_queue[0].call_ret_commit_state == "none"
    assert list(bm._scheduled_queue_call_ret_commit_groups) == []

    bm.on_clock_edge(2)

    assert bm._cfvec_queue[0].rob_commit_state == "pending"
    assert bm._cfvec_queue[0].call_ret_commit_state == "none"
    assert list(bm._scheduled_queue_call_ret_commit_groups) == []

    bm.on_clock_edge(3)

    assert bm._cfvec_queue[0].rob_commit_state == "committed"
    assert bm._cfvec_queue[0].call_ret_commit_state == "pending"
    assert list(bm._pending_queue_call_ret_commit_indices) == []
    scheduled_groups = list(bm._scheduled_queue_call_ret_commit_groups)
    assert len(scheduled_groups) == 1
    assert scheduled_groups[0][0] == 4
    assert len(scheduled_groups[0][1]) == 1
    assert scheduled_groups[0][1][0].queue_index == 0

    bm.on_clock_edge(4)

    assert bm._cfvec_queue[0].call_ret_commit_state == "emitted"
    assert int(dut.io_backend_toFtq_callRetCommit_0_valid.value) == 1
    assert int(dut.io_backend_toFtq_callRetCommit_0_bits_rasAction.value) == 2
    assert int(dut.io_backend_toFtq_callRetCommit_0_bits_ftqPtr_value.value) == 9


def test_cfvec_queue_commit_pops_only_queue_head_ftq_entry() -> None:
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
    assert bm._plan_instruction_commits_for_cycle() == 2

    commit_entry = bm._plan_commit_entry_for_cycle()

    assert commit_entry is not None
    assert (commit_entry.ftq_flag, commit_entry.ftq_value) == (0, 10)
    assert [(entry.pc, entry.ftq_value) for entry in bm._cfvec_queue] == [(0x80000204, 11)]
    assert [(entry.ftq_flag, entry.ftq_value) for entry in bm.ftq_entries] == [(0, 11)]
    assert (bm.commit_ptr_flag, bm.commit_ptr_value) == (0, 10)
    assert bm.commit_count == 1


def test_callret_emitted_mark_uses_ftq_offset_identity_after_pop_head() -> None:
    bm = BackendModel()
    bm._cfvec_queue.extend(
        [
            QueueInstr(
                cycle=0,
                slot=0,
                pc=0x80001000,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=10,
                ftq_offset=0,
                is_last_in_entry=True,
            ),
            QueueInstr(
                cycle=0,
                slot=1,
                pc=0x80001004,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=11,
                ftq_offset=0,
                is_last_in_entry=True,
            ),
        ]
    )
    bm._scheduled_queue_call_ret_commit_groups.append(
        (
            1,
            [
                CommitInstruction(
                    json_index=0,
                    pc=0x80001000,
                    instr=0x00000013,
                    ftq_flag=0,
                    ftq_value=10,
                    ras_action=0,
                    queue_index=0,
                    ftq_offset=0,
                )
            ],
        )
    )

    bm._cfvec_queue_pop_head(1)
    bm.current_cycle = 1
    bm._activate_visible_queue_call_ret_commit_group()

    assert [inst.ftq_value for inst in bm._visible_queue_call_ret_commit_group] == [10]
    assert len(bm._cfvec_queue) == 1
    assert int(bm._cfvec_queue[0].ftq_value) == 11
    assert bm._cfvec_queue[0].call_ret_commit_state == "none"


def test_cfvec_queue_pop_head_recomputes_wrong_path_origin_when_suffix_survives() -> None:
    bm = BackendModel()
    bm._cfvec_queue.extend(
        [
            QueueInstr(
                cycle=0,
                slot=0,
                pc=0x80002000,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=20,
                ftq_offset=0,
                is_last_in_entry=True,
                path_state="correct",
            ),
            QueueInstr(
                cycle=0,
                slot=1,
                pc=0x80002004,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=21,
                ftq_offset=0,
                is_last_in_entry=False,
                path_state="wrong",
            ),
            QueueInstr(
                cycle=0,
                slot=2,
                pc=0x80002008,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=21,
                ftq_offset=2,
                is_last_in_entry=True,
                path_state="wrong",
            ),
        ]
    )
    bm._pending_redirect_origin_index = 1
    bm.pending_events.append(
        BackendEvent(
            kind="redirect",
            ready_cycle=10,
            payload={"target_pc": 0x80002010, "reason": "unit_pending_redirect"},
        )
    )

    bm._cfvec_queue_pop_head(2)

    assert len(bm._cfvec_queue) == 1
    assert bm._cfvec_queue[0].path_state == "wrong"
    assert bm._pending_redirect_origin_index == 0


def test_cfvec_queue_remove_range_does_not_rebuild_origin_without_pending_redirect() -> None:
    bm = BackendModel()
    bm._cfvec_queue.extend(
        [
            QueueInstr(
                cycle=0,
                slot=0,
                pc=0x80002100,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=22,
                ftq_offset=0,
                is_last_in_entry=True,
                path_state="correct",
            ),
            QueueInstr(
                cycle=0,
                slot=1,
                pc=0x80002104,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=23,
                ftq_offset=0,
                is_last_in_entry=False,
                path_state="wrong",
            ),
            QueueInstr(
                cycle=0,
                slot=2,
                pc=0x80002108,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=23,
                ftq_offset=2,
                is_last_in_entry=True,
                path_state="wrong",
            ),
        ]
    )
    bm._pending_redirect_origin_index = None

    bm._cfvec_queue_remove_range(0, 1)

    assert len(bm._cfvec_queue) == 2
    assert all(entry.path_state == "wrong" for entry in bm._cfvec_queue)
    assert bm._pending_redirect_origin_index is None


def test_cfvec_queue_flush_wrong_path_falls_back_to_first_wrong_when_origin_missing() -> None:
    bm = BackendModel()
    bm._cfvec_queue.extend(
        [
            QueueInstr(
                cycle=0,
                slot=0,
                pc=0x80003000,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=30,
                ftq_offset=0,
                is_last_in_entry=True,
                path_state="correct",
            ),
            QueueInstr(
                cycle=0,
                slot=1,
                pc=0x80003004,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=31,
                ftq_offset=0,
                is_last_in_entry=False,
                path_state="wrong",
            ),
            QueueInstr(
                cycle=0,
                slot=2,
                pc=0x80003008,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=31,
                ftq_offset=2,
                is_last_in_entry=True,
                path_state="wrong",
            ),
        ]
    )
    bm._pending_redirect_origin_index = None

    bm._cfvec_queue_flush_wrong_path()

    assert [(entry.pc, entry.path_state) for entry in bm._cfvec_queue] == [(0x80003000, "correct")]
    assert bm._pending_redirect_origin_index is None


def test_cfvec_queue_commit_waits_for_correct_path_cfi_resolve() -> None:
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
    assert bm._plan_instruction_commits_for_cycle() == 1
    commit_entry = bm._plan_commit_entry_for_cycle()

    assert commit_entry is not None
    assert (commit_entry.ftq_flag, commit_entry.ftq_value) == (0, 12)
    assert list(bm._cfvec_queue) == []


def test_cfvec_queue_stale_head_equal_to_commit_ptr_is_dropped() -> None:
    bm = BackendModel()
    bm.set_golden_trace(
        GoldenTrace([TraceEntry(index=0, pc=0x80000000, instr=0x00000013, size=4)])
    )
    bm.commit_count = 10
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 14
    bm.current_cycle = 100
    bm._cfvec_queue.append(
        QueueInstr(
            cycle=90,
            slot=0,
            pc=0x80000000,
            instr=0x00000013,
            is_rvc=False,
            pred_taken=False,
            ftq_flag=0,
            ftq_value=14,
            ftq_offset=0,
            is_last_in_entry=True,
            path_state="correct",
            resolve_state="not_needed",
            rob_commit_state="committed",
            golden_match_state="matched",
            is_cfi=False,
        )
    )

    commit_entry = bm._plan_commit_entry_for_cycle()

    assert commit_entry is None
    assert len(bm._cfvec_queue) == 0




def test_orphan_missing_expected_ftq_is_pruned_and_does_not_block_commit_head() -> None:
    bm = BackendModel()
    bm.set_golden_trace(GoldenTrace([TraceEntry(index=0, pc=0x80000000, instr=0x00000013, size=4)]))
    bm.current_cycle = 100
    bm.commit_count = 1
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 13
    bm._cfvec_queue.extend(
        [
            QueueInstr(
                cycle=90,
                slot=0,
                pc=0x80000040,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=15,
                ftq_offset=0,
                is_last_in_entry=False,
                path_state="correct",
                resolve_state="not_needed",
                rob_commit_state="committed",
                golden_match_state="matched",
                is_cfi=False,
            ),
            QueueInstr(
                cycle=90,
                slot=1,
                pc=0x80000044,
                instr=0x00000013,
                is_rvc=False,
                pred_taken=False,
                ftq_flag=0,
                ftq_value=15,
                ftq_offset=2,
                is_last_in_entry=True,
                path_state="correct",
                resolve_state="not_needed",
                rob_commit_state="committed",
                golden_match_state="matched",
                is_cfi=False,
            ),
        ]
    )
    bm._commit_queue.extend([0, 1])
    bm.ftq_entries.extend(
        [
            FtqEntry(
                ftq_flag=0,
                ftq_value=14,
                total_cfi=0,
                resolved_cfi=0,
                dispatch_complete=True,
                commit_ready_cycle=0,
            ),
            FtqEntry(
                ftq_flag=0,
                ftq_value=15,
                total_cfi=0,
                resolved_cfi=0,
                dispatch_complete=True,
                commit_ready_cycle=0,
            ),
        ]
    )

    commit_entry = bm._plan_commit_entry_for_cycle()

    assert commit_entry is not None
    assert int(commit_entry.ftq_flag) == 0
    assert int(commit_entry.ftq_value) == 15
    assert all((int(entry.ftq_flag), int(entry.ftq_value)) != (0, 14) for entry in bm.ftq_entries)


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


def test_semantic_recovery_window_blocks_fallback_commit_when_queue_empty() -> None:
    bm = BackendModel()
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000000, instr=0x00000013, size=4),
            ]
        )
    )
    bm.current_cycle = 10
    bm._semantic_recovery_target_pc = 0x80000000
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

    assert commit_entry is None
    assert bm.commit_count == 0
    assert len(bm.ftq_entries) == 1


def test_wrong_path_only_cfvec_queue_does_not_block_fallback_commit() -> None:
    bm = BackendModel()
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000000, instr=0x00000013, size=4),
            ]
        )
    )
    bm.current_cycle = 50
    bm.commit_count = 1
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 12
    bm._cfvec_queue.append(
        QueueInstr(
            cycle=49,
            slot=0,
            pc=0x80000040,
            instr=0x00000013,
            is_rvc=False,
            pred_taken=False,
            ftq_flag=0,
            ftq_value=14,
            ftq_offset=0,
            is_last_in_entry=True,
            path_state="wrong",
        )
    )
    bm.ftq_entries.append(
        FtqEntry(
            ftq_flag=0,
            ftq_value=13,
            total_cfi=0,
            resolved_cfi=0,
            dispatch_complete=True,
            commit_ready_cycle=0,
        )
    )

    commit_entry = bm._plan_commit_entry_for_cycle()

    assert commit_entry is not None
    assert (int(commit_entry.ftq_flag), int(commit_entry.ftq_value)) == (0, 13)
    assert (int(bm.commit_ptr_flag), int(bm.commit_ptr_value)) == (0, 13)


def test_pending_older_redirect_blocks_younger_fallback_commit() -> None:
    bm = BackendModel()
    bm.current_cycle = 100
    bm.commit_count = 1
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 0
    bm.ftq_entries.extend(
        [
            FtqEntry(
                ftq_flag=0,
                ftq_value=1,
                total_cfi=0,
                resolved_cfi=0,
                dispatch_complete=True,
                commit_ready_cycle=0,
            ),
            FtqEntry(
                ftq_flag=0,
                ftq_value=2,
                total_cfi=0,
                resolved_cfi=0,
                dispatch_complete=True,
                commit_ready_cycle=0,
            ),
        ]
    )
    bm.pending_events.append(
        BackendEvent(
            kind="redirect",
            ready_cycle=120,
            payload={
                "target_pc": 0x80000028,
                "reason": "golden_first_mismatch_redirect",
                "ftq_flag": 0,
                "ftq_value": 1,
            },
        )
    )

    commit_entry = bm._plan_commit_entry_for_cycle()

    assert commit_entry is None
    assert (int(bm.commit_ptr_flag), int(bm.commit_ptr_value)) == (0, 0)


def test_waiting_target_group_cfi_prefix_queues_new_mismatch_redirect() -> None:
    dut = _DummyDut()
    bm = BackendModel(resolve_min_delay=0, resolve_max_delay=0)
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x80000028, instr=0x0000147D, size=2, kind="normal", taken=False, target_pc=None),
            ]
        )
    )
    bm.current_cycle = 100
    bm._golden_wait_pc = 0x80000028
    bm._golden_wait_requires_redirect = True
    bm._pending_level0_target_ftq = (0, 1)

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x80000020,
        instr=0x00038463,
        pred_taken=1,
        ftq_flag=0,
        ftq_value=1,
        ftq_offset=8,
        is_last=0,
    )

    bm._sample_cfvec()

    reasons = [str(evt.payload.get("reason", "")) for evt in bm.pending_events if evt.kind == "redirect"]
    assert "golden_first_mismatch_redirect" in reasons


def test_same_cycle_wrong_path_slot_after_golden_wait_still_samples_and_queues_redirect() -> None:
    dut = _DummyDut()
    bm = BackendModel(resolve_min_delay=0, resolve_max_delay=0, redirect_min_delay=0, redirect_max_delay=0)
    bm.bind(dut)
    bm.set_golden_trace(
        GoldenTrace(
            [
                TraceEntry(index=0, pc=0x8000001A, instr=0x0000147D, size=2, kind="normal", taken=False, target_pc=None),
                TraceEntry(index=1, pc=0x8000001C, instr=0x0012F393, size=4, kind="normal", taken=False, target_pc=None),
                TraceEntry(index=2, pc=0x80000020, instr=0x00038463, size=4, kind="branch", taken=True, target_pc=0x80000028),
                TraceEntry(index=3, pc=0x80000028, instr=0x0000147D, size=2, kind="normal", taken=False, target_pc=None),
            ]
        )
    )

    _drive_cfvec_slot(
        dut,
        0,
        valid=1,
        pc=0x8000001A,
        instr=0x0000147D,
        is_rvc=1,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=1,
        ftq_offset=5,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        1,
        valid=1,
        pc=0x8000001C,
        instr=0x0012F393,
        is_rvc=0,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=1,
        ftq_offset=6,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        2,
        valid=1,
        pc=0x80000020,
        instr=0x00038463,
        is_rvc=0,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=1,
        ftq_offset=8,
        is_last=0,
    )
    _drive_cfvec_slot(
        dut,
        3,
        valid=1,
        pc=0x80000024,
        instr=0x00000013,
        is_rvc=0,
        pred_taken=0,
        ftq_flag=0,
        ftq_value=1,
        ftq_offset=10,
        is_last=1,
    )

    bm.current_cycle = 0
    bm._sample_cfvec()

    assert bm._golden_wait_pc == 0x80000028
    assert [(entry.pc, entry.path_state) for entry in bm._cfvec_queue] == [
        (0x8000001A, "correct"),
        (0x8000001C, "correct"),
        (0x80000020, "correct"),
        (0x80000024, "wrong"),
    ]
    queued_redirects = [evt for evt in bm.pending_events if evt.kind == "redirect"]
    assert any(str(evt.payload.get("reason", "")) == "golden_first_mismatch_redirect" for evt in queued_redirects)
    mismatch_redirect = next(
        evt for evt in queued_redirects if str(evt.payload.get("reason", "")) == "golden_first_mismatch_redirect"
    )
    assert int(mismatch_redirect.payload["pc"]) == 0x80000020
    assert int(mismatch_redirect.payload["target_pc"]) == 0x80000028


def test_ftq_entry_with_pending_redirect_debt_cannot_commit() -> None:
    bm = BackendModel()
    bm.current_cycle = 100
    bm.commit_count = 1
    bm.commit_ptr_flag = 0
    bm.commit_ptr_value = 0
    bm.ftq_entries.extend(
        [
            FtqEntry(
                ftq_flag=0,
                ftq_value=1,
                total_cfi=0,
                resolved_cfi=0,
                dispatch_complete=True,
                has_redirect=True,
                commit_ready_cycle=0,
            ),
            FtqEntry(
                ftq_flag=0,
                ftq_value=2,
                total_cfi=0,
                resolved_cfi=0,
                dispatch_complete=True,
                commit_ready_cycle=0,
            ),
        ]
    )

    commit_entry = bm._plan_commit_entry_for_cycle()

    assert commit_entry is None
    assert (int(bm.commit_ptr_flag), int(bm.commit_ptr_value)) == (0, 0)


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


def test_ready_redirect_does_not_wait_for_older_unready_redirect() -> None:
    bm = BackendModel()
    bm.current_cycle = 10
    bm.pending_events.extend(
        [
            BackendEvent(
                kind="redirect",
                ready_cycle=20,
                payload={"target_pc": 0x80000040, "reason": "older_redirect", "queued_cycle": 0},
            ),
            BackendEvent(
                kind="redirect",
                ready_cycle=10,
                payload={"target_pc": 0x80000080, "reason": "younger_ready_redirect", "queued_cycle": 1},
            ),
        ]
    )

    payload = bm._ready_redirect_for_cycle()

    assert payload is not None
    assert int(payload["target_pc"]) == 0x80000080
    assert [str(evt.payload.get("reason", "")) for evt in bm.pending_events] == ["older_redirect"]


def test_ready_exception_preempts_older_unready_redirect() -> None:
    bm = BackendModel()
    bm.current_cycle = 10
    bm.pending_events.extend(
        [
            BackendEvent(
                kind="redirect",
                ready_cycle=20,
                payload={"target_pc": 0x80000040, "reason": "older_redirect", "queued_cycle": 0},
            ),
            BackendEvent(
                kind="exception",
                ready_cycle=10,
                payload={"target_pc": 0x80000100, "reason": "exception", "cause": 12, "tval": 0},
            ),
        ]
    )

    payload = bm._ready_redirect_for_cycle()

    assert payload is not None
    assert int(payload["target_pc"]) == 0x80000100
    assert [evt.kind for evt in bm.pending_events] == ["redirect"]


def test_ready_redirect_is_not_dropped_when_target_pc_was_observed() -> None:
    bm = BackendModel()
    bm.current_cycle = 10

    class _Monitor:
        def __init__(self):
            self.observations = [
                Observation(cycle=8, slot=0, pc=0x80000040, instr=0x13, is_rvc=False, pred_taken=False),
                Observation(cycle=10, slot=0, pc=0x80000080, instr=0x13, is_rvc=False, pred_taken=False),
            ]
            self.redirects = []

        def notify_redirect(self, target_pc, reason="redirect"):
            self.redirects.append((int(target_pc), str(reason)))

    monitor = _Monitor()
    bm.attach_monitor(monitor)
    bm.pending_events.append(
        BackendEvent(
            kind="redirect",
            ready_cycle=10,
            payload={"target_pc": 0x80000080, "reason": "unit_redirect", "queued_cycle": 8, "flush_on_drive": True},
        )
    )

    payload = bm._ready_redirect_for_cycle()

    assert payload is not None
    assert int(payload["target_pc"]) == 0x80000080
    assert len(bm.pending_events) == 0
    assert monitor.redirects == [(0x80000080, "unit_redirect")]


def test_unready_exception_does_not_block_commit_entry() -> None:
    bm = BackendModel()
    bm.current_cycle = 10
    bm.ftq_entries.append(
        FtqEntry(
            ftq_flag=0,
            ftq_value=1,
            total_cfi=0,
            resolved_cfi=0,
            dispatch_complete=True,
            commit_ready_cycle=0,
        )
    )
    bm.pending_events.append(
        BackendEvent(
            kind="exception",
            ready_cycle=20,
            payload={"target_pc": 0x80000100, "reason": "exception", "cause": 12, "tval": 0},
        )
    )

    commit_entry = bm._plan_commit_entry_for_cycle()

    assert commit_entry is not None
    assert int(commit_entry.ftq_flag) == 0
    assert int(commit_entry.ftq_value) == 1


def test_ready_exception_does_not_block_commit_entry() -> None:
    bm = BackendModel()
    bm.current_cycle = 10
    bm.ftq_entries.append(
        FtqEntry(
            ftq_flag=0,
            ftq_value=1,
            total_cfi=0,
            resolved_cfi=0,
            dispatch_complete=True,
            commit_ready_cycle=0,
        )
    )
    bm.pending_events.append(
        BackendEvent(
            kind="exception",
            ready_cycle=10,
            payload={"target_pc": 0x80000100, "reason": "exception", "cause": 12, "tval": 0},
        )
    )

    commit_entry = bm._plan_commit_entry_for_cycle()

    assert commit_entry is not None
    assert int(commit_entry.ftq_flag) == 0
    assert int(commit_entry.ftq_value) == 1


def test_commit_queue_fail_fast_on_wrong_path_entry() -> None:
    bm = BackendModel()
    bm.current_cycle = 10
    bm._cfvec_queue.append(
        QueueInstr(
            cycle=1,
            slot=0,
            pc=0x80000000,
            instr=0x00000013,
            is_rvc=False,
            pred_taken=False,
            ftq_flag=0,
            ftq_value=0,
            ftq_offset=0,
            is_last_in_entry=True,
            path_state="wrong",
        )
    )
    bm._commit_queue.append(0)

    import pytest

    with pytest.raises(AssertionError, match="commit_queue invariant violated: non-correct-path instruction"):
        bm._plan_instruction_commits_for_cycle()

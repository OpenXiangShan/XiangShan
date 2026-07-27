from __future__ import annotations

from collections import deque

import pytest

from env.backend_model import BackendModel
from env.model.backend_state import ActiveWrongPathEpisode
from env.model.backend_state import BackendEvent
from env.model.backend_state import ROB_COMMIT_STATE_COMMITTED
from env.model.backend_state import FtqEntry
from env.model.backend_state import PATH_STATE_CORRECT
from env.model.backend_state import PATH_STATE_WRONG
from env.model.backend_state import QueueInstr
from env.model.backend_state import GOLDEN_MATCH_STATE_UNKNOWN
from env.model.backend_state import RESOLVE_STATE_NOT_NEEDED
from env.trace import GoldenTrace
from env.trace import TraceEntry


class _Signal:
    def __init__(self, value: int = 0) -> None:
        self.value = int(value)


class _ObserveIf:
    def __init__(self) -> None:
        self.cfvec_valid = [_Signal() for _ in range(8)]
        self.cfvec_pc = [_Signal() for _ in range(8)]
        self.cfvec_instr = [_Signal(0x13) for _ in range(8)]
        self.cfvec_is_rvc = [_Signal() for _ in range(8)]
        self.cfvec_fixed_taken = [_Signal() for _ in range(8)]
        self.cfvec_ftq_ptr_flag = [_Signal() for _ in range(8)]
        self.cfvec_ftq_ptr_value = [_Signal() for _ in range(8)]
        self.cfvec_ftq_offset = [_Signal() for _ in range(8)]
        self.cfvec_is_last_in_ftq_entry = [_Signal() for _ in range(8)]
        self.cfvec_exception_vec = [[_Signal() for _ in range(24)] for _ in range(8)]


class _EmptyTrace:
    def peek(self):
        return None


def _set_first_cfvec(interface: _ObserveIf, pc: int, *, ftq_value: int = 0) -> None:
    interface.cfvec_valid[0].value = 1
    interface.cfvec_pc[0].value = int(pc)
    interface.cfvec_ftq_ptr_value[0].value = int(ftq_value)


def _queue_instr(pc: int, ftq_flag: int, ftq_value: int) -> QueueInstr:
    return QueueInstr(
        cycle=0,
        slot=0,
        pc=pc,
        instr=0,
        is_rvc=False,
        pred_taken=False,
        ftq_flag=ftq_flag,
        ftq_value=ftq_value,
        ftq_offset=0,
        is_last_in_entry=False,
    )


def test_format_queue_pc_ranges_keeps_adjacent_ftq_segments_distinct() -> None:
    entries = [
        _queue_instr(0x1000, 0, 14),
        _queue_instr(0x1002, 0, 14),
        _queue_instr(0x1004, 0, 16),
        _queue_instr(0x1006, 0, 16),
    ]

    assert BackendModel._format_queue_pc_ranges(entries) == (
        "0x1000-0x1002(0,14),0x1004-0x1006(0,16)"
    )


def test_recovery_target_requires_matching_pc_and_ftq() -> None:
    model = BackendModel()
    model._active_wrong_path_episode_state = ActiveWrongPathEpisode(
        origin_index=0,
        target_pc=0x1000,
        redirect_context=None,
        redirect_driven=True,
        expected_recovery_ftq=(0, 14),
        redirect_driven_cycle=10,
    )

    assert model._queue_entry_matches_recovery_target(
        _queue_instr(0x1000, 0, 14),
        0x1000,
    ) is False

    matching_entry = _queue_instr(0x1000, 0, 14)
    matching_entry.cycle = 11
    assert model._queue_entry_matches_recovery_target(matching_entry, 0x1000) is True

    wrong_ftq_entry = _queue_instr(0x1000, 0, 15)
    wrong_ftq_entry.cycle = 11
    assert model._queue_entry_matches_recovery_target(wrong_ftq_entry, 0x1000) is False


def test_recovery_target_requires_expected_recovery_ftq() -> None:
    model = BackendModel()
    model._active_wrong_path_episode_state = ActiveWrongPathEpisode(
        origin_index=0,
        target_pc=0x1000,
        redirect_context=None,
        redirect_driven=True,
        expected_recovery_ftq=None,
        redirect_driven_cycle=10,
    )
    entry = _queue_instr(0x1000, 0, 14)
    entry.cycle = 11

    assert model._queue_entry_matches_recovery_target(entry, 0x1000) is False


def test_ready_redirect_skips_cfvec_queue_sampling_at_t_and_t_plus_one() -> None:
    model = BackendModel()
    interface = _ObserveIf()
    model.observe_if = interface
    model.current_cycle = 10
    model.pending_events.append(
        BackendEvent(
            kind="redirect",
            ready_cycle=10,
            payload={
                "target_pc": 0x2000,
                "reason": "unit_redirect",
                "pc": 0x1000,
                "taken": 1,
                "ftq_flag": 0,
                "ftq_value": 0,
                "ftq_offset": 0,
                "is_rvc": 0,
                "flush_on_drive": False,
            },
        )
    )

    _set_first_cfvec(interface, 0x1004)
    actions = model.plan_cycle_actions()

    assert actions.redirect_payload is not None
    assert list(model._cfvec_queue) == []

    model.current_cycle = 11
    _set_first_cfvec(interface, 0x1008)
    model.plan_cycle_actions()

    assert list(model._cfvec_queue) == []

    model.current_cycle = 12
    _set_first_cfvec(interface, 0x2000, ftq_value=1)
    model.plan_cycle_actions()

    assert [entry.pc for entry in model._cfvec_queue] == [0x2000]


def test_recovery_first_sampled_cfvec_must_be_target_after_skip_window() -> None:
    model = BackendModel()
    interface = _ObserveIf()
    model.observe_if = interface
    model.current_cycle = 12
    model._active_wrong_path_episode_state = ActiveWrongPathEpisode(
        origin_index=0,
        target_pc=0x2000,
        redirect_context=None,
        redirect_driven=True,
        expected_recovery_ftq=(0, 1),
        redirect_driven_cycle=10,
    )
    _set_first_cfvec(interface, 0x2004, ftq_value=1)

    with pytest.raises(AssertionError, match="redirect recovery first cfvec is not target"):
        model._sample_cfvec()

    assert list(model._cfvec_queue) == []


def test_exception_marked_cfvec_is_queued_without_normal_backend_actions() -> None:
    model = BackendModel()
    interface = _ObserveIf()
    model.observe_if = interface
    model.current_cycle = 20
    model.golden_trace = _EmptyTrace()

    _set_first_cfvec(interface, 0x80003248, ftq_value=3)
    interface.cfvec_instr[0].value = 0x05130000
    interface.cfvec_is_rvc[0].value = 1
    interface.cfvec_fixed_taken[0].value = 1
    interface.cfvec_exception_vec[0][2].value = 1

    model._sample_cfvec()

    assert len(model._cfvec_queue) == 1
    entry = model._cfvec_queue[0]
    assert entry.exception_marked is True
    assert entry.exception_bits == (1 << 2)
    assert entry.is_cfi is False
    assert entry.resolve_state == RESOLVE_STATE_NOT_NEEDED
    assert entry.golden_match_state == GOLDEN_MATCH_STATE_UNKNOWN
    assert model._pending_resolves == deque()
    assert model._queue_instruction_commit_candidate_indices() == []


def test_exception_marked_cfvec_starts_wrong_path_episode() -> None:
    model = BackendModel()
    interface = _ObserveIf()
    model.observe_if = interface
    model.current_cycle = 20
    model.golden_trace = GoldenTrace(
        [TraceEntry(index=0, pc=0x80003240, instr=0x10050663, size=4)]
    )

    prev = _queue_instr(0x80000DC8, 0, 2)
    prev.instr = 0x00008082
    prev.is_rvc = True
    prev.is_cfi = True
    prev.path_state = PATH_STATE_CORRECT
    prev.golden_target_pc = 0x80003240
    model._cfvec_queue = deque([prev])

    _set_first_cfvec(interface, 0x80003248, ftq_value=3)
    interface.cfvec_instr[0].value = 0x05130000
    interface.cfvec_is_rvc[0].value = 1
    interface.cfvec_exception_vec[0][2].value = 1

    model._sample_cfvec()

    assert len(model._cfvec_queue) == 2
    wrong = model._cfvec_queue[1]
    assert wrong.exception_marked is True
    assert wrong.path_state == PATH_STATE_WRONG
    assert model._active_wrong_path_episode() is not None
    assert model._active_wrong_path_episode()["origin_index"] == 1
    assert model._active_wrong_path_episode()["target_pc"] == 0x80003240
    assert model.pending_events
    assert model.pending_events[-1].payload["target_pc"] == 0x80003240
    assert model.golden_trace.peek().pc == 0x80003240


def test_commit_ftq_idx_must_be_contiguous() -> None:
    model = BackendModel()
    model.commit_count = 1
    model.commit_ptr_flag = 0
    model.commit_ptr_value = 14

    with pytest.raises(AssertionError, match="commit ftq_idx is not contiguous"):
        model._assert_commit_ftq_is_contiguous(0, 16, mode="queue")


def test_drop_stale_committed_queue_head_asserts_instead_of_popping() -> None:
    model = BackendModel()
    model.commit_count = 1
    model.commit_ptr_flag = 0
    model.commit_ptr_value = 14
    entry = _queue_instr(0x80000020, 0, 14)
    entry.path_state = PATH_STATE_CORRECT
    entry.rob_commit_state = ROB_COMMIT_STATE_COMMITTED
    entry.is_last_in_entry = True
    model._cfvec_queue = deque([entry])
    model.ftq_entries = deque([FtqEntry(ftq_flag=0, ftq_value=14, observed_last_in_entry=True)])

    with pytest.raises(AssertionError, match="cfvec queue head references committed ftq entry"):
        model._drop_stale_committed_queue_head()


def test_commit_plan_does_not_mutate_ftq_entries_until_apply() -> None:
    model = BackendModel()
    model.golden_trace = object()
    model.current_cycle = 10
    entry = _queue_instr(0x80000020, 0, 14)
    entry.path_state = PATH_STATE_CORRECT
    entry.rob_commit_state = ROB_COMMIT_STATE_COMMITTED
    entry.is_last_in_entry = True
    model._cfvec_queue = deque([entry])
    model.ftq_entries = deque([FtqEntry(ftq_flag=0, ftq_value=14, observed_last_in_entry=True)])

    planned = model._plan_commit_entry_for_cycle(apply=False)

    assert planned is not None
    assert list(model.ftq_entries) == [FtqEntry(ftq_flag=0, ftq_value=14, observed_last_in_entry=True)]
    assert [item.ftq_value for item in model._cfvec_queue] == [14]

    model.commit_entry_driven(planned)

    assert list(model.ftq_entries) == []
    assert list(model._cfvec_queue) == []
    assert model.commit_ptr_flag == 0
    assert model.commit_ptr_value == 14


def test_golden_trace_commit_waits_for_cfvec_queue_span() -> None:
    model = BackendModel()
    model.golden_trace = object()
    model.current_cycle = 10
    model.ftq_entries = deque([FtqEntry(ftq_flag=0, ftq_value=14, observed_last_in_entry=True)])

    planned = model._plan_commit_entry_for_cycle(apply=False)

    assert planned is None
    assert list(model.ftq_entries) == [FtqEntry(ftq_flag=0, ftq_value=14, observed_last_in_entry=True)]
    assert model.commit_count == 0
    assert model.commit_ptr_flag == 0
    assert model.commit_ptr_value == 0


def test_queue_head_commit_span_includes_full_contiguous_ftq() -> None:
    model = BackendModel()
    first = _queue_instr(0x80000052, 0, 3)
    second = _queue_instr(0x80000054, 0, 3)
    third = _queue_instr(0x80000056, 0, 3)
    for entry in (first, second, third):
        entry.path_state = PATH_STATE_CORRECT
        entry.rob_commit_state = ROB_COMMIT_STATE_COMMITTED
    first.is_last_in_entry = True

    model._cfvec_queue = deque([first, second, third])

    assert model._queue_head_ftq_commit_span() == ((0, 3), 3)


def test_commit_invalidates_last_correct_cfi_context() -> None:
    model = BackendModel()
    model.commit_count = 1
    model.commit_ptr_flag = 0
    model.commit_ptr_value = 14
    model._last_correct_cfi_context = {
        "pc": 0x80000020,
        "instr": 0x00000063,
        "is_rvc": 0,
        "pred_taken": 1,
        "ftq_flag": 0,
        "ftq_value": 14,
        "ftq_offset": 0,
        "branch_type": 1,
        "ras_action": 0,
        "queue_index": 0,
        "golden_target_pc": 0x80000040,
    }

    model._invalidate_last_correct_cfi_context_after_commit()

    assert model._last_correct_cfi_context is None


def test_stale_last_correct_cfi_context_is_not_reused_for_redirect() -> None:
    model = BackendModel()
    model.commit_count = 1
    model.commit_ptr_flag = 0
    model.commit_ptr_value = 14
    model._last_correct_cfi_context = {
        "pc": 0x80000020,
        "instr": 0x00000063,
        "is_rvc": 0,
        "pred_taken": 1,
        "ftq_flag": 0,
        "ftq_value": 14,
        "ftq_offset": 0,
        "branch_type": 1,
        "ras_action": 0,
        "queue_index": 0,
        "golden_target_pc": 0x80000040,
    }
    wrong = _queue_instr(0x80000044, 0, 15)
    model._cfvec_queue = deque([wrong])

    redirect_context, target_pc, redirect_queue_index = model._derive_wrong_path_redirect(
        queue_index=0,
        queue_entry=wrong,
    )

    assert redirect_context is None
    assert target_pc is None
    assert redirect_queue_index is None


def test_queue_head_mismatch_uses_last_committed_cfi_context() -> None:
    model = BackendModel()
    wrong = _queue_instr(0x800000cc, 1, 44)
    model._cfvec_queue = deque([wrong])
    model._last_committed_correct_cfi_context = {
        "pc": 0x800000c8,
        "instr": 0x00008067,
        "is_rvc": 0,
        "pred_taken": 1,
        "ftq_flag": 1,
        "ftq_value": 43,
        "ftq_offset": 0,
        "branch_type": 3,
        "ras_action": 0,
        "queue_index": None,
        "queue_context_optional": True,
        "golden_target_pc": 0x800000c2,
    }

    redirect_context, target_pc, redirect_queue_index = model._derive_wrong_path_redirect(
        queue_index=0,
        queue_entry=wrong,
    )

    assert redirect_context is not None
    assert target_pc == 0x800000c2
    assert redirect_queue_index is None


def test_redirect_to_committed_ftq_entry_asserts() -> None:
    model = BackendModel()
    model.commit_count = 1
    model.commit_ptr_flag = 0
    model.commit_ptr_value = 14

    with pytest.raises(AssertionError, match="redirect references committed ftq entry"):
        model._plan_redirect_payload(
            {
                "target_pc": 0x80000040,
                "reason": "golden_first_mismatch_redirect",
                "pc": 0x80000020,
                "taken": 1,
                "ftq_flag": 0,
                "ftq_value": 14,
                "ftq_offset": 0,
                "branch_type": 1,
                "ras_action": 0,
                "is_rvc": 0,
                "level": 0,
            }
        )


def test_redirect_to_committed_ftq_entry_is_allowed_when_last_committed_context_matches() -> None:
    model = BackendModel()
    model.commit_count = 1
    model.commit_ptr_flag = 0
    model.commit_ptr_value = 14
    model._last_committed_correct_cfi_context = {
        "pc": 0x80000020,
        "instr": 0x00000063,
        "is_rvc": 0,
        "pred_taken": 1,
        "ftq_flag": 0,
        "ftq_value": 14,
        "ftq_offset": 0,
        "branch_type": 1,
        "ras_action": 0,
        "queue_index": None,
        "queue_context_optional": True,
        "golden_target_pc": 0x80000040,
    }

    payload = model._plan_redirect_payload(
        {
            "target_pc": 0x80000040,
            "reason": "golden_first_mismatch_redirect",
            "pc": 0x80000020,
            "taken": 1,
            "ftq_flag": 0,
            "ftq_value": 14,
            "ftq_offset": 0,
            "branch_type": 1,
            "ras_action": 0,
            "is_rvc": 0,
            "level": 0,
        }
    )

    assert payload["target_pc"] == 0x80000040


def test_ready_redirect_to_committed_ftq_entry_asserts_instead_of_dropping() -> None:
    model = BackendModel()
    model.current_cycle = 10
    model.commit_count = 1
    model.commit_ptr_flag = 0
    model.commit_ptr_value = 14
    model.pending_events.append(
        BackendEvent(
            kind="redirect",
            ready_cycle=10,
            payload={
                "target_pc": 0x80000040,
                "reason": "golden_first_mismatch_redirect",
                "pc": 0x80000020,
                "taken": 1,
                "ftq_flag": 0,
                "ftq_value": 14,
                "ftq_offset": 0,
                "branch_type": 1,
                "ras_action": 0,
                "is_rvc": 0,
                "level": 0,
            },
        )
    )

    with pytest.raises(AssertionError, match="redirect references committed ftq entry"):
        model._ready_redirect_for_cycle()


def test_ftq_transition_without_last_is_unexpected_on_normal_path() -> None:
    model = BackendModel()
    model._current_ftq_entry = FtqEntry(ftq_flag=0, ftq_value=20)

    assert model._ftq_transition_without_last_is_expected() is False


def test_ftq_transition_without_last_is_expected_when_redirect_interrupted() -> None:
    model = BackendModel()
    model._current_ftq_entry = FtqEntry(ftq_flag=0, ftq_value=20, has_redirect=True)

    assert model._ftq_transition_without_last_is_expected() is True


def test_ftq_transition_without_last_is_expected_during_recovery() -> None:
    model = BackendModel()
    model._current_ftq_entry = FtqEntry(ftq_flag=0, ftq_value=20)
    model._active_wrong_path_episode_state = ActiveWrongPathEpisode(
        origin_index=0,
        target_pc=0x1000,
        redirect_context=None,
        redirect_driven=True,
        expected_recovery_ftq=(0, 21),
        redirect_driven_cycle=10,
    )

    assert model._ftq_transition_without_last_is_expected() is True


def test_recovery_flush_waits_until_target_is_queued() -> None:
    model = BackendModel()
    wrong0 = _queue_instr(0x1010, 0, 22)
    wrong0.path_state = PATH_STATE_WRONG
    wrong1 = _queue_instr(0x1014, 0, 23)
    wrong1.path_state = PATH_STATE_WRONG
    model._cfvec_queue = deque([wrong0, wrong1])
    model._active_wrong_path_episode_state = ActiveWrongPathEpisode(
        origin_index=0,
        target_pc=0x1000,
        redirect_context=None,
        redirect_driven=True,
        expected_recovery_ftq=(0, 21),
        redirect_driven_cycle=10,
    )
    model._skip_cfvec_until_cycle = 11

    model._flush_recovery_residuals_if_target_not_queued()

    assert [entry.pc for entry in model._cfvec_queue] == [0x1010, 0x1014]


def test_redirect_drive_flushes_wrong_path_and_waits_for_recovery_target() -> None:
    model = BackendModel()
    wrong0 = _queue_instr(0x1010, 0, 22)
    wrong0.path_state = PATH_STATE_WRONG
    wrong1 = _queue_instr(0x1014, 0, 23)
    wrong1.path_state = PATH_STATE_WRONG
    model._cfvec_queue = deque([wrong0, wrong1])
    model._active_wrong_path_episode_state = ActiveWrongPathEpisode(
        origin_index=0,
        target_pc=0x1000,
        redirect_context={
            "pc": 0x1008,
            "instr": 0,
            "is_rvc": 0,
            "pred_taken": 1,
            "ftq_flag": 0,
            "ftq_value": 20,
            "ftq_offset": 0,
            "branch_type": 0,
            "ras_action": 0,
            "queue_index": None,
            "queue_context_optional": True,
        },
        redirect_driven=False,
        expected_recovery_ftq=None,
        redirect_driven_cycle=None,
    )

    model._plan_redirect_payload(
        {
            "target_pc": 0x1000,
            "reason": "golden_first_mismatch_redirect",
            "pc": 0x1008,
            "taken": 1,
            "ftq_flag": 0,
            "ftq_value": 20,
            "ftq_offset": 0,
            "branch_type": 0,
            "ras_action": 0,
            "is_rvc": 0,
            "level": 0,
            "flush_on_drive": True,
        }
    )

    assert list(model._cfvec_queue) == []
    assert model._recovery_phase_active() is True
    assert model._current_recovery_target_pc() == 0x1000


def test_recovery_flush_begins_at_first_target_entry() -> None:
    model = BackendModel()
    wrong0 = _queue_instr(0x1010, 0, 22)
    wrong0.path_state = PATH_STATE_WRONG
    target = _queue_instr(0x1000, 0, 21)
    target.path_state = PATH_STATE_CORRECT
    target.cycle = 11
    wrong1 = _queue_instr(0x1014, 0, 23)
    wrong1.path_state = PATH_STATE_WRONG
    model._cfvec_queue = deque([wrong0, target, wrong1])
    model._active_wrong_path_episode_state = ActiveWrongPathEpisode(
        origin_index=0,
        target_pc=0x1000,
        redirect_context=None,
        redirect_driven=True,
        expected_recovery_ftq=(0, 21),
        redirect_driven_cycle=10,
    )

    model._apply_recovery_if_target_queued()

    assert len(model._cfvec_queue) == 1
    assert model._cfvec_queue[0].pc == 0x1000


def test_mmio_redirect_does_not_enter_recovery_or_block_commit() -> None:
    model = BackendModel()
    model._active_wrong_path_episode_state = ActiveWrongPathEpisode(
        origin_index=0,
        target_pc=0x10001000,
        redirect_context={
            "pc": 0x10001006,
            "instr": 0,
            "is_rvc": 0,
            "pred_taken": 1,
            "ftq_flag": 0,
            "ftq_value": 3,
            "ftq_offset": 0,
            "branch_type": 0,
            "ras_action": 0,
            "queue_index": None,
            "queue_context_optional": True,
        },
        redirect_driven=True,
        expected_recovery_ftq=(0, 4),
        redirect_driven_cycle=10,
    )

    assert model._wrong_path_target_is_mmio() is True
    assert model._active_wrong_path_in_recovery() is True
    assert model._current_recovery_target_pc() == 0x10001000
    assert model._current_expected_recovery_ftq() == (0, 4)
    assert model._recovery_commit_block_matches(0, 4) is False
    assert model._active_redirect_context_blocks_commit(0, 4) is False
    assert model.backend_empty_for_dut() == 1


def test_non_cfi_cannot_begin_wrong_path_after_correct_cfi() -> None:
    model = BackendModel()
    entry = _queue_instr(0x8000007c, 0, 18)
    entry.is_cfi = False

    assert model._maybe_begin_active_wrong_path_after_correct_cfi(
        queue_index=0,
        entry=entry,
        target_pc=0x80000090,
        target_visible_immediately=False,
    ) is False


def test_indirect_jump_mismatch_attributes_redirect_to_previous_cfi() -> None:
    model = BackendModel()
    prev = _queue_instr(0x80000332, 0, 32)
    prev.instr = 0x00008067
    prev.is_cfi = True
    prev.path_state = PATH_STATE_CORRECT
    prev.golden_target_pc = 0x8000033A
    wrong = _queue_instr(0x80000336, 0, 34)
    model._cfvec_queue = deque([prev, wrong])
    model._active_wrong_path_episode_state = None
    model.golden_trace = None
    model._last_correct_cfi_context = None

    redirect_context, target_pc, redirect_queue_index = model._derive_wrong_path_redirect(
        queue_index=1,
        queue_entry=wrong,
    )

    assert redirect_queue_index == 0
    assert target_pc == 0x8000033A
    assert redirect_context is not None
    assert int(redirect_context["pc"]) == 0x80000332

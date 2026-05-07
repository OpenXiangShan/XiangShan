from __future__ import annotations

from collections import deque

import pytest

from env.backend_model import BackendModel
from env.model.backend_state import ActiveWrongPathEpisode
from env.model.backend_state import PATH_STATE_CORRECT
from env.model.backend_state import PATH_STATE_WRONG
from env.model.backend_state import QueueInstr


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


def test_commit_ftq_idx_must_be_contiguous() -> None:
    model = BackendModel()
    model.commit_count = 1
    model.commit_ptr_flag = 0
    model.commit_ptr_value = 14

    with pytest.raises(AssertionError, match="commit ftq_idx is not contiguous"):
        model._assert_commit_ftq_is_contiguous(0, 16, mode="queue")


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

    model._flush_recovery_residuals_if_target_not_queued()

    assert [entry.pc for entry in model._cfvec_queue] == [0x1010, 0x1014]


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

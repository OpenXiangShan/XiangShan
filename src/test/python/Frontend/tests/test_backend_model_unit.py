from __future__ import annotations

from env.backend_model import BackendModel
from env.model.backend_state import ActiveWrongPathEpisode
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

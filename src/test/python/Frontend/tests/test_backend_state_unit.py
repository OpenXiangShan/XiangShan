from collections import deque

from env.model.backend_state import BackendEvent, BackendState, FtqEntry


def _ready_entry(flag: int, value: int, *, total_cfi: int = 1, resolved_cfi: int = 1) -> FtqEntry:
    return FtqEntry(
        ftq_flag=int(flag),
        ftq_value=int(value),
        total_cfi=int(total_cfi),
        resolved_cfi=int(resolved_cfi),
        dispatch_complete=True,
        commit_ready_cycle=8,
    )


def test_ftq_pointer_helpers_wrap_in_both_directions():
    state = BackendState(ftq_size=4)

    assert state.increment_ftq_ptr(0, 3) == (1, 0)
    assert state.increment_ftq_ptr(1, 1) == (1, 2)
    assert state.decrement_ftq_ptr(1, 0) == (0, 3)
    assert state.decrement_ftq_ptr(0, 2) == (0, 1)


def test_redirect_survival_helpers_preserve_only_surviving_entries():
    state = BackendState(ftq_size=8)
    state.commit_ptr_flag = 0
    state.commit_ptr_value = 0

    assert state.ftq_ptr_survives_redirect(0, 1, redirect_rank=1, flush_itself=False) is True
    assert state.ftq_ptr_survives_redirect(0, 2, redirect_rank=1, flush_itself=False) is False
    assert state.ftq_ptr_survives_redirect(0, 1, redirect_rank=1, flush_itself=True) is False
    assert state.same_entry_offset_survives(1, 1, flush_itself=False) is True
    assert state.same_entry_offset_survives(1, 1, flush_itself=True) is False

    event = BackendEvent(
        kind="redirect",
        ready_cycle=20,
        payload={"ftq_flag": 0, "ftq_value": 1, "ftq_offset": 2, "queued_cycle": 4},
    )
    assert state.pending_event_survives_redirect(
        event,
        redirect_flag=0,
        redirect_value=1,
        redirect_offset=1,
        redirect_rank=1,
        flush_itself=False,
        keep_cycle=9,
    ) is False


def test_find_next_commitable_entry_reanchors_commit_ptr_to_oldest_survivor():
    state = BackendState(ftq_size=8, current_cycle=8)
    state.commit_count = 1
    state.commit_ptr_flag = 0
    state.commit_ptr_value = 1
    survivor = _ready_entry(0, 4)
    state.ftq_entries = deque([survivor])

    candidate = state.find_next_commitable_entry()

    assert candidate is survivor
    assert (state.commit_ptr_flag, state.commit_ptr_value) == (0, 3)


def test_level0_redirect_wait_blocks_until_target_ftq_arrives_and_then_clears():
    state = BackendState(ftq_size=8, current_cycle=8)
    state.commit_count = 1
    state.commit_ptr_flag = 0
    state.commit_ptr_value = 0
    state.pending_level0_target_ftq = (0, 2)
    state.ftq_entries = deque([_ready_entry(0, 3)])

    assert state.find_next_commitable_entry() is None
    assert state.pending_level0_target_ftq == (0, 2)

    state.ftq_entries = deque([_ready_entry(0, 2)])

    candidate = state.find_next_commitable_entry()

    assert candidate is not None
    assert (candidate.ftq_flag, candidate.ftq_value) == (0, 2)
    assert state.pending_level0_target_ftq is None

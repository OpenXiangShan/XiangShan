from __future__ import annotations

from collections import deque
from types import SimpleNamespace

import pytest

from env.agents.backend_agent import BackendAgent
from env.core.backend_model import BackendModel
from env.core.transactions import BackendRedirectClass, FtqIdxAheadTxn
from env.model.backend_state import ActiveWrongPathEpisode
from env.model.backend_state import BackendEvent
from env.model.backend_state import ROB_COMMIT_STATE_COMMITTED
from env.model.backend_state import FtqEntry
from env.model.backend_state import PATH_STATE_CORRECT
from env.model.backend_state import PATH_STATE_WRONG
from env.model.backend_state import QueueInstr
from env.model.backend_state import ResolveEntry
from env.model.backend_state import GOLDEN_MATCH_STATE_UNKNOWN
from env.model.backend_state import RESOLVE_STATE_NOT_NEEDED
from env.model.backend_state import RESOLVE_STATE_PENDING
from env.model import GoldenTrace
from env.model import TraceEntry
from env.support import fold_pc


class _Signal:
    def __init__(self, value: int = 0) -> None:
        self._value = int(value)
        self.write_count = 0

    @property
    def value(self) -> int:
        return self._value

    @value.setter
    def value(self, value: int) -> None:
        self._value = int(value)
        self.write_count += 1


class _ObserveIf:
    def __init__(self) -> None:
        self.cfvec_valid = [_Signal() for _ in range(8)]
        self.cfvec_foldpc = [_Signal() for _ in range(8)]
        self.cfvec_instr = [_Signal(0x13) for _ in range(8)]
        self.cfvec_is_rvc = [_Signal() for _ in range(8)]
        self.cfvec_fixed_taken = [_Signal() for _ in range(8)]
        self.cfvec_ftq_ptr_flag = [_Signal() for _ in range(8)]
        self.cfvec_ftq_ptr_value = [_Signal() for _ in range(8)]
        self.cfvec_ftq_offset = [_Signal() for _ in range(8)]
        self.cfvec_is_last_in_ftq_entry = [_Signal() for _ in range(8)]
        self.cfvec_exception_vec_1 = [_Signal() for _ in range(8)]
        self.cfvec_exception_vec_2 = [_Signal() for _ in range(8)]
        self.cfvec_exception_vec_12 = [_Signal() for _ in range(8)]
        self.cfvec_exception_vec_19 = [_Signal() for _ in range(8)]
        self.cfvec_exception_vec_20 = [_Signal() for _ in range(8)]


class _EmptyTrace:
    def peek(self):
        return None


def _set_first_cfvec(
    model: BackendModel,
    interface: _ObserveIf,
    pc: int,
    *,
    ftq_value: int = 0,
    is_rvc: bool = False,
) -> None:
    interface.cfvec_valid[0].value = 1
    interface.cfvec_foldpc[0].value = fold_pc(int(pc))
    interface.cfvec_ftq_ptr_value[0].value = int(ftq_value)
    interface.cfvec_is_rvc[0].value = int(bool(is_rvc))
    model._ftq_start_pc_by_value[int(ftq_value)] = int(pc) + (0 if is_rvc else 2)


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


def _source_bound_cfi_model() -> tuple[BackendModel, QueueInstr]:
    model = BackendModel()
    source = _queue_instr(0x80000020, 1, 9)
    source.is_cfi = True
    model._cfvec_queue = deque([source])
    return model, source


def _resolve_entry(
    source: QueueInstr,
    queue_index: int,
    target: int,
    *,
    branch_type: int = 1,
) -> ResolveEntry:
    return ResolveEntry(
        ready_cycle=int(source.cycle),
        inst_pc=int(source.pc),
        pc=int(source.pc),
        target=int(target),
        taken=True,
        mispredict=True,
        ftq_flag=int(source.ftq_flag),
        ftq_value=int(source.ftq_value),
        ftq_offset=int(source.ftq_offset),
        branch_type=int(branch_type),
        ras_action=0,
        queued_cycle=int(source.cycle),
        is_rvc=bool(source.is_rvc),
        queue_index=int(queue_index),
    )


def test_resolve_target_scan_preserves_same_cycle_slot_order() -> None:
    model = BackendModel()
    older_target = _queue_instr(0x2000, 0, 1)
    older_target.cycle = 9
    source = _queue_instr(0x1000, 0, 2)
    source.cycle = 10
    source.slot = 1
    source.is_cfi = True
    target = _queue_instr(0x2000, 0, 2)
    target.cycle = 10
    target.slot = 2
    successor = _queue_instr(0x2004, 0, 2)
    successor.cycle = 10
    successor.slot = 3
    model._cfvec_queue = deque([older_target, source, target, successor])

    assert model._resolve_target_path_state(_resolve_entry(source, 1, 0x2000)) == (
        True,
        True,
        False,
        False,
        False,
    )


def test_resolve_target_scan_ignores_target_before_dynamic_cfi() -> None:
    model = BackendModel()
    old_target = _queue_instr(0x2000, 0, 1)
    source = _queue_instr(0x1000, 0, 2)
    source.cycle = 10
    source.is_cfi = True
    unrelated = _queue_instr(0x3000, 0, 2)
    unrelated.cycle = 11
    model._cfvec_queue = deque([old_target, source, unrelated])

    assert model._resolve_target_path_state(_resolve_entry(source, 1, 0x2000)) == (
        False,
        False,
        False,
        False,
        False,
    )


def test_resolve_target_scan_requires_progress_after_target() -> None:
    model = BackendModel()
    source = _queue_instr(0x1000, 0, 2)
    source.cycle = 10
    source.is_cfi = True
    target0 = _queue_instr(0x2000, 0, 3)
    target0.cycle = 11
    target1 = _queue_instr(0x2000, 0, 3)
    target1.cycle = 11
    successor = _queue_instr(0x2004, 0, 3)
    successor.cycle = 12
    model._cfvec_queue = deque([source, target0, target1])
    entry = _resolve_entry(source, 0, 0x2000)

    assert model._resolve_target_path_state(entry)[:4] == (True, False, True, False)

    model._cfvec_queue.append(successor)

    assert model._resolve_target_path_state(entry)[:4] == (True, True, True, True)


def test_indirect_resolve_scan_binds_to_source_golden_index() -> None:
    model = BackendModel()
    model.golden_trace = GoldenTrace(
        [
            TraceEntry(index=0, pc=0x1000, instr=0x00008067, size=4, kind="jump_indirect", taken=True, target_pc=0x3000),
            TraceEntry(index=1, pc=0x3000, instr=0x13, size=4),
            TraceEntry(index=2, pc=0x3004, instr=0x13, size=4),
            TraceEntry(index=3, pc=0x1000, instr=0x00008067, size=4, kind="jump_indirect", taken=True, target_pc=0x2000),
            TraceEntry(index=4, pc=0x2000, instr=0x13, size=4),
            TraceEntry(index=5, pc=0x2004, instr=0x13, size=4),
        ]
    )
    model.golden_trace.cursor = 0
    source = _queue_instr(0x1000, 0, 2)
    source.cycle = 10
    source.slot = 1
    source.is_cfi = True
    source.golden_index = 3
    target = _queue_instr(0x2000, 0, 2)
    target.cycle = 10
    target.slot = 2
    successor = _queue_instr(0x2004, 0, 2)
    successor.cycle = 10
    successor.slot = 3
    model._cfvec_queue = deque([source, target, successor])

    state = model._resolve_target_path_state(
        _resolve_entry(source, 0, 0x2000, branch_type=3)
    )

    assert state == (True, True, False, False, True)


def test_resolve_target_scan_rejects_queue_identity_mismatch() -> None:
    model = BackendModel()
    source = _queue_instr(0x1000, 0, 2)
    source.is_cfi = True
    model._cfvec_queue = deque([source])
    entry = _resolve_entry(source, 0, 0x2000)
    entry.ftq_offset = 1

    with pytest.raises(AssertionError, match="queue entry identity mismatch"):
        model._resolve_target_path_state(entry)


def test_commit_pop_remaps_resolve_target_scan_source() -> None:
    model = BackendModel()
    older0 = _queue_instr(0x0FF0, 0, 1)
    older1 = _queue_instr(0x0FF4, 0, 1)
    source = _queue_instr(0x1000, 0, 2)
    source.cycle = 10
    source.is_cfi = True
    target = _queue_instr(0x2000, 0, 3)
    target.cycle = 11
    model._cfvec_queue = deque([older0, older1, source, target])
    model._pending_resolves = deque([_resolve_entry(source, 2, 0x2000)])

    model._cfvec_queue_pop_head(2)

    assert model._pending_resolves[0].queue_index == 0
    assert model._resolve_target_path_state(model._pending_resolves[0])[:4] == (
        True,
        False,
        True,
        False,
    )


def test_wrong_path_flush_removes_pending_resolve_with_source() -> None:
    model = BackendModel()
    older = _queue_instr(0x0FF0, 0, 1)
    older.path_state = PATH_STATE_CORRECT
    source = _queue_instr(0x1000, 0, 2)
    source.cycle = 10
    source.is_cfi = True
    source.path_state = PATH_STATE_WRONG
    model._cfvec_queue = deque([older, source])
    model._pending_resolves = deque([_resolve_entry(source, 1, 0x2000)])
    model._active_wrong_path_episode_state = ActiveWrongPathEpisode(
        origin_index=1,
        target_pc=0x2000,
        redirect_context=None,
    )

    model._cfvec_queue_flush_wrong_path()

    assert list(model._cfvec_queue) == [older]
    assert model._pending_resolves == deque()


def test_queue_range_removal_drops_and_remaps_pending_resolves() -> None:
    model = BackendModel()
    older = _queue_instr(0x0FF0, 0, 1)
    removed_source = _queue_instr(0x1000, 0, 2)
    removed_source.is_cfi = True
    kept_source = _queue_instr(0x1100, 0, 3)
    kept_source.cycle = 10
    kept_source.is_cfi = True
    target = _queue_instr(0x2000, 0, 4)
    target.cycle = 11
    model._cfvec_queue = deque([older, removed_source, kept_source, target])
    model._pending_resolves = deque(
        [
            _resolve_entry(removed_source, 1, 0x3000),
            _resolve_entry(kept_source, 2, 0x2000),
        ]
    )

    model._cfvec_queue_remove_range(1, 2)

    assert len(model._pending_resolves) == 1
    assert model._pending_resolves[0].queue_index == 1
    assert model._resolve_target_path_state(model._pending_resolves[0])[:4] == (
        True,
        False,
        True,
        False,
    )


def test_ready_indirect_resolve_uses_queue_without_monitor_history() -> None:
    model = BackendModel()
    model.current_cycle = 13
    model.golden_trace = GoldenTrace(
        [
            TraceEntry(
                index=0,
                pc=0x1000,
                instr=0x00008067,
                size=4,
                kind="jump_indirect",
                taken=True,
                target_pc=0x2000,
            ),
            TraceEntry(index=1, pc=0x2000, instr=0x13, size=4),
            TraceEntry(index=2, pc=0x2004, instr=0x13, size=4),
        ]
    )
    source = _queue_instr(0x1000, 0, 2)
    source.cycle = 10
    source.is_cfi = True
    source.path_state = PATH_STATE_CORRECT
    source.resolve_state = RESOLVE_STATE_PENDING
    source.golden_index = 0
    target = _queue_instr(0x2000, 0, 2)
    target.cycle = 10
    target.slot = 1
    successor = _queue_instr(0x2004, 0, 2)
    successor.cycle = 10
    successor.slot = 2
    model._cfvec_queue = deque([source, target, successor])
    model._pending_resolves = deque(
        [_resolve_entry(source, 0, 0x2000, branch_type=3)]
    )

    ready = model._ready_resolves_for_cycle()

    assert len(ready) == 1
    assert ready[0].mispredict is False
    assert model._pending_resolves == deque()


def test_ready_resolve_rejects_stale_queue_index() -> None:
    model = BackendModel()
    source = _queue_instr(0x1000, 0, 2)
    source.is_cfi = True
    source.path_state = PATH_STATE_CORRECT
    model._pending_resolves = deque([_resolve_entry(source, 1, 0x2000)])

    with pytest.raises(AssertionError, match="queue index is out of range"):
        model._ready_resolves_for_cycle()


@pytest.mark.parametrize(("is_rvc", "expected_pc"), ((False, 0x80001008), (True, 0x8000100A)))
def test_observed_cfvec_pc_uses_ftq_context_and_validates_foldpc(is_rvc, expected_pc) -> None:
    model = BackendModel()
    interface = _ObserveIf()
    model.observe_if = interface
    interface.cfvec_ftq_ptr_flag[0].value = 1
    interface.cfvec_ftq_ptr_value[0].value = 3
    interface.cfvec_ftq_offset[0].value = 5
    interface.cfvec_is_rvc[0].value = int(is_rvc)
    interface.cfvec_foldpc[0].value = fold_pc(expected_pc)
    model._ftq_start_pc_by_value[3] = 0x80001000

    assert model.observed_cfvec_pc(0) == expected_pc


def test_observed_cfvec_pc_rejects_missing_ftq_context() -> None:
    model = BackendModel()
    model.observe_if = _ObserveIf()

    with pytest.raises(AssertionError, match="without an observed start PC"):
        model.observed_cfvec_pc(0)


def test_observed_cfvec_pc_rejects_foldpc_mismatch() -> None:
    model = BackendModel()
    interface = _ObserveIf()
    model.observe_if = interface
    interface.cfvec_foldpc[0].value = fold_pc(0x80001000) ^ 1
    model._ftq_start_pc_by_value[0] = 0x80001002

    with pytest.raises(AssertionError, match="foldpc does not match FTQ-derived PC"):
        model.observed_cfvec_pc(0)


def _redirect_drive_if():
    fields = (
        "redirect_bits_pc",
        "redirect_bits_target",
        "redirect_bits_taken",
        "redirect_bits_ftq_idx_flag",
        "redirect_bits_ftq_idx_value",
        "redirect_bits_ftq_offset",
        "redirect_bits_is_rvc",
        "redirect_bits_attribute_branch_type",
        "redirect_bits_attribute_ras_action",
        "redirect_bits_level",
        "redirect_bits_backend_igpf",
        "redirect_bits_backend_ipf",
        "redirect_bits_backend_iaf",
        "redirect_bits_satp_flush",
        "redirect_bits_debug_is_ctrl",
        "redirect_bits_debug_is_mem_vio",
        "redirect_valid",
    )
    return SimpleNamespace(**{field: _Signal() for field in fields})


def _ftq_idx_ahead_drive_if():
    return SimpleNamespace(
        redirect_valid=_Signal(),
        resolve_valid=[_Signal() for _ in range(3)],
        call_ret_commit_valid=[_Signal() for _ in range(8)],
        call_ret_commit_bits_ras_action=[_Signal() for _ in range(8)],
        call_ret_commit_bits_ftq_ptr_value=[_Signal() for _ in range(8)],
        ftq_idx_ahead_valid=_Signal(),
        ftq_idx_ahead_flag=_Signal(),
        ftq_idx_ahead_value=_Signal(),
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


def test_source_bound_exception_redirect_uses_observed_cfvec_context() -> None:
    model = BackendModel()
    source = _queue_instr(0x80000020, 1, 9)
    source.ftq_offset = 6
    source.is_rvc = True
    source.exception_marked = True
    source.exception_bits = 1 << 12
    model._cfvec_queue = deque([source])

    model.inject_redirect_from_cfvec(
        source_pc=0x80000020,
        source_ftq_flag=1,
        source_ftq_value=9,
        source_ftq_offset=6,
        target_pc=0x80000100,
        reason="instruction-page-fault-trap",
        level=1,
        backend_ipf=1,
    )

    event = model.pending_events[0]
    assert event.payload["pc"] == 0x80000020
    assert event.payload["ftq_flag"] == 1
    assert event.payload["ftq_value"] == 9
    assert event.payload["ftq_offset"] == 6
    assert event.payload["is_rvc"] == 1
    assert event.payload["backend_ipf"] == 1
    assert event.payload["level"] == 1
    assert event.payload["flush_on_drive"] is True


def test_source_bound_exception_redirect_is_not_remapped_to_historical_target() -> None:
    model = BackendModel()
    source = _queue_instr(0x80000020, 0, 4)
    source.ftq_offset = 1
    source.exception_marked = True
    source.exception_bits = 1 << 1
    model._cfvec_queue = deque([source])
    model._ftq_group_pc_history[(0, 13)] = [
        (0x80000240, False),
        (0x80000100, False),
    ]
    model._pc_group_occurrences[0x80000100] = [(0, 13, 1)]

    model.inject_redirect_from_cfvec(
        source_pc=source.pc,
        source_ftq_flag=source.ftq_flag,
        source_ftq_value=source.ftq_value,
        source_ftq_offset=source.ftq_offset,
        target_pc=0x80000100,
        reason="instruction-access-fault-recovery",
        level=1,
        backend_iaf=1,
        redirect_class=BackendRedirectClass.OTHER,
    )

    payload = model._plan_redirect_payload(model.pending_events[0].payload)

    assert payload["pc"] == source.pc
    assert payload["ftq_flag"] == source.ftq_flag
    assert payload["ftq_value"] == source.ftq_value
    assert payload["ftq_offset"] == source.ftq_offset
    assert not model._cfvec_queue


@pytest.mark.parametrize(
    ("ftq_offset", "is_rvc", "expected"),
    (
        (0, False, (1, 9)),
        (0, True, (1, 9)),
        (1, False, (1, 9)),
        (1, True, (1, 10)),
        (2, False, (1, 10)),
    ),
)
def test_flush_itself_recovery_ftq_matches_redirect_new_ftq_idx(
    ftq_offset: int,
    is_rvc: bool,
    expected: tuple[int, int],
) -> None:
    model = BackendModel()

    assert model._expected_recovery_ftq_for_redirect(
        ftq_flag=1,
        ftq_value=9,
        ftq_offset=ftq_offset,
        is_rvc=is_rvc,
        flush_itself=True,
    ) == expected


def test_backend_fault_redirect_suppresses_same_cycle_ftq_commit(monkeypatch) -> None:
    model = BackendModel()
    candidate = FtqEntry(ftq_flag=0, ftq_value=4)
    monkeypatch.setattr(
        model,
        "_ready_redirect_for_cycle",
        lambda: {"backend_iaf": 1, "backend_ipf": 0, "backend_igpf": 0},
    )
    monkeypatch.setattr(model, "_plan_commit_entry_for_cycle", lambda apply=False: candidate)

    actions = model.plan_cycle_actions()

    assert actions.redirect_payload is not None
    assert actions.commit_entry is None


def test_source_bound_fault_redirect_rejects_unmatched_exception_bit() -> None:
    model = BackendModel()
    source = _queue_instr(0x80000020, 1, 9)
    source.exception_marked = True
    source.exception_bits = 1 << 1
    model._cfvec_queue = deque([source])

    with pytest.raises(AssertionError, match="lacks matching cfVec exception bit"):
        model.inject_redirect_from_cfvec(
            source_pc=0x80000020,
            source_ftq_flag=None,
            source_ftq_value=None,
            source_ftq_offset=None,
            target_pc=0x80000100,
            reason="instruction-page-fault-trap",
            level=1,
            backend_ipf=1,
        )


def test_backend_agent_drives_satp_flush_and_redirect_level() -> None:
    agent = BackendAgent()
    drive_if = _redirect_drive_if()
    agent._drive_if = drive_if

    agent.drive_redirect(
        {
            "pc": 0x80000020,
            "target_pc": 0x80000100,
            "level": 1,
            "satp_flush": 1,
            "redirect_class": BackendRedirectClass.OTHER,
        }
    )

    assert drive_if.redirect_bits_pc.value == 0x80000020
    assert drive_if.redirect_bits_target.value == 0x80000100
    assert drive_if.redirect_bits_level.value == 1
    assert drive_if.redirect_bits_satp_flush.value == 1
    assert drive_if.redirect_valid.value == 1


@pytest.mark.parametrize(
    ("redirect_class", "debug_is_ctrl", "debug_is_mem_vio"),
    (
        (BackendRedirectClass.CONTROL_FLOW, 1, 0),
        (BackendRedirectClass.MEMORY_VIOLATION, 0, 1),
        (BackendRedirectClass.OTHER, 0, 0),
    ),
)
def test_backend_agent_encodes_redirect_class(
    redirect_class: BackendRedirectClass,
    debug_is_ctrl: int,
    debug_is_mem_vio: int,
) -> None:
    agent = BackendAgent()
    drive_if = _redirect_drive_if()
    agent._drive_if = drive_if

    agent.drive_redirect({"redirect_class": redirect_class})

    assert drive_if.redirect_bits_debug_is_ctrl.value == debug_is_ctrl
    assert drive_if.redirect_bits_debug_is_mem_vio.value == debug_is_mem_vio


def test_backend_agent_rejects_redirect_without_structured_class() -> None:
    agent = BackendAgent()
    agent._drive_if = _redirect_drive_if()

    with pytest.raises(ValueError, match="valid BackendRedirectClass"):
        agent.drive_redirect({"redirect_class": "control_flow"})


def test_backend_agent_clears_ftq_idx_ahead_valid_without_rewriting_payload() -> None:
    agent = BackendAgent()
    drive_if = _ftq_idx_ahead_drive_if()
    agent._drive_if = drive_if

    agent.drive_ftq_idx_ahead(FtqIdxAheadTxn(ftq_flag=1, ftq_value=63))

    assert drive_if.ftq_idx_ahead_valid.value == 1
    assert drive_if.ftq_idx_ahead_flag.value == 1
    assert drive_if.ftq_idx_ahead_value.value == 63
    drive_if.call_ret_commit_bits_ras_action[0].value = 2
    drive_if.call_ret_commit_bits_ftq_ptr_value[0].value = 9
    valid_write_count = drive_if.ftq_idx_ahead_valid.write_count

    agent.clear_one_shot_signals()
    assert drive_if.ftq_idx_ahead_valid.write_count == valid_write_count + 1
    agent.drive_ftq_idx_ahead(None)
    agent.clear_one_shot_signals()

    assert drive_if.ftq_idx_ahead_valid.value == 0
    assert drive_if.ftq_idx_ahead_valid.write_count == valid_write_count + 1
    assert drive_if.ftq_idx_ahead_flag.value == 1
    assert drive_if.ftq_idx_ahead_value.value == 63
    assert drive_if.call_ret_commit_bits_ras_action[0].value == 2
    assert drive_if.call_ret_commit_bits_ftq_ptr_value[0].value == 9


def test_backend_agent_writes_commit_valid_for_each_transaction_and_clears_once() -> None:
    agent = BackendAgent()
    drive_if = SimpleNamespace(
        commit_valid=_Signal(),
        commit_bits_flag=_Signal(),
        commit_bits_value=_Signal(),
    )
    agent._drive_if = drive_if

    agent.drive_commit(FtqEntry(ftq_flag=0, ftq_value=3))
    valid_write_count = drive_if.commit_valid.write_count
    agent.drive_commit(FtqEntry(ftq_flag=1, ftq_value=4))

    assert drive_if.commit_valid.value == 1
    assert drive_if.commit_valid.write_count == valid_write_count + 1
    assert drive_if.commit_bits_flag.value == 1
    assert drive_if.commit_bits_value.value == 4

    agent.drive_commit(None)
    assert drive_if.commit_valid.value == 0
    assert drive_if.commit_valid.write_count == valid_write_count + 2

    agent.drive_commit(None)
    assert drive_if.commit_valid.write_count == valid_write_count + 2


@pytest.mark.parametrize(
    ("ftq_idx_ahead_flag", "ftq_idx_ahead_value"),
    ((1, 9), (0, 10)),
    ids=("match", "mismatch"),
)
def test_source_bound_redirect_drives_ftq_idx_ahead_one_cycle_early(
    ftq_idx_ahead_flag: int,
    ftq_idx_ahead_value: int,
) -> None:
    model, source = _source_bound_cfi_model()
    model.current_cycle = 20

    model.inject_redirect_from_cfvec(
        source_pc=source.pc,
        source_ftq_flag=source.ftq_flag,
        source_ftq_value=source.ftq_value,
        source_ftq_offset=source.ftq_offset,
        target_pc=0x80000100,
        reason="ftq-idx-ahead-timing",
        delay_cycles=3,
        ftq_idx_ahead_flag=ftq_idx_ahead_flag,
        ftq_idx_ahead_value=ftq_idx_ahead_value,
    )

    assert model.pending_events[0].ready_cycle == 23
    for cycle in (20, 21):
        model.current_cycle = cycle
        assert model._ready_ftq_idx_ahead_for_cycle() is None

    model.current_cycle = 22
    ahead = model._ready_ftq_idx_ahead_for_cycle()
    assert ahead == FtqIdxAheadTxn(
        ftq_flag=ftq_idx_ahead_flag,
        ftq_value=ftq_idx_ahead_value,
    )
    assert model._ready_redirect_for_cycle() is None

    model.current_cycle = 23
    assert model._ready_ftq_idx_ahead_for_cycle() is None
    redirect = model._ready_redirect_for_cycle()
    assert redirect is not None
    assert redirect["ftq_flag"] == source.ftq_flag
    assert redirect["ftq_value"] == source.ftq_value


@pytest.mark.parametrize(
    ("ftq_idx_ahead_flag", "ftq_idx_ahead_value", "error"),
    (
        (1, None, "flag and value cannot be None"),
        (None, 9, "flag and value cannot be None"),
        (2, 9, "flag must be 0 or 1"),
        (1, 64, "value must be within"),
    ),
)
def test_source_bound_redirect_rejects_invalid_ftq_idx_ahead(
    ftq_idx_ahead_flag: int | None,
    ftq_idx_ahead_value: int | None,
    error: str,
) -> None:
    model, source = _source_bound_cfi_model()

    with pytest.raises(ValueError, match=error):
        model.inject_redirect_from_cfvec(
            source_pc=source.pc,
            source_ftq_flag=source.ftq_flag,
            source_ftq_value=source.ftq_value,
            source_ftq_offset=source.ftq_offset,
            target_pc=0x80000100,
            reason="invalid-ftq-idx-ahead",
            delay_cycles=3,
            ftq_idx_ahead_flag=ftq_idx_ahead_flag,
            ftq_idx_ahead_value=ftq_idx_ahead_value,
        )


def test_source_bound_redirect_rejects_ftq_idx_ahead_without_lead_cycle() -> None:
    model, source = _source_bound_cfi_model()

    with pytest.raises(ValueError, match="ready at least one cycle later"):
        model.inject_redirect_from_cfvec(
            source_pc=source.pc,
            source_ftq_flag=source.ftq_flag,
            source_ftq_value=source.ftq_value,
            source_ftq_offset=source.ftq_offset,
            target_pc=0x80000100,
            reason="zero-delay-ftq-idx-ahead",
            delay_cycles=0,
            ftq_idx_ahead_flag=source.ftq_flag,
            ftq_idx_ahead_value=source.ftq_value,
        )


def test_redirect_without_source_rejects_ftq_idx_ahead() -> None:
    model = BackendModel()

    with pytest.raises(ValueError, match="requires source-bound redirect FTQ context"):
        model.inject_redirect(
            target_pc=0x80000100,
            reason="missing-source-ftq-idx-ahead",
            ftq_idx_ahead_flag=1,
            ftq_idx_ahead_value=9,
        )


def test_plan_redirect_payload_preserves_level_and_satp_flush() -> None:
    model = BackendModel()

    payload = model._plan_redirect_payload(
        {
            "target_pc": 0x80000100,
            "reason": "satp-flush",
            "pc": 0x80000020,
            "ftq_flag": 1,
            "ftq_value": 9,
            "ftq_offset": 6,
            "is_rvc": 1,
            "level": 1,
            "satp_flush": 1,
            "redirect_class": BackendRedirectClass.OTHER,
        }
    )

    assert payload["level"] == 1
    assert payload["satp_flush"] == 1
    assert payload["redirect_class"] is BackendRedirectClass.OTHER


def test_redirect_reason_does_not_select_redirect_class() -> None:
    model = BackendModel()

    payload = model._plan_redirect_payload(
        {
            "target_pc": 0x80000100,
            "reason": "memVio",
        }
    )

    assert payload["redirect_class"] is BackendRedirectClass.CONTROL_FLOW


@pytest.mark.parametrize(
    "redirect_class",
    (BackendRedirectClass.MEMORY_VIOLATION, BackendRedirectClass.OTHER),
)
def test_source_bound_non_cfi_redirect_accepts_non_control_class(
    redirect_class: BackendRedirectClass,
) -> None:
    model = BackendModel()
    source = _queue_instr(0x80000020, 0, 9)
    model._cfvec_queue = deque([source])

    model.inject_redirect_from_cfvec(
        source_pc=source.pc,
        source_ftq_flag=source.ftq_flag,
        source_ftq_value=source.ftq_value,
        source_ftq_offset=source.ftq_offset,
        target_pc=0x80000100,
        reason="structured-class",
        redirect_class=redirect_class,
    )

    assert model.pending_events[0].payload["redirect_class"] is redirect_class


def test_source_bound_control_redirect_rejects_non_cfi_source() -> None:
    model = BackendModel()
    source = _queue_instr(0x80000020, 0, 9)
    model._cfvec_queue = deque([source])

    with pytest.raises(AssertionError, match="control redirect source must be a CFI"):
        model.inject_redirect_from_cfvec(
            source_pc=source.pc,
            source_ftq_flag=source.ftq_flag,
            source_ftq_value=source.ftq_value,
            source_ftq_offset=source.ftq_offset,
            target_pc=0x80000100,
            reason="control-flow",
            redirect_class=BackendRedirectClass.CONTROL_FLOW,
        )


def test_memory_violation_redirect_rejects_fault_or_satp_flush() -> None:
    model = BackendModel()
    source = _queue_instr(0x80000020, 0, 9)
    source.exception_marked = True
    source.exception_bits = 1 << 12
    model._cfvec_queue = deque([source])

    with pytest.raises(AssertionError, match="cannot carry backend fault or satpFlush"):
        model.inject_redirect_from_cfvec(
            source_pc=source.pc,
            source_ftq_flag=source.ftq_flag,
            source_ftq_value=source.ftq_value,
            source_ftq_offset=source.ftq_offset,
            target_pc=0x80000100,
            reason="invalid-memory-violation",
            backend_ipf=1,
            redirect_class=BackendRedirectClass.MEMORY_VIOLATION,
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


def test_model_redirect_samples_t_then_skips_cfvec_from_observed_dut_redirect() -> None:
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

    _set_first_cfvec(model, interface, 0x1004)
    actions = model.plan_cycle_actions()

    assert actions.redirect_payload is not None
    assert [entry.pc for entry in model._cfvec_queue] == [0x1004]
    assert model._skip_cfvec_until_cycle is None

    model.current_cycle = 11
    model.note_dut_redirect_observed(11)
    _set_first_cfvec(model, interface, 0x1008)
    model.plan_cycle_actions()

    assert [entry.pc for entry in model._cfvec_queue] == [0x1004]

    model.current_cycle = 12
    _set_first_cfvec(model, interface, 0x1010)
    model.plan_cycle_actions()

    assert [entry.pc for entry in model._cfvec_queue] == [0x1004]

    model.current_cycle = 13
    _set_first_cfvec(model, interface, 0x2000, ftq_value=1)
    model.plan_cycle_actions()

    assert [entry.pc for entry in model._cfvec_queue] == [0x1004, 0x2000]


def test_redirect_flush_samples_current_cfvec_before_arming_recovery() -> None:
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
                "flush_on_drive": True,
            },
        )
    )
    _set_first_cfvec(model, interface, 0x1004)

    actions = model.plan_cycle_actions()

    assert actions.redirect_payload is not None
    assert [entry.pc for entry in model._cfvec_queue] == [0x1004]
    assert model._current_recovery_target_pc() == 0x2000


def test_observed_dut_redirect_extends_cfvec_skip_from_observed_cycle() -> None:
    model = BackendModel()
    model._skip_cfvec_until_cycle = 11

    model.note_dut_redirect_observed(11)

    assert model._skip_cfvec_until_cycle == 12


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
    _set_first_cfvec(model, interface, 0x2004, ftq_value=1)

    with pytest.raises(AssertionError, match="redirect recovery first cfvec is not target"):
        model._sample_cfvec()

    assert list(model._cfvec_queue) == []


def test_exception_marked_cfvec_is_queued_without_normal_backend_actions() -> None:
    model = BackendModel()
    interface = _ObserveIf()
    model.observe_if = interface
    model.current_cycle = 20
    model.golden_trace = _EmptyTrace()

    _set_first_cfvec(model, interface, 0x80003248, ftq_value=3, is_rvc=True)
    interface.cfvec_instr[0].value = 0x05130000
    interface.cfvec_fixed_taken[0].value = 1
    interface.cfvec_exception_vec_2[0].value = 1

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

    _set_first_cfvec(model, interface, 0x80003248, ftq_value=3, is_rvc=True)
    interface.cfvec_instr[0].value = 0x05130000
    interface.cfvec_exception_vec_2[0].value = 1

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

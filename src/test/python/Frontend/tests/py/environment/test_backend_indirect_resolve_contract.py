"""Indirect training must wait for evidence of the delivered successor PC."""

from collections import deque
from types import SimpleNamespace

import pytest

from env.core.backend_model import BackendModel
from env.model.backend_state import (
    PATH_STATE_CORRECT, PATH_STATE_WRONG, RESOLVE_STATE_PENDING,
    QueueInstr, ResolveEntry,
)
from env.model.golden_trace import GoldenTrace, TraceEntry


def _fixture(*, instruction=0x30067, is_rvc=False, target=0x80000300):
    pc = 0x80000140
    model = BackendModel()
    model.set_golden_trace(GoldenTrace([
        TraceEntry(0, pc, instruction, 2 if is_rvc else 4,
                   "jump_indirect", True, target),
        TraceEntry(1, target, 0x13, 4),
    ]))
    model.golden_trace.next_entry()
    model.current_cycle = 20
    source = QueueInstr(
        cycle=10, slot=0, pc=pc, instr=instruction, is_rvc=is_rvc,
        pred_taken=True, ftq_flag=0, ftq_value=2, ftq_offset=0 if is_rvc else 1,
        is_last_in_entry=True, path_state=PATH_STATE_CORRECT,
        resolve_state=RESOLVE_STATE_PENDING, golden_index=0,
        golden_target_pc=target, is_cfi=True,
    )
    resolve = ResolveEntry(
        ready_cycle=14, inst_pc=pc, pc=pc, target=target, taken=True,
        mispredict=False, ftq_flag=0, ftq_value=2, ftq_offset=source.ftq_offset,
        branch_type=3, ras_action=0, queued_cycle=10,
        is_rvc=is_rvc, queue_index=0,
    )
    model._cfvec_queue = deque([source])
    model._pending_resolves = deque([resolve])
    return model, resolve


def _successor(model, pc, *, correct=True, cycle=18):
    model._cfvec_queue.append(QueueInstr(
        cycle=cycle, slot=1, pc=pc, instr=0x13, is_rvc=False,
        pred_taken=False, ftq_flag=0, ftq_value=3, ftq_offset=1,
        is_last_in_entry=True,
        path_state=PATH_STATE_CORRECT if correct else PATH_STATE_WRONG,
    ))


@pytest.mark.parametrize("instruction,is_rvc", [(0x30067, False), (0x8302, True)])
def test_indirect_resolve_waits_for_successor_not_just_latency(instruction, is_rvc):
    model, resolve = _fixture(instruction=instruction, is_rvc=is_rvc)
    assert model._ready_resolves_for_cycle() == ()
    assert list(model._pending_resolves) == [resolve]
    assert model._queue_instruction_commit_candidate_indices() == []


def test_matching_indirect_successor_emits_correct_once():
    model, resolve = _fixture()
    _successor(model, resolve.target)
    emitted, = model._ready_resolves_for_cycle()
    assert emitted.target == resolve.target and not emitted.mispredict
    assert model._ready_resolves_for_cycle() == ()


def test_wrong_first_successor_is_not_erased_by_later_recovery():
    model, resolve = _fixture()
    _successor(model, resolve.inst_pc + 4, correct=False)
    _successor(model, resolve.target)
    model.monitor = SimpleNamespace(observations=[
        SimpleNamespace(cycle=11, pc=resolve.inst_pc + 4),
        SimpleNamespace(cycle=18, pc=resolve.target),
        SimpleNamespace(cycle=19, pc=resolve.target + 4),
    ])
    emitted, = model._ready_resolves_for_cycle()
    assert emitted.mispredict and emitted.target == resolve.target


def test_unrelated_monitor_target_cannot_resolve_unknown_successor():
    model, resolve = _fixture()
    model.monitor = SimpleNamespace(observations=[
        SimpleNamespace(cycle=18, pc=resolve.target),
        SimpleNamespace(cycle=19, pc=resolve.target + 4),
    ])
    assert model._ready_resolves_for_cycle() == ()


def test_known_wrong_path_mispredict_survives_flushed_suffix():
    model, resolve = _fixture()
    resolve.mispredict = True  # Already established by wrong-path detection.
    model.monitor = SimpleNamespace(observations=[
        SimpleNamespace(cycle=18, pc=resolve.target),
        SimpleNamespace(cycle=19, pc=resolve.target + 4),
    ])
    emitted, = model._ready_resolves_for_cycle()
    assert emitted.mispredict


def test_wrong_path_source_never_trains():
    model, _ = _fixture()
    model._cfvec_queue[0].path_state = PATH_STATE_WRONG
    assert model._ready_resolves_for_cycle() == ()
    assert not model._pending_resolves


def test_unknown_indirect_target_is_not_itself_a_mispredict_on_requeue():
    model, _ = _fixture()
    model._pending_resolves.clear()
    model._requeue_resolve_for_queue_cfi(0)
    assert not model._pending_resolves[0].mispredict


def test_self_target_requires_a_distinct_delivered_instruction():
    model, resolve = _fixture(target=0x80000140)
    assert model._ready_resolves_for_cycle() == ()
    _successor(model, resolve.target, cycle=10)
    emitted, = model._ready_resolves_for_cycle()
    assert not emitted.mispredict


def test_direct_resolve_does_not_acquire_indirect_successor_wait():
    model, resolve = _fixture(instruction=0x1C00006F)
    resolve.branch_type = 2
    assert len(model._ready_resolves_for_cycle()) == 1

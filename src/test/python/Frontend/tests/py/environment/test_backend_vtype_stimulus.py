from __future__ import annotations

from collections import deque
from types import SimpleNamespace

import pytest

from env.agents.backend_agent import BackendAgent
from env.core.backend_model import BackendModel
from env.model import GoldenTrace, TraceEntry
from env.model.backend_state import PATH_STATE_CORRECT, ROB_COMMIT_STATE_COMMITTED, QueueInstr


def _queue_entry(pc: int, instr: int, index: int, offset: int) -> QueueInstr:
    return QueueInstr(
        cycle=0,
        slot=0,
        pc=pc,
        instr=instr,
        is_rvc=False,
        pred_taken=False,
        ftq_flag=0,
        ftq_value=0,
        ftq_offset=offset,
        is_last_in_entry=False,
        path_state=PATH_STATE_CORRECT,
        golden_index=index,
    )


def test_vsetvl_commit_flushes_younger_queue_and_replays_trace() -> None:
    vsetvli = 0x0D0072D7
    vsetvl = 0x80007057
    nop = 0x00000013
    entries = [
        TraceEntry(index=0, pc=0x80000000, instr=vsetvli, size=4, vtype_after=0xD0),
        TraceEntry(index=1, pc=0x80000004, instr=vsetvl, size=4, vtype_after=0xD1),
        TraceEntry(index=2, pc=0x80000008, instr=nop, size=4),
    ]
    model = BackendModel()
    model.set_golden_trace(GoldenTrace(entries))
    model.golden_trace.reset(3)
    model.current_cycle = 10
    first = _queue_entry(entries[0].pc, vsetvli, 0, 0)
    source = _queue_entry(entries[1].pc, vsetvl, 1, 2)
    younger = _queue_entry(entries[2].pc, nop, 2, 4)
    model._cfvec_queue = deque((first, source, younger))

    actions = model.plan_cycle_actions()

    assert actions.vtype_commit == (0xD1, True)
    assert actions.redirect_payload["target_pc"] == entries[2].pc
    assert actions.redirect_payload["pc"] == source.pc
    assert model.golden_trace.cursor == 2
    assert [entry.pc for entry in model._cfvec_queue] == [first.pc, source.pc]
    assert first.rob_commit_state == ROB_COMMIT_STATE_COMMITTED
    assert source.rob_commit_state == ROB_COMMIT_STATE_COMMITTED
    assert younger.rob_commit_state != ROB_COMMIT_STATE_COMMITTED


def test_vector_commit_requires_dynamic_vtype_result() -> None:
    instr = 0x80007057
    entry = TraceEntry(index=0, pc=0x80000000, instr=instr, size=4)
    model = BackendModel()
    model.set_golden_trace(GoldenTrace([entry]))
    model.current_cycle = 10
    source = _queue_entry(entry.pc, instr, 0, 0)
    model._cfvec_queue = deque((source,))

    with pytest.raises(AssertionError, match="lacks vtype_after"):
        model.plan_cycle_actions()
    assert source.rob_commit_state != ROB_COMMIT_STATE_COMMITTED


def test_vsetvli_commit_updates_architectural_vtype_without_redirect() -> None:
    instr = 0x0D0072D7
    entry = TraceEntry(index=0, pc=0x80000000, instr=instr, size=4, vtype_after=0xD0)
    model = BackendModel()
    model.set_golden_trace(GoldenTrace([entry]))
    model.current_cycle = 10
    source = _queue_entry(entry.pc, instr, 0, 0)
    model._cfvec_queue = deque((source,))

    actions = model.plan_cycle_actions()

    assert actions.vtype_commit == (0xD0, False)
    assert actions.redirect_payload is None
    assert source.rob_commit_state == ROB_COMMIT_STATE_COMMITTED


class _Signal:
    def __init__(self) -> None:
        self.value = 0


def test_backend_agent_drives_vtype_fields_and_clears_valids() -> None:
    names = ("illegal", "vma", "vta", "vsew", "vlmul")
    signals = {
        f"{prefix}_{name}": _Signal()
        for prefix in ("commit_vtype_bits", "vsetvl_vtype")
        for name in names
    }
    signals.update(commit_vtype_valid=_Signal(), commit_vtype_has_vsetvl=_Signal())
    agent = BackendAgent()
    agent._to_ibuf_if = SimpleNamespace(**signals)

    agent.drive_vtype_commit((0x80000000000000D1, True))
    assert tuple(signals[f"vsetvl_vtype_{name}"].value for name in names) == (1, 1, 1, 2, 1)
    assert signals["commit_vtype_valid"].value == 1
    assert signals["commit_vtype_has_vsetvl"].value == 1

    agent.drive_vtype_commit(None)
    assert signals["commit_vtype_valid"].value == 0
    assert signals["commit_vtype_has_vsetvl"].value == 0

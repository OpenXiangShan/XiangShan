import pytest

from env.checkers.vtype_checker import VTypeChecker
from env.model.backend_runtime import CfVecCycleSnapshot, CfVecSlotSnapshot
from env.model.backend_state import PATH_STATE_CORRECT, ROB_COMMIT_STATE_COMMITTED, QueueInstr
from env.model.golden_trace import GoldenTrace, TraceEntry
from env.model.vtype import VTypeState


def _slot(
    cycle: int,
    pc: int,
    instr: int,
    raw_vtype: int,
    *,
    raw_specvtype: int | None = None,
) -> CfVecCycleSnapshot:
    def _fields(raw: int) -> dict[str, int | bool]:
        return dict(
            illegal=bool(raw >> 63),
            vma=bool(raw & 0x80),
            vta=bool(raw & 0x40),
            vsew=(raw >> 3) & 7,
            vlmul=raw & 7,
        )
    values = {
        f"{prefix}_{name}": value
        for prefix, fields in (
            ("vtype", _fields(raw_vtype)),
            (
                "specvtype",
                _fields(raw_vtype if raw_specvtype is None else raw_specvtype),
            ),
        )
        for name, value in fields.items()
    }
    return CfVecCycleSnapshot(cycle, (CfVecSlotSnapshot(slot=0, valid=True, instr=instr, **values),))


def _queue(cycle: int, pc: int, instr: int, index: int, *, committed: bool = False) -> QueueInstr:
    return QueueInstr(
        cycle=cycle, slot=0, pc=pc, instr=instr, is_rvc=False, pred_taken=False,
        ftq_flag=0, ftq_value=0, ftq_offset=0, is_last_in_entry=True,
        path_state=PATH_STATE_CORRECT, golden_index=index,
        rob_commit_state=ROB_COMMIT_STATE_COMMITTED if committed else "pending",
    )


def test_repeated_pc_uses_dynamic_trace_index() -> None:
    config = 0x0D0072D7
    nop = 0x13
    trace = GoldenTrace([
        TraceEntry(0, 0x100, config, 4, vtype_after=0xD0),
        TraceEntry(1, 0x104, nop, 4),
        TraceEntry(2, 0x100, config, 4, vtype_after=0xD1),
        TraceEntry(3, 0x104, nop, 4),
    ])
    checker = VTypeChecker.from_golden_trace(trace)
    for cycle, index, raw in ((1, 1, 0xD0), (2, 3, 0xD1)):
        snapshot = _slot(cycle, 0x104, nop, raw)
        checker.check_cycle(snapshot, [_queue(cycle, 0x104, nop, index)], lambda _: 0x104, cursor=index + 1)
    assert checker.checked_indices == {1, 3}


def test_keep_vl_trace_preserves_frontend_candidate_fields() -> None:
    config = 0x0D0072D7
    keep_vl = 0x0D807057
    reserved = 0x1D0072D7
    nop = 0x13
    trace = GoldenTrace([
        TraceEntry(0, 0x100, config, 4, vtype_after=0x87),
        TraceEntry(1, 0x104, keep_vl, 4, vtype_after=1 << 63),
        TraceEntry(2, 0x108, nop, 4),
        TraceEntry(3, 0x10C, reserved, 4, vtype_after=1 << 63),
    ])
    checker = VTypeChecker.from_golden_trace(trace)

    expected = checker._expected[1]
    assert expected.vtype == VTypeState(True, True, True, 0b011, 0)
    assert checker._expected[2].specvtype == expected.vtype
    assert checker._expected[3].vtype == VTypeState.initial()

    checker.check_cycle(
        _slot(1, 0x104, keep_vl, (1 << 63) | 0xD8, raw_specvtype=0x87),
        [_queue(1, 0x104, keep_vl, 1)],
        lambda _: 0x104,
        cursor=2,
    )
    assert checker.checked_indices == {1}


def test_uncommitted_vsetvl_younger_output_waits_for_replay() -> None:
    vsetvl = 0x80007057
    nop = 0x13
    trace = GoldenTrace([
        TraceEntry(0, 0x100, vsetvl, 4, vtype_after=0xD0),
        TraceEntry(1, 0x104, nop, 4),
    ])
    checker = VTypeChecker.from_golden_trace(trace)
    source = _queue(1, 0x100, vsetvl, 0)
    snapshot = _slot(2, 0x104, nop, 1 << 63)
    checker.check_cycle(snapshot, [source, _queue(2, 0x104, nop, 1)], lambda _: 0x104, cursor=2)
    assert checker.checked_count == 0

    source.rob_commit_state = ROB_COMMIT_STATE_COMMITTED
    replay = _slot(3, 0x104, nop, 0xD0)
    checker.check_cycle(replay, [source, _queue(3, 0x104, nop, 1)], lambda _: 0x104, cursor=2)
    assert checker.checked_indices == {1}


def test_vsetvl_itself_keeps_old_vtype_until_commit() -> None:
    vsetvl = 0x80007057
    checker = VTypeChecker.from_golden_trace(GoldenTrace([
        TraceEntry(0, 0x100, vsetvl, 4, vtype_after=0xD0),
    ]))
    snapshot = _slot(1, 0x100, vsetvl, 1 << 63)
    checker.check_cycle(snapshot, [_queue(1, 0x100, vsetvl, 0)], lambda _: 0x100, cursor=1)
    assert checker.checked_indices == {0}


def test_recovery_output_with_wrong_vtype_fails(caplog) -> None:
    vsetvl = 0x80007057
    nop = 0x13
    checker = VTypeChecker.from_golden_trace(GoldenTrace([
        TraceEntry(0, 0x100, vsetvl, 4, vtype_after=0xD0),
        TraceEntry(1, 0x104, nop, 4),
    ]))
    snapshot = _slot(5, 0x104, nop, 1 << 63)
    checker.check_cycle(snapshot, [], lambda _: 0x104, cursor=1)
    with pytest.raises(AssertionError, match=r"cycle=5 cursor=2.*field=specvtype"):
        checker.check_cycle(snapshot, [_queue(5, 0x104, nop, 1)], lambda _: 0x104, cursor=2)
    assert "cycle=5 cursor=2" in caplog.text


def test_completion_requires_vector_instruction_to_be_checked(caplog) -> None:
    checker = VTypeChecker.from_golden_trace(GoldenTrace([
        TraceEntry(0, 0x100, 0x80007057, 4, vtype_after=0xD0),
    ]))
    with pytest.raises(AssertionError, match="missing_vector_indices=\\[0\\]"):
        checker.check_completion(cycle=7, cursor=1)
    assert "cycle=7 cursor=1" in caplog.text


def test_completion_excludes_trace_before_start_cursor() -> None:
    checker = VTypeChecker.from_golden_trace(GoldenTrace([
        TraceEntry(0, 0x100, 0x80007057, 4, vtype_after=0xD0),
        TraceEntry(1, 0x104, 0x13, 4),
    ]), start_cursor=1)
    checker.check_completion(cycle=7, cursor=2)

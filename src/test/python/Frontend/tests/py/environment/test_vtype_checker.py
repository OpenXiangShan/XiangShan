import pytest

from env.checkers.vtype_checker import VTypeChecker
from env.model.backend_runtime import CfVecCycleSnapshot, CfVecSlotSnapshot
from env.model.golden_trace import TraceEntry
from env.model.vtype import VTypeModel


def _vsetvli(*, rd: int, rs1: int, zimm: int) -> int:
    return 0x57 | (rd << 7) | (0x7 << 12) | (rs1 << 15) | ((zimm & 0x7FF) << 20)


def _snapshot(*instructions):
    model = VTypeModel()
    slots = []
    for slot, instruction in enumerate(instructions):
        transition = model.step(instruction)
        slots.append(
            CfVecSlotSnapshot(
                slot=slot,
                valid=True,
                foldpc=0x80001000 + slot * 4,
                instr=instruction,
                specvtype_illegal=transition.specvtype.illegal,
                specvtype_vma=transition.specvtype.vma,
                specvtype_vta=transition.specvtype.vta,
                specvtype_vsew=transition.specvtype.vsew,
                specvtype_vlmul=transition.specvtype.vlmul,
                vtype_illegal=transition.vtype.illegal,
                vtype_vma=transition.vtype.vma,
                vtype_vta=transition.vtype.vta,
                vtype_vsew=transition.vtype.vsew,
                vtype_vlmul=transition.vtype.vlmul,
            )
        )
    return CfVecCycleSnapshot(cycle=9, slots=tuple(slots))


def test_vtype_checker_accepts_lane_ordered_snapshot() -> None:
    instructions = (_vsetvli(rd=1, rs1=0, zimm=0b010 | (0b010 << 3)), 0x00000013)
    checker = VTypeChecker(TraceEntry(i, 0x80001000 + 4 * i, instr, 4) for i, instr in enumerate(instructions))
    snapshot = _snapshot(*instructions)

    checker.check_snapshot(snapshot, lambda slot: snapshot.slots[slot].foldpc)
    checker.check_snapshot(snapshot, lambda slot: snapshot.slots[slot].foldpc)

    assert checker.get_stats() == {"checked_count": 4, "error_count": 0, "last_error": None}
    assert checker.observed_pcs == {0x80001000, 0x80001004}


@pytest.mark.parametrize(
    ("corrupted_field", "expected_field"),
    [("vtype_illegal", "vtype"), ("specvtype_illegal", "specvtype")],
)
def test_vtype_checker_reports_instruction_context_on_mismatch(corrupted_field, expected_field) -> None:
    checker = VTypeChecker([TraceEntry(0, 0x80001000, 0x00000013, 4)])
    snapshot = _snapshot(0x00000013)
    slot = snapshot.slots[0]
    bad_slot = slot.__class__(
        **{
            **slot.__dict__,
            corrupted_field: False,
        }
    )
    bad_snapshot = CfVecCycleSnapshot(cycle=9, slots=(bad_slot,))

    with pytest.raises(AssertionError, match=r"pc=0x80001000 instr=0x00000013"):
        checker.check_snapshot(bad_snapshot, lambda slot: bad_snapshot.slots[slot].foldpc)

    assert checker.error_count == 1
    assert checker.last_error["field"] == expected_field


def test_vtype_checker_rejects_wrong_instruction_at_expected_pc() -> None:
    checker = VTypeChecker([TraceEntry(0, 0x80001000, 0x00000013, 4)])
    snapshot = _snapshot(0x00000093)

    with pytest.raises(AssertionError, match="field=instr"):
        checker.check_snapshot(snapshot, lambda slot: snapshot.slots[slot].foldpc)

    assert checker.last_error["expected"] == 0x00000013
    assert checker.last_error["actual"] == 0x00000093


def test_vtype_checker_requires_dynamic_index_for_duplicate_trace_pc() -> None:
    entries = [TraceEntry(i, 0x80001000, 0x00000013, 4) for i in range(2)]
    checker = VTypeChecker(entries)
    with pytest.raises(ValueError, match="dynamic index for duplicate pc"):
        checker.check_snapshot(_snapshot(0x00000013), lambda _: 0x80001000)

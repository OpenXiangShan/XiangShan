from env.model.vtype import VTypeModel, VTypeState


def _vsetvli(*, rd: int, rs1: int, zimm: int) -> int:
    return (
        0x57
        | (int(rd) << 7)
        | (0x7 << 12)
        | (int(rs1) << 15)
        | ((int(zimm) & 0x7FF) << 20)
    )


def _vsetivli(*, rd: int, uimm: int, zimm: int) -> int:
    return (
        0xC0000057
        | (int(rd) << 7)
        | (0x7 << 12)
        | (int(uimm) << 15)
        | ((int(zimm) & 0x3FF) << 20)
    )


def test_vtype_model_starts_with_vill_and_propagates_lane_order() -> None:
    model = VTypeModel()
    first = _vsetvli(rd=5, rs1=0, zimm=0b11 | (0b010 << 3) | (1 << 6) | (1 << 7))
    second = _vsetivli(rd=5, uimm=4, zimm=0b001 | (0b001 << 3))

    transitions = model.step_lanes([first, 0x00000013, second, 0x00000013])

    assert transitions[0].specvtype == VTypeState.initial()
    assert transitions[0].vtype == VTypeState(False, True, True, 0b010, 0b011)
    assert transitions[1].specvtype == transitions[0].vtype
    assert transitions[1].vtype == transitions[0].vtype
    assert transitions[2].specvtype == transitions[0].vtype
    assert transitions[2].vtype == VTypeState(False, False, False, 0b001, 0b001)
    assert transitions[3].specvtype == transitions[2].vtype


def test_vtype_model_marks_reserved_and_unsupported_sew_lmul_illegal() -> None:
    model = VTypeModel()

    reserved = model.step(_vsetvli(rd=1, rs1=0, zimm=(1 << 8) | 0b000))
    unsupported = model.step(_vsetvli(rd=1, rs1=0, zimm=0b111 | (0b011 << 3)))

    assert reserved.vtype == VTypeState.initial()
    assert unsupported.vtype == VTypeState.initial()


def test_vtype_model_rejects_keep_vl_when_vlmax_changes() -> None:
    model = VTypeModel()
    model.step(_vsetvli(rd=1, rs1=0, zimm=0b000 | (0b010 << 3)))

    keep_vl = model.step(_vsetvli(rd=0, rs1=0, zimm=0b000 | (0b011 << 3)))

    assert keep_vl.vtype == VTypeState(True, False, False, 0b011, 0b000)

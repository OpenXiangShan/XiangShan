import pytest

from rm.ras_reference_model import RasReferenceModel, decode_ras_action


@pytest.mark.parametrize("instruction,action,width", [
    (0x000000EF, 2, 4),  # jal x1
    (0x000002EF, 2, 4),  # jal x5
    (0x0000006F, 0, 4),  # jal x0
    (0x00008067, 1, 4),  # jalr x0, 0(x1)
    (0x000280E7, 3, 4),  # jalr x1, 0(x5)
    (0x000080E7, 2, 4),  # jalr x1, 0(x1)
    (0x8082, 1, 2),  # c.jr x1
    (0x9082, 2, 2),  # c.jalr x1
    (0x9282, 3, 2),  # c.jalr x5
])
def test_decode_ras_action_from_instruction(instruction, action, width):
    assert decode_ras_action(instruction) == (action, width)


def test_ras_applies_decoded_instruction_length():
    model = RasReferenceModel()
    model.apply_instruction(0x80000100, 0x9082)
    assert model.apply_instruction(0x80000200, 0x8082) == 0x80000102


def test_ras_returns_call_pc_plus_instruction_length_in_lifo_order():
    model = RasReferenceModel()
    assert model.apply(2, 0x80000100, 4) is None
    assert model.apply(2, 0x80000200, 2) is None

    assert model.apply(1, 0x80000300, 4) == 0x80000202
    assert model.apply(1, 0x80000180, 4) == 0x80000104
    assert model.apply(1, 0x80000190, 4) is None


def test_ras_pop_and_push_uses_old_top_before_new_return_address():
    model = RasReferenceModel()
    model.apply(2, 0x80000100, 4)

    assert model.apply(3, 0x80000200, 4) == 0x80000104
    assert model.top() == 0x80000204
    assert model.apply(0, 0x80000220, 4) is None
    assert model.top() == 0x80000204


def test_ras_recovery_makes_old_top_unknown_but_new_call_known():
    model = RasReferenceModel()
    model.apply(2, 0x80000100, 4)
    model.invalidate()
    assert model.top() is None

    model.apply(2, 0x80000200, 2)
    assert model.apply(1, 0x80000300, 4) == 0x80000202
    assert model.apply(1, 0x80000304, 4) is None
    model.reset()
    assert model.top() is None


def test_ras_rejects_unsupported_action_and_call_width():
    model = RasReferenceModel()
    with pytest.raises(ValueError, match="invalid RAS action"):
        model.apply(4, 0x80000100, 4)
    with pytest.raises(ValueError, match="2 or 4 bytes"):
        model.apply(2, 0x80000100, 8)


def test_prediction_occurrences_use_event_identity_not_pc():
    model = RasReferenceModel()
    model.predict("first", 2, 0x100, 4)
    model.predict("second", 2, 0x100, 4)
    assert model.predict("return", 1, 0x200, 4) == 0x104
    assert model.top() == 0x104
    with pytest.raises(ValueError, match="duplicate"):
        model.predict("second", 2, 0x100, 4)


def test_redirect_restores_pre_event_state_and_replaces_wrong_path():
    model = RasReferenceModel()
    model.predict("older", 2, 0x100, 4)
    model.predict("wrong", 1, 0x200, 4)
    model.predict("younger", 2, 0x300, 4)

    assert model.redirect("wrong", 3, 0x200, 2) == 0x104
    assert model.top() == 0x202
    with pytest.raises(ValueError, match="unknown"):
        model.commit("younger")
    model.commit("wrong")
    assert model.top() == 0x202


def test_commit_keeps_younger_checkpoint_and_does_not_reapply_call():
    model = RasReferenceModel()
    model.predict("committed", 2, 0x100, 4)
    model.predict("younger", 2, 0x200, 2)
    model.commit("committed")
    assert model.top() == 0x202
    assert model.redirect("younger", 0, 0x200, 2) is None
    assert model.top() == 0x104


def test_invalid_redirect_does_not_discard_pending_events():
    model = RasReferenceModel()
    model.predict("older", 2, 0x100, 4)
    model.predict("younger", 2, 0x200, 2)
    with pytest.raises(ValueError, match="2 or 4 bytes"):
        model.redirect("older", 2, 0x100, 8)
    assert model.top() == 0x202
    model.commit("younger")
    assert model.top() == 0x202


def test_unknown_recovery_clears_checkpoints_but_preserves_unknown_base():
    model = RasReferenceModel()
    model.predict("wrong", 2, 0x100, 4)
    model.invalidate()
    with pytest.raises(ValueError, match="unknown"):
        model.redirect("wrong", 0, 0x100, 4)
    model.predict("return", 1, 0x200, 4)
    model.predict("call", 2, 0x300, 2)
    assert model.predict("return2", 1, 0x400, 4) == 0x302
    assert model.top() is None
    model.reset()
    with pytest.raises(ValueError, match="unknown"):
        model.commit("return2")


def test_capacity_boundary_forgets_old_history_but_allows_new_known_calls():
    model = RasReferenceModel()
    for depth in range(16):
        model.apply(2, 0x100 + depth * 4, 4)
    assert model.top() == 0x140

    model.apply(2, 0x200, 4)
    assert model.top() is None
    model.apply(2, 0x300, 2)
    assert model.apply(1, 0x400, 4) == 0x302
    assert model.apply(1, 0x404, 4) is None


def test_redirect_before_capacity_boundary_keeps_old_checkpoint_unknown():
    model = RasReferenceModel()
    model.predict("base", 2, 0x100, 4)
    model.predict("before", 0, 0x104, 4)
    for depth in range(16):
        model.predict(depth, 2, 0x200 + depth * 4, 4)
    assert model.top() is None

    model.redirect(15, 0, 0x23c, 4)
    assert model.top() is None
    model.redirect("before", 0, 0x100, 4)
    assert model.top() is None
    model.predict("new", 2, 0x300, 4)
    assert model.predict("return", 1, 0x400, 4) == 0x304

from rm.ubtb_reference_model import UbtbPrediction, UbtbReferenceModel


def test_ubtb_learns_taken_branch_and_reconstructs_target_from_lookup_pc():
    model = UbtbReferenceModel()
    pc = 0x8000_0040
    assert model.predict(pc) is None

    model.train(pc, taken=False, position=4, target=0x8000_0100,
                branch_type=1, ras_action=0)
    assert model.predict(pc) is None

    model.train(pc, taken=True, position=4, target=0x8000_0100,
                branch_type=1, ras_action=0)
    assert model.predict(pc) == UbtbPrediction(4, 0x8000_0100, 1, 0)
    assert model.predict(pc + (1 << 23)) == UbtbPrediction(
        4, pc + (1 << 23) + 0xC0, 1, 0,
    )

    model.reset()
    assert model.predict(pc) is None


def test_ubtb_wrong_outcomes_age_entry_until_it_stops_predicting():
    model = UbtbReferenceModel()
    pc = 0x8000_0040
    training = dict(position=2, target=0x8000_0100, branch_type=2, ras_action=0)
    model.train(pc, taken=True, **training)
    for _ in range(2):
        model.train(pc, taken=False, **training)
        assert model.predict(pc) is not None
    model.train(pc, taken=False, **training)
    assert model.predict(pc) is None

    model.train(pc, taken=True, position=3, target=0x8000_0200,
                branch_type=2, ras_action=0)
    assert model.predict(pc) == UbtbPrediction(3, 0x8000_0200, 2, 0)

from rm.mbtb_reference_model import MbtbPrediction, MbtbReferenceModel


def test_mbtb_predicts_known_entries_in_two_half_aligned_regions():
    model = MbtbReferenceModel()
    model.train(0x8000001E, target=0x80001000, branch_type=2, ras_action=2)
    model.train(0x80000022, target=0x80002000, branch_type=1, ras_action=0)

    assert model.predict(0x8000000A) == (
        MbtbPrediction(0x8000001E, 15, 0x80001000, 2, 2),
        MbtbPrediction(0x80000022, 17, 0x80002000, 1, 0),
    )
    assert model.predict(0x80000020) == (
        MbtbPrediction(0x80000022, 1, 0x80002000, 1, 0),
    )


def test_mbtb_filters_entries_before_start_and_across_page():
    model = MbtbReferenceModel()
    model.train(0x80000FC2, target=0x80000400, branch_type=2, ras_action=0)
    model.train(0x80000FE2, target=0x80000500, branch_type=2, ras_action=0)
    model.train(0x80001002, target=0x80000600, branch_type=2, ras_action=0)

    assert [p.cfi_pc for p in model.predict(0x80000FC0)] == [0x80000FC2, 0x80000FE2]
    assert [p.cfi_pc for p in model.predict(0x80000FE0)] == [0x80000FE2]
    assert [p.cfi_pc for p in model.predict(0x80000FE4)] == []


def test_mbtb_reconstructs_target_from_query_and_resets():
    model = MbtbReferenceModel()
    cfi_pc = 0x80100040
    model.train(cfi_pc, target=0x80000080, branch_type=2, ras_action=0)
    assert model.predict(cfi_pc) == (
        MbtbPrediction(cfi_pc, 0, 0x80000080, 2, 0),
    )
    alias_pc = cfi_pc + (1 << 21)
    assert model.predict(alias_pc) == ()

    model.train(alias_pc, target=0x802000C0, branch_type=2, ras_action=0)
    model.reset()
    assert model.predict(cfi_pc) == ()
    assert model.predict(alias_pc) == ()


def test_mbtb_target_carry_corrects_cross_boundary_jump():
    model = MbtbReferenceModel()
    cfi_pc = 0x801FFFE2
    model.train(cfi_pc, target=0x80200020, branch_type=2, ras_action=0)

    assert model.predict(0x801FFFE0) == (
        MbtbPrediction(cfi_pc, 1, 0x80200020, 2, 0),
    )


def test_mbtb_target_carry_on_indirect_return_with_zero_training_target():
    model = MbtbReferenceModel()
    model.train(
        0x80000118, start_pc=0x8000011C, target=0,
        branch_type=3, ras_action=1,
    )

    assert model.predict(0x80000100) == (
        MbtbPrediction(0x80000118, 12, 0x7FE00000, 3, 1),
    )


def test_mbtb_not_taken_training_keeps_known_branch_target():
    model = MbtbReferenceModel()
    cfi_pc = 0x80000114
    model.train(cfi_pc, target=0x8000011A, branch_type=1, ras_action=0)
    model.train(
        cfi_pc, target=0x80000116, branch_type=1, ras_action=0,
        taken=False,
    )

    assert model.predict(0x80000100)[0].target == 0x8000011A
    assert model.entries[(model.set_index(cfi_pc), 0)].taken_count == 1


def test_mbtb_not_taken_miss_does_not_allocate_or_evict():
    model = MbtbReferenceModel()
    base = 0x80000560
    for index in range(4):
        model.train(base + 2 * index, target=0x800005AA, branch_type=1, ras_action=0)
    before = model.predict(base)

    for pc in (base + 0x12, base + 0x16, base + 0x1C):
        model.train(pc, target=pc + 2, branch_type=1, ras_action=0, taken=False)

    assert model.predict(base) == before


def test_mbtb_metadata_versions_retire_after_new_target_is_observed():
    model = MbtbReferenceModel()
    cfi_pc = 0x80000286
    start_pc = 0x80000272
    old = (0x80000338, 3, 0)
    new = (0x80000392, 3, 0)
    model.train(cfi_pc, target=old[0], branch_type=3, ras_action=0)
    model.train(cfi_pc, target=new[0], branch_type=3, ras_action=0)

    assert model.allowed_metadata(cfi_pc, start_pc) == (old, new)
    model.observe_metadata(cfi_pc, start_pc, old)
    assert model.allowed_metadata(cfi_pc, start_pc) == (old, new)
    model.observe_metadata(cfi_pc, start_pc, new)
    assert model.allowed_metadata(cfi_pc, start_pc) == (new,)
    model.reset()
    assert model.allowed_metadata(cfi_pc, start_pc) == ()


def test_mbtb_replaces_lru_way_and_only_touches_final_taken_branch():
    model = MbtbReferenceModel()
    pcs = [0x80000000 + 2 * index for index in range(5)]
    for pc in pcs[:4]:
        model.train(pc, target=0x80000100, branch_type=1, ras_action=0)

    assert model.touch(pcs[0])
    model.train(pcs[4], target=0x80000200, branch_type=2, ras_action=0)
    assert [p.cfi_pc for p in model.predict(pcs[0])] == [pcs[0], pcs[2], pcs[3], pcs[4]]

    # Prediction touch is supplied only for the final taken branch.
    assert model.touch(pcs[3])
    model.train(pcs[1], target=0x80000300, branch_type=2, ras_action=0)
    assert [p.cfi_pc for p in model.predict(pcs[0])] == [pcs[0], pcs[1], pcs[3], pcs[4]]


def test_mbtb_conflicting_regions_replace_within_the_same_set():
    model = MbtbReferenceModel()
    pcs = [0x80000000 + (index << 16) for index in range(5)]
    for pc in pcs[:4]:
        model.train(pc, target=pc + 0x80, branch_type=2, ras_action=0)
    assert all(model.predict(pc) for pc in pcs[:4])

    model.touch(pcs[0])
    model.train(pcs[4], target=pcs[4] + 0x80, branch_type=2, ras_action=0)
    assert model.predict(pcs[0])
    assert model.predict(pcs[1]) == ()
    assert model.predict(pcs[4])


def test_mbtb_counter_saturates_and_unconditional_jump_direction_is_unchecked():
    model = MbtbReferenceModel()
    pc = 0x80000000
    for taken in (True, True, False, False, False):
        model.train(pc, target=0x80000080, branch_type=1, ras_action=0, taken=taken)
    assert model.predict(pc)[0].taken is False
    model.train(pc, target=0x80000080, branch_type=1, ras_action=0, taken=True)
    assert model.predict(pc)[0].taken is False
    model.train(pc, target=0x80000080, branch_type=1, ras_action=0, taken=True)
    assert model.predict(pc)[0].taken is True
    model.train(pc + 2, target=0x80000100, branch_type=3, ras_action=1)
    assert model.predict(pc)[1].taken is True
    assert model.predict(pc)[1].direction_known is False


def test_mbtb_direction_is_certain_after_three_same_outcomes():
    model = MbtbReferenceModel()
    pc = 0x80000000
    model.train(pc, target=0x80000080, branch_type=1, ras_action=0, taken=True)
    for taken in (False, False):
        model.train(pc, target=0x80000080, branch_type=1, ras_action=0, taken=taken)
    assert model.predict(pc)[0].direction_known is False
    model.train(pc, target=0x80000080, branch_type=1, ras_action=0, taken=False)
    assert model.predict(pc)[0].direction_known is True
    assert model.predict(pc)[0].taken is False

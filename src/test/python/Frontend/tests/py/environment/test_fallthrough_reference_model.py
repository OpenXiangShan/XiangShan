import pytest

from rm.fallthrough_reference_model import (
    FallThroughPrediction,
    FallThroughReferenceModel,
)


@pytest.mark.parametrize(
    ("start_pc", "target", "position"),
    [
        (0x80000000, 0x80000040, 31),
        (0x8000001E, 0x80000040, 31),
        (0x80000020, 0x80000060, 31),
        (0x80000FA0, 0x80000FE0, 31),
        (0x80000FC0, 0x80001000, 31),
        (0x80000FE0, 0x80001000, 15),
        (0x80000FFE, 0x80001000, 15),
    ],
)
def test_fallthrough_predicts_half_align_and_page_boundaries(
    start_pc: int, target: int, position: int,
) -> None:
    assert FallThroughReferenceModel.predict(start_pc) == FallThroughPrediction(
        target, position,
    )

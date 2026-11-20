from types import SimpleNamespace

import pytest

from tests.py.support.cfvec import iter_valid_cfvec_slots, read_cfvec_exception_bits


def test_valid_cfvec_slots_keep_strict_validity_and_slot_order() -> None:
    observe = SimpleNamespace(
        cfvec_valid=[SimpleNamespace(value=value) for value in (1, 0, 2, 1, -1, 0, 0, 1)],
    )
    assert list(iter_valid_cfvec_slots(observe)) == [0, 3, 7]


def test_valid_cfvec_slots_are_read_lazily() -> None:
    observe = SimpleNamespace(
        cfvec_valid=[SimpleNamespace(value=0) for _ in range(8)],
    )
    observe.cfvec_valid[0].value = 1
    slots = iter_valid_cfvec_slots(observe)
    assert next(slots) == 0
    observe.cfvec_valid[7].value = 1
    assert list(slots) == [7]


@pytest.mark.parametrize(
    "values,strict_bits,nonzero_bits",
    (
        ((0, 0, 0, 0, 0), (), ()),
        ((1, 1, 1, 1, 1), (1, 2, 12, 19, 20), (1, 2, 12, 19, 20)),
        ((None, 1, 0, 1, None), (2, 19), (2, 19)),
        ((2, -1, 1, 0, 3), (12,), (1, 2, 12, 20)),
        (("1", "0", "2", None, "-1"), (1,), (1, 12, 20)),
    ),
)
def test_cfvec_exception_policies(values, strict_bits, nonzero_bits) -> None:
    observe = SimpleNamespace()
    for bit, value in zip((1, 2, 12, 19, 20), values):
        signals = [SimpleNamespace(value=0) for _ in range(8)]
        signals[5].value = value
        setattr(observe, f"cfvec_exception_vec_{bit}", signals)
    assert read_cfvec_exception_bits(observe, 5) == strict_bits
    assert read_cfvec_exception_bits(observe, 5, nonzero=True) == nonzero_bits


def test_cfvec_missing_signal_value_is_zero_but_missing_vector_is_error() -> None:
    observe = SimpleNamespace(**{
        f"cfvec_exception_vec_{bit}": [SimpleNamespace()]
        for bit in (1, 2, 12, 19, 20)
    })
    assert read_cfvec_exception_bits(observe, 0) == ()
    assert read_cfvec_exception_bits(observe, 0, nonzero=True) == ()
    del observe.cfvec_exception_vec_12
    with pytest.raises(AttributeError):
        read_cfvec_exception_bits(observe, 0)


def test_cfvec_required_value_preserves_backend_fault_signal_contract() -> None:
    observe = SimpleNamespace(**{
        f"cfvec_exception_vec_{bit}": [SimpleNamespace(value=None)]
        for bit in (1, 2, 12, 19, 20)
    })
    assert read_cfvec_exception_bits(observe, 0, require_value=True) == ()
    observe.cfvec_exception_vec_1[0].value = ""
    assert read_cfvec_exception_bits(observe, 0, require_value=True) == ()
    del observe.cfvec_exception_vec_12[0].value
    with pytest.raises(AttributeError):
        read_cfvec_exception_bits(observe, 0, require_value=True)

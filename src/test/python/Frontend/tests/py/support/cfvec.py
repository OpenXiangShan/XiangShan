"""CFVec signal readers shared by DUT test observers."""

from collections.abc import Iterator


def iter_valid_cfvec_slots(observe) -> Iterator[int]:
    for slot in range(8):
        if int(observe.cfvec_valid[slot].value) == 1:
            yield slot


def read_cfvec_exception_bits(
    observe, slot: int, *, nonzero: bool = False, require_value: bool = False,
) -> tuple[int, ...]:
    """Keep strict one-bit and nonzero exception policies distinct."""
    bits = []
    for bit in (1, 2, 12, 19, 20):
        signal = getattr(observe, f"cfvec_exception_vec_{bit}")[slot]
        raw = (signal.value or 0) if require_value else getattr(signal, "value", None)
        value = 0 if raw is None else int(raw)
        if (value != 0 if nonzero else value == 1):
            bits.append(bit)
    return tuple(bits)

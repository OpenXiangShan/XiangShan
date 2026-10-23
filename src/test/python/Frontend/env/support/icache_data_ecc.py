"""V3 SRAM geometry and Data ECC scenario names shared by stimulus/observers.

These helpers describe address ranges and stimulus combinations, not DUT ECC
outputs or coverage hit decisions. The canonical CSV owns Bin_ID allocation.
"""

from dataclasses import dataclass
from itertools import product


@dataclass(frozen=True)
class BankRead:
    valid: int
    ready: int
    waymask: int
    sets: tuple[int, ...]


def parity(data: int) -> int:
    assert 0 <= data < 1 << 64, "data must fit one 64-bit bank"
    return data.bit_count() & 1


def request_bank_masks(start_pc: int, end_position: int) -> tuple[int, int]:
    """endPosition is a halfword index from the 32-byte aligned fetch block."""
    assert start_pc >= 0 and start_pc % 2 == 0, "unaligned request PC"
    assert 0 <= end_position < 32, "invalid endPosition"
    end_pc = (start_pc & ~31) + 2 * end_position
    assert start_pc <= end_pc < start_pc + 64, "invalid fetch range"
    masks = [0, 0]
    for pc in range(start_pc, end_pc + 1, 2):
        line, offset = divmod(pc - (start_pc & ~63), 64)
        masks[line] |= 1 << (offset // 8)
    assert not masks[0] & masks[1], "one bank cannot read two lines in one request"
    return tuple(masks)


SINGLE_BANKS = tuple((bank,) for bank in range(8)) + ((0, 1), (0, 7), tuple(range(8)))
LINE0_BANKS = tuple((bank,) for bank in range(4, 8)) + ((6, 7), tuple(range(4, 8)))
LINE1_BANKS = tuple((bank,) for bank in range(4)) + ((1, 2), tuple(range(4)))
_pairs = [((left,), (right,)) for left, right in product(range(4, 8), range(4))]
_pairs += [pair for pair in product(((6,), (6, 7), tuple(range(4, 8))),
                                  ((1,), (0, 1, 2), tuple(range(4)))) if pair not in _pairs]
BOTH_BANKS = tuple(_pairs)


def fault_banks(kind, banks):
    if kind == "both":
        return tuple((line, bank) for line, selected in enumerate(banks) for bank in selected)
    return tuple((int(kind == "line1"), bank) for bank in banks)


def scenario_bin_name(kind, faults):
    """Names are unique within the group, including across its four points."""
    suffix = "_".join(f"l{line}b{bank}" for line, bank in sorted(faults))
    return f"{kind}_{suffix}"


SCENARIO_CASES = {"single": SINGLE_BANKS, "line0": LINE0_BANKS,
                  "line1": LINE1_BANKS, "both": BOTH_BANKS}
SCENARIO_POINTS = {"single": "single_line", "line0": "cross_line0",
                   "line1": "cross_line1", "both": "cross_both"}

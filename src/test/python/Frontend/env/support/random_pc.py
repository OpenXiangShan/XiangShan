"""Reproducible fetch-PC placement; callers must apply the returned memory attributes."""

from __future__ import annotations

import random
from dataclasses import dataclass
from typing import Literal


@dataclass(frozen=True)
class PcBoundary:
    size: int
    displacement: int = 0
    width: int = 4
    relation: Literal["cross", "within", "start"] = "cross"

    def matches(self, pc: int) -> bool:
        if self.size <= 0 or self.width <= 0 or self.width > self.size:
            raise ValueError("boundary size and width must be positive, with width <= size")
        offset = (pc + self.displacement) % self.size
        if self.relation == "cross":
            return offset + self.width > self.size
        if self.relation == "within":
            return offset + self.width <= self.size
        if self.relation == "start":
            return offset == 0
        raise ValueError(f"unknown boundary relation: {self.relation}")


@dataclass(frozen=True)
class PcMemory:
    pbmt: int
    pma_cacheable: bool
    mark_mmio: bool


# An intent, not a side effect: page tables, PMP/PMA, MMIO ranges and payloads
# must be configured by the test for the selected physical address.
PC_MEMORY = {
    "cacheable": PcMemory(pbmt=0, pma_cacheable=True, mark_mmio=False),
    "uncache": PcMemory(pbmt=1, pma_cacheable=True, mark_mmio=False),
    "mmio": PcMemory(pbmt=0, pma_cacheable=False, mark_mmio=True),
}


@dataclass(frozen=True)
class RandomPc:
    va: int
    pa: int
    memory: PcMemory

    @property
    def pc(self) -> int:
        return self.va


def random_pc(
    rng: random.Random,
    *,
    va_range: tuple[int, int],
    pa_range: tuple[int, int],
    slot_size: int = 0x1000,
    alignment: int = 2,
    offsets: tuple[int, ...] | None = None,
    boundaries: tuple[PcBoundary, ...] = (),
    same_address: bool = False,
    loaded_pa_range: tuple[int, int] | None = None,
    instruction_bytes: int = 2,
    memory: Literal["cacheable", "uncache", "mmio"] = "cacheable",
) -> RandomPc:
    """Pick aligned slots with a shared offset; ranges are half-open.

    A slot can reserve contiguous pages for a payload. Boundary sizes must
    divide slot_size so their relations hold for every randomly chosen slot.
    loaded_pa_range checks the first instruction in a contiguous program image;
    it does not load the image or configure the address translation.
    """
    if slot_size <= 0 or alignment <= 0 or slot_size % alignment:
        raise ValueError("slot_size must be positive and divisible by alignment")
    if memory not in PC_MEMORY:
        raise ValueError(f"unknown PC memory kind: {memory}")
    if instruction_bytes <= 0:
        raise ValueError("instruction_bytes must be positive")
    if loaded_pa_range is not None:
        lo, hi = loaded_pa_range
        if lo < 0 or hi <= lo:
            raise ValueError("loaded_pa_range must be a nonempty non-negative half-open interval")

    def slots(address_range: tuple[int, int]) -> range:
        lo, hi = address_range
        if lo < 0 or hi <= lo:
            raise ValueError("PC range must be a nonempty non-negative half-open interval")
        first = (lo + slot_size - 1) // slot_size
        stop = hi // slot_size
        if stop <= first:
            raise ValueError("PC range has no complete aligned slot")
        return range(first, stop)

    va_slots, pa_slots = slots(va_range), slots(pa_range)
    for boundary in boundaries:
        if boundary.size <= 0 or slot_size % boundary.size:
            raise ValueError("boundary size must be positive and divide slot_size")
        boundary.matches(0)  # Validate even when no offset satisfies the relation.

    candidates = offsets if offsets is not None else range(0, slot_size, alignment)
    valid = []
    for offset in candidates:
        if not (0 <= offset < slot_size and offset % alignment == 0
                and all(boundary.matches(offset) for boundary in boundaries)):
            continue
        eligible_pa = pa_slots
        if loaded_pa_range is not None:
            lo, hi = loaded_pa_range
            first = max(eligible_pa.start, (lo - offset + slot_size - 1) // slot_size)
            stop = min(eligible_pa.stop, (hi - instruction_bytes - offset) // slot_size + 1)
            eligible_pa = range(first, stop) if stop > first else range(0)
        eligible_va = va_slots
        if same_address:
            first = max(eligible_va.start, eligible_pa.start)
            stop = min(eligible_va.stop, eligible_pa.stop)
            eligible_va = eligible_pa = range(first, stop) if stop > first else range(0)
        if eligible_va and eligible_pa:
            valid.append((offset, eligible_va, eligible_pa))
    if not valid:
        raise ValueError("no PC satisfies the requested constraints")

    offset, eligible_va, eligible_pa = rng.choice(valid)
    va_slot = rng.choice(eligible_va)
    pa_slot = va_slot if same_address else rng.choice(eligible_pa)
    return RandomPc(
        va=va_slot * slot_size + offset,
        pa=pa_slot * slot_size + offset,
        memory=PC_MEMORY[memory],
    )

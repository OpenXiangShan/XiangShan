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
    address: Literal["va", "pa"] = "va"

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
class PcMapping:
    va_start: int
    pa_start: int
    size: int


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
    va_range: tuple[int, int] | None = None,
    pa_range: tuple[int, int] | None = None,
    mappings: tuple[PcMapping, ...] | None = None,
    slot_size: int = 0x1000,
    alignment: int = 2,
    offsets: tuple[int, ...] | None = None,
    entry_offsets: tuple[int, ...] | None = None,
    boundaries: tuple[PcBoundary, ...] = (),
    same_address: bool = False,
    loaded_pa_range: tuple[int, int] | None = None,
    instruction_bytes: int = 2,
    memory: Literal["cacheable", "uncache", "mmio"] | PcMemory = "cacheable",
) -> RandomPc:
    """Pick a fetch entry from aligned slots or explicitly mapped regions.

    Mappings pair VA/PA spans, including independently mapped physical pages.
    entry_offsets are required with mappings, relative to each mapping's start,
    and identify valid instruction starts. Ranges are half-open. This does not
    load the image or configure translation, memory attributes, or expected PCs.
    """
    if slot_size <= 0 or alignment <= 0 or slot_size % alignment:
        raise ValueError("slot_size must be positive and divisible by alignment")
    if not isinstance(memory, PcMemory) and memory not in PC_MEMORY:
        raise ValueError(f"unknown PC memory kind: {memory}")
    if mappings is None:
        if va_range is None or pa_range is None or entry_offsets is not None:
            raise ValueError("slot sampling requires va_range and pa_range, not entry_offsets")
    elif (not mappings or not entry_offsets or va_range is not None or pa_range is not None
          or offsets is not None):
        raise ValueError("mapped sampling requires mappings and entry_offsets, without ranges or slot offsets")
    if instruction_bytes <= 0:
        raise ValueError("instruction_bytes must be positive")
    if loaded_pa_range is not None:
        lo, hi = loaded_pa_range
        if lo < 0 or hi <= lo:
            raise ValueError("loaded_pa_range must be a nonempty non-negative half-open interval")
    for boundary in boundaries:
        if boundary.address not in ("va", "pa"):
            raise ValueError(f"unknown boundary address: {boundary.address}")
        boundary.matches(0)

    def matches(va: int, pa: int) -> bool:
        return all(boundary.matches(va if boundary.address == "va" else pa) for boundary in boundaries)

    def loaded(pa: int) -> bool:
        return loaded_pa_range is None or (
            loaded_pa_range[0] <= pa and pa + instruction_bytes <= loaded_pa_range[1]
        )

    if mappings is not None:
        valid = []
        for mapping in mappings:
            if mapping.va_start < 0 or mapping.pa_start < 0 or mapping.size <= 0:
                raise ValueError("mapped PC spans must be nonempty and non-negative")
            if mapping.va_start % 0x1000 != mapping.pa_start % 0x1000:
                raise ValueError("mapped VA and PA must share their 4 KiB page offset")
            assert entry_offsets is not None
            for offset in entry_offsets:
                va, pa = mapping.va_start + offset, mapping.pa_start + offset
                if (0 <= offset < mapping.size and va % alignment == 0 and pa % alignment == 0
                        and (not same_address or va == pa) and loaded(pa) and matches(va, pa)):
                    valid.append((va, pa))
        if not valid:
            raise ValueError("no PC satisfies the requested constraints")
        va, pa = rng.choice(valid)
        return RandomPc(va=va, pa=pa, memory=memory if isinstance(memory, PcMemory) else PC_MEMORY[memory])

    def slots(address_range: tuple[int, int]) -> range:
        lo, hi = address_range
        if lo < 0 or hi <= lo:
            raise ValueError("PC range must be a nonempty non-negative half-open interval")
        first = (lo + slot_size - 1) // slot_size
        stop = hi // slot_size
        if stop <= first:
            raise ValueError("PC range has no complete aligned slot")
        return range(first, stop)

    assert va_range is not None and pa_range is not None
    va_slots, pa_slots = slots(va_range), slots(pa_range)

    def eligible_slots(slot_candidates: range, offset: int, constraints: tuple[PcBoundary, ...]):
        if any(not boundary.matches(offset) for boundary in constraints if slot_size % boundary.size == 0):
            return range(0)
        varying = tuple(boundary for boundary in constraints if slot_size % boundary.size)
        if not varying:
            return slot_candidates
        return [slot for slot in slot_candidates if all(
            boundary.matches(slot * slot_size + offset) for boundary in varying
        )]

    va_boundaries = tuple(boundary for boundary in boundaries if boundary.address == "va")
    pa_boundaries = tuple(boundary for boundary in boundaries if boundary.address == "pa")
    candidates = offsets if offsets is not None else range(0, slot_size, alignment)
    valid = []
    for offset in candidates:
        if not (0 <= offset < slot_size and offset % alignment == 0):
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
        if same_address:
            eligible_va = eligible_slots(eligible_va, offset, boundaries)
            eligible_pa = eligible_va
        else:
            eligible_va = eligible_slots(eligible_va, offset, va_boundaries)
            eligible_pa = eligible_slots(eligible_pa, offset, pa_boundaries)
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
        memory=memory if isinstance(memory, PcMemory) else PC_MEMORY[memory],
    )

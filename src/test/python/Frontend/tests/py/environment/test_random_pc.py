from __future__ import annotations

import random

import pytest

from env.support import PC_MEMORY, PcBoundary, random_pc, scenario_rng


_VA = (0x80000000, 0x80080000)
_PA = (0x80400000, 0x80480000)


@pytest.mark.parametrize("kind,pbmt,cacheable,mmio", [
    ("cacheable", 0, True, False),
    ("uncache", 1, True, False),
    ("mmio", 0, False, True),
])
def test_random_pc_preserves_mapping_offset_and_memory_intent(kind, pbmt, cacheable, mmio) -> None:
    placed = random_pc(
        random.Random(11), va_range=_VA, pa_range=_PA, memory=kind,
        slot_size=0x2000, offsets=(0xF9C, 0xFBC),
        boundaries=(PcBoundary(32, displacement=4, width=1, relation="start"),),
    )
    assert _VA[0] <= placed.pc < _VA[1]
    assert _PA[0] <= placed.pa < _PA[1]
    assert placed.pc % 0x2000 == placed.pa % 0x2000
    assert placed.pc % 0x1000 in (0xF9C, 0xFBC)
    assert (placed.pc + 4) % 32 == 0
    assert placed.memory == PC_MEMORY[kind]
    assert (placed.memory.pbmt, placed.memory.pma_cacheable, placed.memory.mark_mmio) == (
        pbmt, cacheable, mmio,
    )


def test_random_pc_combines_cross_page_and_cross_cacheline() -> None:
    boundary = (PcBoundary(0x1000), PcBoundary(64))
    pcs = [random_pc(random.Random(seed), va_range=_VA, pa_range=_PA, boundaries=boundary)
           for seed in range(20)]
    assert all(pc.pc % 0x1000 == 0xFFE and pc.pa % 0x1000 == 0xFFE for pc in pcs)
    assert len({pc.pc for pc in pcs}) > 1


def test_random_pc_replays_with_scenario_seed() -> None:
    def draw(base_seed):
        _, _, rng = scenario_rng("random-pc/replay", base_seed=base_seed)
        return random_pc(rng, va_range=_VA, pa_range=_PA,
                         boundaries=(PcBoundary(64, width=4, relation="within"),))

    assert draw(1) == draw(1)
    assert draw(1) != draw(2)


def test_random_pc_can_randomize_bare_pc_across_multiple_slots() -> None:
    addresses = {
        random_pc(random.Random(seed), va_range=_VA, pa_range=_VA, same_address=True)
        for seed in range(20)
    }
    assert len({placed.pc // 0x1000 for placed in addresses}) > 1
    assert all(placed.pc == placed.pa for placed in addresses)


def test_random_pc_selects_an_instruction_inside_preloaded_mmio_image() -> None:
    image = (0x10001040, 0x10001060)
    for seed in range(20):
        placed = random_pc(
            random.Random(seed), va_range=(0x10001000, 0x10003000),
            pa_range=(0x10001000, 0x10003000), same_address=True,
            loaded_pa_range=image, instruction_bytes=4, memory="mmio",
        )
        assert image[0] <= placed.pc and placed.pc + 4 <= image[1]
        assert placed.memory.mark_mmio


@pytest.mark.parametrize("options", [
    {"boundaries": (PcBoundary(0x1000), PcBoundary(0x1000, relation="start"))},
    {"offsets": (0xFFF,), "alignment": 2},
    {"boundaries": (PcBoundary(24),)},
    {"boundaries": (PcBoundary(64, width=65),)},
    {"pa_range": (0x80400001, 0x80401000)},
    {"same_address": True},
    {"loaded_pa_range": (0x80400000, 0x80400001), "instruction_bytes": 4},
    {"loaded_pa_range": (0x80400000, 0x80400020), "instruction_bytes": 0},
    {"memory": "not-a-memory-kind"},
])
def test_random_pc_rejects_invalid_or_unsatisfiable_constraints(options) -> None:
    with pytest.raises(ValueError):
        random_pc(random.Random(1), **{"va_range": _VA, "pa_range": _PA, **options})

from __future__ import annotations

import pytest

from env.model import PmpPmaPermissionModel
from env.sequences import TranslationPmpPmaEntry
from env.support import PmpPmaConfig, reconstruct_pmp_request_addr


def _entry(kind: str, index: int, config: PmpPmaConfig, addr: int, size: int | None = None) -> TranslationPmpPmaEntry:
    return TranslationPmpPmaEntry(kind=kind, index=index, config=config, addr=addr, size=size)


def test_pmp_napot_requires_the_complete_instruction_range() -> None:
    result = PmpPmaPermissionModel.check_instruction(
        0x1FF8,
        size=8,
        pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", execute=True), 0x1000, 0x1000),),
        pma_enabled=False,
    )
    assert result.pmp_match_index == 0
    assert result.execute_allowed is True

    outside = PmpPmaPermissionModel.check_instruction(
        0x1FFC,
        size=8,
        pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", execute=True), 0x1000, 0x1000),),
        pma_enabled=False,
    )
    assert outside.pmp_match_index is None
    assert outside.pmp_execute_denied is True


def test_tor_uses_the_immediately_previous_entry_as_its_lower_bound() -> None:
    entries = (
        _entry("pma", 0, PmpPmaConfig(match="off"), 0x4000),
        _entry("pma", 1, PmpPmaConfig(match="tor", execute=True, cacheable=True), 0x5000),
    )

    inside = PmpPmaPermissionModel.check_instruction(0x4FF0, size=16, pma_entries=entries, pmp_enabled=False)
    crossing = PmpPmaPermissionModel.check_instruction(0x4FF8, size=16, pma_entries=entries, pmp_enabled=False)

    assert (inside.pma_match_index, inside.execute_allowed, inside.pma_cacheable) == (1, True, True)
    assert crossing.pma_match_index is None
    assert crossing.pma_execute_denied is True


def test_lowest_index_match_wins_for_pmp_and_pma() -> None:
    pmp_entries = (
        _entry("pmp", 0, PmpPmaConfig(match="napot", execute=False), 0x8000, 0x1000),
        _entry("pmp", 1, PmpPmaConfig(match="napot", execute=True), 0x8000, 0x1000),
    )
    pma_entries = (
        _entry("pma", 0, PmpPmaConfig(match="napot", execute=True, cacheable=False), 0x8000, 0x1000),
        _entry("pma", 1, PmpPmaConfig(match="napot", execute=True, cacheable=True), 0x8000, 0x1000),
    )

    result = PmpPmaPermissionModel.check_instruction(0x8100, pmp_entries=pmp_entries, pma_entries=pma_entries)

    assert result.pmp_match_index == 0
    assert result.pma_match_index == 0
    assert result.pmp_execute_denied is True
    assert result.pma_cacheable is False


@pytest.mark.parametrize(
    "locked,expected_denied",
    [(False, False), (True, True)],
)
def test_mmode_bypasses_only_unlocked_pmp_entries(locked: bool, expected_denied: bool) -> None:
    result = PmpPmaPermissionModel.check_instruction(
        0x8000,
        pmp_entries=(_entry("pmp", 0, PmpPmaConfig(match="napot", execute=False, locked=locked), 0x8000, 0x1000),),
        priv_imode=2,
        pma_enabled=False,
    )

    assert result.pmp_execute_denied is expected_denied


def test_unmatched_pma_uses_the_denied_noncacheable_default() -> None:
    result = PmpPmaPermissionModel.check_instruction(
        0x9000,
        pma_entries=(_entry("pma", 0, PmpPmaConfig(match="napot", execute=True, cacheable=True), 0x8000, 0x1000),),
        pmp_enabled=False,
    )

    assert result.pma_match_index is None
    assert result.pma_execute_denied is True
    assert result.pma_cacheable is False


def test_reconstruct_pmp_request_addr_preserves_only_the_page_offset_bits() -> None:
    p_tag = 0x12345
    start_vaddr_pruned = 0x7A5_6FF

    assert reconstruct_pmp_request_addr(p_tag, start_vaddr_pruned) == (p_tag << 12) | 0xDFE

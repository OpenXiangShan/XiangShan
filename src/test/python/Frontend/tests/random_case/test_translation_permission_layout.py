from types import SimpleNamespace

import pytest

from env.agents.ptw_agent import PTWAgent
from env.model.page_table_model import PageTableModel
from env.model.pmp_pma_model import PmpPmaPermissionModel
from env.sequences import TranslationScenarioBuilder
from env.support.pmp_pma import encode_pmp_pma_addr, encode_pmp_pma_cfg
from .translation_timing import _physical_permissions, build_translation_scenario


def test_36_uniform_pages_fit_without_rounding_up_the_address_range():
    pmp, pma = _physical_permissions(0x80400000, 36, pma_pattern=(True,), layout="exact_napot_v1")
    assert [(e.addr, e.size) for e in pmp] == [(0x80400000, 0x20000), (0x80420000, 0x4000)]
    assert [(e.addr, e.size) for e in pma] == [(e.addr, e.size) for e in pmp]
    for addr, allowed in ((0x803FFFFF, False), (0x80400000, True),
                          (0x80423FFF, True), (0x80424000, False)):
        result = PmpPmaPermissionModel.check_instruction(addr, pmp_entries=pmp, pma_entries=pma)
        assert result.execute_allowed == allowed


@pytest.mark.parametrize("offset_pages", [0, 1, 3, 7, 15])
@pytest.mark.parametrize("pages", [1, 3, 8, 15, 31, 36, 65])
def test_exact_byte_cover_preserves_every_page_attribute(offset_pages, pages):
    base = 0x80400000 + offset_pages * 4096
    pattern = (True,) * 9 + (False,) * 8
    pmp, pma = _physical_permissions(base, pages, pma_pattern=pattern, layout="exact_napot_v1")
    for entries in (pmp, pma):
        assert [e.index for e in entries] == list(range(len(entries)))
        cursor = base
        for entry in entries:
            assert entry.addr == cursor  # no hole, overlap or widened prefix
            assert entry.size >= 4096 and entry.size & (entry.size - 1) == 0
            assert entry.addr % entry.size == 0
            encode_pmp_pma_addr(entry.addr, entry.config, size=entry.size)
            encode_pmp_pma_cfg(entry.config)
            assert entry.config.read and entry.config.execute
            assert not entry.config.write and not entry.config.locked
            cursor += entry.size
        assert cursor == base + pages * 4096  # no widened suffix
    for page in range(pages):
        for offset in (0, 1, 2048, 4095):
            addr = base + page * 4096 + offset
            matching = [e for e in pma if e.addr <= addr < e.addr + e.size]
            assert len(matching) == 1
            assert matching[0].config.cacheable == pattern[page % len(pattern)]


def test_alternating_attributes_cannot_be_merged_to_hide_capacity_failure():
    with pytest.raises(ValueError, match="pma layout requires 36 entries"):
        _physical_permissions(0x80400000, 36, pma_pattern=(True, False), layout="exact_napot_v1")


def test_existing_per_page_layout_remains_default_and_unchanged():
    old = _physical_permissions(0x80400000, 15, pma_pattern=(True, False))
    explicit = _physical_permissions(0x80400000, 15, pma_pattern=(True, False), layout="per_page_v1")
    assert old == explicit
    assert all(len(entries) == 15 for entries in old)
    assert all(e.size == 4096 for entries in old for e in entries)
    with pytest.raises(ValueError, match="unsupported translation permission layout"):
        _physical_permissions(0x80400000, 15, pma_pattern=(True,), layout="guess")


def test_compact_layout_is_explicit_and_passes_real_scenario_validation():
    raw = {"schema_version": 1, "randomizer_seed": 0x5A390001, "ordinal": 0}
    spec = SimpleNamespace(scenario_id="permission-layout", ordinal=0, parameters={"translation": raw})
    payload = bytes(36 * 4096)
    validator = TranslationScenarioBuilder(SimpleNamespace(ptw_agent=PTWAgent(PageTableModel())))
    original = build_translation_scenario(spec, payload)
    with pytest.raises(ValueError, match="entry index out of range: 32"):
        validator.validate(original)
    raw["permission_layout"] = "exact_napot_v1"
    compact = build_translation_scenario(spec, payload)
    validator.validate(compact)
    assert compact.page_count == original.page_count == 36
    assert compact.payload == original.payload
    assert compact.s1_pte == original.s1_pte
    assert compact.s2_pte == original.s2_pte


def test_layout_does_not_claim_equivalence_for_access_spanning_old_entry_boundary():
    old_pmp, old_pma = _physical_permissions(0x80400000, 2, pma_pattern=(True,))
    new_pmp, new_pma = _physical_permissions(0x80400000, 2, pma_pattern=(True,), layout="exact_napot_v1")
    # Per-byte coverage is unchanged; entry matching for a hypothetical single
    # cross-page request changes. Never apply this mode to PMP boundary tests.
    old = PmpPmaPermissionModel.check_instruction(0x80400FFE, size=4, pmp_entries=old_pmp, pma_entries=old_pma)
    new = PmpPmaPermissionModel.check_instruction(0x80400FFE, size=4, pmp_entries=new_pmp, pma_entries=new_pma)
    assert not old.execute_allowed
    assert new.execute_allowed

"""C13 model/API negatives; not real translation/PTW/Frontend qualification."""
from types import SimpleNamespace
import pytest
from env.core.backend_model import BackendModel
from env.model.page_table_model import PageTableModel


@pytest.mark.parametrize("pbmt,pma_cacheable,expected", [
    (0, True, False), (0, False, True), (1, True, True), (1, False, True),
    (2, True, True), (2, False, True),
])
def test_active_bare_only_stage2_uses_translated_pa_and_pbmt(pbmt, pma_cacheable, expected):
    va = 0x80200F00
    pa = 0x80400F00
    model = BackendModel()
    table = PageTableModel("bare")
    table.map_stage2_page(va >> 12, pa >> 12, pbmt=pbmt)
    calls = []
    def pma(addr):
        calls.append(addr)
        assert addr == pa, "PMA must see actual translated PA"
        return pma_cacheable
    model.env = SimpleNamespace(page_table=table, pma_cacheable_for_addr=pma,
        translation_oracle=SimpleNamespace(get_active=lambda: {"expected_ptw_request": {"s2xlate": 2}}))
    assert model._cfvec_uses_uncache_fetch(va) is expected
    assert calls == ([] if pbmt else [pa])


@pytest.mark.parametrize("active", [None, {"expected_ptw_request": {"s2xlate": 0}}])
def test_stage2_mapping_alone_cannot_activate_onlystage2(active):
    model = BackendModel()
    table = PageTableModel("bare")
    va = 0x80200F00
    table.map_stage2_page(va >> 12, 0x80400, pbmt=1)
    calls = []
    model.env = SimpleNamespace(page_table=table,
        pma_cacheable_for_addr=lambda addr: calls.append(addr) or True,
        translation_oracle=SimpleNamespace(get_active=lambda: active))
    assert model._cfvec_uses_uncache_fetch(va) is False
    assert calls == [va]

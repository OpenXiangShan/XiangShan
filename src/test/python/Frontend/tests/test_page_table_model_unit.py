from __future__ import annotations

from env.model.page_table_model import PageTableModel


def test_build_ptw_resp_encodes_sector_ppn_layout_for_level0_leaf() -> None:
    pt = PageTableModel(mode="sv39")
    vpn = 0x80000
    ppn = 0x90000
    sector_idx = vpn & 0x7

    pt.map_page(vpn, ppn, v=1, x=1, r=1, level=0)
    resp = pt.build_ptw_resp(vpn)

    assert resp["s1_entry_tag"] == (vpn >> 3)
    assert resp["s1_entry_ppn"] == (ppn >> 3)
    assert resp["s1_addr_low"] == sector_idx
    assert resp[f"s1_ppn_low_{sector_idx}"] == (ppn & 0x7)
    assert resp[f"s1_valididx_{sector_idx}"] == 1
    assert resp[f"s1_pteidx_{sector_idx}"] == 1

    other_idx = (sector_idx + 1) & 0x7
    assert resp[f"s1_ppn_low_{other_idx}"] == 0
    assert resp[f"s1_valididx_{other_idx}"] == 0
    assert resp[f"s1_pteidx_{other_idx}"] == 0


def test_translate_applies_stage2_after_stage1_leaf() -> None:
    pt = PageTableModel(mode="sv39")
    va = 0x80000004
    guest_pa = 0x80200000
    host_pa = 0x80400000

    pt.map_page(va >> 12, guest_pa >> 12, v=1, x=1, r=1, level=0)
    pt.map_stage2_page(guest_pa >> 12, host_pa >> 12, v=1, x=1, r=1, level=0)

    pa, ok, meta = pt.translate(va)

    assert ok is True
    assert pa == host_pa + 4
    assert meta["stage2"] is True
    assert meta["stage1_pa"] == guest_pa
    assert meta["stage2_pa"] == host_pa

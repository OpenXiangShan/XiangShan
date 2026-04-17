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

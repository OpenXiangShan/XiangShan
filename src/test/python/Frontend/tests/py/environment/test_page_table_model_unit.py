from __future__ import annotations

import pytest

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


def test_build_ptw_resp_keeps_faulting_target_sector_lane_matchable() -> None:
    pt = PageTableModel(mode="sv39")
    vpn = 0x80003
    sector_idx = vpn & 0x7

    pt.map_page(vpn, 0x90003, v=0, x=0, r=0, level=0)
    resp = pt.build_ptw_resp(vpn)

    assert (resp["s1_entry_v"], resp["s1_pf"]) == (0, 1)
    assert resp[f"s1_valididx_{sector_idx}"] == 1
    assert resp[f"s1_pteidx_{sector_idx}"] == 1


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


@pytest.mark.parametrize(
    "pte_kwargs,priv_imode,reason,expected_ptw_pf",
    [
        ({"x": 0}, 1, "stage1_execute_denied", 0),
        ({"a": 0}, 1, "stage1_accessed_clear", 0),
        ({"r": 0, "w": 1}, 1, "stage1_write_without_read", 1),
        ({"u": 0}, 0, "stage1_user_denied", 0),
        ({"u": 1}, 1, "stage1_supervisor_denied", 0),
    ],
)
def test_translate_keeps_leaf_pte_visible_while_reporting_stage1_page_fault(
    pte_kwargs: dict, priv_imode: int, reason: str, expected_ptw_pf: int
) -> None:
    pt = PageTableModel(mode="sv39")
    va = 0x80200004
    pt.map_page(va >> 12, 0x80400, **{"v": 1, "r": 1, "x": 1, "a": 1, **pte_kwargs})

    resp = pt.build_ptw_resp(va >> 12, s2xlate=1)
    pa, ok, metadata = pt.translate(va, s2xlate=1, priv_imode=priv_imode)

    assert resp["s1_entry_v"] == 1
    assert resp["s1_pf"] == expected_ptw_pf
    assert pa == 0
    assert ok is False
    assert metadata["outcome"] == "instruction_page_fault"
    assert metadata["reason"] == reason
    assert metadata["stage1_pa"] == 0x80400000


def test_translate_stage2_faults_and_access_fault_priority() -> None:
    pt = PageTableModel(mode="sv39")
    va = 0x80200004
    gpa = 0x80400004
    pt.map_page(va >> 12, gpa >> 12, v=1, r=1, x=1, a=1)
    pt.map_stage2_page(gpa >> 12, 0x80600, v=1, r=1, x=1, a=1)
    pt.set_stage1_response_fault(va >> 12, page_fault=1)
    pt.set_stage2_response_fault(gpa >> 12, guest_access_fault=1)

    resp = pt.build_ptw_resp(va >> 12, s2xlate=3)
    pa, ok, metadata = pt.translate(va, s2xlate=3)

    assert (resp["s1_pf"], resp["s1_af"], resp["s2_gpf"], resp["s2_gaf"]) == (1, 0, 0, 1)
    assert (
        resp["s2_entry_ppn"],
        resp["s2_entry_perm_r"],
        resp["s2_entry_perm_w"],
        resp["s2_entry_perm_x"],
        resp["s2_entry_v"],
    ) == (0, 0, 0, 0, 1)
    assert pa == 0
    assert ok is False
    assert metadata["outcome"] == "instruction_access_fault"
    assert metadata["reason"] == "stage2_guest_access_fault"


def test_translate_handles_stage2_guest_page_fault_and_reserved_pbmt() -> None:
    pt = PageTableModel(mode="sv39")
    va = 0x80200004
    gpa = 0x80400004
    pt.map_page(va >> 12, gpa >> 12, v=1, r=1, x=1, a=1)
    pt.map_stage2_page(gpa >> 12, 0x80600, v=1, r=1, x=1, a=1, pbmt=3)

    pa, ok, metadata = pt.translate(va, s2xlate=3)

    assert pa == 0
    assert ok is False
    assert metadata["outcome"] == "instruction_guest_page_fault"
    assert metadata["reason"] == "stage2_pbmt_reserved"


def test_translate_bare_and_m_mode_bypass_stage1_u_permission() -> None:
    va = 0x80200004
    bare = PageTableModel(mode="bare")
    pa, ok, metadata = bare.translate(va, s2xlate=0)

    assert (pa, ok, metadata["outcome"]) == (va, True, "normal")

    pt = PageTableModel(mode="sv39")
    pt.map_page(va >> 12, 0x80400, v=1, r=1, x=1, a=1, u=1)
    pa, ok, metadata = pt.translate(va, s2xlate=1, priv_imode=2)

    assert pa == 0x80400004
    assert ok is True
    assert metadata["outcome"] == "normal"


def test_translate_composes_sv39_superpage_and_explicit_stage_modes() -> None:
    pt = PageTableModel(mode="sv39")
    va = 0x8023_4004
    vpn = va >> 12
    stage1_ppn = 0x90000
    pt.map_page(vpn, stage1_ppn, v=1, r=1, x=1, a=1, level=1)

    pa, ok, metadata = pt.translate(va, s2xlate=1)
    expected_ppn = (stage1_ppn & ~0x1FF) | (vpn & 0x1FF)

    assert ok is True
    assert pa == (expected_ppn << 12) | (va & 0xFFF)
    assert metadata["stage1_level"] == 1
    assert metadata["stage1_pa"] == expected_ppn << 12

    stage2 = PageTableModel(mode="bare")
    stage2.map_stage2_page(vpn, 0xA0000, v=1, r=1, x=1, a=1, pbmt=1)
    pa, ok, metadata = stage2.translate(va, s2xlate=2)

    assert ok is True
    assert pa == 0xA0000004
    assert metadata["stage1_ok"] is False
    assert metadata["fetch_path"] == "uncache"

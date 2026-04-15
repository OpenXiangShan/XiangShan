from __future__ import annotations

from typing import Dict, Tuple

from .memory_model import PTE


class PageTableModel:
    def __init__(self, mode: str = "bare") -> None:
        self.mode = mode.lower()
        self.pte_map: Dict[int, PTE] = {}

    def clear(self) -> None:
        self.pte_map.clear()

    def set_mode(self, mode: str) -> None:
        mode = mode.lower()
        if mode not in {"bare", "sv39"}:
            raise ValueError(f"unsupported mode: {mode}")
        self.mode = mode

    def map_page(
        self,
        vpn: int,
        ppn: int,
        *,
        v: int = 1,
        r: int = 1,
        w: int = 0,
        x: int = 1,
        u: int = 0,
        g: int = 0,
        a: int = 1,
        d: int = 0,
        level: int = 0,
        asid: int = 0,
        vmid: int = 0,
    ) -> None:
        self.pte_map[int(vpn)] = PTE(
            ppn=int(ppn),
            v=int(v),
            r=int(r),
            w=int(w),
            x=int(x),
            u=int(u),
            g=int(g),
            a=int(a),
            d=int(d),
            level=int(level),
            asid=int(asid),
            vmid=int(vmid),
        )

    def translate(self, va: int) -> Tuple[int, bool, dict]:
        if self.mode == "bare":
            return va, True, {"mode": "bare"}

        vpn = va >> 12
        pte = self.pte_map.get(vpn)
        if pte is None:
            return 0, False, {"mode": "sv39", "page_fault": True, "reason": "miss"}
        if pte.v == 0 or pte.x == 0:
            return 0, False, {"mode": "sv39", "page_fault": True, "reason": "perm"}
        pa = (pte.ppn << 12) | (va & 0xFFF)
        return pa, True, {
            "mode": "sv39",
            "level": pte.level,
            "v": pte.v,
            "x": pte.x,
            "r": pte.r,
        }

    def build_ptw_resp(self, vpn: int) -> dict:
        if self.mode == "bare":
            pte = PTE(ppn=int(vpn), level=0)
        else:
            pte = self.pte_map.get(int(vpn), PTE(ppn=int(vpn), v=0, r=0, x=0, level=0))
        sector_idx = int(vpn) & 0x7
        pf = 1 if (pte.v == 0 or pte.x == 0) else 0
        resp = {
            "s2xlate": 0,
            "s1_entry_tag": (int(vpn) >> 3) & ((1 << 35) - 1),
            "s1_entry_asid": pte.asid,
            "s1_entry_vmid": pte.vmid,
            "s1_entry_n": 0,
            "s1_entry_pbmt": 0,
            "s1_entry_perm_a": pte.a,
            "s1_entry_perm_g": pte.g,
            "s1_entry_perm_u": pte.u,
            "s1_entry_perm_x": pte.x,
            "s1_entry_perm_w": pte.w,
            "s1_entry_perm_r": pte.r,
            "s1_entry_level": pte.level,
            "s1_entry_v": pte.v,
            "s1_entry_ppn": pte.ppn,
            "s1_addr_low": sector_idx,
            "s1_pf": pf,
            "s1_af": 0,
        }
        for i in range(8):
            resp[f"s1_ppn_low_{i}"] = (pte.ppn & 0x7) if (i == sector_idx and pte.v) else 0
            resp[f"s1_valididx_{i}"] = 1 if (i == sector_idx and pte.v) else 0
            resp[f"s1_pteidx_{i}"] = 1 if i == sector_idx else 0
        return resp


__all__ = ["PageTableModel"]

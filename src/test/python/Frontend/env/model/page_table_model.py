from __future__ import annotations

from typing import Dict, Tuple

from .memory_model import PTE


class PageTableModel:
    _SECTOR_IDX_BITS = 3
    _SECTOR_MASK = (1 << _SECTOR_IDX_BITS) - 1
    _VPN_LEVEL_BITS = 9
    _NO_S2XLATE = 0
    _ONLY_STAGE1 = 1
    _ONLY_STAGE2 = 2
    _ALL_STAGE = 3

    def __init__(self, mode: str = "bare") -> None:
        self.mode = mode.lower()
        self.pte_map: Dict[int, PTE] = {}
        self.stage2_pte_map: Dict[int, PTE] = {}

    def clear(self) -> None:
        self.pte_map.clear()
        self.stage2_pte_map.clear()

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

    def map_stage2_page(
        self,
        gvpn: int,
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
        vmid: int = 0,
    ) -> None:
        self.stage2_pte_map[int(gvpn)] = PTE(
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
            asid=0,
            vmid=int(vmid),
        )

    def translate(self, va: int) -> Tuple[int, bool, dict]:
        if self.mode == "bare":
            return va, True, {"mode": "bare"}

        vpn = va >> 12
        pte = self.pte_map.get(vpn)
        if pte is None:
            if vpn in self.stage2_pte_map:
                stage2_pte = self.stage2_pte_map.get(vpn)
                if stage2_pte is None or stage2_pte.v == 0 or stage2_pte.x == 0:
                    return 0, False, {"mode": "sv39", "page_fault": True, "reason": "stage2_perm"}
                gpa_ppn = self._compose_ppn(vpn, stage2_pte)
                pa = (gpa_ppn << 12) | (va & 0xFFF)
                return pa, True, {
                    "mode": "sv39",
                    "stage2": True,
                    "stage1_ok": False,
                    "stage2_ok": True,
                    "stage2_level": stage2_pte.level,
                    "stage2_v": stage2_pte.v,
                    "stage2_x": stage2_pte.x,
                    "stage2_r": stage2_pte.r,
                }
            return 0, False, {"mode": "sv39", "page_fault": True, "reason": "miss"}

        if pte.v == 0 or pte.x == 0:
            return 0, False, {"mode": "sv39", "page_fault": True, "reason": "perm"}

        stage1_ppn = self._compose_ppn(vpn, pte)
        if not self.stage2_pte_map:
            pa = (stage1_ppn << 12) | (va & 0xFFF)
            return pa, True, {
                "mode": "sv39",
                "stage2": False,
                "stage1_ok": True,
                "stage1_level": pte.level,
                "v": pte.v,
                "x": pte.x,
                "r": pte.r,
            }

        stage2_pte = self.stage2_pte_map.get(stage1_ppn)
        if stage2_pte is None:
            return 0, False, {
                "mode": "sv39",
                "page_fault": True,
                "reason": "stage2_miss",
                "stage1_ok": True,
                "stage1_level": pte.level,
                "stage1_v": pte.v,
                "stage1_x": pte.x,
                "stage1_r": pte.r,
                "stage1_pa": stage1_ppn << 12,
            }
        if stage2_pte.v == 0 or stage2_pte.x == 0:
            return 0, False, {
                "mode": "sv39",
                "page_fault": True,
                "reason": "stage2_perm",
                "stage1_ok": True,
                "stage1_level": pte.level,
                "stage1_v": pte.v,
                "stage1_x": pte.x,
                "stage1_r": pte.r,
                "stage1_pa": stage1_ppn << 12,
                "stage2_level": stage2_pte.level,
                "stage2_v": stage2_pte.v,
                "stage2_x": stage2_pte.x,
                "stage2_r": stage2_pte.r,
            }

        host_ppn = self._compose_ppn(stage1_ppn, stage2_pte)
        pa = (host_ppn << 12) | (va & 0xFFF)
        return pa, True, {
            "mode": "sv39",
            "stage2": True,
            "stage1_ok": True,
            "stage2_ok": True,
            "stage1_level": pte.level,
            "stage2_level": stage2_pte.level,
            "v": pte.v,
            "x": pte.x,
            "r": pte.r,
            "stage2_v": stage2_pte.v,
            "stage2_x": stage2_pte.x,
            "stage2_r": stage2_pte.r,
            "stage1_pa": stage1_ppn << 12,
            "stage2_pa": host_ppn << 12,
        }

    @classmethod
    def _build_sector_arrays(cls, vpn: int, ppn: int, level: int, valid: int) -> Tuple[int, list[int], list[int], list[int]]:
        addr_low = vpn & cls._SECTOR_MASK
        pteidx = [0] * (cls._SECTOR_MASK + 1)
        pteidx[addr_low] = 1
        if int(level) == 0:
            valididx = [0] * (cls._SECTOR_MASK + 1)
            valididx[addr_low] = int(valid)
            ppn_low = [0] * (cls._SECTOR_MASK + 1)
            ppn_low[addr_low] = ppn & cls._SECTOR_MASK
        else:
            valididx = [int(valid)] * (cls._SECTOR_MASK + 1)
            ppn_low = [0] * (cls._SECTOR_MASK + 1)
        return addr_low, ppn_low, valididx, pteidx

    def _build_stage2_resp(self, gvpn: int, s2xlate: int) -> dict:
        gvpn = int(gvpn)
        if int(s2xlate) == self._NO_S2XLATE:
            return {
                "s2_entry_tag": 0,
                "s2_entry_vmid": 0,
                "s2_entry_n": 0,
                "s2_entry_pbmt": 0,
                "s2_entry_perm_a": 0,
                "s2_entry_perm_g": 0,
                "s2_entry_perm_u": 0,
                "s2_entry_perm_x": 0,
                "s2_entry_perm_w": 0,
                "s2_entry_perm_r": 0,
                "s2_entry_level": 0,
                "s2_entry_v": 0,
                "s2_entry_ppn": 0,
                "s2_gpf": 0,
                "s2_gaf": 0,
            }

        pte = self.stage2_pte_map.get(gvpn)
        gpf = 1 if pte is None or int(pte.v) == 0 else 0
        if pte is None:
            pte = PTE(ppn=0, v=0, r=0, x=0, level=0)
        return {
            "s2_entry_tag": gvpn,
            "s2_entry_vmid": pte.vmid,
            "s2_entry_n": 0,
            "s2_entry_pbmt": 0,
            "s2_entry_perm_a": pte.a,
            "s2_entry_perm_g": pte.g,
            "s2_entry_perm_u": pte.u,
            "s2_entry_perm_x": pte.x,
            "s2_entry_perm_w": pte.w,
            "s2_entry_perm_r": pte.r,
            "s2_entry_level": pte.level,
            "s2_entry_v": pte.v,
            "s2_entry_ppn": pte.ppn,
            "s2_gpf": gpf,
            "s2_gaf": 0,
        }

    @staticmethod
    def _zero_stage1_resp() -> dict:
        return {
            "s1_entry_tag": 0,
            "s1_entry_asid": 0,
            "s1_entry_vmid": 0,
            "s1_entry_n": 0,
            "s1_entry_pbmt": 0,
            "s1_entry_perm_a": 0,
            "s1_entry_perm_g": 0,
            "s1_entry_perm_u": 0,
            "s1_entry_perm_x": 0,
            "s1_entry_perm_w": 0,
            "s1_entry_perm_r": 0,
            "s1_entry_level": 0,
            "s1_entry_v": 0,
            "s1_entry_ppn": 0,
            "s1_addr_low": 0,
            "s1_ppn_low": [0] * 8,
            "s1_valididx": [0] * 8,
            "s1_pteidx": [0] * 8,
            "s1_pf": 0,
            "s1_af": 0,
        }

    def _compose_ppn(self, vpn: int, pte: PTE) -> int:
        level = max(0, int(pte.level))
        if level <= 0:
            return int(pte.ppn)
        lower_bits = level * self._VPN_LEVEL_BITS
        mask = (1 << lower_bits) - 1 if lower_bits > 0 else 0
        return (int(pte.ppn) & ~mask) | (int(vpn) & mask)

    def _build_stage1_resp(self, vpn: int, pte: PTE, pf: int) -> dict:
        addr_low, ppn_low, valididx, pteidx = self._build_sector_arrays(vpn, pte.ppn, pte.level, pte.v)
        resp = {
            "s1_entry_tag": vpn >> self._SECTOR_IDX_BITS,
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
            "s1_entry_ppn": pte.ppn >> self._SECTOR_IDX_BITS,
            "s1_addr_low": addr_low,
            "s1_ppn_low": ppn_low,
            "s1_valididx": valididx,
            "s1_pteidx": pteidx,
            "s1_pf": int(pf),
            "s1_af": 0,
        }
        for idx in range(len(ppn_low)):
            resp[f"s1_ppn_low_{idx}"] = ppn_low[idx]
            resp[f"s1_valididx_{idx}"] = valididx[idx]
            resp[f"s1_pteidx_{idx}"] = pteidx[idx]
        return resp

    def build_ptw_resp(
        self,
        vpn: int,
        *,
        s2xlate: int = 0,
        get_gpa: int = 0,
        memidx_is_ld: int = 0,
        memidx_is_st: int = 0,
        memidx_idx: int = 0,
        strict_bare_mode: bool = False,
    ) -> dict:
        vpn = int(vpn)
        s2xlate = int(s2xlate)
        get_gpa = int(get_gpa)
        memidx_is_ld = int(memidx_is_ld)
        memidx_is_st = int(memidx_is_st)
        memidx_idx = int(memidx_idx)
        stage1_resp = self._zero_stage1_resp()
        stage2_resp = self._build_stage2_resp(0, self._NO_S2XLATE)

        if int(s2xlate) == self._ONLY_STAGE2:
            stage2_resp = self._build_stage2_resp(vpn, s2xlate)
            return {
                "s2xlate": s2xlate,
                "get_gpa": get_gpa,
                "memidx_is_ld": memidx_is_ld,
                "memidx_is_st": memidx_is_st,
                "memidx_idx": memidx_idx,
                **stage1_resp,
                **stage2_resp,
            }

        if self.mode == "bare":
            if strict_bare_mode:
                stage1_resp = self._build_stage1_resp(vpn, PTE(ppn=0, v=0, r=0, w=0, x=0, a=0, level=0), 1)
                if int(s2xlate) == self._ALL_STAGE:
                    stage2_resp = self._build_stage2_resp(0, self._NO_S2XLATE)
                elif int(s2xlate) == self._ONLY_STAGE1:
                    stage2_resp = self._build_stage2_resp(0, self._NO_S2XLATE)
                else:
                    stage2_resp = self._build_stage2_resp(vpn, s2xlate)
                return {
                    "s2xlate": s2xlate,
                    "get_gpa": get_gpa,
                    "memidx_is_ld": memidx_is_ld,
                    "memidx_is_st": memidx_is_st,
                    "memidx_idx": memidx_idx,
                    **stage1_resp,
                    **stage2_resp,
                }
            pte = PTE(ppn=vpn, level=0)
            pf = 0
        else:
            pte = self.pte_map.get(vpn)
            pf = 1 if pte is None or int(pte.v) == 0 else 0
            if pte is None:
                pte = PTE(ppn=0, v=0, r=0, x=0, level=0)

        stage1_resp = self._build_stage1_resp(vpn, pte, pf)
        if int(s2xlate) == self._ALL_STAGE and pf == 0:
            stage2_resp = self._build_stage2_resp(self._compose_ppn(vpn, pte), s2xlate)
        elif int(s2xlate) in {self._NO_S2XLATE, self._ONLY_STAGE1}:
            stage2_resp = self._build_stage2_resp(0, self._NO_S2XLATE)

        return {
            "s2xlate": s2xlate,
            "get_gpa": get_gpa,
            "memidx_is_ld": memidx_is_ld,
            "memidx_is_st": memidx_is_st,
            "memidx_idx": memidx_idx,
            **stage1_resp,
            **stage2_resp,
        }


__all__ = ["PageTableModel"]

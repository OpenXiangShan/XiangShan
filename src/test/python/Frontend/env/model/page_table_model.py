from __future__ import annotations

from typing import Dict, Optional, Tuple

from .memory_model import PTE


class PageTableModel:
    _PTW_VPN_BITS = 38
    _SECTOR_IDX_BITS = 3
    _SECTOR_MASK = (1 << _SECTOR_IDX_BITS) - 1
    _VPN_LEVEL_BITS = 9
    _NO_S2XLATE = 0
    _ONLY_STAGE1 = 1
    _ONLY_STAGE2 = 2
    _ALL_STAGE = 3

    def __init__(self, mode: str = "bare") -> None:
        self.mode = "bare"
        self.stage2_mode = "sv39"
        self.set_mode(mode)
        self.pte_map: Dict[int, PTE] = {}
        self.stage2_pte_map: Dict[int, PTE] = {}
        self._stage1_sector_response_tags: set[int] = set()
        self._stage1_sector_lane_valid: dict[int, dict[int, int]] = {}
        # Explicit response faults supplement the faults derived from PTE fields.
        self._stage1_fault_map: Dict[int, Tuple[int, int]] = {}
        self._stage2_fault_map: Dict[int, Tuple[int, int]] = {}
        # ``None`` retains the legacy raw-PTW-response mode.  A concrete value
        # models the upstream PTW's PBMTE legality check before Frontend sees a
        # response; the frontend-only DUT does not implement that check itself.
        self.machine_pbmte: Optional[bool] = None
        self.hypervisor_pbmte: Optional[bool] = None

    def clear(self) -> None:
        self.pte_map.clear()
        self.stage2_pte_map.clear()
        self._stage1_sector_response_tags.clear()
        self._stage1_sector_lane_valid.clear()
        self._stage1_fault_map.clear()
        self._stage2_fault_map.clear()

    def set_mode(self, mode: str) -> None:
        mode = mode.lower()
        if mode not in {"bare", "sv39", "sv48"}:
            raise ValueError(f"unsupported mode: {mode}")
        self.mode = mode

    def set_stage2_mode(self, mode: str) -> None:
        mode = mode.lower()
        if mode not in {"sv39", "sv48"}:
            raise ValueError(f"unsupported stage-2 mode: {mode}")
        self.stage2_mode = mode

    def set_ptw_pbmte_policy(
        self,
        *,
        machine: Optional[int] = None,
        hypervisor: Optional[int] = None,
        reset: bool = False,
    ) -> dict:
        """Configure PBMTE legality while synthesizing PTW responses.

        ``None`` means that the response source has already applied the
        architectural PBMTE decision.  This preserves existing directed
        response tests while allowing scenarios to model enabled and disabled
        PBMTE explicitly.
        """
        if reset:
            self.machine_pbmte = None
            self.hypervisor_pbmte = None
        if machine is not None:
            self.machine_pbmte = bool(machine)
        if hypervisor is not None:
            self.hypervisor_pbmte = bool(hypervisor)
        return {
            "machine": self.machine_pbmte,
            "hypervisor": self.hypervisor_pbmte,
        }

    def apply_ptw_pbmte_policy(self, response: dict) -> dict:
        """Apply the configured upstream PBMTE check to one PTW response."""
        normalized = dict(response)
        s2xlate = int(normalized.get("s2xlate", self._NO_S2XLATE))
        if (
            self._stage1_pbmte(s2xlate) is False
            and int(normalized.get("s1_entry_pbmt", 0)) != 0
        ):
            normalized["s1_pf"] = 1
        if self.machine_pbmte is False and int(normalized.get("s2_entry_pbmt", 0)) != 0:
            normalized["s2_gpf"] = 1
            normalized["s2_entry_v"] = 0
        return normalized

    def _stage1_pbmte(self, s2xlate: int) -> Optional[bool]:
        return self.machine_pbmte if int(s2xlate) == self._NO_S2XLATE else self.hypervisor_pbmte

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
        n: int = 0,
        level: int = 0,
        asid: int = 0,
        vmid: int = 0,
        pbmt: int = 0,
    ) -> None:
        self.pte_map[self._pte_key(vpn, level)] = PTE(
            ppn=int(ppn),
            v=int(v),
            r=int(r),
            w=int(w),
            x=int(x),
            u=int(u),
            g=int(g),
            a=int(a),
            d=int(d),
            n=int(n),
            level=int(level),
            asid=int(asid),
            vmid=int(vmid),
            pbmt=int(pbmt),
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
        n: int = 0,
        level: int = 0,
        vmid: int = 0,
        pbmt: int = 0,
    ) -> None:
        self.stage2_pte_map[self._pte_key(gvpn, level)] = PTE(
            ppn=int(ppn),
            v=int(v),
            r=int(r),
            w=int(w),
            x=int(x),
            u=int(u),
            g=int(g),
            a=int(a),
            d=int(d),
            n=int(n),
            level=int(level),
            asid=0,
            vmid=int(vmid),
            pbmt=int(pbmt),
        )

    def enable_stage1_sector_response(self, vpn: int, lane_valid: Optional[dict[int, int]] = None) -> None:
        sector_tag = self.normalize_ptw_vpn(vpn) >> self._SECTOR_IDX_BITS
        self._stage1_sector_response_tags.add(sector_tag)
        self._stage1_sector_lane_valid[sector_tag] = {
            int(lane): int(valid) for lane, valid in (lane_valid or {}).items()
        }

    def set_stage1_response_fault(self, vpn: int, *, page_fault: int = 0, access_fault: int = 0) -> None:
        """Inject response-side S-stage faults without changing the returned PTE."""
        self._stage1_fault_map[self.normalize_ptw_vpn(vpn)] = (int(page_fault), int(access_fault))

    def set_stage2_response_fault(self, gvpn: int, *, guest_page_fault: int = 0, guest_access_fault: int = 0) -> None:
        """Inject response-side G-stage faults without changing the returned PTE."""
        self._stage2_fault_map[self.normalize_ptw_vpn(gvpn)] = (int(guest_page_fault), int(guest_access_fault))

    @classmethod
    def normalize_ptw_vpn(cls, vpn: int) -> int:
        """Encode a VPN as it appears on the generated 38-bit PTW request port."""
        return int(vpn) & ((1 << cls._PTW_VPN_BITS) - 1)

    @classmethod
    def _pte_key(cls, vpn: int, level: int) -> int:
        level = max(0, int(level))
        lower_bits = level * cls._VPN_LEVEL_BITS
        vpn = cls.normalize_ptw_vpn(vpn)
        return vpn & ~((1 << lower_bits) - 1) if lower_bits else vpn

    @classmethod
    def _lookup_pte(cls, pte_map: Dict[int, PTE], vpn: int, mode: str = "sv39") -> Optional[PTE]:
        vpn = int(vpn)
        levels = 4 if str(mode).lower() == "sv48" else 3
        for level in range(levels):
            pte = pte_map.get(cls._pte_key(vpn, level))
            if pte is not None and int(pte.level) == level:
                return pte
        return None

    @staticmethod
    def _pte_metadata(prefix: str, pte: Optional[PTE]) -> dict:
        if pte is None:
            return {}
        return {
            f"{prefix}_level": int(pte.level),
            f"{prefix}_v": int(pte.v),
            f"{prefix}_r": int(pte.r),
            f"{prefix}_w": int(pte.w),
            f"{prefix}_x": int(pte.x),
            f"{prefix}_u": int(pte.u),
            f"{prefix}_a": int(pte.a),
            f"{prefix}_pbmt": int(pte.pbmt),
        }

    @staticmethod
    def _outcome_metadata(fault: Optional[str], reason: str) -> dict:
        if fault is None:
            return {"outcome": "normal", "fault": None, "reason": "ok"}
        return {
            "outcome": f"instruction_{fault}",
            "fault": fault,
            "reason": reason,
        }

    @staticmethod
    def _fetch_path(pbmt: int) -> str:
        return {0: "pma", 1: "uncache", 2: "mmio", 3: "fault"}.get(int(pbmt), "fault")

    def _stage1_fault(
        self,
        vpn: int,
        pte: Optional[PTE],
        priv_imode: int,
        pbmte: Optional[bool],
    ) -> Tuple[Optional[str], str]:
        forced_pf, forced_af = self._stage1_fault_map.get(self.normalize_ptw_vpn(vpn), (0, 0))
        if forced_af:
            return "access_fault", "stage1_access_fault"
        if forced_pf or pte is None or int(pte.v) == 0:
            return "page_fault", "stage1_missing_or_invalid"
        if int(pte.w) and not int(pte.r):
            return "page_fault", "stage1_write_without_read"
        if not (int(pte.r) or int(pte.w) or int(pte.x)):
            return "page_fault", "stage1_nonleaf"
        if not int(pte.x):
            return "page_fault", "stage1_execute_denied"
        if not int(pte.a):
            return "page_fault", "stage1_accessed_clear"
        if int(priv_imode) == 0 and not int(pte.u):
            return "page_fault", "stage1_user_denied"
        if int(priv_imode) == 1 and int(pte.u):
            return "page_fault", "stage1_supervisor_denied"
        if int(pte.pbmt) == 3:
            return "page_fault", "stage1_pbmt_reserved"
        if pbmte is False and int(pte.pbmt) != 0:
            return "page_fault", "stage1_pbmt_disabled"
        return None, "ok"

    def _stage2_fault(self, gvpn: int, pte: Optional[PTE], pbmte: Optional[bool]) -> Tuple[Optional[str], str]:
        forced_gpf, forced_gaf = self._stage2_fault_map.get(self.normalize_ptw_vpn(gvpn), (0, 0))
        if forced_gaf:
            return "access_fault", "stage2_guest_access_fault"
        if forced_gpf or pte is None or int(pte.v) == 0:
            return "guest_page_fault", "stage2_missing_or_invalid"
        if int(pte.w) and not int(pte.r):
            return "guest_page_fault", "stage2_write_without_read"
        if not (int(pte.r) or int(pte.w) or int(pte.x)):
            return "guest_page_fault", "stage2_nonleaf"
        if not int(pte.x):
            return "guest_page_fault", "stage2_execute_denied"
        if not int(pte.a):
            return "guest_page_fault", "stage2_accessed_clear"
        if int(pte.pbmt) == 3:
            return "guest_page_fault", "stage2_pbmt_reserved"
        if pbmte is False and int(pte.pbmt) != 0:
            return "guest_page_fault", "stage2_pbmt_disabled"
        return None, "ok"

    def _infer_s2xlate(self, vpn: int) -> int:
        if not self.stage2_pte_map:
            return self._ONLY_STAGE1
        if (
            self._lookup_pte(self.pte_map, vpn, self.mode) is None
            and self._lookup_pte(self.stage2_pte_map, vpn, self.stage2_mode) is not None
        ):
            return self._ONLY_STAGE2
        return self._ALL_STAGE

    def translate(
        self,
        va: int,
        *,
        s2xlate: Optional[int] = None,
        priv_imode: int = 1,
    ) -> Tuple[int, bool, dict]:
        """Return PA and the architectural instruction-fetch outcome for one address.

        ``s2xlate`` follows the PTW request encoding.  Omitting it retains the
        legacy map-inference behavior used by existing model-only tests.
        """
        va = int(va)
        vpn = va >> 12
        if s2xlate is None:
            if self.mode == "bare":
                return va, True, {
                    "mode": "bare",
                    "stage2": False,
                    "stage1_ok": True,
                    "translated_pa": va,
                    "fetch_path": "pma",
                    **self._outcome_metadata(None, "ok"),
                }
            s2xlate = self._infer_s2xlate(vpn)
        s2xlate = int(s2xlate)
        if s2xlate not in {self._NO_S2XLATE, self._ONLY_STAGE1, self._ONLY_STAGE2, self._ALL_STAGE}:
            raise ValueError(f"unsupported s2xlate: {s2xlate}")

        metadata = {"mode": self.mode, "s2xlate": s2xlate, "stage2": s2xlate in {self._ONLY_STAGE2, self._ALL_STAGE}}
        if s2xlate == self._ONLY_STAGE2:
            stage2_pte = self._lookup_pte(self.stage2_pte_map, vpn, self.stage2_mode)
            fault, reason = self._stage2_fault(vpn, stage2_pte, self.machine_pbmte)
            metadata.update(self._pte_metadata("stage2", stage2_pte))
            metadata["stage1_ok"] = False
            metadata["stage2_ok"] = fault is None
            if stage2_pte is not None and int(stage2_pte.v):
                metadata["stage2_pa"] = self._compose_ppn(vpn, stage2_pte) << 12
            if fault is not None:
                metadata.update(self._outcome_metadata(fault, reason))
                return 0, False, metadata
            host_ppn = self._compose_ppn(vpn, stage2_pte)
            pa = (host_ppn << 12) | (va & 0xFFF)
            metadata.update({"stage2_pa": host_ppn << 12, "translated_pa": pa, "fetch_path": self._fetch_path(stage2_pte.pbmt)})
            metadata.update(self._outcome_metadata(None, "ok"))
            return pa, True, metadata

        if self.mode == "bare":
            stage1_ppn = vpn
            stage1_pte = None
            stage1_fault = None
            stage1_reason = "ok"
            metadata["stage1_ok"] = True
        else:
            stage1_pte = self._lookup_pte(self.pte_map, vpn, self.mode)
            stage1_fault, stage1_reason = self._stage1_fault(
                vpn,
                stage1_pte,
                priv_imode,
                self._stage1_pbmte(s2xlate),
            )
            metadata.update(self._pte_metadata("stage1", stage1_pte))
            metadata["stage1_ok"] = stage1_fault is None
            if stage1_pte is not None and int(stage1_pte.v):
                metadata["stage1_pa"] = self._compose_ppn(vpn, stage1_pte) << 12
            if stage1_fault is not None and (
                s2xlate != self._ALL_STAGE
                or stage1_fault == "access_fault"
                or stage1_pte is None
                or int(stage1_pte.v) == 0
            ):
                metadata.update(self._outcome_metadata(stage1_fault, stage1_reason))
                return 0, False, metadata
            stage1_ppn = self._compose_ppn(vpn, stage1_pte)

        metadata["stage1_pa"] = stage1_ppn << 12
        if s2xlate != self._ALL_STAGE:
            pa = (stage1_ppn << 12) | (va & 0xFFF)
            pbmt = 0 if stage1_pte is None else int(stage1_pte.pbmt)
            metadata.update({"stage2": False, "translated_pa": pa, "fetch_path": self._fetch_path(pbmt)})
            metadata.update(self._outcome_metadata(None, "ok"))
            return pa, True, metadata

        stage2_pte = self._lookup_pte(self.stage2_pte_map, stage1_ppn, self.stage2_mode)
        stage2_fault, stage2_reason = self._stage2_fault(stage1_ppn, stage2_pte, self.machine_pbmte)
        metadata.update(self._pte_metadata("stage2", stage2_pte))
        metadata["stage2_ok"] = stage2_fault is None
        if stage2_pte is not None and int(stage2_pte.v):
            metadata["stage2_pa"] = self._compose_ppn(stage1_ppn, stage2_pte) << 12
        if stage2_fault == "access_fault":
            metadata.update(self._outcome_metadata(stage2_fault, stage2_reason))
            return 0, False, metadata
        if stage1_fault is not None:
            metadata.update(self._outcome_metadata(stage1_fault, stage1_reason))
            return 0, False, metadata
        if stage2_fault is not None:
            metadata.update(self._outcome_metadata(stage2_fault, stage2_reason))
            return 0, False, metadata
        host_ppn = self._compose_ppn(stage1_ppn, stage2_pte)
        pa = (host_ppn << 12) | (va & 0xFFF)
        pbmt = int(stage2_pte.pbmt) if int(stage2_pte.pbmt) else int(stage1_pte.pbmt) if stage1_pte is not None else 0
        metadata.update({"stage2_pa": host_ppn << 12, "translated_pa": pa, "fetch_path": self._fetch_path(pbmt)})
        metadata.update(self._outcome_metadata(None, "ok"))
        return pa, True, metadata

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

        pte = self._lookup_pte(self.stage2_pte_map, gvpn, self.stage2_mode)
        forced_gpf, forced_gaf = self._stage2_fault_map.get(gvpn, (0, 0))
        pbmt_disabled = pte is not None and self.machine_pbmte is False and int(pte.pbmt) != 0
        gpf = 1 if (
            forced_gpf
            or pte is None
            or int(pte.v) == 0
            or (int(pte.w) == 1 and int(pte.r) == 0)
            or pbmt_disabled
        ) else 0
        gaf = 1 if forced_gaf else 0
        if pte is None:
            pte = PTE(ppn=0, v=0, r=0, x=0, level=0)
        resp_pte = (
            PTE(
                ppn=0,
                v=0,
                r=0,
                w=0,
                x=0,
                u=0,
                g=0,
                a=0,
                d=0,
                n=0,
                level=pte.level,
                vmid=pte.vmid,
            )
            if gaf
            else pte
        )
        return {
            "s2_entry_tag": gvpn,
            "s2_entry_vmid": resp_pte.vmid,
            "s2_entry_n": resp_pte.n,
            "s2_entry_pbmt": resp_pte.pbmt,
            "s2_entry_perm_a": resp_pte.a,
            "s2_entry_perm_g": resp_pte.g,
            "s2_entry_perm_u": resp_pte.u,
            "s2_entry_perm_x": resp_pte.x,
            "s2_entry_perm_w": resp_pte.w,
            "s2_entry_perm_r": resp_pte.r,
            "s2_entry_level": resp_pte.level,
            "s2_entry_v": int(not gpf),
            "s2_entry_ppn": resp_pte.ppn,
            "s2_gpf": gpf,
            "s2_gaf": gaf,
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
        if int(pf):
            valididx[addr_low] = 1
        sector_tag = self.normalize_ptw_vpn(vpn) >> self._SECTOR_IDX_BITS
        if int(pte.level) == 0 and sector_tag in self._stage1_sector_response_tags:
            sector_base = self.normalize_ptw_vpn(vpn) & ~self._SECTOR_MASK
            ppn_low = [0] * (self._SECTOR_MASK + 1)
            valididx = [0] * (self._SECTOR_MASK + 1)
            pteidx = [0] * (self._SECTOR_MASK + 1)
            for lane in range(self._SECTOR_MASK + 1):
                lane_pte = self._lookup_pte(self.pte_map, sector_base + lane, self.mode)
                if lane_pte is None or int(lane_pte.level) != 0:
                    continue
                ppn_low[lane] = int(lane_pte.ppn) & self._SECTOR_MASK
                valididx[lane] = self._stage1_sector_lane_valid.get(sector_tag, {}).get(
                    lane, int(lane_pte.v)
                )
                pteidx[lane] = 1
            requested_lane = self.normalize_ptw_vpn(vpn) & self._SECTOR_MASK
            pteidx[requested_lane] = 1
            valididx[requested_lane] = 1
        resp = {
            "s1_entry_tag": vpn >> self._SECTOR_IDX_BITS,
            "s1_entry_asid": pte.asid,
            "s1_entry_vmid": pte.vmid,
            "s1_entry_n": pte.n,
            "s1_entry_pbmt": pte.pbmt,
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
            pte = self._lookup_pte(self.pte_map, vpn, self.mode)
            forced_pf, forced_af = self._stage1_fault_map.get(vpn, (0, 0))
            pbmt_disabled = pte is not None and self._stage1_pbmte(s2xlate) is False and int(pte.pbmt) != 0
            pf = 1 if (
                forced_pf
                or pte is None
                or int(pte.v) == 0
                or (int(pte.w) == 1 and int(pte.r) == 0)
                or pbmt_disabled
            ) else 0
            if pte is None:
                pte = PTE(ppn=0, v=0, r=0, x=0, level=0)

        stage1_resp = self._build_stage1_resp(vpn, pte, pf)
        if self.mode != "bare":
            stage1_resp["s1_af"] = int(forced_af)
        if int(s2xlate) == self._ALL_STAGE and int(pte.v) == 1:
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

from __future__ import annotations

from dataclasses import dataclass
from typing import Optional

from ..bundles import BackendCfVecBundle, BackendFromIfuBundle, BackendToFtqBundle
from ..bundles.ptw import PTWBundle
from ..model.backend_runtime import (
    BackendObservationSnapshot,
    CfVecCycleSnapshot,
    capture_cfvec_snapshot,
)


@dataclass
class Observation:
    cycle: int
    slot: int
    pc: int
    foldpc: int
    instr: int
    is_rvc: bool
    pred_taken: bool


@dataclass(frozen=True)
class FrontendCycleSnapshot:
    cycle: int
    backend: BackendObservationSnapshot
    cfvec: CfVecCycleSnapshot
    redirect: dict
    ptw: dict


@dataclass(frozen=True)
class ICacheRequestSnapshot:
    cycle: int
    valid: int
    source: int
    address: int


@dataclass(frozen=True)
class UncacheRequestSnapshot:
    cycle: int
    valid: int
    address: int
    mem_back_type_mm: int
    mem_page_type_nc: int


@dataclass(frozen=True)
class ICacheControlSnapshot:
    cycle: int
    a_ready: int
    d_valid: int
    d_opcode: int
    d_size: int
    d_source: int
    d_data: int


@dataclass(frozen=True)
class PTWAgentInputSnapshot:
    cycle: int
    sfence_valid: int
    req_valid: int
    req_vpn: int
    req_s2xlate: int
    req_get_gpa: int
    resp_ready: int
    context: dict[str, int]


class FrontendMonitor:
    def __init__(self) -> None:
        self.cfvec_if = None
        self.backend_to_ftq_if = None
        self.backend_from_ifu_if = None
        self.from_ftq_if = None
        self.ptw_if = None
        self.csr_ctrl_if = None
        self.icache_if = None
        self.uncache_if = None
        self.icache_control_if = None
        self.ptw_get_gpa_provider = None
        self.cycle_snapshot: Optional[FrontendCycleSnapshot] = None
        self.icache_request_snapshot: Optional[ICacheRequestSnapshot] = None
        self.uncache_request_snapshot: Optional[UncacheRequestSnapshot] = None
        self.icache_control_snapshot: Optional[ICacheControlSnapshot] = None
        self.ptw_agent_input_snapshot: Optional[PTWAgentInputSnapshot] = None
        self.observations: list[Observation] = []
        self._checker = None

    # Existing test and API callers still read checker state through env.monitor.
    @property
    def errors(self) -> list[dict]:
        return self._checker.errors

    @property
    def expected_pc(self) -> Optional[int]:
        return self._checker.expected_pc

    @property
    def exception_mark_count(self) -> int:
        return self._checker.exception_mark_count

    @property
    def redirect_count(self) -> int:
        return self._checker.redirect_count

    @property
    def last_dut_redirect(self) -> Optional[dict]:
        return self._checker.last_dut_redirect

    @property
    def _recovery_target_pc(self) -> Optional[int]:
        return self._checker._recovery_target_pc

    @property
    def redirect_grace(self) -> int:
        return self._checker.redirect_grace

    @redirect_grace.setter
    def redirect_grace(self, value: int) -> None:
        self._checker.redirect_grace = int(value)

    @property
    def redirect_sync_max(self) -> int:
        return self._checker.redirect_sync_max

    @redirect_sync_max.setter
    def redirect_sync_max(self, value: int) -> None:
        self._checker.redirect_sync_max = int(value)

    @property
    def wait_sync_after_redirect(self) -> bool:
        return self._checker.wait_sync_after_redirect

    @wait_sync_after_redirect.setter
    def wait_sync_after_redirect(self, value: bool) -> None:
        self._checker.wait_sync_after_redirect = bool(value)

    @property
    def _skip_cfvec_until_cycle(self) -> Optional[int]:
        return self._checker._skip_cfvec_until_cycle

    @_skip_cfvec_until_cycle.setter
    def _skip_cfvec_until_cycle(self, value: Optional[int]) -> None:
        self._checker._skip_cfvec_until_cycle = None if value is None else int(value)

    @staticmethod
    def _read(signal, default: int = 0) -> int:
        try:
            value = getattr(signal, "value", None)
            return default if value is None else int(value)
        except Exception:
            return default

    def bind_cfvec(self, target) -> None:
        if not isinstance(target, BackendCfVecBundle):
            raise TypeError("FrontendMonitor requires an explicitly bound BackendCfVecBundle")
        self.cfvec_if = target

    def bind_backend_to_ftq(self, target) -> None:
        if not isinstance(target, BackendToFtqBundle):
            raise TypeError("FrontendMonitor requires an explicitly bound BackendToFtqBundle")
        self.backend_to_ftq_if = target

    def bind_backend_from_ifu(self, target) -> None:
        if not isinstance(target, BackendFromIfuBundle):
            raise TypeError("FrontendMonitor requires an explicitly bound BackendFromIfuBundle")
        self.backend_from_ifu_if = target

    def bind_observation_interfaces(self, *, from_ftq_if, ptw_if, ptw_get_gpa_provider) -> None:
        self.from_ftq_if = from_ftq_if
        self.ptw_if = ptw_if
        self.ptw_get_gpa_provider = ptw_get_gpa_provider

    def bind_ptw_context(self, csr_ctrl_if) -> None:
        self.csr_ctrl_if = csr_ctrl_if

    def bind_icache(self, icache_if) -> None:
        self.icache_if = icache_if

    def bind_uncache(self, uncache_if) -> None:
        self.uncache_if = uncache_if

    def bind_icache_control(self, icache_control_if) -> None:
        self.icache_control_if = icache_control_if

    def attach_checker(self, checker) -> None:
        self._checker = checker

    def _read_cfvec_slot(self, name: str, slot: int) -> int:
        signals = getattr(self.cfvec_if, name, ())
        try:
            return self._read(signals[int(slot)])
        except (IndexError, TypeError):
            return 0

    def capture_cfvec_snapshot(self, cycle: int) -> CfVecCycleSnapshot:
        if self.cfvec_if is None:
            raise AssertionError("FrontendMonitor observation interface is not bound")
        return capture_cfvec_snapshot(cycle, self._read_cfvec_slot)

    def capture_icache_request(self, cycle: int) -> ICacheRequestSnapshot:
        if self.icache_if is None:
            raise AssertionError("FrontendMonitor ICache interface is not bound")
        self.icache_request_snapshot = ICacheRequestSnapshot(
            cycle=int(cycle),
            valid=self._read(self.icache_if.a_valid),
            source=self._read(self.icache_if.a_bits_source),
            address=self._read(self.icache_if.a_bits_address),
        )
        return self.icache_request_snapshot

    def capture_uncache_request(self, cycle: int) -> UncacheRequestSnapshot:
        if self.uncache_if is None:
            raise AssertionError("FrontendMonitor Uncache interface is not bound")
        self.uncache_request_snapshot = UncacheRequestSnapshot(
            cycle=int(cycle),
            valid=self._read(self.uncache_if.a_valid),
            address=self._read(self.uncache_if.a_bits_address),
            mem_back_type_mm=self._read(self.uncache_if.a_bits_user_mem_back_type_mm),
            mem_page_type_nc=self._read(self.uncache_if.a_bits_user_mem_page_type_nc),
        )
        return self.uncache_request_snapshot

    def capture_icache_control(self, cycle: int) -> ICacheControlSnapshot:
        if self.icache_control_if is None:
            raise AssertionError("FrontendMonitor ICache control interface is not bound")
        self.icache_control_snapshot = ICacheControlSnapshot(
            cycle=int(cycle),
            a_ready=self._read(self.icache_control_if.a_ready),
            d_valid=self._read(self.icache_control_if.d_valid),
            d_opcode=self._read(self.icache_control_if.d_bits_opcode),
            d_size=self._read(self.icache_control_if.d_bits_size),
            d_source=self._read(self.icache_control_if.d_bits_source),
            d_data=self._read(self.icache_control_if.d_bits_data),
        )
        return self.icache_control_snapshot

    def capture_ptw_agent_inputs(self, cycle: int) -> PTWAgentInputSnapshot:
        if self.ptw_if is None or self.csr_ctrl_if is None:
            raise AssertionError("FrontendMonitor PTW observation interfaces are not bound")
        ptw = self.ptw_if
        csr = self.csr_ctrl_if
        req_valid = self._read(ptw.req_0_valid)
        sfence_valid = self._read(ptw.sfence_valid)
        context = {
            "cycle": int(cycle),
            "priv_imode": self._read(csr.io_tlbCsr_priv_imode, 3),
            "priv_virt": self._read(csr.io_tlbCsr_priv_virt),
            "satp_mode": self._read(csr.io_tlbCsr_satp_mode),
            "satp_asid": self._read(csr.io_tlbCsr_satp_asid),
            "satp_ppn": self._read(csr.io_tlbCsr_satp_ppn),
            "vsatp_mode": self._read(csr.io_tlbCsr_vsatp_mode),
            "vsatp_asid": self._read(csr.io_tlbCsr_vsatp_asid),
            "vsatp_ppn": self._read(csr.io_tlbCsr_vsatp_ppn),
            "hgatp_mode": self._read(csr.io_tlbCsr_hgatp_mode),
            "hgatp_vmid": self._read(csr.io_tlbCsr_hgatp_vmid),
            "hgatp_ppn": self._read(csr.io_tlbCsr_hgatp_ppn),
            "sfence_valid": sfence_valid,
            "sfence_bits_rs1": self._read(ptw.sfence_bits_rs1),
            "sfence_bits_rs2": self._read(ptw.sfence_bits_rs2),
            "sfence_bits_addr": self._read(ptw.sfence_bits_addr),
            "sfence_bits_id": self._read(ptw.sfence_bits_id),
            "sfence_bits_hv": self._read(ptw.sfence_bits_hv),
            "sfence_bits_hg": self._read(ptw.sfence_bits_hg),
        } if req_valid else {}
        self.ptw_agent_input_snapshot = PTWAgentInputSnapshot(
            cycle=int(cycle),
            sfence_valid=sfence_valid,
            req_valid=req_valid,
            req_vpn=self._read(ptw.req_0_bits_vpn),
            req_s2xlate=self._read(ptw.req_0_bits_s2xlate),
            req_get_gpa=(
                int(self.ptw_get_gpa_provider() or 0)
                if req_valid and self.ptw_get_gpa_provider is not None else 0
            ),
            resp_ready=self._read(ptw.resp_ready, 1),
            context=context,
        )
        return self.ptw_agent_input_snapshot

    def capture_cycle_observation(self, cycle: int) -> FrontendCycleSnapshot:
        if (
            self.cfvec_if is None
            or self.backend_to_ftq_if is None
            or self.from_ftq_if is None
            or self.ptw_if is None
        ):
            raise AssertionError("FrontendMonitor observation interfaces are not bound")
        backend = BackendObservationSnapshot(
            from_ftq_wen=self._read(self.from_ftq_if.io_backend_fromFtq_wen),
            from_ftq_ftq_idx=self._read(self.from_ftq_if.io_backend_fromFtq_ftqIdx),
            from_ftq_start_pc_addr=self._read(self.from_ftq_if.io_backend_fromFtq_startPc_addr),
        )
        cfvec = self.capture_cfvec_snapshot(cycle)
        req_valid = self._read(self.ptw_if.req_0_valid)
        req_ready = self._read(self.ptw_if.req_0_ready)
        resp_valid = self._read(self.ptw_if.resp_valid)
        ptw = {
            "req_valid": req_valid,
            "req_ready": req_ready,
            "req_vpn": self._read(self.ptw_if.req_0_bits_vpn),
            "req_s2xlate": self._read(self.ptw_if.req_0_bits_s2xlate),
            "req_get_gpa": (
                self.ptw_agent_input_snapshot.req_get_gpa
                if req_valid and req_ready and self.ptw_agent_input_snapshot is not None
                and self.ptw_agent_input_snapshot.cycle == int(cycle)
                else int(self.ptw_get_gpa_provider() or 0) if req_valid and req_ready else 0
            ),
            "resp_valid": resp_valid,
            "resp_ready": self._read(self.ptw_if.resp_ready),
            "resp": {} if not resp_valid else {
                name.removeprefix("resp_bits_"): self._read(getattr(self.ptw_if, name))
                for name in PTWBundle.SIGNAL_BINDINGS if name.startswith("resp_bits_")
            },
        }
        if resp_valid:
            for name in ("s1_ppn_low", "s1_valididx", "s1_pteidx"):
                ptw["resp"][name] = tuple(
                    self._read(signal) for signal in getattr(self.ptw_if, f"resp_bits_{name}")
                )
        redirect = {
            "valid": self._read(self.backend_to_ftq_if.redirect_valid),
            "pc": self._read(self.backend_to_ftq_if.redirect_bits_pc),
            "target": self._read(self.backend_to_ftq_if.redirect_bits_target),
            "taken": self._read(self.backend_to_ftq_if.redirect_bits_taken),
            "level": self._read(self.backend_to_ftq_if.redirect_bits_level),
            "debug_is_ctrl": self._read(self.backend_to_ftq_if.redirect_bits_debug_is_ctrl),
            "debug_is_mem_vio": self._read(self.backend_to_ftq_if.redirect_bits_debug_is_mem_vio),
        }
        self.cycle_snapshot = FrontendCycleSnapshot(int(cycle), backend, cfvec, redirect, ptw)
        return self.cycle_snapshot

    def on_hardware_reset(self) -> None:
        self.cycle_snapshot = None
        self.icache_request_snapshot = None
        self.uncache_request_snapshot = None
        self.icache_control_snapshot = None
        self.ptw_agent_input_snapshot = None

    def clear(self) -> None:
        self.cycle_snapshot = None
        self.icache_request_snapshot = None
        self.uncache_request_snapshot = None
        self.icache_control_snapshot = None
        self.ptw_agent_input_snapshot = None
        self._checker.clear()

    def set_expected_pc(self, pc: int) -> None:
        self._checker.set_expected_pc(pc)

    def notify_redirect(self, target_pc: int, reason: str = "", grace_cycles: int = 2) -> None:
        self._checker.notify_redirect(target_pc, reason=reason, grace_cycles=grace_cycles)

    def set_translation_context(self, *, s2xlate: int, priv_imode: int) -> None:
        self._checker.set_translation_context(s2xlate=s2xlate, priv_imode=priv_imode)

    def get_errors(self) -> list[dict]:
        return self._checker.get_errors()

    def get_stats(self) -> dict:
        return self._checker.get_stats()

    def recent_pcs(self, limit: int = 32) -> list[int]:
        if limit <= 0:
            return []
        return [observation.pc for observation in self.observations[-limit:]]


__all__ = [
    "Observation", "FrontendCycleSnapshot", "ICacheRequestSnapshot",
    "UncacheRequestSnapshot", "ICacheControlSnapshot", "FrontendMonitor",
    "PTWAgentInputSnapshot",
]

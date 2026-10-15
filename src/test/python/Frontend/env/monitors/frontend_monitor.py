from __future__ import annotations

from dataclasses import dataclass
from typing import Optional

from ..bundles import BackendObserveBundle
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


class FrontendMonitor:
    def __init__(self) -> None:
        self.interface = None
        self.from_ftq_if = None
        self.ptw_if = None
        self.icache_if = None
        self.uncache_if = None
        self.icache_control_if = None
        self.ptw_get_gpa_provider = None
        self.dut = None
        self.cycle_snapshot: Optional[FrontendCycleSnapshot] = None
        self.icache_request_snapshot: Optional[ICacheRequestSnapshot] = None
        self.uncache_request_snapshot: Optional[UncacheRequestSnapshot] = None
        self.icache_control_snapshot: Optional[ICacheControlSnapshot] = None
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

    def _read_dut_signal(self, name: str) -> int:
        return self._read(getattr(self.dut, name, None))

    def bind(self, target) -> None:
        if not isinstance(target, BackendObserveBundle):
            raise TypeError("FrontendMonitor requires an explicitly bound BackendObserveBundle")
        self.interface = target

    def bind_observation_interfaces(self, *, from_ftq_if, ptw_if, ptw_get_gpa_provider) -> None:
        self.from_ftq_if = from_ftq_if
        self.ptw_if = ptw_if
        self.ptw_get_gpa_provider = ptw_get_gpa_provider

    def bind_icache(self, icache_if) -> None:
        self.icache_if = icache_if

    def bind_uncache(self, uncache_if) -> None:
        self.uncache_if = uncache_if

    def bind_icache_control(self, icache_control_if) -> None:
        self.icache_control_if = icache_control_if

    def bind_dut(self, dut) -> None:
        self.dut = dut

    def attach_checker(self, checker) -> None:
        self._checker = checker

    def _read_cfvec_slot(self, name: str, slot: int) -> int:
        signals = getattr(self.interface, name, ())
        try:
            return self._read(signals[int(slot)])
        except (IndexError, TypeError):
            return 0

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

    def capture_cycle_observation(self, cycle: int) -> FrontendCycleSnapshot:
        if self.interface is None or self.from_ftq_if is None or self.ptw_if is None:
            raise AssertionError("FrontendMonitor observation interfaces are not bound")
        backend = BackendObservationSnapshot(
            from_ftq_wen=self._read(self.from_ftq_if.io_backend_fromFtq_wen),
            from_ftq_ftq_idx=self._read(self.from_ftq_if.io_backend_fromFtq_ftqIdx),
            from_ftq_start_pc_addr=self._read(self.from_ftq_if.io_backend_fromFtq_startPc_addr),
        )
        cfvec = capture_cfvec_snapshot(cycle, self._read_cfvec_slot)
        req_valid = self._read(self.ptw_if.req_0_valid)
        req_ready = self._read(self.ptw_if.req_0_ready)
        resp_valid = self._read(self.ptw_if.resp_valid)
        ptw = {
            "req_valid": req_valid,
            "req_ready": req_ready,
            "req_vpn": self._read(self.ptw_if.req_0_bits_vpn),
            "req_s2xlate": self._read(self.ptw_if.req_0_bits_s2xlate),
            "req_get_gpa": int(self.ptw_get_gpa_provider() or 0) if req_valid and req_ready else 0,
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
            "valid": self._read(self.interface.redirect_valid),
            "pc": self._read(self.interface.redirect_bits_pc),
            "target": self._read(self.interface.redirect_bits_target),
            "taken": self._read(self.interface.redirect_bits_taken),
            "level": self._read_dut_signal("io_backend_toFtq_redirect_bits_level"),
            "debug_is_ctrl": self._read_dut_signal("io_backend_toFtq_redirect_bits_debugIsCtrl"),
            "debug_is_mem_vio": self._read_dut_signal("io_backend_toFtq_redirect_bits_debugIsMemVio"),
        }
        self.cycle_snapshot = FrontendCycleSnapshot(int(cycle), backend, cfvec, redirect, ptw)
        return self.cycle_snapshot

    def on_hardware_reset(self) -> None:
        self.cycle_snapshot = None
        self.icache_request_snapshot = None
        self.uncache_request_snapshot = None
        self.icache_control_snapshot = None

    def clear(self) -> None:
        self.cycle_snapshot = None
        self.icache_request_snapshot = None
        self.uncache_request_snapshot = None
        self.icache_control_snapshot = None
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
]

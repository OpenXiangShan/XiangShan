from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import Callable, Dict, List, Optional

from ..bundles import BackendObserveBundle, bind_bundle_optional
from ..model.branch_checker import BranchChecker
from ..model.memory_model import MemoryModel
from ..rvc_decoder import expand_rvc


@dataclass
class Observation:
    cycle: int
    slot: int
    pc: int
    instr: int
    is_rvc: bool
    pred_taken: bool


class FrontendMonitor:
    def __init__(
        self,
        memory: Optional[MemoryModel] = None,
        branch_checker: Optional[BranchChecker] = None,
        redirect_sync_max: int = 32,
    ) -> None:
        self.logger = logging.getLogger("env.monitor")
        self.interface = None
        self.memory = memory
        self.branch_checker = branch_checker
        self.expected_pc: Optional[int] = None
        self.redirect_grace = 0
        self.wait_sync_after_redirect = False
        self.errors: List[dict] = []
        self.observations: List[Observation] = []

        self.cycles_total = 0
        self.slots_total = 0
        self.slots_valid = 0
        self.redirect_count = 0
        self.exception_mark_count = 0
        self.current_cycle = 0
        self.event_sink: Optional[Callable[[Dict], None]] = None
        self.redirect_sync_max: int = int(redirect_sync_max)
        self.redirect_sync_deadline: int = 0
        self.backend_model = None

    def _golden_step_bytes(self, pc: int) -> int:
        if self.memory is None:
            return 4
        try:
            half = int(self.memory.read_u16(int(pc))) & 0xFFFF
            return 2 if (half & 0x3) != 0x3 else 4
        except Exception:
            return 4

    @staticmethod
    def _decode_b_imm(instr: int) -> int:
        imm = (
            (((instr >> 31) & 0x1) << 12)
            | (((instr >> 7) & 0x1) << 11)
            | (((instr >> 25) & 0x3F) << 5)
            | (((instr >> 8) & 0xF) << 1)
        )
        if imm & (1 << 12):
            imm -= 1 << 13
        return imm

    @staticmethod
    def _decode_j_imm(instr: int) -> int:
        imm = (
            (((instr >> 31) & 0x1) << 20)
            | (((instr >> 12) & 0xFF) << 12)
            | (((instr >> 20) & 0x1) << 11)
            | (((instr >> 21) & 0x3FF) << 1)
        )
        if imm & (1 << 20):
            imm -= 1 << 21
        return imm

    def _golden_next_pc(self, pc: int, instr: int, is_rvc: bool, pred_taken: bool) -> Optional[int]:
        """Return next expected PC after this instruction, or None for indirect jumps."""
        step = 2 if is_rvc else 4
        opc = instr & 0x7F
        if opc == 0x6F:
            return (pc + self._decode_j_imm(instr)) & 0xFFFFFFFFFFFFFFFF
        if opc == 0x63 and pred_taken:
            return (pc + self._decode_b_imm(instr)) & 0xFFFFFFFFFFFFFFFF
        if opc == 0x67:
            return None
        return pc + step

    @staticmethod
    def _sequential_next_pc(pc: int, is_rvc: bool) -> int:
        step = 2 if is_rvc else 4
        return (int(pc) + step) & 0xFFFFFFFFFFFFFFFF

    def _suppress_pc_mismatch(self, is_sync: bool) -> bool:
        if is_sync:
            return False
        if self.wait_sync_after_redirect:
            return True
        if self.redirect_grace > 0:
            self.redirect_grace -= 1
            return True
        return False

    @staticmethod
    def _read(signal, default: int = 0) -> int:
        try:
            value = getattr(signal, "value", None)
            return default if value is None else int(value)
        except Exception:
            return default

    def bind(self, target) -> None:
        if isinstance(target, BackendObserveBundle):
            self.interface = target
            return
        self.interface = bind_bundle_optional(BackendObserveBundle, target)

    def set_event_sink(self, sink: Optional[Callable[[Dict], None]]) -> None:
        self.event_sink = sink

    def attach_backend_model(self, backend_model) -> None:
        self.backend_model = backend_model

    def _emit(self, cycle: int, event_type: str, payload: Dict, level: str = "INFO") -> None:
        if self.event_sink is None:
            return
        self.event_sink(
            {
                "type": event_type,
                "source": "frontend_monitor",
                "cycle": int(cycle),
                "level": level,
                "payload": payload,
            }
        )

    def set_expected_pc(self, pc: int) -> None:
        self.expected_pc = int(pc)
        self.wait_sync_after_redirect = False

    def notify_redirect(self, target_pc: int, reason: str = "", grace_cycles: int = 2) -> None:
        self.expected_pc = int(target_pc)
        self.redirect_grace = max(0, int(grace_cycles))
        self.wait_sync_after_redirect = True
        self.redirect_sync_deadline = self.current_cycle + self.redirect_sync_max
        self.redirect_count += 1
        self.logger.info(
            "redirect notified: target=0x%x reason=%s grace=%d",
            int(target_pc),
            str(reason),
            self.redirect_grace,
        )
        self._emit(self.current_cycle, "monitor.redirect", {"target_pc": int(target_pc), "reason": str(reason)})
        self._emit(
            self.current_cycle,
            "monitor.pc_track",
            {
                "frontend_pc": None,
                "golden_pc": int(target_pc),
                "frontend_valid": False,
                "redirect_update": True,
                "reason": str(reason),
            },
            level="DEBUG",
        )

    def _record_error(self, **kwargs) -> None:
        self.errors.append(kwargs)
        self.logger.warning("monitor error: %s", kwargs)
        self._emit(int(kwargs.get("cycle", 0)), "monitor.error", kwargs, level="WARNING")

    def on_clock_edge(self, cycle: int) -> None:
        if self.interface is None:
            return

        self.current_cycle = int(cycle)
        self.cycles_total += 1
        self.slots_total += 8
        cycle_golden_start = self.expected_pc
        if self.backend_model is not None:
            backend_golden_pc = self.backend_model.current_cycle_start_golden_pc()
            if backend_golden_pc is not None:
                cycle_golden_start = int(backend_golden_pc)
        cycle_frontend_pc: Optional[int] = None
        cycle_frontend_instr = 0
        cycle_frontend_slot = -1
        cycle_frontend_is_rvc = False
        cycle_frontend_pred_taken = False
        cycle_frontend_slots: List[dict] = []

        for i in range(8):
            if self._read(self.interface.cfvec_valid[i], 0) != 1:
                continue

            pc = self._read(self.interface.cfvec_pc[i], 0)
            instr = self._read(self.interface.cfvec_instr[i], 0)
            is_rvc = bool(self._read(self.interface.cfvec_is_rvc[i], 0))
            pred_taken = bool(self._read(self.interface.cfvec_pred_taken[i], 0))

            self.slots_valid += 1
            obs = Observation(
                cycle=cycle,
                slot=i,
                pc=pc,
                instr=instr,
                is_rvc=is_rvc,
                pred_taken=pred_taken,
            )
            self.observations.append(obs)
            if self.branch_checker is not None:
                self.branch_checker.observe(obs)
            cycle_frontend_slots.append(
                {
                    "slot": int(i),
                    "pc": int(pc),
                    "instr": int(instr),
                    "is_rvc": bool(is_rvc),
                    "pred_taken": bool(pred_taken),
                }
            )
            if cycle_frontend_pc is None:
                cycle_frontend_pc = int(pc)
                cycle_frontend_instr = int(instr)
                cycle_frontend_slot = int(i)
                cycle_frontend_is_rvc = bool(is_rvc)
                cycle_frontend_pred_taken = bool(pred_taken)

            golden_pc = self.expected_pc if self.expected_pc is not None else pc

            ex_sum = (
                self._read(self.interface.cfvec_exception_vec[i][1], 0)
                + self._read(self.interface.cfvec_exception_vec[i][2], 0)
                + self._read(self.interface.cfvec_exception_vec[i][12], 0)
                + self._read(self.interface.cfvec_exception_vec[i][19], 0)
                + self._read(self.interface.cfvec_exception_vec[i][20], 0)
            )
            if ex_sum > 0:
                self.exception_mark_count += 1

            is_sync = (golden_pc is None) or (pc == golden_pc)
            if golden_pc is not None and pc != golden_pc and not self._suppress_pc_mismatch(is_sync):
                self._record_error(
                    cycle=cycle,
                    slot=i,
                    kind="PC_MISMATCH",
                    expected=golden_pc,
                    actual=pc,
                )

            if self.memory is not None and not self.wait_sync_after_redirect and self.redirect_grace == 0:
                if is_rvc:
                    raw16 = self.memory.read_u16(int(pc)) & 0xFFFF
                    try:
                        exp = expand_rvc(raw16)
                    except ValueError:
                        self._record_error(
                            cycle=cycle,
                            slot=i,
                            kind="ILLEGAL_RVC",
                            pc=pc,
                            raw16=raw16,
                        )
                        exp = None
                else:
                    exp = self.memory.read_u32(int(pc)) & 0xFFFFFFFF
                got = int(instr) & 0xFFFFFFFF
                if exp is not None and exp != got:
                    self._record_error(
                        cycle=cycle,
                        slot=i,
                        kind="INSTR_MISMATCH",
                        pc=pc,
                        expected=exp,
                        actual=got,
                    )

            if self.wait_sync_after_redirect and not is_sync:
                pass
            else:
                if self.wait_sync_after_redirect and is_sync:
                    self.wait_sync_after_redirect = False
                    self.redirect_grace = 4 if self.expected_pc is None else 2
                ref_pc = int(golden_pc) if golden_pc is not None else int(pc)
                next_pc = self._golden_next_pc(ref_pc, int(instr), bool(is_rvc), bool(pred_taken))
                if next_pc is None:
                    self.wait_sync_after_redirect = True
                    self.expected_pc = None
                    self.redirect_sync_deadline = self.current_cycle + self.redirect_sync_max
                else:
                    if int(next_pc) != self._sequential_next_pc(ref_pc, bool(is_rvc)):
                        self.wait_sync_after_redirect = True
                        self.redirect_sync_deadline = self.current_cycle + self.redirect_sync_max
                    self.expected_pc = next_pc

        if self.wait_sync_after_redirect and self.current_cycle >= self.redirect_sync_deadline:
            self._record_error(
                cycle=cycle,
                kind="REDIRECT_TIMEOUT",
                expected_pc=self.expected_pc,
            )
            self.wait_sync_after_redirect = False
            self.expected_pc = None

        self._emit(
            cycle,
            "monitor.raw_fetch",
            {
                "golden_pc": int(cycle_golden_start) if cycle_golden_start is not None else None,
                "golden_slots": (
                    self.backend_model.current_cycle_start_golden_window(len(cycle_frontend_slots))
                    if self.backend_model is not None and hasattr(self.backend_model, "current_cycle_start_golden_window")
                    else ([int(cycle_golden_start)] if cycle_golden_start is not None and cycle_frontend_slots else [])
                ),
                "slots": cycle_frontend_slots,
            },
            level="DEBUG",
        )

        if self._read(self.interface.redirect_valid, 0) == 1:
            self._emit(
                cycle,
                "monitor.dut_redirect",
                {
                    "pc": int(self._read(self.interface.redirect_bits_pc, 0)),
                    "target_pc": int(self._read(self.interface.redirect_bits_target, 0)),
                    "taken": int(self._read(self.interface.redirect_bits_taken, 0)),
                },
                level="DEBUG",
            )

        if cycle_frontend_pc is not None:
            golden_track_pc = cycle_golden_start if cycle_golden_start is not None else cycle_frontend_pc
        else:
            golden_track_pc = cycle_golden_start
        if golden_track_pc is not None:
            self._emit(
                cycle,
                "monitor.pc_track",
                {
                    "slot": int(cycle_frontend_slot),
                    "frontend_pc": int(cycle_frontend_pc) if cycle_frontend_pc is not None else None,
                    "frontend_instr": int(cycle_frontend_instr) if cycle_frontend_pc is not None else None,
                    "golden_pc": int(golden_track_pc),
                    "frontend_valid": bool(cycle_frontend_pc is not None),
                    "is_rvc": bool(cycle_frontend_is_rvc),
                    "pred_taken": bool(cycle_frontend_pred_taken),
                    "frontend_slots": cycle_frontend_slots,
                    "redirect_update": False,
                },
                level="DEBUG",
            )

    def get_errors(self) -> List[dict]:
        return list(self.errors)

    def recent_pcs(self, limit: int = 32) -> List[int]:
        if limit <= 0:
            return []
        return [o.pc for o in self.observations[-limit:]]

    def get_stats(self) -> dict:
        bubble_ratio = 0.0
        if self.slots_total > 0:
            bubble_ratio = 1.0 - (self.slots_valid / float(self.slots_total))
        avg_fetch_width = 0.0
        if self.cycles_total > 0:
            avg_fetch_width = self.slots_valid / float(self.cycles_total)
        return {
            "cycles_total": self.cycles_total,
            "slots_total": self.slots_total,
            "slots_valid": self.slots_valid,
            "bubble_ratio": bubble_ratio,
            "avg_fetch_width": avg_fetch_width,
            "redirect_count": self.redirect_count,
            "exception_mark_count": self.exception_mark_count,
            "error_count": len(self.errors),
        }

    def clear(self) -> None:
        self.errors.clear()
        self.observations.clear()
        self.cycles_total = 0
        self.slots_total = 0
        self.slots_valid = 0
        self.redirect_count = 0
        self.exception_mark_count = 0
        self.current_cycle = 0
        self.expected_pc = None
        self.redirect_grace = 0
        self.wait_sync_after_redirect = False
        self.redirect_sync_deadline = 0


__all__ = ["Observation", "FrontendMonitor"]

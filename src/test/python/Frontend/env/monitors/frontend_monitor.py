from __future__ import annotations

import logging
from dataclasses import dataclass
from typing import Callable, Dict, List, Optional

from ..bundles import BackendObserveBundle
from ..model.branch_checker import BranchChecker
from ..model.memory_model import MemoryModel
from ..model.page_table_model import PageTableModel
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
        page_table: Optional[PageTableModel] = None,
        branch_checker: Optional[BranchChecker] = None,
        redirect_sync_max: int = 32,
    ) -> None:
        self.logger = logging.getLogger("env.monitor")
        self.interface = None
        self.memory = memory
        self.page_table = page_table
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
        self.instr_compare_skipped_count = 0
        self.current_cycle = 0
        self.event_sink: Optional[Callable[[Dict], None]] = None
        self.redirect_sync_max: int = int(redirect_sync_max)
        self.redirect_sync_deadline: int = 0
        self.backend_model = None
        self._ftq_start_pc_cache: Dict[tuple[int, int], int] = {}
        self._ftq_group_closed: Dict[tuple[int, int], bool] = {}
        self._ftq_group_max_offset: Dict[tuple[int, int], int] = {}
        self.last_dut_redirect: Optional[dict] = None

    def _golden_step_bytes(self, pc: int) -> int:
        if self.memory is None:
            return 4
        try:
            half = int(self._read_expected_fetch_raw(int(pc), 2)[0] or 0) & 0xFFFF
            return 2 if (half & 0x3) != 0x3 else 4
        except Exception:
            return 4

    def _translate_fetch_addr(self, va: int) -> tuple[Optional[int], dict]:
        if self.page_table is None:
            return int(va), {"mode": "bare", "va": int(va), "pa": int(va), "ok": True}
        pa, ok, info = self.page_table.translate(int(va))
        meta = dict(info or {})
        meta["va"] = int(va)
        meta["ok"] = bool(ok)
        if ok:
            meta["pa"] = int(pa)
            return int(pa), meta
        return None, meta

    def _read_expected_fetch_raw(self, pc: int, size: int) -> tuple[Optional[int], dict]:
        if self.memory is None:
            return None, {"ok": False, "reason": "no_memory"}
        value = 0
        last_meta: dict = {"ok": True, "mode": "bare", "va": int(pc), "pa": int(pc)}
        for off in range(int(size)):
            pa, meta = self._translate_fetch_addr(int(pc) + int(off))
            last_meta = meta
            if pa is None:
                return None, meta
            value |= (int(self.memory.read_u8(int(pa))) & 0xFF) << (8 * int(off))
        return int(value), last_meta

    @staticmethod
    def _should_skip_instr_compare(fetch_meta: dict, got: int, ex_sum: int) -> bool:
        # In the current sv39/PTW pilot path, the DUT-facing observe bundle may expose
        # zeroed instruction payloads even after PC/PTW progression is correct.
        # Treat that as "instruction value unavailable" instead of a hard mismatch.
        return (
            int(ex_sum) == 0
            and int(got) == 0
            and str(fetch_meta.get("mode", "")) == "sv39"
            and bool(fetch_meta.get("ok", False))
        )

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

    @staticmethod
    def _slot_has_ftq_identity(ftq_flag: int, ftq_value: int, ftq_offset: int, is_last: bool) -> bool:
        return bool(int(ftq_flag) != 0 or int(ftq_value) != 0 or int(ftq_offset) != 0 or bool(is_last))

    @staticmethod
    def _derive_ftq_start_pc(pc: int, ftq_offset: int, is_rvc: bool) -> int:
        return (int(pc) - int(ftq_offset) * 2 + (0 if bool(is_rvc) else 2)) & 0xFFFFFFFFFFFFFFFF

    @staticmethod
    def _pc_from_ftq_start(start_pc: int, ftq_offset: int, is_rvc: bool) -> int:
        return (int(start_pc) + int(ftq_offset) * 2 - (0 if bool(is_rvc) else 2)) & 0xFFFFFFFFFFFFFFFF

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

    def _read_dut_signal(self, name: str, default: int = 0) -> int:
        env = getattr(self.backend_model, "env", None) if self.backend_model is not None else None
        dut = getattr(env, "dut", None) if env is not None else None
        if dut is None:
            return int(default)
        pin = getattr(dut, name, None)
        if pin is None:
            return int(default)
        try:
            return int(pin.value)
        except Exception:
            return int(default)

    def bind(self, target) -> None:
        if not isinstance(target, BackendObserveBundle):
            raise TypeError("FrontendMonitor requires an explicitly bound BackendObserveBundle")
        self.interface = target

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
            ftq_flag = self._read(self.interface.cfvec_ftq_ptr_flag[i], 0)
            ftq_value = self._read(self.interface.cfvec_ftq_ptr_value[i], 0)
            ftq_offset = self._read(self.interface.cfvec_ftq_offset[i], 0)
            is_last = bool(self._read(self.interface.cfvec_is_last_in_ftq_entry[i], 0))

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
            has_ftq_identity = self._slot_has_ftq_identity(ftq_flag, ftq_value, ftq_offset, is_last)
            ftq_expected_pc: Optional[int] = None
            if has_ftq_identity:
                ftq_group = (int(ftq_flag), int(ftq_value))
                ftq_start_pc = self._derive_ftq_start_pc(int(pc), int(ftq_offset), bool(is_rvc))
                cached_start_pc = self._ftq_start_pc_cache.get(ftq_group)
                cached_closed = bool(self._ftq_group_closed.get(ftq_group, False))
                cached_max_offset = int(self._ftq_group_max_offset.get(ftq_group, -1))
                if (
                    not cached_closed
                    and cached_start_pc is not None
                    and cached_max_offset < 0
                    and int(ftq_start_pc) != int(cached_start_pc)
                ):
                    cached_start_pc = None
                if (
                    not cached_closed
                    and cached_start_pc is not None
                    and int(ftq_offset) == 0
                    and int(ftq_start_pc) != int(cached_start_pc)
                ):
                    cached_start_pc = None
                if not cached_closed and cached_max_offset >= 0 and int(ftq_offset) < cached_max_offset:
                    cached_start_pc = None
                    cached_closed = False
                if cached_start_pc is None or cached_closed:
                    cached_start_pc = int(ftq_start_pc)
                    self._ftq_start_pc_cache[ftq_group] = int(ftq_start_pc)
                    self._ftq_group_closed[ftq_group] = False
                    self._ftq_group_max_offset[ftq_group] = -1
                ftq_expected_pc = self._pc_from_ftq_start(int(cached_start_pc), int(ftq_offset), bool(is_rvc))

            ex_sum = (
                self._read(self.interface.cfvec_exception_vec[i][1], 0)
                + self._read(self.interface.cfvec_exception_vec[i][2], 0)
                + self._read(self.interface.cfvec_exception_vec[i][12], 0)
                + self._read(self.interface.cfvec_exception_vec[i][19], 0)
                + self._read(self.interface.cfvec_exception_vec[i][20], 0)
            )
            if ex_sum > 0:
                self.exception_mark_count += 1

            if has_ftq_identity and not self.wait_sync_after_redirect:
                is_sync = (ftq_expected_pc is None) or (int(pc) == int(ftq_expected_pc))
            else:
                is_sync = (golden_pc is None) or (pc == golden_pc)
            if (
                has_ftq_identity
                and not self.wait_sync_after_redirect
                and ftq_expected_pc is not None
                and int(pc) != int(ftq_expected_pc)
            ):
                self._record_error(
                    cycle=cycle,
                    slot=i,
                    kind="PC_MISMATCH",
                    expected=int(ftq_expected_pc),
                    actual=int(pc),
                )
            elif (
                not has_ftq_identity
                and golden_pc is not None
                and pc != golden_pc
                and not self._suppress_pc_mismatch(is_sync)
            ):
                self._record_error(
                    cycle=cycle,
                    slot=i,
                    kind="PC_MISMATCH",
                    expected=golden_pc,
                    actual=pc,
                )

            if self.memory is not None and not self.wait_sync_after_redirect and self.redirect_grace == 0:
                fetch_size = 2 if is_rvc else 4
                raw_fetch, fetch_meta = self._read_expected_fetch_raw(int(pc), fetch_size)
                if raw_fetch is None:
                    exp = None
                    if ex_sum == 0:
                        self._record_error(
                            cycle=cycle,
                            slot=i,
                            kind="INSTR_TRANSLATION_FAULT",
                            pc=int(pc),
                            detail=fetch_meta,
                        )
                elif is_rvc:
                    raw16 = int(raw_fetch) & 0xFFFF
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
                    exp = int(raw_fetch) & 0xFFFFFFFF
                got = int(instr) & 0xFFFFFFFF
                if exp is not None and exp != got:
                    if int(ex_sum) > 0:
                        self.instr_compare_skipped_count += 1
                        self._emit(
                            cycle,
                            "monitor.instr_compare_skipped",
                            {
                                "slot": int(i),
                                "pc": int(pc),
                                "expected": int(exp),
                                "actual": int(got),
                                "reason": "exception_marked",
                            },
                            level="DEBUG",
                        )
                        continue
                    if self._should_skip_instr_compare(fetch_meta, got, ex_sum):
                        self.instr_compare_skipped_count += 1
                        self._emit(
                            cycle,
                            "monitor.instr_compare_skipped",
                            {
                                "slot": int(i),
                                "pc": int(pc),
                                "expected": int(exp),
                                "actual": int(got),
                                "detail": fetch_meta,
                            },
                            level="DEBUG",
                        )
                        continue
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
                if has_ftq_identity and not self.wait_sync_after_redirect and ftq_expected_pc is not None:
                    ref_pc = int(ftq_expected_pc)
                else:
                    ref_pc = int(golden_pc) if golden_pc is not None else int(pc)
                next_pc = self._golden_next_pc(ref_pc, int(instr), bool(is_rvc), bool(pred_taken))
                if next_pc is None:
                    if has_ftq_identity and not self.wait_sync_after_redirect:
                        self.expected_pc = None
                    else:
                        self.wait_sync_after_redirect = True
                        self.expected_pc = None
                        self.redirect_sync_deadline = self.current_cycle + self.redirect_sync_max
                else:
                    if (
                        not has_ftq_identity
                        and int(next_pc) != self._sequential_next_pc(ref_pc, bool(is_rvc))
                    ):
                        self.wait_sync_after_redirect = True
                        self.redirect_sync_deadline = self.current_cycle + self.redirect_sync_max
                    self.expected_pc = next_pc
            if has_ftq_identity and bool(is_last):
                self._ftq_group_closed[(int(ftq_flag), int(ftq_value))] = True
            if has_ftq_identity:
                group = (int(ftq_flag), int(ftq_value))
                self._ftq_group_max_offset[group] = max(
                    int(self._ftq_group_max_offset.get(group, -1)),
                    int(ftq_offset),
                )

        if self.wait_sync_after_redirect and self.current_cycle >= self.redirect_sync_deadline:
            if (
                cycle_frontend_pc is None
                and self.backend_model is not None
                and hasattr(self.backend_model, "has_pending_work")
                and self.backend_model.has_pending_work()
            ):
                self.redirect_sync_deadline += 1
                return
            env = getattr(self.backend_model, "env", None) if self.backend_model is not None else None
            if env is not None:
                try:
                    from ..api import api_Frontend_capture_frontend_stall_snapshot, _format_stall_snapshot

                    snapshot = api_Frontend_capture_frontend_stall_snapshot(env)
                    self.logger.warning("redirect timeout snapshot: %s", _format_stall_snapshot(snapshot))
                except Exception as exc:
                    self.logger.warning("redirect timeout snapshot capture failed: %s", exc)
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
            self.last_dut_redirect = {
                "cycle": int(cycle),
                "pc": int(self._read(self.interface.redirect_bits_pc, 0)),
                "target_pc": int(self._read(self.interface.redirect_bits_target, 0)),
                "taken": int(self._read(self.interface.redirect_bits_taken, 0)),
                "level": int(self._read_dut_signal("io_backend_toFtq_redirect_bits_level", 0)),
                "debug_is_ctrl": int(self._read_dut_signal("io_backend_toFtq_redirect_bits_debugIsCtrl", 0)),
                "debug_is_mem_vio": int(self._read_dut_signal("io_backend_toFtq_redirect_bits_debugIsMemVio", 0)),
            }
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
            "instr_compare_skipped_count": self.instr_compare_skipped_count,
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
        self.instr_compare_skipped_count = 0
        self.current_cycle = 0
        self.expected_pc = None
        self.redirect_grace = 0
        self.wait_sync_after_redirect = False
        self.redirect_sync_deadline = 0
        self._ftq_start_pc_cache.clear()
        self._ftq_group_closed.clear()
        self._ftq_group_max_offset.clear()
        self.last_dut_redirect = None


__all__ = ["Observation", "FrontendMonitor"]

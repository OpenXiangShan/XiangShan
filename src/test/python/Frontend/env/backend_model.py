from __future__ import annotations

import logging
import random
from collections import deque
from dataclasses import replace
from typing import Callable, Deque, Dict, Optional

from .agents.backend_agent import BackendAgent
from .model.backend_state import BackendEvent, BackendState, FrontendPacket, FtqEntry, ResolveEntry
from .model.ftq_scoreboard import FtqScoreboard
from .signal_utils import get_sig, set_sig
from .trace import GoldenTrace, TraceEntry

_MIN_BACKEND_DELAY = 3
_GOLDEN_TRACE_RESOLVE_MIN_DELAY = 3
_GOLDEN_TRACE_RESOLVE_MAX_DELAY = 5

# BackendModel Task 8 compatibility checklist:
# - Public methods stay source-compatible: bind/attach_*/set_event_sink/set_golden_trace/
#   set_can_accept/inject_redirect/inject_exception/wait_for_commits/on_clock_edge/get_stats/
#   pending_work_count/has_pending_work/recent_events.
# - FrontendEnv clock-edge ordering stays fixed: canAccept+one-shot clear before sampling, then
#   resolves, commits, and finally pending event delivery.
# - commit/resolve/redirect FTQ pointer, valid-bit, and flush semantics must remain compatible.
# - golden trace attach/reset, per-cycle cursor snapshots, and wait_pc/cursor behavior must match.


class BackendModel:
    def __init__(
        self,
        ftq_size: int = 64,
        ibuf_watchdog_threshold: int = 32,
        safe_pc: int = 0x80000000,
        resolve_min_delay: int = 3,
        resolve_max_delay: int = 8,
        auto_redirect_on_golden_mispredict: bool = True,
    ) -> None:
        self.logger = logging.getLogger("env.backend_model")
        self.dut = None
        self.env = None
        self.monitor = None
        self.branch_checker = None
        self.event_sink: Optional[Callable[[Dict], None]] = None

        self.ftq_size = int(ftq_size)
        self.ibuf_watchdog_threshold = int(ibuf_watchdog_threshold)
        self.safe_pc = int(safe_pc)
        self.can_accept = 1

        self.resolve_min_delay = int(resolve_min_delay)
        self.resolve_max_delay = int(resolve_max_delay)
        self._default_resolve_min_delay = int(resolve_min_delay)
        self._default_resolve_max_delay = int(resolve_max_delay)
        self.auto_redirect_on_golden_mispredict = bool(auto_redirect_on_golden_mispredict)

        self.current_cycle = 0
        self.commit_count = 0
        self.ftq_entries: Deque[FtqEntry] = deque()
        self._pending_resolves: Deque[ResolveEntry] = deque()
        self._current_ftq_entry: Optional[FtqEntry] = None
        self._current_ftq_seen_packets: set[tuple[int, int, int, int, int]] = set()
        self.pending_events: Deque[BackendEvent] = deque()
        self.last_events: Deque[dict] = deque(maxlen=64)

        self.commit_ptr_flag = 0
        self.commit_ptr_value = 0
        self._reuse_commit_ptr_once = False
        self.ibuf_full_streak = 0
        self._ftq_start_pc_cache: Dict[int, int] = {}
        self._ftq_start_pc_by_value: Dict[int, int] = {}
        self._ftq_group_pc_history: Dict[tuple[int, int], list[tuple[int, bool]]] = {}
        self._pc_group_occurrences: Dict[int, list[tuple[int, int, int]]] = {}
        self._pending_level0_target_ftq: Optional[tuple[int, int]] = None
        self.golden_trace: Optional[GoldenTrace] = None
        self.golden_resync_window = 64
        self._golden_wait_pc: Optional[int] = None
        self._cycle_start_golden_pc: Optional[int] = None
        self._cycle_start_golden_cursor: Optional[int] = None

        self._backend_state = BackendState(ftq_size=self.ftq_size)
        self._ftq_scoreboard = FtqScoreboard(self._backend_state)
        self._backend_agent = BackendAgent()

    def _sync_backend_state(self) -> BackendState:
        state = self._backend_state
        state.ftq_size = int(self.ftq_size)
        state.current_cycle = int(self.current_cycle)
        state.commit_count = int(self.commit_count)
        state.ftq_entries = self.ftq_entries
        state.pending_resolves = self._pending_resolves
        state.current_ftq_entry = self._current_ftq_entry
        state.current_ftq_seen_packets = self._current_ftq_seen_packets
        state.pending_events = self.pending_events
        state.last_events = self.last_events
        state.commit_ptr_flag = int(self.commit_ptr_flag)
        state.commit_ptr_value = int(self.commit_ptr_value)
        state.reuse_commit_ptr_once = bool(self._reuse_commit_ptr_once)
        state.ibuf_full_streak = int(self.ibuf_full_streak)
        state.ftq_start_pc_cache = self._ftq_start_pc_cache
        state.ftq_start_pc_by_value = self._ftq_start_pc_by_value
        state.ftq_group_pc_history = self._ftq_group_pc_history
        state.pc_group_occurrences = self._pc_group_occurrences
        state.pending_level0_target_ftq = self._pending_level0_target_ftq
        return state

    def _apply_backend_state(self) -> None:
        state = self._backend_state
        self.current_cycle = int(state.current_cycle)
        self.commit_count = int(state.commit_count)
        self.ftq_entries = state.ftq_entries
        self._pending_resolves = state.pending_resolves
        self._current_ftq_entry = state.current_ftq_entry
        self._current_ftq_seen_packets = state.current_ftq_seen_packets
        self.pending_events = state.pending_events
        self.last_events = state.last_events
        self.commit_ptr_flag = int(state.commit_ptr_flag)
        self.commit_ptr_value = int(state.commit_ptr_value)
        self._reuse_commit_ptr_once = bool(state.reuse_commit_ptr_once)
        self.ibuf_full_streak = int(state.ibuf_full_streak)
        self._ftq_start_pc_cache = state.ftq_start_pc_cache
        self._ftq_start_pc_by_value = state.ftq_start_pc_by_value
        self._ftq_group_pc_history = state.ftq_group_pc_history
        self._pc_group_occurrences = state.pc_group_occurrences
        self._pending_level0_target_ftq = state.pending_level0_target_ftq

    @staticmethod
    def _clamp_backend_delay(delay_cycles: int) -> int:
        return max(_MIN_BACKEND_DELAY, int(delay_cycles))

    def _increment_ftq_ptr(self, flag: int, value: int) -> tuple[int, int]:
        return self._sync_backend_state().increment_ftq_ptr(flag, value)

    def _decrement_ftq_ptr(self, flag: int, value: int) -> tuple[int, int]:
        return self._sync_backend_state().decrement_ftq_ptr(flag, value)

    @staticmethod
    def _ftq_entry_matches(entry: FtqEntry, flag: int, value: int) -> bool:
        return BackendState.ftq_entry_matches(entry, flag, value)

    def _has_seen_ftq_ptr(self, flag: int, value: int) -> bool:
        return self._sync_backend_state().has_seen_ftq_ptr(flag, value)

    @staticmethod
    def _redirect_reuses_same_ftq_slot(ftq_offset: int, is_rvc: bool) -> bool:
        return BackendState.redirect_reuses_same_ftq_slot(ftq_offset, is_rvc)

    def _ftq_ptr_rank_after_commit(self, flag: int, value: int) -> int:
        return self._sync_backend_state().ftq_ptr_rank_after_commit(flag, value)

    def _ftq_ptr_survives_redirect(self, flag: int, value: int, redirect_rank: int, flush_itself: bool) -> bool:
        return self._sync_backend_state().ftq_ptr_survives_redirect(flag, value, redirect_rank, flush_itself)

    @staticmethod
    def _same_entry_offset_survives(ftq_offset: int, redirect_offset: int, flush_itself: bool) -> bool:
        return BackendState.same_entry_offset_survives(ftq_offset, redirect_offset, flush_itself)

    def _pending_event_survives_redirect(
        self,
        evt: BackendEvent,
        redirect_flag: int,
        redirect_value: int,
        redirect_offset: int,
        redirect_rank: int,
        flush_itself: bool,
        keep_cycle: int,
    ) -> bool:
        return self._sync_backend_state().pending_event_survives_redirect(
            evt,
            redirect_flag,
            redirect_value,
            redirect_offset,
            redirect_rank,
            flush_itself,
            keep_cycle,
        )

    def _find_next_commitable_entry(self) -> Optional[FtqEntry]:
        candidate = self._sync_backend_state().find_next_commitable_entry()
        self._apply_backend_state()
        return candidate

    def _seal_current_ftq_entry(self) -> None:
        self._sync_backend_state().seal_current_ftq_entry()
        self._apply_backend_state()

    def bind(self, dut) -> None:
        self.dut = dut
        self._backend_agent.bind(dut)

    def attach_env(self, env) -> None:
        self.env = env

    def attach_monitor(self, monitor) -> None:
        self.monitor = monitor

    def attach_branch_checker(self, checker) -> None:
        self.branch_checker = checker

    def set_event_sink(self, sink: Optional[Callable[[Dict], None]]) -> None:
        self.event_sink = sink

    def current_golden_pc(self) -> Optional[int]:
        trace = self.golden_trace
        if trace is None:
            return None
        cur = trace.peek()
        if cur is None:
            return None
        return int(cur.pc)

    def current_cycle_start_golden_pc(self) -> Optional[int]:
        if self._cycle_start_golden_pc is None:
            return self.current_golden_pc()
        return int(self._cycle_start_golden_pc)

    def current_cycle_start_golden_window(self, count: int) -> list[int]:
        trace = self.golden_trace
        if trace is None or self._cycle_start_golden_cursor is None or count <= 0:
            return []
        start = int(self._cycle_start_golden_cursor)
        stop = min(len(trace.entries), start + int(count))
        return [int(trace.entries[idx].pc) for idx in range(start, stop)]

    def set_golden_trace(self, trace: Optional[GoldenTrace]) -> None:
        self.golden_trace = trace
        self._golden_wait_pc = None
        self._cycle_start_golden_pc = None
        self._cycle_start_golden_cursor = None
        if self.golden_trace is not None:
            self.resolve_min_delay = _GOLDEN_TRACE_RESOLVE_MIN_DELAY
            self.resolve_max_delay = _GOLDEN_TRACE_RESOLVE_MAX_DELAY
            self.golden_trace.reset(0)
            self.logger.info("golden trace attached: entries=%d", len(self.golden_trace.entries))
        else:
            self.resolve_min_delay = int(self._default_resolve_min_delay)
            self.resolve_max_delay = int(self._default_resolve_max_delay)
            self.logger.info("golden trace detached")

    def _publish(self, event_type: str, payload: Dict, level: str = "INFO") -> None:
        if self.event_sink is None:
            return
        self.event_sink(
            {
                "type": event_type,
                "source": "backend_model",
                "cycle": int(self.current_cycle),
                "level": level,
                "payload": payload,
            }
        )

    def set_can_accept(self, value: int) -> None:
        self.can_accept = 1 if int(value) else 0
        self.logger.info("backend can_accept=%d", self.can_accept)
        self._publish("backend.can_accept", {"value": self.can_accept}, level="DEBUG")

    def _emit_event(self, kind: str, payload: dict) -> None:
        item = {"cycle": self.current_cycle, "kind": kind, **payload}
        self.last_events.append(item)
        self._publish(f"backend.{kind}", item, level="DEBUG")

    def _has_pending_control_event(self) -> bool:
        for evt in self.pending_events:
            if evt.kind in {"redirect", "exception"}:
                return True
        return False

    @staticmethod
    def _event_source_cycle(evt: BackendEvent) -> int:
        payload = evt.payload if isinstance(evt.payload, dict) else {}
        if "source_cycle" in payload:
            return int(payload["source_cycle"])
        if "queued_cycle" in payload:
            return int(payload["queued_cycle"])
        return -1

    def _target_observed_since(self, target_pc: int, start_cycle: int) -> bool:
        if self.monitor is None:
            return False
        for obs in reversed(self.monitor.observations):
            if int(obs.cycle) < int(start_cycle):
                break
            if int(obs.pc) == int(target_pc):
                return True
        return False

    def _target_observed_after_cycle(self, target_pc: int, start_cycle: int) -> bool:
        if self.monitor is None:
            return False
        for obs in reversed(self.monitor.observations):
            if int(obs.cycle) <= int(start_cycle):
                break
            if int(obs.pc) == int(target_pc):
                return True
        return False

    def _target_path_progressed_since(self, target_pc: int, start_cycle: int) -> bool:
        if self.monitor is None:
            return False
        saw_target = False
        for obs in self.monitor.observations:
            if int(obs.cycle) < int(start_cycle):
                continue
            if not saw_target:
                if int(obs.pc) == int(target_pc):
                    saw_target = True
                continue
            if int(obs.pc) != int(target_pc):
                return True
        return False

    def _target_path_progressed_after_cycle(self, target_pc: int, start_cycle: int) -> bool:
        if self.monitor is None:
            return False
        saw_target = False
        for obs in self.monitor.observations:
            if int(obs.cycle) <= int(start_cycle):
                continue
            if not saw_target:
                if int(obs.pc) == int(target_pc):
                    saw_target = True
                continue
            if int(obs.pc) != int(target_pc):
                return True
        return False

    def _target_observed_after_issue(self, inst_pc: int, target_pc: int, start_cycle: int) -> bool:
        if self.monitor is None:
            return False
        armed = False
        for obs in self.monitor.observations:
            if int(obs.cycle) < int(start_cycle):
                continue
            if not armed:
                if int(obs.cycle) > int(start_cycle):
                    armed = True
                elif int(obs.pc) == int(inst_pc):
                    armed = True
                    continue
                else:
                    continue
            if int(obs.pc) == int(target_pc):
                return True
        return False

    def _target_path_progressed_after_issue(self, inst_pc: int, target_pc: int, start_cycle: int) -> bool:
        if self.monitor is None:
            return False
        armed = False
        saw_target = False
        for obs in self.monitor.observations:
            if int(obs.cycle) < int(start_cycle):
                continue
            if not armed:
                if int(obs.cycle) > int(start_cycle):
                    armed = True
                elif int(obs.pc) == int(inst_pc):
                    armed = True
                    continue
                else:
                    continue
            if not saw_target:
                if int(obs.pc) == int(target_pc):
                    saw_target = True
                continue
            if int(obs.pc) != int(target_pc):
                return True
        return False

    def _queue_redirect_event(
        self,
        target_pc: int,
        reason: str,
        delay_cycles: int = _MIN_BACKEND_DELAY,
        flush_on_drive: bool = False,
        payload_extra: Optional[Dict] = None,
    ) -> None:
        ready_cycle = self.current_cycle + self._clamp_backend_delay(delay_cycles)
        from_pc = int(target_pc)
        if self.monitor is not None and self.monitor.observations:
            from_pc = int(self.monitor.observations[-1].pc)
        payload = {
            "target_pc": int(target_pc),
            "reason": str(reason),
            "flush_on_drive": bool(flush_on_drive),
            "queued_cycle": int(self.current_cycle),
        }
        if payload_extra:
            payload.update(payload_extra)
        if "pc" in payload:
            from_pc = int(payload["pc"])
        self.pending_events.append(
            BackendEvent(
                kind="redirect",
                ready_cycle=ready_cycle,
                payload=payload,
            )
        )
        self.logger.info(
            "redirect queued: target=0x%x reason=%s ready_cycle=%d",
            int(target_pc),
            str(reason),
            int(ready_cycle),
        )
        self._publish(
            "backend.redirect_queued",
            {
                "target_pc": int(target_pc),
                "from_pc": int(from_pc),
                "reason": str(reason),
                "ready_cycle": int(ready_cycle),
            },
            level="DEBUG",
        )

    @staticmethod
    def _trace_entry_matches_obs(entry: TraceEntry, pc: int) -> bool:
        return int(entry.pc) == int(pc)

    @staticmethod
    def _sequential_next_pc(pc: int, is_rvc: bool) -> int:
        step = 2 if is_rvc else 4
        return (int(pc) + step) & 0xFFFFFFFFFFFFFFFF

    @staticmethod
    def _encode_backend_addr(addr: int) -> int:
        return int(addr) >> 1

    @staticmethod
    def _decode_backend_addr(addr: int) -> int:
        return int(addr) << 1

    def _consume_golden_entry(self, pc: int) -> Optional[TraceEntry]:
        trace = self.golden_trace
        if trace is None:
            return None
        cur = trace.peek()
        if cur is None:
            return None
        if self._trace_entry_matches_obs(cur, pc):
            trace.next_entry()
            return cur
        return None

    def _predicted_cfi_outcome(self, instr: int, pc: int, pred_taken: bool, is_rvc: bool) -> Optional[tuple]:
        step = 2 if is_rvc else 4
        opc = instr & 0x7F
        if opc == 0x63:
            branch_target = (int(pc) + self._decode_b_imm(int(instr))) & 0xFFFFFFFFFFFFFFFF
            fall_through = (int(pc) + step) & 0xFFFFFFFFFFFFFFFF
            return bool(pred_taken), (branch_target if pred_taken else fall_through)
        if opc == 0x6F:
            jump_target = (int(pc) + self._decode_j_imm(int(instr))) & 0xFFFFFFFFFFFFFFFF
            if not pred_taken:
                return False, (int(pc) + step) & 0xFFFFFFFFFFFFFFFF
            return True, jump_target
        if opc == 0x67:
            return bool(pred_taken), None
        return None

    def _golden_cfi_outcome(
        self,
        entry: Optional[TraceEntry],
        instr: int,
        pc: int,
        is_rvc: bool,
    ) -> Optional[tuple]:
        if entry is None:
            return None
        step = 2 if is_rvc else 4
        opc = instr & 0x7F
        if opc == 0x63:
            taken = bool(entry.taken)
            if taken:
                if entry.target_pc is not None:
                    target = int(entry.target_pc)
                else:
                    target = int(pc) + self._decode_b_imm(int(instr))
            else:
                target = int(pc) + step
            return taken, target & 0xFFFFFFFFFFFFFFFF
        if opc == 0x6F:
            if entry.target_pc is not None:
                target = int(entry.target_pc)
            else:
                target = int(pc) + self._decode_j_imm(int(instr))
            return True, target & 0xFFFFFFFFFFFFFFFF
        if opc == 0x67:
            if entry.target_pc is None:
                return True, None
            return True, int(entry.target_pc) & 0xFFFFFFFFFFFFFFFF
        return None

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

    def _classify_cfi(self, instr: int, pc: int, pred_taken: bool, is_rvc: bool = False):
        """Return (branch_type, ras_action, target, taken) or None if not CFI.

        target is the actual next PC:
          - taken branch / unconditional jump : branch/jump destination
          - not-taken branch                  : fall-through (pc + instr_size)
        """
        opc = instr & 0x7F
        instr_size = 2 if is_rvc else 4
        if opc == 0x63:
            branch_target = (pc + self._decode_b_imm(instr)) & 0xFFFFFFFFFFFFFFFF
            fall_through  = (pc + instr_size) & 0xFFFFFFFFFFFFFFFF
            target = branch_target if pred_taken else fall_through
            return 1, 0, target, pred_taken
        if opc == 0x6F:
            rd = (instr >> 7) & 0x1F
            target = (pc + self._decode_j_imm(instr)) & 0xFFFFFFFFFFFFFFFF
            ras_action = 2 if rd == 1 else 0  # Push if call (rd=ra)
            return 2, ras_action, target, True
        if opc == 0x67:
            rd = (instr >> 7) & 0x1F
            rs1 = (instr >> 15) & 0x1F
            is_rd_link = rd in (1, 5)
            is_rs1_link = rs1 in (1, 5)
            if is_rd_link and is_rs1_link and rd != rs1:
                ras_action = 3  # PopAndPush
            elif is_rs1_link and not is_rd_link:
                ras_action = 1  # Pop
            elif is_rd_link:
                ras_action = 2  # Push
            else:
                ras_action = 0
            return 3, ras_action, 0, True
        return None

    def _update_ftq_start_pc_cache(self) -> None:
        """Cache per-FTQ-entry start PCs reported by DUT's fromFtq interface.

        When io_backend_fromFtq_wen is high the DUT writes the canonical startPc
        for the given ftqIdx.  We index the cache by the packed FtqPtr integer
        (flag<<6 | value[5:0]) so that _sample_cfvec can look it up by the same
        key reconstructed from cfVec's ftqPtr_flag / ftqPtr_value fields.
        """
        assert self.dut is not None
        if get_sig(self.dut, "io_backend_fromFtq_wen", 0):
            ftq_idx = int(get_sig(self.dut, "io_backend_fromFtq_ftqIdx", 0))
            start_pc = self._decode_backend_addr(get_sig(self.dut, "io_backend_fromFtq_startPc_addr", 0))
            self._ftq_start_pc_cache[ftq_idx] = start_pc
            self._ftq_start_pc_by_value[ftq_idx & 0x3F] = start_pc

    def _has_later_cfvec_slot_matching_pc(self, current_slot: int, target_pc: int) -> bool:
        assert self.dut is not None
        for slot in range(int(current_slot) + 1, 8):
            base = f"io_backend_cfVec_{slot}_"
            if get_sig(self.dut, base + "valid", 0) != 1:
                continue
            if int(get_sig(self.dut, base + "bits_pc", 0)) == int(target_pc):
                return True
        return False

    def _record_ftq_group_pc(self, ftq_flag: int, ftq_value: int, pc: int, is_rvc: bool) -> None:
        group = (int(ftq_flag), int(ftq_value))
        history = self._ftq_group_pc_history.setdefault(group, [])
        entry = (int(pc), bool(is_rvc))
        if history and history[-1] == entry:
            return
        history.append(entry)
        occurrences = self._pc_group_occurrences.setdefault(int(pc), [])
        occurrence = (int(ftq_flag), int(ftq_value), len(history) - 1)
        if occurrences and occurrences[-1] == occurrence:
            return
        occurrences.append(occurrence)

    def _observed_ftq_start_pc(self, flag: int, value: int) -> Optional[int]:
        history = self._ftq_group_pc_history.get((int(flag), int(value)))
        if history:
            return int(history[0][0])
        start_pc = self._ftq_start_pc_by_value.get(int(value))
        if start_pc is None:
            return None
        return int(start_pc)

    def _simfrontend_redirect_drive_override(self, payload: dict) -> Optional[dict]:
        target_pc = int(payload.get("target_pc", self.safe_pc))
        redirect_flag = int(payload.get("ftq_flag", self.commit_ptr_flag))
        redirect_value = int(payload.get("ftq_value", self.commit_ptr_value))
        next_flag, next_value = self._increment_ftq_ptr(redirect_flag, redirect_value)
        next_start_pc = self._observed_ftq_start_pc(next_flag, next_value)
        if next_start_pc is not None and int(next_start_pc) == int(target_pc):
            return None

        best_group: Optional[tuple[int, int]] = None
        best_target_idx = -1
        for group_flag, group_value, target_idx in reversed(self._pc_group_occurrences.get(int(target_pc), [])):
            if target_idx <= 0:
                continue
            group = (int(group_flag), int(group_value))
            group_pcs = self._ftq_group_pc_history.get(group)
            if group_pcs is None or target_idx >= len(group_pcs):
                continue
            if int(group_pcs[target_idx][0]) != int(target_pc):
                continue
            best_group = group
            best_target_idx = target_idx
            break

        if best_group is None or best_target_idx <= 0:
            return None

        group_pcs = self._ftq_group_pc_history[best_group]
        group_start_pc = int(group_pcs[0][0])
        prev_pc, prev_is_rvc = group_pcs[best_target_idx - 1]
        prev_pc = int(prev_pc)
        prev_is_rvc = bool(prev_is_rvc)
        drive_ftq_offset = (
            (prev_pc - group_start_pc + (0 if prev_is_rvc else 2)) // 2
        ) & 0x1F
        return {
            "ftq_flag": int(best_group[0]),
            "ftq_value": int(best_group[1]),
            "pc": int(group_start_pc),
            "ftq_offset": int(drive_ftq_offset),
            "is_rvc": int(prev_is_rvc),
        }

    def _sample_cfvec(self) -> None:
        assert self.dut is not None
        for i in range(8):
            base = f"io_backend_cfVec_{i}_"
            if get_sig(self.dut, base + "valid", 0) != 1:
                continue
            pc = get_sig(self.dut, base + "bits_pc", 0)
            instr = get_sig(self.dut, base + "bits_instr", 0)
            is_rvc = bool(get_sig(self.dut, base + "bits_isRvc", 0))
            pred_taken = bool(get_sig(self.dut, base + "bits_predTaken", 0))
            ftq_flag = get_sig(self.dut, base + "bits_ftqPtr_flag", 0)
            ftq_value = get_sig(self.dut, base + "bits_ftqPtr_value", 0)
            ftq_offset = get_sig(self.dut, base + "bits_ftqOffset", 0)
            is_last = bool(get_sig(self.dut, base + "bits_isLastInFtqEntry", 0))

            if self._golden_wait_pc is not None:
                if int(pc) != int(self._golden_wait_pc):
                    continue
                self._golden_wait_pc = None

            golden_entry = self._consume_golden_entry(int(pc))

            # Start or continue the current FTQ entry
            if self._current_ftq_entry is None or (
                self._current_ftq_entry.ftq_flag != ftq_flag
                or self._current_ftq_entry.ftq_value != ftq_value
            ):
                if self._current_ftq_entry is not None:
                    # Redirect/truncation can cut an FTQ entry short before DUT raises
                    # isLastInFtqEntry for the surviving prefix. Close the prefix so
                    # later resolve/commit bookkeeping does not silently lose it.
                    self.logger.warning(
                        "FTQ entry transition without isLastInFtqEntry: flag=%d value=%d",
                        self._current_ftq_entry.ftq_flag, self._current_ftq_entry.ftq_value,
                    )
                    self._seal_current_ftq_entry()
                self._current_ftq_entry = FtqEntry(ftq_flag=ftq_flag, ftq_value=ftq_value)
                self._current_ftq_seen_packets.clear()
                if self._pending_level0_target_ftq == (int(ftq_flag), int(ftq_value)):
                    self._pending_level0_target_ftq = None

            packet_key = (
                int(pc),
                int(instr),
                1 if bool(is_rvc) else 0,
                int(ftq_offset),
                1 if bool(pred_taken) else 0,
            )
            if packet_key in self._current_ftq_seen_packets:
                continue
            self._current_ftq_seen_packets.add(packet_key)
            self._record_ftq_group_pc(int(ftq_flag), int(ftq_value), int(pc), bool(is_rvc))

            # Identify CFI and enqueue resolve.
            # resolve.pc_addr = fetch-block start PC from DUT's fromFtq.startPc (authoritative);
            # fall back to pc - ftqOffset*2 if the cache entry is not yet available.
            # resolve.target  = actual next PC (branch target if taken, fall-through if not)
            ftq_key = (int(ftq_flag) << 6) | (int(ftq_value) & 0x3F)
            start_pc = self._ftq_start_pc_cache.get(
                ftq_key,
                self._ftq_start_pc_by_value.get(
                    int(ftq_value),
                    (
                        int(pc) - int(ftq_offset) * 2 + (0 if bool(is_rvc) else 2)
                    ) & 0xFFFFFFFFFFFFFFFF,
                ),
            )
            cfi = self._classify_cfi(instr, pc, pred_taken, is_rvc)
            if cfi is not None:
                branch_type, ras_action, target, taken = cfi
                mispredict = False
                golden_cfi = self._golden_cfi_outcome(golden_entry, int(instr), int(pc), bool(is_rvc))
                if golden_cfi is not None:
                    golden_taken, golden_target = golden_cfi
                    taken = bool(golden_taken)
                    if golden_target is not None:
                        target = int(golden_target)
                    pred_cfi = self._predicted_cfi_outcome(int(instr), int(pc), bool(pred_taken), bool(is_rvc))
                    if pred_cfi is not None:
                        pred_taken_out, pred_target = pred_cfi
                        if bool(pred_taken_out) != bool(golden_taken):
                            mispredict = True
                        elif bool(golden_taken) and golden_target is not None and pred_target is None:
                            mispredict = True
                        elif bool(golden_taken) and pred_target is not None and golden_target is not None:
                            mispredict = int(pred_target) != int(golden_target)
                if mispredict and self.branch_checker is not None:
                    self.branch_checker.record_mispredict()
                delay = random.randint(self.resolve_min_delay, self.resolve_max_delay)
                self._pending_resolves.append(ResolveEntry(
                    ready_cycle=self.current_cycle + delay,
                    inst_pc=int(pc),
                    pc=start_pc,
                    target=target,
                    taken=taken,
                    mispredict=mispredict,
                    ftq_flag=ftq_flag,
                    ftq_value=ftq_value,
                    ftq_offset=ftq_offset,
                    branch_type=branch_type,
                    ras_action=ras_action,
                    queued_cycle=int(self.current_cycle),
                    is_rvc=bool(is_rvc),
                ))
                self._current_ftq_entry.total_cfi += 1

            if is_last:
                self._seal_current_ftq_entry()

            if golden_entry is None or self.golden_trace is None:
                continue
            next_entry = self.golden_trace.peek()
            if next_entry is None:
                continue
            if int(next_entry.pc) == self._sequential_next_pc(int(pc), bool(is_rvc)):
                continue
            self._golden_wait_pc = int(next_entry.pc)
            if self._has_later_cfvec_slot_matching_pc(i, int(next_entry.pc)):
                continue
            if self._current_ftq_entry is not None:
                self._seal_current_ftq_entry()
            break

    def _drive_resolves(self) -> None:
        assert self.dut is not None
        driven_entries: list[ResolveEntry] = []
        to_remove = []
        frontier_pc = self.current_golden_pc()
        if frontier_pc is None and self._golden_wait_pc is not None:
            frontier_pc = int(self._golden_wait_pc)
        for entry in self._pending_resolves:
            if len(driven_entries) >= 3:
                break
            if entry.ready_cycle > self.current_cycle:
                continue
            effective_mispredict = bool(entry.mispredict)
            target_seen_after_queue = False
            target_path_progressed_after_queue = False
            target_seen_after_issue = False
            target_path_progressed_after_issue = False
            if frontier_pc is not None:
                target_seen_after_queue = self._target_observed_after_cycle(
                    int(entry.target),
                    int(entry.queued_cycle),
                )
                if target_seen_after_queue:
                    target_path_progressed_after_queue = self._target_path_progressed_after_cycle(
                        int(entry.target),
                        int(entry.queued_cycle),
                    )
                if int(entry.branch_type) != 3:
                    target_seen_after_issue = self._target_observed_after_issue(
                        int(entry.inst_pc),
                        int(entry.target),
                        int(entry.queued_cycle),
                    )
                    if target_seen_after_issue:
                        target_path_progressed_after_issue = self._target_path_progressed_after_issue(
                            int(entry.inst_pc),
                            int(entry.target),
                            int(entry.queued_cycle),
                        )
            if effective_mispredict and int(entry.branch_type) != 3 and (
                target_seen_after_issue or target_path_progressed_after_issue
            ):
                effective_mispredict = False
            if effective_mispredict and int(entry.branch_type) == 3 and target_path_progressed_after_queue:
                effective_mispredict = False
            allow_indirect_catchup_redirect = (
                effective_mispredict
                and int(entry.branch_type) == 3
                and target_seen_after_queue
                and not target_path_progressed_after_queue
            )
            if (
                effective_mispredict
                and frontier_pc is not None
                and int(entry.target) != int(frontier_pc)
                and not allow_indirect_catchup_redirect
            ):
                continue
            driven_entries.append(replace(entry, mispredict=bool(effective_mispredict)))
            redirect_level = 0
            if self.auto_redirect_on_golden_mispredict and effective_mispredict:
                self._queue_redirect_event(
                    target_pc=int(entry.target),
                    reason="golden_resolve_redirect",
                    delay_cycles=_MIN_BACKEND_DELAY,
                    flush_on_drive=True,
                    payload_extra={
                        "pc": int(entry.pc),
                        "taken": int(entry.taken),
                        "ftq_flag": int(entry.ftq_flag),
                        "ftq_value": int(entry.ftq_value),
                        "ftq_offset": int(entry.ftq_offset),
                        "branch_type": int(entry.branch_type),
                        "ras_action": int(entry.ras_action),
                        "source_cycle": int(entry.queued_cycle),
                        "is_rvc": int(entry.is_rvc),
                        "level": redirect_level,
                    },
                )
            entry_flushes_itself = bool(
                effective_mispredict
                and self.auto_redirect_on_golden_mispredict
                and (redirect_level & 0x1)
            )
            self._sync_backend_state()
            self._ftq_scoreboard.note_resolve(entry, self.current_cycle, entry_flushes_itself)
            self._apply_backend_state()
            to_remove.append(entry)
        self._backend_agent.drive_resolves(driven_entries)
        for entry in to_remove:
            self._pending_resolves.remove(entry)

    def _drive_commit(self) -> None:
        assert self.dut is not None
        self._backend_agent.drive_commit(None)
        if self.can_accept == 0:
            return
        if self._has_pending_control_event():
            return
        head = self._find_next_commitable_entry()
        if head is None:
            return
        self.ftq_entries.remove(head)
        if self._reuse_commit_ptr_once and self._ftq_entry_matches(head, self.commit_ptr_flag, self.commit_ptr_value):
            self._reuse_commit_ptr_once = False
        self.commit_ptr_flag = head.ftq_flag
        self.commit_ptr_value = head.ftq_value
        self.commit_count += 1
        self._backend_agent.drive_commit(head)
        self._emit_event("commit", {"commit_count": self.commit_count, "ftq_flag": head.ftq_flag, "ftq_value": head.ftq_value})

    def _clear_one_shot_signals(self) -> None:
        assert self.dut is not None
        self._backend_agent.clear_one_shot_signals()

    def _recompute_cfi_budgets_from_pending_resolves(self) -> None:
        self._sync_backend_state()
        self._ftq_scoreboard.recompute_cfi_budgets_from_pending_resolves()
        self._apply_backend_state()

    def _drive_redirect(self, payload: dict) -> None:
        assert self.dut is not None
        target_pc = int(payload.get("target_pc", self.safe_pc))
        reason = str(payload.get("reason", "redirect"))
        from_pc = int(payload.get("pc", target_pc))
        taken = int(payload.get("taken", 1))
        ftq_flag = int(payload.get("ftq_flag", self.commit_ptr_flag))
        ftq_value = int(payload.get("ftq_value", self.commit_ptr_value))
        ftq_offset = int(payload.get("ftq_offset", 0))
        branch_type = int(payload.get("branch_type", 0))
        ras_action = int(payload.get("ras_action", 0))
        is_rvc = int(payload.get("is_rvc", 0))
        level = int(payload.get("level", 0))
        flush_itself = bool(level & 0x1)
        if flush_itself:
            self._pending_level0_target_ftq = None
        else:
            next_target_ftq = self._increment_ftq_ptr(ftq_flag, ftq_value)
            self._pending_level0_target_ftq = None if self._has_seen_ftq_ptr(*next_target_ftq) else next_target_ftq
        if bool(payload.get("flush_on_drive", False)):
            state = self._sync_backend_state()
            self._ftq_scoreboard.apply_redirect_flush(
                ftq_flag=ftq_flag,
                ftq_value=ftq_value,
                ftq_offset=ftq_offset,
                flush_itself=flush_itself,
                keep_cycle=int(self.current_cycle),
                current_cycle=int(self.current_cycle),
                is_rvc=bool(is_rvc),
                pending_event_survives=state.pending_event_survives_redirect,
            )
            self._apply_backend_state()
        if "pc" not in payload and self.monitor is not None and self.monitor.observations:
            from_pc = int(self.monitor.observations[-1].pc)

        drive_ftq_flag = ftq_flag
        drive_ftq_value = ftq_value
        drive_from_pc = from_pc
        drive_ftq_offset = ftq_offset
        drive_is_rvc = is_rvc
        drive_override = self._simfrontend_redirect_drive_override(payload)
        if drive_override is not None:
            drive_ftq_flag = int(drive_override.get("ftq_flag", drive_ftq_flag))
            drive_ftq_value = int(drive_override.get("ftq_value", drive_ftq_value))
            drive_from_pc = int(drive_override.get("pc", drive_from_pc))
            drive_ftq_offset = int(drive_override.get("ftq_offset", drive_ftq_offset))
            drive_is_rvc = int(drive_override.get("is_rvc", drive_is_rvc))

        self._backend_agent.drive_redirect({
            "pc": drive_from_pc,
            "target_pc": target_pc,
            "taken": taken,
            "ftq_flag": drive_ftq_flag,
            "ftq_value": drive_ftq_value,
            "ftq_offset": drive_ftq_offset,
            "is_rvc": drive_is_rvc,
            "branch_type": branch_type,
            "ras_action": ras_action,
            "level": 0,
        })
        if self.monitor is not None:
            self.monitor.notify_redirect(target_pc, reason=reason)
        if "mispredict" in reason and self.branch_checker is not None:
            self.branch_checker.record_mispredict()
        self._emit_event("redirect", {"target_pc": target_pc, "reason": reason})
        self.logger.info(
            "redirect driven: from=0x%x target=0x%x reason=%s",
            from_pc,
            target_pc,
            reason,
        )
        self._publish(
            "backend.redirect",
            {"from_pc": int(from_pc), "target_pc": int(target_pc), "reason": reason},
        )

    def _process_pending_events(self) -> None:
        if not self.pending_events:
            return
        top = self.pending_events[0]
        if self.current_cycle < top.ready_cycle:
            return
        top = self.pending_events.popleft()
        if top.kind == "redirect":
            target_pc = top.payload.get("target_pc", None)
            queued_cycle = int(top.payload.get("queued_cycle", self.current_cycle))
            if target_pc is not None and self.monitor is not None:
                for obs in reversed(self.monitor.observations):
                    if int(obs.cycle) < queued_cycle:
                        break
                    if int(obs.pc) == int(target_pc):
                        return
        if top.kind == "redirect":
            self._drive_redirect(top.payload)
        elif top.kind == "exception":
            self._drive_redirect(top.payload)
            self._emit_event("exception", {"cause": top.payload.get("cause", 0)})

    def _watchdog(self) -> None:
        assert self.dut is not None
        if get_sig(self.dut, "io_frontendInfo_ibufFull", 0) == 1:
            self.ibuf_full_streak += 1
        else:
            self.ibuf_full_streak = 0
        if self.ibuf_full_streak >= self.ibuf_watchdog_threshold:
            self.inject_redirect(self.safe_pc, "ibuf_full_watchdog", delay_cycles=_MIN_BACKEND_DELAY)
            self.logger.warning(
                "ibufFull watchdog fired: threshold=%d safe_pc=0x%x",
                self.ibuf_watchdog_threshold,
                self.safe_pc,
            )
            self._publish(
                "backend.watchdog",
                {"reason": "ibuf_full", "threshold": int(self.ibuf_watchdog_threshold), "safe_pc": int(self.safe_pc)},
                level="WARNING",
            )
            self.ibuf_full_streak = 0

    def inject_redirect(self, target_pc: int, reason: str, delay_cycles: int = _MIN_BACKEND_DELAY) -> None:
        self._queue_redirect_event(
            target_pc=int(target_pc),
            reason=str(reason),
            delay_cycles=int(delay_cycles),
            flush_on_drive=False,
        )
        self._pending_resolves.clear()
        self._current_ftq_entry = None
        self._current_ftq_seen_packets.clear()
        self._golden_wait_pc = None
        # Note: do NOT clear ftq_entries — entries already dispatch-complete remain valid;
        # they will naturally drain via _drive_commit() until the redirect fires.

    def inject_exception(self, cause: int, tval: int, pc: int, delay_cycles: int = _MIN_BACKEND_DELAY) -> None:
        ready_cycle = self.current_cycle + self._clamp_backend_delay(delay_cycles)
        from_pc = int(pc)
        if self.monitor is not None and self.monitor.observations:
            from_pc = int(self.monitor.observations[-1].pc)
        self.pending_events.append(
            BackendEvent(
                kind="exception",
                ready_cycle=ready_cycle,
                payload={
                    "target_pc": int(pc),
                    "reason": "exception",
                    "cause": int(cause),
                    "tval": int(tval),
                },
            )
        )
        self.logger.info(
            "exception queued: cause=%d tval=0x%x pc=0x%x ready_cycle=%d",
            int(cause),
            int(tval),
            int(pc),
            int(ready_cycle),
        )
        self._publish(
            "backend.exception_queued",
            {
                "cause": int(cause),
                "tval": int(tval),
                "pc": int(pc),
                "from_pc": int(from_pc),
                "ready_cycle": int(ready_cycle),
            },
            level="DEBUG",
        )

    def wait_for_commits(self, n: int, max_cycles: int) -> int:
        target = max(0, int(n))
        start = self.commit_count
        if target == 0:
            return 0
        for _ in range(max(0, int(max_cycles))):
            if self.commit_count - start >= target:
                break
            if self.env is None:
                break
            self.env.step(1)
        return self.commit_count - start

    def on_clock_edge(self, cycle: int) -> None:
        if self.dut is None:
            return
        self.current_cycle = int(cycle)
        self._cycle_start_golden_cursor = None if self.golden_trace is None else int(self.golden_trace.cursor)
        self._cycle_start_golden_pc = self.current_golden_pc()
        self._backend_agent.start_cycle(self.can_accept)
        self._update_ftq_start_pc_cache()
        self._sample_cfvec()
        self._drive_resolves()
        self._drive_commit()
        self._process_pending_events()

    def get_stats(self) -> dict:
        return {
            "commit_count": self.commit_count,
            "ftq_entries_pending": len(self.ftq_entries),
            "pending_resolves": len(self._pending_resolves),
            "pending_events": len(self.pending_events),
            "can_accept": self.can_accept,
        }

    def pending_work_count(self) -> int:
        return (
            len(self.ftq_entries)
            + len(self._pending_resolves)
            + len(self.pending_events)
            + (1 if self._current_ftq_entry is not None else 0)
        )

    def has_pending_work(self) -> bool:
        return self.pending_work_count() > 0

    def recent_events(self, limit: int = 16) -> list:
        if limit <= 0:
            return []
        return list(self.last_events)[-limit:]

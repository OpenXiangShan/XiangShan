from __future__ import annotations

import logging
import random
from collections import deque
from dataclasses import replace
from typing import Callable, Deque, Dict, Optional

from .agents.backend_agent import BackendAgent
from .bundles import BackendCtrlBundle, BackendFromFtqBundle, BackendObserveBundle, FrontendInfoBundle, bind_bundle_optional
from .model.backend_runtime import BackendCycleActions, BackendObservationSnapshot
from .model.backend_state import (
    BackendEvent,
    BackendState,
    CommitInstruction,
    FrontendPacket,
    FtqEntry,
    GOLDEN_MATCH_STATE_MATCHED,
    GOLDEN_MATCH_STATE_MISMATCHED,
    PATH_STATE_CORRECT,
    PATH_STATE_WRONG,
    QueueInstr,
    ResolveEntry,
    ROB_COMMIT_STATE_PENDING,
    ROB_COMMIT_STATE_COMMITTED,
    CALL_RET_STATE_PENDING,
    RESOLVE_STATE_EMITTED,
    RESOLVE_STATE_NOT_NEEDED,
    RESOLVE_STATE_PENDING,
    RESOLVE_STATE_SKIPPED,
)
from .model.ftq_scoreboard import FtqScoreboard
from .trace import GoldenTrace, TraceEntry

_MIN_BACKEND_DELAY = 3
_GOLDEN_TRACE_RESOLVE_MIN_DELAY = 3
_GOLDEN_TRACE_RESOLVE_MAX_DELAY = 5

# BackendModel compatibility checklist:
# - Public methods stay source-compatible: bind/attach_*/set_event_sink/set_golden_trace/
#   set_can_accept/inject_redirect/inject_exception/wait_for_commits/on_clock_edge/get_stats/
#   pending_work_count/has_pending_work/recent_events.
# - FrontendEnv owns backend-agent/backend-observation ordering; BackendModel plans actions and
#   retains FTQ, golden-trace, commit, resolve, and redirect semantics.
# - The legacy model-owned drive path remains available only as a compatibility wrapper.


class BackendModel:
    def __init__(
        self,
        ftq_size: int = 64,
        ibuf_watchdog_threshold: int = 32,
        safe_pc: int = 0x80000000,
        instruction_commit_width: int = 8,
        resolve_min_delay: int = 3,
        resolve_max_delay: int = 8,
        redirect_min_delay: int = 5,
        redirect_max_delay: int = 8,
        commit_min_delay: int = 3,
        commit_max_delay: int = 10,
        auto_redirect_on_golden_mispredict: bool = True,
        random_seed: Optional[int] = None,
    ) -> None:
        self.logger = logging.getLogger("env.backend_model")
        self.dut = None
        self.drive_if = None
        self.observe_if = None
        self.from_ftq_if = None
        self.frontend_info_if = None
        self.env = None
        self.monitor = None
        self.branch_checker = None
        self.event_sink: Optional[Callable[[Dict], None]] = None

        self.ftq_size = int(ftq_size)
        self.ibuf_watchdog_threshold = int(ibuf_watchdog_threshold)
        self.safe_pc = int(safe_pc)
        self.instruction_commit_width = int(instruction_commit_width)
        if not 1 <= self.instruction_commit_width <= 8:
            raise ValueError("instruction_commit_width must be within [1, 8]")
        self.can_accept = 1

        self.resolve_min_delay = int(resolve_min_delay)
        self.resolve_max_delay = int(resolve_max_delay)
        self._default_resolve_min_delay = int(resolve_min_delay)
        self._default_resolve_max_delay = int(resolve_max_delay)
        self.redirect_min_delay = int(redirect_min_delay)
        self.redirect_max_delay = int(redirect_max_delay)
        if self.redirect_max_delay < self.redirect_min_delay:
            raise ValueError("redirect_max_delay must be >= redirect_min_delay")
        self.commit_min_delay = int(commit_min_delay)
        self.commit_max_delay = int(commit_max_delay)
        if self.commit_max_delay < self.commit_min_delay:
            raise ValueError("commit_max_delay must be >= commit_min_delay")
        self.auto_redirect_on_golden_mispredict = bool(auto_redirect_on_golden_mispredict)
        self._rng = random.Random(int(random_seed)) if random_seed is not None else random

        self.current_cycle = 0
        self.commit_count = 0
        self.ftq_entries: Deque[FtqEntry] = deque()
        self._pending_resolves: Deque[ResolveEntry] = deque()
        self._current_ftq_entry: Optional[FtqEntry] = None
        self._current_ftq_seen_packets: set[tuple[int, int, int, int, int]] = set()
        self._current_ftq_max_offset = -1
        self._current_ftq_contains_wait_pc = False
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
        self._cfvec_queue: Deque[QueueInstr] = deque()
        self._commit_queue: Deque[int] = deque()
        self._pending_redirect_origin_index: Optional[int] = None
        self._pending_queue_resolve_indices: Deque[int] = deque()
        self._pending_queue_call_ret_commit_indices: Deque[int] = deque()
        self._scheduled_queue_call_ret_commit_groups: Deque[tuple[int, list[int]]] = deque()
        self._visible_queue_call_ret_commit_group: list[int] = []
        self._semantic_recovery_target_pc: Optional[int] = None
        self._semantic_recovery_queue_start: Optional[int] = None
        self.golden_trace: Optional[GoldenTrace] = None
        self.golden_resync_window = 64
        self._golden_wait_pc: Optional[int] = None
        self._cycle_start_golden_pc: Optional[int] = None
        self._cycle_start_golden_cursor: Optional[int] = None
        self._last_observation = BackendObservationSnapshot()

        self._backend_state = BackendState(ftq_size=self.ftq_size)
        self._ftq_scoreboard = FtqScoreboard(self._backend_state)

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
        state.cfvec_queue = self._cfvec_queue
        state.commit_queue = self._commit_queue
        state.pending_redirect_origin_index = self._pending_redirect_origin_index
        state.pending_queue_resolve_indices = self._pending_queue_resolve_indices
        state.pending_queue_call_ret_commit_indices = self._pending_queue_call_ret_commit_indices
        state.scheduled_queue_call_ret_commit_groups = self._scheduled_queue_call_ret_commit_groups
        state.visible_queue_call_ret_commit_group = self._visible_queue_call_ret_commit_group
        state.commit_min_delay = int(self.commit_min_delay)
        state.commit_max_delay = int(self.commit_max_delay)
        state.rng = self._rng
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
        self._cfvec_queue = state.cfvec_queue
        self._commit_queue = state.commit_queue
        self._pending_redirect_origin_index = state.pending_redirect_origin_index
        self._pending_queue_resolve_indices = state.pending_queue_resolve_indices
        self._pending_queue_call_ret_commit_indices = state.pending_queue_call_ret_commit_indices
        self._scheduled_queue_call_ret_commit_groups = state.scheduled_queue_call_ret_commit_groups
        self._visible_queue_call_ret_commit_group = state.visible_queue_call_ret_commit_group
        self.commit_min_delay = int(state.commit_min_delay)
        self.commit_max_delay = int(state.commit_max_delay)

    @staticmethod
    def _clamp_backend_delay(delay_cycles: int) -> int:
        return max(_MIN_BACKEND_DELAY, int(delay_cycles))

    def _sample_redirect_delay(self) -> int:
        min_delay = self._clamp_backend_delay(self.redirect_min_delay)
        max_delay = self._clamp_backend_delay(self.redirect_max_delay)
        return self._rng.randint(min_delay, max_delay)

    def _increment_ftq_ptr(self, flag: int, value: int) -> tuple[int, int]:
        return self._sync_backend_state().increment_ftq_ptr(flag, value)

    def _decrement_ftq_ptr(self, flag: int, value: int) -> tuple[int, int]:
        return self._sync_backend_state().decrement_ftq_ptr(flag, value)

    @staticmethod
    def _ftq_entry_matches(entry: FtqEntry, flag: int, value: int) -> bool:
        return BackendState.ftq_entry_matches(entry, flag, value)

    def _has_seen_ftq_ptr(self, flag: int, value: int) -> bool:
        return self._sync_backend_state().has_seen_ftq_ptr(flag, value)

    def _has_dispatch_complete_ftq_entry(self, flag: int, value: int) -> bool:
        state = self._sync_backend_state()
        if (
            state.current_ftq_entry is not None
            and state.ftq_entry_matches(state.current_ftq_entry, flag, value)
            and bool(state.current_ftq_entry.dispatch_complete)
        ):
            return True
        return any(
            state.ftq_entry_matches(entry, flag, value) and bool(entry.dispatch_complete)
            for entry in state.ftq_entries
        )

    def _ftq_ptr_is_stale_relative_to_commit(self, flag: int, value: int) -> bool:
        state = self._sync_backend_state()
        if (
            int(state.commit_count) <= 0
            and int(state.commit_ptr_flag) == 0
            and int(state.commit_ptr_value) == 0
            and not bool(state.reuse_commit_ptr_once)
        ):
            return False
        rank = int(state.ftq_ptr_rank_after_commit(int(flag), int(value)))
        return rank == 0 or rank > int(state.ftq_size)

    def _cfvec_queue_mode_active(self) -> bool:
        return self.golden_trace is not None and bool(self._cfvec_queue)

    def _semantic_fallback_commit_blocked(self) -> bool:
        if self.golden_trace is None or self._cfvec_queue:
            return False
        # When semantic recovery/wait is in progress, keep commit gating on the
        # semantic-queue path semantics instead of falling back to FTQ-only rules.
        return (
            self._semantic_recovery_target_pc is not None
            or self._golden_wait_pc is not None
            or self._pending_redirect_origin_index is not None
        )

    def _clear_stale_wait_states(self) -> None:
        if self._pending_level0_target_ftq is not None:
            wait_flag = int(self._pending_level0_target_ftq[0])
            wait_value = int(self._pending_level0_target_ftq[1])
            wait_rank = self._ftq_ptr_rank_after_commit(wait_flag, wait_value)
            wait_present = bool(
                self._cfvec_queue_has_ftq(wait_flag, wait_value)
                or (
                    self._current_ftq_entry is not None
                    and self._ftq_entry_matches(self._current_ftq_entry, wait_flag, wait_value)
                )
                or any(self._ftq_entry_matches(entry, wait_flag, wait_value) for entry in self.ftq_entries)
            )
            if int(wait_rank) >= int(self.ftq_size) and not wait_present:
                self._pending_level0_target_ftq = None

        if self._semantic_recovery_target_pc is not None:
            has_pending_redirect = any(evt.kind == "redirect" for evt in self.pending_events)
            has_wrong_path = any(entry.path_state == PATH_STATE_WRONG for entry in self._cfvec_queue)
            if not has_pending_redirect and self._pending_redirect_origin_index is None and not has_wrong_path:
                self._semantic_recovery_target_pc = None
                self._semantic_recovery_queue_start = None

    def _cfvec_queue_has_ftq(self, ftq_flag: int, ftq_value: int) -> bool:
        return any(
            int(entry.ftq_flag) == int(ftq_flag) and int(entry.ftq_value) == int(ftq_value)
            for entry in self._cfvec_queue
        )

    def _commit_queue_append(self, queue_index: int) -> None:
        idx = int(queue_index)
        if idx < 0 or idx >= len(self._cfvec_queue):
            return
        if any(int(item) == idx for item in self._commit_queue):
            return
        self._commit_queue.append(idx)

    def _commit_queue_remove(self, queue_index: int) -> None:
        idx = int(queue_index)
        if not self._commit_queue:
            return
        self._commit_queue = deque(item for item in self._commit_queue if int(item) != idx)

    def _ensure_commit_queue_consistency(self) -> None:
        if not self._cfvec_queue:
            self._commit_queue.clear()
            return
        if self._commit_queue:
            filtered = deque(
                int(idx)
                for idx in self._commit_queue
                if 0 <= int(idx) < len(self._cfvec_queue)
                and self._cfvec_queue[int(idx)].path_state == PATH_STATE_CORRECT
            )
            if filtered:
                self._commit_queue = filtered
                return
        self._commit_queue = deque(
            idx
            for idx, entry in enumerate(self._cfvec_queue)
            if entry.path_state == PATH_STATE_CORRECT
        )

    def _commit_queue_head_ftq_span(self) -> Optional[tuple[tuple[int, int], int]]:
        if not self._commit_queue:
            return None
        head_index = int(self._commit_queue[0])
        if head_index < 0 or head_index >= len(self._cfvec_queue):
            return None
        head_entry = self._cfvec_queue[head_index]
        key = (int(head_entry.ftq_flag), int(head_entry.ftq_value))
        span_len = 0
        saw_last = False
        for idx in self._commit_queue:
            queue_index = int(idx)
            if queue_index < 0 or queue_index >= len(self._cfvec_queue):
                break
            entry = self._cfvec_queue[queue_index]
            if (int(entry.ftq_flag), int(entry.ftq_value)) != key:
                break
            span_len += 1
            if bool(entry.is_last_in_entry):
                saw_last = True
                break
        if span_len <= 0 or not saw_last:
            return None
        return key, span_len

    def _semantic_commit_can_skip_missing_ftq(self, ftq_flag: int, ftq_value: int) -> bool:
        if self._cfvec_queue_has_ftq(int(ftq_flag), int(ftq_value)):
            return False
        if (
            self._current_ftq_entry is not None
            and self._ftq_entry_matches(self._current_ftq_entry, int(ftq_flag), int(ftq_value))
        ):
            return False
        if any(self._ftq_entry_matches(entry, int(ftq_flag), int(ftq_value)) for entry in self.ftq_entries):
            return False
        if self._has_pending_redirect_for_ftq(int(ftq_flag), int(ftq_value)):
            return False
        return True

    def _append_cfvec_queue_instr(
        self,
        *,
        slot: int,
        pc: int,
        instr: int,
        is_rvc: bool,
        pred_taken: bool,
        ftq_flag: int,
        ftq_value: int,
        ftq_offset: int,
        is_last: bool,
    ) -> int:
        cfi = self._classify_cfi(int(instr), int(pc), bool(pred_taken), bool(is_rvc))
        queue_instr = QueueInstr(
            cycle=int(self.current_cycle),
            slot=int(slot),
            pc=int(pc),
            instr=int(instr),
            is_rvc=bool(is_rvc),
            pred_taken=bool(pred_taken),
            ftq_flag=int(ftq_flag),
            ftq_value=int(ftq_value),
            ftq_offset=int(ftq_offset),
            is_last_in_entry=bool(is_last),
            resolve_state=RESOLVE_STATE_PENDING if cfi is not None else RESOLVE_STATE_NOT_NEEDED,
            is_cfi=bool(cfi is not None),
        )
        if self.golden_trace is None:
            queue_instr.path_state = PATH_STATE_CORRECT
        state = self._sync_backend_state()
        queue_index = state.append_cfvec_queue_instruction(queue_instr)
        if queue_instr.path_state == PATH_STATE_CORRECT:
            state.commit_queue.append(int(queue_index))
        if queue_instr.is_cfi:
            state.pending_queue_resolve_indices.append(int(queue_index))
        self._apply_backend_state()
        return int(queue_index)

    def _cfvec_queue_mark_matched(self, queue_index: int, entry: TraceEntry) -> None:
        if queue_index < 0 or queue_index >= len(self._cfvec_queue):
            return
        queue_entry = self._cfvec_queue[queue_index]
        queue_entry.path_state = PATH_STATE_CORRECT
        queue_entry.golden_match_state = GOLDEN_MATCH_STATE_MATCHED
        queue_entry.golden_index = int(entry.index)
        self._commit_queue_append(int(queue_index))

    def _find_preceding_correct_path_cfi(self, queue_index: int) -> Optional[tuple[int, QueueInstr]]:
        if queue_index <= 0:
            return None
        for idx in range(int(queue_index) - 1, -1, -1):
            entry = self._cfvec_queue[idx]
            if entry.path_state != PATH_STATE_CORRECT:
                continue
            if entry.is_cfi:
                return int(idx), entry
        return None

    def _cfvec_queue_note_mismatch(self, queue_index: int) -> None:
        if queue_index < 0 or queue_index >= len(self._cfvec_queue):
            return
        queue_entry = self._cfvec_queue[queue_index]
        queue_entry.golden_match_state = GOLDEN_MATCH_STATE_MISMATCHED
        if self._pending_redirect_origin_index is None:
            self._pending_redirect_origin_index = int(queue_index)
            queue_entry.path_state = PATH_STATE_WRONG
            self._commit_queue_remove(int(queue_index))
            target_pc = self.current_golden_pc()
            redirect_queue_index: Optional[int] = None
            redirect_entry: Optional[QueueInstr] = None
            prefer_direct_redirect = False
            if bool(queue_entry.is_cfi):
                redirect_queue_index = int(queue_index)
                redirect_entry = queue_entry
                # First mismatch is already a CFI on the wrong path; queue a redirect
                # immediately because this CFI resolve will be skipped by wrong-path gating.
                prefer_direct_redirect = True
            else:
                prev_cfi = self._find_preceding_correct_path_cfi(int(queue_index))
                if prev_cfi is not None:
                    redirect_queue_index, redirect_entry = prev_cfi
            if redirect_entry is not None and target_pc is not None:
                cfi = self._classify_cfi(
                    int(redirect_entry.instr),
                    int(redirect_entry.pc),
                    bool(redirect_entry.pred_taken),
                    bool(redirect_entry.is_rvc),
                )
                if cfi is not None:
                    branch_type, ras_action, _pred_target, _pred_taken = cfi
                    redirect_taken = int(
                        int(target_pc) != int(self._sequential_next_pc(int(redirect_entry.pc), bool(redirect_entry.is_rvc)))
                    )
                    redirected_by_pending_resolve = False
                    for resolve_entry in self._pending_resolves:
                        if resolve_entry.queue_index != int(redirect_queue_index):
                            continue
                        resolve_entry.mispredict = True
                        resolve_entry.target = int(target_pc)
                        resolve_entry.taken = bool(redirect_taken)
                        redirected_by_pending_resolve = True
                        break
                    if not redirected_by_pending_resolve or prefer_direct_redirect:
                        self._queue_redirect_event(
                            target_pc=int(target_pc),
                            reason="golden_first_mismatch_redirect",
                            flush_on_drive=True,
                            payload_extra={
                                "pc": int(redirect_entry.pc),
                                "taken": int(redirect_taken),
                                "ftq_flag": int(redirect_entry.ftq_flag),
                                "ftq_value": int(redirect_entry.ftq_value),
                                "ftq_offset": int(redirect_entry.ftq_offset),
                                "branch_type": int(branch_type),
                                "ras_action": int(ras_action),
                                "is_rvc": int(redirect_entry.is_rvc),
                                "level": 0,
                            },
                        )
            return
        if self._pending_redirect_origin_index is not None and int(queue_index) >= int(self._pending_redirect_origin_index):
            queue_entry.path_state = PATH_STATE_WRONG
            self._commit_queue_remove(int(queue_index))

    def _cfvec_queue_mark_resolve_state(self, queue_index: Optional[int], resolve_state: str) -> None:
        if queue_index is None:
            return
        if queue_index < 0 or queue_index >= len(self._cfvec_queue):
            return
        self._cfvec_queue[queue_index].resolve_state = str(resolve_state)
        self._pending_queue_resolve_indices = deque(
            idx for idx in self._pending_queue_resolve_indices if int(idx) != int(queue_index)
        )

    def _cfvec_queue_mark_committed(self, queue_index: int, instr: int) -> None:
        if queue_index < 0 or queue_index >= len(self._cfvec_queue):
            return
        queue_entry = self._cfvec_queue[queue_index]
        queue_entry.rob_commit_state = ROB_COMMIT_STATE_COMMITTED
        queue_entry.call_ret_ras_action = self._sync_backend_state().decode_commit_ras_action(int(instr))
        queue_entry.call_ret_commit_state = CALL_RET_STATE_PENDING
        self._pending_queue_call_ret_commit_indices.append(int(queue_index))

    def _cfvec_queue_mark_recovery_residual(self, queue_index: int) -> None:
        if queue_index < 0 or queue_index >= len(self._cfvec_queue):
            return
        queue_entry = self._cfvec_queue[queue_index]
        queue_entry.path_state = PATH_STATE_WRONG
        queue_entry.golden_match_state = GOLDEN_MATCH_STATE_MISMATCHED
        self._commit_queue_remove(int(queue_index))
        if queue_entry.is_cfi:
            self._cfvec_queue_mark_resolve_state(int(queue_index), RESOLVE_STATE_SKIPPED)

    def _cfvec_queue_recompute_redirect_origin(self) -> None:
        for idx, entry in enumerate(self._cfvec_queue):
            if entry.path_state == PATH_STATE_WRONG:
                self._pending_redirect_origin_index = int(idx)
                return
        self._pending_redirect_origin_index = None

    def _cfvec_queue_flush_wrong_path(self) -> None:
        state = self._sync_backend_state()
        origin_index = state.pending_redirect_origin_index
        if origin_index is None:
            for idx, entry in enumerate(state.cfvec_queue):
                if entry.path_state == PATH_STATE_WRONG:
                    origin_index = int(idx)
                    break
        if origin_index is None:
            return
        keep_count = max(0, int(origin_index))
        kept = list(state.cfvec_queue)[:keep_count]
        state.cfvec_queue = deque(kept)
        state.pending_resolves = deque(
            entry
            for entry in state.pending_resolves
            if entry.queue_index is None or int(entry.queue_index) < keep_count
        )
        state.pending_queue_resolve_indices = deque(
            idx for idx in state.pending_queue_resolve_indices if int(idx) < keep_count
        )
        state.pending_queue_call_ret_commit_indices = deque(
            idx for idx in state.pending_queue_call_ret_commit_indices if int(idx) < keep_count
        )
        state.commit_queue = deque(
            idx for idx in state.commit_queue if int(idx) < keep_count
        )
        state.scheduled_queue_call_ret_commit_groups = deque(
            (int(ready_cycle), kept_group)
            for ready_cycle, group in state.scheduled_queue_call_ret_commit_groups
            if (
                kept_group := [
                    inst for inst in group if getattr(inst, "queue_index", None) is None or int(getattr(inst, "queue_index")) < keep_count
                ]
            )
        )
        state.visible_queue_call_ret_commit_group = [
            inst
            for inst in state.visible_queue_call_ret_commit_group
            if getattr(inst, "queue_index", None) is None or int(getattr(inst, "queue_index")) < keep_count
        ]
        state.pending_redirect_origin_index = None
        self._apply_backend_state()

    def _cfvec_queue_pop_head(self, count: int) -> None:
        pop_count = max(0, int(count))
        if pop_count <= 0:
            return
        for _ in range(pop_count):
            if not self._cfvec_queue:
                break
            self._cfvec_queue.popleft()
        self._pending_resolves = deque(
            replace(entry, queue_index=(None if entry.queue_index is None else int(entry.queue_index) - pop_count))
            for entry in self._pending_resolves
            if entry.queue_index is None or int(entry.queue_index) >= pop_count
        )
        if self._pending_redirect_origin_index is not None and int(self._pending_redirect_origin_index) >= pop_count:
            self._pending_redirect_origin_index = int(self._pending_redirect_origin_index) - pop_count
        else:
            self._pending_redirect_origin_index = None
        self._cfvec_queue_recompute_redirect_origin()
        self._pending_queue_resolve_indices = deque(
            int(idx) - pop_count
            for idx in self._pending_queue_resolve_indices
            if int(idx) >= pop_count
        )
        self._pending_queue_call_ret_commit_indices = deque(
            int(idx) - pop_count
            for idx in self._pending_queue_call_ret_commit_indices
            if int(idx) >= pop_count
        )
        self._commit_queue = deque(
            int(idx) - pop_count
            for idx in self._commit_queue
            if int(idx) >= pop_count
        )

    def _cfvec_queue_remove_range(self, start: int, stop: int) -> None:
        remove_start = max(0, int(start))
        remove_stop = max(remove_start, int(stop))
        if remove_start >= remove_stop:
            return
        kept_queue: list[QueueInstr] = []
        queue_index_map: dict[int, int] = {}
        for old_idx, entry in enumerate(self._cfvec_queue):
            if remove_start <= int(old_idx) < remove_stop:
                continue
            queue_index_map[int(old_idx)] = len(kept_queue)
            kept_queue.append(entry)
        self._cfvec_queue = deque(kept_queue)
        if self._pending_redirect_origin_index is not None:
            self._pending_redirect_origin_index = queue_index_map.get(int(self._pending_redirect_origin_index))
        self._pending_resolves = deque(
            replace(entry, queue_index=queue_index_map.get(int(entry.queue_index)))
            for entry in self._pending_resolves
            if entry.queue_index is None or int(entry.queue_index) in queue_index_map
        )
        self._pending_queue_resolve_indices = deque(
            int(queue_index_map[int(idx)])
            for idx in self._pending_queue_resolve_indices
            if int(idx) in queue_index_map
        )
        self._pending_queue_call_ret_commit_indices = deque(
            int(queue_index_map[int(idx)])
            for idx in self._pending_queue_call_ret_commit_indices
            if int(idx) in queue_index_map
        )
        self._commit_queue = deque(
            int(queue_index_map[int(idx)])
            for idx in self._commit_queue
            if int(idx) in queue_index_map
        )
        self._scheduled_queue_call_ret_commit_groups = deque(
            (int(ready_cycle), kept_group)
            for ready_cycle, group in self._scheduled_queue_call_ret_commit_groups
            if (
                kept_group := [
                    replace(inst, queue_index=queue_index_map.get(int(inst.queue_index)))
                    for inst in group
                    if getattr(inst, "queue_index", None) is None or int(inst.queue_index) in queue_index_map
                ]
            )
        )
        self._visible_queue_call_ret_commit_group = [
            replace(inst, queue_index=queue_index_map.get(int(inst.queue_index)))
            for inst in self._visible_queue_call_ret_commit_group
            if getattr(inst, "queue_index", None) is None or int(inst.queue_index) in queue_index_map
        ]
        self._cfvec_queue_recompute_redirect_origin()

    def _prune_ftq_entries_to_cfvec_queue(self) -> None:
        active_ftqs = {
            (int(entry.ftq_flag), int(entry.ftq_value))
            for entry in self._cfvec_queue
        }
        self.ftq_entries = deque(
            entry
            for entry in self.ftq_entries
            if (int(entry.ftq_flag), int(entry.ftq_value)) in active_ftqs
        )
        if self._current_ftq_entry is not None and (
            int(self._current_ftq_entry.ftq_flag),
            int(self._current_ftq_entry.ftq_value),
        ) not in active_ftqs:
            self._current_ftq_entry = None
            self._current_ftq_seen_packets.clear()
            self._current_ftq_max_offset = -1
            self._current_ftq_contains_wait_pc = False

    def _cfvec_queue_close_current_ftq_span(self, ftq_flag: int, ftq_value: int) -> None:
        for entry in reversed(self._cfvec_queue):
            if int(entry.ftq_flag) != int(ftq_flag) or int(entry.ftq_value) != int(ftq_value):
                continue
            entry.is_last_in_entry = True
            return

    def _clear_cfvec_queue_state(self) -> None:
        self._cfvec_queue.clear()
        self._commit_queue.clear()
        self._pending_redirect_origin_index = None
        self._pending_queue_resolve_indices.clear()
        self._pending_queue_call_ret_commit_indices.clear()
        self._scheduled_queue_call_ret_commit_groups.clear()
        self._visible_queue_call_ret_commit_group = []
        self._semantic_recovery_target_pc = None
        self._semantic_recovery_queue_start = None

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
        if self._cfvec_queue_mode_active():
            return None
        candidate = self._sync_backend_state().find_next_commitable_entry(
            golden_trace_attached=self.golden_trace is not None,
        )
        self._apply_backend_state()
        return candidate

    def _seal_current_ftq_entry(self, *, observed_last_in_entry: bool = False) -> None:
        self._sync_backend_state().seal_current_ftq_entry(
            observed_last_in_entry=bool(observed_last_in_entry)
        )
        self._apply_backend_state()
        self._current_ftq_max_offset = -1
        self._current_ftq_contains_wait_pc = False

    def bind(self, dut) -> None:
        self.dut = dut
        self.bind_interfaces(
            drive_if=bind_bundle_optional(BackendCtrlBundle, dut),
            observe_if=bind_bundle_optional(BackendObserveBundle, dut),
            from_ftq_if=bind_bundle_optional(BackendFromFtqBundle, dut),
            frontend_info_if=bind_bundle_optional(FrontendInfoBundle, dut),
        )

    @staticmethod
    def _read(signal, default: int = 0) -> int:
        try:
            value = getattr(signal, "value", None)
            return default if value is None else int(value)
        except Exception:
            return default

    def bind_interfaces(self, *, drive_if, observe_if, from_ftq_if, frontend_info_if) -> None:
        self.drive_if = drive_if
        self.observe_if = observe_if
        self.from_ftq_if = from_ftq_if
        self.frontend_info_if = frontend_info_if

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

    def begin_cycle(self, cycle: int) -> None:
        self.current_cycle = int(cycle)
        self._cycle_start_golden_cursor = None if self.golden_trace is None else int(self.golden_trace.cursor)
        self._cycle_start_golden_pc = self.current_golden_pc()

    def consume_backend_observation(self, observation: BackendObservationSnapshot) -> None:
        self._last_observation = observation

    def current_frontend_observation(self) -> BackendObservationSnapshot:
        return self._last_observation

    def _snapshot_bound_observation(self) -> BackendObservationSnapshot:
        return BackendObservationSnapshot(
            from_ftq_wen=self._read(getattr(self.from_ftq_if, "io_backend_fromFtq_wen", None)),
            from_ftq_ftq_idx=self._read(getattr(self.from_ftq_if, "io_backend_fromFtq_ftqIdx", None)),
            from_ftq_start_pc_addr=self._read(getattr(self.from_ftq_if, "io_backend_fromFtq_startPc_addr", None)),
            ibuf_full=self._read(getattr(self.frontend_info_if, "io_frontendInfo_ibufFull", None)),
        )

    def _bound_backend_agent(self) -> BackendAgent:
        assert self.drive_if is not None
        agent = BackendAgent()
        agent.bind(self.drive_if)
        return agent

    def set_golden_trace(self, trace: Optional[GoldenTrace]) -> None:
        self.golden_trace = trace
        self._golden_wait_pc = None
        self._semantic_recovery_target_pc = None
        self._semantic_recovery_queue_start = None
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
            if evt.kind == "exception":
                return True
        return False

    def _has_pending_redirect_for_ftq(self, ftq_flag: int, ftq_value: int) -> bool:
        for evt in self.pending_events:
            if evt.kind != "redirect":
                continue
            payload = evt.payload if isinstance(evt.payload, dict) else {}
            if "ftq_flag" not in payload or "ftq_value" not in payload:
                continue
            if (
                int(payload.get("ftq_flag", -1)) == int(ftq_flag)
                and int(payload.get("ftq_value", -1)) == int(ftq_value)
            ):
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

    def _current_golden_cfi_successor_observed_after_issue(self, inst_pc: int, start_cycle: int) -> bool:
        if self.monitor is None or self.golden_trace is None:
            return False
        trace = self.golden_trace
        start = int(trace.cursor)
        if start + 1 >= len(trace.entries):
            return False
        cur = trace.entries[start]
        nxt = trace.entries[start + 1]
        if int(getattr(cur, "target_pc", 0) or 0) == 0:
            return False

        armed = False
        saw_cur = False
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
            if not saw_cur:
                if int(obs.pc) == int(cur.pc):
                    saw_cur = True
                continue
            if int(obs.pc) == int(nxt.pc):
                return True
        return False

    def _queue_redirect_event(
        self,
        target_pc: int,
        reason: str,
        delay_cycles: Optional[int] = None,
        flush_on_drive: bool = False,
        payload_extra: Optional[Dict] = None,
    ) -> None:
        effective_delay = self._sample_redirect_delay() if delay_cycles is None else self._clamp_backend_delay(delay_cycles)
        ready_cycle = self.current_cycle + effective_delay
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

    def _record_instruction_commit(self, queue_index: Optional[int], instr: int) -> None:
        if queue_index is not None:
            self._cfvec_queue_mark_committed(int(queue_index), int(instr))
        self._apply_backend_state()

    def _plan_instruction_commits_for_cycle(self) -> int:
        self._ensure_commit_queue_consistency()
        if not self._commit_queue:
            return 0
        committed = 0
        width = int(self.instruction_commit_width)
        queue_snapshot = [int(idx) for idx in self._commit_queue]
        for commit_pos, queue_index in enumerate(queue_snapshot):
            if committed >= width:
                break
            if queue_index < 0 or queue_index >= len(self._cfvec_queue):
                continue
            queue_entry = self._cfvec_queue[queue_index]
            if queue_entry.rob_commit_state != ROB_COMMIT_STATE_PENDING:
                continue
            if commit_pos > 0:
                prev_idx = int(queue_snapshot[commit_pos - 1])
                if prev_idx < 0 or prev_idx >= len(self._cfvec_queue):
                    break
                if self._cfvec_queue[prev_idx].rob_commit_state != ROB_COMMIT_STATE_COMMITTED:
                    break
            if queue_entry.path_state != PATH_STATE_CORRECT:
                break
            if int(queue_entry.cycle) >= int(self.current_cycle):
                break
            if queue_entry.is_cfi and queue_entry.resolve_state != RESOLVE_STATE_EMITTED:
                break
            self._record_instruction_commit(int(queue_index), int(queue_entry.instr))
            committed += 1
        return int(committed)

    def _schedule_next_queue_call_ret_commit_group(self) -> None:
        self._sync_backend_state().schedule_next_queue_call_ret_commit_group()
        self._apply_backend_state()

    def _activate_visible_queue_call_ret_commit_group(self) -> None:
        self._sync_backend_state().activate_visible_queue_call_ret_commit_group()
        self._apply_backend_state()

    def _current_semantic_call_ret_commit_group(self) -> tuple[CommitInstruction, ...]:
        return tuple(self._visible_queue_call_ret_commit_group)

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

    def _update_ftq_start_pc_cache(self, observation: BackendObservationSnapshot) -> None:
        """Cache per-FTQ-entry start PCs reported by DUT's fromFtq interface.

        When io_backend_fromFtq_wen is high the DUT writes the canonical startPc
        for the given ftqIdx.  We index the cache by the packed FtqPtr integer
        (flag<<6 | value[5:0]) so that _sample_cfvec can look it up by the same
        key reconstructed from cfVec's ftqPtr_flag / ftqPtr_value fields.
        """
        if int(observation.from_ftq_wen):
            ftq_idx = int(observation.from_ftq_ftq_idx)
            start_pc = self._decode_backend_addr(int(observation.from_ftq_start_pc_addr))
            self._ftq_start_pc_cache[ftq_idx] = start_pc
            self._ftq_start_pc_by_value[ftq_idx & 0x3F] = start_pc

    def _has_later_cfvec_slot_matching_pc(self, current_slot: int, target_pc: int) -> bool:
        assert self.observe_if is not None
        for slot in range(int(current_slot) + 1, 8):
            if self._read(self.observe_if.cfvec_valid[slot], 0) != 1:
                continue
            if int(self._read(self.observe_if.cfvec_pc[slot], 0)) == int(target_pc):
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

    def _should_reset_reused_ftq_slot_state(self, ftq_flag: int, ftq_value: int, pc: int) -> bool:
        group = (int(ftq_flag), int(ftq_value))
        history = self._ftq_group_pc_history.get(group)
        if not history:
            return False
        if self._current_ftq_entry is not None and self._ftq_entry_matches(
            self._current_ftq_entry,
            ftq_flag,
            ftq_value,
        ):
            return False
        if any(self._ftq_entry_matches(entry, ftq_flag, ftq_value) for entry in self.ftq_entries):
            return False
        return not self._cfvec_queue_has_ftq(ftq_flag, ftq_value)

    def _reset_ftq_slot_reuse_state(self, ftq_flag: int, ftq_value: int) -> None:
        group = (int(ftq_flag), int(ftq_value))
        history = self._ftq_group_pc_history.pop(group, [])
        for pc, _is_rvc in history:
            occurrences = self._pc_group_occurrences.get(int(pc))
            if occurrences:
                remaining = [
                    occurrence
                    for occurrence in occurrences
                    if (int(occurrence[0]), int(occurrence[1])) != group
                ]
                if remaining:
                    self._pc_group_occurrences[int(pc)] = remaining
                else:
                    self._pc_group_occurrences.pop(int(pc), None)

    def _simfrontend_redirect_drive_override(self, payload: dict) -> Optional[dict]:
        if int(payload.get("branch_type", 0)) == 3:
            return None
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
        assert self.observe_if is not None

        def _waiting_target_group() -> Optional[tuple[int, int]]:
            if self._pending_level0_target_ftq is not None:
                return (int(self._pending_level0_target_ftq[0]), int(self._pending_level0_target_ftq[1]))
            return None

        def _track_waiting_target_entry_prefix(
            pc: int,
            instr: int,
            is_rvc: bool,
            pred_taken: bool,
            ftq_flag: int,
            ftq_value: int,
            ftq_offset: int,
            is_last: bool,
        ) -> None:
            if self._current_ftq_entry is None or (
                self._current_ftq_entry.ftq_flag != ftq_flag
                or self._current_ftq_entry.ftq_value != ftq_value
            ):
                if self._current_ftq_entry is not None:
                    self._cfvec_queue_close_current_ftq_span(
                        int(self._current_ftq_entry.ftq_flag),
                        int(self._current_ftq_entry.ftq_value),
                    )
                    self._seal_current_ftq_entry()
                if self._should_reset_reused_ftq_slot_state(int(ftq_flag), int(ftq_value), int(pc)):
                    self._reset_ftq_slot_reuse_state(int(ftq_flag), int(ftq_value))
                self._current_ftq_entry = FtqEntry(ftq_flag=ftq_flag, ftq_value=ftq_value)
                self._current_ftq_seen_packets.clear()
                self._current_ftq_max_offset = -1
                self._current_ftq_contains_wait_pc = False

            packet_key = (
                int(pc),
                int(instr),
                1 if bool(is_rvc) else 0,
                int(ftq_offset),
                1 if bool(pred_taken) else 0,
            )
            if packet_key in self._current_ftq_seen_packets:
                return
            self._current_ftq_seen_packets.add(packet_key)
            queue_index = self._append_cfvec_queue_instr(
                slot=-1,
                pc=int(pc),
                instr=int(instr),
                is_rvc=bool(is_rvc),
                pred_taken=bool(pred_taken),
                ftq_flag=int(ftq_flag),
                ftq_value=int(ftq_value),
                ftq_offset=int(ftq_offset),
                is_last=bool(is_last),
            )
            # Post-redirect / wait-prefix packets are recovery residuals of an
            # existing wrong-path episode. Mark them as wrong-path but do not
            # start a new mismatch/redirect cycle.
            self._cfvec_queue_mark_recovery_residual(int(queue_index))
            self._current_ftq_max_offset = max(int(self._current_ftq_max_offset), int(ftq_offset))
            self._record_ftq_group_pc(int(ftq_flag), int(ftq_value), int(pc), bool(is_rvc))
            if is_last:
                self._cfvec_queue_close_current_ftq_span(int(ftq_flag), int(ftq_value))
                self._seal_current_ftq_entry(observed_last_in_entry=True)

        for i in range(8):
            if self._read(self.observe_if.cfvec_valid[i], 0) != 1:
                continue
            pc = self._read(self.observe_if.cfvec_pc[i], 0)
            instr = self._read(self.observe_if.cfvec_instr[i], 0)
            is_rvc = bool(self._read(self.observe_if.cfvec_is_rvc[i], 0))
            pred_taken = bool(self._read(self.observe_if.cfvec_pred_taken[i], 0))
            ftq_flag = self._read(self.observe_if.cfvec_ftq_ptr_flag[i], 0)
            ftq_value = self._read(self.observe_if.cfvec_ftq_ptr_value[i], 0)
            ftq_offset = self._read(self.observe_if.cfvec_ftq_offset[i], 0)
            is_last = bool(self._read(self.observe_if.cfvec_is_last_in_ftq_entry[i], 0))
            active_ftq_ptr = self._cfvec_queue_has_ftq(int(ftq_flag), int(ftq_value))
            stale_ftq_ptr = self._ftq_ptr_is_stale_relative_to_commit(int(ftq_flag), int(ftq_value))

            if self._semantic_recovery_target_pc is not None:
                recovery_target_pc = int(self._semantic_recovery_target_pc)
                recovery_frontier_pc = self.current_golden_pc()
                if int(pc) != int(recovery_target_pc):
                    if stale_ftq_ptr and not active_ftq_ptr:
                        continue
                    _track_waiting_target_entry_prefix(
                        int(pc),
                        int(instr),
                        bool(is_rvc),
                        bool(pred_taken),
                        int(ftq_flag),
                        int(ftq_value),
                        int(ftq_offset),
                        bool(is_last),
                    )
                    continue
                if stale_ftq_ptr and not active_ftq_ptr:
                    continue
                if (
                    self._semantic_recovery_queue_start is not None
                    and len(self._cfvec_queue) > int(self._semantic_recovery_queue_start)
                ):
                    self._cfvec_queue_remove_range(
                        int(self._semantic_recovery_queue_start),
                        len(self._cfvec_queue),
                    )
                    self._prune_ftq_entries_to_cfvec_queue()
                    self._current_ftq_entry = None
                    self._current_ftq_seen_packets.clear()
                    self._current_ftq_max_offset = -1
                    self._current_ftq_contains_wait_pc = False
                self._semantic_recovery_target_pc = None
                self._semantic_recovery_queue_start = None
                if (
                    recovery_frontier_pc is not None
                    and int(recovery_frontier_pc) != int(recovery_target_pc)
                ):
                    continue

            matched_wait_pc = self._golden_wait_pc is not None and int(pc) == int(self._golden_wait_pc)

            if self._golden_wait_pc is not None:
                if int(pc) != int(self._golden_wait_pc):
                    waiting_target_group = _waiting_target_group()
                    if stale_ftq_ptr and not active_ftq_ptr:
                        continue
                    if waiting_target_group is not None and (int(ftq_flag), int(ftq_value)) != waiting_target_group:
                        _track_waiting_target_entry_prefix(
                            int(pc),
                            int(instr),
                            bool(is_rvc),
                            bool(pred_taken),
                            int(ftq_flag),
                            int(ftq_value),
                            int(ftq_offset),
                            bool(is_last),
                        )
                        continue
                    _track_waiting_target_entry_prefix(
                        int(pc),
                        int(instr),
                        bool(is_rvc),
                        bool(pred_taken),
                        int(ftq_flag),
                        int(ftq_value),
                        int(ftq_offset),
                        bool(is_last),
                    )
                    continue
                else:
                    if stale_ftq_ptr and not active_ftq_ptr:
                        continue
                    self._golden_wait_pc = None

            if stale_ftq_ptr and not active_ftq_ptr:
                continue

            expected_golden = self.current_golden_pc()
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
                    self._cfvec_queue_close_current_ftq_span(
                        int(self._current_ftq_entry.ftq_flag),
                        int(self._current_ftq_entry.ftq_value),
                    )
                    self._seal_current_ftq_entry()
                if self._should_reset_reused_ftq_slot_state(int(ftq_flag), int(ftq_value), int(pc)):
                    self._reset_ftq_slot_reuse_state(int(ftq_flag), int(ftq_value))
                self._current_ftq_entry = FtqEntry(ftq_flag=ftq_flag, ftq_value=ftq_value)
                self._current_ftq_seen_packets.clear()
                self._current_ftq_max_offset = -1
                self._current_ftq_contains_wait_pc = False

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
            queue_index = self._append_cfvec_queue_instr(
                slot=int(i),
                pc=int(pc),
                instr=int(instr),
                is_rvc=bool(is_rvc),
                pred_taken=bool(pred_taken),
                ftq_flag=int(ftq_flag),
                ftq_value=int(ftq_value),
                ftq_offset=int(ftq_offset),
                is_last=bool(is_last),
            )
            if golden_entry is not None:
                self._cfvec_queue_mark_matched(int(queue_index), golden_entry)
            elif expected_golden is not None:
                self._cfvec_queue_note_mismatch(int(queue_index))
            self._current_ftq_max_offset = max(int(self._current_ftq_max_offset), int(ftq_offset))
            if matched_wait_pc:
                self._current_ftq_contains_wait_pc = True
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
                delay = self._rng.randint(self.resolve_min_delay, self.resolve_max_delay)
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
                    queue_index=int(queue_index),
                ))
                self._current_ftq_entry.total_cfi += 1

            if is_last:
                closes_waiting_target = (
                    self._pending_level0_target_ftq == (int(ftq_flag), int(ftq_value))
                    and self._current_ftq_contains_wait_pc
                )
                self._seal_current_ftq_entry(observed_last_in_entry=True)
                if closes_waiting_target:
                    self._pending_level0_target_ftq = None

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
                self._cfvec_queue_close_current_ftq_span(
                    int(self._current_ftq_entry.ftq_flag),
                    int(self._current_ftq_entry.ftq_value),
                )
                self._seal_current_ftq_entry()
            break

    def _ready_resolves_for_cycle(self) -> tuple[ResolveEntry, ...]:
        ready_entries: list[ResolveEntry] = []
        to_remove = []
        frontier_pc = self.current_golden_pc()
        if frontier_pc is None and self._golden_wait_pc is not None:
            frontier_pc = int(self._golden_wait_pc)
        for entry in self._pending_resolves:
            if len(ready_entries) >= 3:
                break
            if entry.ready_cycle > self.current_cycle:
                continue
            if entry.queue_index is not None:
                if not (0 <= int(entry.queue_index) < len(self._cfvec_queue)):
                    to_remove.append(entry)
                    continue
                queue_entry = self._cfvec_queue[int(entry.queue_index)]
                if queue_entry.path_state == PATH_STATE_WRONG:
                    self._cfvec_queue_mark_resolve_state(entry.queue_index, RESOLVE_STATE_SKIPPED)
                    to_remove.append(entry)
                    continue
                if queue_entry.path_state != PATH_STATE_CORRECT:
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
            elif (
                effective_mispredict
                and int(entry.branch_type) == 3
                and self.current_golden_pc() is not None
                and int(entry.target) != int(self.current_golden_pc())
                and self._current_golden_cfi_successor_observed_after_issue(
                    int(entry.inst_pc),
                    int(entry.queued_cycle),
                )
            ):
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
            ready_entries.append(replace(entry, mispredict=bool(effective_mispredict)))
            redirect_level = 0
            if self.auto_redirect_on_golden_mispredict and effective_mispredict:
                self._queue_redirect_event(
                    target_pc=int(entry.target),
                    reason="golden_resolve_redirect",
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
            self._cfvec_queue_mark_resolve_state(entry.queue_index, RESOLVE_STATE_EMITTED)
            to_remove.append(entry)
        for entry in to_remove:
            self._pending_resolves.remove(entry)
        return tuple(ready_entries)

    def _drive_resolves(self) -> None:
        self._bound_backend_agent().drive_resolves(self._ready_resolves_for_cycle())

    def _plan_commit_entry_for_cycle(self) -> Optional[FtqEntry]:
        if self.can_accept == 0:
            return None
        if self._has_pending_control_event():
            return None
        self._ensure_commit_queue_consistency()
        if self.golden_trace is not None and self._commit_queue:
            if int(self._commit_queue[0]) != 0:
                return None
            head_span = self._commit_queue_head_ftq_span()
            if head_span is None and self._commit_queue:
                head_queue_index = int(self._commit_queue[0])
                if not (0 <= head_queue_index < len(self._cfvec_queue)):
                    return None
                head_entry = self._cfvec_queue[head_queue_index]
                head_ftq_flag = int(head_entry.ftq_flag)
                head_ftq_value = int(head_entry.ftq_value)
                current_matches_head = bool(
                    self._current_ftq_entry is not None
                    and int(self._current_ftq_entry.ftq_flag) == int(head_ftq_flag)
                    and int(self._current_ftq_entry.ftq_value) == int(head_ftq_value)
                )
                head_dispatch_complete = any(
                    int(entry.ftq_flag) == int(head_ftq_flag)
                    and int(entry.ftq_value) == int(head_ftq_value)
                    and bool(entry.dispatch_complete)
                    for entry in self.ftq_entries
                )
                if not current_matches_head and head_dispatch_complete:
                    last_head_idx = -1
                    for queue_index in self._commit_queue:
                        idx = int(queue_index)
                        if idx < 0 or idx >= len(self._cfvec_queue):
                            break
                        entry = self._cfvec_queue[idx]
                        if (
                            int(entry.ftq_flag) != int(head_ftq_flag)
                            or int(entry.ftq_value) != int(head_ftq_value)
                        ):
                            break
                        last_head_idx = int(idx)
                    if last_head_idx >= 0:
                        self._cfvec_queue[last_head_idx].is_last_in_entry = True
                    head_span = self._commit_queue_head_ftq_span()
            if head_span is None:
                return None
            (ftq_flag, ftq_value), span_len = head_span
            queue_head_indices = [int(idx) for idx in list(self._commit_queue)[:span_len]]
            if len(queue_head_indices) != int(span_len):
                return None
            if queue_head_indices != list(range(int(span_len))):
                return None
            state = self._sync_backend_state()
            have_prior_commit = bool(
                int(state.commit_count) > 0
                or int(state.commit_ptr_flag) != 0
                or int(state.commit_ptr_value) != 0
                or bool(state.reuse_commit_ptr_once)
            )
            if have_prior_commit:
                head_rank = state.ftq_ptr_rank_after_commit(int(ftq_flag), int(ftq_value))
                if not bool(state.reuse_commit_ptr_once) and (
                    int(head_rank) == 0 or int(head_rank) >= int(state.ftq_size)
                ):
                    if int(head_rank) == 0:
                        self._cfvec_queue_pop_head(int(span_len))
                        return None
                    raise AssertionError(
                        "commit queue head falls behind commit_ptr; "
                        f"head=({int(ftq_flag)},{int(ftq_value)}) "
                        f"commit_ptr=({int(state.commit_ptr_flag)},{int(state.commit_ptr_value)})"
                    )
                expected_ftq = (
                    (int(state.commit_ptr_flag), int(state.commit_ptr_value))
                    if bool(state.reuse_commit_ptr_once)
                    else state.increment_ftq_ptr(int(state.commit_ptr_flag), int(state.commit_ptr_value))
                )
            if self._has_pending_redirect_for_ftq(int(ftq_flag), int(ftq_value)):
                return None
            pending_wait_rank = None
            if self._pending_level0_target_ftq is not None:
                head_rank = self._ftq_ptr_rank_after_commit(int(ftq_flag), int(ftq_value))
                pending_wait_rank = self._ftq_ptr_rank_after_commit(
                    int(self._pending_level0_target_ftq[0]),
                    int(self._pending_level0_target_ftq[1]),
                )
                if (
                    (int(ftq_flag), int(ftq_value))
                    != (int(self._pending_level0_target_ftq[0]), int(self._pending_level0_target_ftq[1]))
                    and int(head_rank) >= int(pending_wait_rank)
                ):
                    return None
            if have_prior_commit:
                head_rank = state.ftq_ptr_rank_after_commit(int(ftq_flag), int(ftq_value))
                candidate_ftq = expected_ftq
                # Redirect-truncated holes may disappear entirely from both cfvec/commit queues
                # and FTQ bookkeeping. Those specific gaps can be skipped; any visible
                # intermediate FTQ still blocks commit to preserve in-order semantics.
                for _ in range(max(0, int(head_rank) - 1)):
                    if candidate_ftq == (int(ftq_flag), int(ftq_value)):
                        break
                    if not self._semantic_commit_can_skip_missing_ftq(
                        int(candidate_ftq[0]),
                        int(candidate_ftq[1]),
                    ):
                        return None
                    candidate_ftq = state.increment_ftq_ptr(int(candidate_ftq[0]), int(candidate_ftq[1]))
                if candidate_ftq != (int(ftq_flag), int(ftq_value)):
                    return None
            queue_head_entries = [self._cfvec_queue[idx] for idx in queue_head_indices]
            if any(entry.path_state != PATH_STATE_CORRECT for entry in queue_head_entries):
                return None
            if any(entry.rob_commit_state != ROB_COMMIT_STATE_COMMITTED for entry in queue_head_entries):
                return None
            if any(entry.is_cfi and entry.resolve_state != RESOLVE_STATE_EMITTED for entry in queue_head_entries):
                return None

            committed_entry = FtqEntry(ftq_flag=int(ftq_flag), ftq_value=int(ftq_value))
            if self.ftq_entries:
                matching_entries = [
                    entry
                    for entry in self.ftq_entries
                    if self._ftq_entry_matches(entry, int(ftq_flag), int(ftq_value))
                ]
                if not matching_entries:
                    return None
                first_match_index = next(
                    idx
                    for idx, entry in enumerate(self.ftq_entries)
                    if self._ftq_entry_matches(entry, int(ftq_flag), int(ftq_value))
                )
                for older_entry in list(self.ftq_entries)[:first_match_index]:
                    older_rank = state.ftq_ptr_rank_after_commit(
                        int(older_entry.ftq_flag),
                        int(older_entry.ftq_value),
                    )
                    if not bool(state.reuse_commit_ptr_once) and (
                        int(older_rank) == 0 or int(older_rank) >= int(state.ftq_size)
                    ):
                        continue
                    if self._cfvec_queue_has_ftq(
                        int(older_entry.ftq_flag),
                        int(older_entry.ftq_value),
                    ):
                        continue
                    if bool(older_entry.dispatch_complete) or bool(older_entry.observed_last_in_entry):
                        return None
                head_ftq_entry = matching_entries[0]
                if int(self.current_cycle) < int(head_ftq_entry.commit_ready_cycle):
                    return None
                retained_entries: Deque[FtqEntry] = deque()
                matched_head_entry = False
                while self.ftq_entries:
                    entry = self.ftq_entries.popleft()
                    if self._ftq_entry_matches(entry, int(ftq_flag), int(ftq_value)):
                        committed_entry = entry
                        matched_head_entry = True
                        continue
                    if (
                        not matched_head_entry
                        and not self._cfvec_queue_has_ftq(
                            int(entry.ftq_flag),
                            int(entry.ftq_value),
                        )
                    ):
                        continue
                    retained_entries.append(entry)
                self.ftq_entries = retained_entries
            self._schedule_next_queue_call_ret_commit_group()
            self._cfvec_queue_pop_head(int(span_len))
            committed_rank = self._ftq_ptr_rank_after_commit(int(ftq_flag), int(ftq_value))
            self.commit_ptr_flag = int(ftq_flag)
            self.commit_ptr_value = int(ftq_value)
            if pending_wait_rank is not None and int(pending_wait_rank) <= int(committed_rank):
                self._pending_level0_target_ftq = None
            self.commit_count += 1
            self._emit_event(
                "commit",
                {
                    "commit_count": int(self.commit_count),
                    "ftq_flag": int(ftq_flag),
                    "ftq_value": int(ftq_value),
                    "committed_entries": [{"ftq_flag": int(ftq_flag), "ftq_value": int(ftq_value)}],
                    "cfvec_queue_span": int(span_len),
                },
            )
            return committed_entry
        if self._semantic_fallback_commit_blocked():
            return None
        head = self._find_next_commitable_entry()
        if head is None:
            return None
        if self._has_pending_redirect_for_ftq(int(head.ftq_flag), int(head.ftq_value)):
            return None
        state = self._sync_backend_state()
        committed_entries = []
        retained_entries: Deque[FtqEntry] = deque()
        while state.ftq_entries:
            entry = state.ftq_entries.popleft()
            if self._ftq_entry_matches(entry, int(head.ftq_flag), int(head.ftq_value)):
                committed_entries.append(entry)
                continue
            retained_entries.append(entry)
        state.ftq_entries = retained_entries
        commit_rank = state.ftq_ptr_rank_after_commit(int(head.ftq_flag), int(head.ftq_value))
        if state.reuse_commit_ptr_once and self._ftq_entry_matches(head, state.commit_ptr_flag, state.commit_ptr_value):
            state.reuse_commit_ptr_once = False
        state.commit_ptr_flag = int(head.ftq_flag)
        state.commit_ptr_value = int(head.ftq_value)
        if (
            state.pending_level0_target_ftq is not None
            and state.ftq_ptr_rank_after_commit(
                int(state.pending_level0_target_ftq[0]),
                int(state.pending_level0_target_ftq[1]),
            ) <= int(commit_rank)
        ):
            state.pending_level0_target_ftq = None
        state.ftq_entries = deque(
            entry
            for entry in state.ftq_entries
            if 0 < state.ftq_ptr_rank_after_commit(int(entry.ftq_flag), int(entry.ftq_value)) < int(state.ftq_size)
        )
        state.commit_count += 1
        self._apply_backend_state()
        self._emit_event(
            "commit",
            {
                "commit_count": int(state.commit_count),
                "ftq_flag": head.ftq_flag,
                "ftq_value": head.ftq_value,
                "committed_entries": [
                    {"ftq_flag": int(entry.ftq_flag), "ftq_value": int(entry.ftq_value)}
                    for entry in committed_entries
                ],
            },
        )
        return head

    def _drive_commit(self) -> None:
        self._bound_backend_agent().drive_commit(self._plan_commit_entry_for_cycle())

    def _clear_one_shot_signals(self) -> None:
        self._bound_backend_agent().clear_one_shot_signals()

    def _recompute_cfi_budgets_from_pending_resolves(self) -> None:
        self._sync_backend_state()
        self._ftq_scoreboard.recompute_cfi_budgets_from_pending_resolves()
        self._apply_backend_state()

    def _plan_redirect_payload(self, payload: dict) -> dict:
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
        backend_igpf = int(payload.get("backend_igpf", 0))
        backend_ipf = int(payload.get("backend_ipf", 0))
        backend_iaf = int(payload.get("backend_iaf", 0))
        if any((backend_igpf, backend_ipf, backend_iaf)):
            required_keys = ("ftq_flag", "ftq_value", "ftq_offset", "pc")
            missing = [key for key in required_keys if key not in payload]
            if missing:
                raise AssertionError(
                    "backend-fault redirect requires explicit FTQ context: "
                    + ", ".join(missing)
                )
        original_commit_ptr = (int(self.commit_ptr_flag), int(self.commit_ptr_value))
        pre_flush_current_ftq = (
            (int(self._current_ftq_entry.ftq_flag), int(self._current_ftq_entry.ftq_value))
            if self._current_ftq_entry is not None
            else None
        )
        flush_itself = bool(level & 0x1)
        if flush_itself:
            self._pending_level0_target_ftq = None
        else:
            next_target_ftq = self._increment_ftq_ptr(ftq_flag, ftq_value)
            self._pending_level0_target_ftq = (
                None
                if next_target_ftq == original_commit_ptr
                else next_target_ftq
            )
        if bool(payload.get("flush_on_drive", False)):
            state = self._sync_backend_state()
            redirect_rank = state.ftq_ptr_rank_after_commit(int(ftq_flag), int(ftq_value))
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
            if pre_flush_current_ftq is not None and self._current_ftq_entry is None:
                self._cfvec_queue_close_current_ftq_span(
                    int(pre_flush_current_ftq[0]),
                    int(pre_flush_current_ftq[1]),
                )
            self._cfvec_queue_flush_wrong_path()
            self._semantic_recovery_target_pc = int(target_pc)
            self._semantic_recovery_queue_start = len(self._cfvec_queue)
            self._apply_backend_state()
            if not flush_itself:
                next_target_ftq = self._increment_ftq_ptr(ftq_flag, ftq_value)
                next_target_already_committed = next_target_ftq == original_commit_ptr
                self._pending_level0_target_ftq = (
                    None
                    if next_target_already_committed
                    else next_target_ftq
                )
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

        drive_payload = {
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
            "backend_igpf": backend_igpf,
            "backend_ipf": backend_ipf,
            "backend_iaf": backend_iaf,
        }
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
        return drive_payload

    def _drive_redirect(self, payload: dict) -> None:
        self._bound_backend_agent().drive_redirect(self._plan_redirect_payload(payload))

    def _ready_redirect_for_cycle(self) -> Optional[dict]:
        if not self.pending_events:
            return None
        ready_indices = [
            idx
            for idx, evt in enumerate(self.pending_events)
            if int(self.current_cycle) >= int(evt.ready_cycle)
        ]
        if not ready_indices:
            return None
        ready_exception_index = next(
            (idx for idx in ready_indices if self.pending_events[idx].kind == "exception"),
            None,
        )
        chosen_index = ready_exception_index if ready_exception_index is not None else ready_indices[0]
        top = self.pending_events[chosen_index]
        del self.pending_events[chosen_index]
        if top.kind == "redirect":
            return self._plan_redirect_payload(top.payload)
        elif top.kind == "exception":
            redirect_payload = self._plan_redirect_payload(top.payload)
            self._emit_event("exception", {"cause": top.payload.get("cause", 0)})
            return redirect_payload
        return None

    def _watchdog(self, observation: BackendObservationSnapshot) -> None:
        if int(observation.ibuf_full) == 1:
            self.ibuf_full_streak += 1
        else:
            self.ibuf_full_streak = 0
        if self.ibuf_full_streak >= self.ibuf_watchdog_threshold:
            self.inject_redirect(self.safe_pc, "ibuf_full_watchdog")
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

    def inject_redirect(self, target_pc: int, reason: str, delay_cycles: Optional[int] = None) -> None:
        self._queue_redirect_event(
            target_pc=int(target_pc),
            reason=str(reason),
            delay_cycles=delay_cycles,
            flush_on_drive=False,
        )
        self._pending_resolves.clear()
        self._clear_cfvec_queue_state()
        self._current_ftq_entry = None
        self._current_ftq_seen_packets.clear()
        self._golden_wait_pc = None
        self._semantic_recovery_target_pc = None
        self._semantic_recovery_queue_start = None
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

    def plan_cycle_actions(self) -> BackendCycleActions:
        observation = self._last_observation
        self._activate_visible_queue_call_ret_commit_group()
        self._clear_stale_wait_states()
        self._update_ftq_start_pc_cache(observation)
        self._watchdog(observation)
        if self.observe_if is not None:
            self._sample_cfvec()
        resolve_entries = self._ready_resolves_for_cycle()
        self._plan_instruction_commits_for_cycle()
        commit_entry = self._plan_commit_entry_for_cycle()
        redirect_payload = self._ready_redirect_for_cycle()
        call_ret_commit_group = self._current_semantic_call_ret_commit_group()
        self._schedule_next_queue_call_ret_commit_group()
        return BackendCycleActions(
            can_accept=self.can_accept,
            commit_entry=commit_entry,
            resolve_entries=resolve_entries,
            call_ret_commit_group=call_ret_commit_group,
            redirect_payload=redirect_payload,
        )

    def on_clock_edge(self, cycle: int) -> None:
        if self.drive_if is None or self.observe_if is None or self.from_ftq_if is None or self.frontend_info_if is None:
            return
        self.begin_cycle(cycle)
        agent = self._bound_backend_agent()
        agent.start_cycle(self.can_accept)
        self.consume_backend_observation(self._snapshot_bound_observation())
        actions = self.plan_cycle_actions()
        agent.drive_resolves(actions.resolve_entries)
        agent.drive_call_ret_commit(actions.call_ret_commit_group)
        agent.drive_commit(actions.commit_entry)
        if actions.redirect_payload is not None:
            agent.drive_redirect(actions.redirect_payload)

    def get_stats(self) -> dict:
        return {
            "commit_count": self.commit_count,
            "ftq_entries_pending": len(self.ftq_entries),
            "pending_resolves": len(self._pending_resolves),
            "pending_events": len(self.pending_events),
            "can_accept": self.can_accept,
        }

    def pending_work_count(self) -> int:
        scheduled_call_ret_count = sum(
            len(group)
            for _ready_cycle, group in self._scheduled_queue_call_ret_commit_groups
        )
        return (
            len(self.ftq_entries)
            + len(self._pending_resolves)
            + len(self.pending_events)
            + len(self._commit_queue)
            + len(self._pending_queue_call_ret_commit_indices)
            + int(scheduled_call_ret_count)
            + len(self._visible_queue_call_ret_commit_group)
            + (1 if self._current_ftq_entry is not None else 0)
            + (1 if self._pending_level0_target_ftq is not None else 0)
            + (1 if self._semantic_recovery_target_pc is not None else 0)
        )

    def has_pending_work(self) -> bool:
        return self.pending_work_count() > 0

    def recent_events(self, limit: int = 16) -> list:
        if limit <= 0:
            return []
        return list(self.last_events)[-limit:]

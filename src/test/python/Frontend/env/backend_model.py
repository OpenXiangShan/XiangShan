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
    ActiveWrongPathEpisode,
    BackendEvent,
    BackendState,
    CommitInstruction,
    FtqEntry,
    GOLDEN_MATCH_STATE_MATCHED,
    GOLDEN_MATCH_STATE_MISMATCHED,
    GOLDEN_MATCH_STATE_UNKNOWN,
    PATH_STATE_CORRECT,
    PATH_STATE_UNKNOWN,
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

# BackendModel owns backend-side semantic planning for FTQ, golden-trace,
# resolve, redirect, and commit behavior.


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
        self._rng = random.Random(int(random_seed)) if random_seed is not None else random

        self.current_cycle = 0
        self.commit_count = 0
        self.ftq_entries: Deque[FtqEntry] = deque()
        self._pending_resolves: Deque[ResolveEntry] = deque()
        self._current_ftq_entry: Optional[FtqEntry] = None
        self._current_ftq_seen_packets: set[tuple[int, int, int, int, int]] = set()
        self._current_ftq_max_offset = -1
        self._current_ftq_observed_pending_target_pc = False
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
        self._pending_level0_target_pc: Optional[int] = None
        self._cfvec_queue: Deque[QueueInstr] = deque()
        self._commit_queue: Deque[int] = deque()
        self._pending_queue_resolve_indices: Deque[int] = deque()
        self._pending_queue_call_ret_commit_indices: Deque[int] = deque()
        self._scheduled_queue_call_ret_commit_groups: Deque[tuple[int, list[int]]] = deque()
        self._visible_queue_call_ret_commit_group: list[int] = []
        self._active_wrong_path_episode_state: Optional[ActiveWrongPathEpisode] = None
        self.golden_trace: Optional[GoldenTrace] = None
        self._cycle_start_golden_pc: Optional[int] = None
        self._cycle_start_golden_cursor: Optional[int] = None
        self._last_observation = BackendObservationSnapshot()
        self._explicit_injection_enabled = True
        self._explicit_injection_block_reason = ""
        self._last_correct_cfi_context: Optional[dict] = None
        self._planned_commit_apply: Optional[dict] = None
        self._recovery_commit_block_ftq: Optional[tuple[int, int]] = None
        self._recovery_commit_block_cycle = -1
        self._last_driven_redirect_signature: Optional[tuple[int, int, int, int, int, int]] = None

        self._backend_state = BackendState(ftq_size=self.ftq_size)
        self._ftq_scoreboard = FtqScoreboard(self._backend_state)

    def _set_active_wrong_path_episode(
        self,
        *,
        origin_index: int,
        target_pc: Optional[int],
        redirect_context: Optional[dict],
        redirect_driven: bool = False,
        expected_recovery_ftq: Optional[tuple[int, int]] = None,
    ) -> None:
        normalized_origin = int(origin_index)
        self._active_wrong_path_episode_state = ActiveWrongPathEpisode(
            origin_index=normalized_origin,
            target_pc=(None if target_pc is None else int(target_pc)),
            redirect_context=(None if redirect_context is None else dict(redirect_context)),
            redirect_driven=bool(redirect_driven),
            expected_recovery_ftq=(
                None
                if expected_recovery_ftq is None
                else (int(expected_recovery_ftq[0]), int(expected_recovery_ftq[1]))
            ),
        )

    def _clear_active_wrong_path_episode(self) -> None:
        self._active_wrong_path_episode_state = None
        self._last_driven_redirect_signature = None

    def _has_active_wrong_path_episode(self) -> bool:
        return self._active_wrong_path_episode_state is not None

    def _active_wrong_path_origin_index(self) -> Optional[int]:
        episode = self._active_wrong_path_episode_state
        if episode is None:
            return None
        return int(episode.origin_index)

    def _active_wrong_path_episode(self) -> Optional[dict]:
        episode = self._active_wrong_path_episode_state
        if episode is None:
            return None
        return {
            "origin_index": int(episode.origin_index),
            "target_pc": (
                None
                if episode.target_pc is None
                else int(episode.target_pc)
            ),
            "redirect_context": (
                None
                if episode.redirect_context is None
                else dict(episode.redirect_context)
            ),
            "redirect_driven": bool(episode.redirect_driven),
            "expected_recovery_ftq": (
                None
                if episode.expected_recovery_ftq is None
                else (
                    int(episode.expected_recovery_ftq[0]),
                    int(episode.expected_recovery_ftq[1]),
                )
            ),
        }

    def _active_wrong_path_view(self) -> Optional[dict]:
        episode = self._active_wrong_path_episode()
        if episode is None:
            return None
        return {
            "origin_index": (
                None
                if episode["origin_index"] is None
                else int(episode["origin_index"])
            ),
            "target_pc": (
                None
                if episode["target_pc"] is None
                else int(episode["target_pc"])
            ),
            "redirect_driven": bool(episode["redirect_driven"]),
            "stage": (
                "recovery"
                if bool(episode["redirect_driven"])
                else "pre_redirect"
            ),
            "has_redirect_context": bool(episode["redirect_context"] is not None),
        }

    @staticmethod
    def _copy_active_wrong_path_episode(
        episode: Optional[ActiveWrongPathEpisode],
    ) -> Optional[ActiveWrongPathEpisode]:
        if episode is None:
            return None
        return ActiveWrongPathEpisode(
            origin_index=int(episode.origin_index),
            target_pc=(None if episode.target_pc is None else int(episode.target_pc)),
            redirect_context=(
                None
                if episode.redirect_context is None
                else dict(episode.redirect_context)
            ),
            redirect_driven=bool(episode.redirect_driven),
            expected_recovery_ftq=(
                None
                if episode.expected_recovery_ftq is None
                else (
                    int(episode.expected_recovery_ftq[0]),
                    int(episode.expected_recovery_ftq[1]),
                )
            ),
        )

    def _active_wrong_path_in_recovery(self) -> bool:
        episode = self._active_wrong_path_episode()
        return bool(
            episode is not None
            and episode["redirect_driven"]
            and episode["target_pc"] is not None
        )

    def _current_wrong_path_target_pc(self) -> Optional[int]:
        episode = self._active_wrong_path_episode()
        if episode is None or episode["target_pc"] is None:
            return None
        return int(episode["target_pc"])

    def _current_recovery_target_pc(self) -> Optional[int]:
        episode = self._active_wrong_path_episode()
        if (
            episode is not None
            and episode["redirect_driven"]
            and episode["target_pc"] is not None
        ):
            return int(episode["target_pc"])
        return None

    def _current_expected_recovery_ftq(self) -> Optional[tuple[int, int]]:
        episode = self._active_wrong_path_episode()
        if episode is None or not bool(episode["redirect_driven"]):
            return None
        expected_ftq = episode["expected_recovery_ftq"]
        if expected_ftq is None:
            return None
        return (int(expected_ftq[0]), int(expected_ftq[1]))

    def _queue_entry_matches_recovery_target(
        self,
        entry: QueueInstr,
        target_pc: int,
    ) -> bool:
        if int(entry.pc) != int(target_pc):
            return False
        expected_ftq = self._current_expected_recovery_ftq()
        if expected_ftq is None:
            return True
        return (int(entry.ftq_flag), int(entry.ftq_value)) == expected_ftq

    def _queue_entry_is_after_expected_recovery_ftq(self, entry: QueueInstr) -> bool:
        expected_ftq = self._current_expected_recovery_ftq()
        if expected_ftq is None:
            return True
        modulus = max(1, int(self.ftq_size) * 2)
        expected_abs = int(expected_ftq[0]) * int(self.ftq_size) + int(expected_ftq[1])
        entry_abs = int(entry.ftq_flag) * int(self.ftq_size) + int(entry.ftq_value)
        distance = (int(entry_abs) - int(expected_abs)) % int(modulus)
        return 0 < int(distance) < int(self.ftq_size)

    def _queue_entry_misses_expected_recovery_ftq(self, entry: QueueInstr) -> bool:
        expected_ftq = self._current_expected_recovery_ftq()
        if expected_ftq is None:
            return True
        return (int(entry.ftq_flag), int(entry.ftq_value)) != expected_ftq

    def _expected_recovery_ftq_for_redirect(
        self,
        *,
        ftq_flag: int,
        ftq_value: int,
        ftq_offset: int,
        is_rvc: bool,
        taken: bool = True,
        flush_itself: bool = False,
    ) -> tuple[int, int]:
        del taken
        if not bool(flush_itself):
            return self._increment_ftq_ptr(int(ftq_flag), int(ftq_value))
        if self._redirect_reuses_same_ftq_slot(int(ftq_offset), bool(is_rvc)):
            return (int(ftq_flag), int(ftq_value))
        return self._increment_ftq_ptr(int(ftq_flag), int(ftq_value))

    def _current_recovery_queue_start(self) -> int:
        origin_index = self._active_wrong_path_origin_index()
        if origin_index is None:
            first_non_correct = self._first_non_correct_queue_index()
            return 0 if first_non_correct is None else max(0, int(first_non_correct))
        for idx, entry in enumerate(self._cfvec_queue):
            if int(idx) >= int(origin_index):
                break
            if entry.path_state == PATH_STATE_UNKNOWN:
                return int(idx)
        return max(0, int(origin_index))

    def _recovery_phase_active(self) -> bool:
        return self._active_wrong_path_in_recovery()

    @staticmethod
    def _detach_redirect_context_from_queue(redirect_context: Optional[dict]) -> Optional[dict]:
        if redirect_context is None:
            return None
        detached_context = dict(redirect_context)
        detached_context["queue_index"] = None
        detached_context["queue_context_optional"] = True
        return detached_context

    def _mark_active_wrong_path_redirect_driven(
        self,
        target_pc: int,
        *,
        redirect_context: Optional[dict] = None,
        expected_recovery_ftq: Optional[tuple[int, int]] = None,
    ) -> None:
        episode = self._active_wrong_path_episode()
        origin_index = (
            len(self._cfvec_queue)
            if episode is None or episode["origin_index"] is None
            else int(episode["origin_index"])
        )
        if redirect_context is None and episode is not None:
            redirect_context = self._detach_redirect_context_from_queue(
                episode["redirect_context"]
            )
        else:
            redirect_context = self._detach_redirect_context_from_queue(redirect_context)
        if expected_recovery_ftq is None and episode is not None:
            expected_recovery_ftq = episode["expected_recovery_ftq"]
        if redirect_context is None:
            self._last_driven_redirect_signature = None
        else:
            self._last_driven_redirect_signature = (
                int(target_pc),
                int(redirect_context.get("pc", -1)),
                int(redirect_context.get("ftq_flag", -1)),
                int(redirect_context.get("ftq_value", -1)),
                int(redirect_context.get("ftq_offset", -1)),
                int(redirect_context.get("is_rvc", -1)),
            )
        self._set_active_wrong_path_episode(
            origin_index=int(origin_index),
            target_pc=int(target_pc),
            redirect_context=redirect_context,
            redirect_driven=True,
            expected_recovery_ftq=expected_recovery_ftq,
        )

    def _remap_active_wrong_path_episode(
        self,
        *,
        new_origin_index: Optional[int],
        queue_index_map: Optional[dict[int, int]] = None,
        reason: str = "remap_active_wrong_path_episode",
    ) -> None:
        episode = self._active_wrong_path_episode()
        if episode is None:
            return
        redirect_context = episode["redirect_context"]
        if redirect_context is not None and bool(episode["redirect_driven"]):
            redirect_context = self._detach_redirect_context_from_queue(redirect_context)
        elif redirect_context is not None:
            redirect_context = self._remap_redirect_context_queue_index(
                redirect_context,
                queue_index_map=queue_index_map,
                reason=reason,
            )
        if new_origin_index is None:
            self._clear_active_wrong_path_episode()
            return
        self._set_active_wrong_path_episode(
            origin_index=int(new_origin_index),
            target_pc=episode["target_pc"],
            redirect_context=redirect_context,
            redirect_driven=bool(episode["redirect_driven"]),
            expected_recovery_ftq=episode["expected_recovery_ftq"],
        )

    def _restore_active_wrong_path_episode_after_queue_edit(
        self,
        prior_episode: Optional[dict],
    ) -> None:
        if prior_episode is None:
            return
        if self._has_active_wrong_path_episode():
            episode = self._active_wrong_path_episode()
            assert episode is not None
            redirect_context = episode["redirect_context"]
            if redirect_context is None and not bool(episode["redirect_driven"]):
                redirect_context = prior_episode["redirect_context"]
            if redirect_context is not None and bool(episode["redirect_driven"]):
                redirect_context = self._detach_redirect_context_from_queue(redirect_context)
            elif redirect_context is not None:
                redirect_context = self._remap_redirect_context_queue_index(
                    redirect_context,
                    reason="restore_after_queue_edit",
                )
            self._set_active_wrong_path_episode(
                origin_index=int(episode["origin_index"]),
                target_pc=episode["target_pc"],
                redirect_context=redirect_context,
                redirect_driven=bool(episode["redirect_driven"]),
                expected_recovery_ftq=episode["expected_recovery_ftq"],
            )
            return
        if not any(evt.kind == "redirect" for evt in self.pending_events):
            return
        start_idx = self._first_non_correct_queue_index()
        if start_idx is None:
            return
        redirect_context = None
        if bool(prior_episode["redirect_driven"]):
            redirect_context = self._detach_redirect_context_from_queue(
                prior_episode["redirect_context"]
            )
        else:
            redirect_context = self._remap_redirect_context_queue_index(
                prior_episode["redirect_context"],
                reason="restore_after_queue_edit",
            )
        self._set_active_wrong_path_episode(
            origin_index=int(start_idx),
            target_pc=prior_episode["target_pc"],
            redirect_context=redirect_context,
            redirect_driven=bool(prior_episode["redirect_driven"]),
            expected_recovery_ftq=prior_episode["expected_recovery_ftq"],
        )

    def _remap_last_correct_cfi_context_after_queue_edit(
        self,
        queue_index_map: dict[int, int],
        *,
        reason: str,
    ) -> None:
        if self._last_correct_cfi_context is None:
            return
        queue_index = self._last_correct_cfi_context.get("queue_index")
        if queue_index is None:
            return
        if int(queue_index) not in queue_index_map:
            if str(reason) == "cfvec_queue_pop_head":
                self._last_correct_cfi_context = dict(self._last_correct_cfi_context)
                self._last_correct_cfi_context["queue_index"] = None
                self._last_correct_cfi_context["queue_context_optional"] = True
                return
            self._last_correct_cfi_context = None
            return
        self._last_correct_cfi_context = self._remap_redirect_context_queue_index(
            self._last_correct_cfi_context,
            queue_index_map=queue_index_map,
            reason=reason,
        )

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
        state.active_wrong_path_episode = self._copy_active_wrong_path_episode(
            self._active_wrong_path_episode_state
        )
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
        self._active_wrong_path_episode_state = self._copy_active_wrong_path_episode(state.active_wrong_path_episode)
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

    @staticmethod
    def _ftq_entry_matches(entry: FtqEntry, flag: int, value: int) -> bool:
        return BackendState.ftq_entry_matches(entry, flag, value)

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

    def _redirect_drive_ftq_is_stale_relative_to_commit(self, flag: int, value: int) -> bool:
        state = self._sync_backend_state()
        if (
            int(state.commit_count) <= 0
            and int(state.commit_ptr_flag) == 0
            and int(state.commit_ptr_value) == 0
            and not bool(state.reuse_commit_ptr_once)
        ):
            return False
        rank = int(state.ftq_ptr_rank_after_commit(int(flag), int(value)))
        return rank > int(state.ftq_size)

    def _cfvec_queue_mode_active(self) -> bool:
        return self.golden_trace is not None and bool(self._cfvec_queue)

    def _semantic_fallback_commit_blocked(self) -> bool:
        if self.golden_trace is None:
            return False
        if self._cfvec_queue:
            return True
        # When recovery is in progress, keep commit gating on the
        # semantic-queue path semantics instead of falling back to FTQ-only rules.
        return (
            self._recovery_phase_active()
            or self._has_active_wrong_path_episode()
        )

    def _clear_stale_auxiliary_states(self) -> None:
        if self._pending_level0_target_ftq is not None:
            target_flag = int(self._pending_level0_target_ftq[0])
            target_value = int(self._pending_level0_target_ftq[1])
            target_rank = self._ftq_ptr_rank_after_commit(target_flag, target_value)
            target_present = bool(
                self._cfvec_queue_has_ftq(target_flag, target_value)
                or (
                    self._current_ftq_entry is not None
                    and self._ftq_entry_matches(self._current_ftq_entry, target_flag, target_value)
                )
                or any(self._ftq_entry_matches(entry, target_flag, target_value) for entry in self.ftq_entries)
            )
            if int(target_rank) >= int(self.ftq_size) and not target_present:
                self._clear_pending_level0_target_ftq()

        if (
            self._recovery_phase_active()
            and not any(evt.kind == "redirect" for evt in self.pending_events)
            and not any(entry.path_state != PATH_STATE_CORRECT for entry in self._cfvec_queue)
        ):
            target_pc = self._current_recovery_target_pc()
            if target_pc is None or any(
                entry.path_state == PATH_STATE_CORRECT and int(entry.pc) == int(target_pc)
                for entry in self._cfvec_queue
            ):
                self._clear_active_wrong_path_episode()

        if (
            not self._recovery_phase_active()
            and not any(evt.kind == "redirect" for evt in self.pending_events)
            and any(entry.path_state == PATH_STATE_WRONG for entry in self._cfvec_queue)
        ):
            raise AssertionError(
                "wrong-path residue exists without active redirect/recovery; "
                "silent cleanup is forbidden"
            )

    def _cfvec_queue_has_ftq(self, ftq_flag: int, ftq_value: int) -> bool:
        return any(
            int(entry.ftq_flag) == int(ftq_flag) and int(entry.ftq_value) == int(ftq_value)
            for entry in self._cfvec_queue
        )

    def _cfvec_queue_has_ftq_before_index(self, ftq_flag: int, ftq_value: int, queue_index: int) -> bool:
        return any(
            int(entry.ftq_flag) == int(ftq_flag) and int(entry.ftq_value) == int(ftq_value)
            for entry in list(self._cfvec_queue)[: max(0, int(queue_index))]
        )

    def _cfvec_queue_has_correct_path_pc_in_ftq(self, ftq_flag: int, ftq_value: int, pc: int) -> bool:
        return any(
            int(entry.ftq_flag) == int(ftq_flag)
            and int(entry.ftq_value) == int(ftq_value)
            and int(entry.pc) == int(pc)
            and entry.path_state == PATH_STATE_CORRECT
            for entry in self._cfvec_queue
        )

    def _recovery_commit_block_matches(self, ftq_flag: int, ftq_value: int) -> bool:
        return (
            self._recovery_commit_block_ftq is not None
            and int(self.current_cycle) <= int(self._recovery_commit_block_cycle)
            and (int(ftq_flag), int(ftq_value))
            == (
                int(self._recovery_commit_block_ftq[0]),
                int(self._recovery_commit_block_ftq[1]),
            )
        )

    def _commit_queue_append(self, queue_index: int) -> None:
        # `_commit_queue` is only a derived debug view. Runtime commit
        # eligibility is recomputed from `_cfvec_queue` every cycle.
        del queue_index

    def _assert_queue_index_not_in_commit_queue(self, queue_index: int, *, reason: str) -> None:
        del queue_index, reason

    def _clear_pending_level0_target_ftq(self) -> None:
        self._pending_level0_target_ftq = None
        self._pending_level0_target_pc = None
        self._current_ftq_observed_pending_target_pc = False

    def _set_pending_level0_target_ftq(
        self,
        pending_ftq: Optional[tuple[int, int]],
        *,
        target_pc: Optional[int],
    ) -> None:
        if pending_ftq is None:
            self._clear_pending_level0_target_ftq()
            return
        self._pending_level0_target_ftq = (
            int(pending_ftq[0]),
            int(pending_ftq[1]),
        )
        self._pending_level0_target_pc = (
            None if target_pc is None else int(target_pc)
        )
        self._current_ftq_observed_pending_target_pc = False

    def _pending_level0_target_rank(self) -> Optional[int]:
        if self._pending_level0_target_ftq is None:
            return None
        pending_rank = int(
            self._ftq_ptr_rank_after_commit(
                int(self._pending_level0_target_ftq[0]),
                int(self._pending_level0_target_ftq[1]),
            )
        )
        if pending_rank <= 0 or pending_rank >= int(self.ftq_size):
            return None
        return int(pending_rank)

    def _candidate_is_pending_level0_target(self, ftq_flag: int, ftq_value: int) -> bool:
        return (
            self._pending_level0_target_ftq is not None
            and (int(ftq_flag), int(ftq_value))
            == (
                int(self._pending_level0_target_ftq[0]),
                int(self._pending_level0_target_ftq[1]),
            )
        )

    def _clear_pending_level0_target_ftq_if_reached(self, ftq_flag: int, ftq_value: int) -> None:
        if self._candidate_is_pending_level0_target(int(ftq_flag), int(ftq_value)):
            self._clear_pending_level0_target_ftq()

    def _observe_pending_level0_target_packet(
        self,
        *,
        ftq_flag: int,
        ftq_value: int,
        pc: int,
        is_last: bool,
    ) -> None:
        if not self._candidate_is_pending_level0_target(int(ftq_flag), int(ftq_value)):
            return
        if self._pending_level0_target_pc is None and not bool(is_last):
            self._pending_level0_target_pc = int(pc)
        if (
            self._pending_level0_target_pc is not None
            and int(pc) == int(self._pending_level0_target_pc)
        ):
            self._current_ftq_observed_pending_target_pc = True

    def _refresh_pending_level0_target_ftq_for_redirect(
        self,
        *,
        redirect_ftq_flag: int,
        redirect_ftq_value: int,
        target_pc: int,
        flush_itself: bool,
        commit_ptr: tuple[int, int],
    ) -> None:
        if bool(flush_itself):
            self._clear_pending_level0_target_ftq()
            return
        next_target_ftq = self._increment_ftq_ptr(int(redirect_ftq_flag), int(redirect_ftq_value))
        pending_target_ftq = (
            None
            if next_target_ftq == commit_ptr
            else next_target_ftq
        )
        self._set_pending_level0_target_ftq(
            pending_target_ftq,
            target_pc=(
                None if pending_target_ftq is None else int(target_pc)
            ),
        )

    def _ensure_commit_queue_consistency(self) -> None:
        if not self._cfvec_queue:
            self._commit_queue.clear()
            return
        prefix_end = self._correct_prefix_end_index()
        self._commit_queue = deque(
            idx
            for idx, entry in enumerate(self._cfvec_queue)
            if int(idx) < int(prefix_end) and entry.path_state == PATH_STATE_CORRECT
        )

    def _correct_prefix_end_index(self) -> int:
        for idx, entry in enumerate(self._cfvec_queue):
            if entry.path_state != PATH_STATE_CORRECT:
                return int(idx)
        return len(self._cfvec_queue)

    def _commit_queue_head_ftq_span(self) -> Optional[tuple[tuple[int, int], int]]:
        if not self._commit_queue:
            return None
        prefix_end = self._correct_prefix_end_index()
        head_index = int(self._commit_queue[0])
        if head_index < 0 or head_index >= len(self._cfvec_queue) or int(head_index) >= int(prefix_end):
            return None
        head_entry = self._cfvec_queue[head_index]
        key = (int(head_entry.ftq_flag), int(head_entry.ftq_value))
        span_len = 0
        saw_last = False
        for idx in self._commit_queue:
            queue_index = int(idx)
            if queue_index < 0 or queue_index >= len(self._cfvec_queue) or int(queue_index) >= int(prefix_end):
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

    def _queue_instruction_commit_candidate_indices(self) -> list[int]:
        candidates: list[int] = []
        for idx, entry in enumerate(self._cfvec_queue):
            if entry.path_state != PATH_STATE_CORRECT:
                break
            if entry.rob_commit_state == ROB_COMMIT_STATE_COMMITTED:
                continue
            if entry.rob_commit_state != ROB_COMMIT_STATE_PENDING:
                break
            if int(entry.cycle) >= int(self.current_cycle):
                break
            if entry.is_cfi and entry.resolve_state != RESOLVE_STATE_EMITTED:
                break
            candidates.append(int(idx))
            if len(candidates) >= int(self.instruction_commit_width):
                break
        return candidates

    def _queue_head_ftq_commit_span(self) -> Optional[tuple[tuple[int, int], int]]:
        if not self._cfvec_queue:
            return None
        head_entry = self._cfvec_queue[0]
        if head_entry.path_state != PATH_STATE_CORRECT:
            return None
        key = (int(head_entry.ftq_flag), int(head_entry.ftq_value))
        span_len = 0
        saw_last = False
        for entry in self._cfvec_queue:
            if (int(entry.ftq_flag), int(entry.ftq_value)) != key:
                break
            if entry.path_state != PATH_STATE_CORRECT:
                break
            if entry.rob_commit_state != ROB_COMMIT_STATE_COMMITTED:
                return None
            if entry.is_cfi and entry.resolve_state != RESOLVE_STATE_EMITTED:
                return None
            span_len += 1
            if bool(entry.is_last_in_entry):
                saw_last = True
                break
        if span_len <= 0:
            return None
        if not saw_last and self._queue_head_ftq_boundary_is_known(key):
            self._cfvec_queue[span_len - 1].is_last_in_entry = True
            saw_last = True
        if not saw_last:
            return None
        return key, int(span_len)

    def _queue_head_ftq_boundary_is_known(self, key: tuple[int, int]) -> bool:
        ftq_flag, ftq_value = key
        current_matches_head = bool(
            self._current_ftq_entry is not None
            and int(self._current_ftq_entry.ftq_flag) == int(ftq_flag)
            and int(self._current_ftq_entry.ftq_value) == int(ftq_value)
        )
        if current_matches_head:
            return False
        return any(
            int(entry.ftq_flag) == int(ftq_flag)
            and int(entry.ftq_value) == int(ftq_value)
            and (bool(entry.dispatch_complete) or bool(entry.observed_last_in_entry))
            for entry in self.ftq_entries
        )

    def _semantic_commit_can_skip_missing_ftq(
        self,
        ftq_flag: int,
        ftq_value: int,
        *,
        before_queue_index: Optional[int] = None,
        before_ftq: Optional[tuple[int, int]] = None,
    ) -> bool:
        if before_queue_index is None:
            queue_has_blocking_ftq = self._cfvec_queue_has_ftq(int(ftq_flag), int(ftq_value))
        else:
            queue_has_blocking_ftq = self._cfvec_queue_has_ftq_before_index(
                int(ftq_flag),
                int(ftq_value),
                int(before_queue_index),
            )
        if queue_has_blocking_ftq:
            return False
        if (
            self._current_ftq_entry is not None
            and self._ftq_entry_matches(self._current_ftq_entry, int(ftq_flag), int(ftq_value))
        ):
            return False
        if self._has_pending_redirect_for_ftq(int(ftq_flag), int(ftq_value)):
            return False
        visible_ftq_entries = list(self.ftq_entries)
        if before_ftq is None:
            candidate_prefix = visible_ftq_entries
        else:
            first_candidate_index = next(
                (
                    idx
                    for idx, entry in enumerate(visible_ftq_entries)
                    if self._ftq_entry_matches(entry, int(before_ftq[0]), int(before_ftq[1]))
                ),
                len(visible_ftq_entries),
            )
            candidate_prefix = visible_ftq_entries[: int(first_candidate_index)]
        matching_ftq_entries = [
            entry for entry in candidate_prefix
            if self._ftq_entry_matches(entry, int(ftq_flag), int(ftq_value))
        ]
        if matching_ftq_entries:
            if any(
                self._ftq_entry_matches(resolve_entry, int(ftq_flag), int(ftq_value))
                for resolve_entry in self._pending_resolves
            ):
                return False
            if not all(
                bool(entry.dispatch_complete)
                and int(entry.resolved_cfi) >= int(entry.total_cfi)
                and int(entry.commit_ready_cycle) <= int(self.current_cycle)
                for entry in matching_ftq_entries
            ):
                return False
            raise AssertionError(
                "ftq entry became skippable only because it vanished from cfvec_queue; "
                "silent FTQ pruning is forbidden: "
                f"ftq=({int(ftq_flag)},{int(ftq_value)})"
            )
        return True

    def _assert_no_stale_ftq_entries_behind_commit_ptr(self) -> None:
        have_prior_commit = bool(
            int(self.commit_count) > 0
            or int(self.commit_ptr_flag) != 0
            or int(self.commit_ptr_value) != 0
            or bool(self._reuse_commit_ptr_once)
        )
        if not have_prior_commit:
            return
        for entry in self.ftq_entries:
            rank = self._ftq_ptr_rank_after_commit(int(entry.ftq_flag), int(entry.ftq_value))
            if 0 < int(rank) < int(self.ftq_size):
                continue
            if self._cfvec_queue_has_ftq(int(entry.ftq_flag), int(entry.ftq_value)):
                continue
            if (
                self._current_ftq_entry is not None
                and self._ftq_entry_matches(
                    self._current_ftq_entry,
                    int(entry.ftq_flag),
                    int(entry.ftq_value),
                )
            ):
                continue
            if self._has_pending_redirect_for_ftq(int(entry.ftq_flag), int(entry.ftq_value)):
                continue
            raise AssertionError(
                "stale ftq entry remained behind commit_ptr without commit/redirect: "
                f"ftq=({int(entry.ftq_flag)},{int(entry.ftq_value)}) "
                f"commit_ptr=({int(self.commit_ptr_flag)},{int(self.commit_ptr_value)})"
            )

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
        if queue_entry.is_cfi:
            queue_entry.golden_target_pc = self._golden_redirect_target_from_trace_entry(entry)
        self._commit_queue_append(int(queue_index))

    @staticmethod
    def _golden_redirect_target_from_trace_entry(entry: TraceEntry) -> Optional[int]:
        kind = str(getattr(entry, "kind", "normal"))
        if kind not in {"branch", "jump", "jump_indirect", "call", "ret"}:
            return None
        if bool(entry.taken):
            target_pc = getattr(entry, "target_pc", None)
            if target_pc is None:
                return None
            return int(target_pc)
        return (int(entry.pc) + int(entry.size)) & 0xFFFFFFFFFFFFFFFF

    def _replay_golden_matches_from_queue_head(self) -> None:
        if self.golden_trace is None:
            return
        replayed_any = False
        for queue_index, entry in enumerate(list(self._cfvec_queue)):
            if entry.path_state == PATH_STATE_WRONG:
                break
            if entry.golden_match_state == GOLDEN_MATCH_STATE_MATCHED:
                replayed_any = True
                continue
            golden_entry = self._consume_golden_entry(int(entry.pc))
            if golden_entry is None:
                break
            self._cfvec_queue_mark_matched(int(queue_index), golden_entry)
            replayed_any = True
            next_entry = self.golden_trace.peek()
            if next_entry is None:
                break
            if self._maybe_begin_active_wrong_path_after_correct_cfi(
                queue_index=int(queue_index),
                entry=entry,
                target_pc=int(next_entry.pc),
                target_visible_immediately=False,
            ):
                break
        if replayed_any:
            self._ensure_commit_queue_consistency()

    def _drop_pending_redirects_for_target(self, target_pc: int) -> None:
        self.pending_events = deque(
            evt
            for evt in self.pending_events
            if not (
                evt.kind == "redirect"
                and str(evt.payload.get("reason", "")) == "golden_first_mismatch_redirect"
                and int(evt.payload.get("target_pc", -1)) == int(target_pc)
            )
        )

    def _has_pending_redirect_for_target(self, target_pc: int) -> bool:
        return any(
            evt.kind == "redirect"
            and int(evt.payload.get("target_pc", -1)) == int(target_pc)
            for evt in self.pending_events
        )

    def _find_preceding_correct_path_cfi(self, queue_index: int) -> Optional[tuple[int, QueueInstr]]:
        if queue_index <= 0:
            return None
        for idx in range(int(queue_index) - 1, -1, -1):
            entry = self._cfvec_queue[idx]
            if entry.path_state == PATH_STATE_WRONG:
                break
            if entry.path_state != PATH_STATE_CORRECT:
                continue
            if entry.is_cfi:
                return int(idx), entry
        return None

    def _has_wrong_path_barrier_before(self, queue_index: int) -> bool:
        if queue_index <= 0:
            return False
        for idx in range(int(queue_index) - 1, -1, -1):
            entry = self._cfvec_queue[idx]
            if entry.path_state == PATH_STATE_WRONG:
                return True
            if entry.path_state == PATH_STATE_CORRECT:
                continue
        return False

    def _build_redirect_context_from_queue_entry(
        self,
        queue_index: Optional[int],
        entry: QueueInstr,
    ) -> Optional[dict]:
        cfi = self._classify_cfi(
            int(entry.instr),
            int(entry.pc),
            bool(entry.pred_taken),
            bool(entry.is_rvc),
        )
        if cfi is None:
            return None
        branch_type, ras_action, _pred_target, _pred_taken = cfi
        return {
            "pc": int(entry.pc),
            "instr": int(entry.instr),
            "is_rvc": int(entry.is_rvc),
            "ftq_flag": int(entry.ftq_flag),
            "ftq_value": int(entry.ftq_value),
            "ftq_offset": int(entry.ftq_offset),
            "branch_type": int(branch_type),
            "ras_action": int(ras_action),
            "queue_index": None if queue_index is None else int(queue_index),
        }

    def _assert_active_redirect_context_valid(
        self,
        redirect_context: Optional[dict],
        *,
        reason: str,
    ) -> None:
        if redirect_context is None:
            return
        remapped_context = self._remap_redirect_context_queue_index(
            redirect_context,
            reason=reason,
        )
        redirect_context.clear()
        redirect_context.update(remapped_context)

    def _redirect_context_matches_queue_entry(
        self,
        redirect_context: dict,
        entry: QueueInstr,
    ) -> bool:
        cfi = self._classify_cfi(
            int(entry.instr),
            int(entry.pc),
            bool(entry.pred_taken),
            bool(entry.is_rvc),
        )
        if cfi is None:
            return False
        branch_type, ras_action, _pred_target, _pred_taken = cfi
        return bool(
            entry.is_cfi
            and int(entry.pc) == int(redirect_context.get("pc", -1))
            and (
                redirect_context.get("instr") is None
                or int(entry.instr) == int(redirect_context.get("instr", -1))
            )
            and int(entry.is_rvc) == int(redirect_context.get("is_rvc", -1))
            and int(entry.ftq_flag) == int(redirect_context.get("ftq_flag", -1))
            and int(entry.ftq_value) == int(redirect_context.get("ftq_value", -1))
            and int(entry.ftq_offset) == int(redirect_context.get("ftq_offset", -1))
            and int(branch_type) == int(redirect_context.get("branch_type", -1))
            and int(ras_action) == int(redirect_context.get("ras_action", -1))
        )

    def _matching_redirect_context_queue_indices(self, redirect_context: dict) -> list[int]:
        return [
            int(idx)
            for idx, entry in enumerate(self._cfvec_queue)
            if self._redirect_context_matches_queue_entry(redirect_context, entry)
        ]

    def _raise_stale_redirect_context(
        self,
        redirect_context: dict,
        *,
        reason: str,
        queue_index: object,
    ) -> None:
        ftq_flag = int(redirect_context.get("ftq_flag", -1))
        ftq_value = int(redirect_context.get("ftq_value", -1))
        raise AssertionError(
            "active redirect context became stale before queue/drive: "
            f"reason={str(reason)} ftq=({int(ftq_flag)},{int(ftq_value)}) "
            f"commit_ptr=({int(self.commit_ptr_flag)},{int(self.commit_ptr_value)}) "
            f"queue_index={queue_index} "
            f"context_pc=0x{int(redirect_context.get('pc', -1)):x}"
        )

    def _remap_redirect_context_queue_index(
        self,
        redirect_context: Optional[dict],
        *,
        queue_index_map: Optional[dict[int, int]] = None,
        reason: str,
    ) -> Optional[dict]:
        if redirect_context is None:
            return None
        remapped_context = dict(redirect_context)
        queue_index = remapped_context.get("queue_index")
        if queue_index is None and bool(remapped_context.get("queue_context_optional", False)):
            return remapped_context
        if queue_index_map is not None and queue_index is not None:
            old_idx = int(queue_index)
            new_idx = queue_index_map.get(old_idx)
            if new_idx is None:
                self._raise_stale_redirect_context(
                    remapped_context,
                    reason=reason,
                    queue_index=old_idx,
                )
            remapped_context["queue_index"] = int(new_idx)
            if self._redirect_context_queue_index_matches(remapped_context):
                return remapped_context
            self._raise_stale_redirect_context(
                remapped_context,
                reason=reason,
                queue_index=int(new_idx),
            )
        if queue_index is not None and self._redirect_context_queue_index_matches(remapped_context):
            return remapped_context
        matches = self._matching_redirect_context_queue_indices(remapped_context)
        if len(matches) == 1:
            remapped_context["queue_index"] = int(matches[0])
            return remapped_context
        self._raise_stale_redirect_context(
            remapped_context,
            reason=reason,
            queue_index=queue_index,
        )

    def _redirect_context_queue_index_matches(self, redirect_context: dict) -> bool:
        queue_index = redirect_context.get("queue_index")
        if queue_index is None:
            return False
        idx = int(queue_index)
        if idx < 0 or idx >= len(self._cfvec_queue):
            return False
        return self._redirect_context_matches_queue_entry(
            redirect_context,
            self._cfvec_queue[int(idx)],
        )

    def _queue_entry_predicted_target_mismatches(self, entry: QueueInstr, target_pc: int) -> bool:
        predicted = self._predicted_cfi_outcome(
            int(entry.instr),
            int(entry.pc),
            bool(entry.pred_taken),
            bool(entry.is_rvc),
        )
        if predicted is None:
            return False
        _pred_taken, pred_target = predicted
        if pred_target is None:
            return False
        return int(pred_target) != int(target_pc)

    def _redirect_context_ftq_is_stale_for_drive(self, redirect_context: dict) -> bool:
        if "ftq_flag" not in redirect_context or "ftq_value" not in redirect_context:
            return False
        return self._redirect_drive_ftq_is_stale_relative_to_commit(
            int(redirect_context["ftq_flag"]),
            int(redirect_context["ftq_value"]),
        )

    def _queue_active_wrong_path_redirect(self) -> bool:
        episode = self._active_wrong_path_episode()
        if episode is None or bool(episode["redirect_driven"]):
            return False
        redirect_context = episode["redirect_context"]
        redirect_target_pc = episode["target_pc"]
        if redirect_context is None or redirect_target_pc is None:
            return False
        redirect_target_pc = int(redirect_target_pc)
        self._assert_active_redirect_context_valid(
            redirect_context,
            reason="queue_active_wrong_path_redirect",
        )
        self._set_active_wrong_path_episode(
            origin_index=int(episode["origin_index"]),
            target_pc=int(redirect_target_pc),
            redirect_context=redirect_context,
            redirect_driven=bool(episode["redirect_driven"]),
        )
        if self._has_pending_redirect_for_target(int(redirect_target_pc)):
            return False
        redirect_pc = int(redirect_context["pc"])
        redirect_is_rvc = bool(redirect_context["is_rvc"])
        redirect_taken = int(
            int(redirect_target_pc)
            != int(self._sequential_next_pc(int(redirect_pc), bool(redirect_is_rvc)))
        )
        redirect_context_queue_index = redirect_context.get("queue_index")
        if redirect_context_queue_index is not None:
            for resolve_entry in self._pending_resolves:
                if resolve_entry.queue_index != int(redirect_context_queue_index):
                    continue
                resolve_entry.mispredict = True
                resolve_entry.target = int(redirect_target_pc)
                resolve_entry.taken = bool(redirect_taken)
                break
        self._mark_ftq_redirect_pending(
            int(redirect_context["ftq_flag"]),
            int(redirect_context["ftq_value"]),
        )
        self._queue_redirect_event(
            target_pc=int(redirect_target_pc),
            reason="golden_first_mismatch_redirect",
            flush_on_drive=True,
            payload_extra={
                "pc": int(redirect_pc),
                "taken": int(redirect_taken),
                "ftq_flag": int(redirect_context["ftq_flag"]),
                "ftq_value": int(redirect_context["ftq_value"]),
                "ftq_offset": int(redirect_context["ftq_offset"]),
                "branch_type": int(redirect_context["branch_type"]),
                "ras_action": int(redirect_context["ras_action"]),
                "is_rvc": int(redirect_context["is_rvc"]),
                "level": 0,
            },
        )
        return True

    def _begin_active_wrong_path_episode(
        self,
        *,
        origin_index: int,
        target_pc: Optional[int],
        redirect_context: Optional[dict],
        queue_redirect: bool = True,
    ) -> None:
        self._set_active_wrong_path_episode(
            origin_index=max(0, int(origin_index)),
            target_pc=None if target_pc is None else int(target_pc),
            redirect_context=redirect_context,
        )
        if redirect_context is not None:
            self._mark_ftq_redirect_pending(
                int(redirect_context["ftq_flag"]),
                int(redirect_context["ftq_value"]),
            )
        if bool(queue_redirect):
            self._queue_active_wrong_path_redirect()

    def _refresh_active_wrong_path_redirect_cause(
        self,
        *,
        origin_index: int,
        target_pc: int,
        redirect_context: dict,
        redirect_driven: bool,
    ) -> None:
        self._set_active_wrong_path_episode(
            origin_index=int(origin_index),
            target_pc=int(target_pc),
            redirect_context=dict(redirect_context),
            redirect_driven=bool(redirect_driven),
        )

    def _begin_active_wrong_path_episode_from_redirect_context(
        self,
        *,
        origin_index: int,
        target_pc: int,
        redirect_context: dict,
        queue_redirect: bool = True,
    ) -> None:
        self._begin_active_wrong_path_episode(
            origin_index=int(origin_index),
            target_pc=int(target_pc),
            redirect_context=dict(redirect_context),
            queue_redirect=bool(queue_redirect),
        )

    def _begin_active_wrong_path_episode_from_queue_cfi(
        self,
        *,
        queue_index: int,
        entry: QueueInstr,
        target_pc: int,
        queue_redirect: bool = True,
    ) -> None:
        redirect_context = self._build_redirect_context_from_queue_entry(
            int(queue_index),
            entry,
        )
        if redirect_context is None:
            raise AssertionError(
                "cannot establish wrong-path episode without attributable queue CFI: "
                f"queue_index={int(queue_index)} pc=0x{int(entry.pc):x}"
            )
        self._begin_active_wrong_path_episode_from_redirect_context(
            origin_index=int(queue_index) + 1,
            target_pc=int(target_pc),
            redirect_context=redirect_context,
            queue_redirect=bool(queue_redirect),
        )

    def _maybe_begin_active_wrong_path_after_correct_cfi(
        self,
        *,
        queue_index: int,
        entry: QueueInstr,
        target_pc: Optional[int],
        target_visible_immediately: bool,
        queue_redirect: bool = True,
    ) -> bool:
        if target_pc is None:
            return False
        if int(target_pc) == self._sequential_next_pc(int(entry.pc), bool(entry.is_rvc)):
            return False
        golden_taken = True
        predicted = self._predicted_cfi_outcome(
            int(entry.instr),
            int(entry.pc),
            bool(entry.pred_taken),
            bool(entry.is_rvc),
        )
        if predicted is not None:
            _pred_taken, pred_target = predicted
            if pred_target is not None and int(pred_target) == int(target_pc):
                return False
            if bool(_pred_taken) == bool(golden_taken) and pred_target is None:
                return False
        if bool(target_visible_immediately):
            return False
        if self._has_active_wrong_path_episode():
            return False
        self._begin_active_wrong_path_episode_from_queue_cfi(
            queue_index=int(queue_index),
            entry=entry,
            target_pc=int(target_pc),
            queue_redirect=bool(queue_redirect),
        )
        return True

    def _derive_wrong_path_redirect(
        self,
        *,
        queue_index: int,
        queue_entry: QueueInstr,
    ) -> tuple[Optional[dict], Optional[int], Optional[int]]:
        episode = self._active_wrong_path_episode()
        redirect_queue_index: Optional[int] = None
        redirect_entry: Optional[QueueInstr] = None
        redirect_context: Optional[dict] = None
        redirect_target_pc = None if episode is None else episode["target_pc"]
        if redirect_target_pc is None:
            redirect_target_pc = self.current_golden_pc()
        if episode is not None and episode["redirect_context"] is not None:
            redirect_context = dict(episode["redirect_context"])
        else:
            prev_cfi = self._find_preceding_correct_path_cfi(int(queue_index))
            if prev_cfi is not None:
                prev_idx, prev_entry = prev_cfi
                if (
                    not bool(queue_entry.is_cfi)
                    or (
                        redirect_target_pc is not None
                        and self._queue_entry_predicted_target_mismatches(prev_entry, int(redirect_target_pc))
                    )
                ):
                    redirect_queue_index = int(prev_idx)
                    redirect_entry = prev_entry
                    if prev_entry.golden_target_pc is not None:
                        redirect_target_pc = int(prev_entry.golden_target_pc)
            if redirect_entry is None and bool(queue_entry.is_cfi):
                redirect_queue_index = int(queue_index)
                redirect_entry = queue_entry
            if redirect_entry is not None:
                redirect_context = self._build_redirect_context_from_queue_entry(
                    redirect_queue_index,
                    redirect_entry,
                )
            elif self._last_correct_cfi_context is not None and (
                self._recovery_phase_active()
                or not self._has_wrong_path_barrier_before(int(queue_index))
            ):
                if self._redirect_context_ftq_is_stale_for_drive(self._last_correct_cfi_context):
                    self._last_correct_cfi_context = None
                    return None, None, None
                redirect_context = self._remap_redirect_context_queue_index(
                    self._last_correct_cfi_context,
                    reason="derive_wrong_path_redirect",
                )
                if redirect_context.get("golden_target_pc") is not None:
                    redirect_target_pc = int(redirect_context["golden_target_pc"])
        return redirect_context, (
            None if redirect_target_pc is None else int(redirect_target_pc)
        ), redirect_queue_index

    def _begin_active_wrong_path_episode_for_first_mismatch(
        self,
        *,
        queue_index: int,
        queue_entry: QueueInstr,
        queue_redirect: bool,
    ) -> bool:
        redirect_context, redirect_target_pc, _redirect_queue_index = self._derive_wrong_path_redirect(
            queue_index=int(queue_index),
            queue_entry=queue_entry,
        )
        if redirect_context is not None and redirect_target_pc is not None:
            self._begin_active_wrong_path_episode_from_redirect_context(
                origin_index=int(queue_index),
                target_pc=int(redirect_target_pc),
                redirect_context=redirect_context,
                queue_redirect=bool(queue_redirect),
            )
            return True
        if not bool(queue_redirect):
            self._begin_active_wrong_path_episode(
                origin_index=int(queue_index),
                target_pc=None if redirect_target_pc is None else int(redirect_target_pc),
                redirect_context=None,
                queue_redirect=False,
            )
            return True
        return False

    def _cfvec_queue_note_mismatch(self, queue_index: int, *, queue_redirect: bool = True) -> None:
        if queue_index < 0 or queue_index >= len(self._cfvec_queue):
            return
        queue_entry = self._cfvec_queue[queue_index]
        queue_entry.golden_match_state = GOLDEN_MATCH_STATE_MISMATCHED
        if not self._has_active_wrong_path_episode():
            queue_entry.path_state = PATH_STATE_WRONG
            self._assert_queue_index_not_in_commit_queue(
                int(queue_index),
                reason="first_mismatch",
            )
            queued_redirect = self._begin_active_wrong_path_episode_for_first_mismatch(
                queue_index=int(queue_index),
                queue_entry=queue_entry,
                queue_redirect=bool(queue_redirect),
            )
            if not bool(queue_redirect) and queued_redirect:
                return
            if not queued_redirect:
                mismatch_target_pc = self.current_golden_pc()
                raise AssertionError(
                    "first mismatch has no attributable CFI for redirect; "
                    "treat as env-model or DUT bug: "
                    f"queue_index={int(queue_index)} pc=0x{int(queue_entry.pc):x} "
                    f"target_pc={None if mismatch_target_pc is None else hex(int(mismatch_target_pc))}"
                )
            return
        origin_index = self._active_wrong_path_origin_index()
        if origin_index is not None and int(queue_index) >= int(origin_index):
            queue_entry.path_state = PATH_STATE_WRONG
            self._assert_queue_index_not_in_commit_queue(
                int(queue_index),
                reason="active_wrong_path_mismatch",
            )
            if self._recovery_phase_active():
                recovery_target_pc = self._current_recovery_target_pc()
                if (
                    bool(queue_redirect)
                    and recovery_target_pc is not None
                    and int(queue_entry.pc) == int(recovery_target_pc)
                ):
                    redirect_context, redirect_target_pc, _ = self._derive_wrong_path_redirect(
                        queue_index=int(queue_index),
                        queue_entry=queue_entry,
                    )
                    if (
                        redirect_context is not None
                        and redirect_target_pc is not None
                        and int(redirect_target_pc) != int(recovery_target_pc)
                    ):
                        self._refresh_active_wrong_path_redirect_cause(
                            origin_index=int(origin_index),
                            target_pc=int(redirect_target_pc),
                            redirect_context=redirect_context,
                            redirect_driven=False,
                        )
                        self._queue_active_wrong_path_redirect()
                return
            if bool(queue_redirect):
                if self._queue_active_wrong_path_redirect():
                    return
                redirect_context, redirect_target_pc, _ = self._derive_wrong_path_redirect(
                    queue_index=int(queue_index),
                    queue_entry=queue_entry,
                )
                if redirect_context is not None and redirect_target_pc is not None:
                    self._refresh_active_wrong_path_redirect_cause(
                        origin_index=int(origin_index),
                        target_pc=int(redirect_target_pc),
                        redirect_context=redirect_context,
                        redirect_driven=False,
                    )
                    self._queue_active_wrong_path_redirect()

    def _queue_entry_should_enqueue_resolve(self, queue_index: int) -> bool:
        if queue_index < 0 or queue_index >= len(self._cfvec_queue):
            return False
        entry = self._cfvec_queue[int(queue_index)]
        if not entry.is_cfi:
            return False
        if entry.path_state != PATH_STATE_WRONG:
            return True
        episode = self._active_wrong_path_episode()
        if episode is None:
            return False
        redirect_context = episode["redirect_context"]
        if redirect_context is None or redirect_context.get("queue_index") is None:
            return False
        return int(redirect_context["queue_index"]) == int(queue_index)

    def _cfvec_queue_mark_resolve_state(self, queue_index: Optional[int], resolve_state: str) -> None:
        if queue_index is None:
            return
        if queue_index < 0 or queue_index >= len(self._cfvec_queue):
            return
        self._cfvec_queue[queue_index].resolve_state = str(resolve_state)
        self._pending_queue_resolve_indices = deque(
            idx for idx in self._pending_queue_resolve_indices if int(idx) != int(queue_index)
        )

    def _requeue_resolve_for_queue_cfi(self, queue_index: int) -> None:
        if queue_index < 0 or queue_index >= len(self._cfvec_queue):
            return
        if any(entry.queue_index is not None and int(entry.queue_index) == int(queue_index) for entry in self._pending_resolves):
            return
        queue_entry = self._cfvec_queue[int(queue_index)]
        if not queue_entry.is_cfi:
            return
        cfi = self._classify_cfi(
            int(queue_entry.instr),
            int(queue_entry.pc),
            bool(queue_entry.pred_taken),
            bool(queue_entry.is_rvc),
        )
        if cfi is None:
            return
        branch_type, ras_action, target, taken = cfi
        golden_entry = None
        if self.golden_trace is not None and queue_entry.golden_index is not None:
            golden_index = int(queue_entry.golden_index)
            if 0 <= golden_index < len(self.golden_trace.entries):
                golden_entry = self.golden_trace.entries[golden_index]
        golden_cfi = self._golden_cfi_outcome(
            golden_entry,
            int(queue_entry.instr),
            int(queue_entry.pc),
            bool(queue_entry.is_rvc),
        )
        mispredict = False
        if golden_cfi is not None:
            golden_taken, golden_target = golden_cfi
            taken = bool(golden_taken)
            if golden_target is not None:
                target = int(golden_target)
            pred_cfi = self._predicted_cfi_outcome(
                int(queue_entry.instr),
                int(queue_entry.pc),
                bool(queue_entry.pred_taken),
                bool(queue_entry.is_rvc),
            )
            if pred_cfi is not None:
                pred_taken_out, pred_target = pred_cfi
                if bool(pred_taken_out) != bool(golden_taken):
                    mispredict = True
                elif bool(golden_taken) and golden_target is not None and pred_target is None:
                    mispredict = True
                elif bool(golden_taken) and pred_target is not None and golden_target is not None:
                    mispredict = int(pred_target) != int(golden_target)

        ftq_key = (int(queue_entry.ftq_flag) << 6) | (int(queue_entry.ftq_value) & 0x3F)
        start_pc = self._ftq_start_pc_cache.get(
            ftq_key,
            self._ftq_start_pc_by_value.get(
                int(queue_entry.ftq_value),
                (
                    int(queue_entry.pc)
                    - int(queue_entry.ftq_offset) * 2
                    + (0 if bool(queue_entry.is_rvc) else 2)
                ) & 0xFFFFFFFFFFFFFFFF,
            ),
        )
        delay = self._rng.randint(self.resolve_min_delay, self.resolve_max_delay)
        self._pending_resolves.append(
            ResolveEntry(
                ready_cycle=int(self.current_cycle) + int(delay),
                inst_pc=int(queue_entry.pc),
                pc=int(start_pc),
                target=int(target),
                taken=bool(taken),
                mispredict=bool(mispredict),
                ftq_flag=int(queue_entry.ftq_flag),
                ftq_value=int(queue_entry.ftq_value),
                ftq_offset=int(queue_entry.ftq_offset),
                branch_type=int(branch_type),
                ras_action=int(ras_action),
                queued_cycle=int(self.current_cycle),
                is_rvc=bool(queue_entry.is_rvc),
                queue_index=int(queue_index),
            )
        )
        self._recompute_cfi_budgets_from_pending_resolves()

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
        self._assert_queue_index_not_in_commit_queue(
            int(queue_index),
            reason="recovery_residual",
        )
        if queue_entry.is_cfi:
            self._cfvec_queue_mark_resolve_state(int(queue_index), RESOLVE_STATE_SKIPPED)

    def _coalesce_unknown_suffix_into_wrong_path(self, *, queue_redirect: bool) -> None:
        first_non_correct = self._first_non_correct_queue_index()
        if first_non_correct is None:
            return
        if not self._has_active_wrong_path_episode():
            self._begin_active_wrong_path_episode(
                origin_index=int(first_non_correct),
                target_pc=self._current_recovery_target_pc(),
                redirect_context=self._last_correct_cfi_context,
                queue_redirect=False,
            )
        episode = self._active_wrong_path_episode()
        start_idx = int(episode["origin_index"])
        for idx in range(int(start_idx), len(self._cfvec_queue)):
            entry = self._cfvec_queue[idx]
            if entry.path_state == PATH_STATE_CORRECT:
                continue
            entry.path_state = PATH_STATE_WRONG
            if entry.golden_match_state == GOLDEN_MATCH_STATE_UNKNOWN:
                entry.golden_match_state = GOLDEN_MATCH_STATE_MISMATCHED
            self._assert_queue_index_not_in_commit_queue(
                int(idx),
                reason="coalesce_unknown_suffix",
            )
            if not bool(queue_redirect) and entry.is_cfi and entry.resolve_state == RESOLVE_STATE_NOT_NEEDED:
                continue

    def _first_non_correct_queue_index(self) -> Optional[int]:
        for idx, entry in enumerate(self._cfvec_queue):
            if entry.path_state != PATH_STATE_CORRECT:
                return int(idx)
        return None

    def _cfvec_queue_recompute_redirect_origin(self) -> None:
        # redirect origin tracks an active mismatch episode that is expected to
        # be recovered by a queued redirect event. Do not infer a new origin
        # from residual wrong-path entries when there is no pending redirect.
        start_idx = self._first_non_correct_queue_index()
        if not any(evt.kind == "redirect" for evt in self.pending_events):
            if self._active_wrong_path_in_recovery():
                self._remap_active_wrong_path_episode(
                    new_origin_index=(
                        len(self._cfvec_queue)
                        if start_idx is None
                        else int(start_idx)
                    ),
                    reason="recompute_redirect_origin",
                )
                return
            self._clear_active_wrong_path_episode()
            return
        if start_idx is None:
            if self._has_active_wrong_path_episode():
                self._remap_active_wrong_path_episode(
                    new_origin_index=len(self._cfvec_queue),
                    reason="recompute_redirect_origin",
                )
                return
            self._clear_active_wrong_path_episode()
            return
        for idx, entry in enumerate(self._cfvec_queue):
            if int(idx) < int(start_idx):
                continue
            if entry.path_state == PATH_STATE_WRONG:
                self._remap_active_wrong_path_episode(
                    new_origin_index=int(idx),
                    reason="recompute_redirect_origin",
                )
                return
        self._remap_active_wrong_path_episode(
            new_origin_index=int(start_idx),
            reason="recompute_redirect_origin",
        )

    @staticmethod
    def _format_queue_pc_ranges(entries: list[QueueInstr]) -> str:
        if not entries:
            return "none"
        ranges: list[str] = []
        range_start = int(entries[0].pc)
        range_end = int(entries[0].pc)
        for entry in entries[1:]:
            pc = int(entry.pc)
            if pc == range_end or pc == range_end + 2 or pc == range_end + 4:
                range_end = pc
                continue
            ranges.append(
                f"0x{range_start:x}" if range_start == range_end else f"0x{range_start:x}-0x{range_end:x}"
            )
            range_start = pc
            range_end = pc
        ranges.append(
            f"0x{range_start:x}" if range_start == range_end else f"0x{range_start:x}-0x{range_end:x}"
        )
        return ",".join(ranges)

    def _cfvec_queue_flush_wrong_path(
        self,
        *,
        keep_open_ftqs: Optional[set[tuple[int, int]]] = None,
    ) -> Optional[dict]:
        state = self._sync_backend_state()
        episode = self._active_wrong_path_episode()
        origin_index = None if episode is None else episode["origin_index"]
        first_wrong_index = None
        for idx, entry in enumerate(state.cfvec_queue):
            if entry.path_state == PATH_STATE_WRONG:
                first_wrong_index = int(idx)
                break
        if first_wrong_index is not None:
            if origin_index is None:
                origin_index = int(first_wrong_index)
            else:
                origin_index = min(int(origin_index), int(first_wrong_index))
        if origin_index is None:
            return None
        remove_start = max(0, int(origin_index))
        remove_stop = len(state.cfvec_queue)
        removed_entries = list(state.cfvec_queue)[remove_start:remove_stop]
        summary = {
            "removed_count": len(removed_entries),
            "queue_index_start": remove_start if removed_entries else -1,
            "queue_index_end": remove_start + len(removed_entries) - 1 if removed_entries else -1,
            "observed_first_pc": int(removed_entries[0].pc) if removed_entries else None,
            "observed_last_pc": int(removed_entries[-1].pc) if removed_entries else None,
            "pc_ranges": self._format_queue_pc_ranges(removed_entries),
        }
        kept = [
            entry
            for idx, entry in enumerate(state.cfvec_queue)
            if int(idx) < int(remove_start) or int(idx) >= int(remove_stop)
        ]
        state.cfvec_queue = deque(kept)
        def _kept_index(old_idx: int) -> Optional[int]:
            if int(old_idx) < int(remove_start):
                return int(old_idx)
            if int(old_idx) >= int(remove_stop):
                return int(old_idx) - (int(remove_stop) - int(remove_start))
            return None
        state.pending_resolves = deque(
            (
                entry
                if entry.queue_index is None
                else replace(entry, queue_index=_kept_index(int(entry.queue_index)))
            )
            for entry in state.pending_resolves
            if entry.queue_index is None or _kept_index(int(entry.queue_index)) is not None
        )
        state.pending_queue_resolve_indices = deque(
            int(new_idx)
            for idx in state.pending_queue_resolve_indices
            if (new_idx := _kept_index(int(idx))) is not None
        )
        state.pending_queue_call_ret_commit_indices = deque(
            int(new_idx)
            for idx in state.pending_queue_call_ret_commit_indices
            if (new_idx := _kept_index(int(idx))) is not None
        )
        state.commit_queue = deque(
            int(new_idx)
            for idx in state.commit_queue
            if (new_idx := _kept_index(int(idx))) is not None
        )
        state.scheduled_queue_call_ret_commit_groups = deque(
            (int(ready_cycle), kept_group)
            for ready_cycle, group in state.scheduled_queue_call_ret_commit_groups
            if (
                kept_group := [
                    (
                        inst
                        if getattr(inst, "queue_index", None) is None
                        else replace(inst, queue_index=_kept_index(int(getattr(inst, "queue_index"))))
                    )
                    for inst in group
                    if getattr(inst, "queue_index", None) is None or _kept_index(int(getattr(inst, "queue_index"))) is not None
                ]
            )
        )
        state.visible_queue_call_ret_commit_group = [
            replace(inst, queue_index=_kept_index(int(getattr(inst, "queue_index"))))
            if getattr(inst, "queue_index", None) is not None
            else inst
            for inst in state.visible_queue_call_ret_commit_group
            if getattr(inst, "queue_index", None) is None or _kept_index(int(getattr(inst, "queue_index"))) is not None
        ]
        last_correct_queue_index_map = {
            int(old_idx): int(new_idx)
            for old_idx in range(len(state.cfvec_queue) + len(removed_entries))
            if (new_idx := _kept_index(int(old_idx))) is not None
        }
        self._apply_backend_state()
        self._remap_last_correct_cfi_context_after_queue_edit(
            last_correct_queue_index_map,
            reason="cfvec_queue_flush_wrong_path",
        )
        removed_ftqs = {
            (int(entry.ftq_flag), int(entry.ftq_value))
            for entry in removed_entries
        }
        if keep_open_ftqs:
            removed_ftqs -= {
                (int(ftq_flag), int(ftq_value))
                for ftq_flag, ftq_value in keep_open_ftqs
            }
        self._cfvec_queue_close_surviving_ftq_spans(removed_ftqs)
        self._redirect_flush_ftq_entries(removed_entries)
        if episode is not None:
            self._set_active_wrong_path_episode(
                origin_index=len(self._cfvec_queue),
                target_pc=episode["target_pc"],
                redirect_context=(
                    self._detach_redirect_context_from_queue(episode["redirect_context"])
                    if bool(episode["redirect_driven"])
                    else episode["redirect_context"]
                ),
                redirect_driven=bool(episode["redirect_driven"]),
                expected_recovery_ftq=episode["expected_recovery_ftq"],
            )
        else:
            self._clear_active_wrong_path_episode()
        return summary

    def _cfvec_queue_pop_head(self, count: int) -> None:
        pop_count = max(0, int(count))
        if pop_count <= 0:
            return
        prior_episode = self._active_wrong_path_episode()
        queue_index_map = {
            int(old_idx): int(old_idx) - pop_count
            for old_idx in range(pop_count, len(self._cfvec_queue))
        }
        for _ in range(pop_count):
            if not self._cfvec_queue:
                break
            self._cfvec_queue.popleft()
        self._pending_resolves = deque(
            replace(entry, queue_index=(None if entry.queue_index is None else int(entry.queue_index) - pop_count))
            for entry in self._pending_resolves
            if entry.queue_index is None or int(entry.queue_index) >= pop_count
        )
        episode = self._active_wrong_path_episode()
        origin_index = None if episode is None else episode["origin_index"]
        remapped_origin_index = (
            None
            if origin_index is None or int(origin_index) < pop_count
            else int(origin_index) - pop_count
        )
        self._remap_active_wrong_path_episode(
            new_origin_index=remapped_origin_index,
            queue_index_map=queue_index_map,
            reason="cfvec_queue_pop_head",
        )
        self._remap_last_correct_cfi_context_after_queue_edit(
            queue_index_map,
            reason="cfvec_queue_pop_head",
        )
        self._cfvec_queue_recompute_redirect_origin()
        self._restore_active_wrong_path_episode_after_queue_edit(prior_episode)
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
        prior_episode = self._active_wrong_path_episode()
        removed_entries = list(self._cfvec_queue)[remove_start:remove_stop]
        removed_ftqs = {
            (int(entry.ftq_flag), int(entry.ftq_value))
            for entry in removed_entries
        }
        kept_queue: list[QueueInstr] = []
        queue_index_map: dict[int, int] = {}
        for old_idx, entry in enumerate(self._cfvec_queue):
            if remove_start <= int(old_idx) < remove_stop:
                continue
            queue_index_map[int(old_idx)] = len(kept_queue)
            kept_queue.append(entry)
        self._cfvec_queue = deque(kept_queue)
        episode = self._active_wrong_path_episode()
        origin_index = None if episode is None else episode["origin_index"]
        remapped_origin_index = None
        if origin_index is not None:
            remapped_origin_index = queue_index_map.get(int(origin_index))
        self._remap_active_wrong_path_episode(
            new_origin_index=remapped_origin_index,
            queue_index_map=queue_index_map,
            reason="cfvec_queue_remove_range",
        )
        self._remap_last_correct_cfi_context_after_queue_edit(
            queue_index_map,
            reason="cfvec_queue_remove_range",
        )
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
        self._cfvec_queue_close_surviving_ftq_spans(removed_ftqs)
        self._cfvec_queue_recompute_redirect_origin()
        self._restore_active_wrong_path_episode_after_queue_edit(prior_episode)

    def _redirect_flush_ftq_entries(self, removed_entries: list[QueueInstr]) -> None:
        if not removed_entries:
            return
        removed_ftqs = {
            (int(entry.ftq_flag), int(entry.ftq_value))
            for entry in removed_entries
        }
        surviving_ftqs = {
            (int(entry.ftq_flag), int(entry.ftq_value))
            for entry in self._cfvec_queue
        }
        flushed_ftqs = removed_ftqs - surviving_ftqs
        flushed_ftqs.update(
            (int(entry.ftq_flag), int(entry.ftq_value))
            for entry in self.ftq_entries
            if (int(entry.ftq_flag), int(entry.ftq_value)) not in surviving_ftqs
        )
        if not flushed_ftqs:
            return
        self.ftq_entries = deque(
            entry
            for entry in self.ftq_entries
            if (int(entry.ftq_flag), int(entry.ftq_value)) not in flushed_ftqs
        )
        if (
            self._current_ftq_entry is not None
            and (
                int(self._current_ftq_entry.ftq_flag),
                int(self._current_ftq_entry.ftq_value),
            ) in flushed_ftqs
        ):
            self._current_ftq_entry = None
            self._current_ftq_seen_packets.clear()
            self._current_ftq_max_offset = -1

    def _apply_recovery_if_target_queued(self) -> None:
        target_pc = self._current_recovery_target_pc()
        if target_pc is None:
            return
        target_pc = int(target_pc)
        queue_start = max(
            0,
            min(
                len(self._cfvec_queue),
                int(self._current_recovery_queue_start()),
            ),
        )
        target_idx = next(
            (
                idx
                for idx, entry in enumerate(self._cfvec_queue)
                if (
                    int(idx) >= int(queue_start)
                    and self._queue_entry_matches_recovery_target(entry, int(target_pc))
                )
            ),
            None,
        )
        if target_idx is None:
            return
        self._last_driven_redirect_signature = None
        target_entry = self._cfvec_queue[int(target_idx)]
        self._recovery_commit_block_ftq = (
            int(target_entry.ftq_flag),
            int(target_entry.ftq_value),
        )
        self._recovery_commit_block_cycle = int(self.current_cycle)
        have_prior_commit = bool(
            int(self.commit_count) > 0
            or int(self.commit_ptr_flag) != 0
            or int(self.commit_ptr_value) != 0
            or bool(self._reuse_commit_ptr_once)
        )
        if have_prior_commit and not bool(self._reuse_commit_ptr_once):
            target_rank = self._ftq_ptr_rank_after_commit(
                int(target_entry.ftq_flag),
                int(target_entry.ftq_value),
            )
            if int(target_rank) == 0 or int(target_rank) >= int(self.ftq_size):
                raise AssertionError(
                    "recovery target selected stale committed FTQ: "
                    f"target_pc=0x{int(target_pc):x} "
                    f"target_ftq=({int(target_entry.ftq_flag)},{int(target_entry.ftq_value)}) "
                    f"commit_ptr=({int(self.commit_ptr_flag)},{int(self.commit_ptr_value)})"
                )
        removed_entries = list(self._cfvec_queue)[int(queue_start):int(target_idx)]
        if int(target_idx) > int(queue_start):
            self._cfvec_queue_remove_range(int(queue_start), int(target_idx))
            self._redirect_flush_ftq_entries(removed_entries)
        # Release the kept suffix from the previous wrong-path episode so it
        # can be replayed against golden from the recovery target onward.
        for idx in range(int(queue_start), len(self._cfvec_queue)):
            entry = self._cfvec_queue[idx]
            if entry.path_state == PATH_STATE_WRONG:
                entry.path_state = PATH_STATE_UNKNOWN
                entry.golden_match_state = GOLDEN_MATCH_STATE_UNKNOWN
                if entry.is_cfi and entry.resolve_state == RESOLVE_STATE_SKIPPED:
                    entry.resolve_state = RESOLVE_STATE_PENDING
                    if int(idx) not in self._pending_queue_resolve_indices:
                        self._pending_queue_resolve_indices.append(int(idx))
                    self._requeue_resolve_for_queue_cfi(int(idx))
        if removed_entries:
            self.logger.info(
                "redirect recovery flush: target=0x%x removed=%d queue_idx=[%d,%d] queue_pc_ranges=%s",
                int(target_pc),
                int(len(removed_entries)),
                int(queue_start),
                int(target_idx) - 1,
                self._format_queue_pc_ranges(removed_entries),
            )
        self._clear_active_wrong_path_episode()
        self._drop_pending_redirects_for_target(int(target_pc))
        self._replay_golden_matches_from_queue_head()
        self._ensure_commit_queue_consistency()

    def _flush_recovery_residuals_if_target_not_queued(self) -> None:
        episode = self._active_wrong_path_episode()
        target_pc = self._current_recovery_target_pc()
        if episode is None or target_pc is None:
            return
        queue_start = max(
            0,
            min(
                len(self._cfvec_queue),
                int(self._current_recovery_queue_start()),
            ),
        )
        if queue_start >= len(self._cfvec_queue):
            return
        target_idx = next(
            (
                idx
                for idx, entry in enumerate(self._cfvec_queue)
                if (
                    int(idx) >= int(queue_start)
                    and self._queue_entry_matches_recovery_target(entry, int(target_pc))
                )
            ),
            None,
        )
        if target_idx is not None:
            return
        remove_start = next(
            (
                idx
                for idx, entry in enumerate(self._cfvec_queue)
                if int(idx) >= int(queue_start) and entry.path_state == PATH_STATE_WRONG
            ),
            None,
        )
        if remove_start is None:
            return
        removed_entries = list(self._cfvec_queue)[int(remove_start):]
        if not removed_entries:
            return
        self._cfvec_queue_remove_range(int(remove_start), len(self._cfvec_queue))
        self._redirect_flush_ftq_entries(removed_entries)
        self._set_active_wrong_path_episode(
            origin_index=min(int(remove_start), len(self._cfvec_queue)),
            target_pc=int(target_pc),
            redirect_context=episode["redirect_context"],
            redirect_driven=True,
            expected_recovery_ftq=episode["expected_recovery_ftq"],
        )
        self.logger.debug(
            "redirect recovery residual flush: target=0x%x removed=%d queue_idx=[%d,%d] queue_pc_ranges=%s",
            int(target_pc),
            int(len(removed_entries)),
            int(remove_start),
            int(remove_start) + int(len(removed_entries)) - 1,
            self._format_queue_pc_ranges(removed_entries),
        )

    def _cfvec_queue_close_current_ftq_span(self, ftq_flag: int, ftq_value: int) -> None:
        for entry in reversed(self._cfvec_queue):
            if int(entry.ftq_flag) != int(ftq_flag) or int(entry.ftq_value) != int(ftq_value):
                continue
            entry.is_last_in_entry = True
            return

    def _cfvec_queue_close_surviving_ftq_spans(
        self,
        ftq_keys: set[tuple[int, int]],
    ) -> None:
        for ftq_flag, ftq_value in ftq_keys:
            if not self._cfvec_queue_has_ftq(int(ftq_flag), int(ftq_value)):
                continue
            self._cfvec_queue_close_current_ftq_span(int(ftq_flag), int(ftq_value))

    def _clear_cfvec_queue_state(self) -> None:
        self._cfvec_queue.clear()
        self._commit_queue.clear()
        self._clear_active_wrong_path_episode()
        self._pending_queue_resolve_indices.clear()
        self._pending_queue_call_ret_commit_indices.clear()
        self._scheduled_queue_call_ret_commit_groups.clear()
        self._visible_queue_call_ret_commit_group = []
        self._last_correct_cfi_context = None

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
        if self.golden_trace is not None and self._cfvec_queue:
            return None
        candidate = self._sync_backend_state().find_next_commitable_entry(
            golden_trace_attached=self.golden_trace is not None,
        )
        self._apply_backend_state()
        return candidate

    def _pending_redirect_blocks_commit(self, ftq_flag: int, ftq_value: int) -> bool:
        candidate_rank = int(self._ftq_ptr_rank_after_commit(int(ftq_flag), int(ftq_value)))
        if candidate_rank <= 0 or candidate_rank >= int(self.ftq_size):
            return False
        for evt in self.pending_events:
            if evt.kind != "redirect":
                continue
            payload = evt.payload
            if "ftq_flag" not in payload or "ftq_value" not in payload:
                continue
            pending_rank = int(
                self._ftq_ptr_rank_after_commit(
                    int(payload.get("ftq_flag", -1)),
                    int(payload.get("ftq_value", -1)),
                )
            )
            if pending_rank < 0 or pending_rank >= int(self.ftq_size):
                continue
            if pending_rank == 0:
                return True
            if pending_rank <= candidate_rank:
                return True
        return False

    def _active_redirect_context_blocks_commit(self, ftq_flag: int, ftq_value: int) -> bool:
        episode = self._active_wrong_path_episode()
        if episode is None or not bool(episode["redirect_driven"]):
            return False
        redirect_context = episode["redirect_context"]
        if redirect_context is None:
            return False
        candidate_rank = int(self._ftq_ptr_rank_after_commit(int(ftq_flag), int(ftq_value)))
        if candidate_rank <= 0 or candidate_rank >= int(self.ftq_size):
            return False
        context_rank = int(
            self._ftq_ptr_rank_after_commit(
                int(redirect_context.get("ftq_flag", -1)),
                int(redirect_context.get("ftq_value", -1)),
            )
        )
        if context_rank < 0 or context_rank >= int(self.ftq_size):
            return False
        if context_rank == 0:
            return True
        return context_rank <= candidate_rank

    def _pending_target_redirect_blocks_commit(self, ftq_flag: int, ftq_value: int) -> bool:
        if (
            self.golden_trace is None
            or not self._recovery_phase_active()
        ):
            return False
        candidate_rank = int(self._ftq_ptr_rank_after_commit(int(ftq_flag), int(ftq_value)))
        pending_rank = self._pending_level0_target_rank()
        if candidate_rank <= 0 or candidate_rank >= int(self.ftq_size):
            return False
        if pending_rank is None:
            return False
        if self._candidate_is_pending_level0_target(int(ftq_flag), int(ftq_value)):
            return True
        return candidate_rank >= int(pending_rank)

    def _mark_ftq_redirect_pending(self, ftq_flag: int, ftq_value: int) -> None:
        if (
            self._current_ftq_entry is not None
            and self._ftq_entry_matches(self._current_ftq_entry, int(ftq_flag), int(ftq_value))
        ):
            self._current_ftq_entry.has_redirect = True
        for entry in self.ftq_entries:
            if not self._ftq_entry_matches(entry, int(ftq_flag), int(ftq_value)):
                continue
            entry.has_redirect = True

    def _clear_ftq_redirect_pending(self, ftq_flag: int, ftq_value: int) -> None:
        if (
            self._current_ftq_entry is not None
            and self._ftq_entry_matches(self._current_ftq_entry, int(ftq_flag), int(ftq_value))
        ):
            self._current_ftq_entry.has_redirect = False
        for entry in self.ftq_entries:
            if not self._ftq_entry_matches(entry, int(ftq_flag), int(ftq_value)):
                continue
            entry.has_redirect = False

    def _seal_current_ftq_entry(self, *, observed_last_in_entry: bool = False) -> None:
        sealed_ftq = (
            None
            if self._current_ftq_entry is None
            else (
                int(self._current_ftq_entry.ftq_flag),
                int(self._current_ftq_entry.ftq_value),
                bool(self._current_ftq_entry.observed_last_in_entry or observed_last_in_entry),
            )
        )
        self._sync_backend_state().seal_current_ftq_entry(
            observed_last_in_entry=bool(observed_last_in_entry)
        )
        self._apply_backend_state()
        if (
            sealed_ftq is not None
            and self._candidate_is_pending_level0_target(int(sealed_ftq[0]), int(sealed_ftq[1]))
            and bool(sealed_ftq[2])
            and self._current_ftq_observed_pending_target_pc
        ):
            self._clear_pending_level0_target_ftq()
        else:
            self._current_ftq_observed_pending_target_pc = False
        self._current_ftq_max_offset = -1

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
        self._planned_commit_apply = None

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
        self._clear_active_wrong_path_episode()
        self._last_correct_cfi_context = None
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

    def set_explicit_injection_enabled(self, enabled: bool, reason: str = "") -> None:
        self._explicit_injection_enabled = bool(enabled)
        self._explicit_injection_block_reason = "" if bool(enabled) else str(reason)

    def _assert_explicit_injection_allowed(self, kind: str) -> None:
        if self._explicit_injection_enabled:
            return
        reason = str(self._explicit_injection_block_reason or "explicit injection disabled")
        raise AssertionError(f"{str(kind)} injection is disabled: {reason}")

    def _emit_event(self, kind: str, payload: dict) -> None:
        item = {"cycle": self.current_cycle, "kind": kind, **payload}
        self.last_events.append(item)
        self._publish(f"backend.{kind}", item, level="DEBUG")

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
        if delay_cycles is None:
            effective_delay = self._sample_redirect_delay()
        else:
            effective_delay = 0 if int(delay_cycles) <= 0 else self._clamp_backend_delay(delay_cycles)
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
        driven_signature = self._last_driven_redirect_signature
        if driven_signature is not None:
            candidate_signature = (
                int(target_pc),
                int(payload.get("pc", -1)),
                int(payload.get("ftq_flag", -1)),
                int(payload.get("ftq_value", -1)),
                int(payload.get("ftq_offset", -1)),
                int(payload.get("is_rvc", -1)),
            )
            if candidate_signature == driven_signature:
                if self._recovery_phase_active():
                    keep_open_ftqs = None
                    if "ftq_flag" in payload and "ftq_value" in payload:
                        keep_open_ftqs = {
                            (int(payload["ftq_flag"]), int(payload["ftq_value"]))
                        }
                    self._cfvec_queue_flush_wrong_path(keep_open_ftqs=keep_open_ftqs)
                return
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
        committed = 0
        for queue_index in self._queue_instruction_commit_candidate_indices():
            queue_entry = self._cfvec_queue[int(queue_index)]
            self._record_instruction_commit(int(queue_index), int(queue_entry.instr))
            committed += 1
        self._ensure_commit_queue_consistency()
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
        if bool(is_rvc) and self._is_compressed_indirect_jump(int(instr)):
            return bool(pred_taken), None
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
        if bool(is_rvc) and self._is_compressed_indirect_jump(int(instr)):
            if entry.target_pc is None:
                return True, None
            return True, int(entry.target_pc) & 0xFFFFFFFFFFFFFFFF
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
        if bool(is_rvc) and self._is_compressed_indirect_jump(int(instr)):
            return 3, self._compressed_indirect_ras_action(int(instr)), 0, True
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

    @staticmethod
    def _is_compressed_indirect_jump(instr: int) -> bool:
        if (int(instr) & 0x3) != 0x2:
            return False
        funct4 = (int(instr) >> 12) & 0xF
        rs1 = (int(instr) >> 7) & 0x1F
        rs2 = (int(instr) >> 2) & 0x1F
        return funct4 in (0x8, 0x9) and rs1 != 0 and rs2 == 0

    @staticmethod
    def _compressed_indirect_ras_action(instr: int) -> int:
        funct4 = (int(instr) >> 12) & 0xF
        rs1 = (int(instr) >> 7) & 0x1F
        if funct4 == 0x8:
            return 1 if rs1 in (1, 5) else 0
        return 3 if rs1 in (1, 5) else 2

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

    def _first_later_cfvec_slot_pc(self, current_slot: int) -> Optional[int]:
        assert self.observe_if is not None
        for slot in range(int(current_slot) + 1, 8):
            if self._read(self.observe_if.cfvec_valid[slot], 0) != 1:
                continue
            return int(self._read(self.observe_if.cfvec_pc[slot], 0))
        return None

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
        if "mismatch" in str(payload.get("reason", "")):
            return None
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

    def _redirect_drive_context_for_ftq_group(self, ftq_flag: int, ftq_value: int) -> Optional[dict]:
        group = (int(ftq_flag), int(ftq_value))
        history = self._ftq_group_pc_history.get(group)
        start_pc = self._observed_ftq_start_pc(int(ftq_flag), int(ftq_value))
        if not history or start_pc is None:
            return None
        last_pc, last_is_rvc = history[-1]
        last_pc = int(last_pc)
        last_is_rvc = bool(last_is_rvc)
        drive_ftq_offset = (
            (last_pc - int(start_pc) + (0 if last_is_rvc else 2)) // 2
        ) & 0x1F
        return {
            "ftq_flag": int(ftq_flag),
            "ftq_value": int(ftq_value),
            "pc": int(start_pc),
            "ftq_offset": int(drive_ftq_offset),
            "is_rvc": int(last_is_rvc),
        }

    def _latest_non_stale_redirect_drive_context(self) -> Optional[dict]:
        candidates: list[tuple[int, int]] = []
        for entry in reversed(self._cfvec_queue):
            if entry.path_state == PATH_STATE_WRONG:
                continue
            candidates.append((int(entry.ftq_flag), int(entry.ftq_value)))
        if self._current_ftq_entry is not None:
            candidates.append((int(self._current_ftq_entry.ftq_flag), int(self._current_ftq_entry.ftq_value)))
        candidates.extend((int(entry.ftq_flag), int(entry.ftq_value)) for entry in reversed(self.ftq_entries))

        seen: set[tuple[int, int]] = set()
        for ftq_flag, ftq_value in candidates:
            group = (int(ftq_flag), int(ftq_value))
            if group in seen:
                continue
            seen.add(group)
            if self._redirect_drive_ftq_is_stale_relative_to_commit(int(ftq_flag), int(ftq_value)):
                continue
            context = self._redirect_drive_context_for_ftq_group(int(ftq_flag), int(ftq_value))
            if context is not None:
                return context
        return None

    def _assert_redirect_drive_ftq_not_stale(
        self,
        *,
        payload_ftq_flag: int,
        payload_ftq_value: int,
        drive_ftq_flag: int,
        drive_ftq_value: int,
        target_pc: int,
        reason: str,
    ) -> None:
        if (int(drive_ftq_flag), int(drive_ftq_value)) != (int(payload_ftq_flag), int(payload_ftq_value)):
            return
        if not self._redirect_drive_ftq_is_stale_relative_to_commit(int(drive_ftq_flag), int(drive_ftq_value)):
            return
        raise AssertionError(
            "redirect drive selected stale ftq context: "
            f"reason={str(reason)} "
            f"payload_ftq=({int(payload_ftq_flag)},{int(payload_ftq_value)}) "
            f"drive_ftq=({int(drive_ftq_flag)},{int(drive_ftq_value)}) "
            f"commit_ptr=({int(self.commit_ptr_flag)},{int(self.commit_ptr_value)}) "
            f"target_pc=0x{int(target_pc):x}"
        )

    def _sample_cfvec(self) -> None:
        assert self.observe_if is not None
        recovery_target_seen_this_cycle = False

        def _ensure_current_ftq_entry(ftq_flag: int, ftq_value: int) -> None:
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
                self._current_ftq_entry = FtqEntry(ftq_flag=ftq_flag, ftq_value=ftq_value)
                self._current_ftq_seen_packets.clear()
                self._current_ftq_max_offset = -1
                self._current_ftq_observed_pending_target_pc = False

        def _packet_key(
            pc: int,
            instr: int,
            is_rvc: bool,
            pred_taken: bool,
            ftq_offset: int,
        ) -> tuple[int, int, int, int, int]:
            return (
                int(pc),
                int(instr),
                1 if bool(is_rvc) else 0,
                int(ftq_offset),
                1 if bool(pred_taken) else 0,
            )

        for i in range(8):
            if self._read(self.observe_if.cfvec_valid[i], 0) != 1:
                continue
            pc = self._read(self.observe_if.cfvec_pc[i], 0)
            instr = self._read(self.observe_if.cfvec_instr[i], 0)
            is_rvc = bool(self._read(self.observe_if.cfvec_is_rvc[i], 0))
            pred_taken = bool(self._read(self.observe_if.cfvec_fixed_taken[i], 0))
            ftq_flag = self._read(self.observe_if.cfvec_ftq_ptr_flag[i], 0)
            ftq_value = self._read(self.observe_if.cfvec_ftq_ptr_value[i], 0)
            ftq_offset = self._read(self.observe_if.cfvec_ftq_offset[i], 0)
            is_last = bool(self._read(self.observe_if.cfvec_is_last_in_ftq_entry[i], 0))

            _ensure_current_ftq_entry(int(ftq_flag), int(ftq_value))
            packet_key = _packet_key(
                int(pc),
                int(instr),
                bool(is_rvc),
                bool(pred_taken),
                int(ftq_offset),
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

            episode = self._active_wrong_path_episode()
            recovery_target_pc = self._current_recovery_target_pc()
            hit_recovery_target = False
            expected_golden = self.current_golden_pc()
            golden_entry = None

            if (
                recovery_target_pc is not None
                and episode is not None
                and bool(episode["redirect_driven"])
                and not recovery_target_seen_this_cycle
            ):
                if self._queue_entry_matches_recovery_target(
                    self._cfvec_queue[int(queue_index)],
                    int(recovery_target_pc),
                ):
                    recovery_target_seen_this_cycle = True
                    hit_recovery_target = True
                else:
                    self._cfvec_queue_mark_recovery_residual(int(queue_index))
            elif episode is None:
                golden_entry = self._consume_golden_entry(int(pc))
            if golden_entry is not None:
                self._cfvec_queue_mark_matched(int(queue_index), golden_entry)
            elif expected_golden is not None or episode is not None:
                self._cfvec_queue_note_mismatch(int(queue_index))
            self._current_ftq_max_offset = max(int(self._current_ftq_max_offset), int(ftq_offset))
            self._record_ftq_group_pc(int(ftq_flag), int(ftq_value), int(pc), bool(is_rvc))
            self._observe_pending_level0_target_packet(
                ftq_flag=int(ftq_flag),
                ftq_value=int(ftq_value),
                pc=int(pc),
                is_last=bool(is_last),
            )

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
                if not self._queue_entry_should_enqueue_resolve(int(queue_index)):
                    self._cfvec_queue_mark_resolve_state(int(queue_index), RESOLVE_STATE_SKIPPED)
                else:
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
                    if self._cfvec_queue[int(queue_index)].path_state == PATH_STATE_CORRECT:
                        self._last_correct_cfi_context = {
                            "pc": int(pc),
                            "instr": int(instr),
                            "is_rvc": int(is_rvc),
                            "pred_taken": int(pred_taken),
                            "ftq_flag": int(ftq_flag),
                            "ftq_value": int(ftq_value),
                            "ftq_offset": int(ftq_offset),
                            "branch_type": int(branch_type),
                            "ras_action": int(ras_action),
                            "queue_index": int(queue_index),
                            "golden_target_pc": (
                                None
                                if self._cfvec_queue[int(queue_index)].golden_target_pc is None
                                else int(self._cfvec_queue[int(queue_index)].golden_target_pc)
                            ),
                        }

            if is_last:
                self._seal_current_ftq_entry(observed_last_in_entry=True)

            if hit_recovery_target:
                self._apply_recovery_if_target_queued()

            if golden_entry is None or self.golden_trace is None:
                continue
            next_entry = self.golden_trace.peek()
            if next_entry is None:
                continue
            if not self._maybe_begin_active_wrong_path_after_correct_cfi(
                queue_index=int(queue_index),
                entry=self._cfvec_queue[int(queue_index)],
                target_pc=int(next_entry.pc),
                target_visible_immediately=self._has_later_cfvec_slot_matching_pc(i, int(next_entry.pc)),
            ):
                continue
            if self._current_ftq_entry is not None:
                self._cfvec_queue_close_current_ftq_span(
                    int(self._current_ftq_entry.ftq_flag),
                    int(self._current_ftq_entry.ftq_value),
                )
                self._seal_current_ftq_entry()
            continue

        if self._has_active_wrong_path_episode() or self._recovery_phase_active():
            self._coalesce_unknown_suffix_into_wrong_path(
                queue_redirect=not self._active_wrong_path_in_recovery()
            )
            self._queue_active_wrong_path_redirect()

        self._flush_recovery_residuals_if_target_not_queued()
        self._apply_recovery_if_target_queued()

    def _ready_resolves_for_cycle(self) -> tuple[ResolveEntry, ...]:
        ready_entries: list[ResolveEntry] = []
        to_remove = []
        frontier_pc = (
            self._current_recovery_target_pc()
            if self._recovery_phase_active()
            else self.current_golden_pc()
        )
        if frontier_pc is None:
            frontier_pc = self.current_golden_pc()
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
            ready_entries.append(replace(entry, mispredict=bool(effective_mispredict)))
            entry_flushes_itself = False
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

    def _apply_planned_commit_entry(self) -> None:
        plan = self._planned_commit_apply
        if plan is None:
            return
        self._planned_commit_apply = None

        mode = str(plan.get("mode", ""))
        if mode == "queue":
            ftq_flag = int(plan["ftq_flag"])
            ftq_value = int(plan["ftq_value"])
            span_len = int(plan["span_len"])
            pending_target_rank = self._pending_level0_target_rank()
            self._schedule_next_queue_call_ret_commit_group()
            self._cfvec_queue_pop_head(int(span_len))
            committed_rank = self._ftq_ptr_rank_after_commit(int(ftq_flag), int(ftq_value))
            self.commit_ptr_flag = int(ftq_flag)
            self.commit_ptr_value = int(ftq_value)
            if pending_target_rank is not None and int(pending_target_rank) <= int(committed_rank):
                self._clear_pending_level0_target_ftq_if_reached(int(ftq_flag), int(ftq_value))
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
            return

        if mode == "fallback":
            head = plan.get("entry")
            if head is None:
                return
            head = FtqEntry(
                ftq_flag=int(head.ftq_flag),
                ftq_value=int(head.ftq_value),
                total_cfi=int(getattr(head, "total_cfi", 0)),
                resolved_cfi=int(getattr(head, "resolved_cfi", 0)),
                dispatch_complete=bool(getattr(head, "dispatch_complete", False)),
                observed_last_in_entry=bool(getattr(head, "observed_last_in_entry", False)),
                has_redirect=bool(getattr(head, "has_redirect", False)),
                commit_ready_cycle=int(getattr(head, "commit_ready_cycle", 0)),
            )
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
                and (
                    int(head.ftq_flag),
                    int(head.ftq_value),
                ) == (
                    int(state.pending_level0_target_ftq[0]),
                    int(state.pending_level0_target_ftq[1]),
                )
            ):
                state.pending_level0_target_ftq = None
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

    def commit_entry_driven(self, entry: Optional[FtqEntry]) -> None:
        if entry is None:
            self._planned_commit_apply = None
            return
        self._apply_planned_commit_entry()

    def _plan_commit_entry_for_cycle(self, *, apply: bool = True) -> Optional[FtqEntry]:
        if self.can_accept == 0:
            return None
        self._planned_commit_apply = None
        self._ensure_commit_queue_consistency()
        if self.golden_trace is not None and self._cfvec_queue:
            head_span = self._queue_head_ftq_commit_span()
            if head_span is None:
                return None
            (ftq_flag, ftq_value), span_len = head_span
            queue_head_entries = list(self._cfvec_queue)[: int(span_len)]
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
                        "cfvec queue head falls behind commit_ptr; "
                        f"head=({int(ftq_flag)},{int(ftq_value)}) "
                        f"commit_ptr=({int(state.commit_ptr_flag)},{int(state.commit_ptr_value)})"
                    )
                expected_ftq = (
                    (int(state.commit_ptr_flag), int(state.commit_ptr_value))
                    if bool(state.reuse_commit_ptr_once)
                    else state.increment_ftq_ptr(int(state.commit_ptr_flag), int(state.commit_ptr_value))
                )
            if self._pending_target_redirect_blocks_commit(int(ftq_flag), int(ftq_value)):
                return None
            if self._recovery_commit_block_matches(int(ftq_flag), int(ftq_value)):
                return None
            if self._active_redirect_context_blocks_commit(int(ftq_flag), int(ftq_value)):
                return None
            if self._pending_redirect_blocks_commit(int(ftq_flag), int(ftq_value)):
                return None
            if self._has_pending_redirect_for_ftq(int(ftq_flag), int(ftq_value)):
                return None
            pending_target_rank = self._pending_level0_target_rank()
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
                        before_queue_index=0,
                        before_ftq=(int(ftq_flag), int(ftq_value)),
                    ):
                        return None
                    candidate_ftq = state.increment_ftq_ptr(int(candidate_ftq[0]), int(candidate_ftq[1]))
                if candidate_ftq != (int(ftq_flag), int(ftq_value)):
                    return None
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
                        raise AssertionError(
                            "older ftq entry disappeared before commit without redirect/commit: "
                            f"ftq=({int(entry.ftq_flag)},{int(entry.ftq_value)})"
                        )
                    retained_entries.append(entry)
                self.ftq_entries = retained_entries
            self._planned_commit_apply = {
                "mode": "queue",
                "ftq_flag": int(ftq_flag),
                "ftq_value": int(ftq_value),
                "span_len": int(span_len),
            }
            if bool(apply):
                self._apply_planned_commit_entry()
            return committed_entry
        if self._semantic_fallback_commit_blocked():
            return None
        self._assert_no_stale_ftq_entries_behind_commit_ptr()
        head = self._find_next_commitable_entry()
        if head is None:
            return None
        if self._pending_target_redirect_blocks_commit(int(head.ftq_flag), int(head.ftq_value)):
            return None
        if self._recovery_commit_block_matches(int(head.ftq_flag), int(head.ftq_value)):
            return None
        if self._active_redirect_context_blocks_commit(int(head.ftq_flag), int(head.ftq_value)):
            return None
        if self._pending_redirect_blocks_commit(int(head.ftq_flag), int(head.ftq_value)):
            return None
        if self._has_pending_redirect_for_ftq(int(head.ftq_flag), int(head.ftq_value)):
            return None
        self._planned_commit_apply = {"mode": "fallback", "entry": head}
        if bool(apply):
            self._apply_planned_commit_entry()
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
        expected_recovery_ftq = self._expected_recovery_ftq_for_redirect(
            ftq_flag=int(ftq_flag),
            ftq_value=int(ftq_value),
            ftq_offset=int(ftq_offset),
            is_rvc=bool(is_rvc),
            taken=bool(taken),
            flush_itself=bool(flush_itself),
        )
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
            if pre_flush_current_ftq is not None and self._current_ftq_entry is None:
                self._cfvec_queue_close_current_ftq_span(
                    int(pre_flush_current_ftq[0]),
                    int(pre_flush_current_ftq[1]),
                )
            self._mark_active_wrong_path_redirect_driven(
                int(target_pc),
                expected_recovery_ftq=expected_recovery_ftq,
            )
            if self._recovery_phase_active():
                flush_summary = self._cfvec_queue_flush_wrong_path(
                    keep_open_ftqs={(int(ftq_flag), int(ftq_value))},
                )
                if flush_summary is not None:
                    semantic_start_pc = flush_summary["observed_first_pc"]
                    if (
                        not flush_itself
                        and int(payload.get("taken", 0)) != 0
                        and "pc" in payload
                    ):
                        semantic_start_pc = int(
                            self._sequential_next_pc(
                                int(payload["pc"]),
                                bool(payload.get("is_rvc", 0)),
                            )
                        )
                    self.logger.info(
                        "redirect queue flush: reason=%s target=0x%x removed=%d queue_idx=[%d,%d] pc_start=%s queue_pc_ranges=%s",
                        reason,
                        int(target_pc),
                        int(flush_summary["removed_count"]),
                        int(flush_summary["queue_index_start"]),
                        int(flush_summary["queue_index_end"]),
                        (
                            "none"
                            if semantic_start_pc is None
                            else f"0x{int(semantic_start_pc):x}"
                        ),
                        str(flush_summary["pc_ranges"]),
                    )
                self._mark_active_wrong_path_redirect_driven(
                    int(target_pc),
                    expected_recovery_ftq=expected_recovery_ftq,
                )
            self._sync_backend_state()
            self._apply_backend_state()
        self._refresh_pending_level0_target_ftq_for_redirect(
            redirect_ftq_flag=int(ftq_flag),
            redirect_ftq_value=int(ftq_value),
            target_pc=int(target_pc),
            flush_itself=bool(flush_itself),
            commit_ptr=original_commit_ptr,
        )
        self._clear_ftq_redirect_pending(int(ftq_flag), int(ftq_value))
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
        if self._redirect_drive_ftq_is_stale_relative_to_commit(int(drive_ftq_flag), int(drive_ftq_value)):
            fallback_context = self._latest_non_stale_redirect_drive_context()
            if fallback_context is not None:
                drive_ftq_flag = int(fallback_context.get("ftq_flag", drive_ftq_flag))
                drive_ftq_value = int(fallback_context.get("ftq_value", drive_ftq_value))
                drive_from_pc = int(fallback_context.get("pc", drive_from_pc))
                drive_ftq_offset = int(fallback_context.get("ftq_offset", drive_ftq_offset))
                drive_is_rvc = int(fallback_context.get("is_rvc", drive_is_rvc))
        self._assert_redirect_drive_ftq_not_stale(
            payload_ftq_flag=int(ftq_flag),
            payload_ftq_value=int(ftq_value),
            drive_ftq_flag=int(drive_ftq_flag),
            drive_ftq_value=int(drive_ftq_value),
            target_pc=int(target_pc),
            reason=str(reason),
        )

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
            self.logger.warning(
                "ibufFull watchdog fired without redirect: threshold=%d safe_pc=0x%x",
                self.ibuf_watchdog_threshold,
                self.safe_pc,
            )
            self._publish(
                "backend.watchdog",
                {
                    "reason": "ibuf_full_no_redirect",
                    "threshold": int(self.ibuf_watchdog_threshold),
                    "safe_pc": int(self.safe_pc),
                },
                level="WARNING",
            )
            self.ibuf_full_streak = 0

    def inject_redirect(self, target_pc: int, reason: str, delay_cycles: Optional[int] = None) -> None:
        self._assert_explicit_injection_allowed("redirect")
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
        self._clear_active_wrong_path_episode()
        self._last_correct_cfi_context = None
        # Note: do NOT clear ftq_entries — entries already dispatch-complete remain valid;
        # they will naturally drain via _drive_commit() until the redirect fires.

    def inject_exception(self, cause: int, tval: int, pc: int, delay_cycles: int = _MIN_BACKEND_DELAY) -> None:
        self._assert_explicit_injection_allowed("exception")
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
        self._clear_stale_auxiliary_states()
        self._update_ftq_start_pc_cache(observation)
        self._watchdog(observation)
        if self.observe_if is not None:
            self._sample_cfvec()
        resolve_entries = self._ready_resolves_for_cycle()
        self._plan_instruction_commits_for_cycle()
        commit_entry = self._plan_commit_entry_for_cycle(apply=False)
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
        self.commit_entry_driven(actions.commit_entry)
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
            + len(self._pending_queue_call_ret_commit_indices)
            + int(scheduled_call_ret_count)
            + len(self._visible_queue_call_ret_commit_group)
            + (1 if self._current_ftq_entry is not None else 0)
            + (1 if self._pending_level0_target_ftq is not None else 0)
            + (1 if self._has_active_wrong_path_episode() else 0)
        )

    def golden_completion_pending_work_count(self) -> int:
        matched_queue_indices = {
            int(idx)
            for idx, entry in enumerate(self._cfvec_queue)
            if entry.golden_match_state == GOLDEN_MATCH_STATE_MATCHED
            or entry.golden_index is not None
        }
        pending_resolve_count = sum(
            1
            for entry in self._pending_resolves
            if entry.queue_index is None or int(entry.queue_index) in matched_queue_indices
        )
        pending_call_ret_count = sum(
            1
            for idx in self._pending_queue_call_ret_commit_indices
            if int(idx) in matched_queue_indices
        )
        scheduled_call_ret_count = sum(
            1
            for _ready_cycle, group in self._scheduled_queue_call_ret_commit_groups
            for inst in group
            if int(inst.json_index) >= 0
        )
        visible_call_ret_count = sum(
            1
            for inst in self._visible_queue_call_ret_commit_group
            if int(inst.json_index) >= 0
        )
        active_recovery_count = 1 if self._has_active_wrong_path_episode() else 0
        return (
            len(matched_queue_indices)
            + pending_resolve_count
            + len(self.pending_events)
            + pending_call_ret_count
            + scheduled_call_ret_count
            + visible_call_ret_count
            + active_recovery_count
        )

    def has_pending_work(self) -> bool:
        return self.pending_work_count() > 0

    def recent_events(self, limit: int = 16) -> list:
        if limit <= 0:
            return []
        return list(self.last_events)[-limit:]

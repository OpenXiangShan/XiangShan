from __future__ import annotations

from collections import deque
from typing import Callable

from .backend_state import BackendEvent, BackendState, ResolveEntry


class FtqScoreboard:
    def __init__(self, state: BackendState) -> None:
        self.state = state

    def note_resolve(self, entry: ResolveEntry, current_cycle: int, entry_flushes_itself: bool) -> None:
        for ftq_entry in self.state.ftq_entries:
            if self.state.ftq_entry_matches(ftq_entry, entry.ftq_flag, entry.ftq_value):
                ftq_entry.resolved_cfi += 1
                ftq_entry.has_redirect = ftq_entry.has_redirect or bool(entry_flushes_itself)
                ftq_entry.commit_ready_cycle = max(int(ftq_entry.commit_ready_cycle), int(current_cycle) + 1)
                break
        if (
            self.state.current_ftq_entry is not None
            and self.state.ftq_entry_matches(self.state.current_ftq_entry, entry.ftq_flag, entry.ftq_value)
        ):
            self.state.current_ftq_entry.resolved_cfi += 1
            self.state.current_ftq_entry.has_redirect = (
                self.state.current_ftq_entry.has_redirect or bool(entry_flushes_itself)
            )
            self.state.current_ftq_entry.commit_ready_cycle = max(
                int(self.state.current_ftq_entry.commit_ready_cycle),
                int(current_cycle) + 1,
            )

    def recompute_cfi_budgets_from_pending_resolves(self) -> None:
        pending_by_entry: dict[tuple[int, int], int] = {}
        for entry in self.state.pending_resolves:
            key = (int(entry.ftq_flag), int(entry.ftq_value))
            pending_by_entry[key] = pending_by_entry.get(key, 0) + 1

        for ftq_entry in self.state.ftq_entries:
            key = (int(ftq_entry.ftq_flag), int(ftq_entry.ftq_value))
            surviving_pending = int(pending_by_entry.get(key, 0))
            ftq_entry.total_cfi = max(int(ftq_entry.resolved_cfi), int(ftq_entry.resolved_cfi) + surviving_pending)
        if self.state.current_ftq_entry is not None:
            key = (int(self.state.current_ftq_entry.ftq_flag), int(self.state.current_ftq_entry.ftq_value))
            surviving_pending = int(pending_by_entry.get(key, 0))
            self.state.current_ftq_entry.total_cfi = max(
                int(self.state.current_ftq_entry.resolved_cfi),
                int(self.state.current_ftq_entry.resolved_cfi) + surviving_pending,
            )

    def apply_redirect_flush(
        self,
        *,
        ftq_flag: int,
        ftq_value: int,
        ftq_offset: int,
        flush_itself: bool,
        keep_cycle: int,
        current_cycle: int,
        is_rvc: bool,
        pending_event_survives: Callable[[BackendEvent, int, int, int, int, bool, int], bool],
    ) -> None:
        redirect_rank = self.state.ftq_ptr_rank_after_commit(ftq_flag, ftq_value)
        next_target_ftq = None if flush_itself else self.state.increment_ftq_ptr(ftq_flag, ftq_value)
        current_entry_is_next_target_ftq = bool(
            not flush_itself
            and next_target_ftq is not None
            and self.state.current_ftq_entry is not None
            and self.state.ftq_entry_matches(self.state.current_ftq_entry, *next_target_ftq)
        )
        self.state.pending_resolves = deque(
            entry
            for entry in self.state.pending_resolves
            if (
                int(entry.queued_cycle) >= int(keep_cycle)
                or (
                    self.state.same_entry_offset_survives(entry.ftq_offset, ftq_offset, flush_itself)
                    if self.state.ftq_entry_matches(entry, ftq_flag, ftq_value)
                    else self.state.ftq_ptr_survives_redirect(entry.ftq_flag, entry.ftq_value, redirect_rank, flush_itself)
                )
            )
        )
        self.state.pending_events = deque(
            evt
            for evt in self.state.pending_events
            if pending_event_survives(
                evt,
                ftq_flag,
                ftq_value,
                ftq_offset,
                redirect_rank,
                flush_itself,
                keep_cycle,
            )
        )
        if flush_itself:
            self.state.commit_ptr_flag = int(ftq_flag)
            self.state.commit_ptr_value = int(ftq_value)
            self.state.reuse_commit_ptr_once = self.state.redirect_reuses_same_ftq_slot(ftq_offset, bool(is_rvc))
            self.state.ftq_entries = deque(
                entry
                for entry in self.state.ftq_entries
                if self.state.ftq_ptr_rank_after_commit(entry.ftq_flag, entry.ftq_value) < redirect_rank
            )
            if self.state.current_ftq_entry is not None and (
                self.state.ftq_ptr_rank_after_commit(
                    self.state.current_ftq_entry.ftq_flag,
                    self.state.current_ftq_entry.ftq_value,
                )
                >= redirect_rank
            ):
                self.state.current_ftq_entry = None
                self.state.current_ftq_seen_packets.clear()
        else:
            self.state.reuse_commit_ptr_once = False
            self.state.ftq_entries = deque(
                entry
                for entry in self.state.ftq_entries
                if self.state.ftq_ptr_rank_after_commit(entry.ftq_flag, entry.ftq_value) <= redirect_rank
            )
            if self.state.current_ftq_entry is not None and (
                self.state.ftq_ptr_rank_after_commit(
                    self.state.current_ftq_entry.ftq_flag,
                    self.state.current_ftq_entry.ftq_value,
                )
                > redirect_rank
            ):
                if not current_entry_is_next_target_ftq:
                    self.state.current_ftq_entry = None
                    self.state.current_ftq_seen_packets.clear()
            if self.state.ftq_entries and (
                self.state.commit_count > 0
                or self.state.commit_ptr_flag != 0
                or self.state.commit_ptr_value != 0
            ):
                oldest_survivor = self.state.ftq_entries[0]
                expected_next = self.state.increment_ftq_ptr(self.state.commit_ptr_flag, self.state.commit_ptr_value)
                survivor_ptr = (int(oldest_survivor.ftq_flag), int(oldest_survivor.ftq_value))
                if survivor_ptr != expected_next:
                    self.state.commit_ptr_flag, self.state.commit_ptr_value = self.state.decrement_ftq_ptr(
                        oldest_survivor.ftq_flag,
                        oldest_survivor.ftq_value,
                    )
            for entry in self.state.ftq_entries:
                if self.state.ftq_entry_matches(entry, ftq_flag, ftq_value):
                    entry.commit_ready_cycle = max(int(entry.commit_ready_cycle), int(current_cycle) + 1)
                    break
            if (
                self.state.current_ftq_entry is not None
                and self.state.ftq_entry_matches(self.state.current_ftq_entry, ftq_flag, ftq_value)
            ):
                self.state.current_ftq_entry.commit_ready_cycle = max(
                    int(self.state.current_ftq_entry.commit_ready_cycle),
                    int(current_cycle) + 1,
                )
        self.recompute_cfi_budgets_from_pending_resolves()

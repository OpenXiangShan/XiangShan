from __future__ import annotations

from collections import deque
from dataclasses import dataclass, field
from typing import Deque, Optional


@dataclass
class FrontendPacket:
    cycle: int
    pc: int
    instr: int
    is_rvc: bool


@dataclass
class BackendEvent:
    kind: str
    ready_cycle: int
    payload: dict


@dataclass
class FtqEntry:
    ftq_flag: int
    ftq_value: int
    total_cfi: int = 0
    resolved_cfi: int = 0
    dispatch_complete: bool = False
    has_redirect: bool = False
    commit_ready_cycle: int = 0


@dataclass
class ResolveEntry:
    ready_cycle: int
    inst_pc: int
    pc: int
    target: int
    taken: bool
    mispredict: bool
    ftq_flag: int
    ftq_value: int
    ftq_offset: int
    branch_type: int
    ras_action: int
    queued_cycle: int = 0
    is_rvc: bool = False


@dataclass
class BackendState:
    ftq_size: int = 64
    current_cycle: int = 0
    commit_count: int = 0
    ftq_entries: Deque[FtqEntry] = field(default_factory=deque)
    pending_resolves: Deque[ResolveEntry] = field(default_factory=deque)
    current_ftq_entry: Optional[FtqEntry] = None
    current_ftq_seen_packets: set[tuple[int, int, int, int, int]] = field(default_factory=set)
    pending_events: Deque[BackendEvent] = field(default_factory=deque)
    last_events: Deque[dict] = field(default_factory=lambda: deque(maxlen=64))
    commit_ptr_flag: int = 0
    commit_ptr_value: int = 0
    reuse_commit_ptr_once: bool = False
    ibuf_full_streak: int = 0
    ftq_start_pc_cache: dict[int, int] = field(default_factory=dict)
    ftq_start_pc_by_value: dict[int, int] = field(default_factory=dict)
    ftq_group_pc_history: dict[tuple[int, int], list[tuple[int, bool]]] = field(default_factory=dict)
    pc_group_occurrences: dict[int, list[tuple[int, int, int]]] = field(default_factory=dict)
    pending_level0_target_ftq: Optional[tuple[int, int]] = None

    def increment_ftq_ptr(self, flag: int, value: int) -> tuple[int, int]:
        next_value = int(value) + 1
        next_flag = int(flag)
        if next_value >= int(self.ftq_size):
            next_value = 0
            next_flag ^= 1
        return next_flag, next_value

    def decrement_ftq_ptr(self, flag: int, value: int) -> tuple[int, int]:
        prev_value = int(value) - 1
        prev_flag = int(flag)
        if prev_value < 0:
            prev_value = int(self.ftq_size) - 1
            prev_flag ^= 1
        return prev_flag, prev_value

    @staticmethod
    def ftq_entry_matches(entry: FtqEntry, flag: int, value: int) -> bool:
        return int(entry.ftq_flag) == int(flag) and int(entry.ftq_value) == int(value)

    def has_seen_ftq_ptr(self, flag: int, value: int) -> bool:
        if self.current_ftq_entry is not None and self.ftq_entry_matches(self.current_ftq_entry, flag, value):
            return True
        return any(self.ftq_entry_matches(entry, flag, value) for entry in self.ftq_entries)

    @staticmethod
    def redirect_reuses_same_ftq_slot(ftq_offset: int, is_rvc: bool) -> bool:
        return int(ftq_offset) == 0 or (int(ftq_offset) == 1 and not bool(is_rvc))

    def ftq_ptr_rank_after_commit(self, flag: int, value: int) -> int:
        modulus = max(1, int(self.ftq_size) * 2)
        base = int(self.commit_ptr_flag) * int(self.ftq_size) + int(self.commit_ptr_value)
        target = int(flag) * int(self.ftq_size) + int(value)
        return (target - base) % modulus

    def ftq_ptr_survives_redirect(self, flag: int, value: int, redirect_rank: int, flush_itself: bool) -> bool:
        entry_rank = self.ftq_ptr_rank_after_commit(flag, value)
        if flush_itself:
            return entry_rank < int(redirect_rank)
        return entry_rank <= int(redirect_rank)

    @staticmethod
    def same_entry_offset_survives(ftq_offset: int, redirect_offset: int, flush_itself: bool) -> bool:
        if flush_itself:
            return int(ftq_offset) < int(redirect_offset)
        return int(ftq_offset) <= int(redirect_offset)

    @staticmethod
    def event_source_cycle(evt: BackendEvent) -> int:
        payload = evt.payload if isinstance(evt.payload, dict) else {}
        if "source_cycle" in payload:
            return int(payload["source_cycle"])
        if "queued_cycle" in payload:
            return int(payload["queued_cycle"])
        return -1

    def pending_event_survives_redirect(
        self,
        evt: BackendEvent,
        redirect_flag: int,
        redirect_value: int,
        redirect_offset: int,
        redirect_rank: int,
        flush_itself: bool,
        keep_cycle: int,
    ) -> bool:
        payload = evt.payload if isinstance(evt.payload, dict) else {}
        if self.event_source_cycle(evt) >= int(keep_cycle):
            return True
        if "ftq_flag" in payload and "ftq_value" in payload:
            if (
                int(payload["ftq_flag"]) == int(redirect_flag)
                and int(payload["ftq_value"]) == int(redirect_value)
                and "ftq_offset" in payload
            ):
                return self.same_entry_offset_survives(
                    int(payload["ftq_offset"]),
                    int(redirect_offset),
                    bool(flush_itself),
                )
            return self.ftq_ptr_survives_redirect(
                int(payload["ftq_flag"]),
                int(payload["ftq_value"]),
                int(redirect_rank),
                bool(flush_itself),
            )
        return False

    def find_next_commitable_entry(self) -> Optional[FtqEntry]:
        if not self.ftq_entries:
            return None

        expected: Optional[tuple[int, int]] = None
        if self.commit_count > 0 or self.commit_ptr_flag != 0 or self.commit_ptr_value != 0:
            if self.reuse_commit_ptr_once:
                expected = (int(self.commit_ptr_flag), int(self.commit_ptr_value))
            else:
                expected = self.increment_ftq_ptr(self.commit_ptr_flag, self.commit_ptr_value)

        candidate = None
        if expected is None:
            candidate = self.ftq_entries[0]
        else:
            exp_flag, exp_value = expected
            for entry in self.ftq_entries:
                if self.ftq_entry_matches(entry, exp_flag, exp_value):
                    candidate = entry
                    break
            if candidate is None and self.ftq_entries:
                oldest_survivor = self.ftq_entries[0]
                if self.pending_level0_target_ftq is not None:
                    wait_flag, wait_value = self.pending_level0_target_ftq
                    if self.ftq_entry_matches(oldest_survivor, wait_flag, wait_value):
                        self.pending_level0_target_ftq = None
                    elif self.ftq_ptr_rank_after_commit(wait_flag, wait_value) <= self.ftq_ptr_rank_after_commit(
                        oldest_survivor.ftq_flag,
                        oldest_survivor.ftq_value,
                    ):
                        return None
                self.commit_ptr_flag, self.commit_ptr_value = self.decrement_ftq_ptr(
                    oldest_survivor.ftq_flag,
                    oldest_survivor.ftq_value,
                )
                candidate = oldest_survivor

        if candidate is None:
            return None
        if not candidate.dispatch_complete or candidate.resolved_cfi < candidate.total_cfi:
            return None
        if candidate.has_redirect:
            return None
        if int(self.current_cycle) < int(candidate.commit_ready_cycle):
            return None
        return candidate

    def seal_current_ftq_entry(self) -> None:
        if self.current_ftq_entry is None:
            return
        self.current_ftq_entry.dispatch_complete = True
        self.current_ftq_entry.commit_ready_cycle = max(
            int(self.current_ftq_entry.commit_ready_cycle),
            int(self.current_cycle) + 1,
        )
        self.ftq_entries.append(self.current_ftq_entry)
        self.current_ftq_entry = None
        self.current_ftq_seen_packets.clear()

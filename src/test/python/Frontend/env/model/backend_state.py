from __future__ import annotations

import random
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
class CommitInstruction:
    json_index: int
    pc: int
    instr: int
    ftq_flag: int
    ftq_value: int
    ras_action: int


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
    pending_commit_instructions: Deque[CommitInstruction] = field(default_factory=deque)
    scheduled_commit_groups: Deque[tuple[int, list[CommitInstruction]]] = field(default_factory=deque)
    visible_commit_group: list[CommitInstruction] = field(default_factory=list)
    visible_json_commit_count: int = 0
    ftq_real_instr_total: dict[tuple[int, int], int] = field(default_factory=dict)
    ftq_visible_instr_commits: dict[tuple[int, int], int] = field(default_factory=dict)
    commit_min_delay: int = 3
    commit_max_delay: int = 10

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

    def sample_commit_delay(self) -> int:
        min_delay = max(1, int(self.commit_min_delay))
        max_delay = max(min_delay, int(self.commit_max_delay))
        return random.randint(min_delay, max_delay)

    def extend_commit_ready_cycle(self, entry: FtqEntry, *, current_cycle: Optional[int] = None) -> None:
        base_cycle = int(self.current_cycle) if current_cycle is None else int(current_cycle)
        entry.commit_ready_cycle = max(
            int(entry.commit_ready_cycle),
            base_cycle + self.sample_commit_delay(),
        )

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

    @staticmethod
    def decode_commit_ras_action(instr: int) -> int:
        opc = int(instr) & 0x7F
        if opc == 0x6F:
            rd = (int(instr) >> 7) & 0x1F
            return 2 if rd in (1, 5) else 0
        if opc == 0x67:
            rd = (int(instr) >> 7) & 0x1F
            rs1 = (int(instr) >> 15) & 0x1F
            is_rd_link = rd in (1, 5)
            is_rs1_link = rs1 in (1, 5)
            if is_rd_link and is_rs1_link and rd != rs1:
                return 3
            if is_rs1_link and not is_rd_link:
                return 1
            if is_rd_link:
                return 2
        return 0

    def record_consumed_commit_instruction(
        self,
        json_index: int,
        pc: int,
        instr: int,
        ftq_flag: int,
        ftq_value: int,
    ) -> None:
        key = (int(ftq_flag), int(ftq_value))
        self.pending_commit_instructions.append(
            CommitInstruction(
                json_index=int(json_index),
                pc=int(pc),
                instr=int(instr),
                ftq_flag=int(ftq_flag),
                ftq_value=int(ftq_value),
                ras_action=self.decode_commit_ras_action(int(instr)),
            )
        )
        self.ftq_real_instr_total[key] = self.ftq_real_instr_total.get(key, 0) + 1

    def schedule_next_commit_group(self) -> None:
        if not self.pending_commit_instructions:
            return
        if self.scheduled_commit_groups and int(self.scheduled_commit_groups[-1][0]) >= int(self.current_cycle) + 1:
            return
        group: list[CommitInstruction] = []
        while self.pending_commit_instructions and len(group) < 8:
            group.append(self.pending_commit_instructions.popleft())
        if not group:
            return
        self.scheduled_commit_groups.append((int(self.current_cycle) + 1, group))

    def activate_visible_commit_group(self) -> None:
        self.visible_commit_group = []
        if not self.scheduled_commit_groups:
            return
        ready_cycle, group = self.scheduled_commit_groups[0]
        if int(self.current_cycle) < int(ready_cycle):
            return
        self.scheduled_commit_groups.popleft()
        self.visible_commit_group = list(group)
        self.visible_json_commit_count += len(group)
        for inst in group:
            key = (int(inst.ftq_flag), int(inst.ftq_value))
            self.ftq_visible_instr_commits[key] = self.ftq_visible_instr_commits.get(key, 0) + 1

    def ftq_entry_commit_covered(self, entry: FtqEntry, *, golden_trace_attached: bool) -> bool:
        if not golden_trace_attached:
            if (
                self.ftq_real_instr_total
                or self.ftq_visible_instr_commits
                or self.pending_commit_instructions
                or self.scheduled_commit_groups
                or self.visible_commit_group
            ):
                raise AssertionError("commit visibility requires an attached golden trace")
            return True
        key = (int(entry.ftq_flag), int(entry.ftq_value))
        total_real_instr = int(self.ftq_real_instr_total.get(key, 0))
        if total_real_instr <= 0:
            return False
        visible_instr = int(self.ftq_visible_instr_commits.get(key, 0))
        return visible_instr >= total_real_instr

    def reset_commit_visibility(self) -> None:
        self.pending_commit_instructions.clear()
        self.scheduled_commit_groups.clear()
        self.visible_commit_group = []
        self.visible_json_commit_count = 0
        self.ftq_real_instr_total.clear()
        self.ftq_visible_instr_commits.clear()

    def _commit_visibility_survives_redirect(
        self,
        *,
        ftq_flag: int,
        ftq_value: int,
        redirect_flag: int,
        redirect_value: int,
        redirect_rank: int,
        flush_itself: bool,
    ) -> bool:
        if int(ftq_flag) == int(redirect_flag) and int(ftq_value) == int(redirect_value):
            return True
        return self.ftq_ptr_survives_redirect(int(ftq_flag), int(ftq_value), int(redirect_rank), bool(flush_itself))

    def assert_no_stale_commit_visibility(
        self,
        *,
        redirect_flag: int,
        redirect_value: int,
        flush_itself: bool,
    ) -> None:
        redirect_rank = self.ftq_ptr_rank_after_commit(int(redirect_flag), int(redirect_value))
        stale: list[tuple[int, int]] = []

        def _check_inst(inst) -> None:
            if self._commit_visibility_survives_redirect(
                ftq_flag=int(getattr(inst, "ftq_flag")),
                ftq_value=int(getattr(inst, "ftq_value")),
                redirect_flag=int(redirect_flag),
                redirect_value=int(redirect_value),
                redirect_rank=int(redirect_rank),
                flush_itself=bool(flush_itself),
            ):
                return
            stale.append((int(getattr(inst, "ftq_flag")), int(getattr(inst, "ftq_value"))))

        for inst in self.pending_commit_instructions:
            _check_inst(inst)
        for _, group in self.scheduled_commit_groups:
            for inst in group:
                _check_inst(inst)
        for inst in self.visible_commit_group:
            _check_inst(inst)

        if stale:
            stale_desc = ", ".join(f"({flag},{value})" for flag, value in stale)
            raise AssertionError(f"stale commit visibility survives redirect flush: {stale_desc}")

    def find_next_commitable_entry(self, *, golden_trace_attached: bool = False) -> Optional[FtqEntry]:
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
                if self.ftq_entry_matches(oldest_survivor, self.commit_ptr_flag, self.commit_ptr_value) and not self.reuse_commit_ptr_once:
                    return None
                if expected is not None:
                    duplicate_expected_survivor = any(
                        self.ftq_entry_matches(entry, exp_flag, exp_value)
                        for entry in list(self.ftq_entries)[1:]
                    )
                    if duplicate_expected_survivor and self.ftq_entry_matches(oldest_survivor, exp_flag, exp_value):
                        return None
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
        if not self.ftq_entry_commit_covered(candidate, golden_trace_attached=golden_trace_attached):
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
        self.extend_commit_ready_cycle(self.current_ftq_entry)
        if self.ftq_entries and self.ftq_entry_matches(
            self.ftq_entries[-1],
            self.current_ftq_entry.ftq_flag,
            self.current_ftq_entry.ftq_value,
        ):
            tail = self.ftq_entries[-1]
            tail.total_cfi = max(int(tail.total_cfi), int(self.current_ftq_entry.total_cfi))
            tail.resolved_cfi = max(int(tail.resolved_cfi), int(self.current_ftq_entry.resolved_cfi))
            tail.dispatch_complete = bool(tail.dispatch_complete or self.current_ftq_entry.dispatch_complete)
            tail.has_redirect = bool(tail.has_redirect or self.current_ftq_entry.has_redirect)
            tail.commit_ready_cycle = max(
                int(tail.commit_ready_cycle),
                int(self.current_ftq_entry.commit_ready_cycle),
            )
        else:
            self.ftq_entries.append(self.current_ftq_entry)
        self.current_ftq_entry = None
        self.current_ftq_seen_packets.clear()

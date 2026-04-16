from __future__ import annotations

import random
from collections import deque
from dataclasses import dataclass, field
from typing import Deque, Optional

PATH_STATE_UNKNOWN = "unknown"
PATH_STATE_CORRECT = "correct"
PATH_STATE_WRONG = "wrong"

RESOLVE_STATE_NOT_NEEDED = "not_needed"
RESOLVE_STATE_PENDING = "pending"
RESOLVE_STATE_EMITTED = "emitted"
RESOLVE_STATE_SKIPPED = "skipped"

ROB_COMMIT_STATE_PENDING = "pending"
ROB_COMMIT_STATE_COMMITTED = "committed"

CALL_RET_STATE_NONE = "none"
CALL_RET_STATE_PENDING = "pending"
CALL_RET_STATE_EMITTED = "emitted"

GOLDEN_MATCH_STATE_UNKNOWN = "unknown"
GOLDEN_MATCH_STATE_MATCHED = "matched"
GOLDEN_MATCH_STATE_MISMATCHED = "mismatched"


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
    observed_last_in_entry: bool = False
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
    queue_index: Optional[int] = None


@dataclass
class CommitInstruction:
    json_index: int
    pc: int
    instr: int
    ftq_flag: int
    ftq_value: int
    ras_action: int
    queue_index: Optional[int] = None
    ftq_offset: Optional[int] = None


@dataclass
class QueueInstr:
    cycle: int
    slot: int
    pc: int
    instr: int
    is_rvc: bool
    pred_taken: bool
    ftq_flag: int
    ftq_value: int
    ftq_offset: int
    is_last_in_entry: bool
    path_state: str = PATH_STATE_UNKNOWN
    resolve_state: str = RESOLVE_STATE_NOT_NEEDED
    rob_commit_state: str = ROB_COMMIT_STATE_PENDING
    call_ret_commit_state: str = CALL_RET_STATE_NONE
    call_ret_ras_action: int = 0
    golden_match_state: str = GOLDEN_MATCH_STATE_UNKNOWN
    golden_index: Optional[int] = None
    is_cfi: bool = False


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
    semantic_queue: Deque[QueueInstr] = field(default_factory=deque)
    pending_redirect_origin_index: Optional[int] = None
    pending_queue_resolve_indices: Deque[int] = field(default_factory=deque)
    pending_queue_call_ret_commit_indices: Deque[int] = field(default_factory=deque)
    scheduled_queue_call_ret_commit_groups: Deque[tuple[int, list[CommitInstruction]]] = field(default_factory=deque)
    visible_queue_call_ret_commit_group: list[CommitInstruction] = field(default_factory=list)
    commit_min_delay: int = 3
    commit_max_delay: int = 10
    rng: object = random

    def append_semantic_queue_instruction(self, instr: QueueInstr) -> int:
        self.semantic_queue.append(instr)
        return len(self.semantic_queue) - 1

    def schedule_next_queue_call_ret_commit_group(self) -> None:
        if not self.pending_queue_call_ret_commit_indices:
            return
        if (
            self.scheduled_queue_call_ret_commit_groups
            and int(self.scheduled_queue_call_ret_commit_groups[-1][0]) >= int(self.current_cycle) + 1
        ):
            return
        group: list[CommitInstruction] = []
        while self.pending_queue_call_ret_commit_indices and len(group) < 8:
            idx = int(self.pending_queue_call_ret_commit_indices.popleft())
            if idx < 0 or idx >= len(self.semantic_queue):
                continue
            entry = self.semantic_queue[idx]
            group.append(
                CommitInstruction(
                    json_index=-1 if entry.golden_index is None else int(entry.golden_index),
                    pc=int(entry.pc),
                    instr=int(entry.instr),
                    ftq_flag=int(entry.ftq_flag),
                    ftq_value=int(entry.ftq_value),
                    ras_action=int(entry.call_ret_ras_action),
                    queue_index=int(idx),
                    ftq_offset=int(entry.ftq_offset),
                )
            )
        if not group:
            return
        self.scheduled_queue_call_ret_commit_groups.append((int(self.current_cycle) + 1, group))

    def _call_ret_queue_index_from_identity(self, inst: CommitInstruction) -> Optional[int]:
        inst_offset = getattr(inst, "ftq_offset", None)
        if inst_offset is None:
            return None
        candidates: list[int] = []
        for idx, entry in enumerate(self.semantic_queue):
            if int(entry.ftq_flag) != int(inst.ftq_flag):
                continue
            if int(entry.ftq_value) != int(inst.ftq_value):
                continue
            if int(entry.ftq_offset) != int(inst_offset):
                continue
            candidates.append(int(idx))
        if not candidates:
            return None
        for idx in candidates:
            if self.semantic_queue[int(idx)].call_ret_commit_state == CALL_RET_STATE_PENDING:
                return int(idx)
        return int(candidates[0])

    def activate_visible_queue_call_ret_commit_group(self) -> None:
        self.visible_queue_call_ret_commit_group = []
        if not self.scheduled_queue_call_ret_commit_groups:
            return
        ready_cycle, group = self.scheduled_queue_call_ret_commit_groups[0]
        if int(self.current_cycle) < int(ready_cycle):
            return
        self.scheduled_queue_call_ret_commit_groups.popleft()
        self.visible_queue_call_ret_commit_group = list(group)
        for inst in self.visible_queue_call_ret_commit_group:
            idx = self._call_ret_queue_index_from_identity(inst)
            # Backward compatibility: legacy groups without ftq_offset still rely on queue_index.
            if idx is None and getattr(inst, "ftq_offset", None) is None:
                queue_index = getattr(inst, "queue_index", None)
                if queue_index is not None:
                    idx = int(queue_index)
            if idx is not None and 0 <= int(idx) < len(self.semantic_queue):
                self.semantic_queue[int(idx)].call_ret_commit_state = CALL_RET_STATE_EMITTED

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
        return self.rng.randint(min_delay, max_delay)

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

    def ftq_entry_dispatch_ready_for_commit(self, entry: FtqEntry) -> bool:
        if bool(entry.dispatch_complete):
            return True
        if self.pending_level0_target_ftq is None:
            return False
        wait_flag, wait_value = self.pending_level0_target_ftq
        if self.ftq_entry_matches(entry, wait_flag, wait_value):
            return False
        return self.ftq_ptr_rank_after_commit(
            int(entry.ftq_flag),
            int(entry.ftq_value),
        ) < self.ftq_ptr_rank_after_commit(int(wait_flag), int(wait_value))

    def ftq_entry_is_ready_to_commit(
        self,
        entry: FtqEntry,
        *,
        golden_trace_attached: bool,
        allow_pending_target_match: bool = False,
    ) -> bool:
        dispatch_ready = bool(entry.dispatch_complete)
        if not dispatch_ready and not allow_pending_target_match:
            dispatch_ready = self.ftq_entry_dispatch_ready_for_commit(entry)
        if not dispatch_ready:
            return False
        if entry.resolved_cfi < entry.total_cfi:
            return False
        if entry.has_redirect:
            return False
        if int(self.current_cycle) < int(entry.commit_ready_cycle):
            return False
        return True

    def find_next_commitable_entry(self, *, golden_trace_attached: bool = False) -> Optional[FtqEntry]:
        if not self.ftq_entries:
            return None

        if self.commit_count <= 0 and self.commit_ptr_flag == 0 and self.commit_ptr_value == 0 and not self.reuse_commit_ptr_once:
            oldest = self.ftq_entries[0]
            expected = (int(oldest.ftq_flag), int(oldest.ftq_value))
        elif self.reuse_commit_ptr_once:
            expected = (int(self.commit_ptr_flag), int(self.commit_ptr_value))
        else:
            expected = self.increment_ftq_ptr(self.commit_ptr_flag, self.commit_ptr_value)

        if self.pending_level0_target_ftq is not None:
            matching_entries = [
                entry
                for entry in self.ftq_entries
                if self.ftq_entry_matches(entry, int(expected[0]), int(expected[1]))
            ]
            if not matching_entries:
                return None
            if (int(expected[0]), int(expected[1])) == (
                int(self.pending_level0_target_ftq[0]),
                int(self.pending_level0_target_ftq[1]),
            ):
                return next(
                    (
                        entry
                        for entry in matching_entries
                        if self.ftq_entry_is_ready_to_commit(
                            entry,
                            golden_trace_attached=golden_trace_attached,
                            allow_pending_target_match=True,
                        )
                    ),
                    None,
                )
            return next(
                (
                    entry
                    for entry in matching_entries
                    if self.ftq_entry_is_ready_to_commit(
                        entry,
                        golden_trace_attached=golden_trace_attached,
                    )
                ),
                None,
            )

        matching_entries = [
            entry
            for entry in self.ftq_entries
            if self.ftq_entry_matches(entry, int(expected[0]), int(expected[1]))
        ]
        if not matching_entries:
            return None
        return next(
            (
                entry
                for entry in matching_entries
                if self.ftq_entry_is_ready_to_commit(
                    entry,
                    golden_trace_attached=golden_trace_attached,
                )
            ),
            None,
        )

    def seal_current_ftq_entry(self, *, observed_last_in_entry: bool = False) -> None:
        if self.current_ftq_entry is None:
            return
        self.current_ftq_entry.observed_last_in_entry = bool(
            self.current_ftq_entry.observed_last_in_entry or observed_last_in_entry
        )
        self.current_ftq_entry.dispatch_complete = bool(self.current_ftq_entry.observed_last_in_entry)
        if self.current_ftq_entry.dispatch_complete:
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
            tail.observed_last_in_entry = bool(
                tail.observed_last_in_entry or self.current_ftq_entry.observed_last_in_entry
            )
            tail.has_redirect = bool(tail.has_redirect or self.current_ftq_entry.has_redirect)
            tail.commit_ready_cycle = max(
                int(tail.commit_ready_cycle),
                int(self.current_ftq_entry.commit_ready_cycle),
            )
        else:
            self.ftq_entries.append(self.current_ftq_entry)
        self.current_ftq_entry = None
        self.current_ftq_seen_packets.clear()

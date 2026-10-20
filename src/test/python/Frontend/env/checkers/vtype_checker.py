from __future__ import annotations

import logging
from dataclasses import asdict
from typing import Callable, Iterable, Optional

from ..model.backend_state import PATH_STATE_CORRECT, ROB_COMMIT_STATE_COMMITTED, QueueInstr
from ..model.golden_trace import GoldenTrace
from ..model.vtype import VTypeModel, VTypeState, VTypeTransition


class VTypeChecker:
    """Check cfVec VType fields against instruction or golden-trace results."""

    def __init__(
        self,
        expected_entries: Iterable,
        model: Optional[VTypeModel] = None,
        *,
        trace_results: bool = False,
        start_cursor: int = 0,
    ) -> None:
        model = model or VTypeModel()
        self._entries = list(expected_entries)
        self._start_cursor = int(start_cursor)
        self._expected: list[VTypeTransition] = []
        self.expected_by_pc = {}
        self._ambiguous_pcs: set[int] = set()
        state = VTypeState.initial()
        for entry in self._entries:
            pc = int(entry.pc)
            if trace_results:
                before = state
                kind = self._vector_config_kind(entry.instr)
                if kind is not None:
                    if entry.vtype_after is None:
                        raise ValueError(
                            f"vtype check requires vtype_after at trace index={entry.index} pc=0x{pc:x}"
                        )
                    state = self._decode_vtype(int(entry.vtype_after))
                transition = VTypeTransition(
                    instruction=int(entry.instr),
                    kind=kind or "ordinary",
                    specvtype=before,
                    vtype=before if kind == "vsetvl" else state,
                )
            else:
                transition = model.step(entry.instr)
            self._expected.append(transition)
            if pc in self.expected_by_pc or pc in self._ambiguous_pcs:
                self._ambiguous_pcs.add(pc)
                self.expected_by_pc.pop(pc, None)
            else:
                self.expected_by_pc[pc] = transition
        self.env = None
        self.event_sink: Optional[Callable[[dict], None]] = None
        self.checked_count = 0
        self.error_count = 0
        self.errors: list[dict] = []
        self.last_error: Optional[dict] = None
        self.observed_pcs: set[int] = set()
        self.checked_indices: set[int] = set()

    @classmethod
    def from_golden_trace(cls, trace: GoldenTrace, *, start_cursor: int = 0) -> "VTypeChecker":
        return cls(trace.entries, trace_results=True, start_cursor=start_cursor)

    @staticmethod
    def _vector_config_kind(instr: int) -> str | None:
        if (int(instr) & 0x707F) != 0x7057:
            return None
        return "vsetvl" if (int(instr) & 0xFE00707F) == 0x80007057 else "vseti"

    @staticmethod
    def _decode_vtype(raw: int) -> VTypeState:
        return VTypeState(
            illegal=bool((raw >> 63) & 1),
            vma=bool((raw >> 7) & 1),
            vta=bool((raw >> 6) & 1),
            vsew=(raw >> 3) & 0x7,
            vlmul=raw & 0x7,
        )

    def bind_env(self, env) -> None:
        self.env = env

    def set_event_sink(self, sink: Optional[Callable[[dict], None]]) -> None:
        self.event_sink = sink

    @staticmethod
    def _actual_vtype(slot, prefix: str) -> VTypeState:
        return VTypeState(
            illegal=bool(getattr(slot, f"{prefix}_illegal")),
            vma=bool(getattr(slot, f"{prefix}_vma")),
            vta=bool(getattr(slot, f"{prefix}_vta")),
            vsew=int(getattr(slot, f"{prefix}_vsew")),
            vlmul=int(getattr(slot, f"{prefix}_vlmul")),
        )

    def _record_error(self, cycle: int, slot, pc: int, expected, actual, field: str) -> None:
        entry = {
            "cycle": int(cycle),
            "slot": int(slot.slot),
            "pc": int(pc),
            "instr": int(slot.instr),
            "field": str(field),
            "expected": asdict(expected) if isinstance(expected, VTypeState) else expected,
            "actual": asdict(actual) if isinstance(actual, VTypeState) else actual,
        }
        self.error_count += 1
        self.last_error = entry
        self.errors.append(entry)
        if self.event_sink is not None:
            self.event_sink(
                {
                    "type": "check.vtype_mismatch",
                    "source": type(self).__name__,
                    "cycle": int(cycle),
                    "level": "ERROR",
                    "payload": dict(entry),
                }
            )

    def _check_slot(
        self, cycle: int, slot, pc: int, transition: VTypeTransition,
        *, cursor: Optional[int], index: Optional[int] = None, check_instruction: bool = True,
    ) -> None:
        self.checked_count += 1
        context = (
            f"VType mismatch cycle={cycle} cursor={cursor} slot={slot.slot} "
            f"index={index} pc=0x{pc:x} instr=0x{int(slot.instr):08x}"
        )
        if check_instruction and int(slot.instr) != transition.instruction:
            self._record_error(cycle, slot, pc, transition.instruction, int(slot.instr), "instr")
            message = (
                f"{context} field=instr expected=0x{transition.instruction:08x} "
                f"actual=0x{int(slot.instr):08x}"
            )
            logging.getLogger("env.checkers").error(message)
            raise AssertionError(message)
        for field, expected in (("specvtype", transition.specvtype), ("vtype", transition.vtype)):
            actual = self._actual_vtype(slot, field)
            if expected == actual:
                continue
            self._record_error(cycle, slot, pc, expected, actual, field)
            message = f"{context} field={field} expected={expected} actual={actual}"
            logging.getLogger("env.checkers").error(message)
            raise AssertionError(message)
        self.observed_pcs.add(pc)
        if index is not None:
            self.checked_indices.add(index)

    def check_snapshot(self, snapshot, pc_for_slot: Callable[[int], int]) -> None:
        cycle = int(snapshot.cycle)
        for slot in snapshot.slots:
            if not slot.valid:
                continue
            pc = int(pc_for_slot(slot.slot))
            if pc in self._ambiguous_pcs:
                raise ValueError(f"VTypeChecker requires a dynamic index for duplicate pc=0x{pc:x}")
            transition = self.expected_by_pc.get(pc)
            if transition is not None:
                self._check_slot(cycle, slot, pc, transition, cursor=self._cursor())

    def check_cycle(
        self,
        snapshot,
        queue: Iterable[QueueInstr],
        pc_for_slot: Callable[[int], int],
        *,
        cursor: int,
    ) -> None:
        entries = tuple(queue)
        current = {
            int(entry.slot): entry
            for entry in entries
            if int(entry.cycle) == int(snapshot.cycle)
            and entry.path_state == PATH_STATE_CORRECT
            and entry.golden_index is not None
        }
        pending_vsetvl = [
            int(entry.golden_index)
            for entry in entries
            if entry.path_state == PATH_STATE_CORRECT
            and entry.golden_index is not None
            and entry.rob_commit_state != ROB_COMMIT_STATE_COMMITTED
            and self._vector_config_kind(entry.instr) == "vsetvl"
        ]
        first_pending_vsetvl = min(pending_vsetvl) if pending_vsetvl else None
        for slot in snapshot.slots:
            if not slot.valid or slot.slot not in current:
                continue
            queue_entry = current[slot.slot]
            index = int(queue_entry.golden_index)
            if first_pending_vsetvl is not None and index > first_pending_vsetvl:
                continue
            pc = int(pc_for_slot(slot.slot))
            if not 0 <= index < len(self._expected):
                self._fail_dynamic(snapshot.cycle, cursor, slot, index, pc, "index", "in trace", index)
            golden = self._entries[index]
            if pc != int(golden.pc):
                self._fail_dynamic(snapshot.cycle, cursor, slot, index, pc, "pc", int(golden.pc), pc)
            transition = self._expected[index]
            self._check_slot(
                snapshot.cycle, slot, pc, transition, cursor=cursor, index=index,
                check_instruction=transition.kind != "ordinary",
            )

    @staticmethod
    def _fail_dynamic(cycle: int, cursor: int, slot, index: int, pc: int, field: str, expected, actual) -> None:
        message = (
            f"VType mismatch cycle={cycle} cursor={cursor} slot={slot.slot} "
            f"index={index} pc=0x{pc:x} field={field} expected={expected} actual={actual}"
        )
        logging.getLogger("env.checkers").error(message)
        raise AssertionError(message)

    def check_completion(self, *, cycle: int, cursor: int) -> None:
        vector_indices = {
            index for index, expected in enumerate(self._expected)
            if expected.kind != "ordinary" and self._start_cursor <= index < cursor
        }
        missing = sorted(vector_indices - self.checked_indices)
        if missing:
            message = (
                f"VType check incomplete cycle={cycle} cursor={cursor} "
                f"missing_vector_indices={missing} checked_count={self.checked_count}"
            )
            logging.getLogger("env.checkers").error(message)
            raise AssertionError(message)
        logging.getLogger("env.checkers").info(
            "VType check completed: cycle=%d cursor=%d checked_count=%d "
            "unique_checked=%d checked_indices_sample=%s",
            cycle, cursor, self.checked_count, len(self.checked_indices),
            sorted(self.checked_indices)[:16],
        )

    def _cursor(self) -> Optional[int]:
        if self.env is None:
            return None
        return self.env.backend_model.current_golden_cursor()

    def on_clock_edge(self, cycle: int) -> None:
        if self.env is None:
            return
        snapshot = getattr(getattr(self.env, "monitor", None), "cycle_snapshot", None)
        if snapshot is None or int(snapshot.cycle) != int(cycle):
            raise AssertionError("VType checker requires a current FrontendMonitor snapshot")
        self.check_snapshot(snapshot.cfvec, self.env.observed_cfvec_pc)

    def get_stats(self) -> dict:
        return {
            "checked_count": int(self.checked_count),
            "error_count": int(self.error_count),
            "last_error": None if self.last_error is None else dict(self.last_error),
        }


__all__ = ["VTypeChecker"]

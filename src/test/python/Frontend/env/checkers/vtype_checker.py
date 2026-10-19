from __future__ import annotations

import logging
from dataclasses import asdict
from typing import Callable, Iterable, Optional

from ..model.vtype import VTypeModel, VTypeState


class VTypeChecker:
    """Check IBuffer cfVec VType fields against the lane-ordered model."""

    def __init__(self, expected_entries: Iterable, model: Optional[VTypeModel] = None) -> None:
        model = model or VTypeModel()
        self.expected_by_pc = {}
        for entry in expected_entries:
            pc = int(entry.pc)
            if pc in self.expected_by_pc:
                raise ValueError(f"VTypeChecker requires a linear trace; duplicate pc=0x{pc:x}")
            self.expected_by_pc[pc] = model.step(entry.instr)
        self.env = None
        self.event_sink: Optional[Callable[[dict], None]] = None
        self.checked_count = 0
        self.error_count = 0
        self.errors: list[dict] = []
        self.last_error: Optional[dict] = None
        self.observed_pcs: set[int] = set()

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

    def check_snapshot(self, snapshot, pc_for_slot: Callable[[int], int]) -> None:
        cycle = int(snapshot.cycle)
        for slot in snapshot.slots:
            if not slot.valid:
                continue
            pc = int(pc_for_slot(slot.slot))
            transition = self.expected_by_pc.get(pc)
            if transition is None:
                continue
            self.checked_count += 1
            if int(slot.instr) != transition.instruction:
                self._record_error(cycle, slot, pc, transition.instruction, int(slot.instr), "instr")
                message = (
                    f"VType mismatch cycle={cycle} cursor={self._cursor()} slot={slot.slot} "
                    f"pc=0x{pc:x} field=instr expected=0x{transition.instruction:08x} "
                    f"actual=0x{int(slot.instr):08x}"
                )
                logging.getLogger("env.checkers").error(message)
                raise AssertionError(message)
            for field, expected, actual in (
                ("specvtype", transition.specvtype, self._actual_vtype(slot, "specvtype")),
                ("vtype", transition.vtype, self._actual_vtype(slot, "vtype")),
            ):
                if expected == actual:
                    continue
                self._record_error(cycle, slot, pc, expected, actual, field)
                message = (
                    f"VType mismatch cycle={cycle} cursor={self._cursor()} slot={slot.slot} "
                    f"pc=0x{pc:x} instr=0x{int(slot.instr):08x} "
                    f"field={field} expected={expected} actual={actual}"
                )
                logging.getLogger("env.checkers").error(message)
                raise AssertionError(message)
            self.observed_pcs.add(pc)

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

from __future__ import annotations

import json
import struct
from dataclasses import dataclass
from typing import List, Optional

from .memory_model import MemoryModel


@dataclass
class TraceEntry:
    index: int
    pc: int
    instr: int
    size: int
    kind: str = "normal"
    taken: bool = False
    target_pc: Optional[int] = None
    exception: Optional[int] = None


def _parse_trace_int(value: int | str) -> int:
    if isinstance(value, str):
        return int(value, 0)
    return int(value)


def _is_rvc_halfword(hw: int) -> bool:
    return (hw & 0x3) != 0x3


def _decode_rv32(pc: int, instr: int) -> tuple[str, bool, Optional[int]]:
    opc = instr & 0x7F
    if opc == 0x63:
        imm12 = (instr >> 31) & 0x1
        imm10_5 = (instr >> 25) & 0x3F
        imm4_1 = (instr >> 8) & 0xF
        imm11 = (instr >> 7) & 0x1
        imm = (imm12 << 12) | (imm11 << 11) | (imm10_5 << 5) | (imm4_1 << 1)
        if imm & (1 << 12):
            imm -= 1 << 13
        return "branch", False, pc + imm
    if opc == 0x6F:
        imm20 = (instr >> 31) & 0x1
        imm10_1 = (instr >> 21) & 0x3FF
        imm11 = (instr >> 20) & 0x1
        imm19_12 = (instr >> 12) & 0xFF
        imm = (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1)
        if imm & (1 << 20):
            imm -= 1 << 21
        return "jump", True, pc + imm
    if opc == 0x67:
        return "jump_indirect", True, None
    return "normal", False, None


class GoldenTrace:
    KIND_IDS = {
        0: "normal",
        1: "branch",
        2: "jump",
        3: "jump_indirect",
        4: "call",
        5: "ret",
        6: "exception",
        7: "interrupt",
    }

    def __init__(self, entries: List[TraceEntry]) -> None:
        self.entries = entries
        self.cursor = 0

    @classmethod
    def from_memory(cls, mem: MemoryModel, start_pc: int, count: int) -> "GoldenTrace":
        entries: List[TraceEntry] = []
        pc = int(start_pc)
        for i in range(int(count)):
            hw = mem.read_u16(pc)
            if _is_rvc_halfword(hw):
                instr = hw
                size = 2
                kind = "normal"
                taken = False
                target = None
            else:
                instr = mem.read_u32(pc)
                size = 4
                kind, taken, target = _decode_rv32(pc, instr)
            entries.append(
                TraceEntry(
                    index=i,
                    pc=pc,
                    instr=instr,
                    size=size,
                    kind=kind,
                    taken=taken,
                    target_pc=target,
                )
            )
            pc += size
        return cls(entries)

    @classmethod
    def from_file(cls, path: str) -> "GoldenTrace":
        if path.endswith(".json"):
            with open(path, "r", encoding="utf-8") as f:
                raw = json.load(f)
            return cls._from_raw_list(raw)
        if path.endswith(".jsonl"):
            raw = []
            with open(path, "r", encoding="utf-8") as f:
                for line in f:
                    line = line.strip()
                    if not line:
                        continue
                    raw.append(json.loads(line))
            return cls._from_raw_list(raw)
        if path.endswith(".bin") or path.endswith(".tracebin"):
            return cls._from_bin(path)
        raise ValueError(f"unsupported trace format: {path}")

    @classmethod
    def _from_raw_list(cls, raw: list) -> "GoldenTrace":
        entries = []
        for i, ent in enumerate(raw):
            entries.append(
                TraceEntry(
                    index=int(ent.get("index", i)),
                    pc=_parse_trace_int(ent["pc"]),
                    instr=_parse_trace_int(ent["instr"]),
                    size=int(ent.get("size", 4)),
                    kind=str(ent.get("kind", "normal")),
                    taken=bool(ent.get("taken", False)),
                    target_pc=(
                        None
                        if ent.get("target_pc") is None
                        else _parse_trace_int(ent["target_pc"])
                    ),
                    exception=(
                        None
                        if ent.get("exception") is None
                        else _parse_trace_int(ent["exception"])
                    ),
                )
            )
        return cls(entries)

    @classmethod
    def _from_bin(cls, path: str) -> "GoldenTrace":
        fmt = "<QIBBBqI"
        step = struct.calcsize(fmt)
        data = open(path, "rb").read()
        if len(data) % step != 0:
            raise ValueError("invalid binary trace length")
        entries: List[TraceEntry] = []
        for i in range(0, len(data), step):
            pc, instr, size, kind_id, taken, target, exc = struct.unpack_from(fmt, data, i)
            entries.append(
                TraceEntry(
                    index=len(entries),
                    pc=pc,
                    instr=instr,
                    size=size,
                    kind=cls.KIND_IDS.get(kind_id, "normal"),
                    taken=bool(taken),
                    target_pc=(None if target < 0 else int(target)),
                    exception=(None if exc == 0xFFFFFFFF else int(exc)),
                )
            )
        return cls(entries)

    def reset(self, cursor: int = 0) -> None:
        self.cursor = int(cursor)

    def peek(self) -> Optional[TraceEntry]:
        if self.cursor >= len(self.entries):
            return None
        return self.entries[self.cursor]

    def next_entry(self) -> Optional[TraceEntry]:
        ent = self.peek()
        if ent is None:
            return None
        self.cursor += 1
        return ent


__all__ = ["GoldenTrace", "TraceEntry"]

"""Convert raw NEMU execute/debug logs into GoldenTrace rows.

Task 9 of the memblock-style migration intentionally keeps this helper inside
``env`` for now instead of moving it into a separate ``tools/``
package. The trace-prep APIs still treat it as part of the env-side workflow.
"""

from __future__ import annotations

import argparse
import json
import re
from typing import Iterable, List, Optional


_ANSI_RE = re.compile(r"\x1b\[[0-9;]*m")
_EXEC_LINE_RE = re.compile(
    r"execute\]\s+(?:end_of_loop:\s+)?prev pc = 0x([0-9a-fA-F]+), pc = 0x([0-9a-fA-F]+)"
)
_DEBUG_LINE_RE = re.compile(
    r"debug_hook\]\s+\([^)]+\)0x([0-9a-fA-F]+):\s*(.*)$"
)
_BYTE_RE = re.compile(r"^[0-9a-fA-F]{2}$")

_COND_BRANCH_MNEMONICS = {
    "beq",
    "bne",
    "blt",
    "bge",
    "bltu",
    "bgeu",
    "beqz",
    "bnez",
    "c_beqz",
    "c_bnez",
}
_DIRECT_JUMP_MNEMONICS = {
    "jal",
    "j",
    "call",
    "tail",
    "p_jal",
    "c_j",
    "c_jal",
}
_INDIRECT_JUMP_MNEMONICS = {
    "jalr",
    "jr",
    "ret",
    "p_ret",
    "c_jr",
    "c_jalr",
}


def _strip_ansi(s: str) -> str:
    return _ANSI_RE.sub("", s)


def _bytes_to_instr_le(byte_tokens: List[str]) -> int:
    value = 0
    for i, b in enumerate(byte_tokens):
        value |= (int(b, 16) & 0xFF) << (8 * i)
    return value


def _classify_kind(mnemonic: str, instr: int, size: int) -> str:
    m = mnemonic.lower()
    if m in _COND_BRANCH_MNEMONICS:
        return "branch"
    if m in _DIRECT_JUMP_MNEMONICS:
        return "jump"
    if m in _INDIRECT_JUMP_MNEMONICS:
        return "jump_indirect"

    if size == 4:
        opc = instr & 0x7F
        if opc == 0x63:
            return "branch"
        if opc == 0x6F:
            return "jump"
        if opc == 0x67:
            return "jump_indirect"
    return "normal"


def _parse_exec_line(raw_line: str) -> Optional[tuple[int, int]]:
    line = _strip_ansi(raw_line).strip()
    m = _EXEC_LINE_RE.search(line)
    if m is None:
        return None
    prev_pc = int(m.group(1), 16)
    next_pc = int(m.group(2), 16)
    return prev_pc, next_pc


def _parse_debug_line(raw_line: str) -> Optional[dict]:
    line = _strip_ansi(raw_line).strip()
    m = _DEBUG_LINE_RE.search(line)
    if m is None:
        return None

    pc = int(m.group(1), 16)
    rest = m.group(2).strip()
    if not rest:
        return None

    tokens = rest.split()
    byte_tokens: List[str] = []
    idx = 0
    while idx < len(tokens) and _BYTE_RE.match(tokens[idx]):
        byte_tokens.append(tokens[idx])
        idx += 1
    if not byte_tokens:
        return None

    instr = _bytes_to_instr_le(byte_tokens)
    size = len(byte_tokens)
    mnemonic = tokens[idx] if idx < len(tokens) else ""
    kind = _classify_kind(mnemonic, instr, size)

    return {
        "pc": pc,
        "instr": instr,
        "size": size,
        "kind": kind,
    }


def convert_nemu_log_lines(lines: Iterable[str]) -> List[dict]:
    parsed: List[dict] = []
    pending_execs: List[tuple[int, int]] = []
    for line in lines:
        exec_info = _parse_exec_line(line)
        if exec_info is not None:
            pending_execs.append(exec_info)
            continue

        item = _parse_debug_line(line)
        if item is None:
            continue

        item["next_pc"] = None
        if pending_execs:
            match_index = next(
                (idx for idx, (prev_pc, _next_pc) in enumerate(pending_execs) if int(item["pc"]) == int(prev_pc)),
                None,
            )
            if match_index is not None:
                _prev_pc, next_pc = pending_execs.pop(int(match_index))
                item["next_pc"] = int(next_pc)
        parsed.append(item)

    for idx, item in enumerate(parsed):
        if item["next_pc"] is not None:
            continue
        if idx + 1 >= len(parsed):
            continue
        item["next_pc"] = int(parsed[idx + 1]["pc"])

    out: List[dict] = []
    for i, cur in enumerate(parsed):
        next_pc = cur["next_pc"]
        if next_pc is None:
            continue
        next_pc = int(next_pc)
        kind = str(cur["kind"])
        pc = int(cur["pc"])
        size = int(cur["size"])

        taken = False
        target_pc = None
        if kind == "branch":
            fall_through = pc + size
            taken = int(next_pc) != int(fall_through)
            target_pc = int(next_pc) if taken else int(fall_through)
        elif kind in {"jump", "jump_indirect"}:
            taken = True
            target_pc = int(next_pc)

        out.append(
            {
                "index": i,
                "pc": hex(int(cur["pc"])),
                "instr": hex(int(cur["instr"])),
                "size": int(cur["size"]),
                "kind": kind,
                "taken": bool(taken),
                "target_pc": hex(target_pc) if target_pc is not None else None,
            }
        )
    return out


def convert_nemu_log_file(input_path: str, output_path: str, limit: int = 0) -> int:
    with open(input_path, "r", encoding="utf-8", errors="ignore") as f:
        rows = convert_nemu_log_lines(f)
    if int(limit) > 0:
        rows = rows[: int(limit)]
    with open(output_path, "w", encoding="utf-8") as f:
        if str(output_path).endswith(".json"):
            json.dump(rows, f, ensure_ascii=True, indent=2)
            f.write("\n")
        else:
            for row in rows:
                f.write(json.dumps(row, ensure_ascii=True) + "\n")
    return len(rows)


def main(argv: Optional[List[str]] = None) -> int:
    parser = argparse.ArgumentParser(
        description="Convert NEMU execute/debug_hook log to GoldenTrace JSON/JSONL."
    )
    parser.add_argument("input", help="Path to NEMU log file")
    parser.add_argument("output", help="Path to output JSON/JSONL file")
    parser.add_argument(
        "--limit",
        type=int,
        default=0,
        help="Optional max number of trace entries to emit",
    )
    args = parser.parse_args(argv)
    count = convert_nemu_log_file(args.input, args.output, limit=args.limit)
    print(f"converted {count} entries -> {args.output}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

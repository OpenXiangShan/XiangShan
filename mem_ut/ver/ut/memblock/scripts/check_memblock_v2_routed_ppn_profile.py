#!/usr/bin/env python3
"""将 V2 RTL 的实际地址译码谓词精确归并，与测试框架路由合同比较。"""
from __future__ import annotations

import argparse
from pathlib import Path
import re
import sys

ADDRESS = "auto_in_a_bits_address"
WIDTH = 48


def split_top(text: str, separator: str) -> list[str]:
    depth = 0
    start = 0
    result = []
    cursor = 0
    while cursor < len(text):
        char = text[cursor]
        if char in "({[":
            depth += 1
        elif char in ")}]":
            depth -= 1
        if depth == 0 and text.startswith(separator, cursor):
            result.append(text[start:cursor].strip())
            cursor += len(separator)
            start = cursor
        else:
            cursor += 1
    result.append(text[start:].strip())
    return result


def unwrap(text: str) -> str:
    text = text.strip()
    while text.startswith("(") and text.endswith(")"):
        depth = 0
        encloses = True
        for index, char in enumerate(text):
            if char == "(":
                depth += 1
            elif char == ")":
                depth -= 1
            if depth == 0 and index != len(text) - 1:
                encloses = False
                break
        if not encloses:
            break
        text = text[1:-1].strip()
    return text


# 每一位保存 (源地址bit或None, 常数异或位)，从低位到高位排列。
def parse_bits(text: str, definitions: dict[str, str]) -> list[tuple[int | None, int]]:
    text = unwrap(text)
    pieces = split_top(text, "^")
    if len(pieces) > 1:
        result = parse_bits(pieces[0], definitions)
        for piece in pieces[1:]:
            operand = parse_bits(piece, definitions)
            if len(result) != len(operand):
                raise ValueError(f"XOR宽度不同: {text}")
            updated = []
            for (left, left_const), (right, right_const) in zip(result, operand):
                if left is not None and right is not None:
                    raise ValueError(f"不支持地址位互相XOR: {text}")
                updated.append((left if left is not None else right, left_const ^ right_const))
            result = updated
        return result
    if text.startswith("~"):
        return [(source, constant ^ 1) for source, constant in parse_bits(text[1:], definitions)]
    if text.startswith("{") and text.endswith("}"):
        result = []
        for part in reversed(split_top(text[1:-1], ",")):
            result.extend(parse_bits(part, definitions))
        return result
    constant = re.fullmatch(r"(\d+)'([hbd])([0-9a-fA-F_]+)", text)
    if constant:
        width = int(constant.group(1))
        value = int(constant.group(3).replace("_", ""), {"h": 16, "b": 2, "d": 10}[constant.group(2)])
        return [(None, (value >> index) & 1) for index in range(width)]
    selection = re.fullmatch(r"(\w+)\[(\d+)(?::(\d+))?\]", text)
    if selection:
        name = selection.group(1)
        high = int(selection.group(2))
        low = int(selection.group(3) or selection.group(2))
        source = [(index, 0) for index in range(WIDTH)] if name == ADDRESS else parse_bits(definitions[name], definitions)
        return source[low:high + 1]
    if text in definitions:
        return parse_bits(definitions[text], definitions)
    raise ValueError(f"未识别RTL表达式: {text}")


def decode_term(term: str, definitions: dict[str, str]) -> tuple[int, int] | None:
    equality = split_top(term, "==")
    bits = parse_bits(equality[0], definitions)
    if len(equality) == 2:
        expected_bits = parse_bits(equality[1], definitions)
        if any(source is not None for source, _ in expected_bits):
            raise ValueError("比较右侧必须是常数")
        expected = sum(constant << index for index, (_, constant) in enumerate(expected_bits))
    elif len(equality) == 1 and len(bits) == 1:
        expected = 1
    else:
        raise ValueError(f"未识别地址选择条件: {term}")
    mask = value = 0
    for index, (source, constant) in enumerate(bits):
        required = ((expected >> index) & 1) ^ constant
        if source is None:
            if required:
                return None
        else:
            if mask & (1 << source) and ((value >> source) & 1) != required:
                return None
            mask |= 1 << source
            value |= required << source
    return mask, value


def term_intervals(mask: int, value: int) -> list[tuple[int, int]]:
    if mask == 0:
        return [(0, 1 << WIDTH)]
    trailing = (mask & -mask).bit_length() - 1
    free = [index for index in range(trailing, WIDTH) if not mask & (1 << index)]
    if len(free) > 16:
        raise ValueError("新RTL谓词存在过多不连续自由位，请更新精确归并器")
    result = []
    for combination in range(1 << len(free)):
        first = value
        for position, bit in enumerate(free):
            first |= ((combination >> position) & 1) << bit
        result.append((first, first + (1 << trailing)))
    return result


def merge(intervals: list[tuple[int, int]]) -> list[tuple[int, int]]:
    merged = []
    for first, limit in sorted(intervals):
        if merged and first <= merged[-1][1]:
            merged[-1] = (merged[-1][0], max(limit, merged[-1][1]))
        else:
            merged.append((first, limit))
    return merged


def rtl_intervals(path: Path, output: str) -> list[tuple[int, int]]:
    text = path.read_text()
    definitions = {
        name: expression.strip()
        for name, expression in re.findall(r"\bwire\s+(?:\[[^]]+\]\s*)?(\w+)\s*=\s*(.*?);", text, re.S)
    }
    intervals = []
    for term in split_top(definitions[output], "|"):
        decoded = decode_term(term, definitions)
        if decoded is not None:
            intervals.extend(term_intervals(*decoded))
    return merge(intervals)


def profile_intervals(path: Path) -> list[tuple[int, int]]:
    constants = {
        name: int(value.replace("_", ""), 16)
        for name, value in re.findall(r"\b(MEMBLOCK_ROUTE_(?:FIRST|LIMIT)_\d+)\s*=\s*49'h([0-9a-fA-F_]+)", path.read_text())
    }
    return [(constants[f"MEMBLOCK_ROUTE_FIRST_{index}"], constants[f"MEMBLOCK_ROUTE_LIMIT_{index}"]) for index in range(4)]


def main() -> int:
    root = Path(__file__).resolve().parents[5]
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--rtl", type=Path, default=root / "build/rtl/TLXbar_6.sv")
    parser.add_argument("--profile", type=Path, default=root / "mem_ut/ver/ut/memblock/seq/base_seq_help/memblock_routed_ppn_profile_v2.svh")
    args = parser.parse_args()
    actual = rtl_intervals(args.rtl, "requestAIO_0_1")
    expected = profile_intervals(args.profile)
    internal = rtl_intervals(args.rtl, "requestAIO_0_0")
    if actual != expected or internal != [(0x38022000, 0x38022080)]:
        print(f"FAIL: 外部RTL={actual!r}, profile={expected!r}; 内部RTL={internal!r}", file=sys.stderr)
        return 1
    for first, limit in actual:
        print(f"外部 [0x{first:012x}, 0x{limit:013x})")
    print("PASS: RTL地址谓词与V2路由合同完全一致；APLIC空洞、内部128B、外部部分页已区分")
    return 0


if __name__ == "__main__":
    try:
        raise SystemExit(main())
    except (KeyError, ValueError, OSError) as error:
        print(f"FAIL: {error}", file=sys.stderr)
        raise SystemExit(1)

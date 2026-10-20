#!/usr/bin/env python3
"""Generate deterministic assembly streams for IBuffer vtype propagation."""

from __future__ import annotations

import argparse
from pathlib import Path


CASES = (
    ("e32_m1_ta_ma", "vsetvli t0, zero, e32, m1, ta, ma"),
    ("e16_m2_tu_mu", "vsetivli t0, 4, e16, m2, tu, mu"),
    ("e64_m4_ta_mu", "vsetvli t0, t1, e64, m4, ta, mu"),
    ("e8_mf2_tu_ma", "vsetivli zero, 0, e8, mf2, tu, ma"),
)

EDGE_CASES = (
    ("keep_vl_vill", ".word 0x0d807057"),
    ("reserved_vtype", ".word 0x1d0072d7"),
)


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=Path(__file__).resolve().parent / "generated" / "vector_vtype",
    )
    parser.add_argument("--case-name", default="fe_vector_vtype_sequence")
    return parser.parse_args()


def _render(case_name: str) -> str:
    lines = [
        f"    # Name        : {case_name}",
        "    # Goal        : IBuffer vtype/specvtype propagation for ordered vset instructions",
        "    # ISA         : rv64gcv",
        "    # Scope       : macro-op fetch/order only; no vector arithmetic result check",
        "    # Expected    : each vset updates the following macro-ops in program order",
        "    # Configs     : " + ", ".join(name for name, _ in CASES),
        "",
        "    .section .text, \"ax\"",
        "    .option norvc",
        "    .globl _start",
        "",
        "_start:",
        "    # Enable the vector state before the first vset instruction.",
        "    li t2, 0x600",
        "    csrs mstatus, t2",
        "    li t1, 8",
    ]
    for _, instruction in CASES:
        lines.extend(
            [
                f"    # {instruction}",
                f"    {instruction}",
                "    nop",
            ]
        )
    for _, instruction in EDGE_CASES:
        lines.extend(
            [
                f"    # {instruction}",
                f"    {instruction}",
                "    nop",
            ]
        )
    lines.extend(
        [
            "    addi a0, zero, 0",
            "    .word 0x0005006b",
            "    .rept 128",
            "    nop",
            "    .endr",
            "",
        ]
    )
    return "\n".join(lines)


def main() -> int:
    args = _parse_args()
    args.out_dir.mkdir(parents=True, exist_ok=True)
    output = args.out_dir / f"{args.case_name}.S"
    output.write_text(_render(args.case_name), encoding="utf-8")
    print(output)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

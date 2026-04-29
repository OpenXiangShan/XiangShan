#!/usr/bin/env python3
from __future__ import annotations

import argparse
import random
from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class BranchSite:
    block: int
    slot: int
    taken: bool
    target_pad: int


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Generate reproducible frontend assembly testcase streams.",
    )
    parser.add_argument("--seed", type=int, required=True)
    parser.add_argument("--count", type=int, default=3)
    parser.add_argument("--case-family", choices=("branch_mix",), default="branch_mix")
    parser.add_argument("--blocks", type=int, default=12)
    parser.add_argument("--iterations", type=int, default=24)
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=Path(__file__).resolve().parent / "generated",
    )
    return parser.parse_args()


def _make_branch_sites(rng: random.Random, blocks: int) -> list[BranchSite]:
    sites: list[BranchSite] = []
    for block in range(blocks):
        slot = rng.choice((0, 1, 2, 3, 4, 5))
        taken = rng.random() < 0.60
        target_pad = rng.choice((0, 1, 2, 3, 4, 5, 8, 12))
        sites.append(BranchSite(block=block, slot=slot, taken=taken, target_pad=target_pad))
    return sites


def _emit_branch_mix(
    *,
    seed: int,
    case_index: int,
    blocks: int,
    iterations: int,
    sites: list[BranchSite],
) -> str:
    name = f"fe_random_branch_mix_{seed:08x}_{case_index:02d}"
    lines: list[str] = [
        f"    # Name        : {name}",
        "    # Goal        : generated branch direction and target mix for frontend PC/CFI coverage",
        "    # Entry       : _start",
        "    # Family      : branch_mix",
        f"    # Seed        : {seed}",
        f"    # Case index  : {case_index}",
        f"    # Blocks      : {blocks}",
        f"    # Iterations  : {iterations}",
        "    # Pattern     : generated 32B-aligned branch blocks with recorded taken/target map",
        "    # Expected CFI: see Branch map below; generated file is deterministic for this seed",
        "    # Boundary    : each block label is .align 5",
        "    # Check       : frontend follows reproducible generated branch target/fall-through stream",
        "    # Branch map  : block slot direction target_label target_pad_nops",
    ]
    for site in sites:
        direction = "T" if site.taken else "N"
        lines.append(
            f"    #               b{site.block:02d} slot={site.slot} dir={direction} "
            f"target=branch_{site.block:02d}_target pad={site.target_pad}"
        )

    lines.extend(
        [
            "",
            "    .section .text",
            "    .option norvc",
            "    .globl _start",
            "",
            "    .macro FILL_NOPS count",
            "        .rept \\count",
            "            nop",
            "        .endr",
            "    .endm",
            "",
            "_start:",
            "    # init",
            f"    li s0, {iterations}",
            "    li s1, 1",
            "    li s2, 0",
            "    j block_00",
            "",
        ]
    )

    for site in sites:
        if site.block + 1 < blocks:
            next_path = [f"    j block_{site.block + 1:02d}"]
        else:
            next_path = [
                "    addi s0, s0, -1",
                "    bnez s0, block_00",
                "    j done",
            ]
        lines.extend(
            [
                "    .align 5",
                f"block_{site.block:02d}:",
                f"    # generated branch site: slot={site.slot} direction={'taken' if site.taken else 'not-taken'} target=branch_{site.block:02d}_target",
            ]
        )
        if site.slot > 0:
            lines.append(f"    FILL_NOPS {site.slot}")
        if site.taken:
            lines.append(f"    beq s1, s1, branch_{site.block:02d}_target")
            if site.target_pad > 0:
                lines.append(f"    FILL_NOPS {site.target_pad}")
            lines.append(f"branch_{site.block:02d}_target:")
            lines.append("    addi s3, s3, 1")
            lines.extend(next_path)
        else:
            lines.append(f"    beq s1, s2, branch_{site.block:02d}_target")
            lines.append("    addi s3, s3, 1")
            lines.extend(next_path)
            if site.target_pad > 0:
                lines.append(f"    FILL_NOPS {site.target_pad}")
            lines.append(f"branch_{site.block:02d}_target:")
            lines.append("    addi s3, s3, -1")
            lines.append("    j done")
        lines.append("")

    lines.extend(
        [
            "done:",
            "    # finish",
            "    li a0, 0",
            "    .word 0x0005006b",
            "",
        ]
    )
    return "\n".join(lines)


def _write_branch_mix(args: argparse.Namespace) -> list[Path]:
    args.out_dir.mkdir(parents=True, exist_ok=True)
    written: list[Path] = []
    for case_index in range(int(args.count)):
        case_seed = int(args.seed) + case_index
        rng = random.Random(case_seed)
        sites = _make_branch_sites(rng, int(args.blocks))
        text = _emit_branch_mix(
            seed=case_seed,
            case_index=case_index,
            blocks=int(args.blocks),
            iterations=int(args.iterations),
            sites=sites,
        )
        path = args.out_dir / f"fe_random_branch_mix_{case_seed:08x}_{case_index:02d}.S"
        path.write_text(text, encoding="ascii")
        written.append(path)
    return written


def main() -> int:
    args = _parse_args()
    if int(args.count) <= 0:
        raise SystemExit("--count must be > 0")
    if int(args.blocks) < 2:
        raise SystemExit("--blocks must be >= 2")
    if int(args.iterations) <= 0:
        raise SystemExit("--iterations must be > 0")

    if args.case_family == "branch_mix":
        written = _write_branch_mix(args)
    else:
        raise SystemExit(f"unsupported case family: {args.case_family}")

    for path in written:
        print(path)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

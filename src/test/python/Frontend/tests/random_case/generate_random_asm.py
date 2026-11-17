#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
import random
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Iterable


GENERATOR_SCHEMA_VERSION = 1
FAMILIES = (
    "cfg_layout_mix",
    "ras_indirect_history",
    "ibuffer_pressure_cross",
    "translation_timing_cross",
)
PHASES = ("warmup", "train", "perturb", "stress", "drain")


@dataclass(frozen=True)
class CaseSpec:
    family: str
    seed: int
    ordinal: int = 0
    blocks: int = 96
    iterations: int = 10
    max_call_depth: int = 6
    indirect_target_mode: str = "fixed"
    directed_pattern: str = "none"
    page_tail_probe_repetitions: int | None = None

    @property
    def name(self) -> str:
        return (
            f"fe_random_{self.family}_s{self.seed & ((1 << 64) - 1):016x}"
            f"_o{self.ordinal:03d}"
            + {"fixed": "", "history4": "_ih4", "history4_page": "_ih4p"}[self.indirect_target_mode]
            + {
                "none": "",
                "page_tail_pair": "_ptp",
                "page_tail_pair_dense": "_ptpd",
                "page_tail_pair_sparse": "_ptps",
                "taken_cj_offset_ring": "_cjor",
                "taken_cj_offset_ring_permuted": "_cjorp",
            }[self.directed_pattern]
            + (
                f"_pr{self.page_tail_probe_repetitions:02d}"
                if self.page_tail_probe_repetitions is not None
                else ""
            )
        )

    def validate(self) -> None:
        if self.directed_pattern not in {
            "none", "page_tail_pair", "page_tail_pair_dense", "page_tail_pair_sparse",
            "taken_cj_offset_ring", "taken_cj_offset_ring_permuted",
        }:
            raise ValueError("unsupported directed_pattern")
        if self.directed_pattern in {
            "page_tail_pair", "page_tail_pair_dense", "page_tail_pair_sparse",
        } and self.family != "translation_timing_cross":
            raise ValueError("page_tail_pair currently requires translation_timing_cross")
        if self.directed_pattern in {
            "taken_cj_offset_ring", "taken_cj_offset_ring_permuted",
        } and self.family not in {
            "cfg_layout_mix", "translation_timing_cross",
        }:
            raise ValueError("taken_cj_offset_ring requires a CFG or translation family")
        if self.family not in FAMILIES:
            raise ValueError(f"unsupported family: {self.family}")
        if self.seed < 0 or self.seed >= 1 << 64:
            raise ValueError("seed must fit in an unsigned 64-bit integer")
        if self.ordinal < 0:
            raise ValueError("ordinal must be >= 0")
        if self.blocks < 16:
            raise ValueError("blocks must be >= 16")
        if self.iterations < 2:
            raise ValueError("iterations must be >= 2")
        if self.page_tail_probe_repetitions is not None:
            if self.directed_pattern not in {
                "page_tail_pair", "page_tail_pair_dense", "page_tail_pair_sparse",
            }:
                raise ValueError("page_tail_probe_repetitions requires a page-tail pattern")
            if not 1 <= self.page_tail_probe_repetitions < self.iterations:
                raise ValueError(
                    "page_tail_probe_repetitions must be in [1, iterations); "
                    "omit it to preserve the full historical repetition count"
                )
        if not 2 <= self.max_call_depth <= 12:
            raise ValueError("max_call_depth must be in [2, 12]")
        if self.indirect_target_mode not in {"fixed", "history4", "history4_page"}:
            raise ValueError("unsupported indirect_target_mode")
        if self.indirect_target_mode != "fixed" and self.family not in {
            "cfg_layout_mix", "translation_timing_cross",
        }:
            raise ValueError("history4 indirect targets require a CFG or translation family")


class AsmBuilder:
    def __init__(self) -> None:
        self.lines: list[str] = []
        self.logical_instruction_count = 0

    def add(self, *lines: str) -> None:
        self.lines.extend(lines)

    def instruction(self, instruction: str, *, rvi: bool = False) -> None:
        if rvi:
            self.lines.extend(
                ("    .option push", "    .option norvc", f"    {instruction}", "    .option pop")
            )
        else:
            self.lines.append(f"    {instruction}")
        self.logical_instruction_count += 1

    def padding(self, halfwords: int) -> None:
        if halfwords <= 0:
            return
        self.lines.extend(
            (
                f"    .rept {halfwords}",
                "        c.nop",
                "    .endr",
            )
        )
        self.logical_instruction_count += halfwords

    def render(self) -> str:
        return "\n".join(self.lines) + "\n"


def _sha256_bytes(payload: bytes) -> str:
    return hashlib.sha256(payload).hexdigest()


def _case_rng(spec: CaseSpec) -> random.Random:
    mixed_seed = (spec.seed ^ (0x9E3779B97F4A7C15 * (spec.ordinal + 1))) & ((1 << 64) - 1)
    return random.Random(mixed_seed)


def _phase_for(index: int, count: int) -> str:
    # Keep all five phases present even at the minimum supported block count.
    return PHASES[min(len(PHASES) - 1, (index * len(PHASES)) // count)]


def _header(builder: AsmBuilder, spec: CaseSpec, description: str) -> None:
    builder.add(
        f"    # Name              : {spec.name}",
        f"    # Family            : {spec.family}",
        f"    # Seed              : {spec.seed}",
        f"    # Ordinal           : {spec.ordinal}",
        f"    # Blocks/sites      : {spec.blocks}",
        f"    # Iterations        : {spec.iterations}",
        *(
            [f"    # Page-tail repeats : {spec.page_tail_probe_repetitions}"]
            if spec.page_tail_probe_repetitions is not None
            else []
        ),
        f"    # Max call depth    : {spec.max_call_depth}",
        "    # Phases            : warmup -> train -> perturb -> stress -> drain",
        f"    # Goal              : {description}",
        "    # Replay            : regenerate from the adjacent campaign manifest",
        "",
        '    .section .text, "ax"',
        "    .option rvc",
        "    .globl _start",
        "",
        "_start:",
    )
    builder.instruction(f"li s0, {spec.iterations}", rvi=True)
    builder.instruction("li s1, 1", rvi=True)
    builder.instruction("li s2, 0", rvi=True)
    builder.instruction(f"li s3, {spec.seed & 0x7FFFFFFF}", rvi=True)
    builder.instruction("li s4, 0", rvi=True)
    builder.instruction("li sp, 0x80020000", rvi=True)


def _emit_page_tail_probe(
    builder: AsmBuilder,
    target: str,
    *,
    dense: bool = False,
    sparse: bool = False,
) -> None:
    if dense and sparse:
        raise ValueError("page-tail padding cannot be both dense and sparse")
    builder.add(
        "",
        "    # A full-width instruction begins at page offset 4094.",
        "    .balign 4096",
    )
    if dense:
        builder.add(f"{target}:")
        builder.padding(2047)
    elif sparse:
        # Preserve the architectural page-tail address without turning an
        # unreachable address gap into 2047 static instructions.  Control flow
        # enters only at ``target``; NEMU realization must prove the zero-filled
        # hole is never executed before this candidate can advance.
        builder.add("    .zero 4094", f"{target}:")
    else:
        builder.add("    .fill 2047, 2, 0x0001", f"{target}:")
        builder.logical_instruction_count += 2047
    builder.instruction("addi s4, s4, 1", rvi=True)
    builder.instruction("jalr zero, 0(ra)", rvi=True)


def _taken_cj_execution_order(spec: CaseSpec) -> list[int]:
    order = list(range(16))
    if spec.directed_pattern == "taken_cj_offset_ring_permuted":
        permutation_seed = (
            spec.seed ^ 0x434A4F525045524D ^ (0xD1B54A32D192ED03 * (spec.ordinal + 1))
        ) & ((1 << 64) - 1)
        random.Random(permutation_seed).shuffle(order)
    return order


def _emit_taken_cj_offset_ring(
    builder: AsmBuilder, *, execution_order: list[int] | None = None,
) -> list[dict]:
    """Emit one taken C.J at every halfword offset in a 64-byte block."""
    order = list(range(16)) if execution_order is None else list(execution_order)
    if sorted(order) != list(range(16)):
        raise ValueError("taken C.J execution order must be a permutation of offsets 0..15")
    successor = {
        offset: (order[position + 1] if position + 1 < len(order) else None)
        for position, offset in enumerate(order)
    }
    execution_position = {offset: position for position, offset in enumerate(order)}
    episodes: list[dict] = []
    for offset in range(16):
        label = f"cfg_cj_offset_{offset:02d}"
        next_offset = successor[offset]
        target = (
            f"cfg_cj_offset_{next_offset:02d}"
            if next_offset is not None else "cfg_cj_ring_finish"
        )
        builder.add("", "    .balign 64", f"{label}:")
        builder.padding(offset)
        builder.instruction(f"c.j {target}")
        builder.padding(31 - offset)
        episodes.append(
            {
                "episode_id": f"cfg-cj-offset-{offset:02d}",
                "entry_labels": [label],
                "phase": "stress",
                "kind": "taken_cj_offset_ring",
                "parameters": {
                    "block_bytes": 64,
                    "halfword_offset": offset,
                    "execution_position": execution_position[offset],
                    "successor_offset": next_offset,
                },
            }
        )
    builder.add("", "cfg_cj_ring_finish:")
    return episodes


def _emit_finish(builder: AsmBuilder, label: str) -> None:
    builder.add("", f"{label}:")
    for _ in range(8):
        builder.instruction("c.nop")
    builder.instruction("li a0, 0", rvi=True)
    builder.instruction(".word 0x0005006b", rvi=True)
    # The Frontend can still have speculative fetches in flight after the
    # golden model reaches good-trap. Keep the drain window decodable, matching
    # the established directed ASM convention.
    builder.padding(128)


def _emit_cfg_block(
    builder: AsmBuilder,
    *,
    index: int,
    count: int,
    phase: str,
    kind: str,
    alignment: int,
    cfi_offset: int,
    next_label: str,
    branch_mask: int,
    indirect_target_mode: str = "fixed",
    directed_pattern: str = "none",
    page_tail_probe_repetitions: int | None = None,
) -> None:
    label = f"cfg_{index:03d}"
    taken = f"{label}_taken"
    join = f"{label}_join"
    local_call = f"{label}_call"

    builder.add("", f"    # phase={phase} kind={kind} cfi_offset={cfi_offset}")
    builder.add(f"    .balign {alignment}", f"{label}:")
    builder.padding(cfi_offset // 2)

    if kind == "branch_rvi":
        builder.instruction(f"andi s6, s0, {branch_mask}", rvi=True)
        builder.instruction(f"beq s6, zero, {taken}", rvi=True)
        builder.instruction("addi s1, s1, 3", rvi=True)
        builder.instruction(f"jal zero, {join}", rvi=True)
        builder.add(f"{taken}:")
        builder.instruction("addi s1, s1, -1", rvi=True)
        builder.add(f"{join}:")
    elif kind == "branch_rvc":
        builder.instruction(f"andi a4, s0, {branch_mask}", rvi=True)
        builder.instruction(f"c.beqz a4, {taken}")
        builder.instruction("c.addi s1, 1")
        builder.instruction(f"c.j {join}")
        builder.add(f"{taken}:")
        builder.instruction("c.addi s1, -1")
        builder.add(f"{join}:")
    elif kind == "jal":
        builder.instruction(f"jal zero, {join}", rvi=True)
        builder.instruction("addi s2, s2, 7", rvi=True)
        builder.add(f"{join}:")
    elif kind == "c_j":
        builder.instruction(f"c.j {join}")
        builder.instruction("c.addi s2, 7")
        builder.add(f"{join}:")
    elif kind == "jalr":
        if indirect_target_mode in {"history4", "history4_page"}:
            # Seeded arithmetic history and loop phase select four legal code
            # targets at one JALR site without data loads or host-side forcing.
            builder.instruction("srli t1, s3, 7", rvi=True)
            builder.instruction("xor t1, t1, s0", rvi=True)
            builder.instruction("andi t1, t1, 3", rvi=True)
            target_shift = 12 if indirect_target_mode == "history4_page" else 5
            builder.instruction(f"slli t1, t1, {target_shift}", rvi=True)
            builder.instruction(f"la t0, {label}_target_0", rvi=True)
            builder.instruction("add t0, t0, t1", rvi=True)
        else:
            builder.instruction(f"la t0, {join}", rvi=True)
        builder.instruction("jalr zero, 0(t0)", rvi=True)
        builder.instruction("addi s2, s2, 7", rvi=True)
        if indirect_target_mode in {"history4", "history4_page"}:
            for target in range(4):
                target_alignment = 4096 if indirect_target_mode == "history4_page" else 32
                builder.add(f"    .balign {target_alignment}", f"{label}_target_{target}:")
                builder.instruction(f"addi s2, s2, {target + 1}", rvi=True)
                builder.instruction(f"jal zero, {join}", rvi=True)
        builder.add(f"{join}:")
    elif kind == "call_ret":
        builder.instruction(f"jal ra, {local_call}", rvi=True)
        builder.instruction(f"jal zero, {join}", rvi=True)
        builder.add(f"{local_call}:")
        builder.instruction("addi s3, s3, 1", rvi=True)
        builder.instruction("jalr zero, 0(ra)", rvi=True)
        builder.add(f"{join}:")
    else:
        raise ValueError(f"unsupported cfg block kind: {kind}")

    if index % 3 == 0:
        builder.instruction("c.xor s1, s0")
    elif index % 3 == 1:
        builder.instruction("addi s3, s3, 17", rvi=True)
    else:
        builder.instruction("c.addi s4, 1")

    if index == count - 1:
        if directed_pattern in {
            "page_tail_pair", "page_tail_pair_dense", "page_tail_pair_sparse",
        }:
            if page_tail_probe_repetitions is not None:
                builder.instruction(f"li t6, {page_tail_probe_repetitions}", rvi=True)
                builder.instruction("bltu t6, s0, cfg_page_pattern_skip", rvi=True)
            builder.instruction("jal ra, cfg_page_rvc_probe", rvi=True)
            builder.instruction("jal ra, cfg_page_tail_probe", rvi=True)
            if page_tail_probe_repetitions is not None:
                builder.add("cfg_page_pattern_skip:")
        builder.instruction("addi s0, s0, -1", rvi=True)
        builder.instruction("bne s0, zero, cfg_000", rvi=True)
        builder.instruction(f"jal zero, {next_label}", rvi=True)
    elif kind in {"branch_rvc", "c_j"}:
        builder.instruction(f"c.j {next_label}")
    else:
        builder.instruction(f"jal zero, {next_label}", rvi=True)


def _generate_cfg_layout_mix(spec: CaseSpec) -> tuple[str, dict]:
    rng = _case_rng(spec)
    builder = AsmBuilder()
    _header(
        builder,
        spec,
        "long mixed RVI/RVC CFG with branch history, indirect control flow, calls, and fetch boundaries",
    )
    builder.instruction("jal zero, cfg_000", rvi=True)

    kinds = ("branch_rvi", "branch_rvc", "jal", "c_j", "jalr", "call_ret")
    phase_counts = {phase: 0 for phase in PHASES}
    kind_counts = {kind: 0 for kind in kinds}
    boundary_offsets: list[int] = []
    episodes: list[dict] = []
    for index in range(spec.blocks):
        phase = _phase_for(index, spec.blocks)
        phase_counts[phase] += 1
        if phase == "warmup":
            kind = rng.choice(("branch_rvi", "jal", "call_ret"))
        elif phase == "train":
            kind = rng.choice(("branch_rvi", "branch_rvc", "branch_rvi", "call_ret"))
        else:
            kind = rng.choice(kinds)
        kind_counts[kind] += 1

        alignment = rng.choice((16, 32, 32, 64))
        if phase == "stress" or index % 7 == 0:
            cfi_offset = rng.choice((14, 16, 28, 30))
        else:
            cfi_offset = rng.randrange(0, 32, 2)
        boundary_offsets.append(cfi_offset)
        episodes.append(
            {
                "episode_id": f"cfg-{index:03d}",
                "entry_labels": [f"cfg_{index:03d}"],
                "phase": phase,
                "kind": kind,
                "parameters": {
                    "alignment": alignment,
                    "cfi_offset": cfi_offset,
                },
            }
        )
        if kind == "jalr" and spec.indirect_target_mode in {"history4", "history4_page"}:
            episodes[-1]["parameters"]["indirect_target_mode"] = spec.indirect_target_mode
            episodes[-1]["parameters"]["target_count"] = 4
            if spec.indirect_target_mode == "history4_page":
                episodes[-1]["parameters"]["target_stride_bytes"] = 4096
        next_label = f"cfg_{index + 1:03d}" if index + 1 < spec.blocks else (
            "cfg_cj_ring_entry"
            if spec.directed_pattern in {
                "taken_cj_offset_ring", "taken_cj_offset_ring_permuted",
            }
            else "cfg_page_probe_call"
        )
        _emit_cfg_block(
            builder,
            index=index,
            count=spec.blocks,
            phase=phase,
            kind=kind,
            alignment=alignment,
            cfi_offset=cfi_offset,
            next_label=next_label,
            branch_mask=rng.choice((1, 1, 3, 7)),
            indirect_target_mode=spec.indirect_target_mode,
            directed_pattern=spec.directed_pattern,
            page_tail_probe_repetitions=spec.page_tail_probe_repetitions,
        )

    ring_episodes: list[dict] = []
    if spec.directed_pattern in {
        "taken_cj_offset_ring", "taken_cj_offset_ring_permuted",
    }:
        cj_order = _taken_cj_execution_order(spec)
        builder.add("", "cfg_cj_ring_entry:")
        builder.instruction(f"jal zero, cfg_cj_offset_{cj_order[0]:02d}", rvi=True)
        ring_episodes = _emit_taken_cj_offset_ring(builder, execution_order=cj_order)
        builder.instruction("jal zero, cfg_page_probe_call", rvi=True)
    builder.add("", "cfg_page_probe_call:")
    builder.instruction("jal ra, cfg_page_tail_probe", rvi=True)
    builder.instruction("jal zero, cfg_finish", rvi=True)
    dense = spec.directed_pattern == "page_tail_pair_dense"
    sparse = spec.directed_pattern == "page_tail_pair_sparse"
    if spec.directed_pattern in {
        "page_tail_pair", "page_tail_pair_dense", "page_tail_pair_sparse",
    }:
        builder.add("", "    .balign 4096")
        if dense:
            builder.add("cfg_page_rvc_probe:")
            builder.padding(2047)
        elif sparse:
            builder.add("    .zero 4094", "cfg_page_rvc_probe:")
        else:
            builder.add("    .fill 2047, 2, 0x0001", "cfg_page_rvc_probe:")
            builder.logical_instruction_count += 2047
        builder.instruction("c.nop")
        builder.instruction("addi s4, s4, 1", rvi=True)
        builder.instruction("jalr zero, 0(ra)", rvi=True)
        episodes.append({
            "episode_id": "cfg-page-rvc-probe", "entry_labels": ["cfg_page_rvc_probe"],
            "phase": "stress", "kind": "page_tail_rvc",
            "parameters": {
                "instruction_page_offset": 4094,
                "repetitions": (
                    spec.page_tail_probe_repetitions
                    if spec.page_tail_probe_repetitions is not None
                    else spec.iterations
                ),
                "padding_kind": "zero_hole" if sparse else "executed_nop" if dense else "nop_hole",
            },
        })
    _emit_page_tail_probe(builder, "cfg_page_tail_probe", dense=dense, sparse=sparse)
    _emit_finish(builder, "cfg_finish")
    episodes.append(
        {
            "episode_id": "cfg-page-tail-probe",
            "entry_labels": ["cfg_page_probe_call", "cfg_page_tail_probe"],
            "phase": "stress",
            "kind": "page_crossing_rvi",
            "parameters": {
                "instruction_page_offset": 4094,
                "padding_kind": "zero_hole" if sparse else "executed_nop" if dense else "nop_hole",
            },
        }
    )

    metadata = {
        "phase_counts": phase_counts,
        "kind_counts": kind_counts,
        "boundary_offsets": boundary_offsets,
        "page_tail_probe": True,
        "episodes": [*episodes, *ring_episodes],
    }
    return builder.render(), {**metadata, "logical_static_instructions": builder.logical_instruction_count}


def _emit_ras_function(
    builder: AsmBuilder,
    *,
    chain: int,
    depth: int,
    max_depth: int,
    padding_halfwords: int,
    alignment: int,
    branch_mask: int,
) -> None:
    label = f"ras_chain_{chain:03d}_d{depth:02d}"
    branch_target = f"{label}_branch"
    branch_join = f"{label}_join"
    alt_leaf = f"{label}_alt_leaf"

    builder.add("", f"    .balign {alignment}", f"{label}:")
    builder.padding(padding_halfwords)
    builder.instruction("addi sp, sp, -16", rvi=True)
    builder.instruction("sd ra, 8(sp)", rvi=True)
    builder.instruction(f"andi s6, s0, {branch_mask}", rvi=True)
    builder.instruction(f"beq s6, zero, {branch_target}", rvi=True)
    builder.instruction("addi s2, s2, 1", rvi=True)
    builder.instruction(f"jal zero, {branch_join}", rvi=True)
    builder.add(f"{branch_target}:")
    builder.instruction("addi s2, s2, -1", rvi=True)
    builder.add(f"{branch_join}:")

    if depth + 1 < max_depth:
        builder.instruction(f"jal ra, ras_chain_{chain:03d}_d{depth + 1:02d}", rvi=True)
    else:
        builder.instruction(f"jal t0, {alt_leaf}", rvi=True)
        builder.instruction(f"jal zero, {alt_leaf}_done", rvi=True)
        builder.add(f"{alt_leaf}:")
        builder.instruction("c.addi s3, 1")
        builder.instruction("jalr zero, 0(t0)", rvi=True)
        builder.add(f"{alt_leaf}_done:")

    builder.instruction("ld ra, 8(sp)", rvi=True)
    builder.instruction("addi sp, sp, 16", rvi=True)
    builder.instruction("jalr zero, 0(ra)", rvi=True)


def _generate_ras_indirect_history(spec: CaseSpec) -> tuple[str, dict]:
    rng = _case_rng(spec)
    builder = AsmBuilder()
    _header(
        builder,
        spec,
        "nested RAS traffic mixed with x5 link calls, returns, branches, indirect jumps, and target boundaries",
    )
    builder.instruction("jal zero, ras_main_loop", rvi=True)
    builder.add("", "ras_main_loop:")

    depths: list[int] = []
    episodes: list[dict] = []
    for chain in range(spec.blocks):
        phase = _phase_for(chain, spec.blocks)
        depth = rng.randint(2, spec.max_call_depth)
        depths.append(depth)
        builder.add("", f"    # phase={phase} call_chain={chain} depth={depth}")
        if chain % 4 == 0:
            builder.add("    .balign 64")
            builder.padding(rng.choice((7, 14, 15)))
        builder.add(f"ras_site_{chain:03d}:")
        builder.instruction(f"jal ra, ras_chain_{chain:03d}_d00", rvi=True)
        if chain % 3 == 0:
            indirect_target = f"ras_main_indirect_{chain:03d}"
            builder.instruction(f"la t1, {indirect_target}", rvi=True)
            builder.instruction("jalr zero, 0(t1)", rvi=True)
            builder.instruction("addi s4, s4, 9", rvi=True)
            builder.add(f"{indirect_target}:")
        builder.instruction("c.addi s4, 1")
        episodes.append(
            {
                "episode_id": f"ras-chain-{chain:03d}",
                "entry_labels": [
                    f"ras_site_{chain:03d}",
                    *[f"ras_chain_{chain:03d}_d{level:02d}" for level in range(depth)],
                ],
                "phase": phase,
                "kind": "nested_call_chain",
                "parameters": {"call_depth": depth},
            }
        )

    builder.instruction("addi s0, s0, -1", rvi=True)
    builder.instruction("bne s0, zero, ras_main_loop", rvi=True)
    builder.instruction("jal ra, ras_page_tail_probe", rvi=True)
    builder.instruction("jal zero, ras_finish", rvi=True)

    for chain, depth in enumerate(depths):
        for level in range(depth):
            _emit_ras_function(
                builder,
                chain=chain,
                depth=level,
                max_depth=depth,
                padding_halfwords=rng.choice((0, 1, 7, 14, 15)),
                alignment=rng.choice((16, 32, 64)),
                branch_mask=rng.choice((1, 3, 7)),
            )

    _emit_page_tail_probe(builder, "ras_page_tail_probe")
    _emit_finish(builder, "ras_finish")
    episodes.append(
        {
            "episode_id": "ras-page-tail-probe",
            "entry_labels": ["ras_page_tail_probe"],
            "phase": "stress",
            "kind": "page_crossing_rvi",
            "parameters": {"instruction_page_offset": 4094},
        }
    )

    metadata = {
        "call_depths": depths,
        "max_generated_call_depth": max(depths),
        "page_tail_probe": True,
        "episodes": episodes,
        "phase_counts": {
            phase: sum(_phase_for(index, spec.blocks) == phase for index in range(spec.blocks))
            for phase in PHASES
        },
    }
    return builder.render(), {**metadata, "logical_static_instructions": builder.logical_instruction_count}


def _emit_ibuffer_pressure_site(
    builder: AsmBuilder,
    *,
    index: int,
    phase: str,
    kind: str,
    cfi_offset: int,
    burst_length: int,
    next_label: str,
) -> None:
    label = f"ibp_{index:03d}"
    taken = f"{label}_taken"
    body = f"{label}_body"
    helper = f"{label}_helper"

    builder.add(
        "",
        f"    # phase={phase} kind={kind} cfi_offset={cfi_offset} burst={burst_length}",
        "    .balign 64",
        f"{label}:",
    )
    builder.padding(cfi_offset // 2)
    if kind == "branch_rvi":
        builder.instruction("andi s1, s0, 1", rvi=True)
        builder.instruction(f"beq s1, zero, {taken}", rvi=True)
        builder.instruction("addi s2, s2, 3", rvi=True)
        builder.instruction(f"jal zero, {body}", rvi=True)
        builder.add(f"{taken}:")
        builder.instruction("addi s2, s2, -1", rvi=True)
    elif kind == "branch_rvc":
        builder.instruction("andi s1, s0, 1", rvi=True)
        builder.instruction(f"c.beqz s1, {taken}")
        builder.instruction("c.addi s2, 3")
        builder.instruction(f"jal zero, {body}", rvi=True)
        builder.add(f"{taken}:")
        builder.instruction("c.addi s2, -1")
    elif kind == "call_ret":
        builder.instruction(f"jal ra, {helper}", rvi=True)
        builder.instruction(f"jal zero, {body}", rvi=True)
        builder.add(f"{helper}:")
        builder.instruction("addi s3, s3, 1", rvi=True)
        builder.instruction("jalr zero, 0(ra)", rvi=True)
    elif kind == "jalr":
        builder.instruction(f"la t1, {body}", rvi=True)
        builder.instruction("jalr zero, 0(t1)", rvi=True)
    else:
        builder.instruction(f"jal zero, {body}", rvi=True)

    builder.add(f"{body}:")
    for slot in range(int(burst_length)):
        selector = (int(slot) + int(index)) % 4
        if selector == 0:
            builder.instruction("c.addi s2, 1")
        elif selector == 1:
            builder.instruction("addi s3, s3, 3", rvi=True)
        elif selector == 2:
            builder.instruction("c.addi s4, -1")
        else:
            builder.instruction("xor s5, s5, s3", rvi=True)
    builder.instruction(f"jal zero, {next_label}", rvi=True)


def _generate_ibuffer_pressure_cross(spec: CaseSpec) -> tuple[str, dict]:
    rng = _case_rng(spec)
    builder = AsmBuilder()
    _header(
        builder,
        spec,
        "long dense instruction bursts around boundary CFIs for IBuffer occupancy, backpressure, redirect, and response crosses",
    )
    builder.instruction("jal zero, ibp_main_loop", rvi=True)
    builder.add("", "ibp_main_loop:")

    kinds = ("branch_rvi", "branch_rvc", "call_ret", "jalr", "jal")
    kind_counts = {kind: 0 for kind in kinds}
    boundary_offsets: list[int] = []
    burst_lengths: list[int] = []
    episodes: list[dict] = []
    for index in range(spec.blocks):
        phase = _phase_for(index, spec.blocks)
        kind = rng.choice(kinds)
        if phase in {"train", "stress"} and index % 2 == 0:
            kind = rng.choice(("branch_rvi", "branch_rvc", "call_ret"))
        cfi_offset = rng.choice((14, 28, 30, 60, 62))
        burst_length = rng.choice((32, 40, 48, 64, 80, 96))
        kind_counts[kind] += 1
        boundary_offsets.append(cfi_offset)
        burst_lengths.append(burst_length)
        next_label = f"ibp_{index + 1:03d}" if index + 1 < spec.blocks else "ibp_loop_tail"
        episodes.append(
            {
                "episode_id": f"ibuffer-pressure-{index:03d}",
                "entry_labels": [f"ibp_{index:03d}"],
                "phase": phase,
                "kind": "ibuffer_pressure_burst",
                "parameters": {
                    "cfi_kind": kind,
                    "cfi_offset": cfi_offset,
                    "burst_length": burst_length,
                },
            }
        )
        _emit_ibuffer_pressure_site(
            builder,
            index=index,
            phase=phase,
            kind=kind,
            cfi_offset=cfi_offset,
            burst_length=burst_length,
            next_label=next_label,
        )

    builder.add("", "ibp_loop_tail:")
    builder.instruction("addi s0, s0, -1", rvi=True)
    builder.instruction("bne s0, zero, ibp_main_loop", rvi=True)
    builder.instruction("jal ra, ibp_page_tail_probe", rvi=True)
    builder.instruction("jal zero, ibp_finish", rvi=True)
    _emit_page_tail_probe(builder, "ibp_page_tail_probe")
    _emit_finish(builder, "ibp_finish")
    episodes.append(
        {
            "episode_id": "ibuffer-pressure-page-tail",
            "entry_labels": ["ibp_page_tail_probe"],
            "phase": "stress",
            "kind": "page_crossing_rvi",
            "parameters": {"instruction_page_offset": 4094},
        }
    )
    metadata = {
        "kind_counts": kind_counts,
        "boundary_offsets": boundary_offsets,
        "pressure_burst_lengths": burst_lengths,
        "maximum_pressure_burst": max(burst_lengths),
        "page_tail_probe": True,
        "episodes": episodes,
        "phase_counts": {
            phase: sum(_phase_for(index, spec.blocks) == phase for index in range(spec.blocks))
            for phase in PHASES
        },
    }
    return builder.render(), {
        **metadata,
        "logical_static_instructions": builder.logical_instruction_count,
    }


def generate_case(spec: CaseSpec) -> tuple[str, dict]:
    spec.validate()
    if spec.family == "cfg_layout_mix":
        source, family_metadata = _generate_cfg_layout_mix(spec)
    elif spec.family == "ras_indirect_history":
        source, family_metadata = _generate_ras_indirect_history(spec)
    elif spec.family == "ibuffer_pressure_cross":
        source, family_metadata = _generate_ibuffer_pressure_cross(spec)
    elif spec.family == "translation_timing_cross":
        # The instruction stream stays long and CFI-rich; the DUT-side
        # translation profile supplies the independent VA/PA/PTW dimensions.
        source, family_metadata = _generate_cfg_layout_mix(spec)
        family_metadata = {
            **family_metadata,
            "translation_timing_profile": "translation_timing_cross",
        }
    else:
        raise ValueError(f"unsupported family: {spec.family}")

    source_bytes = source.encode("ascii")
    metadata = {
        "schema_version": GENERATOR_SCHEMA_VERSION,
        "name": spec.name,
        "spec": asdict(spec),
        "source_sha256": _sha256_bytes(source_bytes),
        "generator_sha256": _sha256_bytes(Path(__file__).read_bytes()),
        **family_metadata,
    }
    return source, metadata


def write_cases(specs: Iterable[CaseSpec], out_dir: Path) -> list[dict]:
    out_dir.mkdir(parents=True, exist_ok=True)
    records: list[dict] = []
    for spec in specs:
        source, metadata = generate_case(spec)
        source_path = out_dir / f"{spec.name}.S"
        metadata_path = out_dir / f"{spec.name}.generator.json"
        source_path.write_text(source, encoding="ascii")
        metadata_path.write_text(json.dumps(metadata, indent=2, sort_keys=True) + "\n", encoding="ascii")
        records.append(
            {
                **metadata,
                "source_path": str(source_path.resolve()),
                "generator_manifest_path": str(metadata_path.resolve()),
            }
        )
    return records


def _int_auto(value: str) -> int:
    return int(value, 0)


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate replayable long Frontend random ASM cases")
    parser.add_argument("--family", choices=FAMILIES, default="cfg_layout_mix")
    parser.add_argument("--seed", type=_int_auto, required=True)
    parser.add_argument("--count", type=int, default=1)
    parser.add_argument("--ordinal-start", type=int, default=0)
    parser.add_argument("--blocks", type=int, default=48)
    parser.add_argument("--iterations", type=int, default=10)
    parser.add_argument("--max-call-depth", type=int, default=6)
    parser.add_argument("--indirect-target-mode", choices=("fixed", "history4", "history4_page"), default="fixed")
    parser.add_argument(
        "--directed-pattern",
        choices=(
            "none", "page_tail_pair", "page_tail_pair_dense", "page_tail_pair_sparse",
            "taken_cj_offset_ring", "taken_cj_offset_ring_permuted",
        ),
        default="none",
    )
    parser.add_argument(
        "--page-tail-probe-repetitions",
        type=int,
        default=None,
        help=(
            "execute the in-loop page-tail pair only this many times; must be "
            "less than --iterations, and omission preserves historical behavior"
        ),
    )
    parser.add_argument(
        "--out-dir",
        type=Path,
        default=Path(__file__).resolve().parent / "generated",
    )
    parser.add_argument("--manifest", type=Path, default=None)
    return parser.parse_args()


def main() -> int:
    args = _parse_args()
    if args.count <= 0:
        raise SystemExit("--count must be > 0")
    specs = [
        CaseSpec(
            family=args.family,
            seed=(args.seed + ordinal) & ((1 << 64) - 1),
            ordinal=args.ordinal_start + ordinal,
            blocks=args.blocks,
            iterations=args.iterations,
            max_call_depth=args.max_call_depth,
            indirect_target_mode=args.indirect_target_mode,
            directed_pattern=args.directed_pattern,
            page_tail_probe_repetitions=args.page_tail_probe_repetitions,
        )
        for ordinal in range(args.count)
    ]
    records = write_cases(specs, args.out_dir)
    manifest_path = args.manifest or args.out_dir / "generated_cases.json"
    manifest_path.parent.mkdir(parents=True, exist_ok=True)
    manifest_path.write_text(
        json.dumps(
            {
                "schema_version": GENERATOR_SCHEMA_VERSION,
                "generator": str(Path(__file__).resolve()),
                "cases": records,
            },
            indent=2,
            sort_keys=True,
        )
        + "\n",
        encoding="ascii",
    )
    for record in records:
        print(record["source_path"])
    print(f"manifest={manifest_path.resolve()}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

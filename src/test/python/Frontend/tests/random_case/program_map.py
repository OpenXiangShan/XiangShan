#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
import re
import shutil
import subprocess
import tempfile
from collections import Counter, defaultdict
from pathlib import Path
from typing import Any, Iterable

try:
    from .scenario_contract import RealizedEvent, write_json_contract
except ImportError:
    from scenario_contract import RealizedEvent, write_json_contract


PROGRAM_MAP_SCHEMA_VERSION = 1
TRACE_ANALYSIS_SCHEMA_VERSION = 1
FETCH_BLOCK_BYTES = 32
CACHELINE_BYTES = 64
PAGE_BYTES = 4096

_LABEL_RE = re.compile(r"^\s*([0-9a-fA-F]+)\s+<([^>]+)>:$")
_INSTRUCTION_RE = re.compile(
    r"^\s*([0-9a-fA-F]+):\s+([0-9a-fA-F]{4,16})\s+([^\s]+)(?:\s+(.*?))?\s*$"
)
_BRANCH_MNEMONICS = frozenset(
    {
        "beq",
        "bne",
        "blt",
        "bge",
        "bltu",
        "bgeu",
        "c.beqz",
        "c.bnez",
    }
)
_DIRECT_JUMP_MNEMONICS = frozenset({"jal", "c.j", "c.jal"})
_INDIRECT_JUMP_MNEMONICS = frozenset({"jalr", "c.jr", "c.jalr"})
_LINK_REGISTERS = frozenset({"x1", "x5", "ra", "t0"})
_ZERO_REGISTERS = frozenset({"x0", "zero"})


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _sha256_bytes(payload: bytes) -> str:
    return hashlib.sha256(payload).hexdigest()


def _resolve_tool(explicit: str | None, candidates: Iterable[str]) -> str:
    if explicit:
        resolved = shutil.which(explicit)
        if resolved:
            return resolved
        raise ValueError(f"tool is not executable or not on PATH: {explicit}")
    for candidate in candidates:
        resolved = shutil.which(candidate)
        if resolved:
            return resolved
    raise ValueError(f"none of the required tools are on PATH: {', '.join(candidates)}")


def _tool_version(path: str) -> str:
    result = subprocess.run(
        [path, "--version"],
        check=False,
        capture_output=True,
        text=True,
        timeout=10,
    )
    lines = result.stdout.splitlines()
    return lines[0].strip() if lines else "unavailable"


def parse_disassembly(text: str) -> tuple[list[dict[str, Any]], dict[str, int]]:
    instructions: list[dict[str, Any]] = []
    labels: dict[str, int] = {}
    labels_at_pc: dict[int, list[str]] = {}
    for line in text.splitlines():
        label_match = _LABEL_RE.match(line)
        if label_match:
            pc = int(label_match.group(1), 16)
            label = label_match.group(2)
            if label in labels and labels[label] != pc:
                raise ValueError(
                    f"conflicting disassembly label {label!r}: "
                    f"0x{labels[label]:x} and 0x{pc:x}"
                )
            labels[label] = pc
            labels_at_pc.setdefault(pc, []).append(label)
            continue
        instruction_match = _INSTRUCTION_RE.match(line)
        if not instruction_match:
            continue
        pc = int(instruction_match.group(1), 16)
        encoding = instruction_match.group(2).lower()
        size = len(encoding) // 2
        if size not in {2, 4, 6, 8}:
            raise ValueError(f"unsupported instruction width at 0x{pc:x}: {encoding}")
        mnemonic = instruction_match.group(3)
        operands = (instruction_match.group(4) or "").split("#", 1)[0].strip()
        instructions.append(
            {
                "pc": pc,
                "encoding": encoding,
                "size": size,
                "mnemonic": mnemonic,
                "operands": operands,
                "labels": [],
            }
        )
    for instruction in instructions:
        instruction["labels"] = sorted(labels_at_pc.get(int(instruction["pc"]), []))
    previous_end: int | None = None
    previous_pc: int | None = None
    for instruction in instructions:
        pc = int(instruction["pc"])
        end = pc + int(instruction["size"])
        if previous_end is not None and pc < previous_end:
            raise ValueError(
                f"out-of-order, overlapping, or duplicate disassembly instruction at 0x{pc:x}; "
                f"previous instruction starts at 0x{previous_pc:x} and ends at 0x{previous_end:x}"
            )
        previous_pc = pc
        previous_end = end
    return instructions, labels


def _register_operands(operands: str) -> list[str]:
    return [item.strip().split("(", 1)[-1].rstrip(")") for item in operands.split(",")]


def _classify_instruction(instruction: dict[str, Any]) -> dict[str, bool]:
    mnemonic = str(instruction["mnemonic"])
    operands = str(instruction["operands"])
    registers = _register_operands(operands)
    conditional_branch = mnemonic in _BRANCH_MNEMONICS
    direct_jump = mnemonic in _DIRECT_JUMP_MNEMONICS
    indirect_jump = mnemonic in _INDIRECT_JUMP_MNEMONICS
    call = False
    return_instruction = False
    if mnemonic == "jal" and registers:
        call = registers[0] in _LINK_REGISTERS
    elif mnemonic == "c.jal":
        call = True
    elif mnemonic == "jalr" and registers:
        destination = registers[0]
        source = registers[-1]
        call = destination in _LINK_REGISTERS
        return_instruction = destination in _ZERO_REGISTERS and source in _LINK_REGISTERS
    elif mnemonic == "c.jalr":
        call = True
    elif mnemonic == "c.jr" and registers:
        return_instruction = registers[0] in _LINK_REGISTERS
    return {
        "conditional_branch": conditional_branch,
        "direct_jump": direct_jump,
        "indirect_jump": indirect_jump,
        "call": call,
        "return": return_instruction,
    }


def _boundary_attributes(pc: int, size: int) -> dict[str, Any]:
    return {
        "fetch_block_offset": pc % FETCH_BLOCK_BYTES,
        "cacheline_offset": pc % CACHELINE_BYTES,
        "page_offset": pc % PAGE_BYTES,
        "crosses_fetch_block": pc // FETCH_BLOCK_BYTES != (pc + size - 1) // FETCH_BLOCK_BYTES,
        "crosses_cacheline": pc // CACHELINE_BYTES != (pc + size - 1) // CACHELINE_BYTES,
        "crosses_page": pc // PAGE_BYTES != (pc + size - 1) // PAGE_BYTES,
    }


def _episode_regions(
    generator_metadata: dict[str, Any],
    labels: dict[str, int],
    text_end: int,
) -> list[dict[str, Any]]:
    anchors: list[dict[str, Any]] = []
    unresolved: list[str] = []
    for episode in generator_metadata.get("episodes") or []:
        for label in episode.get("entry_labels") or []:
            if label not in labels:
                unresolved.append(str(label))
                continue
            anchors.append(
                {
                    "episode_id": str(episode["episode_id"]),
                    "entry_label": str(label),
                    "start_pc": int(labels[label]),
                    "phase": str(episode.get("phase") or ""),
                    "kind": str(episode.get("kind") or ""),
                    "parameters": dict(episode.get("parameters") or {}),
                }
            )
    if unresolved:
        raise ValueError("episode labels missing from linked ELF: " + ", ".join(sorted(unresolved)))
    anchors.sort(key=lambda item: (item["start_pc"], item["episode_id"], item["entry_label"]))
    for index, anchor in enumerate(anchors):
        anchor["end_pc_exclusive"] = (
            int(anchors[index + 1]["start_pc"]) if index + 1 < len(anchors) else text_end
        )
    return anchors


def _attach_episode_ids(
    instructions: list[dict[str, Any]],
    regions: list[dict[str, Any]],
) -> None:
    region_index = 0
    for instruction in instructions:
        pc = int(instruction["pc"])
        while (
            region_index + 1 < len(regions)
            and pc >= int(regions[region_index]["end_pc_exclusive"])
        ):
            region_index += 1
        if regions and int(regions[region_index]["start_pc"]) <= pc < int(
            regions[region_index]["end_pc_exclusive"]
        ):
            instruction["episode_id"] = regions[region_index]["episode_id"]
            instruction["phase"] = regions[region_index]["phase"]
            instruction["episode_kind"] = regions[region_index]["kind"]
            instruction["episode_parameters"] = dict(regions[region_index]["parameters"])
        else:
            instruction["episode_id"] = ""
            instruction["phase"] = ""
            instruction["episode_kind"] = ""
            instruction["episode_parameters"] = {}


def build_program_map(
    source_path: Path,
    binary_path: Path,
    generator_metadata: dict[str, Any],
    *,
    link_address: int,
    binary_base: int,
    gcc: str | None = None,
    objcopy: str | None = None,
    objdump: str | None = None,
) -> dict[str, Any]:
    source_path = source_path.resolve()
    binary_path = binary_path.resolve()
    if not source_path.is_file() or not binary_path.is_file():
        raise ValueError("program map requires an existing source and linked binary")
    if link_address < binary_base:
        raise ValueError("link_address must be >= binary_base")

    gcc_path = _resolve_tool(
        gcc,
        ("riscv64-unknown-elf-gcc", "riscv64-linux-gnu-gcc", "riscv64-unknown-linux-gnu-gcc"),
    )
    objcopy_path = _resolve_tool(
        objcopy,
        (
            "riscv64-unknown-elf-objcopy",
            "riscv64-linux-gnu-objcopy",
            "riscv64-unknown-linux-gnu-objcopy",
        ),
    )
    objdump_path = _resolve_tool(
        objdump,
        (
            "riscv64-unknown-elf-objdump",
            "riscv64-linux-gnu-objdump",
            "riscv64-unknown-linux-gnu-objdump",
        ),
    )

    with tempfile.TemporaryDirectory(prefix="frontend-program-map.") as temporary:
        temporary_root = Path(temporary)
        elf_path = temporary_root / "case.elf"
        raw_path = temporary_root / "case.text.bin"
        compile_command = [
            gcc_path,
            "-march=rv64gc_zicsr_zifencei",
            "-mabi=lp64",
            "-static",
            "-no-pie",
            "-nostdlib",
            "-nostartfiles",
            "-Wl,--build-id=none",
            f"-Wl,-Ttext=0x{link_address:x}",
            "-Wl,-e,_start",
            "-o",
            str(elf_path),
            str(source_path),
        ]
        subprocess.run(compile_command, check=True, capture_output=True, text=True)
        subprocess.run(
            [objcopy_path, "-O", "binary", "-j", ".text", str(elf_path), str(raw_path)],
            check=True,
            capture_output=True,
            text=True,
        )
        disassembly = subprocess.run(
            [objdump_path, "-d", "-M", "no-aliases,numeric", str(elf_path)],
            check=True,
            capture_output=True,
            text=True,
        ).stdout
        raw_text = raw_path.read_bytes()

    binary = binary_path.read_bytes()
    text_offset = link_address - binary_base
    binary_text = binary[text_offset : text_offset + len(raw_text)]
    if binary_text != raw_text:
        raise ValueError(
            "independently linked .text does not match the DUT/NEMU binary; "
            "refusing to build an untrusted ProgramMap"
        )

    instructions, labels = parse_disassembly(disassembly)
    if not instructions:
        raise ValueError("objdump produced no instructions")
    text_start = min(int(item["pc"]) for item in instructions)
    text_end = max(int(item["pc"]) + int(item["size"]) for item in instructions)
    regions = _episode_regions(generator_metadata, labels, text_end)
    _attach_episode_ids(instructions, regions)

    feature_counts: Counter[str] = Counter()
    for instruction in instructions:
        pc = int(instruction["pc"])
        size = int(instruction["size"])
        classification = _classify_instruction(instruction)
        boundary = _boundary_attributes(pc, size)
        instruction.update(classification)
        instruction.update(boundary)
        feature_counts["rvc_instruction" if size == 2 else "rvi_instruction"] += 1
        for feature, present in {**classification, **boundary}.items():
            if isinstance(present, bool) and present:
                feature_counts[feature] += 1

    return {
        "schema_version": PROGRAM_MAP_SCHEMA_VERSION,
        "source_path": str(source_path),
        "source_sha256": _sha256(source_path),
        "binary_path": str(binary_path),
        "binary_sha256": _sha256(binary_path),
        "binary_base": binary_base,
        "link_address": link_address,
        "text_offset_in_binary": text_offset,
        "text_size_bytes": len(raw_text),
        "text_sha256": _sha256_bytes(raw_text),
        "binary_text_verified": True,
        "generator_sha256": str(generator_metadata.get("generator_sha256") or ""),
        "toolchain": {
            "gcc": {"path": gcc_path, "version": _tool_version(gcc_path)},
            "objcopy": {"path": objcopy_path, "version": _tool_version(objcopy_path)},
            "objdump": {"path": objdump_path, "version": _tool_version(objdump_path)},
        },
        "summary": {
            "static_instruction_count": len(instructions),
            "unique_label_count": len(labels),
            "episode_region_count": len(regions),
            "feature_counts": dict(sorted(feature_counts.items())),
        },
        "labels": dict(sorted(labels.items())),
        "episode_regions": regions,
        "instructions": instructions,
    }


def analyze_trace(trace_path: Path, program_map: dict[str, Any]) -> dict[str, Any]:
    trace_path = trace_path.resolve()
    instructions = sorted(list(program_map["instructions"]), key=lambda item: int(item["pc"]))
    previous_end: int | None = None
    for index, instruction in enumerate(instructions):
        try:
            pc = int(instruction["pc"])
            size = int(instruction["size"])
        except (KeyError, TypeError, ValueError) as exc:
            raise ValueError(f"invalid ProgramMap instruction at index {index}: {exc}") from exc
        if size not in {2, 4, 6, 8}:
            raise ValueError(f"invalid ProgramMap instruction size at 0x{pc:x}: {size}")
        if previous_end is not None and pc < previous_end:
            raise ValueError(f"ProgramMap instructions overlap at 0x{pc:x}")
        previous_end = pc + size
    instruction_by_pc = {int(instruction["pc"]): instruction for instruction in instructions}
    event_counts: Counter[str] = Counter()
    event_first: dict[str, dict[str, int]] = {}
    episode_counts: Counter[str] = Counter()
    missing_pcs: Counter[int] = Counter()
    unique_pcs: set[int] = set()
    pc_visits: Counter[int] = Counter()
    control_flow_edges: Counter[tuple[int, int]] = Counter()
    branch_outcomes: dict[int, Counter[str]] = defaultdict(Counter)
    indirect_targets: dict[int, Counter[int]] = defaultdict(Counter)
    boundary_transitions: Counter[str] = Counter()
    site8_relations: dict[int, set[bool]] = defaultdict(set)
    previous: tuple[int, dict[str, Any]] | None = None
    current_call_depth = 0
    max_call_depth = 0
    taken_cj_ring_offsets: set[int] = set()
    taken_cj_ring_sequence: list[int] = []

    def observe(event: str, entry: dict[str, Any], count: int = 1) -> None:
        event_counts[event] += count
        event_first.setdefault(
            event,
            {
                "first_index": int(entry.get("index", 0)),
                "first_pc": int(str(entry["pc"]), 0),
            },
        )

    with trace_path.open(encoding="utf-8") as stream:
        for line_number, line in enumerate(stream, start=1):
            if not line.strip():
                continue
            try:
                entry = json.loads(line)
                pc = int(str(entry["pc"]), 0)
            except (KeyError, TypeError, ValueError, json.JSONDecodeError) as exc:
                raise ValueError(f"invalid trace entry at line {line_number}: {exc}") from exc
            observe("trace.committed_instruction", entry)
            unique_pcs.add(pc)
            pc_visits[pc] += 1
            instruction = instruction_by_pc.get(pc)
            if instruction is None:
                missing_pcs[pc] += 1
                previous = None
                continue

            # Use consecutive committed PCs, not predicted targets or layout
            # labels. These are program-space metrics, not DUT timing crosses.
            if previous is not None:
                prior_pc, prior_instruction = previous
                if (prior_pc % PAGE_BYTES == PAGE_BYTES - 2
                        and int(prior_instruction["size"]) == 2 and pc == prior_pc + 2):
                    observe("trace.page_tail_rvc_fallthrough", entry)
                for label, span in (("fetch_block", FETCH_BLOCK_BYTES), ("cacheline", CACHELINE_BYTES), ("page", PAGE_BYTES)):
                    if prior_pc // span != pc // span:
                        boundary_transitions[label] += 1
                if any(prior_instruction.get(key) for key in (
                    "conditional_branch", "direct_jump", "indirect_jump", "call", "return",
                )):
                    control_flow_edges[(prior_pc, pc)] += 1
                parameters = prior_instruction.get("episode_parameters") or {}
                if (prior_instruction.get("conditional_branch")
                        and parameters.get("branch_history_mode") == "site8"
                        and parameters.get("branch_history_label") in prior_instruction.get("labels", [])):
                    fallthrough = prior_pc + int(prior_instruction["size"])
                    # Accept only either actual linked branch successor. Declared
                    # trace taken/target fields cannot satisfy this opt-in gate.
                    try:
                        target = int(prior_instruction["operands"].split(",")[-1].strip().split()[0], 16)
                    except (KeyError, ValueError, IndexError):
                        target = None
                    if pc in (fallthrough, target):
                        site8_relations[prior_pc].add(pc == fallthrough)
                if prior_instruction.get("indirect_jump"):
                    indirect_targets[prior_pc][pc] += 1
            previous = (pc, instruction)

            map_size = int(instruction["size"])
            if entry.get("size") is not None and int(entry["size"]) != map_size:
                raise ValueError(
                    f"trace instruction size mismatch at line {line_number}, pc 0x{pc:x}: "
                    f"trace={int(entry['size'])} ProgramMap={map_size}"
                )
            size = map_size
            observe("trace.rvc_instruction" if size == 2 else "trace.rvi_instruction", entry)
            if (
                instruction.get("episode_kind") == "taken_cj_offset_ring"
                and instruction.get("mnemonic") == "c.j"
            ):
                parameters = instruction.get("episode_parameters") or {}
                try:
                    halfword_offset = int(parameters["halfword_offset"])
                except (KeyError, TypeError, ValueError):
                    pass
                else:
                    if 0 <= halfword_offset < 16:
                        if not taken_cj_ring_offsets:
                            event_first["trace.taken_cj_offset_ring"] = {
                                "first_index": int(entry.get("index", 0)),
                                "first_pc": pc,
                            }
                        taken_cj_ring_offsets.add(halfword_offset)
                        taken_cj_ring_sequence.append(halfword_offset)
            for attribute, event in (
                ("conditional_branch", "trace.conditional_branch"),
                ("direct_jump", "trace.direct_jump"),
                ("indirect_jump", "trace.indirect_jump"),
                ("call", "trace.call"),
                ("return", "trace.return"),
                ("crosses_fetch_block", "trace.cross_fetch_block_instruction"),
                ("crosses_cacheline", "trace.cross_cacheline_instruction"),
                ("crosses_page", "trace.cross_page_instruction"),
            ):
                if instruction.get(attribute):
                    observe(event, entry)
            if instruction.get("conditional_branch"):
                branch_outcomes[pc]["taken" if bool(entry.get("taken")) else "not_taken"] += 1
                observe(
                    "trace.branch_taken" if bool(entry.get("taken")) else "trace.branch_not_taken",
                    entry,
                )
            if instruction.get("call"):
                current_call_depth += 1
                max_call_depth = max(max_call_depth, current_call_depth)
            if instruction.get("return"):
                current_call_depth = max(0, current_call_depth - 1)
            episode_id = str(instruction.get("episode_id") or "")
            if episode_id:
                episode_counts[episode_id] += 1

    event_counts["trace.unique_pc"] = len(unique_pcs)
    event_counts["trace.max_call_depth"] = max_call_depth
    event_counts["trace.indirect_four_target_site"] = sum(
        len(targets) >= 4 for targets in indirect_targets.values()
    )
    event_counts["trace.indirect_four_target_page_site"] = sum(
        len({target // PAGE_BYTES for target in targets}) >= 4
        for targets in indirect_targets.values()
    )
    event_counts["trace.taken_cj_offset_ring"] = len(taken_cj_ring_offsets)
    event_counts["trace.taken_cj_offset_transition"] = len(
        set(zip(taken_cj_ring_sequence, taken_cj_ring_sequence[1:]))
    )
    if any((i.get("episode_parameters") or {}).get("branch_history_mode") == "site8" for i in instructions):
        event_counts["trace.site8_branch_sites_with_both_successors"] = sum(len(v) == 2 for v in site8_relations.values())
    shared_instructions = [i for i in instructions if i.get("episode_kind") == "shared_return_contexts"
                           and i.get("return") and i.get("indirect_jump")
                           and (i.get("episode_parameters") or {}).get("return_label") in i.get("labels", [])]
    if shared_instructions:
        counts = []
        for instruction in shared_instructions:
            parameters = instruction["episode_parameters"]
            labels = parameters.get("return_context_labels") or []
            label_map = program_map.get("labels") or {}
            legal = {label_map[label] for label in labels if label in label_map}
            counts.append(len(set(indirect_targets[int(instruction["pc"])]) & legal))
        # Multiple private returns do not add up to one shared-return context.
        event_counts["trace.shared_return_distinct_successors"] = max(counts, default=0)
    events = []
    for event, count in sorted(event_counts.items()):
        first = event_first.get(event, {})
        events.append(
            {
                "event": event,
                "count": count,
                "evidence_source": "nemu_committed_trace",
                "relation": "observed",
                "anchor": "",
                **first,
            }
        )
    return {
        "schema_version": TRACE_ANALYSIS_SCHEMA_VERSION,
        "trace_path": str(trace_path),
        "trace_sha256": _sha256(trace_path),
        "program_map_binary_sha256": str(program_map["binary_sha256"]),
        "events": events,
        "realized_episode_counts": dict(sorted(episode_counts.items())),
        "missing_program_map_pc_count": sum(missing_pcs.values()),
        "missing_program_map_pcs": [f"0x{pc:x}" for pc in sorted(missing_pcs)],
        "program_diversity": {
            "schema_version": 1,
            "evidence_source": "nemu_committed_trace",
            "policy": (
                "Program-space observations only; not DUT cross coverage or a randomness score. "
                "Training-loop repetition can be intentional. Counts do not authorize corpus replacement."
            ),
            "unique_pc_count": len(unique_pcs),
            "dynamic_instruction_count": sum(pc_visits.values()),
            "max_pc_visit_count": max(pc_visits.values(), default=0),
            "unique_control_flow_edges": len(control_flow_edges),
            "dynamic_control_flow_edges": sum(control_flow_edges.values()),
            "conditional_branch_sites": len(branch_outcomes),
            "conditional_branch_sites_with_both_outcomes": sum(
                bool(counts["taken"] and counts["not_taken"]) for counts in branch_outcomes.values()
            ),
            "indirect_sites_with_observed_successor": len(indirect_targets),
            "indirect_sites_with_multiple_targets": sum(len(targets) > 1 for targets in indirect_targets.values()),
            "indirect_target_region_diversity": {
                label: {
                    "sites_with_targets_in_multiple_regions": sum(
                        len({target // span for target in targets}) > 1
                        for targets in indirect_targets.values()
                    ),
                    "sites_with_target_outside_source_region": sum(
                        any(target // span != pc // span for target in targets)
                        for pc, targets in indirect_targets.items()
                    ),
                }
                for label, span in (("fetch_block", FETCH_BLOCK_BYTES), ("cacheline", CACHELINE_BYTES), ("page", PAGE_BYTES))
            },
            "instruction_start_boundary_transitions": {
                label: boundary_transitions[label] for label in ("fetch_block", "cacheline", "page")
            },
            "taken_cj_offset_ring_offsets": sorted(taken_cj_ring_offsets),
            "taken_cj_offset_ring_sequence": taken_cj_ring_sequence,
            "branch_outcomes": [
                {"pc": f"0x{pc:x}", "taken": counts["taken"], "not_taken": counts["not_taken"]}
                for pc, counts in sorted(branch_outcomes.items())
            ],
            "indirect_targets": [
                {"pc": f"0x{pc:x}", "targets": [
                    {"pc": f"0x{target:x}", "count": count} for target, count in sorted(targets.items())
                ]}
                for pc, targets in sorted(indirect_targets.items())
            ],
        },
    }


def realized_events(trace_analysis: dict[str, Any]) -> tuple[RealizedEvent, ...]:
    return tuple(
        RealizedEvent(
            event=str(item["event"]),
            count=int(item["count"]),
            evidence_source=str(item["evidence_source"]),
            relation=str(item.get("relation") or "observed"),
            anchor=str(item.get("anchor") or ""),
            first_index=(int(item["first_index"]) if "first_index" in item else None),
            first_pc=(int(item["first_pc"]) if "first_pc" in item else None),
        )
        for item in trace_analysis["events"]
    )


def _int_auto(value: str) -> int:
    return int(value, 0)


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Build a verified Frontend random-case ProgramMap")
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--binary", type=Path, required=True)
    parser.add_argument("--generator-manifest", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--trace", type=Path, default=None)
    parser.add_argument("--trace-analysis-output", type=Path, default=None)
    parser.add_argument("--link-address", type=_int_auto, default=0x80000000)
    parser.add_argument("--binary-base", type=_int_auto, default=0x80000000)
    return parser.parse_args()


def main() -> int:
    args = _parse_args()
    metadata = json.loads(args.generator_manifest.read_text(encoding="utf-8"))
    program_map = build_program_map(
        args.source,
        args.binary,
        metadata,
        link_address=args.link_address,
        binary_base=args.binary_base,
    )
    write_json_contract(args.output, program_map)
    if args.trace is not None:
        trace_analysis = analyze_trace(args.trace, program_map)
        trace_output = args.trace_analysis_output or args.output.with_name(
            f"{args.output.stem}.trace_analysis.json"
        )
        write_json_contract(trace_output, trace_analysis)
    print(
        f"instructions={program_map['summary']['static_instruction_count']} "
        f"binary_verified={int(program_map['binary_text_verified'])} output={args.output.resolve()}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

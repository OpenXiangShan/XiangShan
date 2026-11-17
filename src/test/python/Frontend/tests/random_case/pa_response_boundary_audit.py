#!/usr/bin/env python3
"""Audit physical response attribution against a configured ProgramMap.

This is an offline evidence check.  A response is attributed to a cacheline,
not claimed as instruction delivery; exact containing instructions are listed
separately for human review.
"""

from __future__ import annotations

import argparse
import hashlib
import json
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


PAGE_BYTES = 4096
CACHELINE_BYTES = 64
RESPONSE_TYPES = {"handshake.icache_d", "handshake.uncache_d"}
BOUNDARY_FEATURES = (
    "crosses_fetch_block",
    "crosses_cacheline",
    "crosses_page",
    "conditional_branch",
    "call",
    "return",
)
SPATIAL_BOUNDARY_SPANS = {
    "crosses_fetch_block": 32,
    "crosses_cacheline": CACHELINE_BYTES,
    "crosses_page": PAGE_BYTES,
}


def _sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def _load(path: Path) -> dict[str, Any]:
    payload = json.loads(path.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError(f"expected JSON object: {path}")
    return payload


def _parse_address(value: Any) -> int | None:
    if isinstance(value, bool):
        return None
    try:
        address = int(value, 0) if isinstance(value, str) else int(value)
    except (TypeError, ValueError):
        return None
    return address if address >= 0 else None


def _instructions_by_line(program_map: dict[str, Any]) -> dict[int, list[dict[str, Any]]]:
    result: dict[int, list[dict[str, Any]]] = {}
    for instruction in program_map.get("instructions") or []:
        if not isinstance(instruction, dict):
            continue
        try:
            pc = int(instruction["pc"])
            size = int(instruction.get("size") or 0)
        except (KeyError, TypeError, ValueError):
            continue
        if size <= 0:
            continue
        first_line = pc & ~(CACHELINE_BYTES - 1)
        last_line = (pc + size - 1) & ~(CACHELINE_BYTES - 1)
        for line in range(first_line, last_line + CACHELINE_BYTES, CACHELINE_BYTES):
            result.setdefault(line, []).append(instruction)
    return result


def _validate_program_map_layout(program_map: dict[str, Any]) -> list[str]:
    raw_instructions = program_map.get("instructions")
    if not isinstance(raw_instructions, list) or not raw_instructions:
        return ["program_map_instructions_missing_or_invalid"]
    reasons: list[str] = []
    parsed: list[tuple[int, int, int]] = []
    try:
        binary_base = program_map["binary_base"]
        text_size = program_map["text_size_bytes"]
        if isinstance(binary_base, bool) or isinstance(text_size, bool):
            raise ValueError
        binary_base = int(binary_base)
        text_size = int(text_size)
        if binary_base < 0 or text_size <= 0:
            raise ValueError
    except (KeyError, TypeError, ValueError):
        binary_base = None
        text_size = None
        reasons.append("program_map_text_span_missing_or_invalid")
    for index, instruction in enumerate(raw_instructions):
        if not isinstance(instruction, dict):
            reasons.append(f"program_map_instruction_{index}_not_object")
            continue
        try:
            raw_pc = instruction["pc"]
            raw_size = instruction["size"]
            if isinstance(raw_pc, bool) or isinstance(raw_size, bool):
                raise ValueError
            pc = int(raw_pc)
            size = int(raw_size)
        except (KeyError, TypeError, ValueError):
            reasons.append(f"program_map_instruction_{index}_invalid_pc_or_size")
            continue
        if pc < 0 or pc & 1:
            reasons.append(f"program_map_instruction_{index}_invalid_pc:{pc:#x}")
        if size not in {2, 4, 6, 8}:
            reasons.append(f"program_map_instruction_{index}_invalid_size:{size}")
            continue
        parsed.append((pc, size, index))
        encoding = instruction.get("encoding")
        try:
            if (
                not isinstance(encoding, str)
                or len(encoding) != size * 2
                or int(encoding, 16) < 0
            ):
                raise ValueError
        except ValueError:
            reasons.append(f"program_map_instruction_{index}_encoding_size_mismatch")
        for field, span in (
            ("fetch_block_offset", 32),
            ("cacheline_offset", CACHELINE_BYTES),
            ("page_offset", PAGE_BYTES),
        ):
            recorded = instruction.get(field)
            if isinstance(recorded, bool):
                recorded = None
            try:
                recorded = int(recorded)
            except (TypeError, ValueError):
                recorded = None
            if recorded != pc % span:
                reasons.append(f"program_map_instruction_{index}_{field}_mismatch")
        if (
            binary_base is not None
            and text_size is not None
            and not (binary_base <= pc and pc + size <= binary_base + text_size)
        ):
            reasons.append(f"program_map_instruction_{index}_outside_text_span")
        for feature, span in SPATIAL_BOUNDARY_SPANS.items():
            expected = pc // span != (pc + size - 1) // span
            if instruction.get(feature) is not expected:
                reasons.append(f"program_map_instruction_{index}_{feature}_mismatch")
    previous_end: int | None = None
    for pc, size, index in sorted(parsed):
        if previous_end is not None and pc < previous_end:
            reasons.append(f"program_map_instruction_{index}_overlaps_previous:{pc:#x}")
        previous_end = max(previous_end or 0, pc + size)
    summary = program_map.get("summary") or {}
    try:
        recorded_count = int(summary["static_instruction_count"])
    except (KeyError, TypeError, ValueError):
        reasons.append("program_map_static_instruction_count_missing_or_invalid")
    else:
        if recorded_count != len(raw_instructions):
            reasons.append("program_map_static_instruction_count_mismatch")
    return reasons


def _map_physical(address: int, pages: list[dict[str, Any]]) -> tuple[str, list[int]]:
    candidates: list[int] = []
    for page in pages:
        if not isinstance(page, dict):
            continue
        try:
            va = int(page["va"])
            pa = int(page["pa"])
        except (KeyError, TypeError, ValueError):
            continue
        if pa <= (address & ~(PAGE_BYTES - 1)) < pa + PAGE_BYTES:
            candidates.append(va + (address & (PAGE_BYTES - 1)))
    candidates.sort()
    if not candidates:
        return "unmapped", candidates
    if len(candidates) > 1:
        return "ambiguous_alias", candidates
    return "mapped", candidates


def _validate_pages(pages: list[Any]) -> list[str]:
    reasons: list[str] = []
    virtual_pages: dict[int, int] = {}
    physical_pages: dict[int, int] = {}
    for index, page in enumerate(pages):
        if not isinstance(page, dict):
            reasons.append(f"mapping_page_{index}_not_object")
            continue
        try:
            va = int(page["va"])
            pa = int(page["pa"])
        except (KeyError, TypeError, ValueError):
            reasons.append(f"mapping_page_{index}_invalid_address")
            continue
        if va < 0 or pa < 0:
            reasons.append(f"mapping_page_{index}_negative_address")
        if va & (PAGE_BYTES - 1):
            reasons.append(f"mapping_page_{index}_va_not_page_aligned")
        if pa & (PAGE_BYTES - 1):
            reasons.append(f"mapping_page_{index}_pa_not_page_aligned")
        if "ok" in page and page["ok"] is not True:
            reasons.append(f"mapping_page_{index}_not_usable")
        if va in virtual_pages:
            if virtual_pages[va] == pa:
                reasons.append(f"mapping_virtual_page_duplicate:{va:#x}")
            else:
                reasons.append(f"mapping_virtual_page_conflict:{va:#x}")
        else:
            virtual_pages[va] = pa
        if pa in physical_pages:
            if physical_pages[pa] == va:
                reasons.append(f"mapping_physical_page_duplicate:{pa:#x}")
            else:
                reasons.append(f"mapping_physical_page_alias:{pa:#x}")
        else:
            physical_pages[pa] = va
    return reasons


def _parse_selected_boundary_lines(
    timeline: dict[str, Any],
    instructions_by_line: dict[int, list[dict[str, Any]]],
) -> tuple[set[int], list[str]]:
    raw = timeline.get("selected_boundary_lines")
    if not isinstance(raw, list):
        return set(), ["selected_boundary_lines_missing_or_invalid"]
    selected: list[int] = []
    reasons: list[str] = []
    for index, value in enumerate(raw):
        try:
            line = int(value, 0) if isinstance(value, str) else int(value)
        except (TypeError, ValueError):
            reasons.append(f"selected_boundary_line_{index}_invalid")
            continue
        if line < 0 or line & (CACHELINE_BYTES - 1):
            reasons.append(f"selected_boundary_line_{index}_not_cacheline_aligned")
            continue
        selected.append(line)
    if len(selected) != len(set(selected)):
        reasons.append("selected_boundary_lines_duplicate")
    boundary_lines = {
        line
        for line, instructions in instructions_by_line.items()
        if any(any(bool(instruction.get(name)) for name in BOUNDARY_FEATURES)
               for instruction in instructions)
    }
    reasons.extend(
        f"selected_boundary_line_without_program_boundary:{line:#x}"
        for line in sorted(set(selected) - boundary_lines)
    )
    timing_plan = timeline.get("timing_plan") or {}
    try:
        maximum = int(timing_plan["maximum_backpressure_windows"])
    except (KeyError, TypeError, ValueError):
        reasons.append("timing_plan_maximum_backpressure_windows_missing_or_invalid")
    else:
        if maximum < 0 or len(set(selected)) > maximum:
            reasons.append("selected_boundary_lines_exceed_timing_plan_maximum")
    return set(selected), reasons


def _line_evidence(
    line: int,
    instructions_by_line: dict[int, list[dict[str, Any]]],
) -> dict[str, Any]:
    instructions = instructions_by_line.get(line, [])
    boundary = [
        instruction
        for instruction in instructions
        if any(bool(instruction.get(name)) for name in BOUNDARY_FEATURES)
    ]
    return {
        "line_episode_ids": sorted(
            {str(instruction["episode_id"]) for instruction in instructions
             if instruction.get("episode_id")}
        ),
        "boundary_episode_ids": sorted(
            {str(instruction["episode_id"]) for instruction in boundary
             if instruction.get("episode_id")}
        ),
        "boundary_instruction_pcs": sorted(int(instruction["pc"]) for instruction in boundary),
    }


def build_report(timeline_path: Path, program_map_path: Path) -> dict[str, Any]:
    timeline = _load(timeline_path)
    program_map = _load(program_map_path)
    mapping = timeline.get("program_address_mapping") or {}
    pages = list(mapping.get("pages") or [])
    if not pages or mapping.get("valid") is not True:
        raise ValueError("timeline lacks a valid configured program-address mapping")
    mapping_failures = _validate_pages(pages)
    program_map_layout_failures = _validate_program_map_layout(program_map)
    instructions_by_line = _instructions_by_line(program_map)
    selected_boundary_lines, selected_boundary_failures = _parse_selected_boundary_lines(
        timeline, instructions_by_line
    )
    timeline_binary_sha = str(timeline.get("program_map_binary_sha256") or "")
    program_map_binary_sha = str(program_map.get("binary_sha256") or "")
    binary_binding_valid = bool(
        timeline_binary_sha
        and program_map_binary_sha
        and timeline_binary_sha == program_map_binary_sha
        and program_map.get("binary_text_verified") is True
    )
    response_events = [
        item
        for item in timeline.get("timeline") or []
        if str(item.get("type") or "") in RESPONSE_TYPES
    ]
    responses: list[tuple[dict[str, Any], int]] = []
    malformed_responses: list[dict[str, Any]] = []
    for item in response_events:
        payload = item.get("payload")
        reason = None
        address = None
        if not isinstance(payload, dict):
            reason = "payload_not_object"
        elif "address" not in payload:
            reason = "address_missing"
        else:
            address = _parse_address(payload["address"])
            if address is None:
                reason = "address_invalid"
        if reason is not None:
            malformed_responses.append(
                {
                    "cycle": int(item.get("cycle") or 0),
                    "type": str(item.get("type") or ""),
                    "reason": reason,
                }
            )
        else:
            assert address is not None
            responses.append((item, address))
    records: list[dict[str, Any]] = []
    status_counts: dict[str, int] = {}
    exact_count = 0
    for item, address in responses:
        status, candidates = _map_physical(address, pages)
        record: dict[str, Any] = {
            "cycle": int(item.get("cycle") or 0),
            "type": str(item.get("type") or ""),
            "physical_address": address,
            "mapping_status": status,
            "program_address_candidates": candidates,
            "attribution_scope": "cacheline" if len(candidates) == 1 else "none",
            "exact_instruction_attribution": False,
        }
        if len(candidates) == 1:
            va = candidates[0]
            line = va & ~(CACHELINE_BYTES - 1)
            line_instructions = instructions_by_line.get(line, [])
            line_evidence = _line_evidence(line, instructions_by_line)
            exact = [
                int(instruction["pc"])
                for instruction in line_instructions
                if int(instruction["pc"]) <= va < int(instruction["pc"]) + int(instruction.get("size") or 0)
            ]
            record.update(
                {
                    "virtual_address": va,
                    "cacheline_address": line,
                    "line_instruction_count": len(line_instructions),
                    **line_evidence,
                    "exact_address_instruction_pcs": sorted(exact),
                    "selected_boundary_line": line in selected_boundary_lines,
                }
            )
            exact_count += len(exact)
        status_counts[status] = status_counts.get(status, 0) + 1
        records.append(record)

    actions = []
    action_failures: list[str] = []
    for action_index, action in enumerate(timeline.get("actions") or []):
        if not isinstance(action, dict):
            action_id = f"action-{action_index}"
            actions.append(
                {
                    "action_id": action_id,
                    "trigger_event": "",
                    "mapping_status": "malformed",
                    "recomputed_mapping_status": "missing",
                }
            )
            action_failures.append(action_id + ":malformed")
            continue
        # Older/current coordinator timelines store trigger attribution on the
        # action itself; accept a nested context when a future schema provides it.
        context = action.get("program_context") or action
        if not isinstance(context, dict):
            action_id = str(action.get("action_id") or f"action-{action_index}")
            actions.append(
                {
                    "action_id": action_id,
                    "trigger_event": str(action.get("trigger_event") or ""),
                    "mapping_status": "malformed",
                    "recomputed_mapping_status": "missing",
                }
            )
            action_failures.append(action_id + ":malformed_context")
            continue
        status = str(context.get("mapping_status") or "missing")
        action_physical = context.get("physical_address")
        parsed_action_physical = _parse_address(action_physical)
        recomputed_status = "missing"
        recomputed_candidates: list[int] = []
        recomputed_cacheline = None
        recomputed_line_evidence = {
            "line_episode_ids": [],
            "boundary_episode_ids": [],
            "boundary_instruction_pcs": [],
        }
        recomputed_selected_boundary = False
        if parsed_action_physical is not None:
            recomputed_status, recomputed_candidates = _map_physical(parsed_action_physical, pages)
            if len(recomputed_candidates) == 1:
                recomputed_cacheline = recomputed_candidates[0] & ~(CACHELINE_BYTES - 1)
                recomputed_line_evidence = _line_evidence(
                    recomputed_cacheline, instructions_by_line
                )
                recomputed_selected_boundary = recomputed_cacheline in selected_boundary_lines
        recorded_episode_source = (
            context.get("line_episode_ids")
            if "line_episode_ids" in context
            else context.get("episode_ids")
        )
        recorded_episode_ids = sorted(str(value) for value in (recorded_episode_source or []))
        recorded_boundary_episode_ids = sorted(
            str(value) for value in (context.get("boundary_episode_ids") or [])
        )
        recorded_boundary_instruction_pcs = []
        boundary_pc_values = context.get("boundary_instruction_pcs") or []
        boundary_pc_parse_failed = not isinstance(boundary_pc_values, list)
        if not boundary_pc_parse_failed:
            try:
                recorded_boundary_instruction_pcs = sorted(
                    int(value) for value in boundary_pc_values
                )
            except (TypeError, ValueError):
                boundary_pc_parse_failed = True
        actions.append(
            {
                "action_id": str(action.get("action_id") or ""),
                "trigger_event": str(action.get("trigger_event") or ""),
                "mapping_status": status,
                "recomputed_mapping_status": recomputed_status,
                "recomputed_program_address_candidates": recomputed_candidates,
                "recomputed_cacheline_address": recomputed_cacheline,
                "recomputed_line_episode_ids": recomputed_line_evidence["line_episode_ids"],
                "recomputed_boundary_episode_ids": recomputed_line_evidence["boundary_episode_ids"],
                "recomputed_boundary_instruction_pcs": recomputed_line_evidence["boundary_instruction_pcs"],
                "recomputed_selected_boundary_line": recomputed_selected_boundary,
                "cacheline_address": context.get("cacheline_address"),
                "selected_boundary_line": bool(context.get("selected_boundary_line")),
                "line_episode_ids": recorded_episode_ids,
                "boundary_episode_ids": recorded_boundary_episode_ids,
                "boundary_instruction_pcs": recorded_boundary_instruction_pcs,
            }
        )
        if boundary_pc_parse_failed:
            action_failures.append(str(action.get("action_id") or "unnamed") + ":invalid_boundary_pcs")
        elif status != "mapped":
            action_failures.append(str(action.get("action_id") or "unnamed"))
        elif parsed_action_physical is None or recomputed_status != "mapped":
            action_failures.append(str(action.get("action_id") or "unnamed") + ":recompute")
        elif list(context.get("program_address_candidates") or []) != recomputed_candidates:
            action_failures.append(str(action.get("action_id") or "unnamed") + ":candidate_mismatch")
        elif context.get("cacheline_address") != recomputed_cacheline:
            action_failures.append(str(action.get("action_id") or "unnamed") + ":cacheline_mismatch")
        elif recorded_episode_ids != recomputed_line_evidence["line_episode_ids"]:
            action_failures.append(str(action.get("action_id") or "unnamed") + ":episode_mismatch")
        elif bool(context.get("selected_boundary_line")) != recomputed_selected_boundary:
            action_failures.append(
                str(action.get("action_id") or "unnamed") + ":selected_boundary_mismatch"
            )
        elif (
            "boundary_episode_ids" in context
            and recorded_boundary_episode_ids != recomputed_line_evidence["boundary_episode_ids"]
        ):
            action_failures.append(
                str(action.get("action_id") or "unnamed") + ":boundary_episode_mismatch"
            )
        elif (
            "boundary_instruction_pcs" in context
            and recorded_boundary_instruction_pcs
            != recomputed_line_evidence["boundary_instruction_pcs"]
        ):
            action_failures.append(
                str(action.get("action_id") or "unnamed") + ":boundary_pc_mismatch"
            )

    failures = [
        f"response_mapping_{status}={count}"
        for status, count in sorted(status_counts.items())
        if status != "mapped"
    ]
    failures.extend(mapping_failures)
    failures.extend(program_map_layout_failures)
    failures.extend(selected_boundary_failures)
    if not response_events:
        failures.append("response_events_missing")
    if malformed_responses:
        failures.append(f"malformed_response_events={len(malformed_responses)}")
    if not binary_binding_valid:
        failures.append("program_map_binary_binding_invalid")
    failures.extend(f"action_not_mapped:{action_id}" for action_id in action_failures)
    dropped_events = int(timeline.get("dropped_timeline_events") or 0)
    qualification_reasons = failures + (["timeline_dropped_events"] if dropped_events else [])
    qualified = not qualification_reasons
    return {
        "schema_version": 1,
        "generated_at_utc": datetime.now(timezone.utc).isoformat(),
        "policy": (
            "Configured PA-to-VA mapping is required. Responses are attributed at cacheline "
            "granularity; exact instruction PCs are evidence only and do not prove delivery."
        ),
        "inputs": {
            "timeline": {"path": str(timeline_path.resolve()), "sha256": _sha256(timeline_path)},
            "program_map": {"path": str(program_map_path.resolve()), "sha256": _sha256(program_map_path)},
        },
        "program_map_binary_binding": {
            "timeline_binary_sha256": timeline_binary_sha,
            "program_map_binary_sha256": program_map_binary_sha,
            "program_map_binary_text_verified": program_map.get("binary_text_verified"),
            "pass": binary_binding_valid,
        },
        "program_map_layout": {
            "pass": not program_map_layout_failures,
            "validation_reasons": program_map_layout_failures,
            "instruction_count": len(program_map.get("instructions") or []),
        },
        "mapping": {
            "valid": not mapping_failures,
            "validation_reasons": mapping_failures,
            "page_count": len(pages),
            "pages": pages,
            "policy": mapping.get("policy"),
        },
        "selected_boundary_lines": {
            "recorded": sorted(selected_boundary_lines),
            "validation_reasons": selected_boundary_failures,
            "valid": not selected_boundary_failures,
        },
        "response_summary": {
            "response_count": len(responses),
            "observed_response_event_count": len(response_events),
            "malformed_response_event_count": len(malformed_responses),
            "mapping_status_counts": status_counts,
            "exact_containing_instruction_count": exact_count,
            "dropped_timeline_events": dropped_events,
        },
        "malformed_response_events": malformed_responses,
        "actions": actions,
        "records": records,
        "qualification": {
            "pass": qualified,
            "reasons": qualification_reasons,
            "status": "proved" if qualified else "not_proved",
            "exact_instruction_delivery_claimed": False,
        },
    }


def write_json_exclusive(path: Path, payload: dict[str, Any]) -> None:
    path = path.resolve()
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("x", encoding="utf-8") as stream:
        json.dump(payload, stream, indent=2, sort_keys=True)
        stream.write("\n")


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--timeline", type=Path, required=True)
    parser.add_argument("--program-map", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    report = build_report(args.timeline.resolve(), args.program_map.resolve())
    write_json_exclusive(args.output, report)
    print(
        f"qualified={int(report['qualification']['pass'])} "
        f"responses={report['response_summary']['response_count']} "
        f"output={args.output.resolve()}"
    )
    return 0 if report["qualification"]["pass"] else 1


if __name__ == "__main__":
    raise SystemExit(main())

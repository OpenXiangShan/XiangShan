#!/usr/bin/env python3
from __future__ import annotations

import argparse
import json
import re
from collections import defaultdict
from pathlib import Path
from typing import Any


AUDIT_SCHEMA_VERSION = 1
RECOVERY_PATTERN = re.compile(
    r"redirect recovery first cfvec is not target: "
    r"slot=(?P<slot>\d+) actual_pc=0x(?P<actual>[0-9a-fA-F]+) "
    r"target_pc=0x(?P<target>[0-9a-fA-F]+) "
    r"cycle=(?P<cycle>\d+) cursor=(?P<cursor>\d+)"
)
UNATTRIBUTED_PATTERN = re.compile(
    r"first mismatch has no attributable CFI for redirect; "
    r"[^\n]*?"
    r"queue_index=(?P<queue_index>\d+) pc=0x(?P<actual>[0-9a-fA-F]+) "
    r"target_pc=0x(?P<target>[0-9a-fA-F]+) "
    r"cycle=(?P<cycle>\d+) cursor=(?P<cursor>\d+)"
)


def _atomic_json(path: Path, payload: dict[str, Any]) -> None:
    path = path.resolve()
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(f".{path.name}.tmp")
    temporary.write_text(
        json.dumps(payload, indent=2, sort_keys=True) + "\n",
        encoding="utf-8",
    )
    temporary.replace(path)


def _read_logs(record: dict[str, Any]) -> tuple[str, list[str]]:
    root = Path(str(record.get("artifact_root") or ""))
    paths = sorted((root / "logs").glob("*.log")) if root.is_dir() else []
    chunks: list[str] = []
    for path in paths:
        try:
            chunks.append(path.read_text(encoding="utf-8", errors="replace"))
        except OSError:
            continue
    return "\n".join(chunks), [str(path.resolve()) for path in paths]


def _trace_context(path: Path, cursor: int, radius: int = 7) -> list[dict[str, Any]]:
    if not path.is_file():
        return []
    start = max(0, int(cursor) - int(radius))
    stop = int(cursor) + int(radius) + 1
    selected: list[dict[str, Any]] = []
    with path.open("r", encoding="utf-8") as stream:
        for line_number, line in enumerate(stream):
            if line_number < start:
                continue
            if line_number >= stop:
                break
            try:
                payload = json.loads(line)
            except ValueError:
                continue
            if isinstance(payload, dict):
                selected.append(dict(payload))
    return selected


def _parse_pc(value: object) -> int | None:
    if value is None:
        return None
    try:
        return int(str(value), 0)
    except (TypeError, ValueError):
        return None


def _page_tail_proof(
    context: list[dict[str, Any]], target_pc: int, actual_pc: int
) -> dict[str, Any]:
    by_pc: dict[int, list[dict[str, Any]]] = defaultdict(list)
    for entry in context:
        pc = _parse_pc(entry.get("pc"))
        if pc is not None:
            by_pc[int(pc)].append(entry)
    target_entries = by_pc.get(int(target_pc), [])
    actual_entries = by_pc.get(int(actual_pc), [])
    target_indexes = [int(item.get("index", -1)) for item in target_entries]
    actual_indexes = [int(item.get("index", -1)) for item in actual_entries]
    predecessors = [
        item
        for item in context
        if _parse_pc(item.get("target_pc")) == int(target_pc)
        and _parse_pc(item.get("pc")) is not None
        and int(_parse_pc(item.get("pc")) or 0) < int(target_pc)
    ]
    return {
        "target_page_offset": int(target_pc) & 0xFFF,
        "target_instruction_crosses_page": any(
            int(target_pc) // 0x1000
            != (int(target_pc) + int(item.get("size", 0)) - 1) // 0x1000
            for item in target_entries
            if int(item.get("size", 0)) > 0
        ),
        "target_in_trace_context": bool(target_entries),
        "actual_in_trace_context": bool(actual_entries),
        "target_precedes_actual": bool(
            target_indexes
            and actual_indexes
            and min(target_indexes) < min(actual_indexes)
        ),
        "predecessor_reaches_target": bool(predecessors),
        "target_trace_indexes": target_indexes,
        "actual_trace_indexes": actual_indexes,
    }


def _failure_record(record: dict[str, Any]) -> dict[str, Any]:
    text, log_paths = _read_logs(record)
    recovery = RECOVERY_PATTERN.search(text)
    unattributed = UNATTRIBUTED_PATTERN.search(text)
    parsed: dict[str, Any] = {}
    if recovery is not None:
        values = recovery.groupdict()
        actual_pc = int(values["actual"], 16)
        target_pc = int(values["target"], 16)
        cursor = int(values["cursor"])
        signature = (
            "redirect_recovery_page_tail"
            if target_pc & 0xFFF == 0xFFE and actual_pc > target_pc
            else "redirect_recovery_other"
        )
        parsed = {
            "failure_signature": signature,
            "error_message": recovery.group(0),
            "slot": int(values["slot"]),
            "actual_pc": f"0x{actual_pc:x}",
            "target_pc": f"0x{target_pc:x}",
            "actual_minus_target": actual_pc - target_pc,
            "cycle": int(values["cycle"]),
            "golden_cursor": cursor,
        }
    elif unattributed is not None:
        values = unattributed.groupdict()
        actual_pc = int(values["actual"], 16)
        target_pc = int(values["target"], 16)
        cursor = int(values["cursor"])
        signature = "unattributed_cfi_redirect"
        parsed = {
            "failure_signature": signature,
            "error_message": unattributed.group(0),
            "queue_index": int(values["queue_index"]),
            "actual_pc": f"0x{actual_pc:x}",
            "target_pc": f"0x{target_pc:x}",
            "actual_minus_target": actual_pc - target_pc,
            "cycle": int(values["cycle"]),
            "golden_cursor": cursor,
        }
    else:
        actual_pc = target_pc = None
        cursor = -1
        signature = "unclassified"
        parsed = {
            "failure_signature": signature,
            "error_message": text.strip().splitlines()[-1] if text.strip() else "",
        }

    trace_path = Path(str(record.get("trace_path") or ""))
    context = _trace_context(trace_path, cursor) if cursor >= 0 else []
    spec = dict(record.get("spec") or {})
    replay_command = (
        f"TB_SEED={int(spec.get('seed', 0))} "
        f"TB_BACKEND_RANDOM_SEED={int(record.get('timing_seed', 0))} "
        f"bash src/test/python/Frontend/scripts/run_baremode_asm_bin_trace.sh "
        f"{record.get('source_path', '')}"
    )
    return {
        "name": str(record.get("name") or ""),
        "program_seed": int(spec.get("seed", 0)),
        "timing_seed": int(record.get("timing_seed", 0)),
        "dynamic_trace_instructions": int(
            record.get("dynamic_trace_instructions") or 0
        ),
        "exit_code": int(record.get("exit_code") or 0),
        "execution_status": str(record.get("execution_status") or ""),
        "replay_command": replay_command,
        "log_paths": log_paths,
        "trace_path": str(trace_path.resolve()) if trace_path.is_file() else "",
        "trace_context": context,
        "trace_sequence_proof": (
            _page_tail_proof(context, int(target_pc), int(actual_pc))
            if target_pc is not None and actual_pc is not None
            else {}
        ),
        **parsed,
    }


def build_baseline_audit(campaign_path: Path) -> dict[str, Any]:
    campaign_path = campaign_path.resolve()
    campaign = json.loads(campaign_path.read_text(encoding="utf-8"))
    records = list(campaign.get("cases") or [])
    completed = [
        record
        for record in records
        if record.get("execution_status") == "completed"
    ]
    qualified = [record for record in completed if record.get("qualification_pass")]
    failures = [record for record in completed if not record.get("qualification_pass")]
    raw_union = set().union(
        *(set(record.get("observed_bin_ids") or []) for record in qualified)
    ) if qualified else set()
    failure_records = [_failure_record(record) for record in failures]
    clusters: dict[str, list[str]] = defaultdict(list)
    for record in failure_records:
        clusters[str(record["failure_signature"])].append(str(record["name"]))
    complete = len(completed) == len(records)
    return {
        "schema_version": AUDIT_SCHEMA_VERSION,
        "report_status": "complete_baseline" if complete else "partial_live_baseline",
        "campaign_path": str(campaign_path),
        "repo_head": str(campaign.get("repo_head") or ""),
        "summary": {
            "manifest_cases": len(records),
            "completed_cases": len(completed),
            "qualified_cases": len(qualified),
            "failed_cases": len(failures),
            "raw_union_hit_bins": len(raw_union),
        },
        "failure_clusters": [
            {
                "failure_signature": signature,
                "count": len(names),
                "case_names": sorted(names),
            }
            for signature, names in sorted(clusters.items())
        ],
        "failures": failure_records,
    }


def _parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(
        description="Audit Frontend random baseline failures with golden-trace context"
    )
    parser.add_argument("--campaign", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--require-complete", action="store_true")
    return parser.parse_args()


def main() -> int:
    args = _parse_args()
    report = build_baseline_audit(args.campaign)
    _atomic_json(args.output, report)
    summary = report["summary"]
    print(
        "baseline-audit "
        f"status={report['report_status']} "
        f"completed={summary['completed_cases']}/{summary['manifest_cases']} "
        f"qualified={summary['qualified_cases']} failed={summary['failed_cases']} "
        f"raw={summary['raw_union_hit_bins']} output={args.output.resolve()}"
    )
    return int(args.require_complete and report["report_status"] != "complete_baseline")


if __name__ == "__main__":
    raise SystemExit(main())

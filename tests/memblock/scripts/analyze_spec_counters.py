#!/usr/bin/env python3
"""Summarise the final performance-counter block of SPEC checkpoints.

The simulator emits cumulative ``[PERF]`` blocks more than once per run.  The
last block is the only one used here, so a checkpoint is counted once even
when it contains periodic dumps.  The report deliberately contains only
MemBlock/LSU and directly related producer counters; it is a workload
calibration aid, not an RTL correctness oracle.
"""

from __future__ import annotations

import argparse
import json
import re
from collections import Counter
from pathlib import Path
from typing import Iterable


PERF_RE = re.compile(
    r"^\[PERF \]\[time=(?P<time>\d+)\] (?P<component>[^:]+): "
    r"(?P<metric>[^,]+), (?P<value>-?\d+)\s*$"
)


def final_perf_block(path: Path) -> tuple[int, dict[tuple[str, str], int]]:
    """Return ``(timestamp, counters)`` for the last complete PERF block."""

    blocks: dict[int, dict[tuple[str, str], int]] = {}
    with path.open("r", encoding="utf-8", errors="replace") as stream:
        for line in stream:
            match = PERF_RE.match(line.rstrip("\n"))
            if not match:
                continue
            timestamp = int(match.group("time"))
            key = (match.group("component"), match.group("metric"))
            blocks.setdefault(timestamp, {})[key] = int(match.group("value"))
    if not blocks:
        raise ValueError(f"no PERF counters found in {path}")
    timestamp = max(blocks)
    return timestamp, blocks[timestamp]


def _sum_metric(
    counters: dict[tuple[str, str], int],
    metric: str,
    component: str | None = None,
    component_contains: str | None = None,
    component_endswith: str | None = None,
) -> int:
    total = 0
    for (name, key), value in counters.items():
        if key != metric:
            continue
        if component is not None and name != component:
            continue
        if component_contains is not None and component_contains not in name:
            continue
        if (
            component_endswith is not None
            and not name.endswith(component_endswith)
        ):
            continue
        total += value
    return total


def classify_checkpoint(counters: dict[tuple[str, str], int]) -> dict[str, int]:
    """Extract stable, externally useful workload dimensions.

    Component names are matched by semantic suffixes used by the generated
    Kunminghu-v2 counters.  Missing counters remain zero, which is important
    for older checkpoints and for vector-free workloads.
    """

    rob = "ctrlBlock.rob"
    memblock = ".core.memBlock.inner."
    metrics = {
        "scalar_load_instructions": _sum_metric(
            counters, "load_instr_cnt", component_contains=rob
        ),
        "scalar_store_instructions": _sum_metric(
            counters, "store_instr_cnt", component_contains=rob
        ),
        "load_unit_tlb_miss_first_issue": _sum_metric(
            counters,
            "s1_tlb_miss_first_issue",
            component_contains=memblock + "LoadUnit_",
        ),
        "load_unit_dcache_real_miss_first_issue": _sum_metric(
            counters,
            "s2_dcache_real_miss_first_issue",
            component_contains=memblock + "LoadUnit_",
        ),
        "vector_mem_issue_instructions": _sum_metric(
            counters,
            "issue_instr_count",
            component_contains="IssueQueueVlduVstu",
        ),
        "vector_ordinary_issue_instructions": _sum_metric(
            counters,
            "issue_instr_count",
            component_endswith="IssueQueueVlduVstu",
        ),
        "vector_segment_issue_instructions": _sum_metric(
            counters,
            "issue_instr_count",
            component_endswith="IssueQueueVlduVstuVseglduVsegstu",
        ),
        "vector_mem_enqueue": _sum_metric(
            counters,
            "enq_fire_cnt",
            component_contains="IssueQueueVlduVstu",
        ),
        "dcache_primary_miss_allocations": _sum_metric(
            counters,
            "miss_req_allocate",
            component_contains=memblock + "dcache.dcache.missQueue",
        ),
        "dcache_merged_loads": _sum_metric(
            counters,
            "miss_req_merge_load",
            component_contains=memblock + "dcache.dcache.missQueue",
        ),
        "dcache_rejected_loads": _sum_metric(
            counters,
            "miss_req_reject_load",
            component_contains=memblock + "dcache.dcache.missQueue",
        ),
        "dcache_probes": _sum_metric(
            counters,
            "ProbeL1DCache",
            component_contains=memblock + "dcache.dcache.probeQueue",
        ),
        "dcache_release_lines": _sum_metric(
            counters,
            "wb_release",
            component_contains=memblock + "dcache.dcache.wb.entries_",
        ),
        "dcache_probe_responses": _sum_metric(
            counters,
            "wb_probe_resp",
            component_contains=memblock + "dcache.dcache.wb.entries_",
        ),
        "dcache_bank_conflicts": _sum_metric(
            counters,
            "dcache_read_bank_conflict",
            component_contains=memblock + "dcache.dcache.ldu_",
        ),
        "dcache_conflict_replays": _sum_metric(
            counters,
            "load_replay_for_dcache_conflict",
            component_contains=memblock + "dcache.dcache.ldu_",
        ),
        "dcache_miss_queue_multi_enqueues": _sum_metric(
            counters,
            "miss_queue_has_muti_enq_req",
            component_endswith="memBlock.inner.dcache.dcache",
        ),
        "dcache_probe_blocked_by_miss": _sum_metric(
            counters,
            "probe_blocked_by_miss",
            component_contains=memblock + "dcache.dcache.missQueue",
        ),
        "uncache_requests": _sum_metric(
            counters, "q0_acquire", component_contains=memblock + "uncache"
        ),
        "uncache_mmio_loads": _sum_metric(
            counters, "uncache_mmio_load", component_contains=memblock + "uncache"
        ),
        "uncache_mmio_stores": _sum_metric(
            counters, "uncache_mmio_store", component_contains=memblock + "uncache"
        ),
        "uncache_nc_loads": _sum_metric(
            counters, "uncache_nc_load", component_contains=memblock + "uncache"
        ),
        "uncache_nc_stores": _sum_metric(
            counters, "uncache_nc_store", component_contains=memblock + "uncache"
        ),
    }
    # The queue counter is cumulative and emitted once per checkpoint.  Keep a
    # latency sample count as a separate signal instead of treating mean as a
    # request count.
    metrics["dcache_a_to_d_samples"] = _sum_metric(
        counters,
        "a_to_d_penalty_sampled",
        component_contains=memblock + "dcache.dcache.missQueue.entries_",
    )
    metrics["dcache_a_to_d_penalty_sum"] = _sum_metric(
        counters,
        "a_to_d_penalty_sum",
        component_contains=memblock + "dcache.dcache.missQueue.entries_",
    )
    return metrics


def discover_logs(root: Path) -> list[Path]:
    if root.is_file():
        return [root]
    return sorted(root.rglob("simulator_err.txt"))


def aggregate(root: Path) -> dict[str, object]:
    files = discover_logs(root)
    totals: Counter[str] = Counter()
    checkpoints = []
    skipped = []
    for path in files:
        try:
            timestamp, counters = final_perf_block(path)
        except (OSError, ValueError) as error:
            skipped.append({"path": str(path), "reason": str(error)})
            continue
        metrics = classify_checkpoint(counters)
        totals.update(metrics)
        checkpoints.append({"path": str(path), "time": timestamp, "metrics": metrics})
    return {
        "schema": 1,
        "root": str(root),
        "files_discovered": len(files),
        "checkpoints": len(checkpoints),
        "skipped": skipped,
        "totals": dict(sorted(totals.items())),
    }


def render_text(report: dict[str, object]) -> str:
    totals = report["totals"]
    assert isinstance(totals, dict)
    lines = [
        f"checkpoints={report['checkpoints']}/{report['files_discovered']}",
        "metric                              total",
        "-----------------------------------  ----------------",
    ]
    lines.extend(f"{name:35}  {value}" for name, value in totals.items())
    if report["skipped"]:
        lines.append(f"skipped={len(report['skipped'])}")
    return "\n".join(lines)


def main(argv: Iterable[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "root", type=Path, help="checkpoint directory or simulator_err.txt"
    )
    parser.add_argument("--json", type=Path, help="write the machine-readable report")
    args = parser.parse_args(list(argv) if argv is not None else None)
    report = aggregate(args.root)
    if args.json:
        args.json.parent.mkdir(parents=True, exist_ok=True)
        args.json.write_text(
            json.dumps(report, indent=2, sort_keys=True) + "\n",
            encoding="utf-8",
        )
    print(render_text(report))
    return 0 if report["checkpoints"] else 1


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Measure every MemBlock leaf scenario and record its observed traffic."""

from __future__ import annotations

import argparse
import datetime as dt
import json
import os
import re
import sys
import time
from pathlib import Path
from typing import Any

import run_regression


SCENARIOS = (
    "smoke",
    "pin-space",
    "frontend-bridge",
    "single-load",
    "load-feedback",
    "memory-violation",
    "rar-violation",
    "ifetch-prefetch",
    "hardware-prefetch",
    "fp-loads",
    "trigger-contracts",
    "metadata-contracts",
    "dcache-errors",
    "dcache-coherence",
    "atomic-contracts",
    "atomic-dchannel-errors",
    "uncache-errors",
    "uncache-widths",
    "mmio-contracts",
    "cbo-zero-contracts",
    "reset-recovery",
    "vector-load",
    "vector-split-load",
    "vector-store-forwarding",
    "store-forwarding",
    "dcache-release",
    "store-rdata-order",
    "store-tlb-miss-preserve",
    "iq-slow-feedback",
    "redirect",
    "queue-pressure",
    "scalar-misaligned",
    "misaligned-stores",
    "vector-addressing",
    "exception-contracts",
    "l2-tlb-contracts",
    "ifetch-ptw-bridge",
    "two-stage-translation",
    "translation-matrix",
    "translation-fence",
    "translation-fence-selective",
    "translation-fence-sv48",
    "translation-fence-sv48-selective",
    "translation-fence-sv39-sv48x4",
    "translation-fence-sv39-sv48x4-selective",
    "translation-fence-sv48-sv39x4",
    "translation-fence-sv48-sv39x4-selective",
    "translation-inflight-context",
    "translation-inflight-context-sv48",
    "translation-inflight-context-sv39-sv48x4",
    "translation-inflight-context-sv48-sv39x4",
    "translation-context",
    "translation-bare",
    "translation-faults",
    "translation-permissions",
    "translation-pbmt",
    "translation-superpages",
    "vector-guest-fault",
    "vector-guest-fault-split",
    "scalar-guest-fault",
    "random-loads",
    "random-vector-loads",
    "random-vector-forwarding",
    "random-forwarding",
    "random-mixed",
    "random-stress",
    "random-boundary-hunt",
)

TRANSACTION_OVERRIDES = {
    "frontend-bridge": 4096,
    "random-forwarding": 48,
    "random-vector-forwarding": 24,
    "random-boundary-hunt": 512,
}

TERMINAL_RE = re.compile(r"^MEMBLOCK_[A-Z0-9_]+_(PASS|FAIL)(?:\s|$)")


class BenchmarkError(RuntimeError):
    pass


def parse_terminal(output: str) -> dict[str, Any]:
    lines = [line.strip() for line in output.splitlines() if TERMINAL_RE.match(line)]
    if len(lines) != 1:
        raise BenchmarkError(
            f"expected one terminal summary, observed {len(lines)}"
        )
    words = lines[0].split()
    fields: dict[str, Any] = {
        "summary": lines[0],
        "status": "pass" if words[0].endswith("_PASS") else "fail",
    }
    for word in words[1:]:
        if "=" not in word:
            continue
        key, value = word.split("=", 1)
        try:
            fields[key] = int(value, 0)
        except ValueError:
            fields[key] = value
    return fields


def first_integer(fields: dict[str, Any], *names: str) -> int | None:
    for name in names:
        value = fields.get(name)
        if isinstance(value, int) and not isinstance(value, bool):
            return value
    return None


def normalized_metrics(
    fields: dict[str, Any], scenario: str
) -> dict[str, int | None]:
    scalar_completions = first_integer(fields, "scalar_writebacks", "writebacks")
    prefetch = first_integer(fields, "prefetch_writebacks", "prefetch")
    vector_only = scenario.startswith("vector-") or scenario.startswith(
        "random-vector-"
    )
    scalar_loads = first_integer(fields, "scalar_loads")
    if not vector_only and scalar_loads is None:
        scalar_loads = first_integer(fields, "loads")
    if not vector_only and scalar_loads is None and scalar_completions is not None:
        scalar_loads = scalar_completions - (prefetch or 0)
    vector_loads = first_integer(fields, "vector_load_writebacks")
    vector_stores = first_integer(fields, "vector_store_writebacks")
    if vector_only:
        vector_loads = vector_loads or first_integer(fields, "writebacks", "loads")
        vector_stores = vector_stores or first_integer(fields, "stores")
    return {
        "cycles": first_integer(fields, "cycle", "cycles"),
        "actions": first_integer(fields, "transactions"),
        "scalar_loads": scalar_loads,
        "scalar_stores": None if vector_only else first_integer(
            fields, "scalar_stores", "store_writebacks", "stores"
        ),
        "vector_loads": vector_loads,
        "vector_stores": vector_stores,
        "prefetches": prefetch,
        "dcache_a": first_integer(fields, "dcache_a", "tilelink_requests"),
        "dcache_refills": first_integer(fields, "dcache_refills"),
        "ptw_a": first_integer(fields, "ptw_requests", "ptw"),
        "uncache": first_integer(fields, "uncache_requests", "uncache"),
        "probes": first_integer(fields, "probes"),
        "grant_acks": first_integer(fields, "grant_acks"),
        "release_data": first_integer(fields, "release_data"),
    }


def run_scenario(
    binary: Path,
    scenario: str,
    seed: int,
    transactions: int,
    timeout_seconds: float,
    environment: dict[str, str],
    constraint_profile: str,
    constraint_overrides: tuple[str, ...],
) -> dict[str, Any]:
    scenario_transactions = TRANSACTION_OVERRIDES.get(scenario, transactions)
    command = [
        str(binary),
        "--test",
        scenario,
        "--seed",
        str(seed),
        "--transactions",
        str(scenario_transactions),
    ]
    if scenario == run_regression.CONSTRAINED_SCENARIO:
        command.extend(("--constraints", constraint_profile))
        for override in constraint_overrides:
            command.extend(("--constraint", override))
    started = time.monotonic()
    returncode, output, timed_out = run_regression._run_process(
        command, timeout_seconds, environment
    )
    elapsed = round(time.monotonic() - started, 6)
    if timed_out:
        return {
            "scenario": scenario,
            "status": "timeout",
            "elapsed_seconds": elapsed,
            "command": command,
            "error": f"scenario exceeded {timeout_seconds:g} seconds",
            "output": output,
        }
    try:
        result = parse_terminal(output)
    except BenchmarkError as error:
        result = {"status": "error", "error": str(error), "output": output}
    result.update(
        {
            "scenario": scenario,
            "elapsed_seconds": elapsed,
            "returncode": returncode,
            "command": command,
        }
    )
    if returncode != 0 and result["status"] == "pass":
        result["status"] = "error"
        result["error"] = "simulator returned nonzero after a pass summary"
    result["metrics"] = normalized_metrics(result, scenario)
    return result


def render_markdown(document: dict[str, Any]) -> str:
    columns = (
        ("cycles", "Cycles"),
        ("actions", "Actions"),
        ("scalar_loads", "Ld"),
        ("scalar_stores", "St"),
        ("vector_loads", "VLd"),
        ("vector_stores", "VSt"),
        ("dcache_a", "D$ A"),
        ("dcache_refills", "Refill"),
        ("ptw_a", "PTW A"),
        ("uncache", "Uncache"),
        ("probes", "Probe"),
        ("release_data", "ReleaseData"),
    )
    lines = [
        "# MemBlock Test Scale",
        "",
        f"Measured at `{document['created_at']}` using one process per scenario.",
        "Wall time is host-load dependent; protocol counts and cycles are deterministic for the recorded seed and runtime.",
        "",
        "| Scenario | Status | Wall (s) | "
        + " | ".join(label for _, label in columns)
        + " |",
        "| --- | --- | ---: | " + " | ".join("---:" for _ in columns) + " |",
    ]
    for result in document["results"]:
        metrics = result.get("metrics", {})
        values = [
            "-" if metrics.get(name) is None else str(metrics[name])
            for name, _ in columns
        ]
        lines.append(
            f"| `{result['scenario']}` | {result['status']} | "
            f"{result['elapsed_seconds']:.3f} | " + " | ".join(values) + " |"
        )
    lines.extend(
        (
            "",
            "`Actions` is the requested generator action budget, not a bus-request count. "
            "`D$ A` counts all DCache A-channel requests; `Refill` counts only AcquireBlock. "
            "A dash means that the scenario's terminal summary does not expose that metric.",
            "",
        )
    )
    return "\n".join(lines)


def write_text_atomic(path: Path, contents: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_text(contents, encoding="utf-8")
    os.replace(temporary, path)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--runtime-metadata", type=Path, required=True)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--markdown", type=Path)
    parser.add_argument("--scenarios", default=",".join(SCENARIOS))
    parser.add_argument("--seed", type=int, default=1)
    parser.add_argument("--transactions", type=int, default=16384)
    parser.add_argument("--timeout-seconds", type=float, default=1800)
    parser.add_argument(
        "--constraints", choices=run_regression.CONSTRAINT_PROFILES, default="spec"
    )
    parser.add_argument("--constraint", action="append", default=[])
    args = parser.parse_args()

    requested = tuple(item.strip() for item in args.scenarios.split(",") if item.strip())
    if not requested or len(set(requested)) != len(requested) or set(requested) - set(SCENARIOS):
        parser.error("--scenarios contains an unknown or duplicate scenario")
    if args.seed < 0 or args.transactions < 256 or args.timeout_seconds <= 0:
        parser.error("seed, transactions, and timeout are outside the supported range")

    try:
        runtime = run_regression.verify_runtime_metadata(args.runtime_metadata)
    except (OSError, json.JSONDecodeError, run_regression.RegressionError) as error:
        print(f"benchmark_tests.py: error: {error}", file=sys.stderr)
        return 2
    environment = os.environ.copy()
    environment["LD_LIBRARY_PATH"] = str(runtime["root"])
    environment["LD_BIND_NOW"] = "1"

    results = []
    started = time.monotonic()
    for scenario in requested:
        result = run_scenario(
            runtime["binary"],
            scenario,
            args.seed,
            args.transactions,
            args.timeout_seconds,
            environment,
            args.constraints,
            tuple(args.constraint),
        )
        results.append(result)
        print(
            f"MEMBLOCK_BENCHMARK scenario={scenario} status={result['status']} "
            f"elapsed_seconds={result['elapsed_seconds']:.3f}",
            flush=True,
        )
        if result["status"] != "pass":
            break

    document = {
        "schema_version": 1,
        "created_at": dt.datetime.now(dt.timezone.utc).isoformat(),
        "elapsed_seconds": round(time.monotonic() - started, 6),
        "runtime_metadata": str(runtime["metadata"]),
        "runtime_metadata_sha256": runtime["metadata_sha256"],
        "binary_sha256": runtime["artifact_hashes"]["binary"],
        "configuration": {
            "seed": args.seed,
            "transactions": args.transactions,
            "timeout_seconds": args.timeout_seconds,
            "constraint_profile": args.constraints,
            "constraint_overrides": args.constraint,
            "scenarios": list(requested),
        },
        "results": results,
    }
    write_text_atomic(args.output, json.dumps(document, indent=2, sort_keys=True) + "\n")
    if args.markdown is not None:
        write_text_atomic(args.markdown, render_markdown(document))
    passed = len(results) == len(requested) and all(
        result["status"] == "pass" for result in results
    )
    print(
        f"MEMBLOCK_BENCHMARK_{'PASS' if passed else 'FAIL'} "
        f"scenarios={len(results)}/{len(requested)} "
        f"elapsed_seconds={document['elapsed_seconds']:.3f} output={args.output}"
    )
    return 0 if passed else 1


if __name__ == "__main__":
    raise SystemExit(main())

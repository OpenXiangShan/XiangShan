#!/usr/bin/env python3
"""Measure every MemBlock leaf scenario and record its observed traffic."""

from __future__ import annotations

import argparse
import concurrent.futures
import datetime as dt
import json
import os
import re
import sys
import threading
import time
from pathlib import Path
from typing import Any

import run_regression


SCENARIOS = (
    "smoke",
    "l2-flush-contracts",
    "top-control-contracts",
    "trace-bridge-contracts",
    "dft-bridge-contracts",
    "pin-space",
    "frontend-bridge",
    "frontend-reset-recovery",
    "single-load",
    "load-feedback",
    "topdown-contracts",
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
    "uncache-outstanding",
    "sbuffer-flush",
    "sbuffer-timeout",
    "mmio-contracts",
    "cbo-zero-contracts",
    "cmo-contracts",
    "wfi-safety",
    "reset-recovery",
    "reset-tree-contracts",
    "vector-load",
    "vector-split-load",
    "vector-fof",
    "vector-segment",
    "vector-segment-fof",
    "vector-issue-order",
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
    "pmp-contracts",
    "hypervisor-contracts",
    "pointer-masking-contracts",
    "mbmc-contracts",
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
    "ptw-errors",
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
TERMINAL_MARKERS = {
    scenario: "MEMBLOCK_" + scenario.upper().replace("-", "_")
    for scenario in SCENARIOS
}
TERMINAL_MARKERS.update(
    {
        "random-loads": "MEMBLOCK_RANDOM",
        "random-forwarding": "MEMBLOCK_RANDOM_FORWARD",
        "random-vector-loads": "MEMBLOCK_RANDOM_VECTOR",
        "random-vector-forwarding": "MEMBLOCK_RANDOM_VECTOR_FORWARD",
        "store-forwarding": "MEMBLOCK_STORE_FORWARD",
        "two-stage-translation": "MEMBLOCK_TWO_STAGE",
        "vector-guest-fault-split": "MEMBLOCK_VECTOR_GUEST_FAULT",
        "vector-store-forwarding": "MEMBLOCK_VECTOR_STORE_FORWARD",
    }
)
for _scenario in SCENARIOS:
    if _scenario.startswith("translation-fence"):
        TERMINAL_MARKERS[_scenario] = "MEMBLOCK_TRANSLATION_FENCE"
    elif _scenario.startswith("translation-inflight-context"):
        TERMINAL_MARKERS[_scenario] = "MEMBLOCK_TRANSLATION_INFLIGHT_CONTEXT"


class BenchmarkError(RuntimeError):
    pass


def parse_terminal(
    output: str,
    *,
    expected_scenario: str | None = None,
    expected_seed: int | None = None,
    expected_transactions: int | None = None,
    expected_rtl_sha256: str | None = None,
) -> dict[str, Any]:
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
    if expected_scenario is not None:
        marker = TERMINAL_MARKERS.get(expected_scenario)
        if marker is None:
            raise BenchmarkError(f"unsupported expected scenario: {expected_scenario}")
        if words[0] not in (marker + "_PASS", marker + "_FAIL"):
            raise BenchmarkError(
                f"terminal summary {words[0]} does not match {expected_scenario}"
            )
    for word in words[1:]:
        if "=" not in word:
            continue
        key, value = word.split("=", 1)
        try:
            fields[key] = int(value, 0)
        except ValueError:
            fields[key] = value
    for name, expected in (
        ("seed", expected_seed),
        ("transactions", expected_transactions),
    ):
        if expected is not None and name in fields and fields[name] != expected:
            raise BenchmarkError(
                f"terminal summary {name} is {fields[name]!r}, expected {expected}"
            )
    if (
        fields["status"] == "pass"
        and expected_rtl_sha256 is not None
        and fields.get("rtl_sha256") != expected_rtl_sha256
    ):
        raise BenchmarkError(
            "terminal summary rtl_sha256 is "
            f"{fields.get('rtl_sha256')!r}, expected {expected_rtl_sha256}"
        )
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
    expected_rtl_sha256: str | None = None,
    cancellation_event: threading.Event | None = None,
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
        command, timeout_seconds, environment, cancellation_event
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
        result = parse_terminal(
            output,
            expected_scenario=scenario,
            expected_seed=seed,
            expected_transactions=scenario_transactions,
            expected_rtl_sha256=expected_rtl_sha256,
        )
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
    # Keep the bounded tail for parsed failures too.  It contains the
    # scenario's phase/reason and is essential for reproducing a benchmark
    # failure; _run_process already caps the captured size.
    if result["status"] != "pass" or returncode != 0:
        result["output"] = output
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
        f"Overall status: **{document['status'].upper()}**.",
        "Frozen runtime: **{}**; controller inputs: **{}**; RTL identity: **{}**.".format(
            "PASS" if document["runtime"]["unchanged"] else "FAIL",
            "PASS" if document["controller"]["unchanged"] else "FAIL",
            "PASS" if document["rtl_identity"]["consistent"] else "FAIL",
        ),
        "",
        f"Measured at `{document['created_at']}` using up to {document['configuration']['jobs']} processes.",
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
    parser.add_argument("--rtl-metadata", type=Path, required=True)
    parser.add_argument(
        "--controller-file",
        type=Path,
        action="append",
        default=[],
        help="additional harness/config source to hash before and after the run",
    )
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--markdown", type=Path)
    parser.add_argument("--scenarios", default=",".join(SCENARIOS))
    parser.add_argument("--seed", type=int, default=1)
    parser.add_argument("--transactions", type=int, default=16384)
    parser.add_argument("--timeout-seconds", type=float, default=1800)
    parser.add_argument("--jobs", type=int, default=1)
    parser.add_argument(
        "--constraints", choices=run_regression.CONSTRAINT_PROFILES, default="spec"
    )
    parser.add_argument("--constraint", action="append", default=[])
    args = parser.parse_args()

    requested = tuple(item.strip() for item in args.scenarios.split(",") if item.strip())
    if not requested or len(set(requested)) != len(requested) or set(requested) - set(SCENARIOS):
        parser.error("--scenarios contains an unknown or duplicate scenario")
    if (
        args.seed < 0
        or args.transactions < 256
        or args.timeout_seconds <= 0
        or args.jobs <= 0
    ):
        parser.error(
            "seed, transactions, timeout, and jobs are outside the supported range"
        )

    try:
        runtime = run_regression.verify_runtime_metadata(args.runtime_metadata)
        rtl_metadata = args.rtl_metadata.resolve()
        rtl_metadata_sha256 = run_regression.sha256(rtl_metadata)
        if rtl_metadata_sha256 != runtime["artifact_hashes"]["rtl_metadata"]:
            raise run_regression.RegressionError(
                "--rtl-metadata does not match the frozen runtime artifact"
            )
        complete_rtl_sha256 = run_regression.read_complete_rtl_sha256(rtl_metadata)
    except (OSError, json.JSONDecodeError, run_regression.RegressionError) as error:
        print(f"benchmark_tests.py: error: {error}", file=sys.stderr)
        return 2
    environment = os.environ.copy()
    environment["LD_LIBRARY_PATH"] = str(runtime["root"])
    environment["LD_BIND_NOW"] = "1"

    worker_count = min(args.jobs, len(requested))
    if worker_count > 1 and environment.get("MEMBLOCK_MEM_DIRECT_TRACE_FILE"):
        print(
            "benchmark_tests.py: error: a shared MEMBLOCK_MEM_DIRECT_TRACE_FILE "
            "is unsafe with parallel leaves; use --jobs 1 for debug tracing",
            file=sys.stderr,
        )
        return 2

    controller_paths = {
        "benchmark_runner": Path(__file__).resolve(),
        "process_runner": Path(run_regression.__file__).resolve(),
    }
    controller_paths["rtl_metadata"] = rtl_metadata
    seen_controller_paths = set(controller_paths.values())
    for index, path in enumerate(args.controller_file):
        resolved = path.resolve()
        if not resolved.is_file():
            print(
                f"benchmark_tests.py: error: controller file is not a file: {resolved}",
                file=sys.stderr,
            )
            return 2
        if resolved in seen_controller_paths:
            print(
                f"benchmark_tests.py: error: duplicate controller file: {resolved}",
                file=sys.stderr,
            )
            return 2
        seen_controller_paths.add(resolved)
        controller_paths[f"controller_file_{index}"] = resolved
    try:
        controller_hashes_before = {
            role: run_regression.sha256(path)
            for role, path in controller_paths.items()
        }
    except OSError as error:
        print(f"benchmark_tests.py: error: {error}", file=sys.stderr)
        return 2

    started = time.monotonic()
    results_by_index: list[dict[str, Any] | None] = [None] * len(requested)
    cancellation_event = threading.Event()
    executor = concurrent.futures.ThreadPoolExecutor(
        max_workers=worker_count, thread_name_prefix="memblock-benchmark"
    )
    futures: dict[concurrent.futures.Future[dict[str, Any]], int] = {}
    try:
        futures = {
            executor.submit(
                run_scenario,
                runtime["binary"],
                scenario,
                args.seed,
                args.transactions,
                args.timeout_seconds,
                environment,
                args.constraints,
                tuple(args.constraint),
                complete_rtl_sha256,
                cancellation_event,
            ): index
            for index, scenario in enumerate(requested)
        }
        for future in concurrent.futures.as_completed(futures):
            index = futures[future]
            scenario = requested[index]
            try:
                result = future.result()
            except Exception as error:  # pragma: no cover - defensive worker boundary
                result = {
                    "scenario": scenario,
                    "status": "error",
                    "elapsed_seconds": 0.0,
                    "error": f"benchmark worker failed: {error}",
                }
            results_by_index[index] = result
            print(
                f"MEMBLOCK_BENCHMARK scenario={scenario} status={result['status']} "
                f"elapsed_seconds={result['elapsed_seconds']:.3f}",
                flush=True,
            )
    except KeyboardInterrupt:
        cancellation_event.set()
        for future in futures:
            future.cancel()
        executor.shutdown(wait=True, cancel_futures=True)
        print("MEMBLOCK_BENCHMARK_INTERRUPTED", file=sys.stderr, flush=True)
        return 130
    else:
        executor.shutdown(wait=True)

    # Completion order is nondeterministic; preserve command-line order in the
    # artifact so repeated benchmark reports remain easy to diff.
    results = [result for result in results_by_index if result is not None]

    runtime_after: dict[str, Any] | None = None
    runtime_unchanged = True
    runtime_error: str | None = None
    try:
        runtime_after = run_regression.verify_runtime_metadata(runtime["metadata"])
        runtime_unchanged = all(
            runtime[key] == runtime_after[key]
            for key in (
                "metadata_sha256",
                "artifact_hashes",
                "external_dependency_hashes",
            )
        )
        if not runtime_unchanged:
            runtime_error = "runtime hashes changed during the benchmark"
    except (OSError, json.JSONDecodeError, run_regression.RegressionError) as error:
        runtime_unchanged = False
        runtime_error = str(error)
    try:
        controller_hashes_after = {
            role: run_regression.sha256(path)
            for role, path in controller_paths.items()
        }
        controller_unchanged = controller_hashes_before == controller_hashes_after
        controller_error = (
            None
            if controller_unchanged
            else "controller inputs changed during the benchmark"
        )
    except OSError as error:
        controller_hashes_after = None
        controller_unchanged = False
        controller_error = str(error)

    observed_rtl_hashes = sorted(
        {
            str(result["rtl_sha256"])
            for result in results
            if "rtl_sha256" in result
        }
    )
    rtl_hash_consistent = (
        len(results) == len(requested)
        and all(
            result.get("rtl_sha256") == complete_rtl_sha256
            for result in results
        )
    )
    passed = (
        len(results) == len(requested)
        and all(result["status"] == "pass" for result in results)
        and runtime_unchanged
        and controller_unchanged
        and rtl_hash_consistent
    )
    document = {
        "schema_version": 2,
        "status": "pass" if passed else "fail",
        "created_at": dt.datetime.now(dt.timezone.utc).isoformat(),
        "elapsed_seconds": round(time.monotonic() - started, 6),
        "runtime_metadata": str(runtime["metadata"]),
        "runtime_metadata_sha256": runtime["metadata_sha256"],
        "binary_sha256": runtime["artifact_hashes"]["binary"],
        "complete_rtl_sha256": complete_rtl_sha256,
        "rtl_identity": {
            "metadata": str(rtl_metadata),
            "metadata_sha256": rtl_metadata_sha256,
            "observed_sha256": observed_rtl_hashes,
            "consistent": rtl_hash_consistent,
        },
        "runtime": {
            "metadata_sha256_before": runtime["metadata_sha256"],
            "metadata_sha256_after": (
                None
                if runtime_after is None
                else runtime_after["metadata_sha256"]
            ),
            "artifact_hashes_before": runtime["artifact_hashes"],
            "artifact_hashes_after": (
                None
                if runtime_after is None
                else runtime_after["artifact_hashes"]
            ),
            "external_dependency_hashes_before": runtime[
                "external_dependency_hashes"
            ],
            "external_dependency_hashes_after": (
                None
                if runtime_after is None
                else runtime_after["external_dependency_hashes"]
            ),
            "unchanged": runtime_unchanged,
            "error": runtime_error,
        },
        "controller": {
            "paths": {
                role: str(path) for role, path in controller_paths.items()
            },
            "hashes_before": controller_hashes_before,
            "hashes_after": controller_hashes_after,
            "unchanged": controller_unchanged,
            "error": controller_error,
        },
        "configuration": {
            "seed": args.seed,
            "transactions": args.transactions,
            "timeout_seconds": args.timeout_seconds,
            "jobs": args.jobs,
            "constraint_profile": args.constraints,
            "constraint_overrides": args.constraint,
            "scenarios": list(requested),
        },
        "results": results,
    }
    write_text_atomic(args.output, json.dumps(document, indent=2, sort_keys=True) + "\n")
    if args.markdown is not None:
        write_text_atomic(args.markdown, render_markdown(document))
    print(
        f"MEMBLOCK_BENCHMARK_{'PASS' if passed else 'FAIL'} "
        f"scenarios={len(results)}/{len(requested)} "
        f"elapsed_seconds={document['elapsed_seconds']:.3f} output={args.output}"
    )
    return 0 if passed else 1


if __name__ == "__main__":
    raise SystemExit(main())

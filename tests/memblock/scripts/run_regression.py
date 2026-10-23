#!/usr/bin/env python3
"""Run deterministic MemBlock random scenarios in parallel and write JSON results."""

from __future__ import annotations

import argparse
import concurrent.futures
import datetime as dt
import json
import math
import os
import platform
import signal
import subprocess
import sys
import tempfile
import threading
import time
import uuid
from pathlib import Path
from typing import Any


class RegressionError(RuntimeError):
    pass


FRONTEND_BRIDGE_SCENARIO = "frontend-bridge"
DEFAULT_SCENARIOS = (
    "random-loads",
    "random-forwarding",
    "random-vector-loads",
    "random-vector-forwarding",
    "random-mixed",
    FRONTEND_BRIDGE_SCENARIO,
)
BOUNDARY_HUNT_SCENARIO = "random-boundary-hunt"
STRESS_SCENARIO = "random-stress"
CONSTRAINED_SCENARIO = "random-mixed"
CONSTRAINT_PROFILES = ("coverage", "spec", "corner")
DEFAULT_JOBS = 8
MINIMUM_MIXED_TRANSACTIONS = 6144
DEFAULT_TRANSACTIONS = 16384
DEFAULT_MIXED_TRANSACTIONS = 16384
DEFAULT_TIMEOUT_SECONDS = 1800
MAX_CAPTURED_OUTPUT_BYTES = 32768
SUPPORTED_SCENARIOS = frozenset(
    (*DEFAULT_SCENARIOS, BOUNDARY_HUNT_SCENARIO, STRESS_SCENARIO)
)
RUNTIME_ROLES = frozenset({"binary", "model", "rtl_metadata", "xspcomm"})
FORWARDING_SCENARIOS = frozenset(
    {"random-forwarding", "random-vector-forwarding"}
)
TERMINAL_MARKERS = {
    "random-loads": "MEMBLOCK_RANDOM",
    "random-forwarding": "MEMBLOCK_RANDOM_FORWARD",
    "random-vector-loads": "MEMBLOCK_RANDOM_VECTOR",
    "random-vector-forwarding": "MEMBLOCK_RANDOM_VECTOR_FORWARD",
    "random-mixed": "MEMBLOCK_RANDOM_MIXED",
    FRONTEND_BRIDGE_SCENARIO: "MEMBLOCK_FRONTEND_BRIDGE",
    STRESS_SCENARIO: "MEMBLOCK_RANDOM_STRESS",
    BOUNDARY_HUNT_SCENARIO: "MEMBLOCK_RANDOM_BOUNDARY_HUNT",
}
COMPLETED_TRANSACTION_CAPS = {
    "random-forwarding": 48,
    "random-vector-forwarding": 24,
}


def transaction_count_for_scenario(
    scenario: str,
    transactions: int,
    forwarding_transactions: int,
    mixed_transactions: int = DEFAULT_MIXED_TRANSACTIONS,
) -> int:
    if scenario in ("random-mixed", STRESS_SCENARIO):
        return mixed_transactions
    if scenario == BOUNDARY_HUNT_SCENARIO:
        return transactions
    return forwarding_transactions if scenario in FORWARDING_SCENARIOS else transactions


def completed_transaction_count(scenario: str, requested: int) -> int:
    cap = COMPLETED_TRANSACTION_CAPS.get(scenario)
    return requested if cap is None else min(requested, cap)


def validate_worker_environment(
    jobs: int, environment: dict[str, str] | None = None
) -> None:
    trace_file = (environment or {}).get("MEMBLOCK_MEM_DIRECT_TRACE_FILE")
    if jobs > 1 and trace_file:
        raise RegressionError(
            "a shared MEMBLOCK_MEM_DIRECT_TRACE_FILE is unsafe with --jobs > 1; "
            "use --jobs 1 for debug tracing"
        )


def attach_failure_reports(fields: dict[str, Any], output: str) -> None:
    """Retain structured diagnostics without replacing the terminal verdict.

    Captured output can end in the middle of a long diagnostic line. Preserve
    that parse error explicitly; it must not erase an already observed failure.
    """
    reports = []
    errors = []
    prefix = "MEMBLOCK_FAILURES "
    for line in output.splitlines():
        if not line.startswith(prefix):
            continue
        try:
            report = json.loads(line[len(prefix):])
            if (not isinstance(report, dict) or report.get("schema_version") != 1
                    or type(report.get("truncated")) is not bool
                    or not isinstance(report.get("failures"), list)):
                raise ValueError("unsupported failure report schema")
            for failure in report["failures"]:
                if (not isinstance(failure, dict)
                        or any(not isinstance(failure.get(key), str)
                               for key in ("component", "code", "classification", "message", "file"))
                        or type(failure.get("line")) is not int
                        or failure["line"] <= 0
                        or (failure.get("cycle") is not None
                            and (type(failure["cycle"]) is not int or failure["cycle"] < 0))):
                    raise ValueError("invalid failure record")
            reports.append(report)
        except (ValueError, TypeError) as error:
            errors.append(str(error))
    if reports:
        fields["failure_reports"] = reports
    if errors:
        fields["failure_report_errors"] = errors


def parse_summary(
    output: str,
    *,
    expected_scenario: str | None = None,
    expected_seed: int | None = None,
    expected_transactions: int | None = None,
) -> dict[str, Any]:
    lines = [line.strip() for line in output.splitlines() if line.strip()]
    valid_tokens = {
        marker + suffix
        for marker in TERMINAL_MARKERS.values()
        for suffix in ("_PASS", "_FAIL")
    }
    summaries = [line for line in lines if line.split()[0] in valid_tokens]
    if not summaries:
        raise RegressionError("simulation output has no MEMBLOCK_RANDOM summary")
    if len(summaries) != 1:
        raise RegressionError(
            f"simulation output has {len(summaries)} terminal summaries, expected one"
        )
    fields: dict[str, Any] = {"summary": summaries[0]}
    words = summaries[0].split()
    if expected_scenario is not None:
        expected_marker = TERMINAL_MARKERS.get(expected_scenario)
        if expected_marker is None:
            raise RegressionError(f"unsupported expected scenario: {expected_scenario}")
        if words[0] not in (expected_marker + "_PASS", expected_marker + "_FAIL"):
            raise RegressionError(
                f"terminal summary {words[0]} does not match {expected_scenario}"
            )
    fields["status"] = "pass" if words[0].endswith("_PASS") else "fail"
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
        if expected is not None and fields.get(name) != expected:
            raise RegressionError(
                f"terminal summary {name} is {fields.get(name)!r}, expected {expected}"
            )
    attach_failure_reports(fields, output)
    return fields


def _metadata_path(entry: Any, description: str) -> Path:
    if not isinstance(entry, dict):
        raise RegressionError(f"{description} is not an object")
    raw_path = entry.get("path")
    if not isinstance(raw_path, str) or not raw_path:
        raise RegressionError(f"{description} has no valid path")
    return Path(raw_path)


def verify_runtime_metadata(path: Path) -> dict[str, Any]:
    metadata = path.resolve()
    document = json.loads(metadata.read_text(encoding="utf-8"))
    if document.get("schema_version") != 1:
        raise RegressionError(f"unsupported runtime metadata schema: {metadata}")
    entries = document.get("artifacts")
    if not isinstance(entries, list):
        raise RegressionError(f"runtime metadata has no artifact list: {metadata}")

    root = metadata.parent.resolve()
    roles: dict[str, Path] = {}
    for index, entry in enumerate(entries):
        relative = _metadata_path(entry, f"artifact {index}")
        role = entry.get("role")
        if not isinstance(role, str) or role in roles:
            raise RegressionError(f"invalid or duplicate runtime role: {role!r}")
        if relative.is_absolute() or relative.parts != (relative.name,):
            raise RegressionError(f"runtime artifact must be a local filename: {relative}")
        unresolved_artifact = root / relative
        artifact = unresolved_artifact.resolve()
        if (
            artifact.parent != root
            or unresolved_artifact.is_symlink()
            or not artifact.is_file()
        ):
            raise RegressionError(f"invalid runtime artifact: {artifact}")
        roles[role] = artifact
    if set(roles) != RUNTIME_ROLES:
        raise RegressionError(
            f"runtime roles are {sorted(roles)}, expected {sorted(RUNTIME_ROLES)}"
        )
    if not os.access(roles["binary"], os.X_OK):
        raise RegressionError(f"frozen binary is not executable: {roles['binary']}")

    external_dependencies: list[str] = []
    external = document.get("external_dependencies")
    if not isinstance(external, list):
        raise RegressionError("runtime metadata has no external dependency list")
    for index, entry in enumerate(external):
        dependency = _metadata_path(entry, f"external dependency {index}")
        if not dependency.is_absolute():
            raise RegressionError(f"external dependency is not absolute: {dependency}")
        dependency = dependency.resolve()
        if not dependency.is_file():
            raise RegressionError(f"external dependency is not a file: {dependency}")
        external_dependencies.append(str(dependency))

    return {
        "metadata": metadata,
        "root": root,
        "binary": roles["binary"],
        "artifacts": roles,
        "external_dependencies": external_dependencies,
    }


def _terminate_process_group(process: subprocess.Popen[bytes]) -> None:
    try:
        os.killpg(process.pid, signal.SIGTERM)
    except ProcessLookupError:
        pass
    deadline = time.monotonic() + 5.0
    while True:
        process.poll()
        try:
            os.killpg(process.pid, 0)
        except ProcessLookupError:
            break
        remaining = deadline - time.monotonic()
        if remaining <= 0:
            try:
                os.killpg(process.pid, signal.SIGKILL)
            except ProcessLookupError:
                pass
            break
        time.sleep(min(remaining, 0.05))
    process.wait()


def _run_process(
    command: list[str],
    timeout_seconds: float,
    environment: dict[str, str] | None,
    cancellation_event: threading.Event | None = None,
) -> tuple[int | None, str, bool]:
    with tempfile.TemporaryFile(mode="w+b") as output_file:
        process = subprocess.Popen(
            command,
            stdout=output_file,
            stderr=subprocess.STDOUT,
            env=environment,
            start_new_session=True,
        )
        timed_out = False
        try:
            if cancellation_event is None:
                process.wait(timeout=timeout_seconds)
            else:
                deadline = time.monotonic() + timeout_seconds
                while process.poll() is None:
                    if cancellation_event.is_set():
                        _terminate_process_group(process)
                        break
                    remaining = deadline - time.monotonic()
                    if remaining <= 0:
                        raise subprocess.TimeoutExpired(command, timeout_seconds)
                    cancellation_event.wait(timeout=min(remaining, 0.05))
        except subprocess.TimeoutExpired:
            timed_out = True
            _terminate_process_group(process)
        output_file.flush()
        size = output_file.seek(0, os.SEEK_END)
        output_file.seek(max(0, size - MAX_CAPTURED_OUTPUT_BYTES))
        output = output_file.read().decode(errors="replace")
        return (None if timed_out else process.returncode), output, timed_out


def run_seed(
    binary: Path,
    seed: int,
    scenario: str,
    transactions: int,
    forwarding_transactions: int,
    mixed_transactions: int,
    timeout_seconds: float,
    backpressure: bool,
    environment: dict[str, str] | None = None,
    hunt_boundaries: bool = False,
    constraint_profile: str = "coverage",
    constraint_overrides: tuple[str, ...] = (),
) -> dict[str, Any]:
    command = [
        str(binary),
        "--test",
        scenario,
        "--seed",
        str(seed),
        "--transactions",
        str(transaction_count_for_scenario(
            scenario, transactions, forwarding_transactions, mixed_transactions
        )),
    ]
    if scenario == CONSTRAINED_SCENARIO:
        command.extend(("--constraints", constraint_profile))
        for constraint in constraint_overrides:
            command.extend(("--constraint", constraint))
    if not backpressure:
        command.append("--no-backpressure")
    if hunt_boundaries:
        command.append("--hunt-boundaries")
    started = time.monotonic()
    returncode, output, timed_out = _run_process(
        command, timeout_seconds, environment
    )
    if not timed_out:
        try:
            result = parse_summary(
                output,
                expected_scenario=scenario,
                expected_seed=seed,
                expected_transactions=completed_transaction_count(
                    scenario,
                    transaction_count_for_scenario(
                        scenario,
                        transactions,
                        forwarding_transactions,
                        mixed_transactions,
                    ),
                ),
            )
        except RegressionError as error:
            result = {"status": "error", "error": str(error)}
        result["returncode"] = returncode
        if returncode != 0 and result["status"] == "pass":
            result["status"] = "error"
            result["error"] = "simulation returned nonzero after a pass summary"
    else:
        result = {
            "status": "timeout",
            "returncode": None,
            "error": f"seed exceeded {timeout_seconds:g} seconds",
        }
    result.update(
        {
            "seed": seed,
            "scenario": scenario,
            "elapsed_seconds": round(time.monotonic() - started, 6),
            "command": command,
            "output": "" if result.get("status") == "pass" else output,
        }
    )
    return result


def write_results(path: Path, document: dict[str, Any]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    temporary = path.with_name(path.name + ".tmp")
    temporary.write_text(json.dumps(document, indent=2, sort_keys=True) + "\n")
    os.replace(temporary, path)


def main() -> int:
    default_binary = (
        Path(__file__).resolve().parents[3]
        / "build/memblock/picker/UT_MemBlock/build/UTMemBlock_example"
    )
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--binary", type=Path)
    parser.add_argument(
        "--runtime-metadata",
        type=Path,
        help="frozen runtime.json; its artifact paths are verified",
    )
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--jobs", type=int, default=DEFAULT_JOBS)
    parser.add_argument("--start-seed", type=int, default=1)
    parser.add_argument("--seeds", type=int, default=8)
    parser.add_argument("--transactions", type=int, default=DEFAULT_TRANSACTIONS)
    parser.add_argument("--forwarding-transactions", type=int, default=48)
    parser.add_argument(
        "--mixed-transactions", type=int, default=DEFAULT_MIXED_TRANSACTIONS
    )
    parser.add_argument(
        "--constraints",
        choices=CONSTRAINT_PROFILES,
        default="coverage",
        help="random-mixed constraint preset",
    )
    parser.add_argument(
        "--constraint",
        action="append",
        default=[],
        help="random-mixed key=value override; may be repeated",
    )
    parser.add_argument(
        "--scenarios", default=",".join(DEFAULT_SCENARIOS),
        help="comma-separated test names",
    )
    parser.add_argument(
        "--timeout-seconds", type=float, default=DEFAULT_TIMEOUT_SECONDS
    )
    parser.add_argument("--duration-seconds", type=float)
    parser.add_argument(
        "--progress-interval-seconds",
        type=float,
        default=0,
        help="print an aggregate heartbeat instead of every passing seed",
    )
    parser.add_argument("--no-backpressure", action="store_true")
    parser.add_argument(
        "--hunt-boundaries", action="store_true",
        help="enable constrained-random split guest-fault boundary stimuli",
    )
    parser.add_argument(
        "--continue-on-failure", action="store_true",
        help="continue submitting seeds after a failure for bug hunting",
    )
    args = parser.parse_args()
    try:
        validate_worker_environment(args.jobs, os.environ)
    except RegressionError as error:
        print(f"run_regression.py: error: {error}", file=sys.stderr)
        return 2
    run_id = uuid.uuid4().hex
    invoked_at = dt.datetime.now(dt.timezone.utc)
    try:
        write_results(
            args.output,
            {
                "schema_version": 2,
                "campaign_status": "running",
                "run_id": run_id,
                "started_at": invoked_at.isoformat(),
                "results": [],
            },
        )
    except OSError as error:
        print(f"run_regression.py: error: cannot initialize output: {error}", file=sys.stderr)
        return 2

    runtime_before: dict[str, Any] | None = None
    execution_environment: dict[str, str] | None = None
    try:
        if args.runtime_metadata is not None:
            runtime_before = verify_runtime_metadata(args.runtime_metadata)
            binary = runtime_before["binary"]
            if args.binary is not None and args.binary.resolve() != binary:
                raise RegressionError(
                    "--binary does not match the binary in --runtime-metadata"
                )
            execution_environment = os.environ.copy()
            execution_environment["LD_LIBRARY_PATH"] = str(runtime_before["root"])
            execution_environment["LD_BIND_NOW"] = "1"
        else:
            binary = (args.binary or default_binary).resolve()
    except (OSError, json.JSONDecodeError, RegressionError) as error:
        print(f"run_regression.py: error: {error}", file=sys.stderr)
        return 2
    if not binary.is_file() or not os.access(binary, os.X_OK):
        print(f"run_regression.py: error: binary is not executable: {binary}", file=sys.stderr)
        return 2
    scenarios = [scenario.strip() for scenario in args.scenarios.split(",") if scenario.strip()]
    if (
        not scenarios
        or len(set(scenarios)) != len(scenarios)
        or set(scenarios) - SUPPORTED_SCENARIOS
    ):
        print("run_regression.py: error: unsupported or empty scenarios", file=sys.stderr)
        return 2
    if (
        args.jobs < 1
        or args.start_seed < 0
        or args.seeds < 1
        or args.transactions < 1
        or args.forwarding_transactions < 1
        or args.mixed_transactions < MINIMUM_MIXED_TRANSACTIONS
    ):
        print(
            "run_regression.py: error: jobs and transaction counts must be positive; "
            "mixed transactions must be at least "
            f"{MINIMUM_MIXED_TRANSACTIONS}",
            file=sys.stderr,
        )
        return 2
    duration_values = [args.timeout_seconds, args.progress_interval_seconds]
    if args.duration_seconds is not None:
        duration_values.append(args.duration_seconds)
    if (
        not all(math.isfinite(value) for value in duration_values)
        or args.timeout_seconds <= 0
        or (args.duration_seconds is not None and args.duration_seconds <= 0)
        or args.progress_interval_seconds < 0
    ):
        print("run_regression.py: error: durations must be finite and positive", file=sys.stderr)
        return 2
    started_wall = invoked_at
    started = time.monotonic()
    deadline = None if args.duration_seconds is None else started + args.duration_seconds
    results: list[dict[str, Any]] = []
    next_case = 0
    stop_submitting = False
    last_progress = started

    with concurrent.futures.ThreadPoolExecutor(max_workers=args.jobs) as executor:
        pending: dict[
            concurrent.futures.Future[dict[str, Any]], tuple[int, float]
        ] = {}

        def submit() -> None:
            nonlocal next_case
            seed = args.start_seed + next_case // len(scenarios)
            scenario = scenarios[next_case % len(scenarios)]
            future = executor.submit(
                run_seed,
                binary,
                seed,
                scenario,
                args.transactions,
                args.forwarding_transactions,
                args.mixed_transactions,
                args.timeout_seconds,
                not args.no_backpressure,
                execution_environment,
                args.hunt_boundaries,
                args.constraints,
                tuple(args.constraint),
            )
            pending[future] = (next_case, time.monotonic() - started)
            next_case += 1

        requested_cases = args.seeds * len(scenarios)
        initial = args.jobs if deadline is not None else min(args.jobs, requested_cases)
        for _ in range(initial):
            submit()

        while pending:
            done, _ = concurrent.futures.wait(
                pending, return_when=concurrent.futures.FIRST_COMPLETED
            )
            for future in done:
                _, submitted_offset = pending.pop(future)
                result = future.result()
                now = time.monotonic()
                result["submitted_offset_seconds"] = round(submitted_offset, 6)
                result["completed_offset_seconds"] = round(now - started, 6)
                results.append(result)
                if result["status"] != "pass" or args.progress_interval_seconds == 0:
                    print(
                        result.get(
                            "summary", f"seed={result['seed']} {result['status']}"
                        ),
                        flush=True,
                    )
                elif now - last_progress >= args.progress_interval_seconds:
                    print(
                        "MEMBLOCK_REGRESSION_PROGRESS cases={} elapsed_seconds={:.1f} "
                        "latest_seed={}".format(
                            len(results), now - started, result["seed"]
                        ),
                        flush=True,
                    )
                    last_progress = now
                if result["status"] != "pass" and not args.continue_on_failure:
                    stop_submitting = True
            while len(pending) < args.jobs and not stop_submitting:
                if deadline is None:
                    if next_case >= requested_cases:
                        break
                elif time.monotonic() >= deadline:
                    break
                submit()

    finished_wall = dt.datetime.now(dt.timezone.utc)
    results.sort(key=lambda result: (result["seed"], result["scenario"]))
    statuses: dict[str, int] = {}
    for result in results:
        statuses[result["status"]] = statuses.get(result["status"], 0) + 1
    document = {
        "schema_version": 2,
        "campaign_status": "complete",
        "run_id": run_id,
        "started_at": started_wall.isoformat(),
        "finished_at": finished_wall.isoformat(),
        "elapsed_seconds": round(time.monotonic() - started, 6),
        "host": platform.node(),
        "platform": platform.platform(),
        "binary": str(binary),
        "runtime": None if runtime_before is None else {
            "metadata": str(runtime_before["metadata"]),
            "root": str(runtime_before["root"]),
            "artifacts": {
                role: str(path)
                for role, path in runtime_before["artifacts"].items()
            },
            "external_dependencies": runtime_before["external_dependencies"],
        },
        "configuration": {
            "jobs": args.jobs,
            "start_seed": args.start_seed,
            "requested_seeds": None if deadline is not None else args.seeds,
            "duration_seconds": args.duration_seconds,
            "transactions_per_seed": args.transactions,
            "forwarding_transactions_per_seed": args.forwarding_transactions,
            "mixed_transactions_per_seed": args.mixed_transactions,
            "scenarios": scenarios,
            "timeout_seconds": args.timeout_seconds,
            "backpressure": not args.no_backpressure,
            "progress_interval_seconds": args.progress_interval_seconds,
            "hunt_boundaries": args.hunt_boundaries,
            "constraint_profile": args.constraints,
            "constraint_overrides": args.constraint,
        },
        "summary": {
            "seeds_completed": len(results),
            "statuses": statuses,
            "transactions_completed": sum(
                int(result.get("transactions", 0))
                for result in results
                if result["status"] == "pass"
            ),
        },
        "results": results,
    }
    write_results(args.output, document)
    print(
        "MEMBLOCK_REGRESSION_{} seeds={} transactions={} elapsed_seconds={:.3f} output={}".format(
            "PASS"
            if set(statuses) <= {"pass"} else "FAIL",
            len(results),
            document["summary"]["transactions_completed"],
            document["elapsed_seconds"],
            args.output,
        )
    )
    return 0 if set(statuses) <= {"pass"} else 1


if __name__ == "__main__":
    raise SystemExit(main())

#!/usr/bin/env python3
"""Verify a MemBlock duration-regression artifact without loading it in memory."""

from __future__ import annotations

import argparse
import datetime as dt
import json
import math
import sys
from collections import Counter
from pathlib import Path
from typing import Any, TextIO

import run_regression


# Artifacts produced by the enhanced mixed-test format have carried these
# coverage fields since 128 transactions. Keep their validation independent
# from the larger minimum accepted for new constrained-random submissions.
ENHANCED_MIXED_COVERAGE_TRANSACTIONS = 128


class VerificationError(RuntimeError):
    pass


# The focused forwarding scenarios intentionally stop before LSQ pointer reuse.
# The command records the requested stress level; the simulator summary records
# the number of transactions that the bounded scenario can legally complete.
def completed_transaction_count(scenario: str, requested: int) -> int:
    return run_regression.completed_transaction_count(scenario, requested)


class StreamingJsonReader:
    """Small structured reader for one large top-level JSON document."""

    def __init__(self, stream: TextIO, chunk_size: int = 1024 * 1024):
        if chunk_size < 1:
            raise ValueError("chunk_size must be positive")
        self.stream = stream
        self.chunk_size = chunk_size
        self.buffer = ""
        self.offset = 0
        self.eof = False
        self.decoder = json.JSONDecoder()

    def _fill(self) -> None:
        if self.offset:
            self.buffer = self.buffer[self.offset :]
            self.offset = 0
        chunk = self.stream.read(self.chunk_size)
        if chunk:
            self.buffer += chunk
        else:
            self.eof = True

    def _ensure_data(self) -> bool:
        while self.offset >= len(self.buffer) and not self.eof:
            self._fill()
        return self.offset < len(self.buffer)

    def skip_whitespace(self) -> None:
        while self._ensure_data():
            while self.offset < len(self.buffer) and self.buffer[self.offset].isspace():
                self.offset += 1
            if self.offset < len(self.buffer) or self.eof:
                return

    def consume(self, character: str) -> bool:
        self.skip_whitespace()
        if self._ensure_data() and self.buffer[self.offset] == character:
            self.offset += 1
            return True
        return False

    def expect(self, character: str) -> None:
        if not self.consume(character):
            found = "end of file"
            if self._ensure_data():
                found = repr(self.buffer[self.offset])
            raise VerificationError(f"expected {character!r}, found {found}")

    def value(self, maximum_bytes: int = 16 * 1024 * 1024) -> Any:
        self.skip_whitespace()
        start_size = len(self.buffer) - self.offset
        while True:
            try:
                value, end = self.decoder.raw_decode(self.buffer, self.offset)
                if end == len(self.buffer) and not self.eof:
                    self._fill()
                    continue
                if (
                    end < len(self.buffer)
                    and self.buffer[end] not in " \t\r\n,:]}"
                ):
                    if not self.eof:
                        self._fill()
                        continue
                    raise VerificationError(
                        f"invalid character after JSON value: {self.buffer[end]!r}"
                    )
                self.offset = end
                return value
            except json.JSONDecodeError as error:
                if self.eof:
                    raise VerificationError(f"invalid JSON: {error}") from error
                if len(self.buffer) - self.offset > maximum_bytes:
                    raise VerificationError(
                        f"single JSON value exceeds {maximum_bytes} bytes"
                    ) from error
                previous_size = len(self.buffer) - self.offset
                self._fill()
                current_size = len(self.buffer) - self.offset
                if current_size <= previous_size and current_size <= start_size:
                    raise VerificationError(f"invalid JSON: {error}") from error

    def finish(self) -> None:
        self.skip_whitespace()
        if self._ensure_data():
            raise VerificationError("trailing data after top-level JSON object")


def _require(condition: bool, message: str) -> None:
    if not condition:
        raise VerificationError(message)


def _positive_csv(result: dict[str, Any], name: str, fields: int) -> None:
    value = result.get(name)
    _require(isinstance(value, str), f"{name} is not a string")
    try:
        counts = [int(item, 10) for item in value.split(",")]
    except ValueError as error:
        raise VerificationError(f"{name} is not a decimal count list: {value!r}") from error
    _require(len(counts) == fields, f"{name} has {len(counts)} fields, expected {fields}")
    _require(all(count > 0 for count in counts), f"{name} has an uncovered class: {value}")


def _positive_csv_prefix(
    result: dict[str, Any], name: str, required_fields: int, total_fields: int
) -> None:
    value = result.get(name)
    _require(isinstance(value, str), f"{name} is not a string")
    try:
        counts = [int(item, 10) for item in value.split(",")]
    except ValueError as error:
        raise VerificationError(f"{name} is not a decimal count list: {value!r}") from error
    _require(len(counts) == total_fields, f"{name} has {len(counts)} fields, expected {total_fields}")
    _require(
        all(count > 0 for count in counts[:required_fields]),
        f"{name} has an uncovered required class: {value}",
    )


def _balanced_queue(result: dict[str, Any], name: str) -> None:
    value = result.get(name)
    _require(isinstance(value, str), f"{name} accounting is not a string")
    try:
        retired, allocated = value.split("/", 1)
        dequeued, canceled = retired.split("+", 1)
        balanced = int(dequeued, 10) + int(canceled, 10) == int(allocated, 10)
    except (ValueError, AttributeError) as error:
        raise VerificationError(f"invalid {name} accounting: {value!r}") from error
    _require(balanced, f"unbalanced {name} accounting: {value}")


def _check_mixed_coverage(
    result: dict[str, Any], require_backpressure: bool = False
) -> None:
    for name, fields in (
        ("load_ops", 7),
        ("store_ops", 4),
        ("scalar", 2),
        ("vector", 2),
        ("eew_load", 4),
        ("eew_store", 4),
        ("prefetch", 3),
        ("vstart", 2),
        ("vl", 2),
        ("align", 2),
        ("store_order", 2),
        ("forwarding", 4),
        ("memory_types", 2),
        ("dcache", 2),
        ("dispatch_widths", 6),
        ("dispatch_lanes", 6),
    ):
        _positive_csv(result, name, fields)
    for name in (
        "masked",
        "unmasked",
        "waves",
        "coissue",
        "ptw_requests",
        "uncache_requests",
        "tlb_reuse",
        "redirects",
        "dirty",
        "release_data",
    ):
        value = result.get(name)
        _require(
            isinstance(value, int) and not isinstance(value, bool) and value > 0,
            f"{name} coverage is absent: {value!r}",
        )
    transactions = result.get("transactions")
    if (
        isinstance(transactions, int)
        and transactions >= ENHANCED_MIXED_COVERAGE_TRANSACTIONS
    ):
        _positive_csv(result, "vec_load_modes", 4)
        _positive_csv(result, "vec_store_modes", 4)
        _positive_csv(result, "vec_load_stride", 3)
        _positive_csv(result, "vec_store_stride", 2)
        value = result.get("scalar_misaligned")
        _require(
            isinstance(value, int) and not isinstance(value, bool) and value > 0,
            f"scalar_misaligned coverage is absent: {value!r}",
        )
        _positive_csv(result, "store_misaligned", 2)
        for name in ("vector_replays", "virtualization", "exceptions"):
            value = result.get(name)
            _require(
                isinstance(value, int) and not isinstance(value, bool) and value > 0,
                f"{name} coverage is absent: {value!r}",
            )
        _positive_csv(result, "concurrent_ops", 5)
        for name in ("concurrent",):
            value = result.get(name)
            _require(isinstance(value, str), f"{name} coverage is not a string")
            try:
                windows, actions, overlap, unresolved, classes = (
                    int(item, 10) for item in value.split(",")
                )
            except (ValueError, TypeError) as error:
                raise VerificationError(f"invalid {name} coverage: {value!r}") from error
            _require(windows >= 4, f"{name} has too few windows: {value}")
            _require(actions >= 20, f"{name} has too few actions: {value}")
            _require(overlap > 0 and unresolved >= 2 and classes >= 2,
                     f"{name} lacks unresolved overlap: {value}")
        if require_backpressure:
            _positive_csv(result, "backpressure", 6)
    _require(
        isinstance(result.get("max_outstanding"), int)
        and result["max_outstanding"] > 1,
        "mixed traffic never had heterogeneous outstanding work",
    )
    _balanced_queue(result, "lq")
    _balanced_queue(result, "sq")


def _check_stress_coverage(
    result: dict[str, Any], require_backpressure: bool = False
) -> None:
    for name, fields in (
        ("stress_load_ops", 7),
        ("stress_store_ops", 4),
        ("stress_load_lanes", 3),
        ("stress_address_lanes", 2),
        ("stress_data_lanes", 2),
        ("stress_store_order", 2),
        ("stress_eew_load", 4),
        ("stress_eew_store", 4),
        ("stress_vec_load_modes", 3),
        ("stress_vec_store_modes", 3),
        ("stress_vec_lanes", 2),
        ("stress_prefetch", 3),
        ("stress_vstart", 2),
        ("stress_vl", 2),
        ("stress_alignment", 2),
        ("stress_forwarding", 2),
        ("stress_dcache", 2),
        ("stress_combinations", 4),
    ):
        if name in ("stress_vec_load_modes", "stress_vec_store_modes"):
            _positive_csv_prefix(result, name, fields, 4)
        else:
            _positive_csv(result, name, fields)
    for name in (
        "stress_masked",
        "stress_unmasked",
        "stress_misaligned",
        "stress_waves",
        "stress_regions",
    ):
        value = result.get(name)
        _require(
            isinstance(value, int) and not isinstance(value, bool) and value > 0,
            f"{name} coverage is absent: {value!r}",
        )
    _require(
        isinstance(result.get("stress_waves"), int)
        and result["stress_waves"] >= 4,
        "stress campaign has too few bursts",
    )
    _require(
        isinstance(result.get("stress_max_outstanding"), int)
        and result["stress_max_outstanding"] >= 10,
        "stress campaign never accumulated enough outstanding work",
    )
    _require(
        result.get("stress_actions") == result.get("transactions"),
        "stress coverage action count disagrees with transactions",
    )
    if require_backpressure:
        value = result.get("stress_backpressure")
        _require(isinstance(value, str), "stress_backpressure coverage is not a string")
        try:
            dcache_request, dcache_response, *_ = (
                int(item, 10) for item in value.split(",")
            )
        except (ValueError, TypeError) as error:
            raise VerificationError(
                f"invalid stress_backpressure coverage: {value!r}"
            ) from error
        _require(
            dcache_request > 0 and dcache_response > 0,
            f"stress campaign lacks DCache backpressure: {value}",
        )


def _check_result(
    result: Any,
    index: int,
    requested_transactions: dict[str, int],
    completed_transactions: dict[str, int],
    require_backpressure: bool = False,
) -> tuple[int, str, str, int, str, float]:
    prefix = f"result {index}"
    _require(isinstance(result, dict), f"{prefix} is not an object")
    _require(result.get("status") == "pass", f"{prefix} status is {result.get('status')!r}")
    _require(result.get("returncode") == 0, f"{prefix} return code is {result.get('returncode')!r}")
    _require(result.get("output") == "", f"{prefix} retained failure output")

    elapsed = result.get("elapsed_seconds")
    submitted_offset = result.get("submitted_offset_seconds")
    completed_offset = result.get("completed_offset_seconds")
    for name, value in (
        ("elapsed_seconds", elapsed),
        ("submitted_offset_seconds", submitted_offset),
        ("completed_offset_seconds", completed_offset),
    ):
        _require(
            isinstance(value, (int, float))
            and not isinstance(value, bool)
            and math.isfinite(value),
            f"{prefix} has invalid {name} {value!r}",
        )
    _require(elapsed >= 0, f"{prefix} has negative elapsed time")
    _require(submitted_offset >= 0, f"{prefix} was submitted before campaign start")
    _require(
        completed_offset >= submitted_offset,
        f"{prefix} completed before it was submitted",
    )
    _require(
        completed_offset + 0.01 >= submitted_offset + elapsed,
        f"{prefix} elapsed time exceeds its campaign interval",
    )

    seed = result.get("seed")
    _require(
        isinstance(seed, int) and not isinstance(seed, bool) and seed >= 0,
        f"{prefix} has invalid seed {seed!r}",
    )
    scenario = result.get("scenario")
    _require(
        isinstance(scenario, str) and scenario in requested_transactions,
        f"{prefix} scenario is {scenario!r}",
    )
    requested_count = requested_transactions[scenario]
    completed_count = completed_transactions[scenario]
    transactions = result.get("transactions")
    _require(
        transactions == completed_count,
        f"{prefix} transactions are {transactions!r}, expected {completed_count}",
    )
    rtl_hash = result.get("rtl_sha256")
    _require(
        isinstance(rtl_hash, str)
        and len(rtl_hash) == 64
        and set(rtl_hash) <= set("0123456789abcdef"),
        f"{prefix} has invalid RTL hash {rtl_hash!r}",
    )

    command = result.get("command")
    expected_tail = [
        "--test",
        scenario,
        "--seed",
        str(seed),
        "--transactions",
        str(requested_count),
    ]
    _require(
        isinstance(command, list)
        and len(command) == len(expected_tail) + 1
        and command[1:] == expected_tail,
        f"{prefix} command does not replay its recorded case",
    )

    summary = result.get("summary")
    _require(isinstance(summary, str), f"{prefix} has no simulator summary")
    try:
        parsed = run_regression.parse_summary(
            summary,
            expected_scenario=scenario,
            expected_seed=seed,
            expected_transactions=completed_count,
        )
    except run_regression.RegressionError as error:
        raise VerificationError(f"{prefix} has invalid simulator summary: {error}") from error
    for name, value in parsed.items():
        if name != "summary":
            _require(
                result.get(name) == value,
                f"{prefix} summary disagrees on {name}: {value!r} != {result.get(name)!r}",
            )
    if scenario == "random-mixed":
        _check_mixed_coverage(result, require_backpressure)
    elif scenario == run_regression.STRESS_SCENARIO:
        _check_stress_coverage(result, require_backpressure)
    return (
        seed,
        scenario,
        str(command[0]),
        int(transactions),
        rtl_hash,
        float(completed_offset),
    )


def _read_document(
    path: Path,
    requested_transactions: dict[str, int],
    completed_transactions: dict[str, int],
    require_backpressure: bool,
    chunk_size: int,
) -> tuple[dict[str, Any], dict[str, Any]]:
    metadata: dict[str, Any] = {}
    cases: set[tuple[int, str]] = set()
    scenario_counts: Counter[str] = Counter()
    binaries: set[str] = set()
    rtl_hashes: set[str] = set()
    statuses: Counter[str] = Counter()
    transaction_sum = 0
    result_count = 0
    completion_offsets: list[float] = []

    with path.open("r", encoding="utf-8") as stream:
        reader = StreamingJsonReader(stream, chunk_size=chunk_size)
        reader.expect("{")
        first_member = True
        while not reader.consume("}"):
            if not first_member:
                reader.expect(",")
            key = reader.value()
            _require(isinstance(key, str), "top-level JSON key is not a string")
            _require(key not in metadata, f"duplicate top-level key {key!r}")
            reader.expect(":")
            if key != "results":
                metadata[key] = reader.value()
            else:
                metadata[key] = None
                reader.expect("[")
                first_result = True
                while not reader.consume("]"):
                    if not first_result:
                        reader.expect(",")
                    result = reader.value()
                    try:
                        (
                            seed,
                            scenario,
                            binary,
                            transactions,
                            rtl_hash,
                            completed_offset,
                        ) = _check_result(
                            result,
                            result_count,
                            requested_transactions,
                            completed_transactions,
                            require_backpressure,
                        )
                    except VerificationError as error:
                        raise VerificationError(f"result {result_count}: {error}") from error
                    case = (seed, scenario)
                    _require(case not in cases, f"duplicate case {case!r}")
                    cases.add(case)
                    scenario_counts[scenario] += 1
                    binaries.add(binary)
                    rtl_hashes.add(rtl_hash)
                    statuses["pass"] += 1
                    transaction_sum += transactions
                    completion_offsets.append(completed_offset)
                    result_count += 1
                    first_result = False
            first_member = False
        reader.finish()

    return metadata, {
        "result_count": result_count,
        "cases": cases,
        "scenario_counts": dict(scenario_counts),
        "binaries": binaries,
        "rtl_hashes": rtl_hashes,
        "statuses": dict(statuses),
        "transactions": transaction_sum,
        "completion_offsets": completion_offsets,
    }


def _same_before_after(document: Any, name: str, fields: tuple[str, ...]) -> None:
    _require(isinstance(document, dict), f"{name} metadata is absent")
    _require(document.get("unchanged") is True, f"{name} is not marked unchanged")
    _require(document.get("error") is None, f"{name} records an error: {document.get('error')!r}")
    for field in fields:
        before = document.get(field + "_before")
        after = document.get(field + "_after")
        _require(before is not None, f"{name} has no {field}_before")
        _require(before == after, f"{name} {field} changed during the run")


def _parse_time(value: Any, name: str) -> dt.datetime:
    _require(isinstance(value, str), f"{name} is not a timestamp")
    try:
        timestamp = dt.datetime.fromisoformat(value)
    except ValueError as error:
        raise VerificationError(f"invalid {name}: {value!r}") from error
    _require(timestamp.tzinfo is not None, f"{name} has no timezone")
    return timestamp


def verify_regression(
    path: Path,
    *,
    min_duration_seconds: float,
    min_results: int,
    expected_transactions: int,
    expected_scenario: str | None = None,
    expected_scenarios: tuple[str, ...] | None = None,
    expected_forwarding_transactions: int | None = None,
    expected_mixed_transactions: int | None = None,
    expected_jobs: int | None = None,
    expected_rtl_sha256: str | None = None,
    expected_file_sha256: str | None = None,
    require_backpressure: bool = False,
    require_frozen_runtime: bool = False,
    runtime_metadata: Path | None = None,
    rtl_metadata: Path | None = None,
    runner: Path | None = None,
    controller_files: tuple[Path, ...] = (),
    chunk_size: int = 1024 * 1024,
    allow_finite: bool = False,
) -> dict[str, Any]:
    _require(
        math.isfinite(min_duration_seconds) and min_duration_seconds > 0,
        "minimum duration must be finite and positive",
    )
    _require(min_results > 0, "minimum result count must be positive")
    if expected_scenarios is None:
        scenarios = (expected_scenario or "random-mixed",)
    else:
        _require(
            expected_scenario is None,
            "specify expected_scenario or expected_scenarios, not both",
        )
        scenarios = expected_scenarios
    _require(bool(scenarios), "expected scenario list is empty")
    _require(
        len(set(scenarios)) == len(scenarios),
        "expected scenarios contain duplicates",
    )
    _require(
        set(scenarios) <= run_regression.SUPPORTED_SCENARIOS,
        "expected scenarios contain an unsupported scenario",
    )
    forwarding_transactions = (
        expected_transactions
        if expected_forwarding_transactions is None
        else expected_forwarding_transactions
    )
    mixed_transactions = (
        expected_transactions
        if expected_mixed_transactions is None
        else expected_mixed_transactions
    )
    requested_transaction_counts = {
        scenario: run_regression.transaction_count_for_scenario(
            scenario,
            expected_transactions,
            forwarding_transactions,
            mixed_transactions,
        )
        for scenario in scenarios
    }
    completed_transaction_counts = {
        scenario: completed_transaction_count(
            scenario, requested_transaction_counts[scenario]
        )
        for scenario in scenarios
    }

    path = path.resolve()
    _require(path.is_file(), f"result artifact is not a file: {path}")
    stat_before = path.stat()
    artifact_hash = run_regression.sha256(path)
    if expected_file_sha256 is not None:
        _require(
            artifact_hash == expected_file_sha256,
            f"artifact SHA-256 is {artifact_hash}, expected {expected_file_sha256}",
        )

    if rtl_metadata is not None:
        current_rtl_hash = run_regression.read_complete_rtl_sha256(rtl_metadata)
        if expected_rtl_sha256 is not None:
            _require(
                expected_rtl_sha256 == current_rtl_hash,
                "expected RTL hash disagrees with current RTL metadata",
            )
        expected_rtl_sha256 = current_rtl_hash

    metadata, observed = _read_document(
        path,
        requested_transaction_counts,
        completed_transaction_counts,
        require_backpressure,
        chunk_size,
    )
    stat_after = path.stat()
    _require(
        (stat_before.st_ino, stat_before.st_size, stat_before.st_mtime_ns)
        == (stat_after.st_ino, stat_after.st_size, stat_after.st_mtime_ns),
        "result artifact changed while it was being verified",
    )

    _require(metadata.get("schema_version") == 2, "unsupported regression schema")
    _require(
        metadata.get("campaign_status") == "complete",
        "regression campaign is not complete",
    )
    run_id = metadata.get("run_id")
    _require(
        isinstance(run_id, str)
        and len(run_id) == 32
        and set(run_id) <= set("0123456789abcdef"),
        "regression campaign has no valid run id",
    )
    elapsed = metadata.get("elapsed_seconds")
    _require(
        isinstance(elapsed, (int, float))
        and not isinstance(elapsed, bool)
        and math.isfinite(elapsed),
        "elapsed_seconds is not finite numeric data",
    )
    _require(elapsed >= min_duration_seconds, f"elapsed time {elapsed} is below requirement")
    started = _parse_time(metadata.get("started_at"), "started_at")
    finished = _parse_time(metadata.get("finished_at"), "finished_at")
    wall_seconds = (finished - started).total_seconds()
    _require(wall_seconds >= min_duration_seconds, "wall-clock timestamps are too short")
    _require(
        abs(float(elapsed) - wall_seconds) <= max(5.0, float(elapsed) * 0.01),
        "monotonic and wall-clock durations disagree",
    )

    configuration = metadata.get("configuration")
    _require(isinstance(configuration, dict), "configuration is absent")
    if expected_jobs is not None:
        _require(
            configuration.get("jobs") == expected_jobs,
            "configured worker count differs from verifier expectation",
        )
    requested_duration = configuration.get("duration_seconds")
    if allow_finite and requested_duration is None:
        pass
    else:
        _require(
            isinstance(requested_duration, (int, float))
            and not isinstance(requested_duration, bool)
            and math.isfinite(requested_duration)
            and requested_duration >= min_duration_seconds,
            "requested duration is below requirement or not finite",
        )
    _require(
        configuration.get("scenarios") == list(scenarios),
        "configured scenarios differ from verifier expectation",
    )
    if "random-mixed" in scenarios or run_regression.STRESS_SCENARIO in scenarios:
        _require(
            configuration.get("mixed_transactions_per_seed") == mixed_transactions,
            "configured mixed transaction count differs from verifier expectation",
        )
    if any(scenario in run_regression.FORWARDING_SCENARIOS for scenario in scenarios):
        _require(
            configuration.get("forwarding_transactions_per_seed")
            == forwarding_transactions,
            "configured forwarding transaction count differs from verifier expectation",
        )
    if any(
        scenario not in run_regression.FORWARDING_SCENARIOS
        and scenario != "random-mixed"
        for scenario in scenarios
    ):
        _require(
            configuration.get("transactions_per_seed") == expected_transactions,
            "configured transaction count differs from verifier expectation",
        )
    if require_backpressure:
        _require(configuration.get("backpressure") is True, "backpressure was disabled")

    result_count = observed["result_count"]
    _require(result_count >= min_results, f"only {result_count} results were recorded")
    _require(
        max(observed["completion_offsets"], default=-1) >= min_duration_seconds,
        "no recorded result completed after the required duration",
    )
    start_seed = configuration.get("start_seed")
    _require(isinstance(start_seed, int), "configuration has no integer start seed")
    expected_cases = {
        (
            start_seed + index // len(scenarios),
            scenarios[index % len(scenarios)],
        )
        for index in range(result_count)
    }
    _require(
        observed["cases"] == expected_cases,
        "recorded cases are not a continuous round-robin prefix",
    )

    complete_rtl_hash = metadata.get("complete_rtl_sha256")
    _require(
        isinstance(complete_rtl_hash, str), "complete RTL hash is absent from artifact"
    )
    if expected_rtl_sha256 is not None:
        _require(
            complete_rtl_hash == expected_rtl_sha256,
            "artifact complete RTL hash differs from expected RTL",
        )
    _require(
        observed["rtl_hashes"] == {complete_rtl_hash},
        "per-result RTL hashes are inconsistent with complete RTL metadata",
    )
    _require(
        observed["binaries"] == {metadata.get("binary")},
        "per-result commands do not all use the recorded binary",
    )

    summary = metadata.get("summary")
    _require(isinstance(summary, dict), "aggregate summary is absent")
    expected_summary = {
        "seeds_completed": result_count,
        "statuses": observed["statuses"],
        "transactions_completed": observed["transactions"],
        "rtl_sha256": [complete_rtl_hash],
        "rtl_hash_consistent": True,
        "runtime_unchanged": True,
        "controller_unchanged": True,
    }
    for name, value in expected_summary.items():
        _require(summary.get(name) == value, f"aggregate summary disagrees on {name}")

    controller = metadata.get("controller")
    _same_before_after(controller, "controller", ("hashes",))
    controller_hashes = controller["hashes_before"]
    if runner is not None:
        _require(
            controller_hashes.get("runner") == run_regression.sha256(runner),
            "current regression runner differs from the recorded controller",
        )
    if rtl_metadata is not None:
        _require(
            controller_hashes.get("rtl_metadata") == run_regression.sha256(rtl_metadata),
            "current RTL metadata differs from the recorded controller input",
        )
    if controller_files:
        recorded_paths = controller.get("paths")
        _require(isinstance(recorded_paths, dict), "controller paths are absent")
        recorded_by_path = {
            str(Path(value).resolve()): role
            for role, value in recorded_paths.items()
            if isinstance(value, str)
        }
        for controller_file in controller_files:
            resolved = str(controller_file.resolve())
            role = recorded_by_path.get(resolved)
            _require(role is not None, f"controller file is not recorded: {resolved}")
            _require(
                controller_hashes.get(role) == run_regression.sha256(Path(resolved)),
                f"current controller file differs from the recorded input: {resolved}",
            )

    runtime = metadata.get("runtime")
    if require_frozen_runtime or runtime_metadata is not None:
        _same_before_after(
            runtime,
            "runtime",
            ("metadata_sha256", "artifact_hashes", "external_dependency_hashes"),
        )
        _require(
            metadata.get("binary_sha256") == runtime["artifact_hashes_before"].get("binary"),
            "recorded binary hash differs from frozen runtime",
        )
    if runtime_metadata is not None:
        current_runtime = run_regression.verify_runtime_metadata(runtime_metadata)
        _require(
            runtime["metadata_sha256_before"] == current_runtime["metadata_sha256"],
            "current runtime manifest differs from the recorded manifest",
        )
        _require(
            runtime["artifact_hashes_before"] == current_runtime["artifact_hashes"],
            "current frozen artifacts differ from the recorded runtime",
        )
        _require(
            runtime["external_dependency_hashes_before"]
            == current_runtime["external_dependency_hashes"],
            "current system libraries differ from the recorded runtime",
        )

    return {
        "artifact_sha256": artifact_hash,
        "elapsed_seconds": float(elapsed),
        "result_count": result_count,
        "transactions": observed["transactions"],
        "first_seed": min(seed for seed, _ in observed["cases"]),
        "last_seed": max(seed for seed, _ in observed["cases"]),
        "scenario_counts": observed["scenario_counts"],
        "rtl_sha256": complete_rtl_hash,
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input", type=Path, required=True)
    parser.add_argument("--min-duration-seconds", type=float, default=14400)
    parser.add_argument("--min-results", type=int, default=1)
    parser.add_argument("--scenario")
    parser.add_argument("--scenarios", help="comma-separated ordered test names")
    parser.add_argument("--transactions", type=int, default=64)
    parser.add_argument("--forwarding-transactions", type=int)
    parser.add_argument("--mixed-transactions", type=int)
    parser.add_argument("--expected-jobs", type=int)
    parser.add_argument("--rtl-sha256")
    parser.add_argument("--expected-file-sha256")
    parser.add_argument("--require-backpressure", action="store_true")
    parser.add_argument("--require-frozen-runtime", action="store_true")
    parser.add_argument("--runtime-metadata", type=Path)
    parser.add_argument("--rtl-metadata", type=Path)
    parser.add_argument("--runner", type=Path)
    parser.add_argument(
        "--controller-file", type=Path, action="append", default=[],
        help="source file that must be present and hash-matched in controller metadata",
    )
    parser.add_argument(
        "--allow-finite",
        action="store_true",
        help="verify a finite seed prefix without a requested duration",
    )
    args = parser.parse_args()

    try:
        expected_scenarios = None
        if args.scenarios is not None:
            expected_scenarios = tuple(
                scenario.strip()
                for scenario in args.scenarios.split(",")
                if scenario.strip()
            )
        result = verify_regression(
            args.input,
            min_duration_seconds=args.min_duration_seconds,
            min_results=args.min_results,
            expected_transactions=args.transactions,
            expected_scenario=(
                None if expected_scenarios is not None else args.scenario
            ),
            expected_scenarios=expected_scenarios,
            expected_forwarding_transactions=args.forwarding_transactions,
            expected_mixed_transactions=args.mixed_transactions,
            expected_jobs=args.expected_jobs,
            expected_rtl_sha256=args.rtl_sha256,
            expected_file_sha256=args.expected_file_sha256,
            require_backpressure=args.require_backpressure,
            require_frozen_runtime=args.require_frozen_runtime,
            runtime_metadata=args.runtime_metadata,
            rtl_metadata=args.rtl_metadata,
            runner=args.runner,
            controller_files=tuple(args.controller_file),
            allow_finite=args.allow_finite,
        )
    except (OSError, json.JSONDecodeError, run_regression.RegressionError, VerificationError) as error:
        print(f"verify_regression.py: error: {error}", file=sys.stderr)
        return 1

    print(
        "MEMBLOCK_REGRESSION_ARTIFACT_PASS "
        f"seeds={result['first_seed']}..{result['last_seed']} "
        f"results={result['result_count']} transactions={result['transactions']} "
        f"elapsed_seconds={result['elapsed_seconds']:.6f} "
        f"rtl_sha256={result['rtl_sha256']} "
        f"artifact_sha256={result['artifact_sha256']}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())

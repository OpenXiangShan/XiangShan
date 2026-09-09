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
    counts = _csv_counts(result, name, fields)
    _require(
        all(count > 0 for count in counts),
        f"{name} has an uncovered class: {result.get(name)}",
    )


def _csv_counts(result: dict[str, Any], name: str, fields: int) -> list[int]:
    value = result.get(name)
    _require(isinstance(value, str), f"{name} is not a string")
    try:
        counts = [int(item, 10) for item in value.split(",")]
    except ValueError as error:
        raise VerificationError(f"{name} is not a decimal count list: {value!r}") from error
    _require(len(counts) == fields, f"{name} has {len(counts)} fields, expected {fields}")
    _require(all(count >= 0 for count in counts), f"{name} has a negative count: {value}")
    return counts


def _expected_probe_source_lifecycle(
    probes: int, source_space: int
) -> list[int]:
    unique_sources = min(probes, source_space)
    return [
        unique_sources,
        probes - unique_sources,
        0 if probes == 0 else (probes - 1) // source_space,
    ]


def _check_vector_shape_cross(
    result: dict[str, Any],
    target_operations: list[int],
    vector_targets: dict[str, list[int]],
    vector_actuals: dict[str, list[int]],
    policy_targets: dict[str, int],
    vector_directions: list[int],
    vector_shape_operations: int,
) -> None:
    """Check direction x addressing x legal RVV shape and its marginals."""

    vector_cross = _csv_counts(
        result, "actual_vector_cross", 2 * 4 * 4 * 4 * 7
    )
    fixed_policies = sum(
        policy_targets[name] == 1000
        for name in ("masked", "partial_vl", "nonzero_vstart")
    )
    minimum_vlmax = 3 if fixed_policies == 3 else 2 if fixed_policies == 2 else 1
    crossed_direction = [0, 0]
    crossed = {
        "addressing": [0] * 4,
        "eew": [0] * 4,
        "sew": [0] * 4,
        "lmul": [0] * 7,
        "emul": [0] * 7,
    }
    index = 0
    for direction in range(2):
        for addressing in range(4):
            for eew in range(4):
                for sew in range(4):
                    for lmul in range(7):
                        count = vector_cross[index]
                        index += 1
                        lmul_log2 = lmul - 3
                        emul_log2 = eew - sew + lmul_log2
                        vector_bytes = (
                            16 >> -lmul_log2
                            if lmul_log2 < 0
                            else 16 << lmul_log2
                        )
                        legal = (
                            lmul_log2 >= sew - 3
                            and -3 <= emul_log2 <= 3
                            and vector_bytes >> sew >= minimum_vlmax
                        )
                        enabled = (
                            target_operations[2 + direction] != 0
                            and vector_targets["addressing"][addressing] != 0
                            and vector_targets["eew"][eew] != 0
                            and vector_targets["sew"][sew] != 0
                            and vector_targets["lmul"][lmul] != 0
                            and legal
                            and vector_targets["emul"][emul_log2 + 3] != 0
                        )
                        _require(
                            (count > 0) == enabled,
                            "actual_vector_cross does not match enabled legal "
                            "shapes: "
                            f"direction={direction} addressing={addressing} "
                            f"eew={eew} sew={sew} lmul_log2={lmul_log2}",
                        )
                        crossed_direction[direction] += count
                        crossed["addressing"][addressing] += count
                        crossed["eew"][eew] += count
                        crossed["sew"][sew] += count
                        crossed["lmul"][lmul] += count
                        if legal:
                            crossed["emul"][emul_log2 + 3] += count
    _require(
        crossed_direction == vector_directions
        and all(
            crossed[dimension] == vector_actuals[dimension]
            for dimension in crossed
        )
        and sum(vector_cross) == vector_shape_operations,
        "ordinary vector cross/marginal coverage is not conserved",
    )


def _check_constraint_coverage(result: dict[str, Any]) -> None:
    schema = result.get("constraint_schema")
    if schema is None:
        return
    _require(
        schema in (
            2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18,
            19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,
            33, 34, 35, 36, 37, 38, 39, 40, 41,
        ),
        f"unsupported constraint_schema: {schema!r}",
    )

    target_translation = _csv_counts(result, "target_translation", 3)
    actual_translation = _csv_counts(result, "actual_translation", 3)
    target_stage1 = _csv_counts(result, "target_stage1_mode", 2)
    actual_stage1 = _csv_counts(result, "actual_stage1_mode", 2)
    target_vs = _csv_counts(result, "target_vs_mode", 2)
    actual_vs = _csv_counts(result, "actual_vs_mode", 2)
    target_g = _csv_counts(result, "target_g_mode", 2)
    actual_g = _csv_counts(result, "actual_g_mode", 2)

    _require(
        all(
            weight == 0 or count > 0
            for weight, count in zip(target_translation, actual_translation)
        ),
        "actual_translation has an enabled but uncovered class: "
        f"{actual_translation}",
    )
    if target_translation[1] != 0:
        _require(
            all(
                weight == 0 or count > 0
                for weight, count in zip(target_stage1, actual_stage1)
            ),
            f"actual_stage1_mode has an enabled but uncovered class: {actual_stage1}",
        )
    if target_translation[2] != 0:
        for name, target, actual in (
            ("actual_vs_mode", target_vs, actual_vs),
            ("actual_g_mode", target_g, actual_g),
        ):
            _require(
                all(
                    weight == 0 or count > 0
                    for weight, count in zip(target, actual)
                ),
                f"{name} has an enabled but uncovered class: {actual}",
            )

    actual_nested = _csv_counts(result, "actual_nested_pairs", 4)
    if target_translation[2] != 0:
        for vs_mode, vs_weight in enumerate(target_vs):
            for g_mode, g_weight in enumerate(target_g):
                if vs_weight != 0 and g_weight != 0:
                    _require(
                        actual_nested[vs_mode * 2 + g_mode] > 0,
                        "actual_nested_pairs has an enabled but uncovered pair: "
                        f"{actual_nested}",
                    )

    if schema >= 12:
        target_stage1_napot = result.get("target_stage1_napot")
        target_nested_vs_napot = result.get("target_nested_vs_napot")
        target_nested_g_napot = result.get("target_nested_g_napot")
        for name, value in (
            ("target_stage1_napot", target_stage1_napot),
            ("target_nested_vs_napot", target_nested_vs_napot),
            ("target_nested_g_napot", target_nested_g_napot),
        ):
            _require(
                isinstance(value, int) and not isinstance(value, bool)
                and 0 <= value <= 1000,
                f"{name} is not a per-mille integer: {value!r}",
            )

        actual_stage1_leaf = _csv_counts(result, "actual_stage1_leaf", 2)
        actual_nested_leaf = _csv_counts(
            result, "actual_nested_leaf_topology", 4
        )
        if target_translation[1] != 0:
            _require(
                (target_stage1_napot == 1000 or actual_stage1_leaf[0] > 0)
                and (target_stage1_napot == 0 or actual_stage1_leaf[1] > 0),
                "actual_stage1_leaf has an enabled but uncovered class: "
                f"{actual_stage1_leaf}",
            )
        else:
            _require(
                actual_stage1_leaf == [0, 0],
                "disabled stage-1 translation has leaf observations: "
                f"{actual_stage1_leaf}",
            )
        if target_translation[2] != 0:
            for vs_napot in range(2):
                for g_napot in range(2):
                    vs_enabled = (
                        target_nested_vs_napot != 0
                        if vs_napot else target_nested_vs_napot != 1000
                    )
                    g_enabled = (
                        target_nested_g_napot != 0
                        if g_napot else target_nested_g_napot != 1000
                    )
                    count = actual_nested_leaf[vs_napot * 2 + g_napot]
                    _require(
                        (count > 0) == (vs_enabled and g_enabled),
                        "actual_nested_leaf_topology does not match enabled "
                        f"classes: {actual_nested_leaf}",
                    )
        else:
            _require(
                actual_nested_leaf == [0, 0, 0, 0],
                "disabled nested translation has leaf observations: "
                f"{actual_nested_leaf}",
            )

    target_fence = _csv_counts(result, "target_fence_kind", 3)
    target_scope = _csv_counts(result, "target_fence_scope", 2)
    actual_fences = _csv_counts(result, "actual_fences", 6)
    target_flush = result.get("target_tlb_flush")
    _require(
        isinstance(target_flush, int) and not isinstance(target_flush, bool),
        f"target_tlb_flush is not an integer: {target_flush!r}",
    )
    if target_flush != 0:
        for kind, weight in enumerate(target_fence):
            compatible = (
                kind == 0 and target_translation[1] != 0
            ) or (
                kind != 0 and target_translation[2] != 0
            )
            if not compatible or weight == 0:
                continue
            for scope, scope_weight in enumerate(target_scope):
                if scope_weight != 0:
                    _require(
                        actual_fences[kind * 2 + scope] > 0,
                        "actual_fences has an enabled but uncovered class: "
                        f"{actual_fences}",
                    )

    translated = target_translation[1] != 0 or target_translation[2] != 0
    walk_reuse = _csv_counts(result, "actual_translation_walk_reuse", 2)
    if translated:
        _require(
            all(count > 0 for count in walk_reuse),
            "actual_translation_walk_reuse lacks cold-walk or reuse coverage: "
            f"{walk_reuse}",
        )
    required_contexts = int(target_translation[0] != 0)
    if target_translation[1] != 0:
        required_contexts += sum(weight != 0 for weight in target_stage1)
    if target_translation[2] != 0:
        required_contexts += (
            sum(weight != 0 for weight in target_vs)
            * sum(weight != 0 for weight in target_g)
        )
    actual_switches = result.get("actual_translation_switch")
    _require(
        isinstance(actual_switches, int) and not isinstance(actual_switches, bool),
        f"actual_translation_switch is not an integer: {actual_switches!r}",
    )
    if required_contexts > 1:
        _require(actual_switches > 0, "translation contexts never switched")

    if schema >= 3:
        target_probe = result.get("target_probe")
        target_to_b = result.get("target_probe_to_b")
        target_need_data = result.get("target_probe_need_data")
        for name, value in (
            ("target_probe", target_probe),
            ("target_probe_to_b", target_to_b),
            ("target_probe_need_data", target_need_data),
        ):
            _require(
                isinstance(value, int) and not isinstance(value, bool)
                and 0 <= value <= 1000,
                f"{name} is not a per-mille integer: {value!r}",
            )
        actual_sequences = result.get("actual_probe_sequences")
        actual_caps = _csv_counts(result, "actual_probe_caps", 2)
        actual_need_data = _csv_counts(
            result, "actual_probe_need_data", 2
        )
        target_probe_overlap = (
            result.get("target_probe_overlap") if schema >= 14 else 0
        )
        target_probe_triple_overlap = (
            result.get("target_probe_triple_overlap") if schema >= 32 else 0
        )
        target_probe_deep_depth = (
            _csv_counts(result, "target_probe_deep_depth", 6)
            if schema >= 33
            else [1]
        )
        actual_probe_overlap = (
            _csv_counts(result, "actual_probe_overlap", 2)
            if schema >= 14
            else [0, 0]
        )
        actual_probe_depth = (
            _csv_counts(result, "actual_probe_depth", 8 if schema >= 33 else 3)
            if schema >= 32
            else [actual_probe_overlap[0], actual_probe_overlap[1], 0]
        )
        actual_probe_cross = (
            _csv_counts(result, "actual_probe_cross", 32)
            if schema >= 33
            else []
        )
        probe_max_outstanding = (
            result.get("probe_max_outstanding") if schema >= 14 else 0
        )
        probe_source_space = (
            result.get("probe_source_space") if schema >= 34 else 0
        )
        probe_source_lifecycle = (
            _csv_counts(result, "probe_source_lifecycle", 3)
            if schema >= 34
            else [0, 0, 0]
        )
        if schema >= 34:
            _require(
                probe_source_space == 64,
                f"unexpected DCache Probe source space: {probe_source_space!r}",
            )
        if schema >= 14:
            _require(
                isinstance(target_probe_overlap, int)
                and not isinstance(target_probe_overlap, bool)
                and 0 <= target_probe_overlap <= 1000,
                "target_probe_overlap is not a per-mille integer: "
                f"{target_probe_overlap!r}",
            )
            _require(
                isinstance(probe_max_outstanding, int)
                and not isinstance(probe_max_outstanding, bool)
                and probe_max_outstanding >= 0,
                "probe_max_outstanding is invalid: "
                f"{probe_max_outstanding!r}",
            )
        if schema >= 32:
            _require(
                isinstance(target_probe_triple_overlap, int)
                and not isinstance(target_probe_triple_overlap, bool)
                and 0 <= target_probe_triple_overlap <= 1000,
                "target_probe_triple_overlap is not a per-mille integer: "
                f"{target_probe_triple_overlap!r}",
            )
        if (
            schema >= 33
            and target_probe != 0
            and target_probe_overlap != 0
            and target_probe_triple_overlap != 0
        ):
            _require(
                any(weight != 0 for weight in target_probe_deep_depth),
                "target_probe_deep_depth cannot be all zero when deep Probe "
                "overlap is enabled",
            )
        cmo_probe_count = 0
        if schema >= 36:
            cmo_probe_depths = _csv_counts(
                result, "actual_cmo_probe_depth", 8
            )
            cmo_probe_count = sum(
                (depth + 1) * count
                for depth, count in enumerate(cmo_probe_depths)
            )
        elif schema >= 13:
            cmo_probe_count = _csv_counts(
                result, "actual_ops",
                15 if schema >= 39 else 14 if schema >= 21 else 13 if schema >= 20 else
                12 if schema >= 19 else 11,
            )[10]
            if schema >= 15:
                cmo_probe_count -= sum(
                    _csv_counts(result, "actual_cmo_error_kind", 2)
                )
                _require(
                    cmo_probe_count >= 0,
                    "CMO error count exceeds CMO operation count",
                )
        atomic_probe_count = 0
        if schema >= 37:
            atomic_probe_depths = _csv_counts(
                result, "actual_atomic_probe_depth", 9
            )
            atomic_probe_count = sum(
                depth * count
                for depth, count in enumerate(atomic_probe_depths)
            )
        _require(
            isinstance(actual_sequences, int)
            and not isinstance(actual_sequences, bool)
            and actual_sequences >= 0,
            f"actual_probe_sequences is invalid: {actual_sequences!r}",
        )
        if target_probe != 0:
            _require(actual_sequences > 0, "no constrained Probe sequence ran")
            for name, target, actual in (
                ("actual_probe_caps", target_to_b, actual_caps),
                ("actual_probe_need_data", target_need_data, actual_need_data),
            ):
                _require(
                    (target == 1000 or actual[0] > 0)
                    and (target == 0 or actual[1] > 0),
                    f"{name} has an enabled but uncovered class: {actual}",
                )
        elif schema >= 13:
            _require(
                actual_sequences == 0
                and actual_caps == [0, 0]
                and actual_need_data == [0, 0]
                and (schema < 14 or actual_probe_overlap == [0, 0])
                and (
                    schema < 32
                    or actual_probe_depth == [0] * len(actual_probe_depth)
                )
                and (schema < 33 or actual_probe_cross == [0] * 32),
                "disabled constrained Probe has coverage observations",
            )
        if target_probe != 0 or schema >= 13:
            _require(
                sum(actual_caps) == actual_sequences
                and sum(actual_need_data) == actual_sequences,
                "constrained Probe coverage is not conserved",
            )
            if schema >= 14 and target_probe != 0:
                _require(
                    (
                        target_probe_overlap == 0
                        and actual_probe_overlap[0] > 0
                        and actual_probe_overlap[1] == 0
                    )
                    or (
                        target_probe_overlap == 1000
                        and actual_probe_overlap[0] == 0
                        and actual_probe_overlap[1] > 0
                    )
                    or (
                        0 < target_probe_overlap < 1000
                        and actual_probe_overlap[0] > 0
                        and actual_probe_overlap[1] > 0
                    ),
                    "actual_probe_overlap has an enabled but uncovered class: "
                    f"{actual_probe_overlap}",
                )
                _require(
                    sum(actual_probe_overlap) == actual_sequences,
                    "Probe overlap coverage is not conserved",
                )
                if actual_probe_overlap[1] > 0:
                    _require(
                        probe_max_outstanding >= 2,
                        "Probe overlap never reached two outstanding sources",
                    )
                if schema >= 32:
                    depth_enabled = (
                        [
                            target_probe_overlap != 1000,
                            target_probe_overlap != 0
                            and target_probe_triple_overlap != 1000,
                            *[
                                target_probe_overlap != 0
                                and target_probe_triple_overlap != 0
                                and weight != 0
                                for weight in target_probe_deep_depth
                            ],
                        ]
                        if schema >= 33
                        else [
                            target_probe_overlap != 1000,
                            target_probe_overlap != 0
                            and target_probe_triple_overlap != 1000,
                            target_probe_overlap != 0
                            and target_probe_triple_overlap != 0,
                        ]
                    )
                    _require(
                        all(
                            (actual_probe_depth[index] > 0) == enabled
                            for index, enabled in enumerate(depth_enabled)
                        ),
                        "actual_probe_depth has an enabled but uncovered class: "
                        f"{actual_probe_depth}",
                    )
                    _require(
                        sum(actual_probe_depth) == actual_sequences,
                        "Probe depth coverage is not conserved",
                    )
                    _require(
                        actual_probe_overlap
                        == [
                            actual_probe_depth[0],
                            sum(actual_probe_depth[1:]),
                        ],
                        "Probe overlap compatibility projection is not conserved",
                    )
                    highest_probe_depth = max(
                        (
                            index + 1
                            for index, count in enumerate(actual_probe_depth)
                            if count > 0
                        ),
                        default=0,
                    )
                    if highest_probe_depth > 0:
                        _require(
                            probe_max_outstanding >= highest_probe_depth,
                            "Probe overlap never reached its selected outstanding depth",
                        )
                    if schema >= 33:
                        crossed_depths = [0] * 8
                        crossed_caps = [0, 0]
                        crossed_data = [0, 0]
                        for depth in range(8):
                            for cap in range(2):
                                cap_enabled = (
                                    target_to_b != 0
                                    if cap else target_to_b != 1000
                                )
                                for data in range(2):
                                    data_enabled = (
                                        target_need_data != 0
                                        if data else target_need_data != 1000
                                    )
                                    index = depth * 4 + cap * 2 + data
                                    count = actual_probe_cross[index]
                                    enabled = (
                                        depth_enabled[depth]
                                        and cap_enabled
                                        and data_enabled
                                    )
                                    _require(
                                        (count > 0) == enabled,
                                        "actual_probe_cross has an enabled but "
                                        f"uncovered class: {actual_probe_cross}",
                                    )
                                    crossed_depths[depth] += count
                                    crossed_caps[cap] += count
                                    crossed_data[data] += count
                        _require(
                            crossed_depths == actual_probe_depth
                            and crossed_caps == actual_caps
                            and crossed_data == actual_need_data,
                            "Probe cross coverage does not match its marginals",
                        )
            probes = result.get("probes")
            auxiliary_probes = (
                sum(
                    depth_class * count
                    for depth_class, count in enumerate(actual_probe_depth)
                )
                if schema >= 32
                else actual_probe_overlap[1]
            )
            _require(
                isinstance(probes, int) and not isinstance(probes, bool)
                and probes == actual_sequences + actual_caps[1]
                + cmo_probe_count + atomic_probe_count + auxiliary_probes,
                "manager Probe count does not match constrained/overlap/CMO "
                "accounting including atomic composition",
            )
            if schema >= 34:
                _require(
                    probe_source_lifecycle
                    == _expected_probe_source_lifecycle(
                        probes, probe_source_space
                    ),
                    "DCache Probe source lifecycle does not match accepted "
                    "manager traffic",
                )

    if schema >= 4:
        # These top-level pulses expose useful implementation diagnostics, but
        # their presence and cardinality are not architectural obligations.
        # Parse their shape without turning replay policy into a closure gate.
        _csv_counts(result, "load_wakeups", 3)
        _csv_counts(result, "load_cancels", 3)

    if schema >= 5:
        ifetch_prefetches = result.get("ifetch_prefetches")
        _require(
            isinstance(ifetch_prefetches, int)
            and not isinstance(ifetch_prefetches, bool)
            and ifetch_prefetches > 0,
            "no software instruction-prefetch output was observed",
        )

    if schema >= 6:
        target_stride_stream = result.get("target_stride_stream")
        stride_prefetches = result.get("l2_stride_prefetches")
        _csv_counts(result, "raw_load_wakeups", 3)
        _csv_counts(result, "raw_load_cancels", 3)
        _require(
            isinstance(target_stride_stream, int)
            and not isinstance(target_stride_stream, bool)
            and 0 <= target_stride_stream <= 1000,
            f"target_stride_stream is not a per-mille integer: "
            f"{target_stride_stream!r}",
        )
        _require(
            isinstance(stride_prefetches, int)
            and not isinstance(stride_prefetches, bool)
            and stride_prefetches >= 0,
            f"l2_stride_prefetches is invalid: {stride_prefetches!r}",
        )

    if schema >= 7:
        operation_fields = (
            15 if schema >= 39 else 14 if schema >= 21 else 13 if schema >= 20 else
            12 if schema >= 19 else
            11 if schema >= 13 else 10 if schema >= 8 else 9
        )
        target_operations = _csv_counts(
            result, "target_ops", operation_fields
        )
        actual_operations = _csv_counts(
            result, "actual_ops", operation_fields
        )
        _require(
            all(
                weight == 0 or count > 0
                for weight, count in zip(target_operations, actual_operations)
            ),
            "actual_ops has an enabled but uncovered class: "
            f"{actual_operations}",
        )
        target_hypervisor = _csv_counts(
            result, "target_hypervisor_family", 3
        )
        actual_hypervisor = _csv_counts(
            result, "actual_hypervisor_family", 3
        )
        hypervisor_index = 9 if schema >= 8 else 8
        if target_operations[hypervisor_index] != 0:
            _require(
                all(
                    weight == 0 or count > 0
                    for weight, count in zip(
                        target_hypervisor, actual_hypervisor
                    )
                ),
                "actual_hypervisor_family has an enabled but uncovered class: "
                f"{actual_hypervisor}",
            )
            _require(
                sum(actual_hypervisor) == actual_operations[hypervisor_index],
                "hypervisor operation/family coverage is not conserved",
            )

        if schema >= 22:
            target_spvp_user = result.get("target_hypervisor_spvp_user")
            actual_spvp = _csv_counts(result, "actual_hypervisor_spvp", 2)
            actual_cross = _csv_counts(result, "actual_hypervisor_cross", 6)
            _require(
                isinstance(target_spvp_user, int)
                and not isinstance(target_spvp_user, bool)
                and 0 <= target_spvp_user <= 1000,
                "target_hypervisor_spvp_user is not a per-mille integer: "
                f"{target_spvp_user!r}",
            )
            hypervisor_enabled = target_operations[hypervisor_index] != 0
            cross_spvp = [0, 0]
            for family in range(3):
                cross_family = 0
                for spvp in range(2):
                    enabled = (
                        hypervisor_enabled
                        and target_hypervisor[family] != 0
                        and (
                            target_spvp_user != 1000
                            if spvp == 0
                            else target_spvp_user != 0
                        )
                    )
                    count = actual_cross[family * 2 + spvp]
                    _require(
                        (count > 0) == enabled,
                        "actual_hypervisor_cross does not match enabled "
                        f"classes: family={family} spvp={spvp}",
                    )
                    cross_family += count
                    cross_spvp[spvp] += count
                _require(
                    cross_family == actual_hypervisor[family],
                    "hypervisor family/cross coverage is not conserved",
                )
            _require(
                cross_spvp == actual_spvp
                and sum(actual_spvp) == actual_operations[hypervisor_index],
                "hypervisor SPVP/cross coverage is not conserved",
            )

        if schema >= 23:
            target_misaligned = result.get("target_misaligned")
            actual_alignment = _csv_counts(
                result, "actual_hypervisor_alignment", 2
            )
            actual_alignment_cross = _csv_counts(
                result, "actual_hypervisor_alignment_cross", 12
            )
            _require(
                isinstance(target_misaligned, int)
                and not isinstance(target_misaligned, bool)
                and 0 <= target_misaligned <= 1000,
                "target_misaligned is not a per-mille integer: "
                f"{target_misaligned!r}",
            )
            cross_alignment = [0, 0]
            for family in range(3):
                for spvp in range(2):
                    pair_total = 0
                    for alignment in range(2):
                        enabled = (
                            target_operations[hypervisor_index] != 0
                            and target_hypervisor[family] != 0
                            and (
                                target_spvp_user != 1000
                                if spvp == 0
                                else target_spvp_user != 0
                            )
                            and (
                                target_misaligned != 1000
                                if alignment == 0
                                else target_misaligned != 0
                            )
                        )
                        index = family * 4 + spvp * 2 + alignment
                        count = actual_alignment_cross[index]
                        _require(
                            (count > 0) == enabled,
                            "actual_hypervisor_alignment_cross does not "
                            "match enabled classes: "
                            f"family={family} spvp={spvp} "
                            f"alignment={alignment}",
                        )
                        pair_total += count
                        cross_alignment[alignment] += count
                    _require(
                        pair_total == actual_cross[family * 2 + spvp],
                        "hypervisor alignment/cross coverage is not "
                        "conserved",
                    )
            _require(
                cross_alignment == actual_alignment
                and sum(actual_alignment)
                == actual_operations[hypervisor_index],
                "hypervisor alignment coverage is not conserved",
            )

        if schema >= 24:
            target_pbmt = _csv_counts(
                result, "target_hypervisor_pbmt_pair", 5
            )
            actual_pbmt = _csv_counts(
                result, "actual_hypervisor_pbmt_pair", 5
            )
            actual_pbmt_cross = _csv_counts(
                result, "actual_hypervisor_pbmt_cross", 30
            )
            cross_pbmt = [0] * 5
            for family in range(3):
                for spvp in range(2):
                    pair_total = 0
                    for pbmt in range(5):
                        enabled = (
                            target_operations[hypervisor_index] != 0
                            and target_hypervisor[family] != 0
                            and (
                                target_spvp_user != 1000
                                if spvp == 0
                                else target_spvp_user != 0
                            )
                            and target_pbmt[pbmt] != 0
                        )
                        index = family * 10 + spvp * 5 + pbmt
                        count = actual_pbmt_cross[index]
                        _require(
                            (count > 0) == enabled,
                            "actual_hypervisor_pbmt_cross does not match "
                            "enabled classes: "
                            f"family={family} spvp={spvp} pbmt={pbmt}",
                        )
                        pair_total += count
                        cross_pbmt[pbmt] += count
                    _require(
                        pair_total == actual_cross[family * 2 + spvp],
                        "hypervisor PBMT/cross coverage is not conserved",
                    )
            _require(
                cross_pbmt == actual_pbmt
                and sum(actual_pbmt) == actual_operations[hypervisor_index],
                "hypervisor PBMT coverage is not conserved",
            )

        if schema >= 25:
            target_pma_device = result.get("target_hypervisor_pma_device")
            _require(
                type(target_pma_device) is int and 0 <= target_pma_device <= 1000,
                "target_hypervisor_pma_device must be an integer in 0..1000",
            )
            actual_pma_device = _csv_counts(
                result, "actual_hypervisor_pma_device", 2
            )
            actual_pma_device_cross = _csv_counts(
                result, "actual_hypervisor_pma_device_cross", 12
            )
            cross_pma_device = [0, 0]
            for family in range(3):
                for spvp in range(2):
                    pair_total = 0
                    for pma_device in range(2):
                        enabled = (
                            target_operations[hypervisor_index] != 0
                            and target_hypervisor[family] != 0
                            and (
                                target_spvp_user != 1000
                                if spvp == 0
                                else target_spvp_user != 0
                            )
                            and (
                                target_pma_device != 1000
                                if pma_device == 0
                                else target_pma_device != 0
                            )
                        )
                        index = family * 4 + spvp * 2 + pma_device
                        count = actual_pma_device_cross[index]
                        _require(
                            (count > 0) == enabled,
                            "actual_hypervisor_pma_device_cross does not match "
                            "enabled classes: "
                            f"family={family} spvp={spvp} "
                            f"pma_device={pma_device}",
                        )
                        pair_total += count
                        cross_pma_device[pma_device] += count
                    _require(
                        pair_total == actual_cross[family * 2 + spvp],
                        "hypervisor PMA-device/cross coverage is not conserved",
                    )
            _require(
                cross_pma_device == actual_pma_device
                and sum(actual_pma_device)
                == actual_operations[hypervisor_index],
                "hypervisor PMA-device coverage is not conserved",
            )

        if schema >= 26:
            target_pmp_relation = _csv_counts(
                result, "target_hypervisor_pmp_relation", 7
            )
            actual_pmp_relation = _csv_counts(
                result, "actual_hypervisor_pmp_relation", 7
            )
            actual_pmp_relation_cross = _csv_counts(
                result, "actual_hypervisor_pmp_relation_cross", 42
            )
            cross_pmp_relation = [0] * 7
            for family in range(3):
                for spvp in range(2):
                    pair_total = 0
                    for relation in range(7):
                        enabled = (
                            target_operations[hypervisor_index] != 0
                            and target_hypervisor[family] != 0
                            and (
                                target_spvp_user != 1000
                                if spvp == 0
                                else target_spvp_user != 0
                            )
                            and target_pmp_relation[relation] != 0
                        )
                        index = family * 14 + spvp * 7 + relation
                        count = actual_pmp_relation_cross[index]
                        _require(
                            (count > 0) == enabled,
                            "actual_hypervisor_pmp_relation_cross does not "
                            "match enabled classes: "
                            f"family={family} spvp={spvp} relation={relation}",
                        )
                        pair_total += count
                        cross_pmp_relation[relation] += count
                    _require(
                        pair_total == actual_cross[family * 2 + spvp],
                        "hypervisor PMP-relation/cross coverage is not conserved",
                    )
            _require(
                cross_pmp_relation == actual_pmp_relation
                and sum(actual_pmp_relation)
                == actual_operations[hypervisor_index],
                "hypervisor PMP-relation coverage is not conserved",
            )

        if schema >= 13:
            target_cmo_operations = _csv_counts(
                result, "target_cmo_operation", 3
            )
            actual_cmo_operations = _csv_counts(
                result, "actual_cmo_operation", 3
            )
            target_cmo_dirty = result.get("target_cmo_dirty")
            target_cmo_overlap = result.get("target_cmo_younger_overlap")
            actual_cmo_states = _csv_counts(
                result, "actual_cmo_line_state", 2
            )
            actual_cmo_overlaps = _csv_counts(
                result, "actual_cmo_younger_overlap", 2
            )
            if schema >= 36:
                target_cmo_probe_depths = _csv_counts(
                    result, "target_cmo_probe_depth", 8
                )
                actual_cmo_probe_depths = _csv_counts(
                    result, "actual_cmo_probe_depth", 8
                )
                actual_cmo_probe_crosses = _csv_counts(
                    result, "actual_cmo_probe_cross", 48
                )
            for name, value in (
                ("target_cmo_dirty", target_cmo_dirty),
                ("target_cmo_younger_overlap", target_cmo_overlap),
            ):
                _require(
                    isinstance(value, int) and not isinstance(value, bool)
                    and 0 <= value <= 1000,
                    f"{name} is not a per-mille integer: {value!r}",
                )
            cmo_count = actual_operations[10]
            if target_operations[10] != 0:
                _require(
                    all(
                        (weight == 0 and count == 0)
                        or (weight != 0 and count > 0)
                        for weight, count in zip(
                            target_cmo_operations, actual_cmo_operations
                        )
                    ),
                    "actual_cmo_operation has an enabled but uncovered class: "
                    f"{actual_cmo_operations}",
                )
                for name, target, actual in (
                    ("actual_cmo_line_state", target_cmo_dirty, actual_cmo_states),
                    (
                        "actual_cmo_younger_overlap",
                        target_cmo_overlap,
                        actual_cmo_overlaps,
                    ),
                ):
                    _require(
                        (
                            target == 0
                            and actual[0] > 0
                            and actual[1] == 0
                        )
                        or (
                            target == 1000
                            and actual[0] == 0
                            and actual[1] > 0
                        )
                        or (
                            0 < target < 1000
                            and actual[0] > 0
                            and actual[1] > 0
                        ),
                        f"{name} has an enabled but uncovered class: {actual}",
                    )
                _require(
                    sum(actual_cmo_operations) == cmo_count
                    and sum(actual_cmo_states) == cmo_count
                    and sum(actual_cmo_overlaps) == cmo_count,
                    "CMO operation/state/overlap coverage is not conserved",
                )
                if schema >= 15:
                    target_cmo_error = result.get("target_cmo_error")
                    target_cmo_error_denied = result.get(
                        "target_cmo_error_denied"
                    )
                    for name, value in (
                        ("target_cmo_error", target_cmo_error),
                        ("target_cmo_error_denied", target_cmo_error_denied),
                    ):
                        _require(
                            isinstance(value, int)
                            and not isinstance(value, bool)
                            and 0 <= value <= 1000,
                            f"{name} is not a per-mille integer: {value!r}",
                        )
                    actual_cmo_error = _csv_counts(
                        result, "actual_cmo_error", 2
                    )
                    actual_cmo_error_kind = _csv_counts(
                        result, "actual_cmo_error_kind", 2
                    )
                    actual_cmo_operation_error = _csv_counts(
                        result, "actual_cmo_operation_error", 6
                    )
                    _require(
                        (
                            target_cmo_error == 0
                            and actual_cmo_error[0] > 0
                            and actual_cmo_error[1] == 0
                        )
                        or (
                            target_cmo_error == 1000
                            and actual_cmo_error[0] == 0
                            and actual_cmo_error[1] > 0
                        )
                        or (
                            0 < target_cmo_error < 1000
                            and actual_cmo_error[0] > 0
                            and actual_cmo_error[1] > 0
                        ),
                        "actual_cmo_error has an enabled but uncovered class: "
                        f"{actual_cmo_error}",
                    )
                    _require(
                        sum(actual_cmo_error) == cmo_count,
                        "CMO error presence is not conserved",
                    )
                    if target_cmo_error == 0:
                        _require(
                            actual_cmo_error_kind == [0, 0]
                            and actual_cmo_operation_error == [0] * 6,
                            "disabled CMO errors have coverage observations",
                        )
                    else:
                        _require(
                            (
                                target_cmo_error_denied == 0
                                and actual_cmo_error_kind[0] > 0
                                and actual_cmo_error_kind[1] == 0
                            )
                            or (
                                target_cmo_error_denied == 1000
                                and actual_cmo_error_kind[0] == 0
                                and actual_cmo_error_kind[1] > 0
                            )
                            or (
                                0 < target_cmo_error_denied < 1000
                                and actual_cmo_error_kind[0] > 0
                                and actual_cmo_error_kind[1] > 0
                            ),
                            "actual_cmo_error_kind has an enabled but uncovered "
                            f"class: {actual_cmo_error_kind}",
                        )
                        _require(
                            sum(actual_cmo_error_kind) == actual_cmo_error[1]
                            and sum(actual_cmo_operation_error) ==
                                actual_cmo_error[1],
                            "CMO error kind/cross coverage is not conserved",
                        )
                        for operation in range(3):
                            corrupt, denied = actual_cmo_operation_error[
                                operation * 2 : operation * 2 + 2
                            ]
                            _require(
                                (target_cmo_operations[operation] != 0)
                                == (corrupt + denied > 0),
                                "CMO error cross does not match enabled "
                                f"operation: {actual_cmo_operation_error}",
                            )
                            _require(
                                corrupt + denied <=
                                actual_cmo_operations[operation],
                                "CMO error cross exceeds operation count",
                            )
                            _require(
                                (target_cmo_error_denied == 1000 or corrupt > 0)
                                and (target_cmo_error_denied == 0 or denied > 0),
                                "CMO operation/error cross has an uncovered "
                                f"class: {actual_cmo_operation_error}",
                            )
                    if schema >= 36:
                        _require(
                            target_cmo_error == 1000
                            or any(target_cmo_probe_depths),
                            "target_cmo_probe_depth cannot be all zero when a "
                            "successful CMO is enabled",
                        )
                        crossed_operations = [0, 0, 0]
                        crossed_states = [0, 0]
                        crossed_depths = [0] * 8
                        for operation in range(3):
                            operation_enabled = (
                                target_cmo_operations[operation] != 0
                            )
                            for state in range(2):
                                state_enabled = (
                                    target_cmo_dirty != 1000
                                    if state == 0
                                    else target_cmo_dirty != 0
                                )
                                for depth in range(8):
                                    index = operation * 16 + state * 8 + depth
                                    count = actual_cmo_probe_crosses[index]
                                    enabled = (
                                        target_cmo_error != 1000
                                        and operation_enabled
                                        and state_enabled
                                        and target_cmo_probe_depths[depth] != 0
                                    )
                                    _require(
                                        (count > 0) == enabled,
                                        "actual_cmo_probe_cross has an enabled "
                                        "but uncovered class: "
                                        f"operation={operation} state={state} "
                                        f"depth={depth + 1}",
                                    )
                                    crossed_operations[operation] += count
                                    crossed_states[state] += count
                                    crossed_depths[depth] += count
                        operation_errors = [
                            sum(actual_cmo_operation_error[index:index + 2])
                            for index in range(0, 6, 2)
                        ]
                        _require(
                            all(
                                crossed_operations[operation]
                                + operation_errors[operation]
                                == actual_cmo_operations[operation]
                                for operation in range(3)
                            ),
                            "CMO Probe cross/error counts do not match operation "
                            "marginals",
                        )
                        _require(
                            crossed_depths == actual_cmo_probe_depths,
                            "CMO Probe cross does not match depth marginals",
                        )
                        _require(
                            all(
                                crossed_states[state] <= actual_cmo_states[state]
                                for state in range(2)
                            )
                            and sum(
                                actual_cmo_states[state] - crossed_states[state]
                                for state in range(2)
                            ) == actual_cmo_error[1],
                            "CMO Probe cross/error counts do not match line-state "
                            "marginals",
                        )
                        _require(
                            sum(actual_cmo_probe_depths) == actual_cmo_error[0],
                            "CMO Probe depth/success coverage is not conserved",
                        )
                        highest_cmo_probe_depth = max(
                            (
                                depth + 1
                                for depth, count in enumerate(
                                    actual_cmo_probe_depths
                                )
                                if count > 0
                            ),
                            default=0,
                        )
                        _require(
                            probe_max_outstanding >= highest_cmo_probe_depth,
                            "CMO Probe burst never reached its selected "
                            "outstanding depth",
                        )
            else:
                _require(
                    actual_cmo_operations == [0, 0, 0]
                    and actual_cmo_states == [0, 0]
                    and actual_cmo_overlaps == [0, 0],
                    "disabled CMO operation has coverage observations",
                )
                if schema >= 15:
                    _require(
                        _csv_counts(result, "actual_cmo_error", 2) == [0, 0]
                        and _csv_counts(
                            result, "actual_cmo_error_kind", 2
                        ) == [0, 0]
                        and _csv_counts(
                            result, "actual_cmo_operation_error", 6
                        ) == [0] * 6,
                        "disabled CMO operation has error observations",
                    )
                if schema >= 36:
                    _require(
                        actual_cmo_probe_depths == [0] * 8
                        and actual_cmo_probe_crosses == [0] * 48,
                        "disabled CMO operation has Probe-depth observations",
                    )

        if schema >= 16:
            target_uncache_error = result.get("target_uncache_error")
            target_uncache_load_denied = result.get(
                "target_uncache_load_error_denied"
            )
            target_store_shares = [
                result.get("target_nc_store"),
                result.get("target_mmio_store"),
            ]
            for name, value in (
                ("target_uncache_error", target_uncache_error),
                (
                    "target_uncache_load_error_denied",
                    target_uncache_load_denied,
                ),
                ("target_nc_store", target_store_shares[0]),
                ("target_mmio_store", target_store_shares[1]),
            ):
                _require(
                    isinstance(value, int)
                    and not isinstance(value, bool)
                    and 0 <= value <= 1000,
                    f"{name} is not a per-mille integer: {value!r}",
                )
            actual_directions = [
                _csv_counts(result, "actual_nc_direction", 2),
                _csv_counts(result, "actual_mmio_direction", 2),
            ]
            actual_uncache_error = _csv_counts(
                result, "actual_uncache_error", 2
            )
            actual_uncache_error_kind = _csv_counts(
                result, "actual_uncache_error_kind", 2
            )
            actual_uncache_outcome = _csv_counts(
                result, "actual_uncache_outcome", 12
            )
            for memory_type, operation_index in enumerate((7, 8)):
                outcomes = actual_uncache_outcome[
                    memory_type * 6 : memory_type * 6 + 6
                ]
                store_share = target_store_shares[memory_type]
                for direction in range(2):
                    direction_enabled = (
                        target_operations[operation_index] != 0
                        and (
                            store_share != 1000 if direction == 0
                            else store_share != 0
                        )
                    )
                    clean, corrupt, denied = outcomes[
                        direction * 3 : direction * 3 + 3
                    ]
                    expected = (
                        direction_enabled and target_uncache_error != 1000,
                        direction_enabled
                        and direction == 0
                        and target_uncache_error != 0
                        and target_uncache_load_denied != 1000,
                        direction_enabled
                        and target_uncache_error != 0
                        and (
                            direction == 1
                            or target_uncache_load_denied != 0
                        ),
                    )
                    _require(
                        all(
                            (count > 0) == enabled
                            for count, enabled in zip(
                                (clean, corrupt, denied), expected
                            )
                        ),
                        "actual_uncache_outcome does not match enabled "
                        f"classes for memory_type={memory_type} "
                        f"direction={direction}: {outcomes}",
                    )
                _require(
                    sum(outcomes) == actual_operations[operation_index],
                    "Uncache outcome/operation coverage is not conserved",
                )
                derived_directions = [
                    sum(outcomes[0:3]), sum(outcomes[3:6])
                ]
                _require(
                    derived_directions == actual_directions[memory_type]
                    and all(
                        (count > 0) == (
                            target_operations[operation_index] != 0
                            and (
                                store_share != 1000 if direction == 0
                                else store_share != 0
                            )
                        )
                        for direction, count in enumerate(
                            actual_directions[memory_type]
                        )
                    ),
                    "Uncache direction coverage does not match outcomes or "
                    "enabled classes",
                )
            clean = sum(actual_uncache_outcome[index] for index in (0, 3, 6, 9))
            corrupt = sum(actual_uncache_outcome[index] for index in (1, 7))
            denied = sum(actual_uncache_outcome[index] for index in (2, 5, 8, 11))
            _require(
                actual_uncache_error == [clean, corrupt + denied]
                and actual_uncache_error_kind == [corrupt, denied],
                "Uncache aggregate error coverage does not match outcomes",
            )
            _require(
                sum(actual_uncache_error) ==
                    actual_operations[7] + actual_operations[8]
                and sum(actual_uncache_error_kind) == actual_uncache_error[1],
                "Uncache error coverage is not conserved",
            )

        if schema >= 17:
            target_dcache_load_error = result.get("target_dcache_load_error")
            target_dcache_load_denied = result.get(
                "target_dcache_load_error_denied"
            )
            for name, value in (
                ("target_dcache_load_error", target_dcache_load_error),
                (
                    "target_dcache_load_error_denied",
                    target_dcache_load_denied,
                ),
            ):
                _require(
                    isinstance(value, int)
                    and not isinstance(value, bool)
                    and 0 <= value <= 1000,
                    f"{name} is not a per-mille integer: {value!r}",
                )
            actual_dcache_load_error = _csv_counts(
                result, "actual_dcache_load_error", 2
            )
            actual_dcache_load_error_kind = _csv_counts(
                result, "actual_dcache_load_error_kind", 2
            )
            actual_dcache_load_outcome = _csv_counts(
                result, "actual_dcache_load_outcome", 3
            )
            actual_dcache_load_manager = _csv_counts(
                result, "actual_dcache_load_error_manager", 5
            )
            scalar_load_enabled = target_operations[0] != 0
            expected_outcomes = (
                scalar_load_enabled and target_dcache_load_error != 1000,
                scalar_load_enabled
                and target_dcache_load_error != 0
                and target_dcache_load_denied != 1000,
                scalar_load_enabled
                and target_dcache_load_error != 0
                and target_dcache_load_denied != 0,
            )
            _require(
                all(
                    (count > 0) == enabled
                    for count, enabled in zip(
                        actual_dcache_load_outcome, expected_outcomes
                    )
                ),
                "actual_dcache_load_outcome does not match enabled classes: "
                f"{actual_dcache_load_outcome}",
            )
            clean, corrupt, denied = actual_dcache_load_outcome
            errors = corrupt + denied
            _require(
                actual_dcache_load_error == [clean, errors]
                and actual_dcache_load_error_kind == [corrupt, denied],
                "DCache load aggregate error coverage does not match outcomes",
            )
            _require(
                clean + errors == actual_operations[0]
                and sum(actual_dcache_load_error_kind) == errors,
                "DCache load error coverage is not conserved",
            )
            _require(
                actual_dcache_load_manager
                == [errors, denied * 2, errors * 2, errors, errors],
                "DCache load error manager accounting is not conserved",
            )

        if schema >= 40:
            target_bank_conflict = result.get("target_bank_conflict")
            _require(
                isinstance(target_bank_conflict, int)
                and not isinstance(target_bank_conflict, bool)
                and 0 <= target_bank_conflict <= 1000,
                "target_bank_conflict is not a per-mille integer: "
                f"{target_bank_conflict!r}",
            )
            actual_bank_depth = _csv_counts(
                result, "actual_bank_conflict_depth", 2
            )
            actual_bank = _csv_counts(
                result, "actual_bank_conflict_bank", 8
            )
            actual_bank_translation = _csv_counts(
                result, "actual_bank_conflict_translation", 3
            )
            actual_bank_cross = _csv_counts(
                result, "actual_bank_conflict_cross", 2 * 8 * 3
            )
            bank_enabled = (
                target_operations[0] != 0 and target_bank_conflict != 0
            )
            crossed_depth = [0, 0]
            crossed_bank = [0] * 8
            crossed_translation = [0] * 3
            cross_total = 0
            derived_loads = 0
            for depth in range(2):
                for bank in range(8):
                    for regime in range(3):
                        index = depth * 24 + bank * 3 + regime
                        count = actual_bank_cross[index]
                        enabled = bank_enabled and target_translation[regime] != 0
                        _require(
                            (count > 0) == enabled,
                            "actual_bank_conflict_cross does not match enabled "
                            f"classes: depth={depth} bank={bank} "
                            f"regime={regime}",
                        )
                        cross_total += count
                        derived_loads += count * (depth + 2)
                        crossed_depth[depth] += count
                        crossed_bank[bank] += count
                        crossed_translation[regime] += count
            _require(
                crossed_depth == actual_bank_depth
                and crossed_bank == actual_bank
                and crossed_translation == actual_bank_translation,
                "bank-conflict cross/marginal coverage is not conserved",
            )
            if not bank_enabled:
                _require(
                    cross_total == 0,
                    "disabled bank-conflict has coverage observations",
                )
            terminal = _csv_counts(
                result, "actual_bank_conflict_terminal", 4
            )
            _require(
                terminal == [
                    cross_total, derived_loads, derived_loads, derived_loads
                ],
                "bank-conflict architectural terminal accounting is not "
                "conserved",
            )

        if schema >= 18:
            target_atomic_families = _csv_counts(
                result, "target_atomic_family", 3
            )
            target_atomic_widths = _csv_counts(
                result, "target_atomic_width", 2
            )
            target_atomic_error = result.get("target_atomic_error")
            target_atomic_denied = result.get("target_atomic_error_denied")
            for name, value in (
                ("target_atomic_error", target_atomic_error),
                ("target_atomic_error_denied", target_atomic_denied),
            ):
                _require(
                    isinstance(value, int)
                    and not isinstance(value, bool)
                    and 0 <= value <= 1000,
                    f"{name} is not a per-mille integer: {value!r}",
                )
            actual_atomic_families = _csv_counts(
                result, "actual_atomic_family", 3
            )
            actual_atomic_widths = _csv_counts(
                result, "actual_atomic_width", 2
            )
            actual_atomic_error = _csv_counts(
                result, "actual_atomic_error", 2
            )
            actual_atomic_error_kind = _csv_counts(
                result, "actual_atomic_error_kind", 2
            )
            actual_atomic_outcomes = _csv_counts(
                result, "actual_atomic_outcome", 18
            )
            actual_atomic_manager = _csv_counts(
                result, "actual_atomic_error_manager", 5
            )
            atomic_enabled = target_operations[6] != 0
            atomic_outcome_enabled = (
                target_atomic_error != 1000,
                target_atomic_error != 0 and target_atomic_denied != 1000,
                target_atomic_error != 0 and target_atomic_denied != 0,
            )
            derived_families = [0, 0, 0]
            derived_widths = [0, 0]
            outcome_totals = [0, 0, 0]
            for family in range(3):
                for width in range(2):
                    for outcome in range(3):
                        enabled = (
                            atomic_enabled
                            and target_atomic_families[family] != 0
                            and target_atomic_widths[width] != 0
                            and atomic_outcome_enabled[outcome]
                        )
                        index = family * 6 + width * 3 + outcome
                        count = actual_atomic_outcomes[index]
                        _require(
                            (count > 0) == enabled,
                            "actual_atomic_outcome does not match enabled "
                            f"classes: {actual_atomic_outcomes}",
                        )
                        derived_families[family] += count
                        derived_widths[width] += count
                        outcome_totals[outcome] += count
            clean, corrupt, denied = outcome_totals
            errors = corrupt + denied
            _require(
                actual_atomic_families == derived_families
                and actual_atomic_widths == derived_widths,
                "atomic family/width coverage does not match outcomes",
            )
            _require(
                actual_atomic_error == [clean, errors]
                and actual_atomic_error_kind == [corrupt, denied],
                "atomic aggregate error coverage does not match outcomes",
            )
            _require(
                clean + errors == actual_operations[6]
                and sum(actual_atomic_error_kind) == errors,
                "atomic error coverage is not conserved",
            )
            _require(
                actual_atomic_manager
                == [errors, denied * 2, errors * 2, errors, errors],
                "atomic error manager accounting is not conserved",
            )
            if schema >= 37:
                target_atomic_probe_depths = _csv_counts(
                    result, "target_atomic_probe_depth", 9
                )
                actual_atomic_probe_depths = _csv_counts(
                    result, "actual_atomic_probe_depth", 9
                )
                actual_atomic_probe_crosses = _csv_counts(
                    result, "actual_atomic_probe_cross", 54
                )
                _require(
                    not atomic_enabled
                    or target_atomic_error == 1000
                    or any(target_atomic_probe_depths),
                    "target_atomic_probe_depth cannot be all zero when a "
                    "successful atomic is enabled",
                )
                crossed_families = [0, 0, 0]
                crossed_widths = [0, 0]
                crossed_depths = [0] * 9
                for family in range(3):
                    for width in range(2):
                        pair_successes = 0
                        for depth in range(9):
                            index = family * 18 + width * 9 + depth
                            count = actual_atomic_probe_crosses[index]
                            enabled = (
                                atomic_enabled
                                and target_atomic_error != 1000
                                and target_atomic_families[family] != 0
                                and target_atomic_widths[width] != 0
                                and target_atomic_probe_depths[depth] != 0
                            )
                            _require(
                                (count > 0) == enabled,
                                "actual_atomic_probe_cross has an enabled but "
                                "uncovered class: "
                                f"family={family} width={width} depth={depth}",
                            )
                            pair_successes += count
                            crossed_families[family] += count
                            crossed_widths[width] += count
                            crossed_depths[depth] += count
                        _require(
                            pair_successes
                            == actual_atomic_outcomes[
                                family * 6 + width * 3
                            ],
                            "atomic Probe crosses do not match success outcomes",
                        )
                _require(
                    crossed_depths == actual_atomic_probe_depths
                    and sum(actual_atomic_probe_depths) == clean,
                    "atomic Probe depth/cross coverage is not conserved",
                )
                _require(
                    all(
                        crossed_families[family] <=
                        actual_atomic_families[family]
                        for family in range(3)
                    )
                    and all(
                        crossed_widths[width] <= actual_atomic_widths[width]
                        for width in range(2)
                    ),
                    "atomic Probe crosses exceed family/width marginals",
                )
                highest_atomic_probe_depth = max(
                    (
                        depth
                        for depth, count in enumerate(
                            actual_atomic_probe_depths
                        )
                        if count > 0
                    ),
                    default=0,
                )
                _require(
                    probe_max_outstanding >= highest_atomic_probe_depth,
                    "atomic Probe burst never reached its selected "
                    "outstanding depth",
                )

        if schema >= 19:
            target_ptw_sites = _csv_counts(
                result, "target_ptw_error_site", 5
            )
            target_ptw_levels = _csv_counts(
                result, "target_ptw_error_level", 3
            )
            target_ptw_store = result.get("target_ptw_error_store")
            target_ptw_denied = result.get("target_ptw_error_denied")
            target_ptw_corrupt_first = result.get(
                "target_ptw_error_corrupt_first"
            )
            for name, value in (
                ("target_ptw_error_store", target_ptw_store),
                ("target_ptw_error_denied", target_ptw_denied),
                (
                    "target_ptw_error_corrupt_first",
                    target_ptw_corrupt_first,
                ),
            ):
                _require(
                    isinstance(value, int)
                    and not isinstance(value, bool)
                    and 0 <= value <= 1000,
                    f"{name} is not a per-mille integer: {value!r}",
                )
            actual_ptw_outcomes = _csv_counts(
                result, "actual_ptw_error_outcome", 90
            )
            actual_ptw_modes = _csv_counts(
                result, "actual_ptw_error_mode", 20
            )
            actual_ptw_target_levels = _csv_counts(
                result, "actual_ptw_error_target_level", 20
            )
            actual_ptw_manager = _csv_counts(
                result, "actual_ptw_error_manager", 3
            )
            ptw_enabled = target_operations[11] != 0
            outcome_enabled = (
                target_ptw_denied != 0,
                target_ptw_denied != 1000
                and target_ptw_corrupt_first != 0,
                target_ptw_denied != 1000
                and target_ptw_corrupt_first != 1000,
            )
            direction_enabled = (
                target_ptw_store != 1000,
                target_ptw_store != 0,
            )
            site_totals = [0] * 5
            denied_actions = 0
            corrupt_actions = 0
            for site in range(5):
                for direction in range(2):
                    for level_class in range(3):
                        for outcome in range(3):
                            index = (
                                site * 18
                                + direction * 9
                                + level_class * 3
                                + outcome
                            )
                            count = actual_ptw_outcomes[index]
                            enabled = (
                                ptw_enabled
                                and target_ptw_sites[site] != 0
                                and direction_enabled[direction]
                                and target_ptw_levels[level_class] != 0
                                and outcome_enabled[outcome]
                            )
                            _require(
                                (count > 0) == enabled,
                                "actual_ptw_error_outcome does not match "
                                f"enabled classes: site={site} "
                                f"direction={direction} level={level_class} "
                                f"outcome={outcome}",
                            )
                            site_totals[site] += count
                            if outcome == 0:
                                denied_actions += count
                            else:
                                corrupt_actions += count

            for site in range(5):
                mode_counts = actual_ptw_modes[site * 4 : site * 4 + 4]
                if site == 0:
                    expected_modes = [
                        ptw_enabled
                        and target_ptw_sites[site] != 0
                        and target_stage1[mode] != 0
                        if mode < 2
                        else False
                        for mode in range(4)
                    ]
                elif site == 1:
                    expected_modes = [
                        ptw_enabled
                        and target_ptw_sites[site] != 0
                        and target_g[mode] != 0
                        if mode < 2
                        else False
                        for mode in range(4)
                    ]
                else:
                    expected_modes = [
                        ptw_enabled
                        and target_ptw_sites[site] != 0
                        and target_vs[mode // 2] != 0
                        and target_g[mode % 2] != 0
                        for mode in range(4)
                    ]
                _require(
                    all(
                        (count > 0) == enabled
                        for count, enabled in zip(mode_counts, expected_modes)
                    ),
                    "actual_ptw_error_mode does not match enabled classes: "
                    f"site={site} counts={mode_counts}",
                )
                _require(
                    sum(mode_counts) == site_totals[site],
                    "PTW error mode/site coverage is not conserved",
                )

                governing_modes = (
                    target_stage1
                    if site == 0
                    else target_vs
                    if site == 3
                    else target_g
                )
                level_counts = actual_ptw_target_levels[
                    site * 4 : site * 4 + 4
                ]
                expected_levels = []
                for target_level in range(4):
                    enabled = False
                    for mode, mode_weight in enumerate(governing_modes):
                        levels = 3 if mode == 0 else 4
                        enabled = enabled or (
                            ptw_enabled
                            and target_ptw_sites[site] != 0
                            and mode_weight != 0
                            and (
                                (
                                    target_level == 0
                                    and target_ptw_levels[2] != 0
                                )
                                or (
                                    target_level + 1 == levels
                                    and target_ptw_levels[0] != 0
                                )
                                or (
                                    0 < target_level < levels - 1
                                    and target_ptw_levels[1] != 0
                                )
                            )
                        )
                    expected_levels.append(enabled)
                _require(
                    all(
                        (count > 0) == enabled
                        for count, enabled in zip(
                            level_counts, expected_levels
                        )
                    ),
                    "actual_ptw_error_target_level does not match enabled "
                    f"classes: site={site} counts={level_counts}",
                )
                _require(
                    sum(level_counts) == site_totals[site],
                    "PTW error target-level/site coverage is not conserved",
                )

            ptw_actions = sum(site_totals)
            _require(
                ptw_actions == actual_operations[11],
                "PTW error outcome/operation coverage is not conserved",
            )
            _require(
                (
                    actual_ptw_manager == [0, 0, 0]
                    if ptw_actions == 0
                    else (
                        actual_ptw_manager[0] >= ptw_actions
                        and actual_ptw_manager[1] >= denied_actions * 2
                        and actual_ptw_manager[1] % 2 == 0
                        and actual_ptw_manager[1] // 2
                        <= actual_ptw_manager[0]
                        and actual_ptw_manager[0]
                        - actual_ptw_manager[1] // 2
                        >= corrupt_actions
                        and actual_ptw_manager[2]
                        == actual_ptw_manager[0]
                        + actual_ptw_manager[1] // 2
                    )
                ),
                "PTW error manager accounting is not conserved",
            )

        if schema >= 20:
            target_merge_depth = _csv_counts(
                result, "target_load_merge_depth", 2
            )
            target_merge_pattern = _csv_counts(
                result, "target_load_merge_pattern", 3
            )
            actual_merge_shapes = _csv_counts(
                result, "actual_load_merge_shape", 12
            )
            actual_merge_translations = _csv_counts(
                result, "actual_load_merge_translation", 3
            )
            actual_merge_manager = _csv_counts(
                result, "actual_load_merge_manager", 4
            )
            actual_merge_loads = result.get("actual_load_merge_loads")
            _require(
                isinstance(actual_merge_loads, int)
                and not isinstance(actual_merge_loads, bool)
                and actual_merge_loads >= 0,
                "actual_load_merge_loads is not a nonnegative integer: "
                f"{actual_merge_loads!r}",
            )
            merge_enabled = target_operations[12] != 0
            shape_total = 0
            derived_loads = 0
            for depth in range(2):
                for pattern in range(3):
                    enabled = (
                        merge_enabled
                        and target_merge_depth[depth] != 0
                        and target_merge_pattern[pattern] != 0
                    )
                    for beat in range(2):
                        count = actual_merge_shapes[
                            depth * 6 + pattern * 2 + beat
                        ]
                        _require(
                            (count > 0) == enabled,
                            "actual_load_merge_shape does not match enabled "
                            f"classes: depth={depth} pattern={pattern} "
                            f"beat={beat}",
                        )
                        shape_total += count
                        derived_loads += count * (depth + 2)
            _require(
                shape_total == actual_operations[12],
                "load-merge shape/operation coverage is not conserved",
            )
            for regime, count in enumerate(actual_merge_translations):
                _require(
                    (count > 0) ==
                    (merge_enabled and target_translation[regime] != 0),
                    "actual_load_merge_translation does not match enabled "
                    f"classes: {actual_merge_translations}",
                )
            _require(
                sum(actual_merge_translations) == actual_operations[12],
                "load-merge translation/operation coverage is not conserved",
            )
            merge_actions = actual_operations[12]
            _require(
                actual_merge_loads == derived_loads
                and actual_merge_manager[0] >= merge_actions
                and actual_merge_manager[1] >= actual_merge_manager[0]
                and actual_merge_manager[2] == actual_merge_manager[1]
                and actual_merge_manager[3] == actual_merge_loads,
                "load-merge manager accounting is not conserved",
            )

        if schema >= 21:
            target_pressure_depth = _csv_counts(
                result, "target_set_pressure_depth", 2
            )
            target_pressure_width = _csv_counts(
                result, "target_set_pressure_width", 4
            )
            target_pressure_set = _csv_counts(
                result, "target_set_pressure_set", 4
            )
            if schema >= 27:
                target_pressure_dirty = result.get("target_set_pressure_dirty")
                _require(
                    isinstance(target_pressure_dirty, int)
                    and not isinstance(target_pressure_dirty, bool)
                    and 0 <= target_pressure_dirty <= 1000,
                    "target_set_pressure_dirty is not a per-mille integer: "
                    f"{target_pressure_dirty!r}",
                )
                actual_pressure_state = _csv_counts(
                    result, "actual_set_pressure_line_state", 2
                )
                if schema >= 28:
                    target_pressure_overlap = result.get(
                        "target_set_pressure_refill_overlap"
                    )
                    _require(
                        isinstance(target_pressure_overlap, int)
                        and not isinstance(target_pressure_overlap, bool)
                        and 0 <= target_pressure_overlap <= 1000,
                        "target_set_pressure_refill_overlap is not a per-mille "
                        f"integer: {target_pressure_overlap!r}",
                    )
                    actual_pressure_overlap = _csv_counts(
                        result, "actual_set_pressure_refill_overlap", 2
                    )
                    actual_pressure_overlap_manager = _csv_counts(
                        result, "actual_set_pressure_overlap_manager", 5
                    )
                    if schema >= 29:
                        target_pressure_backpressure = result.get(
                            "target_set_pressure_release_backpressure"
                        )
                        _require(
                            isinstance(target_pressure_backpressure, int)
                            and not isinstance(
                                target_pressure_backpressure, bool
                            )
                            and 0 <= target_pressure_backpressure <= 1000,
                            "target_set_pressure_release_backpressure is not "
                            "a per-mille integer: "
                            f"{target_pressure_backpressure!r}",
                        )
                        actual_pressure_backpressure = _csv_counts(
                            result,
                            "actual_set_pressure_release_backpressure",
                            2,
                        )
                        actual_pressure_backpressure_manager = _csv_counts(
                            result,
                            "actual_set_pressure_backpressure_manager",
                            5,
                        )
                        if schema >= 38:
                            target_pressure_windows = _csv_counts(
                                result, "target_set_pressure_window", 8
                            )
                            actual_pressure_dual = _csv_counts(
                                result,
                                "actual_set_pressure_dual_window",
                                2,
                            )
                            actual_pressure_windows = _csv_counts(
                                result,
                                "actual_set_pressure_window_count",
                                8,
                            )
                            target_pressure_dual = 0
                            target_pressure_triple = 0
                            target_pressure_quad = 0
                        elif schema >= 30:
                            target_pressure_dual = result.get(
                                "target_set_pressure_dual_window"
                            )
                            _require(
                                isinstance(target_pressure_dual, int)
                                and not isinstance(target_pressure_dual, bool)
                                and 0 <= target_pressure_dual <= 1000,
                                "target_set_pressure_dual_window is not a "
                                "per-mille integer: "
                                f"{target_pressure_dual!r}",
                            )
                            actual_pressure_dual = _csv_counts(
                                result,
                                "actual_set_pressure_dual_window",
                                2,
                            )
                            if schema >= 31:
                                target_pressure_triple = result.get(
                                    "target_set_pressure_triple_window"
                                )
                                _require(
                                    isinstance(target_pressure_triple, int)
                                    and not isinstance(
                                        target_pressure_triple, bool
                                    )
                                    and 0 <= target_pressure_triple <= 1000,
                                    "target_set_pressure_triple_window is not "
                                    "a per-mille integer: "
                                    f"{target_pressure_triple!r}",
                                )
                                if schema >= 35:
                                    target_pressure_quad = result.get(
                                        "target_set_pressure_quad_window"
                                    )
                                    _require(
                                        isinstance(target_pressure_quad, int)
                                        and not isinstance(
                                            target_pressure_quad, bool
                                        )
                                        and 0 <= target_pressure_quad <= 1000,
                                        "target_set_pressure_quad_window is "
                                        "not a per-mille integer: "
                                        f"{target_pressure_quad!r}",
                                    )
                                    actual_pressure_windows = _csv_counts(
                                        result,
                                        "actual_set_pressure_window_count",
                                        4,
                                    )
                                else:
                                    target_pressure_quad = 0
                                    actual_pressure_windows = _csv_counts(
                                        result,
                                        "actual_set_pressure_window_count",
                                        3,
                                    ) + [0]
                            else:
                                target_pressure_triple = 0
                                target_pressure_quad = 0
                                actual_pressure_windows = [
                                    actual_pressure_dual[0],
                                    actual_pressure_dual[1],
                                    0,
                                    0,
                                ]
                        else:
                            target_pressure_dual = 0
                            actual_pressure_dual = [
                                actual_operations[13],
                                0,
                            ]
                            target_pressure_triple = 0
                            target_pressure_quad = 0
                            actual_pressure_windows = [
                                actual_operations[13], 0, 0, 0
                            ]
                    else:
                        target_pressure_backpressure = 0
                        actual_pressure_backpressure = [
                            actual_operations[13],
                            0,
                        ]
                        actual_pressure_backpressure_manager = [0] * 5
                        target_pressure_dual = 0
                        actual_pressure_dual = [actual_operations[13], 0]
                        target_pressure_triple = 0
                        target_pressure_quad = 0
                        actual_pressure_windows = [
                            actual_operations[13], 0, 0, 0
                        ]
                else:
                    target_pressure_overlap = 0
                    actual_pressure_overlap = [actual_operations[13], 0]
                    actual_pressure_overlap_manager = [0] * 5
                    target_pressure_backpressure = 0
                    actual_pressure_backpressure = [actual_operations[13], 0]
                    actual_pressure_backpressure_manager = [0] * 5
                    target_pressure_dual = 0
                    actual_pressure_dual = [actual_operations[13], 0]
                    target_pressure_triple = 0
                    target_pressure_quad = 0
                    actual_pressure_windows = [
                        actual_operations[13], 0, 0, 0
                    ]
                actual_pressure_cross = _csv_counts(
                    result,
                    "actual_set_pressure_cross",
                    1536 if schema >= 38
                    else 768 if schema >= 35
                    else 576 if schema >= 31
                    else 384 if schema >= 30
                    else 192 if schema >= 29
                    else 96 if schema >= 28
                    else 48,
                )
                actual_pressure_clean_manager = _csv_counts(
                    result, "actual_set_pressure_clean_manager", 12
                )
            else:
                target_pressure_dirty = 1000
                target_pressure_overlap = 0
                actual_pressure_state = [0, actual_operations[13]]
                actual_pressure_overlap = [actual_operations[13], 0]
                actual_pressure_overlap_manager = [0] * 5
                target_pressure_backpressure = 0
                actual_pressure_backpressure = [actual_operations[13], 0]
                actual_pressure_backpressure_manager = [0] * 5
                target_pressure_dual = 0
                actual_pressure_dual = [actual_operations[13], 0]
                target_pressure_triple = 0
                target_pressure_quad = 0
                actual_pressure_windows = [
                    actual_operations[13], 0, 0, 0
                ]
                actual_pressure_cross = _csv_counts(
                    result, "actual_set_pressure_cross", 24
                )
                actual_pressure_clean_manager = [0] * 12
            actual_pressure_set = _csv_counts(
                result, "actual_set_pressure_set", 4
            )
            actual_pressure_issue = _csv_counts(
                result, "actual_set_pressure_issue_order", 2
            )
            actual_pressure_manager = _csv_counts(
                result, "actual_set_pressure_manager", 9
            )
            pressure_enabled = target_operations[13] != 0
            pressure_actions = 0
            pressure_stores = 0
            pressure_min_releases = 0
            pressure_clean_loads = 0
            pressure_min_revisits = 0
            pressure_overlap_actions = 0
            pressure_overlap_windows = 0
            pressure_clean_overlap_windows = 0
            pressure_overlap_min_releases = 0
            pressure_backpressure_actions = 0
            pressure_window_actions = [0] * len(actual_pressure_windows)
            if schema >= 38:
                window_enabled_classes = [
                    weight != 0 for weight in target_pressure_windows
                ]
            else:
                window_enabled_classes = [
                    target_pressure_quad != 1000
                    and target_pressure_triple != 1000
                    and target_pressure_dual != 1000,
                    target_pressure_quad != 1000
                    and target_pressure_triple != 1000
                    and target_pressure_dual != 0,
                    target_pressure_quad != 1000
                    and target_pressure_triple != 0,
                    target_pressure_quad != 0,
                ]
            for state in range(2):
                state_enabled = (
                    target_pressure_dirty != 1000
                    if state == 0
                    else target_pressure_dirty != 0
                )
                for overlap in range(2):
                    overlap_enabled = (
                        target_pressure_overlap != 1000
                        if overlap == 0
                        else target_pressure_overlap != 0
                    )
                    for backpressure in range(2):
                        backpressure_enabled = (
                            target_pressure_backpressure != 1000
                            if backpressure == 0
                            else target_pressure_backpressure != 0
                        )
                        for window_class in range(len(actual_pressure_windows)):
                            window_enabled = window_enabled_classes[window_class]
                            windows = window_class + 1
                            for depth in range(2):
                                for width in range(4):
                                    for regime in range(3):
                                        if schema >= 38:
                                            index = (
                                                state * 768
                                                + overlap * 384
                                                + backpressure * 192
                                                + window_class * 24
                                                + depth * 12
                                                + width * 3
                                                + regime
                                            )
                                            count = actual_pressure_cross[index]
                                        elif schema >= 35:
                                            index = (
                                                state * 384
                                                + overlap * 192
                                                + backpressure * 96
                                                + window_class * 24
                                                + depth * 12
                                                + width * 3
                                                + regime
                                            )
                                            count = actual_pressure_cross[index]
                                        elif schema >= 31 and window_class < 3:
                                            index = (
                                                state * 288
                                                + overlap * 144
                                                + backpressure * 72
                                                + window_class * 24
                                                + depth * 12
                                                + width * 3
                                                + regime
                                            )
                                            count = actual_pressure_cross[index]
                                        elif schema >= 30 and window_class < 2:
                                            index = (
                                                state * 192
                                                + overlap * 96
                                                + backpressure * 48
                                                + window_class * 24
                                                + depth * 12
                                                + width * 3
                                                + regime
                                            )
                                            count = actual_pressure_cross[index]
                                        elif window_class != 0:
                                            count = 0
                                        elif schema >= 29:
                                            index = (
                                                state * 96
                                                + overlap * 48
                                                + backpressure * 24
                                                + depth * 12
                                                + width * 3
                                                + regime
                                            )
                                            count = actual_pressure_cross[index]
                                        elif backpressure != 0:
                                            count = 0
                                        elif schema >= 28:
                                            index = (
                                                state * 48
                                                + overlap * 24
                                                + depth * 12
                                                + width * 3
                                                + regime
                                            )
                                            count = actual_pressure_cross[index]
                                        elif overlap != 0:
                                            count = 0
                                        elif schema >= 27:
                                            count = actual_pressure_cross[
                                                state * 24
                                                + depth * 12
                                                + width * 3
                                                + regime
                                            ]
                                        elif state == 0:
                                            count = 0
                                        else:
                                            count = actual_pressure_cross[
                                                depth * 12 + width * 3 + regime
                                            ]
                                        enabled = (
                                            pressure_enabled
                                            and state_enabled
                                            and overlap_enabled
                                            and backpressure_enabled
                                            and window_enabled
                                            and target_pressure_depth[depth] != 0
                                            and target_pressure_width[width] != 0
                                            and target_translation[regime] != 0
                                        )
                                        _require(
                                            (count > 0) == enabled,
                                            "actual_set_pressure_cross does "
                                            "not match enabled classes: "
                                            f"state={state} overlap={overlap} "
                                            f"backpressure={backpressure} "
                                            f"window_class={window_class} "
                                            f"depth={depth} "
                                            f"width={width} regime={regime}",
                                        )
                                        pressure_actions += count
                                        pressure_window_actions[
                                            window_class
                                        ] += count
                                        if state == 0:
                                            pressure_clean_loads += (
                                                count * windows * (depth + 9)
                                            )
                                            pressure_min_revisits += (
                                                count * windows * (depth + 1)
                                            )
                                        else:
                                            pressure_stores += (
                                                count * windows * (depth + 9)
                                            )
                                            pressure_min_releases += (
                                                count * windows * (depth + 1)
                                            )
                                        if overlap != 0:
                                            pressure_overlap_actions += count
                                            pressure_overlap_windows += (
                                                count * windows
                                            )
                                            pressure_overlap_min_releases += (
                                                count * windows * (depth + 1)
                                            )
                                            if state == 0:
                                                pressure_clean_overlap_windows += (
                                                    count * windows
                                                )
                                        if backpressure != 0:
                                            pressure_backpressure_actions += count
            _require(
                pressure_actions == actual_operations[13]
                and sum(actual_pressure_state) == pressure_actions,
                "set-pressure cross/operation coverage is not conserved",
            )
            for state, count in enumerate(actual_pressure_state):
                state_enabled = (
                    target_pressure_dirty != 1000
                    if state == 0
                    else target_pressure_dirty != 0
                )
                _require(
                    (count > 0) == (pressure_enabled and state_enabled),
                    "actual_set_pressure_line_state does not match enabled "
                    f"classes: {actual_pressure_state}",
                )
            for overlap, count in enumerate(actual_pressure_overlap):
                overlap_enabled = (
                    target_pressure_overlap != 1000
                    if overlap == 0
                    else target_pressure_overlap != 0
                )
                _require(
                    (count > 0) == (pressure_enabled and overlap_enabled),
                    "actual_set_pressure_refill_overlap does not match enabled "
                    f"classes: {actual_pressure_overlap}",
                )
            _require(
                actual_pressure_overlap[1] == pressure_overlap_actions
                and actual_pressure_overlap[0]
                == pressure_actions - pressure_overlap_actions,
                "set-pressure overlap/operation coverage is not conserved",
            )
            for backpressure, count in enumerate(
                actual_pressure_backpressure
            ):
                backpressure_enabled = (
                    target_pressure_backpressure != 1000
                    if backpressure == 0
                    else target_pressure_backpressure != 0
                )
                _require(
                    (count > 0) ==
                    (pressure_enabled and backpressure_enabled),
                    "actual_set_pressure_release_backpressure does not match "
                    f"enabled classes: {actual_pressure_backpressure}",
                )
            _require(
                actual_pressure_backpressure[1]
                == pressure_backpressure_actions
                and actual_pressure_backpressure[0]
                == pressure_actions - pressure_backpressure_actions,
                "set-pressure backpressure/operation coverage is not "
                "conserved",
            )
            for window_class, count in enumerate(actual_pressure_windows):
                _require(
                    (count > 0)
                    == (pressure_enabled and window_enabled_classes[window_class]),
                    "actual_set_pressure_window_count does not match enabled "
                    f"classes: {actual_pressure_windows}",
                )
            _require(
                actual_pressure_windows == pressure_window_actions
                and sum(actual_pressure_windows) == pressure_actions,
                "set-pressure window-count/operation coverage is not "
                "conserved",
            )
            _require(
                actual_pressure_dual
                == [
                    sum(actual_pressure_windows)
                    - actual_pressure_windows[1],
                    actual_pressure_windows[1],
                ],
                "set-pressure dual-window compatibility projection is not "
                "conserved",
            )
            for set_quartile, count in enumerate(actual_pressure_set):
                _require(
                    (count > 0) == (
                        pressure_enabled
                        and target_pressure_set[set_quartile] != 0
                    ),
                    "actual_set_pressure_set does not match enabled classes: "
                    f"{actual_pressure_set}",
                )
            _require(
                sum(actual_pressure_set) == pressure_actions,
                "set-pressure set/operation coverage is not conserved",
            )
            _require(
                sum(actual_pressure_issue) == pressure_stores
                and (
                    (
                        actual_pressure_state[1] == 0
                        and actual_pressure_issue == [0, 0]
                    )
                    or (
                        actual_pressure_state[1] != 0
                        and all(count > 0 for count in actual_pressure_issue)
                    )
                ),
                "set-pressure issue-order coverage is not conserved",
            )
            _require(
                actual_pressure_manager[0] == actual_pressure_state[1]
                and actual_pressure_manager[1] == pressure_stores
                and actual_pressure_manager[2] == pressure_stores
                and actual_pressure_manager[3] >= pressure_min_releases
                and actual_pressure_manager[4] >=
                    actual_pressure_manager[3]
                and actual_pressure_manager[5] ==
                    actual_pressure_manager[4]
                and actual_pressure_manager[6] == pressure_stores
                and actual_pressure_manager[7] == pressure_stores
                and actual_pressure_manager[8] ==
                    actual_pressure_manager[3],
                "set-pressure manager accounting is not conserved",
            )
            _require(
                actual_pressure_clean_manager[0] == actual_pressure_state[0]
                and actual_pressure_clean_manager[1] == pressure_clean_loads
                and actual_pressure_clean_manager[2] == pressure_clean_loads
                and actual_pressure_clean_manager[3] == pressure_clean_loads
                and actual_pressure_clean_manager[4] >= pressure_min_revisits
                and actual_pressure_clean_manager[5] >= pressure_min_revisits
                and actual_pressure_clean_manager[6] == 0
                and actual_pressure_clean_manager[7]
                >= actual_pressure_clean_manager[5]
                and actual_pressure_clean_manager[7]
                >= actual_pressure_clean_manager[8]
                and actual_pressure_clean_manager[9]
                == actual_pressure_clean_manager[8]
                and actual_pressure_clean_manager[10]
                == pressure_clean_loads * 2 + pressure_clean_overlap_windows
                and actual_pressure_clean_manager[11]
                == pressure_clean_loads * 2 + pressure_clean_overlap_windows,
                "clean set-pressure manager accounting is not conserved",
            )
            expected_overlap_manager_items = (
                pressure_overlap_windows
                if schema >= 30
                else pressure_overlap_actions
            )
            _require(
                actual_pressure_overlap_manager[0]
                == expected_overlap_manager_items
                and actual_pressure_overlap_manager[1]
                == expected_overlap_manager_items
                and actual_pressure_overlap_manager[2]
                >= pressure_overlap_min_releases
                and actual_pressure_overlap_manager[3]
                == expected_overlap_manager_items
                and actual_pressure_overlap_manager[4]
                == expected_overlap_manager_items,
                "set-pressure refill-overlap accounting is not conserved",
            )
            if schema >= 29:
                _require(
                    actual_pressure_backpressure_manager[0]
                    == pressure_backpressure_actions
                    and actual_pressure_backpressure_manager[1]
                    == pressure_backpressure_actions
                    and actual_pressure_backpressure_manager[2]
                    == pressure_backpressure_actions * 16
                    and actual_pressure_backpressure_manager[3]
                    == pressure_backpressure_actions * 16
                    and actual_pressure_backpressure_manager[4]
                    == pressure_actions,
                    "set-pressure release-backpressure accounting is not "
                    "conserved",
                )

        if schema >= 39:
            target_miss_depth = _csv_counts(
                result, "target_miss_burst_depth", 15
            )
            target_miss_width = _csv_counts(
                result, "target_miss_burst_issue_width", 3
            )
            actual_miss_depth = _csv_counts(
                result, "actual_miss_burst_depth", 15
            )
            actual_miss_width = _csv_counts(
                result, "actual_miss_burst_issue_width", 3
            )
            actual_miss_translation = _csv_counts(
                result, "actual_miss_burst_translation", 3
            )
            actual_miss_cross = _csv_counts(
                result, "actual_miss_burst_cross", 15 * 3 * 3
            )
            actual_miss_manager = _csv_counts(
                result, "actual_miss_burst_manager", 7
            )
            actual_miss_max = result.get("actual_miss_burst_max_outstanding")
            _require(
                isinstance(actual_miss_max, int)
                and not isinstance(actual_miss_max, bool)
                and actual_miss_max >= 0,
                "actual_miss_burst_max_outstanding is invalid: "
                f"{actual_miss_max!r}",
            )
            miss_enabled = target_operations[14] != 0
            cross_total = 0
            derived_loads = 0
            crossed_depth = [0] * 15
            crossed_width = [0] * 3
            crossed_translation = [0] * 3
            for depth in range(15):
                for width in range(3):
                    for regime in range(3):
                        index = depth * 9 + width * 3 + regime
                        count = actual_miss_cross[index]
                        enabled = (
                            miss_enabled
                            and target_miss_depth[depth] != 0
                            and target_miss_width[width] != 0
                            and width + 1 <= depth + 2
                            and target_translation[regime] != 0
                        )
                        _require(
                            (count > 0) == enabled,
                            "actual_miss_burst_cross does not match enabled "
                            f"classes: depth={depth} width={width} "
                            f"regime={regime}",
                        )
                        cross_total += count
                        derived_loads += count * (depth + 2)
                        crossed_depth[depth] += count
                        crossed_width[width] += count
                        crossed_translation[regime] += count
            _require(
                cross_total == actual_operations[14]
                and crossed_depth == actual_miss_depth
                and crossed_width == actual_miss_width
                and crossed_translation == actual_miss_translation,
                "miss-burst cross/operation coverage is not conserved",
            )
            if not miss_enabled:
                _require(
                    actual_miss_manager == [0] * 7 and actual_miss_max == 0,
                    "disabled miss-burst has manager observations",
                )
            else:
                _require(
                    actual_miss_manager[0] == actual_operations[14]
                    and actual_miss_manager[1] == derived_loads
                    and actual_miss_manager[2] >= derived_loads
                    and actual_miss_manager[3] >= actual_miss_manager[2]
                    and actual_miss_manager[4] == actual_miss_manager[3]
                    and actual_miss_manager[5] == derived_loads
                    and actual_miss_manager[6] == derived_loads
                    and actual_miss_max >= 2,
                    "miss-burst manager accounting is not conserved",
                )

    if schema >= 8:
        target_segment_store = result.get("target_vector_segment_store")
        _require(
            isinstance(target_segment_store, int)
            and not isinstance(target_segment_store, bool)
            and 0 <= target_segment_store <= 1000,
            "target_vector_segment_store is not a per-mille integer: "
            f"{target_segment_store!r}",
        )
        actual_segment_direction = _csv_counts(
            result, "actual_vector_segment_direction", 2
        )
        if target_operations[4] != 0:
            _require(
                (target_segment_store == 1000 or actual_segment_direction[0] > 0)
                and (target_segment_store == 0 or actual_segment_direction[1] > 0),
                "actual_vector_segment_direction has an enabled but uncovered "
                f"class: {actual_segment_direction}",
            )
            _require(
                sum(actual_segment_direction) == actual_operations[4],
                "vector segment operation/direction coverage is not conserved",
            )
        if schema == 8:
            actual_segment_eew = _csv_counts(
                result, "actual_vector_segment_eew", 4
            )
            actual_segment_nf = _csv_counts(
                result, "actual_vector_segment_nf", 7
            )
            if target_operations[4] != 0:
                _require(
                    all(count > 0 for count in actual_segment_eew),
                    "actual_vector_segment_eew has an uncovered class: "
                    f"{actual_segment_eew}",
                )
                _require(
                    all(count > 0 for count in actual_segment_nf),
                    "actual_vector_segment_nf has an uncovered class: "
                    f"{actual_segment_nf}",
                )
                _require(
                    sum(actual_segment_eew) == actual_operations[4]
                    and sum(actual_segment_nf) == actual_operations[4],
                    "vector segment operation/subclass coverage is not conserved",
                )

    if schema >= 9:
        segment_dimensions = (
            ("addressing", 4),
            ("eew", 4),
            ("sew", 4),
            ("lmul", 7),
            ("emul", 7),
            ("nf", 7),
        )
        for dimension, fields in segment_dimensions:
            target = _csv_counts(
                result, f"target_vector_segment_{dimension}", fields
            )
            actual = _csv_counts(
                result, f"actual_vector_segment_{dimension}", fields
            )
            if target_operations[4] != 0:
                _require(
                    all(
                        (weight == 0 and count == 0)
                        or (weight != 0 and count > 0)
                        for weight, count in zip(target, actual)
                    ),
                    f"actual_vector_segment_{dimension} does not match its "
                    f"enabled classes: target={target} actual={actual}",
                )
                _require(
                    sum(actual) == actual_operations[4],
                    "vector segment operation/subclass coverage is not "
                    f"conserved for {dimension}",
                )

    if schema >= 10:
        vector_dimensions = (
            ("addressing", 4),
            ("eew", 4),
            ("sew", 4),
            ("lmul", 7),
            ("emul", 7),
        )
        vector_targets: dict[str, list[int]] = {}
        vector_shape_operations = result.get("actual_vector_shape_ops")
        vector_uops = result.get("actual_vector_uops")
        vector_multi_uop = result.get("actual_vector_multi_uop")
        vector_directions = _csv_counts(
            result, "actual_vector_direction", 2
        )
        for name, value in (
            ("actual_vector_shape_ops", vector_shape_operations),
            ("actual_vector_uops", vector_uops),
            ("actual_vector_multi_uop", vector_multi_uop),
        ):
            _require(
                isinstance(value, int) and not isinstance(value, bool)
                and value >= 0,
                f"{name} is not a nonnegative integer: {value!r}",
            )
        vector_enabled = target_operations[2] != 0 or target_operations[3] != 0
        _require(
            all(
                (weight == 0 and count == 0)
                or (weight != 0 and count > 0)
                for weight, count in zip(
                    target_operations[2:4], vector_directions
                )
            ),
            "actual_vector_direction does not match enabled vector operation "
            f"classes: target={target_operations[2:4]} "
            f"actual={vector_directions}",
        )
        for dimension, fields in vector_dimensions:
            target = _csv_counts(result, f"target_vector_{dimension}", fields)
            actual = _csv_counts(result, f"actual_vector_{dimension}", fields)
            vector_targets[dimension] = target
            if vector_enabled:
                _require(
                    all(
                        (weight == 0 and count == 0)
                        or (weight != 0 and count > 0)
                        for weight, count in zip(target, actual)
                    ),
                    f"actual_vector_{dimension} does not match its enabled "
                    f"classes: target={target} actual={actual}",
                )
                _require(
                    sum(actual) == vector_shape_operations,
                    "ordinary vector shape/subclass coverage is not conserved "
                    f"for {dimension}",
                )
            else:
                _require(
                    all(count == 0 for count in actual),
                    f"actual_vector_{dimension} is nonzero while ordinary "
                    "vector operations are disabled",
                )
        if vector_enabled:
            _require(
                vector_shape_operations > 0
                and vector_shape_operations
                <= actual_operations[2] + actual_operations[3],
                "ordinary vector shape operations do not fit operation coverage",
            )
            _require(
                sum(vector_directions) == vector_shape_operations,
                "ordinary vector operation/direction coverage is not conserved",
            )
            _require(
                vector_shape_operations <= vector_uops
                <= 8 * vector_shape_operations,
                "ordinary vector uop count is outside the architectural 1..8 "
                "uops per instruction range",
            )
            _require(
                0 <= vector_multi_uop <= vector_shape_operations
                and vector_uops >= vector_shape_operations + vector_multi_uop,
                "ordinary vector multi-uop accounting is inconsistent",
            )
            enabled_multi_uop = (
                any(vector_targets["emul"][4:])
                or (
                    any(vector_targets["addressing"][2:])
                    and any(vector_targets["lmul"][4:])
                )
            )
            if enabled_multi_uop:
                _require(
                    vector_multi_uop > 0,
                    "enabled ordinary vector shapes produced no multi-uop "
                    "instruction",
                )
        else:
            _require(
                vector_shape_operations == 0
                and vector_uops == 0
                and vector_multi_uop == 0,
                "ordinary vector counts are nonzero while vector operations "
                "are disabled",
            )

    if schema >= 11:
        vector_enabled = target_operations[2] != 0 or target_operations[3] != 0
        vector_shape_operations = result.get("actual_vector_shape_ops")
        policy_targets: dict[str, int] = {}
        for policy in (
            "masked",
            "vma",
            "vta",
            "partial_vl",
            "nonzero_vstart",
        ):
            target = result.get(f"target_vector_{policy}")
            _require(
                isinstance(target, int)
                and not isinstance(target, bool)
                and 0 <= target <= 1000,
                f"target_vector_{policy} is not a per-mille integer: {target!r}",
            )
            actual = _csv_counts(result, f"actual_vector_{policy}", 2)
            policy_targets[policy] = target
            if vector_enabled:
                _require(
                    (
                        target == 0
                        and actual[0] > 0
                        and actual[1] == 0
                    )
                    or (
                        target == 1000
                        and actual[0] == 0
                        and actual[1] > 0
                    )
                    or (
                        0 < target < 1000
                        and actual[0] > 0
                        and actual[1] > 0
                    ),
                    f"actual_vector_{policy} has an enabled but uncovered "
                    f"class: target={target} actual={actual}",
                )
                _require(
                    sum(actual) == vector_shape_operations,
                    f"ordinary vector policy coverage is not conserved for {policy}",
                )
            else:
                _require(
                    all(count == 0 for count in actual),
                    f"actual_vector_{policy} is nonzero while ordinary vector "
                    "operations are disabled",
                )

        agnostic = _csv_counts(result, "actual_vector_agnostic", 2)
        load_enabled = target_operations[2] != 0
        mask_agnostic_enabled = (
            load_enabled
            and policy_targets["vma"] != 0
            and policy_targets["masked"] != 0
        )
        tail_agnostic_enabled = (
            load_enabled
            and policy_targets["vta"] != 0
            and policy_targets["partial_vl"] != 0
        )
        _require(
            (agnostic[0] > 0) == mask_agnostic_enabled,
            "mask-agnostic semantic coverage does not match enabled vector policy",
        )
        _require(
            (agnostic[1] > 0) == tail_agnostic_enabled,
            "tail-agnostic semantic coverage does not match enabled vector policy",
        )
        _require(
            all(count <= vector_directions[0] for count in agnostic),
            "vector agnostic semantic counts exceed constrained vector loads",
        )

    if schema >= 41:
        vector_actuals = {
            dimension: _csv_counts(
                result, f"actual_vector_{dimension}", fields
            )
            for dimension, fields in (
                ("addressing", 4),
                ("eew", 4),
                ("sew", 4),
                ("lmul", 7),
                ("emul", 7),
            )
        }
        _check_vector_shape_cross(
            result,
            target_operations,
            vector_targets,
            vector_actuals,
            policy_targets,
            vector_directions,
            vector_shape_operations,
        )


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


def _balanced_queue(result: dict[str, Any], name: str) -> tuple[int, int]:
    value = result.get(name)
    _require(isinstance(value, str), f"{name} accounting is not a string")
    try:
        retired, allocated = value.split("/", 1)
        dequeued, canceled = retired.split("+", 1)
        allocated_count = int(allocated, 10)
        canceled_count = int(canceled, 10)
        balanced = int(dequeued, 10) + canceled_count == allocated_count
    except (ValueError, AttributeError) as error:
        raise VerificationError(f"invalid {name} accounting: {value!r}") from error
    _require(balanced, f"unbalanced {name} accounting: {value}")
    return allocated_count, canceled_count


def _check_mixed_coverage(
    result: dict[str, Any], require_backpressure: bool = False
) -> None:
    _check_constraint_coverage(result)
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
        vector_replays = result.get("vector_replays")
        _require(
            isinstance(vector_replays, int)
            and not isinstance(vector_replays, bool)
            and vector_replays >= 0,
            f"vector_replays diagnostic is invalid: {vector_replays!r}",
        )
        for name in ("virtualization", "exceptions"):
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
    lq_allocated, lq_canceled = _balanced_queue(result, "lq")
    sq_allocated, sq_canceled = _balanced_queue(result, "sq")
    lsq_monitor_schema = result.get("lsq_monitor_schema", 0)
    _require(
        lsq_monitor_schema in (0, 1, 2),
        f"unknown LSQ enqueue monitor schema: {lsq_monitor_schema!r}",
    )
    if lsq_monitor_schema in (1, 2):
        observed = _csv_counts(result, "lsq_enqueued_observed", 2)
        _require(
            observed == [lq_allocated, sq_allocated],
            "observed LSQ enqueue counts disagree with allocated queue totals",
        )
    if lsq_monitor_schema == 2:
        redirect_cancels = _csv_counts(
            result, "redirect_cancels_observed", 3
        )
        unobserved_cancels = _csv_counts(result, "unobserved_cancels", 2)
        _require(
            redirect_cancels[0] > 0,
            "no redirect cancellation event was independently observed",
        )
        _require(
            redirect_cancels[1] + unobserved_cancels[0] == lq_canceled
            and redirect_cancels[2] + unobserved_cancels[1] == sq_canceled,
            "observed and unobserved cancellation classes do not conserve queue totals",
        )
        _require(
            unobserved_cancels == [0, 0],
            "random-mixed used an unobserved queue cancellation",
        )


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


def _check_frontend_bridge_coverage(result: dict[str, Any]) -> None:
    transactions = result.get("transactions")
    _require(
        isinstance(transactions, int)
        and not isinstance(transactions, bool)
        and transactions >= 32,
        f"frontend bridge has insufficient transactions: {transactions!r}",
    )
    _require(
        result.get("requests") == transactions * 3,
        "frontend bridge request count disagrees with three request paths",
    )
    _require(
        result.get("responses") == transactions * 4,
        "frontend bridge response count disagrees with the expected beat count",
    )
    for name in ("request_stalls", "response_stalls", "source_credit_stalls"):
        value = result.get(name)
        _require(
            isinstance(value, int) and not isinstance(value, bool) and value > 0,
            f"frontend bridge {name} coverage is absent: {value!r}",
        )
    field_checks = result.get("field_checks")
    _require(
        isinstance(field_checks, int)
        and not isinstance(field_checks, bool)
        and field_checks >= transactions * 39,
        f"frontend bridge field checking is incomplete: {field_checks!r}",
    )


def _check_result(
    result: Any,
    index: int,
    requested_transactions: dict[str, int],
    completed_transactions: dict[str, int],
    require_backpressure: bool = False,
) -> tuple[int, str, str, int, str, float, tuple[str, ...]]:
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
        and len(command) >= len(expected_tail) + 1
        and command[1 : len(expected_tail) + 1] == expected_tail
        and all(isinstance(item, str) for item in command),
        f"{prefix} command does not replay its recorded case",
    )
    command_options = tuple(command[len(expected_tail) + 1 :])

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
    grant_fields = ("dcache_refills", "dcache_acquire_perms", "grant_acks")
    if any(name in result for name in grant_fields):
        _require(
            all(
                isinstance(result.get(name), int)
                and not isinstance(result.get(name), bool)
                and result[name] >= 0
                for name in grant_fields
            ),
            f"{prefix} has an incomplete DCache grant/ack accounting tuple",
        )
        _require(
            result["grant_acks"]
            == result["dcache_refills"] + result["dcache_acquire_perms"],
            f"{prefix} DCache grants and GrantAcks are not conserved",
        )
    if scenario == "random-mixed":
        _check_mixed_coverage(result, require_backpressure)
    elif scenario == run_regression.STRESS_SCENARIO:
        _check_stress_coverage(result, require_backpressure)
    elif scenario == run_regression.FRONTEND_BRIDGE_SCENARIO:
        _check_frontend_bridge_coverage(result)
    return (
        seed,
        scenario,
        str(command[0]),
        int(transactions),
        rtl_hash,
        float(completed_offset),
        command_options,
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
    command_options: set[tuple[str, tuple[str, ...]]] = set()

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
                            options,
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
                    command_options.add((scenario, options))
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
        "command_options": command_options,
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

    backpressure = configuration.get("backpressure")
    _require(isinstance(backpressure, bool), "configuration has no boolean backpressure mode")
    hunt_boundaries = configuration.get("hunt_boundaries", False)
    _require(isinstance(hunt_boundaries, bool), "configuration has invalid boundary-hunt mode")
    expected_command_options: set[tuple[str, tuple[str, ...]]] = set()
    for scenario in scenarios:
        options: list[str] = []
        if scenario == run_regression.CONSTRAINED_SCENARIO:
            has_profile = "constraint_profile" in configuration
            has_overrides = "constraint_overrides" in configuration
            _require(
                has_profile == has_overrides,
                "constraint profile and overrides must be recorded together",
            )
            if has_profile:
                profile = configuration.get("constraint_profile")
                overrides = configuration.get("constraint_overrides")
                _require(
                    profile in run_regression.CONSTRAINT_PROFILES,
                    f"invalid recorded constraint profile: {profile!r}",
                )
                _require(
                    isinstance(overrides, list)
                    and all(isinstance(item, str) for item in overrides),
                    "invalid recorded constraint overrides",
                )
                options.extend(("--constraints", profile))
                for override in overrides:
                    options.extend(("--constraint", override))
        if not backpressure:
            options.append("--no-backpressure")
        if hunt_boundaries:
            options.append("--hunt-boundaries")
        expected_command_options.add((scenario, tuple(options)))
    _require(
        observed["command_options"] == expected_command_options,
        "per-result command options differ from recorded configuration",
    )

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

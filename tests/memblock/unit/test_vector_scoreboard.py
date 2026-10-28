#!/usr/bin/env python3
"""Run the production C++ vector scoreboard against valid/corrupted completions."""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]


class VectorScoreboardTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.workspace = tempfile.TemporaryDirectory(prefix="memblock-scoreboard-")
        cls.addClassCleanup(cls.workspace.cleanup)
        root = Path(cls.workspace.name)
        cls.binary = root / "vector_scoreboard"
        source = root / "scoreboard.cpp"
        source.write_text(
            '#include "memblock_env.hpp"\nnamespace memblock {\n'
            '#include "environment/scoreboards.inc"\n}\n'
            '#include "vector_scoreboard_fixture.cpp"\n'
        )
        subprocess.run(
            ["c++", "-std=c++20", "-O0", "-I", str(MEMBLOCK_ROOT / "cpp"),
             "-I", str(MEMBLOCK_ROOT / "unit"), str(source), "-o", str(cls.binary)],
            check=True, capture_output=True, text=True, timeout=60,
        )

    def check_case(self, name: str) -> None:
        result = subprocess.run([str(self.binary), name], capture_output=True,
                                text=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_late_raw_redirect_requires_a_fresh_strict_refetch_epoch(self) -> None:
        self.check_case("late-raw-refetch")

    def test_permuted_repeated_indices_reject_wrong_data_routing(self) -> None:
        self.check_case("permuted-indices")

    def test_high_mask_bits_and_vstart_classification(self) -> None:
        self.check_case("high-mask")

    def test_canceled_result_requires_opt_in_and_cancel_pulse(self) -> None:
        self.check_case("canceled")

    def test_unit_and_strided_destination_index(self) -> None:
        self.check_case("ordinary")

    def test_scalar_reset_cancellation_preserves_exact_identity_and_errors(self) -> None:
        self.check_case("scalar-reset")

    def test_exceptional_segment_fix_counts_as_directional_completion(self) -> None:
        self.check_case("segment-exception-counts")

    def test_indexed_split_and_fractional_destination_index(self) -> None:
        self.check_case("indexed")

    def test_segment_field_destination_index(self) -> None:
        self.check_case("segment")

    def test_whole_register_destination_index(self) -> None:
        self.check_case("whole")

    def test_suppressed_fof_data_retains_destination_index_check(self) -> None:
        self.check_case("fof")

    def test_irrelevant_destination_index_is_unconstrained(self) -> None:
        self.check_case("irrelevant")

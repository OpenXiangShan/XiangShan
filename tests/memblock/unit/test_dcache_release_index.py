#!/usr/bin/env python3
"""Run the production DcacheReleaseIndex against full/partial release shapes."""

from __future__ import annotations

import subprocess
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]


class DcacheReleaseIndexTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls) -> None:
        cls.workspace = tempfile.TemporaryDirectory(prefix="memblock-release-index-")
        cls.addClassCleanup(cls.workspace.cleanup)
        root = Path(cls.workspace.name)
        cls.binary = root / "release_index"
        source = root / "release_index.cpp"
        source.write_text(
            "#include <cstdint>\n"
            "#include <iostream>\n"
            "#include <optional>\n"
            "#include <stdexcept>\n"
            "#include <string>\n"
            "#include <unordered_map>\n"
            "namespace memblock {\n"
            '#include "environment/dcache_a_contract.inc"\n'
            "}\n"
            '#include "dcache_release_index_fixture.cpp"\n'
        )
        subprocess.run(
            ["c++", "-std=c++20", "-O0", "-I", str(MEMBLOCK_ROOT / "cpp"),
             "-I", str(MEMBLOCK_ROOT / "unit"), str(source),
             "-o", str(cls.binary)],
            check=True, capture_output=True, text=True, timeout=60,
        )

    def check_case(self, name: str) -> None:
        result = subprocess.run([str(self.binary), name], capture_output=True,
                                text=True, timeout=10)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    def test_full_line_release_and_data_counts(self) -> None:
        self.check_case("full-line")

    def test_nonfull_release_disables_index(self) -> None:
        self.check_case("nonfull")

    def test_unaligned_query_disables_index(self) -> None:
        self.check_case("unaligned")

    def test_clear_resets_counts_and_flag(self) -> None:
        self.check_case("clear")


if __name__ == "__main__":
    unittest.main()

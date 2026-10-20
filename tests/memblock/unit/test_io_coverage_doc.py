#!/usr/bin/env python3

from __future__ import annotations

import json
import sys
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import check_io_coverage  # noqa: E402


class IoCoverageDocTest(unittest.TestCase):
    def test_current_document_matches_manifest_and_constraint_schema(self) -> None:
        check_io_coverage.check(
            MEMBLOCK_ROOT / "config/expected_ports.json",
            MEMBLOCK_ROOT / "docs/IO_COVERAGE.md",
            MEMBLOCK_ROOT / "cpp/random/constraints.inc",
            (MEMBLOCK_ROOT / "docs/CONSTRAINED_RANDOM.md",),
        )

    def test_inventory_renders_all_manifest_groups(self) -> None:
        manifest = json.loads(
            (MEMBLOCK_ROOT / "config/expected_ports.json").read_text(encoding="utf-8")
        )
        inventory = check_io_coverage.inventory_markdown(manifest)
        self.assertIn("| **Total** | **1335** | **749** | **586** |", inventory)
        for group in (
            "dcache_tilelink",
            "frontend_tilelink",
            "infrastructure",
            "mem_to_ooo",
            "misc",
            "ooo_to_mem",
            "perf",
            "ptw_tilelink",
            "uncache_tilelink",
        ):
            self.assertIn(f"| `{group}` |", inventory)

    def test_stale_inventory_is_rejected(self) -> None:
        document = (MEMBLOCK_ROOT / "docs/IO_COVERAGE.md").read_text(encoding="utf-8")
        stale = document.replace("| **Total** | **1335** |", "| **Total** | **1334** |")
        with tempfile.TemporaryDirectory() as temporary:
            path = Path(temporary) / "IO_COVERAGE.md"
            path.write_text(stale, encoding="utf-8")
            with self.assertRaisesRegex(
                check_io_coverage.CoverageDocError, "inventory is stale"
            ):
                check_io_coverage.check(
                    MEMBLOCK_ROOT / "config/expected_ports.json",
                    path,
                    MEMBLOCK_ROOT / "cpp/random/constraints.inc",
                )


if __name__ == "__main__":
    unittest.main()

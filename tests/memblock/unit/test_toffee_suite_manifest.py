#!/usr/bin/env python3

from __future__ import annotations

import json
import sys
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "toffee" / "scripts"))

import check_suite_manifest  # noqa: E402


class ToffeeSuiteManifestTest(unittest.TestCase):
    def test_every_legacy_scenario_has_one_toffee_suite(self) -> None:
        migrated, total = check_suite_manifest.check(
            MEMBLOCK_ROOT / "toffee/suite_manifest.json",
            MEMBLOCK_ROOT / "scripts/benchmark_tests.py",
            strict_complete=False,
        )
        self.assertEqual(total, 90)
        self.assertEqual(migrated, 2)

    def test_duplicate_assignment_is_rejected(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            scenario_file = root / "scenarios.py"
            scenario_file.write_text("SCENARIOS = ('one',)\n", encoding="utf-8")
            manifest = root / "manifest.json"
            manifest.write_text(
                json.dumps(
                    {
                        "schema_version": 1,
                        "suites": [
                            {
                                "name": "a",
                                "status": "planned",
                                "marker": "a",
                                "tests": None,
                                "exact": ["one"],
                                "prefixes": [],
                            },
                            {
                                "name": "b",
                                "status": "planned",
                                "marker": "b",
                                "tests": None,
                                "exact": [],
                                "prefixes": ["on"],
                            },
                        ],
                    }
                ),
                encoding="utf-8",
            )
            with self.assertRaisesRegex(
                check_suite_manifest.ManifestError,
                "more than once",
            ):
                check_suite_manifest.check(manifest, scenario_file, strict_complete=False)


if __name__ == "__main__":
    unittest.main()

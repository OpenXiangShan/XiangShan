#!/usr/bin/env python3

from __future__ import annotations

import json
import sys
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import bootstrap_toffee  # noqa: E402


class BootstrapToffeeTest(unittest.TestCase):
    def test_previous_revisions_reads_environment_metadata(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            metadata = Path(temporary) / "environment.json"
            metadata.write_text(
                json.dumps(
                    {
                        "picker_commit": "picker",
                        "xcomm_commit": "xcomm",
                    }
                ),
                encoding="utf-8",
            )
            self.assertEqual(
                bootstrap_toffee.previous_revisions(metadata),
                ("picker", "xcomm"),
            )

    def test_missing_metadata_requires_a_fresh_environment(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            metadata = Path(temporary) / "missing.json"
            self.assertEqual(
                bootstrap_toffee.previous_revisions(metadata),
                ("", ""),
            )


if __name__ == "__main__":
    unittest.main()

#!/usr/bin/env python3

from __future__ import annotations

import json
import stat
import sys
import tempfile
import unittest
from pathlib import Path
from unittest import mock


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import freeze_runtime  # noqa: E402


class FreezeRuntimeTest(unittest.TestCase):
    def test_freezes_named_read_only_artifacts(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            sources = root / "sources"
            sources.mkdir()
            binary = sources / "sim"
            model = sources / "model.so"
            xspcomm = sources / "xspcomm.so"
            rtl_metadata = sources / "rtl.json"
            binary.write_bytes(b"binary")
            model.write_bytes(b"model")
            xspcomm.write_bytes(b"xspcomm")
            rtl_metadata.write_text(
                json.dumps({"complete_rtl_sha256": "a" * 64}),
                encoding="utf-8",
            )
            output = root / "runtime"

            def dependencies(frozen_binary: Path, directory: Path):
                return [
                    {
                        "soname": "libUTMemBlock.so",
                        "path": str((directory / "libUTMemBlock.so").resolve()),
                        "sha256": freeze_runtime.sha256(directory / "libUTMemBlock.so"),
                        "size": (directory / "libUTMemBlock.so").stat().st_size,
                    },
                    {
                        "soname": "libxspcomm.so.0.0.1",
                        "path": str((directory / "libxspcomm.so.0.0.1").resolve()),
                        "sha256": freeze_runtime.sha256(
                            directory / "libxspcomm.so.0.0.1"
                        ),
                        "size": (directory / "libxspcomm.so.0.0.1").stat().st_size,
                    },
                ]

            with mock.patch.object(
                freeze_runtime, "resolved_dependencies", side_effect=dependencies
            ):
                metadata = freeze_runtime.freeze(
                    binary, model, xspcomm, rtl_metadata, output
                )

            document = json.loads(metadata.read_text())
            self.assertEqual(
                {entry["role"] for entry in document["artifacts"]},
                {"binary", "model", "rtl_metadata", "xspcomm"},
            )
            self.assertEqual(
                stat.S_IMODE((output / "memblock_sim").stat().st_mode), 0o555
            )
            self.assertEqual(
                stat.S_IMODE((output / "libUTMemBlock.so").stat().st_mode), 0o444
            )
            self.assertEqual(stat.S_IMODE((output / "rtl.json").stat().st_mode), 0o444)
            self.assertEqual(
                (output / "rtl.json").read_text(encoding="utf-8"),
                rtl_metadata.read_text(encoding="utf-8"),
            )
            self.assertEqual(stat.S_IMODE(metadata.stat().st_mode), 0o444)

    def test_dependency_parser_rejects_unrecognized_lines(self) -> None:
        completed = mock.Mock(returncode=0, stdout="not valid ldd output\n")
        with mock.patch.object(freeze_runtime.subprocess, "run", return_value=completed):
            with self.assertRaises(freeze_runtime.FreezeError):
                freeze_runtime.resolved_dependencies(Path("sim"), Path("runtime"))


if __name__ == "__main__":
    unittest.main()

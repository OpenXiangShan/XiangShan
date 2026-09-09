#!/usr/bin/env python3

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import prepare_picker_export  # noqa: E402


class PreparePickerExportTest(unittest.TestCase):
    def test_adds_cpp_language_to_nested_mem_direct_export(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            picker = Path(temporary)
            makefile = picker / "Makefile"
            makefile.write_text(
                "RW_TYPE := MEM_DIRECT\n"
                "\t@picker export root.h \\\n\t\t--source_dir template/mem_direct \\\n"
                "\t\t--source_module_name MemBlock --sim verilator\n",
                encoding="utf-8",
            )

            prepare_picker_export.prepare(picker)
            prepared = makefile.read_text(encoding="utf-8")
            self.assertIn("--language $(TLANG)", prepared)
            prepare_picker_export.prepare(picker)
            self.assertEqual(makefile.read_text(encoding="utf-8"), prepared)

    def test_rejects_non_mem_direct_export(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            picker = Path(temporary)
            (picker / "Makefile").write_text(
                "RW_TYPE := DPI\n"
                "\t--source_module_name MemBlock --sim verilator\n",
                encoding="utf-8",
            )

            with self.assertRaises(prepare_picker_export.PickerExportError):
                prepare_picker_export.prepare(picker)


if __name__ == "__main__":
    unittest.main()

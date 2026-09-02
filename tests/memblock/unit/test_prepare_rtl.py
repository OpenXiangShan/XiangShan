#!/usr/bin/env python3

from __future__ import annotations

import sys
import tempfile
import unittest
from pathlib import Path


MEMBLOCK_ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(MEMBLOCK_ROOT / "scripts"))

import prepare_rtl  # noqa: E402


class PrepareRtlTest(unittest.TestCase):
    def test_resolves_paths_excludes_system_top_and_appends_bind(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            (root / "Dep.sv").write_text("module Dep; endmodule\n", encoding="utf-8")
            (root / "MemBlock.sv").write_text(
                "module MemBlock; Dep dep(); endmodule\n", encoding="utf-8"
            )
            (root / "XSTop.sv").write_text("module XSTop; endmodule\n", encoding="utf-8")
            (root / "memory_ext.v").write_text(
                "module memory_ext; endmodule\n", encoding="utf-8"
            )
            bind = root / "bind.sv"
            bind.write_text("bind MemBlock Dep monitor();\n", encoding="utf-8")
            filelist = root / "filelist.f"
            filelist.write_text("Dep.sv\nMemBlock.sv\nXSTop.sv\n", encoding="utf-8")
            config = {
                "module": "MemBlock",
                "rtl_filelist": {
                    "top_file": "MemBlock.sv",
                    "exclude_basenames": ["XSTop.sv"],
                    "supplemental_globs": ["*_ext.v"],
                },
            }
            files, metadata = prepare_rtl.prepare(filelist, bind, config)
            self.assertEqual(
                ["Dep.sv", "MemBlock.sv", "memory_ext.v", "bind.sv"],
                [p.name for p in files],
            )
            self.assertEqual(3, metadata["source_file_count"])
            self.assertEqual(3, metadata["selected_file_count"])
            self.assertEqual(1, len(metadata["excluded_files"]))
            self.assertEqual(1, len(metadata["supplemental_files"]))
            self.assertRegex(metadata["complete_rtl_sha256"], r"^[0-9a-f]{64}$")

            original_hash = metadata["complete_rtl_sha256"]
            (root / "Dep.sv").write_text("module Dep; wire x; endmodule\n")
            _, changed_metadata = prepare_rtl.prepare(filelist, bind, config)
            self.assertNotEqual(
                original_hash, changed_metadata["complete_rtl_sha256"]
            )

    def test_rejects_duplicate_module_declarations(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            root = Path(temporary)
            for name in ("MemBlock.sv", "Other.sv"):
                (root / name).write_text("module MemBlock; endmodule\n", encoding="utf-8")
            bind = root / "bind.sv"
            bind.write_text("// bind\n", encoding="utf-8")
            filelist = root / "filelist.f"
            filelist.write_text("MemBlock.sv\nOther.sv\n", encoding="utf-8")
            config = {"module": "MemBlock", "rtl_filelist": {"top_file": "MemBlock.sv"}}
            with self.assertRaisesRegex(prepare_rtl.FilelistError, "duplicate module"):
                prepare_rtl.prepare(filelist, bind, config)


if __name__ == "__main__":
    unittest.main()

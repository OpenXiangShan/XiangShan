"""Unit tests for PPA report formatting, FO4 framing, and git highlighting."""

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import ppa_report  # noqa: E402

MOCK_FO4 = {
    "fo4_ps": 14.37,
    "primary_cell": "INVx1_ASAP7_75t_R",
    "target_fo4": 41.1,
    "asap7_target_ps": 590.8,
}

MOCK_SYNTH_BUNDLE = {
    "stage": "synth",
    "modules": {
        "VirtualStoreQueue": {
            "module": "VirtualStoreQueue",
            "stage": "synth",
            "primary_group": "reg2reg",
            "fo4": 34.8,
            "fo4_fir": 31.2,
            "period_ps": 500.1,
            "wns_ps": 499.9,
            "tns_ps": 0.0,
            "levels": 18,
            "flops": 840,
            "comb_cells": 4120,
            "sram_bits": 0,
            "icgs": 12,
            "max_fanout": 64,
            "unclocked_regs": 0,
            "mem2mem_hazard": False,
            "area_um2": 612.4,
            "macro_area_um2": 0.0,
            "buffers_total": 90,
            "buffers_added": 28,
            "elapsed_s": 4.2,
            "path_groups": {
                "reg2reg": {
                    "fo4": 34.8,
                    "period_ps": 500.1,
                    "wns_ps": 499.9,
                    "levels": 18,
                    "startpoint": "deqPtr_r/Q",
                    "endpoint": "recovery_r/D",
                    "end_loc": "src/main/scala/xiangshan/mem/lsqueue/VirtualStoreQueue.scala:153",
                }
            },
            "source_files": ["src/main/scala/xiangshan/mem/lsqueue/VirtualStoreQueue.scala"],
        },
        "MicroBtb": {
            "module": "MicroBtb",
            "stage": "synth",
            "primary_group": "reg2reg",
            "fo4": 22.1,
            "fo4_fir": 20.5,
            "period_ps": 317.6,
            "wns_ps": 682.4,
            "tns_ps": 0.0,
            "levels": 11,
            "flops": 310,
            "comb_cells": 1400,
            "sram_bits": 0,
            "icgs": 4,
            "max_fanout": 32,
            "unclocked_regs": 0,
            "mem2mem_hazard": False,
            "area_um2": 210.0,
            "macro_area_um2": 0.0,
            "buffers_total": 20,
            "buffers_added": 5,
            "elapsed_s": 2.1,
            "path_groups": {},
            "source_files": ["src/main/scala/xiangshan/frontend/bpu/ubtb/MicroBtb.scala"],
        },
    },
}


class PpaReportTest(unittest.TestCase):
    def test_fo4_first_then_fo4_fir_and_full_asap7_ps(self):
        changed = {"src/main/scala/xiangshan/frontend/bpu/ubtb/MicroBtb.scala"}
        txt = ppa_report.format_report(
            "synth",
            MOCK_SYNTH_BUNDLE,
            MOCK_FO4,
            base_ref="origin/main",
            mb_sha="aa6b52033add528a504bdb2c2b726100d89f070a",
            changed_files=changed,
        )
        # Summary leads with FO4 (synth), immediately followed by FO4 (fir)
        self.assertLess(txt.index("FO4 (synth)"), txt.index("FO4 (fir)"))
        self.assertLess(txt.index("FO4 (fir)"), txt.index("Period (ps)"))
        # MicroBtb is git-highlighted first (`* MicroBtb`)
        self.assertIn("* MicroBtb", txt)
        self.assertIn("  VirtualStoreQueue", txt)
        self.assertLess(txt.index("* MicroBtb"), txt.index("  VirtualStoreQueue"))
        # Actual ASAP7 ps and area are reported in Full ASAP7 section
        self.assertIn("500.1", txt)
        self.assertIn("612.4", txt)

    def test_full_report_with_silent_merge_base_when_no_module_touched(self):
        txt = ppa_report.format_report(
            "synth",
            MOCK_SYNTH_BUNDLE,
            MOCK_FO4,
            base_ref="origin/main",
            mb_sha="aa6b52033add528a504bdb2c2b726100d89f070a",
            changed_files={"README.md"},
        )
        # Full report is still rendered with both modules
        self.assertIn("VirtualStoreQueue", txt)
        self.assertIn("MicroBtb", txt)
        self.assertIn("500.1", txt)
        # Merge-base highlighting stays completely silent when no RTL module changed
        self.assertNotIn("Git merge-base", txt)
        self.assertNotIn("touched since git merge-base", txt)
        self.assertNotIn("* ", txt)

    def test_fir_stage_surfaces_touched_extra_modules_and_stays_compact_when_clean(self):
        fir_bundle = {
            "stage": "fir",
            "modules": {
                "DecodeStage": {
                    "module": "DecodeStage",
                    "stage": "fir",
                    "primary_group": "reg2reg",
                    "fo4": 29.4,
                    "levels": 22,
                    "flops": 328,
                    "flop_bits": 1409,
                    "comb_cells": 3974582,
                    "sram_bits": 0,
                    "icgs": 0,
                    "max_fanout": 88,
                    "mem2mem_hazard": False,
                    "path_groups": {},
                    "own_source_files": ["src/main/scala/xiangshan/backend/decode/DecodeStage.scala"],
                    "source_files": ["src/main/scala/xiangshan/backend/decode/DecodeStage.scala"],
                },
            },
            "extra_modules": {
                "MicroBtb": {
                    "module": "MicroBtb",
                    "stage": "fir",
                    "primary_group": "in2reg",
                    "fo4": 17.8,
                    "levels": 19,
                    "flops": 203,
                    "flop_bits": 1896,
                    "comb_cells": 21555,
                    "sram_bits": 0,
                    "icgs": 0,
                    "max_fanout": 212,
                    "mem2mem_hazard": False,
                    "path_groups": {},
                    "own_source_files": ["src/main/scala/xiangshan/frontend/bpu/ubtb/MicroBtb.scala"],
                    "source_files": ["src/main/scala/xiangshan/frontend/bpu/ubtb/MicroBtb.scala"],
                },
            },
        }
        # Untouched run keeps only the panel module (DecodeStage), omitting extra_modules
        clean_txt = ppa_report.format_report("fir", fir_bundle, MOCK_FO4, changed_files=set())
        self.assertIn("DecodeStage", clean_txt)
        self.assertNotIn("MicroBtb", clean_txt)

        # Touching MicroBtb.scala automatically surfaces & highlights `* MicroBtb`
        touched_txt = ppa_report.format_report(
            "fir",
            fir_bundle,
            MOCK_FO4,
            base_ref="origin/main",
            mb_sha="aa6b52033a",
            changed_files={"src/main/scala/xiangshan/frontend/bpu/ubtb/MicroBtb.scala"},
        )
        self.assertIn("* MicroBtb", touched_txt)
        self.assertIn("  DecodeStage", touched_txt)


if __name__ == "__main__":
    unittest.main()

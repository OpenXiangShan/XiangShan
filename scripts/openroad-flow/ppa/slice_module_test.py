"""Unit tests for deterministic SV module slicing and line-shift cache stability."""

import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import slice_module  # noqa: E402


class SliceModuleTest(unittest.TestCase):
    def test_strips_locs_for_cache_stability_and_keeps_srcmap(self):
        with tempfile.TemporaryDirectory() as d:
            sv_dir = os.path.join(d, "sv")
            os.makedirs(sv_dir)
            with open(os.path.join(sv_dir, "Leaf.sv"), "w") as f:
                f.write(
                    "module Leaf(\n"
                    "  input  wire a,\n"
                    "  output wire y\n"
                    ");\n"
                    "  assign y = ~a; // @[src/main/scala/xiangshan/Leaf.scala:10:5]\n"
                    "endmodule\n"
                )
            with open(os.path.join(sv_dir, "Top.sv"), "w") as f:
                f.write(
                    '`include "defs.vh"\n'
                    "module Top(\n"
                    "  input  wire clock,\n"
                    "  input  wire x,\n"
                    "  output wire z\n"
                    ");\n"
                    "  reg r_q; // @[src/main/scala/xiangshan/Top.scala:22:11]\n"
                    "  Leaf u_leaf ( // @[src/main/scala/xiangshan/Top.scala:25:9]\n"
                    "    .a(x),\n"
                    "    .y(z)\n"
                    "  );\n"
                    "endmodule\n"
                )
            with open(os.path.join(sv_dir, "Unrelated.sv"), "w") as f:
                f.write("module Unrelated(input a, output b); assign b = a; endmodule\n")

            out1 = os.path.join(d, "Top1.sv")
            map1 = os.path.join(d, "Top1.json")
            slice_module.slice_module(sv_dir, "Top", out1, map1)

            # Shift line numbers in Top.sv and mutate Unrelated.sv: Top's SV slice must stay byte-identical!
            with open(os.path.join(sv_dir, "Top.sv"), "w") as f:
                f.write(
                    "module Top(\n"
                    "  input  wire clock,\n"
                    "  input  wire x,\n"
                    "  output wire z\n"
                    ");\n"
                    "  reg r_q; // @[src/main/scala/xiangshan/Top.scala:99:11]\n"
                    "  Leaf u_leaf ( // @[src/main/scala/xiangshan/Top.scala:102:9]\n"
                    "    .a(x),\n"
                    "    .y(z)\n"
                    "  );\n"
                    "endmodule\n"
                )
            with open(os.path.join(sv_dir, "Unrelated.sv"), "w") as f:
                f.write("module Unrelated(input a, output b); assign b = ~a; endmodule\n")

            out2 = os.path.join(d, "Top2.sv")
            map2 = os.path.join(d, "Top2.json")
            slice_module.slice_module(sv_dir, "Top", out2, map2)

            with open(out1) as f1, open(out2) as f2:
                self.assertEqual(f1.read(), f2.read())
            with open(map1) as mf:
                mdata = json.load(mf)
            self.assertEqual(
                mdata["source_files"],
                ["src/main/scala/xiangshan/Leaf.scala", "src/main/scala/xiangshan/Top.scala"],
            )
            self.assertEqual(mdata["signal_locs"]["r_q"], "src/main/scala/xiangshan/Top.scala:22")


if __name__ == "__main__":
    unittest.main()

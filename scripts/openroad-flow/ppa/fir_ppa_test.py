"""Unit tests for the static CIRCT MLIR (.fir) FO4 & structural PPA estimator."""

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import fir_ppa  # noqa: E402

SAMPLE_MLIR = """\
module {
  hw.module @array_64x32(in %R0_addr : i6, in %R0_en : i1, in %R0_clk : !seq.clock, out R0_data : i32) {
    %c0_i32 = hw.constant 0 : i32
    hw.output %c0_i32 : i32
  }
  hw.module @ChildCmp(in %a : i64, in %b : i64, out hit : i1) {
    %0 = comb.add %a, %b : i64 loc(#loc1)
    %1 = comb.icmp eq %0, %b : i64 loc(#loc1)
    hw.output %1 : i1
  }
  hw.module @SampleQueue(in %clock : !seq.clock, in %reset : i1, in %io_in : i64, in %io_sel : i1, out io_out : i64) {
    %c0_i64 = hw.constant 0 : i64
    %r0 = seq.firreg %next0 clock %clock reset sync %reset, %c0_i64 : i64 loc(#loc2)
    %r1 = seq.firreg %next1 clock %clock : i64 loc(#loc2)
    %cg.Q = hw.instance "cg" @ClockGate(TE: %reset: i1, E: %io_sel: i1, CK: %clock: !seq.clock) -> (Q: !seq.clock)
    %mem.R0_data = hw.instance "tag_mem" @array_64x32(R0_addr: %addr: i6, R0_en: %io_sel: i1, R0_clk: %clock: !seq.clock) -> (R0_data: i32)
    %cmp.hit = hw.instance "u_cmp" @ChildCmp(a: %r0: i64, b: %r1: i64) -> (hit: i1)
    %addr = comb.extract %r0 from 0 : (i64) -> i6
    %ext = comb.concat %mem.R0_data, %mem.R0_data : i32, i32
    %sum = comb.add %r0, %r1 : i64 loc(#loc2)
    %next0 = comb.mux %cmp.hit, %sum, %io_in : i64 loc(#loc2)
    %next1 = comb.mux %io_sel, %ext, %r0 : i64 loc(#loc2)
    hw.output %r1 : i64
  }
}
#loc1 = loc("src/main/scala/xiangshan/ChildCmp.scala":12:8)
#loc2 = loc("src/main/scala/xiangshan/SampleQueue.scala":48:15)
"""


class FirPpaTest(unittest.TestCase):
    def test_sample_module_metrics(self):
        res = fir_ppa.analyze_mlir_text(SAMPLE_MLIR.splitlines(True))
        mods = res["modules"]
        self.assertIn("SampleQueue", mods)
        sq = mods["SampleQueue"]
        self.assertEqual(sq["flops"], 2)
        self.assertEqual(sq["flop_bits"], 128)
        self.assertEqual(sq["icgs"], 1)
        self.assertEqual(sq["sram_count"], 1)
        self.assertEqual(sq["sram_bits"], 64 * 32)
        self.assertIn("reg2reg", sq["path_groups"])
        self.assertIn("mem2reg", sq["path_groups"])
        self.assertIn("reg2mem", sq["path_groups"])
        self.assertGreater(sq["fo4"], 10.0)
        self.assertIn("src/main/scala/xiangshan/SampleQueue.scala", sq["source_files"])
        self.assertIn("src/main/scala/xiangshan/ChildCmp.scala", sq["source_files"])

    def test_pipeline_cut_reduces_fo4(self):
        # Cutting a two-adder chain with a register must reduce reg2reg FO4
        long_comb = """\
module {
  hw.module @PipeTest(in %clock : !seq.clock, in %a : i64, out y : i64) {
    %r0 = seq.compreg %a, %clock : i64
    %s1 = comb.add %r0, %a : i64
    %s2 = comb.add %s1, %r0 : i64
    %s3 = comb.icmp ult %s2, %r0 : i64
    %s4 = comb.mux %s3, %s2, %s1 : i64
    %r1 = seq.compreg %s4, %clock : i64
    hw.output %r1 : i64
  }
}
"""
        pipelined = """\
module {
  hw.module @PipeTest(in %clock : !seq.clock, in %a : i64, out y : i64) {
    %r0 = seq.compreg %a, %clock : i64
    %s1 = comb.add %r0, %a : i64
    %rmid = seq.compreg %s1, %clock : i64
    %s3 = comb.icmp ult %rmid, %r0 : i64
    %s4 = comb.mux %s3, %rmid, %r0 : i64
    %r1 = seq.compreg %s4, %clock : i64
    hw.output %r1 : i64
  }
}
"""
        m_long = fir_ppa.analyze_mlir_text(long_comb.splitlines(True))["modules"]["PipeTest"]
        m_pipe = fir_ppa.analyze_mlir_text(pipelined.splitlines(True))["modules"]["PipeTest"]
        self.assertLess(m_pipe["fo4"], m_long["fo4"])
        self.assertGreater(m_pipe["flops"], m_long["flops"])

    def test_deep_combinational_chain_no_recursion_error(self):
        lines = [
            "module {\n",
            "  hw.module @DeepChain(in %clock : !seq.clock, in %a : i8, out y : i8) {\n",
            "    %r0 = seq.compreg %a, %clock : i8\n",
        ]
        prev = "%r0"
        for i in range(1600):
            cur = f"%n{i}"
            lines.append(f"    {cur} = comb.xor {prev}, %a : i8\n")
            prev = cur
        lines.extend([
            f"    %r1 = seq.compreg {prev}, %clock : i8\n",
            "    hw.output %r1 : i8\n",
            "  }\n",
            "}\n",
        ])
        m = fir_ppa.analyze_mlir_text(lines)["modules"]["DeepChain"]
        self.assertEqual(m["levels"], 1600)
        self.assertGreater(m["fo4"], 1600.0)

    def test_per_port_ilm_prevents_false_cross_port_chaining(self):
        # StageMod has registered data (`d_in -> d_out`) AND combinational ready (`r_out -> r_in`).
        # Chaining 4 StageMods must NOT couple `d_in` through `r_out -> r_in` across stages!
        mlir = """\
module {
  hw.module private @sram_array_1p128x88m1s1_dat(in %RW0_clk : !seq.clock, in %RW0_addr : i7, out RW0_rdata : i88) {
    %c0 = hw.constant 0 : i88
    hw.output %c0 : i88
  }
  hw.module @StageMod(in %clock : !seq.clock, in %d_in : i64, out d_out : i64, out r_in : i1, in %r_out : i1) {
    %q = seq.compreg %s, %clock : i64
    %s = comb.add %d_in, %q : i64
    %rin = comb.xor %r_out, %r_out : i1
    hw.output %q, %rin : i64, i1
  }
  hw.module @Pipe4(in %clock : !seq.clock, in %x : i64, in %rdy : i1, out y : i64, out rdy_o : i1) {
    %s0.d_out, %s0.r_in = hw.instance "s0" @StageMod(clock: %clock: !seq.clock, d_in: %x: i64, r_out: %s1.r_in: i1) -> (d_out: i64, r_in: i1)
    %s1.d_out, %s1.r_in = hw.instance "s1" @StageMod(clock: %clock: !seq.clock, d_in: %s0.d_out: i64, r_out: %s2.r_in: i1) -> (d_out: i64, r_in: i1)
    %s2.d_out, %s2.r_in = hw.instance "s2" @StageMod(clock: %clock: !seq.clock, d_in: %s1.d_out: i64, r_out: %s3.r_in: i1) -> (d_out: i64, r_in: i1)
    %s3.d_out, %s3.r_in = hw.instance "s3" @StageMod(clock: %clock: !seq.clock, d_in: %s2.d_out: i64, r_out: %rdy: i1) -> (d_out: i64, r_in: i1)
    %addr = comb.extract %s3.d_out from 0 : (i64) -> i7
    %m.rdata = hw.instance "m" @sram_array_1p128x88m1s1_dat(RW0_clk: %clock: !seq.clock, RW0_addr: %addr: i7) -> (RW0_rdata: i88)
    hw.output %s3.d_out, %s0.r_in : i64, i1
  }
}
"""
        mods = fir_ppa.analyze_mlir_text(mlir.splitlines(True))["modules"]
        stage_fo4 = mods["StageMod"]["fo4"]
        pipe4 = mods["Pipe4"]
        # reg2reg in Pipe4 equals a single stage's adder, NOT 4x!
        self.assertAlmostEqual(pipe4["path_groups"]["reg2reg"]["fo4"], stage_fo4, delta=1.5)
        self.assertEqual(pipe4["sram_count"], 1)
        self.assertEqual(pipe4["sram_bits"], 128 * 88)

    def test_submodule_path_prefix_preserved_and_panel_extra_modules(self):
        self.assertEqual(
            fir_ppa._norm_repo_src("XSCache/src/main/scala/coupledL2/Directory.scala"),
            "XSCache/src/main/scala/coupledL2/Directory.scala",
        )
        self.assertEqual(
            fir_ppa._norm_repo_src("/sandbox/src/main/scala/xiangshan/frontend/Bpu.scala"),
            "src/main/scala/xiangshan/frontend/Bpu.scala",
        )
        res = fir_ppa.analyze_mlir_text(SAMPLE_MLIR.splitlines(True), panel_modules=["SampleQueue"])
        self.assertEqual(list(res["modules"].keys()), ["SampleQueue"])
        self.assertIn("ChildCmp", res["extra_modules"])
        self.assertEqual(
            res["extra_modules"]["ChildCmp"]["own_source_files"],
            ["src/main/scala/xiangshan/ChildCmp.scala"],
        )


if __name__ == "__main__":
    unittest.main()

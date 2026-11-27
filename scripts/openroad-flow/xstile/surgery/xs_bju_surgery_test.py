"""xs_bju_surgery.py: the edits, the controls, and every shape it names
stopping the edit when it is not there exactly."""

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import xs_bju_surgery as bju  # noqa: E402
from xs_surgery import SurgeryError  # noqa: E402

# The modules as firtool writes them, cut to the ports and lines the
# surgery reads, with a module around them it must leave alone.
ADDR_ADD_MODULE = """module AddrAddModule(
  input  [51:0] io_pcExtend,
  input         io_taken,
  input  [31:0] io_imm,
  output [63:0] io_target,
  input  [6:0]  io_nextPcOffset
);

""" + bju.ADDR_ADD + "endmodule\n"

BRANCH_UNIT = (
    """module BranchUnit(
  input  [49:0] io_in_bits_ctrl_predictInfo_target,
  input         io_in_bits_ctrl_predictInfo_predTaken,
  input  [63:0] io_in_bits_data_src_1,
                io_in_bits_data_src_0,
                io_in_bits_data_imm,
  input  [49:0] io_in_bits_data_pc,
  output        io_out_bits_res_redirect_bits_isMisPred
);

  wire [63:0] _addModule_io_target;
  wire        _dataModule_io_taken;
  wire        _dataModule_io_mispredict;
"""
    + bju.ISMISPRED
    + bju.ADDR_ADD_USE
    + """  assign io_out_bits_res_redirect_bits_isMisPred = isMisPred;
endmodule
"""
)

JUMP_UNIT = (
    """module JumpUnit(
  input  [11:0] io_in_bits_ctrl_fuOpType,
  input  [49:0] io_in_bits_ctrl_predictInfo_target,
  input  [63:0] io_in_bits_data_src_0,
                io_in_bits_data_imm,
  input  [49:0] io_in_bits_data_pc,
  output        io_out_bits_res_redirect_valid
);

"""
    + bju.JUMP
    + """  assign io_out_bits_res_redirect_valid = targetWrong;
endmodule
"""
)

OTHER = """// header
module Other(
  input  a,
  output b
);
  // BranchUnit JumpUnit: names in another module's text are not modules
  assign b = ~a;
endmodule
"""

DESIGN = OTHER + ADDR_ADD_MODULE + BRANCH_UNIT + JUMP_UNIT


def module(text, name):
    s, e = bju.module_span(text, name)
    return text[s:e]


class Edits(unittest.TestCase):
    def test_both_units_edited_the_rest_byte_identical(self):
        out = bju.surgery(DESIGN)
        self.assertNotIn("_addModule_io_target[49:0] !=", out)
        self.assertNotIn("_jumpTarget_T_2[49:0] !=", out)
        self.assertIn("~_takenTargetHit", module(out, "BranchUnit"))
        self.assertIn("_jmpMade[48:0] == _jmpNeed[49:1]", module(out, "JumpUnit"))
        for name in ("Other", "AddrAddModule"):
            self.assertEqual(module(out, name), module(DESIGN, name))
        self.assertTrue(out.startswith(OTHER))

    def test_the_adders_stay_for_the_targets(self):
        out = bju.surgery(DESIGN)
        self.assertIn(bju.ADDR_ADD_USE, out)
        self.assertIn("_jumpTarget_T_2 =", out)

    def test_one_unit(self):
        out = bju.surgery(DESIGN, units=("BranchUnit",))
        self.assertEqual(module(out, "JumpUnit"), JUMP_UNIT)
        self.assertNotEqual(module(out, "BranchUnit"), BRANCH_UNIT)

    def test_a_second_run_stops(self):
        # the chain never applies it twice without saying so
        with self.assertRaisesRegex(SurgeryError, "isMisPred"):
            bju.surgery(bju.surgery(DESIGN))


class Controls(unittest.TestCase):
    def test_each_control_is_one_line_off_the_edit(self):
        for unit in bju.UNITS:
            good = bju.surgery(DESIGN, units=(unit,)).splitlines()
            bad = bju.surgery(DESIGN, units=(unit,), control=True).splitlines()
            self.assertEqual(len(good), len(bad))
            self.assertEqual(sum(g != b for g, b in zip(good, bad)), 1, unit)

    def test_a_control_names_its_unit(self):
        with self.assertRaisesRegex(SurgeryError, "--unit"):
            bju.surgery(DESIGN, control=True)


class Refusals(unittest.TestCase):
    """Every shape the surgery names, changed by one character, stops it."""

    def refused(self, old, new, what):
        self.assertEqual(DESIGN.count(old), 1, old)
        with self.assertRaisesRegex(SurgeryError, what):
            bju.surgery(DESIGN.replace(old, new))

    def test_the_taken_target_adder(self):
        self.refused(
            "{{37{io_imm[14]}}, io_imm[14:0]}",
            "{{38{io_imm[13]}}, io_imm[13:0]}",
            "AddrAddModule's target",
        )

    def test_the_adder_instance(self):
        self.refused(
            ".io_target       (_addModule_io_target)",
            ".io_target       (_other)",
            "BranchUnit's AddrAddModule",
        )

    def test_ismispred(self):
        self.refused(
            "& _addModule_io_target[49:0] !=",
            "& _addModule_io_target[50:1] !=",
            "isMisPred",
        )

    def test_the_jump_target(self):
        self.refused(
            ") + io_in_bits_data_imm;", ") - io_in_bits_data_imm;", "JumpUnit's target"
        )

    def test_a_port_width(self):
        self.refused(
            "  input  [49:0] io_in_bits_data_pc,\n  output        io_out_bits_res_redirect_bits_isMisPred",
            "  input  [50:0] io_in_bits_data_pc,\n  output        io_out_bits_res_redirect_bits_isMisPred",
            "io_in_bits_data_pc",
        )

    def test_a_missing_unit(self):
        with self.assertRaisesRegex(SurgeryError, "JumpUnit defined 0 times"):
            bju.surgery(OTHER + ADDR_ADD_MODULE + BRANCH_UNIT)

    def test_a_unit_defined_twice(self):
        with self.assertRaisesRegex(SurgeryError, "BranchUnit defined 2 times"):
            bju.surgery(DESIGN + BRANCH_UNIT)

    def test_a_firtool_copy(self):
        """A second BranchUnit, JumpUnit or AddrAddModule that firtool
        names <name>_<n> would keep its adder: it stops the edit."""
        for text in (ADDR_ADD_MODULE, BRANCH_UNIT, JUMP_UNIT):
            name = text[len("module ") : text.index("(")]
            copy = text.replace("module %s(" % name, "module %s_1(" % name, 1)
            with self.assertRaisesRegex(SurgeryError, "%s_1" % name):
                bju.surgery(DESIGN + copy)


if __name__ == "__main__":
    unittest.main()

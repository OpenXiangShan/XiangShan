"""substitute.py swaps an extracted module into a core and refuses a
substitution the proofs would not cover."""

import os
import subprocess
import sys
import unittest

SUBSTITUTE = os.path.join(os.path.dirname(os.path.abspath(__file__)), "substitute.py")

CORE = """module Top(input a, output y);
  Cam cam (.a(a), .y(y));
endmodule
module Cam(input a, output y);
  assign y = ~a;
endmodule
module Other(input a, output y);
  assign y = a;
endmodule
"""

EXTRACTED = """module Cam(a, y);
  input a;
  output y;
  CamEntry e0 (.in0(a), .out0(y));
endmodule
module CamEntry(in0, out0);
  input in0;
  output out0;
  assign out0 = ~in0;
endmodule
"""


class Substitute(unittest.TestCase):
    def setUp(self):
        self.work = os.environ.get("TEST_TMPDIR", os.getcwd())

    def write(self, name, text):
        path = os.path.join(self.work, name)
        with open(path, "w") as f:
            f.write(text)
        return path

    def substitute(self, core, extracted):
        out = os.path.join(self.work, "out.sv")
        r = subprocess.run(
            [sys.executable, SUBSTITUTE, self.write("core.sv", core), out]
            + [self.write("x%d.v" % i, e) for i, e in enumerate(extracted)],
            capture_output=True,
            text=True,
        )
        return r, out

    def test_swapped(self):
        r, out = self.substitute(CORE, [EXTRACTED])
        self.assertEqual(r.returncode, 0, r.stderr)
        text = open(out).read()
        self.assertEqual(text.count("module Cam("), 1)
        self.assertNotIn("assign y = ~a;", text)
        self.assertIn("module CamEntry(", text)
        self.assertIn("module Top(", text)
        self.assertIn("module Other(", text)

    def test_missing_module_refused(self):
        r, _ = self.substitute(CORE.replace("module Cam(", "module Kam("), [EXTRACTED])
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("not defined exactly once", r.stderr)

    def test_duplicate_module_refused(self):
        core = CORE + "module Cam(input a, output y);\nendmodule\n"
        r, _ = self.substitute(core, [EXTRACTED])
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("not defined exactly once", r.stderr)

    def test_entry_already_in_core_refused(self):
        core = CORE + "module CamEntry(input in0, output out0);\nendmodule\n"
        r, _ = self.substitute(core, [EXTRACTED])
        self.assertNotEqual(r.returncode, 0)
        self.assertIn("already defines CamEntry", r.stderr)


if __name__ == "__main__":
    unittest.main()

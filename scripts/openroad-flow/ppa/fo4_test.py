"""Unit tests for FO4 Liberty measurement and VT congruence."""

import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import fo4  # noqa: E402

SYNTH_LIB = """\
library (mock_asap7) {
  cell (INVx1_TEST) {
    pin (A) {
      direction : input ;
      capacitance : 0.5 ;
    }
    pin (Y) {
      direction : output ;
      timing () {
        cell_rise (t) {
          index_1 ("5.0, 25.0");
          index_2 ("1.0, 4.0");
          values ("10.0, 16.0", "14.0, 20.0");
        }
        cell_fall (t) {
          index_1 ("5.0, 25.0");
          index_2 ("1.0, 4.0");
          values ("9.0, 15.0", "13.0, 19.0");
        }
        rise_transition (t) {
          index_1 ("5.0, 25.0");
          index_2 ("1.0, 4.0");
          values ("8.0, 14.0", "12.0, 18.0");
        }
        fall_transition (t) {
          index_1 ("5.0, 25.0");
          index_2 ("1.0, 4.0");
          values ("7.0, 13.0", "11.0, 17.0");
        }
      }
    }
  }
}
"""


class Fo4Test(unittest.TestCase):
    def test_synthetic_convergence(self):
        d = fo4.measure_fo4_ps(SYNTH_LIB, "INVx1_TEST")
        self.assertGreater(d, 10.0)
        self.assertLess(d, 20.0)

    def test_real_asap7_vt_congruence(self):
        # When invoked with real ASAP7 libs via args: <RVT lib cell LVT lib cell SLVT lib cell>
        extra = [a for a in sys.argv[1:] if not a.startswith("-")]
        if len(extra) >= 9:
            triples = [tuple(extra[i : i + 3]) for i in range(0, 9, 3)]
            cal = fo4.build_fo4_calibration(triples)
            self.assertAlmostEqual(cal["fo4_ps"], 14.37, delta=0.5)
            targets = [
                fo4.XIANGSHAN_7NM_TARGET_PS
                / fo4.PUBLISHED_ASAP7_FO4_PS[vt]
                * cal["vt_fo4_ps"][vt]
                for vt, _, _ in triples
            ]
            spread = (max(targets) - min(targets)) / min(targets)
            self.assertLessEqual(spread, 0.015, targets)


if __name__ == "__main__":
    unittest.main(argv=[sys.argv[0]])

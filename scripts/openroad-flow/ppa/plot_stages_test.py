"""Unit test for `plot_stages` accuracy metrics and matplotlib rendering."""

import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import plot_stages  # noqa: E402


class PlotStagesTest(unittest.TestCase):
    def test_metrics_and_png_generation(self):
        base_grt = {"ModA": 20.0, "ModB": 35.0, "ModC": 50.0, "ModD": 65.0}
        multipliers = {
            "fir": (0.78, 0.01),
            "synth": (0.86, 2.5),
            "floorplan": (0.87, 5.0),
            "place": (0.94, 14.0),
            "cts": (0.97, 19.0),
            "grt": (1.00, 26.0),
        }
        bundles = {}
        for s, (mult, t_s) in multipliers.items():
            bundles[s] = {
                "stage": s,
                "modules": {
                    m: {"fo4": round(v * mult, 2), "elapsed_s": t_s * (i + 1)}
                    for i, (m, v) in enumerate(base_grt.items())
                },
            }
        per_mod, per_stage = plot_stages.compute_stage_metrics(bundles)
        self.assertEqual(per_stage["grt"]["median_accuracy_pct"], 100.0)
        self.assertGreater(
            per_stage["place"]["median_accuracy_pct"],
            per_stage["fir"]["median_accuracy_pct"],
        )
        self.assertAlmostEqual(per_stage["synth"]["spearman_rho"], 1.0, places=3)

        with tempfile.TemporaryDirectory() as d:
            out_png = os.path.join(d, "stages.png")
            plot_stages.render_plot(per_mod, per_stage, out_png)
            self.assertTrue(os.path.isfile(out_png))
            self.assertGreater(os.path.getsize(out_png), 10000)


if __name__ == "__main__":
    unittest.main()

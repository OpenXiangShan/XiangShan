"""`bazelisk run //:plot_stages` — Plot running time vs. accuracy of `grt` across all modules.

Builds (via Bazel cache) `fir`, `synth`, `floorplan`, `place`, `cts`, and `grt`
stage PPA bundles across all tracked XiangShan modules, computes each stage's
accuracy relative to `grt` ground-truth FO4 (`100 * (1 - |FO4_s - FO4_grt| / FO4_grt)`),
prints a stage summary table, and writes a matplotlib figure (`ppa_stages_accuracy.png`
by default, resolved against `BUILD_WORKING_DIRECTORY`).
"""

import argparse
import json
import math
import os
import statistics
import subprocess
import sys

import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt  # noqa: E402

STAGES = ("fir", "synth", "floorplan", "place", "cts", "grt")
STAGE_COLORS = {
    "fir": "#7f8c8d",
    "synth": "#2980b9",
    "floorplan": "#8e44ad",
    "place": "#e67e22",
    "cts": "#27ae60",
    "grt": "#c0392b",
}
STAGE_MARKERS = {
    "fir": "D",
    "synth": "o",
    "floorplan": "s",
    "place": "^",
    "cts": "P",
    "grt": "*",
}


def _ranks(vals):
    indexed = sorted(enumerate(vals), key=lambda kv: kv[1])
    ranks = [0.0] * len(vals)
    i = 0
    while i < len(indexed):
        j = i
        while j + 1 < len(indexed) and indexed[j + 1][1] == indexed[i][1]:
            j += 1
        avg_r = (i + j) / 2.0 + 1.0
        for k in range(i, j + 1):
            ranks[indexed[k][0]] = avg_r
        i = j + 1
    return ranks


def spearman_rho(xs, ys):
    if len(xs) < 3:
        return float("nan")
    rx = _ranks(xs)
    ry = _ranks(ys)
    mx = statistics.mean(rx)
    my = statistics.mean(ry)
    num = sum((a - mx) * (b - my) for a, b in zip(rx, ry))
    den = math.sqrt(sum((a - mx) ** 2 for a in rx) * sum((b - my) ** 2 for b in ry))
    return num / den if den > 1e-12 else 0.0


def compute_stage_metrics(stage_bundles):
    """Return `(per_module_rows, per_stage_summary)` comparing every stage against `grt`."""
    grt_mods = stage_bundles["grt"]["modules"]
    common_mods = sorted(
        m
        for m in grt_mods
        if float(grt_mods[m].get("fo4", 0.0)) > 0.0
        and all(m in stage_bundles[s]["modules"] for s in STAGES)
    )

    per_mod = {m: {} for m in common_mods}
    per_stage = {}

    for s in STAGES:
        s_mods = stage_bundles[s]["modules"]
        times = []
        accs = []
        errs = []
        xs = []
        ys = []
        for m in common_mods:
            fo4_grt = float(grt_mods[m]["fo4"])
            fo4_s = float(s_mods[m].get("fo4", 0.0))
            t_s = max(float(s_mods[m].get("elapsed_s", 0.01)), 0.005)
            rel_err = abs(fo4_s - fo4_grt) / fo4_grt * 100.0
            acc = max(0.0, 100.0 - rel_err)
            per_mod[m][s] = {
                "fo4": fo4_s,
                "fo4_grt": fo4_grt,
                "elapsed_s": t_s,
                "rel_error_pct": round(rel_err, 2),
                "accuracy_pct": round(acc, 2),
            }
            times.append(t_s)
            accs.append(acc)
            errs.append(rel_err)
            xs.append(fo4_s)
            ys.append(fo4_grt)

        per_stage[s] = {
            "stage": s,
            "n_modules": len(common_mods),
            "median_time_s": round(statistics.median(times), 3) if times else 0.0,
            "total_time_s": round(sum(times), 2),
            "median_accuracy_pct": round(statistics.median(accs), 2) if accs else 0.0,
            "mean_accuracy_pct": round(statistics.mean(accs), 2) if accs else 0.0,
            "mape_pct": round(statistics.mean(errs), 2) if errs else 0.0,
            "p25_accuracy_pct": round(
                sorted(accs)[max(0, len(accs) // 4)] if accs else 0.0, 2
            ),
            "p75_accuracy_pct": round(
                sorted(accs)[min(len(accs) - 1, (3 * len(accs)) // 4)] if accs else 0.0, 2
            ),
            "spearman_rho": round(spearman_rho(xs, ys), 3) if len(xs) >= 3 else 1.0,
        }

    return per_mod, per_stage


def render_plot(per_mod, per_stage, out_png):
    """Plot all modules' running time vs. accuracy of `grt` + stage-vs-grt FO4 scatter."""
    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(14.5, 6.2), dpi=160)

    # Panel 1: Running time (s, log scale) vs. Accuracy of grt (%)
    for m, sdict in sorted(per_mod.items()):
        tx = [sdict[s]["elapsed_s"] for s in STAGES]
        ay = [sdict[s]["accuracy_pct"] for s in STAGES]
        ax1.plot(tx, ay, color="#bdc3c7", alpha=0.40, linewidth=0.9, zorder=1)

    for s in STAGES:
        tx = [per_mod[m][s]["elapsed_s"] for m in per_mod]
        ay = [per_mod[m][s]["accuracy_pct"] for m in per_mod]
        ax1.scatter(
            tx,
            ay,
            label=f"{s} (med {per_stage[s]['median_accuracy_pct']:.1f}%, {per_stage[s]['median_time_s']:.2f}s)",
            color=STAGE_COLORS[s],
            marker=STAGE_MARKERS[s],
            s=62 if s == "grt" else 40,
            alpha=0.85,
            edgecolors="white",
            linewidths=0.4,
            zorder=3,
        )

    med_x = [max(per_stage[s]["median_time_s"], 0.005) for s in STAGES]
    med_y = [per_stage[s]["median_accuracy_pct"] for s in STAGES]
    p25_y = [per_stage[s]["p25_accuracy_pct"] for s in STAGES]
    p75_y = [per_stage[s]["p75_accuracy_pct"] for s in STAGES]

    ax1.plot(
        med_x,
        med_y,
        color="#1f2d3d",
        linewidth=2.2,
        linestyle="-",
        zorder=4,
        label="Stage median trajectory",
    )
    ax1.fill_between(med_x, p25_y, p75_y, color="#34495e", alpha=0.10, zorder=2)

    for s, x, y in zip(STAGES, med_x, med_y):
        ax1.annotate(
            s,
            (x, y),
            textcoords="offset points",
            xytext=(6, -12 if s in ("floorplan", "cts") else 7),
            fontsize=9,
            fontweight="bold",
            color=STAGE_COLORS[s],
        )

    ax1.set_xscale("log")
    ax1.set_ylim(-2, 104)
    ax1.set_xlabel("Cumulative Module Running Time (seconds, log scale)", fontsize=10.5)
    ax1.set_ylabel("Accuracy vs. Global Route FO4  [100 - |FO4 - FO4_grt| / FO4_grt %]", fontsize=10.5)
    ax1.set_title(
        f"Running Time vs. Global-Route Timing Accuracy ({len(per_mod)} XiangShan Modules)",
        fontsize=11.5,
        fontweight="bold",
    )
    ax1.grid(True, which="both", linestyle="--", alpha=0.35)
    ax1.legend(loc="lower right", fontsize=8.5, framealpha=0.92)

    # Panel 2: FO4 (stage) vs FO4 (grt) scatter across all modules
    all_grt = [per_mod[m]["grt"]["fo4_grt"] for m in per_mod]
    hi = max(all_grt + [per_mod[m][s]["fo4"] for m in per_mod for s in STAGES] + [50.0]) * 1.08
    ax2.plot([0, hi], [0, hi], color="#2c3e50", linestyle="--", linewidth=1.3, label="y = x (exact grt)")

    for s in ("fir", "synth", "floorplan", "place", "cts"):
        sx = [per_mod[m]["grt"]["fo4_grt"] for m in per_mod]
        sy = [per_mod[m][s]["fo4"] for m in per_mod]
        rho = per_stage[s]["spearman_rho"]
        mape = per_stage[s]["mape_pct"]
        ax2.scatter(
            sx,
            sy,
            color=STAGE_COLORS[s],
            marker=STAGE_MARKERS[s],
            s=38,
            alpha=0.80,
            label=f"{s}: rho={rho:.2f}, MAPE={mape:.1f}%",
        )

    ax2.set_xlim(0, hi)
    ax2.set_ylim(0, hi)
    ax2.set_xlabel("Ground-Truth Global Route Timing — FO4 (grt)", fontsize=10.5)
    ax2.set_ylabel("Earlier Stage Timing — FO4 (stage)", fontsize=10.5)
    ax2.set_title("Stage FO4 vs. Global-Route FO4 Across Modules", fontsize=11.5, fontweight="bold")
    ax2.grid(True, linestyle="--", alpha=0.35)
    ax2.legend(loc="upper left", fontsize=8.5, framealpha=0.92)

    fig.tight_layout()
    os.makedirs(os.path.dirname(os.path.abspath(out_png)), exist_ok=True)
    fig.savefig(out_png, dpi=160)
    plt.close(fig)


def build_all_stages(workspace_dir, bazelisk_bin="bazelisk"):
    targets = ["//scripts/openroad-flow/ppa:ppa_fir_bundle"] + [
        f"//scripts/openroad-flow/ppa:ppa_{s}_bundle"
        for s in ("synth", "floorplan", "place", "cts", "grt")
    ]
    r = subprocess.run([bazelisk_bin, "build"] + targets, cwd=workspace_dir)
    if r.returncode != 0:
        sys.exit(r.returncode)

    exec_root_r = subprocess.run(
        [bazelisk_bin, "info", "execution_root"],
        cwd=workspace_dir,
        capture_output=True,
        text=True,
        check=True,
    )
    exec_root = exec_root_r.stdout.strip()

    cq = subprocess.run(
        [bazelisk_bin, "cquery", "--output=files", " + ".join(targets)],
        cwd=workspace_dir,
        capture_output=True,
        text=True,
        check=True,
    )
    bundles = {}
    for line in cq.stdout.splitlines():
        p = line.strip()
        for s in STAGES:
            if p.endswith(f"ppa_{s}.json"):
                with open(os.path.join(exec_root, p), "r", encoding="utf-8") as fh:
                    bundles[s] = json.load(fh)
    missing = [s for s in STAGES if s not in bundles]
    if missing:
        sys.exit(f"plot_stages: missing stage bundles: {missing}")
    return bundles


def main(argv=None):
    ap = argparse.ArgumentParser(
        description="Plot module running time against accuracy of global route (grt)."
    )
    ap.add_argument(
        "--out",
        default="ppa_stages_accuracy.png",
        help="Output PNG path (relative to working directory; default: ppa_stages_accuracy.png).",
    )
    ap.add_argument(
        "--json-out",
        default=None,
        help="Optional JSON summary path.",
    )
    ap.add_argument("--bundles-json", nargs=6, default=None, help=argparse.SUPPRESS)
    args = ap.parse_args(argv)

    ws_dir = os.environ.get("BUILD_WORKSPACE_DIRECTORY") or os.getcwd()
    work_dir = os.environ.get("BUILD_WORKING_DIRECTORY") or os.getcwd()

    if args.bundles_json:
        bundles = {}
        for s, p in zip(STAGES, args.bundles_json):
            with open(p, "r", encoding="utf-8") as fh:
                bundles[s] = json.load(fh)
    else:
        bundles = build_all_stages(ws_dir)

    per_mod, per_stage = compute_stage_metrics(bundles)
    out_png = args.out if os.path.isabs(args.out) else os.path.join(work_dir, args.out)
    render_plot(per_mod, per_stage, out_png)

    print("=== Stage Running Time vs. Accuracy of Global Route (grt) ===")
    print(f"{'Stage':<10s}  {'Modules':>7s}  {'Med Time (s)':>12s}  {'Total (s)':>9s}  {'Med Acc (%)':>11s}  {'MAPE (%)':>8s}  {'Spearman rho':>12s}")
    print("-" * 82)
    for s in STAGES:
        st = per_stage[s]
        print(
            f"{s:<10s}  {st['n_modules']:7d}  {st['median_time_s']:12.3f}  "
            f"{st['total_time_s']:9.1f}  {st['median_accuracy_pct']:11.1f}  "
            f"{st['mape_pct']:8.1f}  {st['spearman_rho']:12.3f}"
        )
    rel_shown = os.path.relpath(out_png, work_dir) if out_png.startswith(work_dir) else out_png
    print(f"\nWrote plot: {rel_shown}")

    if args.json_out:
        jp = args.json_out if os.path.isabs(args.json_out) else os.path.join(work_dir, args.json_out)
        with open(jp, "w", encoding="utf-8") as fh:
            json.dump({"stages": per_stage, "modules": per_mod}, fh, indent=2, sort_keys=True)
            fh.write("\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())

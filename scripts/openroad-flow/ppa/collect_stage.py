"""Aggregate per-module stage PPA JSONs + ORFS stage logs + FO4 calibration.

Reads:
  --fo4 <asap7_fo4.json>
  --stage <synth|floorplan|place|cts|grt>
  --fir-json <fir_metrics.json>
  --out <ppa_<stage>.json>
  and per-module entries `--mod <name> <ppa_json> <srcmap_json> <logs_csv>`
"""

import argparse
import json
import os
import re
import sys

RE_ELAPSED = re.compile(r"Elapsed time:\s*(?:(\d+):)?(\d+):([\d.]+)\[h:\]min:sec")
RE_END_SCRIPT = re.compile(r"End of script\..*\btime:\s*([\d.]+)s")
RE_PEAK_KB = re.compile(r"Peak memory:\s*(\d+)KB")

STAGE_ORDER = {"synth": 1, "floorplan": 2, "place": 3, "cts": 4, "grt": 5}


def parse_log_stats(log_paths, max_stage_idx):
    """Return `(stage_seconds, cumulative_seconds, peak_mb)` across ORFS logs."""
    by_stage = {i: 0.0 for i in range(1, 6)}
    peak_kb = 0
    for lp in log_paths:
        if not lp or not os.path.isfile(lp):
            continue
        base = os.path.basename(lp)
        m_idx = re.match(r"^(\d+)_", base)
        if not m_idx:
            continue
        s_idx = int(m_idx.group(1))
        if s_idx > max_stage_idx or s_idx not in by_stage:
            continue
        try:
            with open(lp, "r", encoding="utf-8", errors="replace") as fh:
                txt = fh.read()
        except OSError:
            continue
        secs = 0.0
        for m in RE_ELAPSED.finditer(txt):
            h = int(m.group(1) or 0)
            mi = int(m.group(2))
            sc = float(m.group(3))
            secs = max(secs, h * 3600.0 + mi * 60.0 + sc)
        if secs == 0.0:
            for m in RE_END_SCRIPT.finditer(txt):
                secs = max(secs, float(m.group(1)))
        by_stage[s_idx] += secs
        for m in RE_PEAK_KB.finditer(txt):
            peak_kb = max(peak_kb, int(m.group(1)))
    stage_s = by_stage.get(max_stage_idx, 0.0)
    cum_s = sum(by_stage[i] for i in range(1, max_stage_idx + 1))
    return round(stage_s, 2), round(cum_s, 2), round(peak_kb / 1024.0, 1)


def resolve_endpoint_loc(pin_name, sig_locs):
    """Match an OpenSTA pin/instance name back to `src/main/scala/...:line`."""
    # Strip trailing pin name (`/D`, `/Q`, `/CLK`, `$_DFF_...`)
    clean = pin_name.replace("\\", "")
    parts = [p for p in re.split(r"[/$.]", clean) if p]
    for p in reversed(parts):
        if p in sig_locs:
            return sig_locs[p]
        base = re.sub(r"_\d+$", "", p)
        if base in sig_locs:
            return sig_locs[base]
    return ""


def main(argv=None):
    ap = argparse.ArgumentParser(description="Bundle per-module PPA JSONs for a stage.")
    ap.add_argument("--fo4", required=True)
    ap.add_argument("--stage", required=True, choices=list(STAGE_ORDER.keys()))
    ap.add_argument("--fir-json", required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument(
        "--entry",
        action="append",
        nargs=4,
        metavar=("MODULE", "PPA_JSON", "SRCMAP_JSON", "LOGS_CSV"),
        default=[],
    )
    args = ap.parse_args(argv)

    with open(args.fo4, "r", encoding="utf-8") as fh:
        fo4_cal = json.load(fh)
    fo4_ps = float(fo4_cal["fo4_ps"])

    with open(args.fir_json, "r", encoding="utf-8") as fh:
        fir_bundle = json.load(fh)
    fir_mods = fir_bundle.get("modules", {})

    max_idx = STAGE_ORDER[args.stage]
    modules_out = {}

    for mod_name, ppa_path, srcmap_path, logs_csv in args.entry:
        with open(ppa_path, "r", encoding="utf-8") as fh:
            ppa = json.load(fh)
        with open(srcmap_path, "r", encoding="utf-8") as fh:
            srcmap = json.load(fh)

        log_files = [x for x in logs_csv.split(",") if x]
        stage_s, cum_s, peak_mb = parse_log_stats(log_files, max_idx)
        if stage_s == 0.0:
            stage_s = round(float(ppa.get("probe_elapsed_s", 0.0)), 2)
        if cum_s == 0.0:
            cum_s = stage_s

        sig_locs = srcmap.get("signal_locs", {})
        pg = ppa.get("path_groups", {})
        for g_name, g_data in pg.items():
            g_data["fo4"] = round(float(g_data["period_ps"]) / fo4_ps, 2)
            g_data["wns_fo4"] = round(float(g_data["wns_ps"]) / fo4_ps, 2)
            g_data["start_loc"] = resolve_endpoint_loc(g_data.get("startpoint", ""), sig_locs)
            g_data["end_loc"] = resolve_endpoint_loc(g_data.get("endpoint", ""), sig_locs)

        for ep in ppa.get("top_endpoints", []):
            ep["fo4"] = round(float(ep["period_ps"]) / fo4_ps, 2)
            ep["loc"] = (
                resolve_endpoint_loc(ep.get("endpoint", ""), sig_locs)
                or resolve_endpoint_loc(ep.get("startpoint", ""), sig_locs)
            )

        # Headline FO4 & period: worst internal sequential cycle (`reg2reg`, `mem2reg`, `reg2mem`, `mem2mem`),
        # falling back to boundary groups (`in2reg`, `reg2out`, `in2out`) for purely combinational / single-stage modules.
        seq_groups = [g for g in ("reg2reg", "mem2reg", "reg2mem", "mem2mem") if g in pg]
        bnd_groups = [g for g in ("in2reg", "reg2out", "in2out") if g in pg]
        cand_groups = seq_groups if seq_groups else bnd_groups

        if cand_groups:
            primary_grp = max(cand_groups, key=lambda g: pg[g]["period_ps"])
            period_ps = float(pg[primary_grp]["period_ps"])
            fo4_val = round(period_ps / fo4_ps, 2)
            wns_ps = float(pg[primary_grp]["wns_ps"])
            tns_ps = float(pg[primary_grp]["tns_ps"])
            levels = int(pg[primary_grp]["levels"])
        else:
            primary_grp = "-"
            period_ps = 0.0
            fo4_val = 0.0
            wns_ps = 0.0
            tns_ps = 0.0
            levels = 0

        fir_m = fir_mods.get(mod_name, {})
        srcs = sorted(set(srcmap.get("source_files", [])) | set(fir_m.get("source_files", [])))

        modules_out[mod_name] = {
            "module": mod_name,
            "stage": args.stage,
            "primary_group": primary_grp,
            "fo4": fo4_val,
            "fo4_fir": fir_m.get("fo4"),
            "period_ps": round(period_ps, 1),
            "wns_ps": round(wns_ps, 1),
            "tns_ps": round(tns_ps, 1),
            "levels": levels,
            "flops": ppa.get("flops", 0),
            "flop_bits_fir": fir_m.get("flop_bits", 0),
            "comb_cells": ppa.get("comb_cells", 0),
            "icgs": ppa.get("icgs", 0),
            "buffers_total": ppa.get("buffers_total", 0),
            "buffers_added": ppa.get("buffers_added", 0),
            "sram_count": ppa.get("sram_count", 0),
            "sram_bits": fir_m.get("sram_bits", 0),
            "srams": fir_m.get("srams", []),
            "area_um2": ppa.get("area_um2", 0.0),
            "macro_area_um2": ppa.get("macro_area_um2", 0.0),
            "max_fanout": ppa.get("max_fanout", 0),
            "unclocked_regs": ppa.get("unclocked_regs", 0),
            "mem2mem_hazard": "mem2mem" in pg,
            "stage_elapsed_s": stage_s,
            "elapsed_s": cum_s,
            "peak_mb": peak_mb,
            "top_fanout_nets": ppa.get("top_fanout_nets", []),
            "top_endpoints": ppa.get("top_endpoints", []),
            "path_groups": pg,
            "source_files": srcs,
        }

    bundle = {
        "stage": args.stage,
        "fo4_calibration": fo4_cal,
        "fir_elapsed_s": fir_bundle.get("elapsed_s", 0.0),
        "total_stage_elapsed_s": round(sum(m["stage_elapsed_s"] for m in modules_out.values()), 2),
        "total_cumulative_elapsed_s": round(sum(m["elapsed_s"] for m in modules_out.values()), 2),
        "module_count": len(modules_out),
        "modules": modules_out,
    }
    with open(args.out, "w", encoding="utf-8") as fh:
        json.dump(bundle, fh, indent=2, sort_keys=True)
        fh.write("\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())

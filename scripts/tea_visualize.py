#!/usr/bin/env python3
"""Plot XiangShan Tea_0 samples; PICS layout follows TEA (ISCA 2023), Figs. 6/10."""

import argparse
from bisect import bisect_right
from collections import Counter, defaultdict
import csv
import json
from pathlib import Path
import re
import sqlite3
import subprocess

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
import numpy as np


EVENTS = ("DR_L1", "DR_TLB", "DR_SQ", "FL_MB", "FL_EX", "FL_MO", "ST_L1", "ST_TLB")
INCOMPLETE = {-1: "Cancelled", -2: "Unresolved", -3: "Unknown target/cause"}
STATES = ("Computing", "Stalled", "Flushed", "Drained")
SPEC_EXCLUSIONS = ((0x10000000, 0x10100000), (0x80000000, 0x80000C00))
XLEN_MASK = (1 << 64) - 1


def signature(psv):
    if psv in INCOMPLETE:
        return INCOMPLETE[psv]
    return " + ".join(event for bit, event in enumerate(EVENTS) if psv & (1 << bit)) or "No recorded event"


def target_key(row, prefix):
    return tuple(row[prefix + field] for field in ("ALLOCATIONCYCLE", "ROBINDEX", "RANK"))


def normalize_pc(pc):
    return None if pc is None else pc & XLEN_MASK


def load_data(path, label, args):
    counts, weights = defaultdict(Counter), defaultdict(Counter)
    summary = dict(label=label, database=str(path), rows=0,
                   selected_rows=0, valid_lanes=0,
                   selected_lanes=0, excluded_lanes=0, sample_weight=0.0,
                   cancelled_rows=0, unresolved_rows=0, unknown_rows=0,
                   unsupported_events="ST_LLC",
                   cycle_min=None, cycle_max=None)
    summary.update({event: 0 for event in EVENTS})
    summary.update({f"state_{state.lower()}": 0 for state in STATES})
    query, params = "SELECT * FROM Tea_0", ()
    if args.cycle_range:
        query += " WHERE CYCLE >= ? AND CYCLE < ?"
        params = tuple(args.cycle_range)
    with sqlite3.connect(path.as_uri() + "?mode=ro", uri=True) as con:
        con.row_factory = sqlite3.Row
        columns = {row["name"] for row in con.execute("PRAGMA table_info(Tea_0)")}
        width = sum(name.startswith("PCVEC_") for name in columns)
        chunks = sum(name.startswith("VALIDMASK_") for name in columns)
        if not chunks:
            raise ValueError(f"{path}: expected architectural-member VALIDMASK_n schema")
        rank_width = width // chunks
        finals = {}
        drains = []
        for final in con.execute("SELECT * FROM TeaDelayedFinal_0 ORDER BY CYCLE"):
            if final["ISDRAIN"]:
                drains.append(final)
            else:
                key = target_key(final, "MEMBER_TARGET_")
                if key in finals:
                    raise ValueError(f"{path}: duplicate final member {key}")
                finals[key] = final
        drain_cycles = [final["CYCLE"] for final in drains]

        def resolve(row):
            mask = sum(row[f"VALIDMASK_{i}"] << (i * rank_width) for i in range(chunks))
            state = row["STATE"]
            if state not in range(4) or not 0 <= mask < (1 << width) or (state == 0 and not mask):
                raise ValueError(f"{path}: invalid state/valid mask in row {row['ID']}")
            if state in (0, 2):
                return ([(normalize_pc(row[f"PCVEC_{i}"]), row[f"PSVVEC_{i}"])
                         for i in range(width) if mask & (1 << i)] or [(None, -3)])
            if state == 1:
                if not row["TARGETVALID"]:
                    return [(None, -3)]
                final = finals.get(target_key(row, "TARGET_"))
            else:
                index = bisect_right(drain_cycles, row["CYCLE"])
                final = drains[index] if index < len(drains) else None
            if final is None:
                return [(None, -2)]
            return [(normalize_pc(final["MEMBER_PC"]),
                     -1 if final["CANCELLED"] else final["MEMBER_PSV"])]

        def select(lanes):
            return [lane for lane in lanes if not args.spec_checkpoint or lane[0] is None or
                    not any(start <= lane[0] < end for start, end in SPEC_EXCLUSIONS)]

        for row in con.execute(query, params):
            state = row["STATE"]
            all_lanes = resolve(row)
            nvalid = len(all_lanes)
            summary["rows"] += 1
            summary["valid_lanes"] += nvalid
            lanes = select(all_lanes)
            summary["excluded_lanes"] += nvalid - len(lanes)
            if not lanes:
                continue
            cycle = row["CYCLE"]
            summary["cycle_min"] = cycle if summary["cycle_min"] is None else min(summary["cycle_min"], cycle)
            summary["cycle_max"] = cycle if summary["cycle_max"] is None else max(summary["cycle_max"], cycle)
            summary["selected_rows"] += 1
            summary[f"state_{STATES[state].lower()}"] += 1
            # Divide before PC exclusion: filtering must not inflate the surviving lane's weight.
            weight = 1.0 / nvalid if state == 0 else 1.0
            for pc, psv in lanes:
                if psv not in INCOMPLETE and not 0 <= psv < (1 << len(EVENTS)):
                    raise ValueError(f"{path}: invalid PSV in row {row['ID']}")
                counts[pc][psv] += 1
                weights[pc][psv] += weight
                summary["selected_lanes"] += 1
                summary["sample_weight"] += weight
                if psv < 0:
                    summary[{-1: "cancelled_rows", -2: "unresolved_rows", -3: "unknown_rows"}[psv]] += 1
                for bit, event in enumerate(EVENTS):
                    summary[event] += psv >= 0 and bool(psv & (1 << bit))

        timeline = []
        if summary["selected_rows"]:
            start, end = summary["cycle_min"], summary["cycle_max"] + 1
            nbins = min(args.bins, end - start)
            edges = [start + (end - start) * i // nbins for i in range(nbins + 1)]
            timeline = [dict(cycle_start=edges[i], cycle_end=edges[i + 1], rows=0,
                             incomplete_rows=0, lanes=0,
                             **{state: 0 for state in STATES}, **{event: 0 for event in EVENTS})
                        for i in range(nbins)]
            # A second streaming pass avoids retaining the full trace in memory.
            for row in con.execute(query, params):
                lanes = select(resolve(row))
                if not lanes:
                    continue
                i = int(np.searchsorted(edges, row["CYCLE"], side="right")) - 1
                bucket = timeline[i]
                bucket["rows"] += 1
                bucket["incomplete_rows"] += any(psv < 0 for _, psv in lanes)
                bucket["lanes"] += len(lanes)
                bucket[STATES[row["STATE"]]] += 1
                for _, psv in lanes:
                    for bit, event in enumerate(EVENTS):
                        bucket[event] += psv >= 0 and bool(psv & (1 << bit))
    return dict(summary=summary, counts=counts, weights=weights, timeline=timeline)


def write_csv(path, rows, fields):
    with path.open("w", newline="") as out:
        writer = csv.DictWriter(out, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def save_figure(fig, path):
    for suffix in (".png", ".pdf"):
        fig.savefig(path.with_suffix(suffix), dpi=180, bbox_inches="tight")
    plt.close(fig)


def category_style(runs):
    totals = Counter()
    for run in runs:
        for pc_weights in run["weights"].values():
            totals.update(pc_weights)
    categories = [0] + [psv for psv, _ in totals.most_common() if psv > 0][:7] + list(INCOMPLETE)
    labels = [signature(psv) for psv in categories] + ["Other combinations"]
    colors = ["#223b53"] + list(plt.get_cmap("tab20").colors[:len(categories) - 1]) + ["#b9bdc3"]
    return categories, labels, colors


def category_values(weights, categories):
    return [weights[psv] for psv in categories] + [sum(value for psv, value in weights.items()
                                                        if psv not in categories)]


def annotate_elf(path, pcs, objdump):
    if path is None:
        return {}
    result = subprocess.run([objdump, "-d", str(path)], check=True, capture_output=True, text=True)
    instructions = {}
    for line in result.stdout.splitlines():
        match = re.match(r"^\s*([0-9a-fA-F]+):\s+[0-9a-fA-F]+\s+(.+)$", line)
        if match and int(match[1], 16) in pcs:
            instructions[int(match[1], 16)] = " ".join(match[2].split())
    return instructions


def plot_pics(run, directory, categories, labels, colors, args, elf):
    weights = run["weights"]
    pcs = sorted(weights, key=lambda pc: (-sum(weights[pc].values()), pc or 0))[:args.top]
    instructions = annotate_elf(elf, set(weights) - {None}, args.objdump)
    fig, ax = plt.subplots(figsize=(13, max(5.5, 2.5 + 0.4 * len(pcs))))
    left = np.zeros(len(pcs))
    values = np.array([category_values(weights[pc], categories) for pc in pcs])
    for i, (label, color) in enumerate(zip(labels, colors)):
        component = values[:, i] if pcs else np.array([])
        ax.barh(range(len(pcs)), component, left=left, label=label, color=color)
        left += component
    ax.set_yticks(range(len(pcs)), [f"{pc:#x}  {instructions.get(pc, '')}".rstrip()
                                  if pc is not None else "Unresolved / unknown PC" for pc in pcs], fontsize=9)
    ax.invert_yaxis()
    ax.set_xlabel("Captured sample equivalents")
    ax.set_title("Each Computing sample shares weight 1 across all valid lanes; other states have weight 1", fontsize=9)
    ax.grid(axis="x", alpha=0.2)
    ax.set_axisbelow(True)
    if not pcs:
        ax.text(0.5, 0.5, "No samples in selected scope", ha="center", transform=ax.transAxes)
    summary = run["summary"]
    fig.suptitle(f"Captured PICS — {summary['label']} (TEA paper Fig. 6 / Fig. 10 layout)")
    fig.legend(*ax.get_legend_handles_labels(), loc="lower center", bbox_to_anchor=(0.5, 0.03), ncol=3, fontsize=8)
    fig.text(0.5, 0.005, f"{summary['selected_rows']} selected boundaries; incomplete targets retain their weight. PSV=0 means no recorded event.", ha="center", fontsize=8)
    fig.tight_layout(rect=(0, 0.15, 1, 0.94))
    save_figure(fig, directory / "pics")
    # Keep all PCs and exact PSV combinations in CSV, including categories grouped as 'Other'.
    rows = [dict(pc=hex(pc) if pc is not None else "unknown", instruction=instructions.get(pc, ""), psv=psv,
                 signature=signature(psv), lane_occurrences=run["counts"][pc][psv], sample_weight=weight)
            for pc in sorted(weights, key=lambda pc: (-sum(weights[pc].values()), pc or 0))
            for psv, weight in sorted(weights[pc].items())]
    write_csv(directory / "hotspots.csv", rows,
              ("pc", "instruction", "psv", "signature", "lane_occurrences", "sample_weight"))


def plot_comparison(runs, output, categories, labels, colors):
    fig, axes = plt.subplots(2, 1, figsize=(12, 8.5))
    x = np.arange(len(runs))
    bottom = np.zeros(len(runs))
    totals = []
    for run in runs:
        total = Counter()
        for weights in run["weights"].values():
            total.update(weights)
        totals.append(category_values(total, categories))
    values = np.array(totals)
    values = 100 * values / np.maximum(1e-12, values.sum(axis=1, keepdims=True))
    for i, (label, color) in enumerate(zip(labels, colors)):
        axes[0].bar(x, values[:, i], bottom=bottom, color=color, label=label, width=0.6)
        bottom += values[:, i]
    slice_labels = [run["summary"]["label"] for run in runs]
    axes[0].set_xticks(x, slice_labels)
    axes[0].set_xlim(-0.5, len(runs) - 0.5)
    axes[0].set_ylim(0, 100)
    axes[0].set_ylabel("% of captured sample weight")
    axes[0].set_title("Exclusive PSV combinations and incomplete attribution", fontsize=10)
    rates = np.array([[100 * run["summary"][event] / max(1, run["summary"]["selected_lanes"])
                       for run in runs] for event in EVENTS])
    empty = np.array([run["summary"]["selected_lanes"] == 0 for run in runs])
    masked_rates = np.ma.array(rates, mask=np.broadcast_to(empty, rates.shape))
    heatmap = axes[1].imshow(masked_rates, aspect="auto", cmap="Blues", vmin=0, vmax=100)
    axes[1].set_facecolor("#e0e0e0")
    axes[1].set_xticks(x, slice_labels)
    axes[1].set_yticks(range(len(EVENTS)), EVENTS)
    axes[1].set_title("PSV bit occurrences / selected attributions (bits may overlap)", fontsize=10)
    for i in range(len(EVENTS)):
        for j in range(len(runs)):
            axes[1].text(j, i, "N/A" if empty[j] else f"{rates[i, j]:.1f}%", ha="center", va="center", fontsize=8,
                         color="white" if rates[i, j] > 55 else "black")
    for j in np.flatnonzero(empty):
        axes[0].text(j, 50, "No samples", ha="center", fontsize=9)
    fig.suptitle("TEA sample composition across slices")
    fig.legend(*axes[0].get_legend_handles_labels(), loc="upper center", bbox_to_anchor=(0.5, 0.96), ncol=3, fontsize=8)
    quality = [f"{s['label']}: cancelled {s['cancelled_rows']}, unresolved {s['unresolved_rows']}, unknown {s['unknown_rows']}"
               for s in (run["summary"] for run in runs)]
    fig.text(0.5, 0.01, "Incomplete attribution (included): " + "\n".join(quality) + "\nST_LLC is unsupported.", ha="center", fontsize=8)
    fig.subplots_adjust(left=0.1, right=0.9, bottom=0.12 + 0.017 * len(runs), top=0.83, hspace=0.3)
    fig.colorbar(heatmap, ax=axes, label="% of selected attributions", fraction=0.025, pad=0.02)
    save_figure(fig, output / "comparison")


def plot_timeline(run, directory):
    timeline = run["timeline"]
    fig, axes = plt.subplots(2, 1, figsize=(12, 6.5), sharex=True)
    if timeline:
        lane_counts = np.array([row["lanes"] for row in timeline])
        rates = np.array([[100 * row[event] / max(1, row["lanes"]) for row in timeline] for event in EVENTS])
        rates = np.ma.array(rates, mask=np.broadcast_to(lane_counts == 0, rates.shape))
        edges = [row["cycle_start"] for row in timeline] + [timeline[-1]["cycle_end"]]
        heatmap = axes[0].pcolormesh(edges, np.arange(len(EVENTS) + 1), rates, cmap="Blues", vmin=0, vmax=100)
        axes[0].set_facecolor("#e0e0e0")
        axes[0].set_yticks(np.arange(len(EVENTS)) + 0.5, EVENTS)
        axes[0].invert_yaxis()
        fig.colorbar(heatmap, ax=axes, label="% of selected attributions", fraction=0.025, pad=0.02)
        row_counts = np.array([row["rows"] for row in timeline])
        denominator = np.maximum(1, row_counts)
        bottom = np.zeros(len(timeline))
        for state, color in zip(STATES, ("#517ca4", "#cc983b", "#986ab8", "#71a988")):
            share = 100 * np.array([row[state] for row in timeline]) / denominator
            axes[1].bar(edges[:-1], share, bottom=bottom, width=np.diff(edges), align="edge", label=state, color=color)
            bottom += share
        incomplete = 100 * np.array([row["incomplete_rows"] for row in timeline]) / denominator
        centers = (np.array(edges[:-1]) + np.array(edges[1:])) / 2
        axes[1].plot(centers, np.where(row_counts, incomplete, np.nan), "--", color="#aa3030", label="Incomplete attribution")
        axes[1].legend(loc="upper center", bbox_to_anchor=(0.5, -0.25), ncol=5, fontsize=8)
        axes[1].set_ylim(0, 100)
    else:
        axes[0].text(0.5, 0.5, "No samples in selected scope", ha="center", transform=axes[0].transAxes)
    axes[0].set_title("PSV bit occurrences; gray bins contain no selected lanes", fontsize=9)
    axes[1].set_title("State and incomplete-attribution fractions among selected boundaries", fontsize=9)
    axes[1].set_ylabel("% of selected rows")
    axes[1].set_xlabel("Sampling boundary (CYCLE)")
    axes[1].ticklabel_format(axis="x", style="plain", useOffset=False)
    fig.suptitle(f"TEA sampled timeline — {run['summary']['label']}")
    fig.text(0.5, 0.005, "CYCLE is the original sampling boundary. STAMP is not used. Row fractions are not cycle occupancy.", ha="center", fontsize=8)
    fig.subplots_adjust(left=0.09, right=0.86, top=0.88, bottom=0.2, hspace=0.3)
    save_figure(fig, directory / "timeline")
    write_csv(directory / "timeline.csv", timeline,
              ("cycle_start", "cycle_end", "rows", "incomplete_rows", "lanes", *STATES, *EVENTS))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("databases", nargs="+", type=Path, help="Tea_0 SQLite databases (read-only)")
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--labels", nargs="+", help="one display label per database; default: parent directory")
    parser.add_argument("--spec-checkpoint", action="store_true", help="exclude this SPEC archive's flash/restorer PC ranges")
    parser.add_argument("--cycle-range", nargs=2, type=int, metavar=("START", "END"), help="CYCLE range [START, END)")
    parser.add_argument("--top", type=int, default=12, help="number of PCs in each PICS figure")
    parser.add_argument("--bins", type=int, default=40, help="number of timeline bins")
    parser.add_argument("--elf", nargs="+", help="matching ELF per database, or '-' to skip assembly annotation")
    parser.add_argument("--objdump", default="riscv64-unknown-linux-gnu-objdump")
    args = parser.parse_args()
    for name in ("labels", "elf"):
        if getattr(args, name) is not None and len(getattr(args, name)) != len(args.databases):
            parser.error(f"--{name} needs one value per database")
    if args.top <= 0 or args.bins <= 0:
        parser.error("--top and --bins must be positive")
    if args.cycle_range and not 0 <= args.cycle_range[0] < args.cycle_range[1]:
        parser.error("--cycle-range needs 0 <= START < END")
    paths = [path.expanduser().resolve() for path in args.databases]
    labels = args.labels or [path.parent.name for path in paths]
    elfs = [None if path == "-" else Path(path).expanduser().resolve() for path in args.elf] if args.elf else [None] * len(paths)
    runs = [load_data(path, label, args) for path, label in zip(paths, labels)]
    output = args.output.expanduser().resolve()
    output.mkdir(parents=True, exist_ok=True)
    summaries = [run["summary"] for run in runs]
    write_csv(output / "summary.csv", summaries, list(summaries[0]))
    (output / "summary.json").write_text(json.dumps(dict(
        unit="captured sample equivalents", spec_checkpoint=args.spec_checkpoint,
        cycle_range=args.cycle_range, slices=summaries), indent=2) + "\n")
    categories, category_labels, colors = category_style(runs)
    plot_comparison(runs, output, categories, category_labels, colors)
    for i, (run, elf) in enumerate(zip(runs, elfs), 1):
        slug = re.sub(r"[^A-Za-z0-9_.-]+", "_", run["summary"]["label"])
        directory = output / f"{i:02d}_{slug}"
        directory.mkdir(exist_ok=True)
        plot_pics(run, directory, categories, category_labels, colors, args, elf)
        plot_timeline(run, directory)
        print(f"{run['summary']['label']}: {run['summary']['selected_rows']} rows, "
              f"{run['summary']['selected_lanes']} lanes -> {directory}")
    print(f"Comparison and summaries: {output}")


if __name__ == "__main__":
    main()

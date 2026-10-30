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
COMMIT_BLOCKS = ("misPredBlock", "lastCycleFlush", "WFI", "redirect", "deqNeedFlush",
                 "deqFlushBlock", "criticalError", "traceBlock", "walk")
# Decode numeric Top-down IDs using the matching checkout, not a second enum copy.
_reason_source = Path(__file__).resolve().parents[1] / "src/main/scala/xiangshan/package.scala"
_reason_enum = (_reason_source.read_text().partition("object TopDownCounters extends Enumeration {")[2]
                if _reason_source.exists() else "")
HEAD_REASONS = re.findall(r'^\s*val \w+ = Value\("([^"\n]+)"\)', _reason_enum, re.MULTILINE)


SPEC_PERF = re.compile(r"\[PERF\s*\]\[time=\s*(\d+)\]\s+[^:]+\.rob:\s*(clock_cycle|commitInstr),\s*(\d+)")


def read_spec_roi(directory):
    output = (directory / "simulator_out.txt").read_text()
    if not any(marker in output for marker in ("EXCEEDING CYCLE/INSTR LIMIT", "HIT GOOD TRAP")):
        raise ValueError(f"{directory}: checkpoint did not finish successfully")
    counters = {"clock_cycle": [], "commitInstr": []}
    with (directory / "simulator_err.txt").open() as stream:
        for line in stream:
            match = SPEC_PERF.search(line)
            if match:
                stamp, name, value = match.groups()
                counters[name].append((int(stamp), int(value)))
    if any(len(values) != 2 for values in counters.values()):
        raise ValueError(f"{directory}: expected warmup and final ROB counter dumps")
    warm, final = counters["clock_cycle"]
    start, end = warm[1], final[0] + 1
    if (warm[1] != warm[0] + 1 or not start < end or final[1] != end - start
            or [stamp for stamp, _ in counters["commitInstr"]] != [warm[0], final[0]]):
        raise ValueError(f"{directory}: inconsistent counter timestamps or reset boundary")
    warm_instructions, instructions = [value for _, value in counters["commitInstr"]]
    if not (20_000_000 <= warm_instructions < 20_100_000
            and 19_900_000 <= instructions < 20_100_000):
        raise ValueError(f"{directory}: counters do not match the default SPEC 20M warmup / 40M total window")
    return (start, end), instructions, final[1]


def sampling_coverage(path, cycle_range, sample_period):
    start, end = cycle_range
    first = ((start + sample_period - 1) // sample_period) * sample_period
    expected = max(0, (end - 1 - first) // sample_period + 1)
    with sqlite3.connect(path.as_uri() + "?mode=ro", uri=True) as con:
        rows, unique, off_grid, grid_unique, low, high = con.execute(
            "SELECT count(*), count(DISTINCT CYCLE), coalesce(sum(CYCLE % ? != 0), 0), "
            "count(DISTINCT CASE WHEN CYCLE % ? = 0 THEN CYCLE END), "
            "min(CASE WHEN CYCLE % ? = 0 THEN CYCLE END), "
            "max(CASE WHEN CYCLE % ? = 0 THEN CYCLE END) "
            "FROM Tea_0 WHERE CYCLE >= ? AND CYCLE < ?",
            (sample_period, sample_period, sample_period, sample_period, start, end)).fetchone()
    span = (high - low) // sample_period + 1 if low is not None else 0
    return dict(sample_period=sample_period, expected_sample_boundaries=expected,
                observed_sample_rows=rows, duplicate_sample_rows=rows - unique,
                off_grid_sample_rows=off_grid, missing_interior_samples=span - grid_unique,
                missing_endpoint_samples=expected - span)


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
    contexts = Counter()
    state_weights = {state: defaultdict(Counter) for state in STATES}
    summary = dict(label=label, database=str(path), rows=0,
                   selected_rows=0, valid_lanes=0,
                   selected_lanes=0, excluded_lanes=0, sample_weight=0.0, excluded_weight=0.0,
                   cancelled_rows=0, unresolved_rows=0, unknown_rows=0,
                   unsupported_events="ST_LLC",
                   cycle_min=None, cycle_max=None)
    summary.update({event: 0 for event in EVENTS})
    summary.update({event + "_weight": 0.0 for event in EVENTS})
    summary.update({name + "_weight": 0.0 for name in ("cancelled", "unresolved", "unknown")})
    summary.update({f"state_{state.lower()}": 0 for state in STATES})
    summary.update({f"state_{state.lower()}_weight": 0.0 for state in STATES})
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
            # Divide before PC exclusion: filtering must not inflate the surviving lane's weight.
            weight = 1.0 / nvalid if state == 0 else 1.0
            summary["rows"] += 1
            summary["valid_lanes"] += nvalid
            lanes = select(all_lanes)
            summary["excluded_lanes"] += nvalid - len(lanes)
            summary["excluded_weight"] += weight * (nvalid - len(lanes))
            if not lanes:
                continue
            cycle = row["CYCLE"]
            summary["cycle_min"] = cycle if summary["cycle_min"] is None else min(summary["cycle_min"], cycle)
            summary["cycle_max"] = cycle if summary["cycle_max"] is None else max(summary["cycle_max"], cycle)
            summary["selected_rows"] += 1
            summary[f"state_{STATES[state].lower()}"] += 1
            for pc, psv in lanes:
                if psv not in INCOMPLETE and not 0 <= psv < (1 << len(EVENTS)):
                    raise ValueError(f"{path}: invalid PSV in row {row['ID']}")
                counts[pc][psv] += 1
                weights[pc][psv] += weight
                state_weights[STATES[state]][pc][psv] += weight
                if "CONTEXT_COMMITBLOCKMASK" in columns:
                    mask = row["CONTEXT_COMMITBLOCKMASK"]
                    reason = row["CONTEXT_HEADREASON"] if row["CONTEXT_HEADREASONVALID"] else -1
                    contexts[(pc, state, mask, reason, row["CONTEXT_HEADREADY"])] += weight
                summary["selected_lanes"] += 1
                summary["sample_weight"] += weight
                summary[f"state_{STATES[state].lower()}_weight"] += weight
                if psv < 0:
                    cause = {-1: "cancelled", -2: "unresolved", -3: "unknown"}[psv]
                    summary[cause + "_rows"] += 1
                    summary[cause + "_weight"] += weight
                for bit, event in enumerate(EVENTS):
                    present = psv >= 0 and bool(psv & (1 << bit))
                    summary[event] += present
                    summary[event + "_weight"] += weight * present

        timeline = []
        if summary["selected_rows"]:
            start, end = summary["cycle_min"], summary["cycle_max"] + 1
            nbins = min(args.bins, end - start)
            edges = [start + (end - start) * i // nbins for i in range(nbins + 1)]
            timeline = [dict(cycle_start=edges[i], cycle_end=edges[i + 1], rows=0,
                             incomplete_rows=0, lanes=0,
                             sample_weight=0.0, incomplete_weight=0.0,
                             **{state: 0 for state in STATES}, **{event: 0 for event in EVENTS},
                             **{state + "_weight": 0.0 for state in STATES},
                             **{event + "_weight": 0.0 for event in EVENTS})
                        for i in range(nbins)]
            # A second streaming pass avoids retaining the full trace in memory.
            for row in con.execute(query, params):
                all_lanes = resolve(row)
                lanes = select(all_lanes)
                if not lanes:
                    continue
                i = int(np.searchsorted(edges, row["CYCLE"], side="right")) - 1
                bucket = timeline[i]
                bucket["rows"] += 1
                bucket["incomplete_rows"] += any(psv < 0 for _, psv in lanes)
                bucket["lanes"] += len(lanes)
                bucket[STATES[row["STATE"]]] += 1
                weight = 1.0 / len(all_lanes) if row["STATE"] == 0 else 1.0
                bucket["sample_weight"] += weight * len(lanes)
                bucket[STATES[row["STATE"]] + "_weight"] += weight * len(lanes)
                for _, psv in lanes:
                    bucket["incomplete_weight"] += weight * (psv < 0)
                    for bit, event in enumerate(EVENTS):
                        present = psv >= 0 and bool(psv & (1 << bit))
                        bucket[event] += present
                        bucket[event + "_weight"] += weight * present
    return dict(summary=summary, counts=counts, weights=weights,
                state_weights=state_weights, timeline=timeline, contexts=contexts)


def write_csv(path, rows, fields):
    with path.open("w", newline="") as out:
        writer = csv.DictWriter(out, fieldnames=fields)
        writer.writeheader()
        writer.writerows(rows)


def export_cycle_stats(path, directory):
    """Export exact cumulative counters separately from PC/cycle-filtered samples."""
    with sqlite3.connect(path.as_uri() + "?mode=ro", uri=True) as con:
        con.row_factory = sqlite3.Row
        tables = {row[0] for row in con.execute("SELECT name FROM sqlite_master WHERE type='table'")}
        if "TeaCycleSummary_0" not in tables:
            return
        row = con.execute("SELECT * FROM TeaCycleSummary_0 ORDER BY CYCLE DESC, ID DESC LIMIT 1").fetchone()
        if row is None:
            (directory / "cycle_summary.json").write_text(json.dumps(dict(
                available=False, reason="No snapshot: collection was disabled or the table was not captured"),
                indent=2) + "\n")
        else:
            states = {state: row[f"STATES_{i}"] for i, state in enumerate(STATES)}
            cycles, retired = sum(states.values()), row["RETIRED"]
            result = dict(available=True, scope="All enabled cycles through observed_end; not PC/cycle filtered",
                          observed_end=row["CYCLE"], collection_closed=bool(row["CLOSED"]),
                          cycles=cycles, retired=retired, states=states,
                          ipc=retired / cycles if cycles else None,
                          cycles_per_kinst={state: count * 1000 / retired if retired else None
                                            for state, count in states.items()},
                          complete_gap_histogram={name: row[f"GAPS_{i}"] for i, name in
                                                  enumerate(("1-7", "8-31", "32-127", "128-511", "512+"))},
                          censored_gaps=row["CENSOREDGAPS"], pending_gap_cycles=row["PENDING"])
            (directory / "cycle_summary.json").write_text(json.dumps(result, indent=2) + "\n")
        if "TeaNoCommit_0" in tables:
            def intervals():
                for row in con.execute("SELECT * FROM TeaNoCommit_0 ORDER BY START"):
                    length = row["END"] - row["START"]
                    states = {state: row[f"STATES_{i}"] for i, state in enumerate(STATES[1:])}
                    if length <= 0 or sum(states.values()) != length:
                        raise ValueError(f"{path}: inconsistent no-commit interval at {row['START']}")
                    yield dict(start=row["START"], end=row["END"], cycles=length,
                               left_censored=row["LEFTCENSORED"], right_censored=row["RIGHTCENSORED"], **states)
            write_csv(directory / "no_commit_intervals.csv", intervals(),
                      ("start", "end", "cycles", "left_censored", "right_censored", *STATES[1:]))


def export_context(run, directory):
    rows = []
    for (pc, state, mask, reason, ready), weight in run["contexts"].most_common():
        blocks = [name for bit, name in enumerate(COMMIT_BLOCKS) if mask & (1 << bit)]
        head_reason = (HEAD_REASONS[reason] if 0 <= reason < len(HEAD_REASONS)
                       else f"reason_{reason}" if reason >= 0 else "Unknown")
        # Context on Computing samples is not charged as a stall.
        primary = "Computing" if state == 0 else blocks[0] if blocks else head_reason
        scope = "None" if state == 0 else "Global" if blocks else "Entry" if reason >= 0 else "Unknown"
        rows.append(dict(pc=hex(pc) if pc is not None else "unknown", state=STATES[state],
                         primary=primary, scope=scope, commit_block_mask=mask,
                         commit_blocks="|".join(blocks), head_reason_id=reason,
                         head_reason=head_reason, head_ready=ready, sample_weight=weight))
    write_csv(directory / "head_context.csv", rows,
              ("pc", "state", "primary", "scope", "commit_block_mask", "commit_blocks",
               "head_reason_id", "head_reason", "head_ready", "sample_weight"))


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
    state_rows = [dict(pc=hex(pc) if pc is not None else "unknown",
                       instruction=instructions.get(pc, ""), state=state, psv=psv,
                       signature=signature(psv), sample_weight=weight,
                       selected_weight_pct=100 * weight / summary["sample_weight"],
                       pc_weight_pct=100 * weight / sum(weights[pc].values()))
                  for state, mapping in run["state_weights"].items()
                  for pc in sorted(mapping, key=lambda pc: (-sum(weights[pc].values()), pc or 0))
                  for psv, weight in sorted(mapping[pc].items())]
    fields = ["pc", "instruction", "state", "psv", "signature", "sample_weight",
              "selected_weight_pct", "pc_weight_pct"]
    if "measured_instructions" in summary and "sample_period" in summary:
        fields.append("sampled_cycles_per_ki")
        for row in state_rows:
            row["sampled_cycles_per_ki"] = (row["sample_weight"] * summary["sample_period"]
                                           * 1000 / summary["measured_instructions"])
    write_csv(directory / "hotspot_states.csv", state_rows, fields)


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
    rates = np.array([[100 * run["summary"][event + "_weight"] / max(1e-12, run["summary"]["sample_weight"])
                       for run in runs] for event in EVENTS])
    empty = np.array([run["summary"]["sample_weight"] == 0 for run in runs])
    masked_rates = np.ma.array(rates, mask=np.broadcast_to(empty, rates.shape))
    heatmap = axes[1].imshow(masked_rates, aspect="auto", cmap="Blues", vmin=0, vmax=100)
    axes[1].set_facecolor("#e0e0e0")
    axes[1].set_xticks(x, slice_labels)
    axes[1].set_yticks(range(len(EVENTS)), EVENTS)
    axes[1].set_title("PSV signature sample weights (bits may overlap)", fontsize=10)
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
    fig.colorbar(heatmap, ax=axes, label="% of selected sample weight", fraction=0.025, pad=0.02)
    save_figure(fig, output / "comparison")
    if all("measured_instructions" in run["summary"] and "sample_period" in run["summary"]
           for run in runs):
        plot_sampled_costs(runs, output)


def plot_sampled_costs(runs, output):
    """Compare captured time per retired instruction without renormalizing missing samples."""
    rows = []
    for run in runs:
        summary = run["summary"]
        scale = summary["sample_period"] * 1000 / summary["measured_instructions"]
        components = [("state", state, summary["state_" + state.lower() + "_weight"])
                      for state in STATES]
        components += [("state", "Excluded PC", summary["excluded_weight"])]
        components += [("event", event, summary[event + "_weight"]) for event in EVENTS]
        components += [("incomplete", name, summary[name.lower() + "_weight"])
                       for name in ("Cancelled", "Unresolved", "Unknown")]
        rows.extend(dict(label=summary["label"], kind=kind, name=name, sample_weight=weight,
                         sampled_cycles_per_ki=weight * scale,
                         measured_instructions=summary["measured_instructions"],
                         measured_cycles=summary["measured_cycles"], cpi=summary["cpi"],
                         sample_period=summary["sample_period"])
                    for kind, name, weight in components)
    write_csv(output / "sampled_costs.csv", rows, list(rows[0]))
    fig, axes = plt.subplots(2, 1, figsize=(12, 8.5))
    x = np.arange(len(runs))
    bottom = np.zeros(len(runs))
    for name, color in zip((*STATES, "Excluded PC"),
                           ("#517ca4", "#cc983b", "#986ab8", "#71a988", "#aaaaaa")):
        values = [row["sampled_cycles_per_ki"] for row in rows
                  if row["kind"] == "state" and row["name"] == name]
        axes[0].bar(x, values, bottom=bottom, color=color, label=name, width=0.6)
        bottom += values
    measured = [1000 * run["summary"]["cpi"] for run in runs]
    axes[0].scatter(x, measured, color="black", marker="x", label="Measured total")
    axes[0].set_ylim(0, 1.1 * max(*bottom, *measured))
    slice_labels = [run["summary"]["label"] for run in runs]
    axes[0].set_xticks(x, slice_labels)
    axes[0].set_ylabel("Sampled cycle equivalents / 1k instructions")
    axes[0].set_title("State costs; measured total includes unsampled endpoints", fontsize=10)
    axes[0].legend(ncol=3, fontsize=8)
    rates = np.array([[row["sampled_cycles_per_ki"] for row in rows
                       if row["kind"] == "event" and row["name"] == event]
                      for event in EVENTS])
    heatmap = axes[1].imshow(rates, aspect="auto", cmap="Blues", vmin=0)
    axes[1].set_xticks(x, slice_labels)
    axes[1].set_yticks(range(len(EVENTS)), EVENTS)
    axes[1].set_title("PSV costs overlap; they are not additive stall cycles", fontsize=10)
    for i in range(len(EVENTS)):
        for j in range(len(runs)):
            axes[1].text(j, i, f"{rates[i, j]:.1f}", ha="center", va="center", fontsize=8,
                         color="white" if rates[i, j] > rates.max() * 0.55 else "black")
    fig.colorbar(heatmap, ax=axes[1], label="Sampled cycle equivalents / 1k instructions",
                 fraction=0.025, pad=0.02)
    fig.suptitle("TEA captured time per retired instruction")
    fig.text(0.5, 0.005, "Captured weight × sample period; no missing-sample correction. "
             "These are attribution estimates, not recoverable cycles.", ha="center", fontsize=8)
    fig.tight_layout(rect=(0, 0.035, 1, 0.95))
    save_figure(fig, output / "sampled_costs")


def write_deltas(runs, output):
    """Compare each candidate with the first run using independently measured ROI sizes."""
    baseline = runs[0]
    cost_rows, hotspot_rows = [], []

    def costs(summary):
        scale = summary["sample_period"] * 1000 / summary["measured_instructions"]
        values = {("state", state): summary["state_" + state.lower() + "_weight"] * scale
                  for state in STATES}
        values[("state", "Excluded PC")] = summary["excluded_weight"] * scale
        values.update({("event", event): summary[event + "_weight"] * scale for event in EVENTS})
        values.update({("incomplete", name): summary[name.lower() + "_weight"] * scale
                       for name in ("Cancelled", "Unresolved", "Unknown")})
        values[("measured", "Total")] = summary["cpi"] * 1000
        values[("sampling", "Measured minus captured")] = (
            summary["cpi"] * 1000 - (summary["sample_weight"] + summary["excluded_weight"]) * scale)
        return values

    baseline_costs = costs(baseline["summary"])
    for candidate in runs[1:]:
        names = dict(baseline=baseline["summary"]["label"], candidate=candidate["summary"]["label"])
        for (kind, name), value in costs(candidate["summary"]).items():
            previous = baseline_costs[(kind, name)]
            cost_rows.append(dict(**names, kind=kind, name=name,
                                  baseline_cycles_per_ki=previous, candidate_cycles_per_ki=value,
                                  delta_cycles_per_ki=value - previous))
        sides = []
        for run in (baseline, candidate):
            scale = run["summary"]["sample_period"] * 1000 / run["summary"]["measured_instructions"]
            sides.append({(pc, state, psv): weight * scale
                          for state, mapping in run["state_weights"].items()
                          for pc, combinations in mapping.items() for psv, weight in combinations.items()})
        for pc, state, psv in sides[0].keys() | sides[1].keys():
            previous, value = [side.get((pc, state, psv), 0.0) for side in sides]
            hotspot_rows.append(dict(**names, pc="Unknown" if pc is None else f"0x{pc:x}",
                                     state=state, psv=psv, signature=signature(psv),
                                     baseline_sampled_cycles_per_ki=previous,
                                     candidate_sampled_cycles_per_ki=value,
                                     delta_sampled_cycles_per_ki=value - previous))
    hotspot_rows.sort(key=lambda row: (-abs(row["delta_sampled_cycles_per_ki"]),
                                      row["candidate"], row["pc"], row["state"], row["psv"]))
    write_csv(output / "sampled_cost_deltas.csv", cost_rows,
              ["baseline", "candidate", "kind", "name", "baseline_cycles_per_ki",
               "candidate_cycles_per_ki", "delta_cycles_per_ki"])
    write_csv(output / "hotspot_deltas.csv", hotspot_rows,
              ["baseline", "candidate", "pc", "state", "psv", "signature",
               "baseline_sampled_cycles_per_ki", "candidate_sampled_cycles_per_ki",
               "delta_sampled_cycles_per_ki"])


def plot_timeline(run, directory):
    timeline = run["timeline"]
    fig, axes = plt.subplots(2, 1, figsize=(12, 6.5), sharex=True)
    if timeline:
        sample_weights = np.array([row["sample_weight"] for row in timeline])
        rates = np.array([[100 * row[event + "_weight"] / max(1e-12, row["sample_weight"])
                           for row in timeline] for event in EVENTS])
        rates = np.ma.array(rates, mask=np.broadcast_to(sample_weights == 0, rates.shape))
        edges = [row["cycle_start"] for row in timeline] + [timeline[-1]["cycle_end"]]
        heatmap = axes[0].pcolormesh(edges, np.arange(len(EVENTS) + 1), rates, cmap="Blues", vmin=0, vmax=100)
        axes[0].set_facecolor("#e0e0e0")
        axes[0].set_yticks(np.arange(len(EVENTS)) + 0.5, EVENTS)
        axes[0].invert_yaxis()
        fig.colorbar(heatmap, ax=axes, label="% of selected sample weight", fraction=0.025, pad=0.02)
        denominator = np.maximum(1e-12, sample_weights)
        bottom = np.zeros(len(timeline))
        for state, color in zip(STATES, ("#517ca4", "#cc983b", "#986ab8", "#71a988")):
            share = 100 * np.array([row[state + "_weight"] for row in timeline]) / denominator
            axes[1].bar(edges[:-1], share, bottom=bottom, width=np.diff(edges), align="edge", label=state, color=color)
            bottom += share
        incomplete = 100 * np.array([row["incomplete_weight"] for row in timeline]) / denominator
        centers = (np.array(edges[:-1]) + np.array(edges[1:])) / 2
        axes[1].plot(centers, np.where(sample_weights, incomplete, np.nan), "--", color="#aa3030", label="Incomplete attribution")
        axes[1].legend(loc="upper center", bbox_to_anchor=(0.5, -0.25), ncol=5, fontsize=8)
        axes[1].set_ylim(0, 100)
    else:
        axes[0].text(0.5, 0.5, "No samples in selected scope", ha="center", transform=axes[0].transAxes)
    axes[0].set_title("PSV signature sample weights; gray bins contain no selected weight", fontsize=9)
    axes[1].set_title("State and incomplete-attribution fractions of selected sample weight", fontsize=9)
    axes[1].set_ylabel("% of selected sample weight")
    axes[1].set_xlabel("Sampling boundary (CYCLE)")
    axes[1].ticklabel_format(axis="x", style="plain", useOffset=False)
    fig.suptitle(f"TEA sampled timeline — {run['summary']['label']}")
    fig.text(0.5, 0.005, "CYCLE is the original sampling boundary. STAMP is not used. Sample weights are not measured stall cycles.", ha="center", fontsize=8)
    fig.subplots_adjust(left=0.09, right=0.86, top=0.88, bottom=0.2, hspace=0.3)
    save_figure(fig, directory / "timeline")
    write_csv(directory / "timeline.csv", timeline,
              ("cycle_start", "cycle_end", "rows", "incomplete_rows", "lanes", *STATES, *EVENTS,
               "sample_weight", "incomplete_weight",
               *(state + "_weight" for state in STATES), *(event + "_weight" for event in EVENTS)))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("databases", nargs="+", type=Path, help="Tea_0 SQLite databases (read-only)")
    parser.add_argument("--output", required=True, type=Path)
    parser.add_argument("--labels", nargs="+", help="one display label per database; default: parent directory")
    parser.add_argument("--compare-first", action="store_true",
                        help="export same-checkpoint candidate-minus-first deltas; requires SPEC logs and sample periods")
    parser.add_argument("--spec-checkpoint", action="store_true", help="exclude this SPEC archive's flash/restorer PC ranges")
    window = parser.add_mutually_exclusive_group()
    window.add_argument("--cycle-range", nargs=2, type=int, metavar=("START", "END"), help="CYCLE range [START, END)")
    window.add_argument("--spec-log-dir", nargs="+", type=Path,
                        help="one simulator log directory per database; validate the default SPEC measured window")
    parser.add_argument("--top", type=int, default=12, help="number of PCs in each PICS figure")
    parser.add_argument("--bins", type=int, default=40, help="number of timeline bins")
    parser.add_argument("--elf", nargs="+", help="matching ELF per database, or '-' to skip assembly annotation")
    parser.add_argument("--objdump", default="riscv64-unknown-linux-gnu-objdump")
    args = parser.parse_args()
    for name in ("labels", "elf", "spec_log_dir"):
        if getattr(args, name) is not None and len(getattr(args, name)) != len(args.databases):
            parser.error(f"--{name.replace('_', '-')} needs one value per database")
    if args.top <= 0 or args.bins <= 0:
        parser.error("--top and --bins must be positive")
    if args.cycle_range and not 0 <= args.cycle_range[0] < args.cycle_range[1]:
        parser.error("--cycle-range needs 0 <= START < END")
    if args.compare_first and (len(args.databases) < 2 or not args.spec_log_dir):
        parser.error("--compare-first needs at least two databases and matching --spec-log-dir values")
    paths = [path.expanduser().resolve() for path in args.databases]
    labels = args.labels or [path.parent.name for path in paths]
    elfs = [None if path == "-" else Path(path).expanduser().resolve() for path in args.elf] if args.elf else [None] * len(paths)
    runs = []
    for index, (path, label) in enumerate(zip(paths, labels)):
        selection = argparse.Namespace(**vars(args))
        metadata = {}
        if args.spec_log_dir:
            directory = args.spec_log_dir[index].expanduser().resolve()
            try:
                selection.cycle_range, instructions, cycles = read_spec_roi(directory)
            except (OSError, ValueError) as error:
                parser.error(str(error))
            metadata = dict(spec_log_dir=str(directory), measured_instructions=instructions,
                            measured_cycles=cycles, cpi=cycles / instructions,
                            cycle_start=selection.cycle_range[0],
                            cycle_end=selection.cycle_range[1])
            periods = re.findall(r"teaSamplePeriod0\s*=\s*(\d+)",
                                 (directory / "simulator_out.txt").read_text())
            if periods:
                metadata.update(sampling_coverage(path, selection.cycle_range, max(1, int(periods[-1]))))
        run = load_data(path, label, selection)
        if args.spec_log_dir and not run["summary"]["selected_rows"]:
            parser.error(f"{path}: no TEA samples in the validated SPEC measured window")
        run["summary"].update(metadata)
        runs.append(run)
    if args.compare_first and any("sample_period" not in run["summary"] for run in runs):
        parser.error("--compare-first needs teaSamplePeriod0 in each SPEC output log")
    output = args.output.expanduser().resolve()
    output.mkdir(parents=True, exist_ok=True)
    summaries = [run["summary"] for run in runs]
    write_csv(output / "summary.csv", summaries, list(summaries[0]))
    (output / "summary.json").write_text(json.dumps(dict(
        unit="captured sample equivalents", spec_checkpoint=args.spec_checkpoint,
        cycle_range=args.cycle_range, slices=summaries), indent=2) + "\n")
    categories, category_labels, colors = category_style(runs)
    plot_comparison(runs, output, categories, category_labels, colors)
    if args.compare_first:
        write_deltas(runs, output)
    for i, (run, elf) in enumerate(zip(runs, elfs), 1):
        slug = re.sub(r"[^A-Za-z0-9_.-]+", "_", run["summary"]["label"])
        directory = output / f"{i:02d}_{slug}"
        directory.mkdir(exist_ok=True)
        plot_pics(run, directory, categories, category_labels, colors, args, elf)
        plot_timeline(run, directory)
        export_context(run, directory)
        export_cycle_stats(paths[i - 1], directory)
        print(f"{run['summary']['label']}: {run['summary']['selected_rows']} rows, "
              f"{run['summary']['selected_lanes']} lanes -> {directory}")
    print(f"Comparison and summaries: {output}")


if __name__ == "__main__":
    main()

"""`bazelisk run //:ppa` — Fast-turnaround XiangShan synthesis & stage PPA report.

Trusts Bazel's content-addressed action cache: builds the full module set for
the requested `--stage` (`synth` by default, or `fir`, `floorplan`, `place`,
`cts`, `grt`), so unchanged modules are instant cache hits and only edited
modules re-synthesize. Uses `git merge-base` strictly at report time to
highlight modules whose Chisel/Scala sources changed on the current branch or
working tree.
"""

import argparse
import fnmatch
import json
import os
import subprocess
import sys

VALID_STAGES = ("fir", "synth", "floorplan", "place", "cts", "grt")


def find_merge_base(repo_dir, explicit_base=None):
    """Resolve `(base_label, merge_base_sha, changed_files_set)` via git."""
    candidates = (
        [explicit_base]
        if explicit_base
        else ["origin/main", "origin/master", "origin/HEAD", "origin/kunminghu-v3", "kunminghu-v3"]
    )
    chosen_ref = None
    mb_sha = None
    for ref in candidates:
        r = subprocess.run(
            ["git", "-C", repo_dir, "merge-base", "HEAD", ref],
            capture_output=True,
            text=True,
        )
        if r.returncode == 0 and r.stdout.strip():
            chosen_ref = ref
            mb_sha = r.stdout.strip()
            break

    if not mb_sha:
        return None, None, set()

    changed = set()
    # Committed + staged + unstaged changes relative to merge-base
    for cmd in (
        ["git", "-C", repo_dir, "diff", "--name-only", mb_sha],
        ["git", "-C", repo_dir, "ls-files", "--others", "--exclude-standard"],
    ):
        r = subprocess.run(cmd, capture_output=True, text=True)
        if r.returncode == 0:
            for line in r.stdout.splitlines():
                p = line.strip().replace("\\", "/")
                if p:
                    changed.add(p)
    return chosen_ref, mb_sha, changed


def module_is_changed(mod_entry, changed_files):
    """Return sorted list of changed files touching `mod_entry`."""
    if not changed_files:
        return []
    srcs = set(mod_entry.get("source_files", []))
    hits = sorted(srcs & changed_files)
    return hits


def _fmt_table(headers, rows, right_align_cols):
    all_rows = [headers] + rows
    widths = [max(len(str(r[c])) for r in all_rows) for c in range(len(headers))]
    lines = []
    hdr_line = "  ".join(
        str(headers[c]).rjust(widths[c]) if c in right_align_cols else str(headers[c]).ljust(widths[c])
        for c in range(len(headers))
    )
    sep_line = "  ".join("-" * widths[c] for c in range(len(headers)))
    lines.append(hdr_line)
    lines.append(sep_line)
    for r in rows:
        lines.append(
            "  ".join(
                str(r[c]).rjust(widths[c]) if c in right_align_cols else str(r[c]).ljust(widths[c])
                for c in range(len(headers))
            )
        )
    return "\n".join(lines)


def _fmt_md_table(headers, rows, right_align_cols):
    lines = [
        "| " + " | ".join(str(h) for h in headers) + " |",
        "| " + " | ".join("---:" if i in right_align_cols else "---" for i in range(len(headers))) + " |",
    ]
    for r in rows:
        lines.append("| " + " | ".join(str(c) for c in r) + " |")
    return "\n".join(lines)


def format_report(
    stage,
    bundle,
    fo4_cal,
    base_ref=None,
    mb_sha=None,
    changed_files=None,
    module_filter=None,
    markdown=False,
):
    """Render summary (FO4-first) + full ASAP7 report."""
    changed_files = changed_files or set()
    fo4_ps = float(fo4_cal["fo4_ps"])
    target_fo4 = float(fo4_cal["target_fo4"])
    asap7_target_ps = float(fo4_cal["asap7_target_ps"])

    mods = dict(bundle.get("modules", {}))
    extra_mods = bundle.get("extra_modules", {}) if stage == "fir" else {}
    container_wrappers = {
        "XSTop", "XSTile", "XSCore", "Backend", "Frontend", "MemBlock", "L2Top", "CtrlBlock",
    }

    if module_filter and extra_mods:
        for n, entry in extra_mods.items():
            if n not in mods and fnmatch.fnmatch(n.lower(), f"*{module_filter.lower()}*"):
                mods[n] = entry
    elif changed_files and extra_mods:
        # Auto-surface non-panel modules whose direct CIRCT `#loc` sources (`own_source_files`)
        # were modified since merge-base, giving 100% core-wide PR coverage in `--stage=fir`
        # while keeping untouched runs compact (18 panel rows).
        direct_extra = {}
        for n, entry in extra_mods.items():
            own_hits = sorted(set(entry.get("own_source_files", [])) & changed_files)
            if own_hits:
                direct_extra[n] = (entry, own_hits)
        non_wrappers = {n: v for n, v in direct_extra.items() if n not in container_wrappers}
        chosen_extra = non_wrappers if non_wrappers else direct_extra
        for n, (entry, _) in chosen_extra.items():
            mods[n] = entry

    names = sorted(mods.keys())
    if module_filter:
        names = [
            n for n in names
            if fnmatch.fnmatch(n.lower(), f"*{module_filter.lower()}*")
        ]

    mod_hits = {n: module_is_changed(mods[n], changed_files) for n in names}
    highlighted = [n for n in names if mod_hits[n]]
    # Order highlighted modules first (by descending FO4), then remaining by descending FO4
    ordered = sorted(
        names,
        key=lambda n: (0 if mod_hits[n] else 1, -float(mods[n].get("fo4", 0.0)), n),
    )

    out = []
    short_mb = mb_sha[:10] if mb_sha else "none"
    rtl_changed = sorted(f for f in changed_files if f.endswith((".scala", ".sv", ".v")))

    has_hl = bool(highlighted)

    if markdown:
        out.append(f"## XiangShan PPA Report (`--stage={stage}`)")
        out.append("")
        out.append(
            f"- **FO4 calibration:** ASAP7 `{fo4_cal['primary_cell']}` = **`{fo4_ps:.2f} ps`** "
            f"(Kunminghu 3.0 GHz / 333 ps 7 nm target = **`{target_fo4:.1f} FO4`** / `{asap7_target_ps:.0f} ps` on ASAP7)"
        )
        if mb_sha and has_hl:
            out.append(
                f"- **Git highlight base:** `{base_ref}` (`{short_mb}`) — "
                f"**{len(rtl_changed)}** RTL/Scala file(s) modified, "
                f"**{len(highlighted)}** tracked module(s) highlighted (`*`)"
            )
    else:
        out.append(f"=== XiangShan PPA Report (stage: {stage}) ===")
        out.append(
            f"FO4 calibration : ASAP7 {fo4_cal['primary_cell']} = {fo4_ps:.2f} ps  "
            f"|  3.0 GHz (333 ps 7nm) target = {target_fo4:.1f} FO4 ({asap7_target_ps:.0f} ps ASAP7)"
        )
        if mb_sha and has_hl:
            out.append(
                f"Git merge-base  : {base_ref} ({short_mb}) — "
                f"{len(rtl_changed)} RTL/Scala file(s) changed, "
                f"{len(highlighted)} module(s) highlighted (*)"
            )
            if rtl_changed:
                shown = ", ".join(rtl_changed[:6])
                more = f" (+{len(rtl_changed) - 6} more)" if len(rtl_changed) > 6 else ""
                out.append(f"Changed files   : {shown}{more}")

    out.append("")

    def _mod_label(n):
        if not has_hl:
            return n
        return f"* {n}" if mod_hits[n] else f"  {n}"

    # --- Section 1: Summary Table (PDK-independent FO4 framing first!) ---
    if stage == "fir":
        sum_headers = [
            "Module", "Grp", "FO4 (fir)", "Lvls", "Flops", "FlopBits",
            "CombGE", "SRAM bits", "ICGs", "MaxFO", "Flags",
        ]
        right_cols = {2, 3, 4, 5, 6, 7, 8, 9}
        sum_rows = []
        for n in ordered:
            m = mods[n]
            flags = []
            if m.get("mem2mem_hazard"):
                flags.append("MEM2MEM!")
            if m.get("max_fanout", 0) > 128:
                flags.append(f"FO={m['max_fanout']}")
            if float(m.get("fo4", 0.0)) > target_fo4:
                flags.append(f">{target_fo4:.0f}FO4")
            sum_rows.append([
                _mod_label(n),
                m.get("primary_group", "-"),
                f"{float(m.get('fo4', 0.0)):.1f}",
                str(m.get("levels", 0)),
                str(m.get("flops", 0)),
                str(m.get("flop_bits", 0)),
                str(m.get("comb_cells", 0)),
                str(m.get("sram_bits", 0)),
                str(m.get("icgs", 0)),
                str(m.get("max_fanout", 0)),
                ",".join(flags) or "-",
            ])
    else:
        fo4_col = f"FO4 ({stage})"
        sum_headers = [
            "Module", "Grp", fo4_col, "FO4 (fir)", "Lvls", "Flops",
            "Comb", "SRAM bits", "ICGs", "MaxFO", "Flags",
        ]
        right_cols = {2, 3, 4, 5, 6, 7, 8, 9}
        sum_rows = []
        for n in ordered:
            m = mods[n]
            flags = []
            if m.get("mem2mem_hazard"):
                flags.append("MEM2MEM!")
            if m.get("unclocked_regs", 0) > 0:
                flags.append(f"UNCLK:{m['unclocked_regs']}")
            if m.get("max_fanout", 0) > 128:
                flags.append(f"FO={m['max_fanout']}")
            if float(m.get("fo4", 0.0)) > target_fo4:
                flags.append(f">{target_fo4:.0f}FO4")
            fo4_fir = m.get("fo4_fir")
            fo4_fir_str = f"{float(fo4_fir):.1f}" if fo4_fir is not None else "-"
            sum_rows.append([
                _mod_label(n),
                m.get("primary_group", "-"),
                f"{float(m.get('fo4', 0.0)):.1f}",
                fo4_fir_str,
                str(m.get("levels", 0)),
                str(m.get("flops", 0)),
                str(m.get("comb_cells", 0)),
                str(m.get("sram_bits", 0)),
                str(m.get("icgs", 0)),
                str(m.get("max_fanout", 0)),
                ",".join(flags) or "-",
            ])

    if markdown:
        title_sum = "### Summary (PDK-independent FO4 framing)"
    elif has_hl:
        title_sum = "--- Summary (PDK-independent FO4 framing; * = touched since git merge-base) ---"
    else:
        title_sum = "--- Summary (PDK-independent FO4 framing) ---"
    out.append(title_sum)
    out.append(
        _fmt_md_table(sum_headers, sum_rows, right_cols)
        if markdown
        else _fmt_table(sum_headers, sum_rows, right_cols)
    )
    out.append("")

    # --- Section 2: Full ASAP7 Report (actual ps, um^2, buffers, runtime) ---
    if stage != "fir":
        asap_headers = [
            "Module", "Grp", f"FO4 ({stage})", "Period (ps)", "WNS (ps)",
            "TNS (ps)", "Area (um2)", "Macro (um2)", "Bufs (+add)", "Time (s)",
        ]
        asap_right = {2, 3, 4, 5, 6, 7, 8, 9}
        asap_rows = []
        for n in ordered:
            m = mods[n]
            asap_rows.append([
                _mod_label(n),
                m.get("primary_group", "-"),
                f"{float(m.get('fo4', 0.0)):.1f}",
                f"{float(m.get('period_ps', 0.0)):.1f}",
                f"{float(m.get('wns_ps', 0.0)):.1f}",
                f"{float(m.get('tns_ps', 0.0)):.0f}",
                f"{float(m.get('area_um2', 0.0)):.1f}",
                f"{float(m.get('macro_area_um2', 0.0)):.1f}",
                f"{m.get('buffers_total', 0)} (+{m.get('buffers_added', 0)})",
                f"{float(m.get('elapsed_s', 0.0)):.1f}",
            ])
        title_full = (
            f"### Full ASAP7 Data (`{stage}`, SDC = 1000 ps, 1 FO4 = {fo4_ps:.2f} ps)"
            if markdown
            else f"--- Full ASAP7 Data (stage: {stage}, SDC = 1000.0 ps, 1 FO4 = {fo4_ps:.2f} ps) ---"
        )
        out.append(title_full)
        out.append(
            _fmt_md_table(asap_headers, asap_rows, asap_right)
            if markdown
            else _fmt_table(asap_headers, asap_rows, asap_right)
        )
        out.append("")

    # --- Section 3: Path-group & Chisel endpoint drill-down for highlighted/filtered modules ---
    detail_mods = highlighted if highlighted else (ordered if module_filter else ordered[:5])
    if detail_mods:
        hdr = "### Critical Path Groups & Chisel Locations" if markdown else "--- Critical Path Groups & Chisel Locations ---"
        out.append(hdr)
        for n in detail_mods:
            m = mods[n]
            tag = " [CHANGED since merge-base]" if mod_hits[n] else ""
            out.append(f"[{n}]{tag}")
            if mod_hits[n]:
                out.append(f"  touched Scala: {', '.join(mod_hits[n])}")
            pg = m.get("path_groups", {})
            for g in ("reg2reg", "mem2reg", "reg2mem", "mem2mem", "in2reg", "reg2out", "in2out"):
                if g not in pg:
                    continue
                gd = pg[g]
                if stage == "fir":
                    loc_s = f"  @[{gd['loc']}]" if gd.get("loc") else ""
                    out.append(
                        f"  {g:<8s}  {gd['fo4']:6.1f} FO4  ({gd['levels']:2d} lvls)  "
                        f"{gd['startpoint']} -> {gd['endpoint']}{loc_s}"
                    )
                else:
                    loc = gd.get("end_loc") or gd.get("start_loc")
                    loc_s = f"  @[{loc}]" if loc else ""
                    out.append(
                        f"  {g:<8s}  {gd['fo4']:6.1f} FO4  ({gd['period_ps']:6.1f} ps, WNS {gd['wns_ps']:+6.1f} ps, {gd['levels']:2d} lvls)  "
                        f"{gd['startpoint']} -> {gd['endpoint']}{loc_s}"
                    )
        out.append("")

    return "\n".join(out)


def build_and_load(workspace_dir, stage, bazelisk_bin="bazelisk"):
    """Build only the requested stage's Bazel targets and load JSON outputs."""
    fo4_target = "//scripts/openroad-flow/ppa:asap7_fo4"
    stage_target = (
        "//scripts/openroad-flow/ppa:ppa_fir_bundle"
        if stage == "fir"
        else f"//scripts/openroad-flow/ppa:ppa_{stage}_bundle"
    )
    cmd = [bazelisk_bin, "build", fo4_target, stage_target]
    r = subprocess.run(cmd, cwd=workspace_dir)
    if r.returncode != 0:
        sys.exit(r.returncode)

    # Resolve execution_root so `--symlink_prefix=.bazel/` works transparently
    exec_root_r = subprocess.run(
        [bazelisk_bin, "info", "execution_root"],
        cwd=workspace_dir,
        capture_output=True,
        text=True,
        check=True,
    )
    exec_root = exec_root_r.stdout.strip()

    # Query exact output paths via cquery --output=files
    cq = subprocess.run(
        [bazelisk_bin, "cquery", "--output=files", f"{fo4_target} + {stage_target}"],
        cwd=workspace_dir,
        capture_output=True,
        text=True,
        check=True,
    )
    fo4_path = None
    bundle_path = None
    for line in cq.stdout.splitlines():
        p = line.strip()
        if p.endswith("asap7_fo4.json"):
            fo4_path = os.path.join(exec_root, p)
        elif p.endswith(f"ppa_{stage}.json"):
            bundle_path = os.path.join(exec_root, p)

    if not fo4_path or not bundle_path:
        sys.exit(f"ppa: failed to locate built artifacts for stage {stage}")

    with open(fo4_path, "r", encoding="utf-8") as fh:
        fo4_cal = json.load(fh)
    with open(bundle_path, "r", encoding="utf-8") as fh:
        bundle = json.load(fh)
    return fo4_cal, bundle


def main(argv=None):
    ap = argparse.ArgumentParser(
        description="XiangShan PPA synthesis & stage report (highlights changes since git merge-base)."
    )
    ap.add_argument(
        "--stage",
        default="synth",
        choices=VALID_STAGES,
        help="Flow stage to report (`fir` uses only FIRRTL/MLIR; default: `synth`).",
    )
    ap.add_argument(
        "--base",
        default=None,
        help="Git base ref for merge-base highlighting (default: origin/main -> origin/HEAD -> origin/kunminghu-v3).",
    )
    ap.add_argument(
        "--module",
        default=None,
        help="Optional substring/glob filter on module name.",
    )
    ap.add_argument(
        "--markdown",
        action="store_true",
        help="Format tables as GitHub-Flavored Markdown for PR comments.",
    )
    ap.add_argument(
        "--json-out",
        default=None,
        help="Optional file path (relative to working dir) to write the raw stage JSON.",
    )
    # Internal test/offline mode: pass prebuilt JSON files directly
    ap.add_argument("--fo4-json", default=None, help=argparse.SUPPRESS)
    ap.add_argument("--bundle-json", default=None, help=argparse.SUPPRESS)
    args = ap.parse_args(argv)

    ws_dir = os.environ.get("BUILD_WORKSPACE_DIRECTORY") or os.getcwd()
    work_dir = os.environ.get("BUILD_WORKING_DIRECTORY") or os.getcwd()

    if args.fo4_json and args.bundle_json:
        with open(args.fo4_json, "r", encoding="utf-8") as fh:
            fo4_cal = json.load(fh)
        with open(args.bundle_json, "r", encoding="utf-8") as fh:
            bundle = json.load(fh)
    else:
        fo4_cal, bundle = build_and_load(ws_dir, args.stage)

    base_ref, mb_sha, changed_files = find_merge_base(ws_dir, args.base)
    report_txt = format_report(
        args.stage,
        bundle,
        fo4_cal,
        base_ref=base_ref,
        mb_sha=mb_sha,
        changed_files=changed_files,
        module_filter=args.module,
        markdown=args.markdown,
    )
    sys.stdout.write(report_txt + "\n")

    if args.json_out:
        out_p = (
            args.json_out
            if os.path.isabs(args.json_out)
            else os.path.join(work_dir, args.json_out)
        )
        with open(out_p, "w", encoding="utf-8") as fh:
            json.dump(
                {
                    "stage": args.stage,
                    "base_ref": base_ref,
                    "merge_base": mb_sha,
                    "changed_files": sorted(changed_files),
                    "fo4_calibration": fo4_cal,
                    "bundle": bundle,
                },
                fh,
                indent=2,
                sort_keys=True,
            )
            fh.write("\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())

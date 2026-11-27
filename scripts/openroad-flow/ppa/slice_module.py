"""Extract a self-contained SystemVerilog slice + Scala source map for one module.

Reads `firtool`'s split-Verilog directory (`<Module>.sv` per module) plus
`ClockGate.sv`, transitively collects all instantiated submodules, and writes:

1. `<out_sv>`: deterministic SystemVerilog with `// @[...]` location comments
   and `` `include `` directives stripped, so line-number shifts in unrelated
   Scala code never invalidate Bazel's synthesis/P&R cache for this module.
2. `<out_srcmap>`: JSON mapping module/signal names to `src/main/scala/...:line`
   locators and listing all contributing Scala files for `git merge-base`
   highlighting and critical-path attribution.
"""

import argparse
import json
import os
import re
import sys

RE_MOD_DECL = re.compile(r"^\s*module\s+([A-Za-z_][A-Za-z0-9_$]*)\b")
RE_ENDMOD = re.compile(r"^\s*endmodule\b")
# Matches standard firtool module instantiation: `  SubModName inst_name (`
SV_KEYWORDS = {
    "module", "endmodule", "input", "output", "inout", "wire", "reg", "logic",
    "assign", "always", "always_ff", "always_comb", "always_latch", "initial",
    "begin", "end", "if", "else", "case", "casex", "casez", "endcase", "for",
    "generate", "endgenerate", "localparam", "parameter", "function",
    "endfunction", "task", "endtask", "posedge", "negedge", "or", "and",
}
RE_INST = re.compile(
    r"^\s*([A-Za-z_][A-Za-z0-9_$]*)\s+(?:#\s*\([^;]*?\)\s*)?([A-Za-z_][A-Za-z0-9_$]*)\s*\("
)
RE_LOC_COMMENT = re.compile(r"\s*//\s*@\[([^\]]+)\]\s*$")
RE_LOC_FILE = re.compile(r"([A-Za-z0-9_./-]+\.scala):(\d+)")
RE_DECL_SIG = re.compile(
    r"^\s*(?:reg|wire|logic|input|output)\s+(?:\[[^\]]+\]\s+)?([A-Za-z_][A-Za-z0-9_$]*)"
)


def _norm_src(path):
    p = path.replace("\\", "/")
    # Check git submodule roots before `src/main/scala/` so paths like
    # `XSCache/src/main/scala/...` keep their repo-root submodule prefix.
    for marker in (
        "XSCache/",
        "yunsuan/",
        "utility/",
        "rocket-chip/",
        "ChiselAIA/",
        "ChiselIOPMP/",
        "openncb/",
        "difftest/",
        "macros/",
        "src/main/scala/",
    ):
        idx = p.find(marker)
        if idx >= 0:
            return p[idx:]
    return p.lstrip("./")


def index_sv_sources(sv_dir, extra_files=()):
    """Map `module_name -> file_path` across `sv_dir` and `extra_files`."""
    mod_to_path = {}
    if os.path.isdir(sv_dir):
        for fn in sorted(os.listdir(sv_dir)):
            if fn.endswith((".sv", ".v")):
                stem = os.path.splitext(fn)[0]
                full = os.path.join(sv_dir, fn)
                mod_to_path[stem] = full
    for ef in extra_files:
        if os.path.isfile(ef):
            with open(ef, "r", encoding="utf-8", errors="replace") as fh:
                for line in fh:
                    m = RE_MOD_DECL.match(line)
                    if m:
                        mod_to_path[m.group(1)] = ef
    return mod_to_path


def parse_and_clean_sv(path):
    """Return `(clean_text, instantiated_mods, source_files, sig_locs)`."""
    clean_lines = []
    children = []
    seen_children = set()
    src_files = set()
    sig_locs = {}
    in_ifdef_synth = 0

    with open(path, "r", encoding="utf-8", errors="replace") as fh:
        for raw in fh:
            s = raw.strip()
            if s.startswith("`include"):
                continue
            # Extract `// @[file.scala:line:col, ...]` before stripping
            m_loc = RE_LOC_COMMENT.search(raw)
            first_loc = None
            if m_loc:
                for f_m in RE_LOC_FILE.finditer(m_loc.group(1)):
                    norm_f = _norm_src(f_m.group(1))
                    src_files.add(norm_f)
                    if first_loc is None:
                        first_loc = f"{norm_f}:{f_m.group(2)}"
                line = raw[: m_loc.start()] + "\n"
            else:
                line = raw

            if first_loc:
                m_sig = RE_DECL_SIG.match(line)
                if m_sig and m_sig.group(1) not in sig_locs:
                    sig_locs[m_sig.group(1)] = first_loc

            m_inst = RE_INST.match(line)
            if m_inst:
                cand = m_inst.group(1)
                inst_name = m_inst.group(2)
                if cand not in SV_KEYWORDS and cand not in seen_children:
                    seen_children.add(cand)
                    children.append(cand)
                if first_loc and inst_name not in sig_locs:
                    sig_locs[inst_name] = first_loc

            # Drop pure Verification/assert `ifndef SYNTHESIS` blocks? Keep SV intact;
            # slang ignores `ifndef SYNTHESIS` when SYNTHESIS is defined by ORFS!
            clean_lines.append(line.rstrip() + "\n")

    return "".join(clean_lines), children, src_files, sig_locs


def slice_module(sv_dir, top_module, out_sv, out_srcmap, extra_files=()):
    mod_to_path = index_sv_sources(sv_dir, extra_files)
    if top_module not in mod_to_path:
        raise SystemExit(
            f"slice_module: module '{top_module}' not found in {sv_dir} "
            f"(have {len(mod_to_path)} modules)"
        )

    ordered = []
    visited = set()
    all_srcs = set()
    all_sig_locs = {}

    def dfs(m):
        if m in visited or m not in mod_to_path:
            return
        visited.add(m)
        clean_txt, kids, srcs, sig_locs = parse_and_clean_sv(mod_to_path[m])
        all_srcs.update(srcs)
        if m == top_module:
            all_sig_locs.update(sig_locs)
        else:
            for k, v in sig_locs.items():
                all_sig_locs.setdefault(k, v)
        for c in kids:
            dfs(c)
        ordered.append((m, clean_txt))

    dfs(top_module)

    with open(out_sv, "w", encoding="utf-8") as fh:
        for _, txt in ordered:
            fh.write(txt)
            if not txt.endswith("\n"):
                fh.write("\n")

    srcmap = {
        "module": top_module,
        "submodules": [m for m, _ in ordered if m != top_module],
        "source_files": sorted(all_srcs),
        "signal_locs": all_sig_locs,
    }
    with open(out_srcmap, "w", encoding="utf-8") as fh:
        json.dump(srcmap, fh, indent=2, sort_keys=True)
        fh.write("\n")


def main(argv=None):
    ap = argparse.ArgumentParser(description="Slice one SV module + transitive deps.")
    ap.add_argument("--sv-dir", required=True)
    ap.add_argument("--top", required=True)
    ap.add_argument("--out-sv", required=True)
    ap.add_argument("--out-srcmap", required=True)
    ap.add_argument("--extra-sv", nargs="*", default=[])
    args = ap.parse_args(argv)
    slice_module(args.sv_dir, args.top, args.out_sv, args.out_srcmap, args.extra_sv)
    return 0


if __name__ == "__main__":
    sys.exit(main())

"""Static PPA & FO4 timing estimator directly on CIRCT HW/Comb/Seq MLIR (`.fir`).

Reads the output of `ChiselStage.emitHWDialect` (or classic FIRRTL as fallback)
without running `firtool` Verilog emission, Yosys, or OpenROAD.

Per `hw.module`, builds the exact SSA combinational DAG, counts registers,
register bits, `ClockGate` ICG instances, inferred SRAM `array_<D>x<W>` /
`ram_<D>x<W>` macros, bit-weighted fanout, and estimates path delays in
technology-independent FO4 units across `reg2reg`, `mem2reg`, `reg2mem`,
`mem2mem`, `in2reg`, `reg2out`, and `in2out`.
"""

import argparse
from collections import Counter, defaultdict
import json
import math
import re
import sys
import time

# Calibrated CMOS / ASAP7 FO4 weights (1 FO4 ~= 14.37 ps on ORFS ASAP7 RVT FF)
REG_CLK2Q_FO4 = 2.0
REG_SETUP_FO4 = 1.5
MEM_CLK2Q_BASE_FO4 = 5.5
MEM_SETUP_FO4 = 2.2

RE_LOC_DEF = re.compile(r'^#loc(\d+)\s*=\s*loc\("([^"]+)"(?::(\d+):(\d+))?\)')
RE_LOC_INLINE = re.compile(r'loc\("([^"]+)"(?::(\d+):(\d+))?\)')
RE_LOC_REF = re.compile(r'loc\(#loc(\d+)\)')
RE_HW_MOD = re.compile(r'^\s*hw\.module(?:\s+private)?\s+@([^\s(]+)\s*\((.*)\)\s*(?:attributes\s*\{.*\}\s*)?\{')
RE_MEM_NAME = re.compile(
    r'^(?:(?:array|ram|sram)_(\d+)x(\d+)(?:$|_)|sram_array_\d+p(\d+)x(\d+)m)',
    re.I,
)
RE_INST_IN_BIND = re.compile(r'([A-Za-z0-9_$.]+)\s*:\s*(%[^\s:,)]+)\s*:\s*([^,)]+)')
RE_INST_OUT_BIND = re.compile(r'([A-Za-z0-9_$.]+)\s*:\s*([^,)]+)')
RE_SSA = re.compile(r'%[A-Za-z0-9_$.#-]+')
RE_INT_TYPE = re.compile(r'\bi(\d+)\b')
RE_ARR_TYPE = re.compile(r'!hw\.array<(\d+)xi(\d+)>')


def _match_mem_dims(mod_name):
    m = RE_MEM_NAME.match(mod_name)
    if not m:
        return None
    if m.group(1) is not None:
        return int(m.group(1)), int(m.group(2))
    return int(m.group(3)), int(m.group(4))


def _type_bits(type_str):
    m_arr = RE_ARR_TYPE.search(type_str)
    if m_arr:
        return int(m_arr.group(1)) * int(m_arr.group(2))
    m_int = RE_INT_TYPE.search(type_str)
    if m_int:
        return int(m_int.group(1))
    return 1


def _parse_ports(sig):
    """Return `(inputs, outputs)` lists of `(name, bits)` from hw.module header."""
    inputs = []
    outputs = []
    # Split top-level comma-separated port specs (ignoring commas inside <...>)
    depth = 0
    cur = []
    parts = []
    for ch in sig:
        if ch in "<(":
            depth += 1
        elif ch in ">)":
            depth -= 1
        if ch == "," and depth == 0:
            parts.append("".join(cur).strip())
            cur = []
        else:
            cur.append(ch)
    if cur:
        last = "".join(cur).strip()
        if last:
            parts.append(last)

    for p in parts:
        # e.g. `in %clock : !seq.clock` or `in %io_a : i64` or `out io_b : i32`
        p_clean = re.sub(r'loc\([^)]*\)', '', p)
        p_clean = re.sub(r'\{[^}]*\}', '', p_clean).strip()
        m_in = re.match(r'^in\s+(%[^\s:]+)\s*:\s*(.+)$', p_clean)
        if m_in:
            name = m_in.group(1)
            t = m_in.group(2).strip()
            is_clk = "!seq.clock" in t or name in ("%clock", "%clk", "%reset", "%io_clock")
            inputs.append((name, _type_bits(t), is_clk))
            continue
        m_out = re.match(r'^out\s+([^\s:]+)\s*:\s*(.+)$', p_clean)
        if m_out:
            name = m_out.group(1).lstrip("%")
            t = m_out.group(2).strip()
            outputs.append((name, _type_bits(t)))
    return inputs, outputs


def _op_fo4_and_ge(op, n_args, width, pred=""):
    """Return `(delay_fo4, levels, gate_equiv)` for a `comb.*` or `hw.*` op."""
    w = max(width, 1)
    lgw = math.log2(max(w, 2))
    lgk = math.log2(max(n_args, 2))
    if op in ("hw.constant", "comb.extract", "comb.concat", "comb.replicate", "hw.bitcast",
              "hw.array_create", "hw.array_slice", "hw.struct_create", "hw.struct_extract"):
        return 0.0, 0, 0
    if op == "comb.mux":
        return 1.0, 1, 2 * w
    if op == "hw.array_get":
        lv = max(1, int(math.ceil(lgw)))
        return 1.0 * lv, lv, 2 * w * lv
    if op in ("comb.and", "comb.or"):
        lv = max(1, int(math.ceil(lgk)))
        return 0.65 * lv, lv, max(1, n_args - 1) * w
    if op == "comb.xor":
        lv = max(1, int(math.ceil(lgk)))
        return 1.05 * lv, lv, max(1, n_args - 1) * 2 * w
    if op == "comb.parity":
        lv = max(1, int(math.ceil(lgw)))
        return 1.05 * lv, lv, 2 * w
    if op in ("comb.add", "comb.sub"):
        lv = max(1, int(math.ceil(lgw)) + max(0, n_args - 2))
        return 0.95 * lgw + 0.8 * max(0, n_args - 1), lv, 5 * w * max(1, n_args - 1)
    if op == "comb.icmp":
        if pred in ("eq", "ne", "ceq", "cne", "weq", "wne"):
            lv = max(1, 1 + int(math.ceil(math.log2(max(w, 2)) / 2.0)))
            return 0.9 + 0.55 * lgw, lv, 3 * w
        lv = max(1, 1 + int(math.ceil(lgw)))
        return 0.8 + 0.90 * lgw, lv, 4 * w
    if op in ("comb.shl", "comb.shru", "comb.shrs"):
        lv = max(1, int(math.ceil(lgw)))
        return 0.90 * lgw, lv, 2 * w * lv
    if op == "comb.mul":
        lv = max(2, int(math.ceil(2.2 * lgw)))
        return 2.2 * lgw + 2.0, lv, 6 * w * w
    if op.startswith("comb.div") or op.startswith("comb.mod"):
        lv = max(4, 2 * w)
        return 4.0 * w, lv, 8 * w * w
    return 0.65, 1, w


def _norm_repo_src(path):
    """Normalize a source locator path to repo-relative form (`XSCache/...`, `src/...`)."""
    p = path.replace("\\", "/")
    # Check git submodule roots before `src/main/scala/` so submodule paths keep their prefix.
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


def analyze_mlir_text(lines_iter, target_modules=None, panel_modules=None):
    """Stream-parse CIRCT HW/Comb/Seq MLIR lines and return `{mod: metrics}`."""
    t0 = time.monotonic()
    raw_modules = {}
    loc_defs = {}

    cur_name = None
    cur_header = None
    cur_body = []
    brace_depth = 0

    for line in lines_iter:
        if cur_name is None:
            m_mod = RE_HW_MOD.match(line)
            if m_mod:
                cur_name = m_mod.group(1)
                cur_header = m_mod.group(2)
                cur_body = []
                brace_depth = line.count("{") - line.count("}")
                if brace_depth <= 0:
                    raw_modules[cur_name] = (cur_header, cur_body)
                    cur_name = None
                continue
            if line.startswith("#loc"):
                m_loc = RE_LOC_DEF.match(line.strip())
                if m_loc:
                    src = _norm_repo_src(m_loc.group(2))
                    ln = m_loc.group(3)
                    loc_defs[m_loc.group(1)] = (src, f"{src}:{ln}" if ln else src)
        else:
            brace_depth += line.count("{") - line.count("}")
            if brace_depth <= 0:
                raw_modules[cur_name] = (cur_header, cur_body)
                cur_name = None
            else:
                cur_body.append(line)

    # Evaluate bottom-up so child module summaries & source files roll up cleanly
    memo = {}
    visiting = set()

    def eval_module(mod_name):
        if mod_name in memo:
            return memo[mod_name]
        if mod_name not in raw_modules or mod_name in visiting:
            return None
        visiting.add(mod_name)
        header, body = raw_modules[mod_name]
        inputs, outputs = _parse_ports(header)

        constants = set()
        # node_defs[ssa] = (op, args_list, width, delay_fo4, levels, loc_ref_or_inline)
        node_defs = {}
        # regs: list of (reg_ssa, next_ssa, en_ssa_or_none, width, loc_tag)
        regs = []
        # child instances: list of (inst_name, child_mod, arg_ssas, result_ssas, result_widths, loc_tag)
        instances = []
        out_ssas = []
        bit_fanout = Counter()
        own_comb_ge = 0
        own_icgs = 0
        own_srcs = set()

        def extract_loc(s):
            m_r = RE_LOC_REF.search(s)
            if m_r:
                return ("ref", m_r.group(1))
            m_i = RE_LOC_INLINE.search(s)
            if m_i:
                src = _norm_repo_src(m_i.group(1))
                ln = m_i.group(2)
                own_srcs.add(src)
                return ("str", f"{src}:{ln}" if ln else src)
            return None

        for raw in body:
            s = raw.strip()
            if not s or s.startswith("//"):
                continue
            loc_tok = extract_loc(s)
            if loc_tok and loc_tok[0] == "ref" and loc_tok[1] in loc_defs:
                own_srcs.add(loc_defs[loc_tok[1]][0])

            if s.startswith("hw.output"):
                lhs_part = s.split(":", 1)[0]
                out_ssas = RE_SSA.findall(lhs_part)
                for u in out_ssas:
                    bit_fanout[u] += 1
                continue

            if " = hw.constant " in s:
                lhs = s.split("=", 1)[0].strip()
                constants.add(lhs)
                continue

            if " = seq.firreg " in s or " = seq.compreg " in s:
                lhs, rhs = s.split("=", 1)
                reg_ssa = lhs.strip()
                # Width is after final `:` before `loc`
                no_loc = re.sub(r'\bloc\([^)]*\)\s*$', '', rhs).strip()
                type_part = no_loc.rsplit(":", 1)[-1]
                w = _type_bits(type_part)
                ssas = RE_SSA.findall(no_loc.rsplit(":", 1)[0])
                # First SSA operand is next-state value; exclude clock/reset
                next_ssa = ssas[0] if ssas else None
                if next_ssa:
                    bit_fanout[next_ssa] += w
                regs.append((reg_ssa, next_ssa, w, loc_tok))
                continue

            if "hw.instance " in s:
                # e.g. `%u.o0, %u.o1 = hw.instance "u" @Child(a: %x: i8, ...) -> (o0: i8, ...)`
                if "=" in s.split("hw.instance", 1)[0]:
                    lhs_str, rest = s.split("=", 1)
                    res_ssas = [x.strip() for x in RE_SSA.findall(lhs_str)]
                else:
                    rest = s
                    res_ssas = []
                m_inst = re.search(r'hw\.instance\s+"([^"]*)"\s+(?:sym\s+@[^\s]+\s+)?@([^\s(]+)\s*\((.*)\)\s*->\s*\(([^)]*)\)', rest)
                if m_inst:
                    inst_name = m_inst.group(1)
                    child_mod = m_inst.group(2)
                    in_part = m_inst.group(3)
                    out_part = m_inst.group(4)
                    in_binds = [
                        (p_name, ssa)
                        for p_name, ssa, p_type in RE_INST_IN_BIND.findall(in_part)
                        if "!seq.clock" not in p_type and ssa not in ("%clock", "%clk", "%reset", "%io_clock")
                    ]
                    out_names = [p_name for p_name, _ in RE_INST_OUT_BIND.findall(out_part)]
                    for _, u in in_binds:
                        bit_fanout[u] += 2
                    if child_mod == "ClockGate" or child_mod.startswith("ClockGate_"):
                        own_icgs += 1
                    else:
                        instances.append((inst_name, child_mod, in_binds, out_names, res_ssas, loc_tok))
                continue

            if " = " in s:
                lhs, rhs = s.split("=", 1)
                lhs_ssa = lhs.strip()
                if not lhs_ssa.startswith("%"):
                    continue
                no_loc = re.sub(r'\bloc\([^)]*\)\s*$', '', rhs).strip()
                op_tokens = no_loc.split(None, 2)
                if not op_tokens:
                    continue
                op = op_tokens[0]
                pred = op_tokens[1] if op == "comb.icmp" and len(op_tokens) > 1 else ""
                type_part = no_loc.rsplit(":", 1)[-1] if ":" in no_loc else "i1"
                # Handle `(i64) -> i12`
                if "->" in type_part:
                    type_part = type_part.rsplit("->", 1)[-1]
                w = _type_bits(type_part)
                arg_part = no_loc.rsplit(":", 1)[0] if ":" in no_loc else no_loc
                args = RE_SSA.findall(arg_part)
                # Constant shift amounts have 0 gate delay
                if op in ("comb.shl", "comb.shru", "comb.shrs") and len(args) >= 2 and args[1] in constants:
                    d_fo4, lv, ge = 0.0, 0, 0
                else:
                    non_const = [a for a in args if a not in constants]
                    if not non_const and args:
                        d_fo4, lv, ge = 0.0, 0, 0
                    else:
                        d_fo4, lv, ge = _op_fo4_and_ge(op, len(args), w, pred)
                own_comb_ge += ge
                if op == "comb.mux" and args:
                    # Select drives all `w` bit-muxes!
                    bit_fanout[args[0]] += w
                    for a in args[1:]:
                        bit_fanout[a] += 1
                else:
                    for a in args:
                        bit_fanout[a] += 1
                node_defs[lhs_ssa] = (op, args, w, d_fo4, lv, loc_tok)

        # Roll up child instances
        total_flops = len(regs)
        total_flop_bits = sum(w for _, _, w, _ in regs)
        total_comb_ge = own_comb_ge
        total_icgs = own_icgs
        sram_counter = Counter()
        all_srcs = set(own_srcs)

        # Map each SSA to `(reg_best, mem_best, in_map)` where:
        #   reg_best: None or (fo4, lv, sp_name)
        #   mem_best: None or (fo4, lv, sp_name)
        #   in_map:   dict[in_port_name -> (fo4, lv)]
        EMPTY_ARR = (None, None, {})
        arrivals = {}
        for p_name, p_bits, is_clk in inputs:
            if not is_clk:
                clean_in = p_name.lstrip("%")
                arrivals[p_name] = (None, None, {clean_in: (0.0, 0)})

        for reg_ssa, _, _, _ in regs:
            arrivals[reg_ssa] = ((REG_CLK2Q_FO4, 0, reg_ssa.lstrip("%")), None, {})

        # Pre-seed SRAM / child instance outputs with per-port ILM summaries
        inst_out_Lookup = {}
        for inst_name, child_mod, in_binds, out_names, res_ssas, loc_tok in instances:
            mem_dims = _match_mem_dims(child_mod)
            if mem_dims is not None:
                depth, width = mem_dims
                sram_counter[(child_mod, depth, width)] += 1
                mem_q_fo4 = MEM_CLK2Q_BASE_FO4 + 0.75 * math.log2(max(depth, 2))
                for r_ssa in res_ssas:
                    arrivals[r_ssa] = (None, (mem_q_fo4, 0, f"{inst_name}/{child_mod}"), {})
            else:
                csum = eval_module(child_mod)
                if csum is not None:
                    total_flops += csum["flops"]
                    total_flop_bits += csum["flop_bits"]
                    total_comb_ge += csum["comb_cells"]
                    total_icgs += csum["icgs"]
                    for k, cnt in csum["_sram_counter"].items():
                        sram_counter[k] += cnt
                    all_srcs.update(csum["source_files"])

                    out_reg_map = csum["_out_reg"]
                    out_mem_map = csum["_out_mem"]
                    in_to_out_map = csum["_in_to_out"]
                    for idx_o, r_ssa in enumerate(res_ssas):
                        o_name = out_names[idx_o] if idx_o < len(out_names) else f"out_{idx_o}"
                        r_seed = None
                        m_seed = None
                        if o_name in out_reg_map:
                            rf, rlv, rsp = out_reg_map[o_name]
                            r_seed = (rf, rlv, f"{inst_name}/{rsp}")
                        if o_name in out_mem_map:
                            mf, mlv, msp = out_mem_map[o_name]
                            m_seed = (mf, mlv, f"{inst_name}/{msp}")
                        if r_seed is not None or m_seed is not None:
                            arrivals[r_ssa] = (r_seed, m_seed, {})

                        port_arcs = in_to_out_map.get(o_name)
                        if port_arcs and in_binds:
                            coupled = [
                                (in_ssa, port_arcs[p_name][0], port_arcs[p_name][1])
                                for p_name, in_ssa in in_binds
                                if p_name in port_arcs
                            ]
                            if coupled:
                                inst_out_Lookup[r_ssa] = coupled

        eval_stack = set()

        def fanout_fo4(ssa):
            fo = bit_fanout.get(ssa, 1)
            if fo <= 4:
                return 0.0
            return 0.80 * (math.log(fo / 4.0) / math.log(4.0))

        def _cap_in_map(in_map):
            if len(in_map) <= 64:
                return in_map
            top_items = sorted(in_map.items(), key=lambda kv: kv[1][0], reverse=True)[:64]
            return dict(top_items)

        def get_arr(root_ssa):
            if root_ssa in constants:
                return EMPTY_ARR
            if root_ssa in arrivals and root_ssa not in inst_out_Lookup:
                return arrivals[root_ssa]

            stack = [(root_ssa, False)]
            while stack:
                ssa, entered = stack.pop()
                if not entered:
                    if ssa in constants or (ssa in arrivals and ssa not in inst_out_Lookup):
                        continue
                    if ssa in eval_stack:
                        continue
                    eval_stack.add(ssa)
                    stack.append((ssa, True))
                    if ssa in inst_out_Lookup:
                        coupled = inst_out_Lookup[ssa]
                        for a, _, _ in reversed(coupled):
                            if (
                                a not in constants
                                and not (a in arrivals and a not in inst_out_Lookup)
                                and a not in eval_stack
                            ):
                                stack.append((a, False))
                    elif ssa in node_defs:
                        _, args, _, _, _, _ = node_defs[ssa]
                        for a in reversed(args):
                            if (
                                a not in constants
                                and not (a in arrivals and a not in inst_out_Lookup)
                                and a not in eval_stack
                            ):
                                stack.append((a, False))
                else:
                    seed_reg, seed_mem, seed_in = arrivals.get(ssa, EMPTY_ARR)
                    best_reg = seed_reg
                    best_mem = seed_mem
                    best_in = dict(seed_in) if seed_in else {}
                    fo_pen = fanout_fo4(ssa)

                    if ssa in inst_out_Lookup:
                        coupled = inst_out_Lookup.pop(ssa)
                        for a, c_fo4, c_lv in coupled:
                            if a in constants:
                                continue
                            a_reg, a_mem, a_in = arrivals.get(a, EMPTY_ARR)
                            step_fo4 = c_fo4 + fo_pen
                            if a_reg is not None:
                                cand = (a_reg[0] + step_fo4, a_reg[1] + c_lv, a_reg[2])
                                if best_reg is None or cand[0] > best_reg[0]:
                                    best_reg = cand
                            if a_mem is not None:
                                cand = (a_mem[0] + step_fo4, a_mem[1] + c_lv, a_mem[2])
                                if best_mem is None or cand[0] > best_mem[0]:
                                    best_mem = cand
                            for p_in, (if4, ilv) in a_in.items():
                                cand_in = (if4 + step_fo4, ilv + c_lv)
                                prev_in = best_in.get(p_in)
                                if prev_in is None or cand_in[0] > prev_in[0]:
                                    best_in[p_in] = cand_in
                    elif ssa in node_defs:
                        op, args, w, d_fo4, lv, _ = node_defs.pop(ssa)
                        for idx, a in enumerate(args):
                            if a in constants:
                                continue
                            a_reg, a_mem, a_in = arrivals.get(a, EMPTY_ARR)
                            if a_reg is None and a_mem is None and not a_in:
                                continue
                            edge_fo4 = ((d_fo4 + 0.15) if (op == "comb.mux" and idx == 0) else d_fo4) + fo_pen
                            if a_reg is not None:
                                cand = (a_reg[0] + edge_fo4, a_reg[1] + lv, a_reg[2])
                                if best_reg is None or cand[0] > best_reg[0]:
                                    best_reg = cand
                            if a_mem is not None:
                                cand = (a_mem[0] + edge_fo4, a_mem[1] + lv, a_mem[2])
                                if best_mem is None or cand[0] > best_mem[0]:
                                    best_mem = cand
                            for p_in, (if4, ilv) in a_in.items():
                                cand_in = (if4 + edge_fo4, ilv + lv)
                                prev_in = best_in.get(p_in)
                                if prev_in is None or cand_in[0] > prev_in[0]:
                                    best_in[p_in] = cand_in

                    eval_stack.discard(ssa)
                    arrivals[ssa] = (best_reg, best_mem, _cap_in_map(best_in))
            return arrivals.get(root_ssa, EMPTY_ARR)

        path_groups = {}
        in_to_reg_map = {}
        in_to_mem_map = {}

        def record_group(grp, fo4_val, lv_val, sp, ep, loc_tok=None):
            prev = path_groups.get(grp)
            if prev is None or fo4_val > prev["fo4"]:
                loc_str = ""
                if loc_tok:
                    if loc_tok[0] == "str":
                        loc_str = loc_tok[1]
                    elif loc_tok[0] == "ref" and loc_tok[1] in loc_defs:
                        loc_str = loc_defs[loc_tok[1]][1]
                path_groups[grp] = {
                    "fo4": round(fo4_val, 2),
                    "levels": int(lv_val),
                    "startpoint": sp,
                    "endpoint": ep,
                    "loc": loc_str,
                }

        def record_in_sink(sink_map, grp_or_none, p_in, fo4_val, lv_val, ep_name, loc_tok=None):
            prev = sink_map.get(p_in)
            if prev is None or fo4_val > prev[0]:
                sink_map[p_in] = (fo4_val, int(lv_val), ep_name)
            if grp_or_none:
                record_group(grp_or_none, fo4_val, lv_val, p_in, ep_name, loc_tok)

        # 1. Register endpoints (own registers)
        for reg_ssa, next_ssa, w, loc_tok in regs:
            if not next_ssa:
                continue
            ep_name = reg_ssa.lstrip("%")
            a_reg, a_mem, a_in = get_arr(next_ssa)
            if a_reg is not None:
                record_group("reg2reg", a_reg[0] + REG_SETUP_FO4, a_reg[1], a_reg[2], ep_name, loc_tok)
            if a_mem is not None:
                record_group("mem2reg", a_mem[0] + REG_SETUP_FO4, a_mem[1], a_mem[2], ep_name, loc_tok)
            for p_in, (if4, ilv) in a_in.items():
                record_in_sink(in_to_reg_map, "in2reg", p_in, if4 + REG_SETUP_FO4, ilv, ep_name, loc_tok)

        # 2. Child instance endpoints (SRAM inputs -> reg2mem/mem2mem; child per-port in2reg/in2mem)
        for inst_name, child_mod, in_binds, out_names, res_ssas, loc_tok in instances:
            if _match_mem_dims(child_mod) is not None:
                ep_mem = f"{inst_name}/{child_mod}"
                for _, u in in_binds:
                    a_reg, a_mem, a_in = get_arr(u)
                    if a_reg is not None:
                        record_group("reg2mem", a_reg[0] + MEM_SETUP_FO4, a_reg[1], a_reg[2], ep_mem, loc_tok)
                    if a_mem is not None:
                        record_group("mem2mem", a_mem[0] + MEM_SETUP_FO4, a_mem[1], a_mem[2], ep_mem, loc_tok)
                    for p_in, (if4, ilv) in a_in.items():
                        record_in_sink(in_to_mem_map, None, p_in, if4 + MEM_SETUP_FO4, ilv, ep_mem, loc_tok)
            else:
                csum = eval_module(child_mod)
                if csum is not None:
                    for internal_g in ("reg2reg", "mem2reg", "reg2mem", "mem2mem"):
                        cg = csum["path_groups"].get(internal_g)
                        if cg:
                            record_group(
                                internal_g,
                                cg["fo4"],
                                cg["levels"],
                                f"{inst_name}/{cg['startpoint']}",
                                f"{inst_name}/{cg['endpoint']}",
                            )
                    c_in2reg = csum["_in_to_reg"]
                    c_in2mem = csum["_in_to_mem"]
                    for p_child, u in in_binds:
                        reg_sink = c_in2reg.get(p_child)
                        mem_sink = c_in2mem.get(p_child)
                        if reg_sink is None and mem_sink is None:
                            continue
                        a_reg, a_mem, a_in = get_arr(u)
                        if reg_sink is not None:
                            sf4, slv, sep = reg_sink
                            ep_full = f"{inst_name}/{sep}"
                            if a_reg is not None:
                                record_group("reg2reg", a_reg[0] + sf4, a_reg[1] + slv, a_reg[2], ep_full, loc_tok)
                            if a_mem is not None:
                                record_group("mem2reg", a_mem[0] + sf4, a_mem[1] + slv, a_mem[2], ep_full, loc_tok)
                            for p_in, (if4, ilv) in a_in.items():
                                record_in_sink(in_to_reg_map, "in2reg", p_in, if4 + sf4, ilv + slv, ep_full, loc_tok)
                        if mem_sink is not None:
                            sf4, slv, sep = mem_sink
                            ep_full = f"{inst_name}/{sep}"
                            if a_reg is not None:
                                record_group("reg2mem", a_reg[0] + sf4, a_reg[1] + slv, a_reg[2], ep_full, loc_tok)
                            if a_mem is not None:
                                record_group("mem2mem", a_mem[0] + sf4, a_mem[1] + slv, a_mem[2], ep_full, loc_tok)
                            for p_in, (if4, ilv) in a_in.items():
                                record_in_sink(in_to_mem_map, None, p_in, if4 + sf4, ilv + slv, ep_full, loc_tok)

        # 3. Module output endpoints (`reg2out`, `in2out`, per-port ILM maps)
        out_reg_map = {}
        out_mem_map = {}
        in_to_out_map = {}
        for idx, o_ssa in enumerate(out_ssas):
            o_name = outputs[idx][0] if idx < len(outputs) else f"out_{idx}"
            a_reg, a_mem, a_in = get_arr(o_ssa)
            if a_reg is not None:
                out_reg_map[o_name] = a_reg
                record_group("reg2out", a_reg[0], a_reg[1], a_reg[2], o_name)
            if a_mem is not None:
                out_mem_map[o_name] = a_mem
            if a_in:
                in_to_out_map[o_name] = a_in
                for p_in, (if4, ilv) in a_in.items():
                    record_group("in2out", if4, ilv, p_in, o_name)

        # Primary headline FO4: internal sequential cycle (`reg2reg`, `mem2reg`, `reg2mem`, `mem2mem`),
        # falling back to boundary paths (`in2reg`, `reg2out`, `in2out`) for purely combinational / single-stage modules.
        seq_candidates = [
            path_groups[g]["fo4"]
            for g in ("reg2reg", "mem2reg", "reg2mem", "mem2mem")
            if g in path_groups
        ]
        bnd_candidates = [
            path_groups[g]["fo4"]
            for g in ("in2reg", "reg2out", "in2out")
            if g in path_groups
        ]
        primary_fo4 = max(seq_candidates) if seq_candidates else (max(bnd_candidates) if bnd_candidates else 0.0)
        primary_grp = None
        for g in ("reg2reg", "mem2reg", "reg2mem", "mem2mem", "in2reg", "reg2out", "in2out"):
            if g in path_groups and path_groups[g]["fo4"] == primary_fo4:
                primary_grp = g
                break

        srams_list = [
            {"module": m, "depth": d, "width": w, "instances": cnt, "bits": d * w * cnt}
            for (m, d, w), cnt in sorted(sram_counter.items())
        ]
        sram_bits = sum(x["bits"] for x in srams_list)
        sram_count = sum(x["instances"] for x in srams_list)
        port_bits = sum(b for _, b, is_c in inputs if not is_c) + sum(b for _, b in outputs)
        max_fo = max(bit_fanout.values()) if bit_fanout else 0

        res = {
            "module": mod_name,
            "stage": "fir",
            "fo4": round(primary_fo4, 2),
            "primary_group": primary_grp or "-",
            "levels": path_groups[primary_grp]["levels"] if primary_grp else 0,
            "flops": total_flops,
            "flop_bits": total_flop_bits,
            "comb_cells": total_comb_ge,
            "icgs": total_icgs,
            "sram_count": sram_count,
            "sram_bits": sram_bits,
            "srams": srams_list,
            "max_fanout": max_fo,
            "port_bits": port_bits,
            "mem2mem_hazard": "mem2mem" in path_groups,
            "path_groups": path_groups,
            "own_source_files": sorted(own_srcs),
            "source_files": sorted(all_srcs),
            "_sram_counter": sram_counter,
            "_out_reg": out_reg_map,
            "_out_mem": out_mem_map,
            "_in_to_out": in_to_out_map,
            "_in_to_reg": in_to_reg_map,
            "_in_to_mem": in_to_mem_map,
        }
        visiting.discard(mod_name)
        memo[mod_name] = res
        return res

    names_to_eval = target_modules if target_modules else sorted(raw_modules.keys())
    all_results = {}
    for m in names_to_eval:
        if m in raw_modules and _match_mem_dims(m) is None:
            r = eval_module(m)
            if r is not None:
                # Strip internal helper keys
                clean = {k: v for k, v in r.items() if not k.startswith("_")}
                all_results[m] = clean

    if panel_modules:
        panel_set = set(panel_modules)
        results = {m: all_results[m] for m in panel_modules if m in all_results}
        extra_results = {m: v for m, v in all_results.items() if m not in panel_set}
    else:
        results = all_results
        extra_results = {}

    elapsed = round(time.monotonic() - t0, 3)
    per_mod_s = round(elapsed / max(len(results), 1), 4)
    for m in all_results.values():
        m["elapsed_s"] = per_mod_s
        m["total_fir_parse_s"] = elapsed

    out = {
        "stage": "fir",
        "elapsed_s": elapsed,
        "module_count": len(results),
        "modules": results,
    }
    if extra_results:
        out["extra_modules"] = extra_results
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description="Estimate per-module FO4 & PPA from CIRCT .fir.")
    ap.add_argument("--fir", required=True, help="Input .fir (CIRCT HW/Comb/Seq MLIR) file.")
    ap.add_argument("--out", required=True, help="Output JSON path.")
    ap.add_argument("--modules", nargs="*", help="Optional subset of module names to emit.")
    ap.add_argument(
        "--panel-modules",
        nargs="*",
        help="Primary panel module names (`modules`), while emitting remaining modules in `extra_modules`.",
    )
    args = ap.parse_args(argv)

    with open(args.fir, "r", encoding="utf-8", errors="replace") as fh:
        data = analyze_mlir_text(
            fh,
            target_modules=args.modules or None,
            panel_modules=args.panel_modules or None,
        )

    with open(args.out, "w", encoding="utf-8") as out_fh:
        json.dump(data, out_fh, indent=2, sort_keys=True)
        out_fh.write("\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())

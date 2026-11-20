#!/usr/bin/env python3
"""The branch and jump units' mispredict compares without the adder.

BranchUnit's isMisPred and JumpUnit's targetWrong compare the low 50
bits of a sum with the predicted target: pc + imm, and (jalr's src_0 or
the pc) + imm. Written as an add and a compare, synthesis computes the
sum first: yosys's share merges BranchUnit's pc + imm with
AddrAddModule's taken-muxed adder, so the compare waits for taken, and
ABC maps the 50-bit compare as a chain. Both sit on the parent's paths
into Frontend's Ftq and ResolveQueue.

(A + B)[n-1:0] == K needs no sum: per bit, the carry the sum needs there
is Need = A ^ B ^ K, and the carry the bit then makes is
Made = A & B | (A | B) & ~K; the sum is K exactly when bit 0 needs no
carry in and every bit makes the carry the next one needs:
~Need[0] & Made[n-2:0] == Need[n-1:1]. This script writes each compare
that way, from the operands the RTL adds, and leaves the adders for the
targets the units send on.

    xs_bju_surgery.py <in.sv> <out.sv>
    xs_bju_surgery.py --unit BranchUnit [--control] <in.sv> <out.sv>

Without --unit both units are edited, as every XiangShan flow reads them
(surgery:xiangshan_bju). --unit edits one, for its module-size LEC; with
--control, the edit is broken on purpose, the negative control that LEC
must find different: BranchUnit without the bit-0 check, JumpUnit with
its operand mux the wrong way round.

Each unit's text, the ports it reads and the adder it replaces are named
exactly below; anything else stops with the shape named, a second copy
of a unit or of AddrAddModule under firtool's <name>_<n> included, so a
new XiangShan reapplies or stops, never skips. Python 3.6.
"""

import argparse
import os
import re
import sys

# beside this file; rules_python does not put its directory on the path
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from xs_surgery import SurgeryError, parse_header  # noqa: E402

# AddrAddModule as BranchUnit's compare relies on it: taken, the target
# is pcExtend + imm[14:0] sign-extended, and target[49:0] its low bits.
ADDR_ADD = """  wire [51:0] _io_target_T_7 =
    io_taken
      ? io_pcExtend + {{37{io_imm[14]}}, io_imm[14:0]}
      : io_pcExtend + {45'h0, io_nextPcOffset};
  assign io_target = {{12{_io_target_T_7[51]}}, _io_target_T_7};
"""

# BranchUnit's instance of it: pcExtend's low 50 bits are the pc, taken
# is BranchModule's, and _addModule_io_target is its target.
ADDR_ADD_USE = """  AddrAddModule addModule (
    .io_pcExtend
      ({{2{(io_instrAddrTransType_sv39 | io_instrAddrTransType_sv48)
             & io_in_bits_data_pc[49]}},
        io_in_bits_data_pc}),
    .io_taken        (_dataModule_io_taken),
    .io_imm          (io_in_bits_data_imm[31:0]),
    .io_target       (_addModule_io_target),
    .io_nextPcOffset (io_in_bits_data_nextPcOffset)
  );
"""

ISMISPRED = """  wire        isMisPred =
    _dataModule_io_mispredict | io_in_bits_ctrl_predictInfo_predTaken
    & _dataModule_io_taken
    & _addModule_io_target[49:0] != io_in_bits_ctrl_predictInfo_target;
"""

ISMISPRED_NEW = """  // surgery (xs_bju_surgery.py): the predicted target against the taken
  // target, pc + imm, the only one the AND with taken lets through,
  // without an adder: (A + B)[49:0] == K when bit 0 needs no carry in and
  // each bit's carry out, made with the carry in the sum needs there, is
  // the carry in the next bit needs
  wire [49:0] _tgtA = io_in_bits_data_pc;
  wire [49:0] _tgtB = {{35{io_in_bits_data_imm[14]}}, io_in_bits_data_imm[14:0]};
  wire [49:0] _tgtK = io_in_bits_ctrl_predictInfo_target;
  wire [49:0] _tgtNeed = _tgtA ^ _tgtB ^ _tgtK;
  wire [49:0] _tgtMade = _tgtA & _tgtB | (_tgtA | _tgtB) & ~_tgtK;
  wire        _takenTargetHit = ~_tgtNeed[0] & _tgtMade[48:0] == _tgtNeed[49:1];
  wire        isMisPred =
    _dataModule_io_mispredict | io_in_bits_ctrl_predictInfo_predTaken
    & _dataModule_io_taken
    & ~_takenTargetHit;
"""

# JumpUnit's target, then its compare: the edit replaces the compare.
JUMP = """  wire [63:0] _jumpTarget_T_2 =
    (io_in_bits_ctrl_fuOpType[0]
       ? io_in_bits_data_src_0
       : {{14{(io_instrAddrTransType_sv39 | io_instrAddrTransType_sv48)
                & io_in_bits_data_pc[49]}},
          io_in_bits_data_pc}) + io_in_bits_data_imm;
  wire        targetWrong = _jumpTarget_T_2[49:0] != io_in_bits_ctrl_predictInfo_target;
"""

TARGET_WRONG = """  wire        targetWrong = _jumpTarget_T_2[49:0] != io_in_bits_ctrl_predictInfo_target;
"""

TARGET_WRONG_NEW = """  // surgery (xs_bju_surgery.py): (operand + imm)[49:0] against the
  // predicted target without the adder, as in BranchUnit; the operand mux
  // by fuOpType[0] (jalr's src_0 or the pc) stays in front of it
  wire [49:0] _jmpA = io_in_bits_ctrl_fuOpType[0] ? io_in_bits_data_src_0[49:0] : io_in_bits_data_pc;
  wire [49:0] _jmpB = io_in_bits_data_imm[49:0];
  wire [49:0] _jmpK = io_in_bits_ctrl_predictInfo_target;
  wire [49:0] _jmpNeed = _jmpA ^ _jmpB ^ _jmpK;
  wire [49:0] _jmpMade = _jmpA & _jmpB | (_jmpA | _jmpB) & ~_jmpK;
  wire        targetWrong = ~(~_jmpNeed[0] & _jmpMade[48:0] == _jmpNeed[49:1]);
"""

# The negative controls, each one exact edit of the surgery's own text.
CONTROLS = {
    "BranchUnit": ("~_tgtNeed[0] & _tgtMade", "_tgtMade"),
    "JumpUnit": (
        "io_in_bits_ctrl_fuOpType[0] ? io_in_bits_data_src_0[49:0] : io_in_bits_data_pc",
        "io_in_bits_ctrl_fuOpType[0] ? io_in_bits_data_pc : io_in_bits_data_src_0[49:0]",
    ),
}

# The ports each compare reads, at the widths the operands above assume.
PORTS = {
    "BranchUnit": {
        "io_in_bits_data_pc": "[49:0]",
        "io_in_bits_data_imm": "[63:0]",
        "io_in_bits_ctrl_predictInfo_target": "[49:0]",
    },
    "JumpUnit": {
        "io_in_bits_ctrl_fuOpType": "[11:0]",
        "io_in_bits_data_src_0": "[63:0]",
        "io_in_bits_data_pc": "[49:0]",
        "io_in_bits_data_imm": "[63:0]",
        "io_in_bits_ctrl_predictInfo_target": "[49:0]",
    },
}
ADDR_ADD_PORTS = {
    "io_pcExtend": "[51:0]",
    "io_imm": "[31:0]",
    "io_target": "[63:0]",
}

UNITS = ("BranchUnit", "JumpUnit")


def module_span(text, name):
    """Where module `name` is defined, exactly once: (start, end). A copy
    firtool writes of it under another name, <name>_<n>, stops the edit
    too: it would be left unedited without a word."""
    copies = re.findall(r"^module (%s_[0-9]+)\(" % re.escape(name), text, re.M)
    if copies:
        raise SurgeryError(
            "module %s has firtool copies the surgery does not edit: %s"
            % (name, ", ".join(copies))
        )
    found = [
        m.start() for m in re.finditer(r"^module %s\(" % re.escape(name), text, re.M)
    ]
    if len(found) != 1:
        raise SurgeryError(
            "module %s defined %d times, expected once" % (name, len(found))
        )
    end = text.find("\nendmodule\n", found[0])
    if end < 0:
        raise SurgeryError("module %s is not closed" % name)
    return found[0], end + len("\nendmodule\n")


def check_ports(module, chunk, widths):
    header = chunk[chunk.index("(") + 1 : chunk.index("\n);")]
    ports = {n: (d, r) for d, r, n in parse_header(header)}
    for port, rng in sorted(widths.items()):
        if port not in ports or ports[port][1] != rng:
            raise SurgeryError(
                "%s: port %s is %s, the surgery reads it as %s"
                % (module, port, ports.get(port, ("absent", ""))[1] or "absent", rng)
            )


def replace_once(chunk, old, new, what):
    n = chunk.count(old)
    if n != 1:
        raise SurgeryError("%s: expected shape found %d times" % (what, n))
    return chunk.replace(old, new)


def edit_branch_unit(text, control=False):
    s, e = module_span(text, "AddrAddModule")
    adder = text[s:e]
    check_ports("AddrAddModule", adder, ADDR_ADD_PORTS)
    replace_once(adder, ADDR_ADD, ADDR_ADD, "AddrAddModule's target")
    s, e = module_span(text, "BranchUnit")
    unit = text[s:e]
    check_ports("BranchUnit", unit, PORTS["BranchUnit"])
    replace_once(unit, ADDR_ADD_USE, ADDR_ADD_USE, "BranchUnit's AddrAddModule")
    body = ISMISPRED_NEW
    if control:
        body = replace_once(body, *CONTROLS["BranchUnit"], what="BranchUnit's control")
    unit = replace_once(unit, ISMISPRED, body, "BranchUnit's isMisPred")
    return text[:s] + unit + text[e:]


def edit_jump_unit(text, control=False):
    s, e = module_span(text, "JumpUnit")
    unit = text[s:e]
    check_ports("JumpUnit", unit, PORTS["JumpUnit"])
    replace_once(unit, JUMP, JUMP, "JumpUnit's target and targetWrong")
    body = TARGET_WRONG_NEW
    if control:
        body = replace_once(body, *CONTROLS["JumpUnit"], what="JumpUnit's control")
    unit = replace_once(unit, TARGET_WRONG, body, "JumpUnit's targetWrong")
    return text[:s] + unit + text[e:]


EDITS = {"BranchUnit": edit_branch_unit, "JumpUnit": edit_jump_unit}


def surgery(text, units=UNITS, control=False):
    if control and len(units) != 1:
        raise SurgeryError("--control breaks one unit: name it with --unit")
    for unit in units:
        text = EDITS[unit](text, control)
    return text


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    parser.add_argument("--unit", action="append", choices=UNITS)
    parser.add_argument("--control", action="store_true")
    parser.add_argument("src")
    parser.add_argument("out")
    args = parser.parse_args(argv)
    with open(args.src) as f:
        text = f.read()
    try:
        text = surgery(text, tuple(args.unit or UNITS), args.control)
    except SurgeryError as e:
        sys.exit("xs_bju_surgery: %s" % e)
    with open(args.out, "w") as f:
        f.write(text)


if __name__ == "__main__":
    main()

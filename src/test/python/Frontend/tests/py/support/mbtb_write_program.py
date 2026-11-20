"""Legal far JALR program for equal/change MainBtb write-buffer checks.

MainBtb stores twenty halfword target bits plus a sign carry, reconstructing
only the current or adjacent 2 MiB region.  A target two regions above the
source remains a real indirect-target misprediction when the other predictors
are disabled.  Repeating that target requests equal writes; adding four bytes
requests a changed lower target.  The oracle follows the actual program, never
the deliberately limited predictor reconstruction.
"""
from env.model.golden_trace import GoldenTrace, TraceEntry


def _addi(rd, rs1, immediate):
    assert -2048 <= immediate < 2048
    return ((immediate & 4095) << 20) | (rs1 << 15) | (rd << 7) | 0x13


def _bne(rs1, rs2, offset):
    assert offset % 2 == 0 and -4096 <= offset < 4096
    value = offset & 8191
    return (((value >> 12) << 31) | (((value >> 5) & 63) << 25)
            | (rs2 << 20) | (rs1 << 15) | (1 << 12)
            | (((value >> 1) & 15) << 8) | (((value >> 11) & 1) << 7) | 0x63)


def _jal(offset):
    assert offset % 2 == 0 and -(1 << 20) <= offset < (1 << 20)
    value = offset & ((1 << 21) - 1)
    return (((value >> 20) << 31) | (((value >> 1) & 1023) << 21)
            | (((value >> 11) & 1) << 20) | (((value >> 12) & 255) << 12) | 0x6f)


def build_mbtb_write_compare_program(base=0x80000000, *, stable_iterations=16, changed_iterations=16):
    """Return executable bytes, a bounded architectural trace, and source facts.

    This is a finite prefix of a real program.  x6 holds the full JALR target,
    x8 the return address of the source loop, and x7 selects the +4 transition.
    No branch result, FTQ pointer, or resolve is supplied by a test-side guess.
    """
    assert base % (1 << 21) == 0 and 3 <= stable_iterations < 2048 and changed_iterations >= 2
    source, target = base + 0x40, base + 0x401000
    return_stub = target + 0x30
    instructions = {
        base: 0x00401317,                 # auipc x6,0x401 -> base+0x401000
        base + 4: 0x00000417,             # auipc x8,0 -> base+4
        base + 8: _addi(8, 8, 0x3c),      # x8 -> source
        base + 12: _addi(7, 0, stable_iterations),
        base + 16: _jal(source - (base + 16)),
        source: 0x00030067,               # jalr x0,x6,0: OtherIndirect, no RAS
        target: 0x00000013,
        target + 4: _addi(7, 7, -1),
        target + 8: _bne(7, 0, return_stub - (target + 8)),
        target + 12: _addi(6, 6, 4),      # same source, lower target changes by 2 halfwords
        target + 16: 0x00040067,          # jalr x0,x8,0 -> source
        return_stub: 0x00040067,
    }
    image = bytearray((0x00000013).to_bytes(4, "little") * ((0x402000) // 4))
    for pc, instruction in instructions.items():
        image[pc - base:pc - base + 4] = instruction.to_bytes(4, "little")
    entries = []

    def append(pc, kind="normal", taken=False, next_pc=None):
        entries.append(TraceEntry(len(entries), pc, instructions[pc], 4, kind, taken, next_pc))

    for pc in (base, base + 4, base + 8, base + 12):
        append(pc)
    append(base + 16, "jump", True, source)
    for index in range(stable_iterations):
        append(source, "jump_indirect", True, target)
        append(target)
        append(target + 4)
        taken = index < stable_iterations - 1
        append(target + 8, "branch", taken, return_stub if taken else target + 12)
        if taken:
            append(return_stub, "jump_indirect", True, source)
    append(target + 12)
    append(target + 16, "jump_indirect", True, source)
    switch_index = len(entries)
    for _ in range(changed_iterations):
        append(source, "jump_indirect", True, target + 4)
        append(target + 4)
        append(target + 8, "branch", True, return_stub)
        append(return_stub, "jump_indirect", True, source)
    return bytes(image), GoldenTrace(entries), {
        "source_pc": source, "source_instruction": instructions[source],
        "initial_target": target, "changed_target": target + 4,
        "switch_trace_index": switch_index,
        "stable_iterations": stable_iterations, "changed_iterations": changed_iterations,
        "mbtb_target_width": 20, "mbtb_predicted_initial_target": base + 0x201000,
        "mbtb_predicted_changed_target": base + 0x201004,
        # Frozen c242 native addr is halfword encoded: [14:7] set, [30:15] tag.
        # CFI position is the final halfword of a 32-bit instruction.
        "align_bank": (source >> 5) & 1, "internal_bank": (source >> 6) & 3,
        "set_idx": (source >> 8) & 255, "tag": (source >> 16) & 65535,
        "position": ((source + 2) >> 1) & 15, "target_carry": 1,
    }

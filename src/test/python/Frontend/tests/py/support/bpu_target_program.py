"""A real repeated JALR whose architectural target later changes by four bytes.

The trace is derived from this fixed AUIPC/ADDI/JALR/BNE/JAL program, independent
of predictor outputs. No host-injected resolve or DUT observation supplies it.
"""
from env.model.golden_trace import GoldenTrace, TraceEntry


def _jal(offset):
    assert offset % 2 == 0 and -(1 << 20) <= offset < (1 << 20)
    imm = offset & ((1 << 21) - 1)
    return (((imm >> 20) << 31) | (((imm >> 1) & 1023) << 21)
            | (((imm >> 11) & 1) << 20) | (((imm >> 12) & 255) << 12) | 0x6f)


def _bne_x7_x0(offset):
    assert offset % 2 == 0 and -(1 << 12) <= offset < (1 << 12)
    imm = offset & 8191
    return (((imm >> 12) << 31) | (((imm >> 5) & 63) << 25) | (7 << 15)
            | (1 << 12) | (((imm >> 1) & 15) << 8) | (((imm >> 11) & 1) << 7) | 0x63)


def build_bpu_target_change_program(base=0x80000000, *, stable_iterations=32, changed_iterations=32):
    assert base % 4096 == 0 and 2 <= stable_iterations < 2048 and changed_iterations >= 2
    source, target = base + 0x40, base + 0x1000
    instructions = {
        base: 0x00001317,                 # auipc x6,1 -> target
        base + 4: (stable_iterations << 20) | (7 << 7) | 0x13,  # addi x7,x0,N
        base + 8: _jal(source - (base + 8)),
        source: 0x00030067,               # jalr x0,x6,0: OtherIndirect
        target: 0x00000013,               # nop
        target + 4: 0xfff38393,           # addi x7,x7,-1
        target + 8: _bne_x7_x0(source - (target + 8)),
        target + 12: 0x00430313,          # addi x6,x6,4: same JALR, new target
        target + 16: _jal(source - (target + 16)),
    }
    image = bytearray((0x00000013).to_bytes(4, "little") * (0x2000 // 4))
    for pc, instruction in instructions.items():
        image[pc - base:pc - base + 4] = instruction.to_bytes(4, "little")
    entries = []

    def append(pc, kind="normal", taken=False, next_pc=None):
        entries.append(TraceEntry(len(entries), pc, instructions[pc], 4, kind, taken, next_pc))

    append(base)
    append(base + 4)
    append(base + 8, "jump", True, source)
    for index in range(stable_iterations):
        append(source, "jump_indirect", True, target)
        append(target)
        append(target + 4)
        taken = index < stable_iterations - 1
        append(target + 8, "branch", taken, source if taken else target + 12)
    append(target + 12)
    append(target + 16, "jump", True, source)
    switch_index = len(entries)
    for _ in range(changed_iterations):
        append(source, "jump_indirect", True, target + 4)
        append(target + 4)
        append(target + 8, "branch", True, source)
    return bytes(image), GoldenTrace(entries), {
        "source_pc": source, "initial_target": target, "changed_target": target + 4,
        "switch_trace_index": switch_index, "stable_iterations": stable_iterations,
        "changed_iterations": changed_iterations,
    }

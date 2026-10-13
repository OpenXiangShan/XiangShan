"""Instruction encoders and program layouts owned by directed ICache tests."""

from __future__ import annotations

from collections.abc import Iterable

from env.core.transactions import ProgramImage

CACHEABLE_BASE = 0x80040000
DUAL_FETCH_BASE = 0x80060000
BPU_TRAINING_BASE = 0x80000000
BPU_TRAINING_BLOCK_BYTES = 64
BPU_TRAINING_BLOCK_COUNT = 8
NOP = 0x00000013
CNOP = 0x0001


def encode_c_j(offset: int) -> int:
    """Encode the RISC-V compressed jump used by the training stream."""
    assert int(offset) % 2 == 0
    assert -(1 << 11) <= int(offset) < (1 << 11)
    imm = int(offset) & 0xFFF
    return (
        (0b101 << 13)
        | (((imm >> 11) & 0x1) << 12)
        | (((imm >> 4) & 0x1) << 11)
        | (((imm >> 8) & 0x3) << 9)
        | (((imm >> 10) & 0x1) << 8)
        | (((imm >> 6) & 0x1) << 7)
        | (((imm >> 7) & 0x1) << 6)
        | (((imm >> 1) & 0x7) << 3)
        | (((imm >> 5) & 0x1) << 2)
        | 0b01
    )


def build_bpu_training_program() -> bytes:
    """Build eight 64-byte blocks with a taken C.J at byte offset 30.

    Each jump skips the remaining 32 bytes; the final jump returns to the
    first block. Repetition trains predictions without register execution.
    """
    halfwords: list[int] = []
    for block in range(BPU_TRAINING_BLOCK_COUNT):
        halfwords.extend([CNOP] * 15)
        branch_pc = block * BPU_TRAINING_BLOCK_BYTES + 30
        target = (
            (block + 1) * BPU_TRAINING_BLOCK_BYTES
            if block + 1 < BPU_TRAINING_BLOCK_COUNT
            else 0
        )
        halfwords.append(encode_c_j(target - branch_pc))
        halfwords.extend([CNOP] * 16)
    assert len(halfwords) * 2 == BPU_TRAINING_BLOCK_BYTES * BPU_TRAINING_BLOCK_COUNT
    return b"".join(int(halfword).to_bytes(2, "little") for halfword in halfwords)


def encode_jal(rd: int, offset: int) -> int:
    """Encode JAL rd, offset (PC-relative, must be even)."""
    imm = offset & 0x1FFFFF
    return (
        (((imm >> 20) & 0x1) << 31)
        | (((imm >> 1) & 0x3FF) << 21)
        | (((imm >> 11) & 0x1) << 20)
        | (((imm >> 12) & 0xFF) << 12)
        | (rd << 7)
        | 0x6F
    )


def encode_beq(rs1: int, rs2: int, offset: int) -> int:
    """Encode BEQ rs1, rs2, offset (PC-relative, must be even)."""
    imm = offset & 0x1FFF
    return (
        (((imm >> 12) & 0x1) << 31)
        | (((imm >> 5) & 0x3F) << 25)
        | (rs2 << 20)
        | (rs1 << 15)
        | (((imm >> 1) & 0xF) << 8)
        | (((imm >> 11) & 0x1) << 7)
        | 0x63
    )


def encode_bne(rs1: int, rs2: int, offset: int) -> int:
    """Encode BNE rs1, rs2, offset (PC-relative, must be even)."""
    imm = offset & 0x1FFF
    return (
        (((imm >> 12) & 0x1) << 31)
        | (((imm >> 5) & 0x3F) << 25)
        | (rs2 << 20)
        | (rs1 << 15)
        | (0x1 << 12)  # funct3 = 1 (BNE)
        | (((imm >> 1) & 0xF) << 8)
        | (((imm >> 11) & 0x1) << 7)
        | 0x63
    )


def instructions_to_bytes(instructions: Iterable[int]) -> bytes:
    buf = bytearray()
    for instr in instructions:
        buf.extend((int(instr) & 0xFFFFFFFF).to_bytes(4, "little"))
    return bytes(buf)


def program_image(
    instructions: Iterable[int], base_addr: int = BPU_TRAINING_BASE
) -> ProgramImage:
    return ProgramImage(
        payload=instructions_to_bytes(instructions), base_addr=base_addr
    )


def build_dual_fetch_program() -> bytes:
    """Build trained-case-equivalent RVC blocks on disjoint data banks.

    Consecutive 32-byte blocks alternate between the upper half of one line
    and the lower half of the next.  Their MainPipe bank masks therefore do
    not trigger the intentional two-request SRAM conflict fallback even when
    adjacent lines occupy the same replacement way.
    """
    payload = bytearray()
    payload.extend((0x02000413).to_bytes(4, "little"))  # addi s0, zero, 32
    payload.extend(NOP.to_bytes(4, "little") * 7)
    for block in range(8):
        payload.extend((0x0001).to_bytes(2, "little") * 15)
        # The final C.J returns to the first aligned block, unlike the BPU loop.
        c_jump = 0xA009 if block != 7 else 0xB709
        payload.extend(int(c_jump).to_bytes(2, "little"))
    return bytes(payload)


def build_nop_program(words: int = 256) -> bytes:
    return NOP.to_bytes(4, "little") * int(words)

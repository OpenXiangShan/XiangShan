"""
RISC-V C (RVC) instruction expander for RV64GC.
Converts a 16-bit compressed instruction to its 32-bit ISA equivalent.
"""
from __future__ import annotations

# 32-bit opcode constants
_OP_LOAD   = 0x03
_OP_STORE  = 0x23
_OP_IMM    = 0x13
_OP_IMM32  = 0x1B  # ADDIW / SLLIW / etc.
_OP_REG    = 0x33
_OP_REG32  = 0x3B  # ADDW / SUBW / etc.
_OP_LUI    = 0x37
_OP_JAL    = 0x6F
_OP_JALR   = 0x67
_OP_BRANCH = 0x63
_OP_SYSTEM = 0x73


def _creg(x: int) -> int:
    """3-bit compressed register field → physical register (x8..x15)."""
    return (x & 0x7) + 8


def _sign_ext(val: int, bits: int) -> int:
    """Sign-extend val from bits-wide field to Python int."""
    mask = 1 << (bits - 1)
    return (val & (mask - 1)) - (val & mask)


# ── 32-bit encoding helpers ───────────────────────────────────────────────────

def _i_type(imm12: int, rs1: int, funct3: int, rd: int, opcode: int) -> int:
    return (
        ((imm12 & 0xFFF) << 20)
        | ((rs1 & 0x1F) << 15)
        | ((funct3 & 0x7) << 12)
        | ((rd & 0x1F) << 7)
        | (opcode & 0x7F)
    )


def _s_type(imm12: int, rs2: int, rs1: int, funct3: int, opcode: int) -> int:
    return (
        (((imm12 >> 5) & 0x7F) << 25)
        | ((rs2 & 0x1F) << 20)
        | ((rs1 & 0x1F) << 15)
        | ((funct3 & 0x7) << 12)
        | ((imm12 & 0x1F) << 7)
        | (opcode & 0x7F)
    )


def _r_type(funct7: int, rs2: int, rs1: int, funct3: int, rd: int, opcode: int) -> int:
    return (
        ((funct7 & 0x7F) << 25)
        | ((rs2 & 0x1F) << 20)
        | ((rs1 & 0x1F) << 15)
        | ((funct3 & 0x7) << 12)
        | ((rd & 0x1F) << 7)
        | (opcode & 0x7F)
    )


def _j_type(imm21: int, rd: int, opcode: int) -> int:
    i = imm21 & 0x1FFFFF
    return (
        (((i >> 20) & 0x1) << 31)
        | (((i >> 1) & 0x3FF) << 21)
        | (((i >> 11) & 0x1) << 20)
        | (((i >> 12) & 0xFF) << 12)
        | ((rd & 0x1F) << 7)
        | (opcode & 0x7F)
    )


def _b_type(imm13: int, rs2: int, rs1: int, funct3: int, opcode: int) -> int:
    i = imm13 & 0x1FFF
    return (
        (((i >> 12) & 0x1) << 31)
        | (((i >> 5) & 0x3F) << 25)
        | ((rs2 & 0x1F) << 20)
        | ((rs1 & 0x1F) << 15)
        | ((funct3 & 0x7) << 12)
        | (((i >> 1) & 0xF) << 8)
        | (((i >> 11) & 0x1) << 7)
        | (opcode & 0x7F)
    )


def _u_type(imm20: int, rd: int, opcode: int) -> int:
    return ((imm20 & 0xFFFFF) << 12) | ((rd & 0x1F) << 7) | (opcode & 0x7F)


# ── Quadrant decoders ─────────────────────────────────────────────────────────

def _q0(i: int) -> int:
    """Q0: bits[1:0] = 00"""
    funct3 = (i >> 13) & 0x7
    rd_s  = (i >> 2) & 0x7   # rd'  (CL / CIW format)
    rs1_s = (i >> 7) & 0x7   # rs1' (CL / CS format)
    rs2_s = (i >> 2) & 0x7   # rs2' (CS format — same bits as rd')

    if funct3 == 0b000:  # C.ADDI4SPN → ADDI rd', x2, nzuimm
        nzuimm = (
            (((i >> 11) & 0x3) << 4)   # bits[12:11] → nzuimm[5:4]
            | (((i >> 7) & 0xF) << 6)  # bits[10:7]  → nzuimm[9:6]
            | (((i >> 6) & 0x1) << 2)  # bit[6]      → nzuimm[2]
            | (((i >> 5) & 0x1) << 3)  # bit[5]      → nzuimm[3]
        )
        if nzuimm == 0:
            raise ValueError(f"C.ADDI4SPN nzuimm=0: illegal {i:#06x}")
        return _i_type(nzuimm, 2, 0, _creg(rd_s), _OP_IMM)

    if funct3 == 0b010:  # C.LW → LW rd', offset[6:2](rs1')
        offset = (
            (((i >> 10) & 0x7) << 3)   # bits[12:10] → offset[5:3]
            | (((i >> 6) & 0x1) << 2)  # bit[6]      → offset[2]
            | (((i >> 5) & 0x1) << 6)  # bit[5]      → offset[6]
        )
        return _i_type(offset, _creg(rs1_s), 2, _creg(rd_s), _OP_LOAD)

    if funct3 == 0b011:  # C.LD (RV64) → LD rd', offset[7:3](rs1')
        offset = (
            (((i >> 10) & 0x7) << 3)   # bits[12:10] → offset[5:3]
            | (((i >> 5) & 0x3) << 6)  # bits[6:5]   → offset[7:6]
        )
        return _i_type(offset, _creg(rs1_s), 3, _creg(rd_s), _OP_LOAD)

    if funct3 == 0b110:  # C.SW → SW rs2', offset[6:2](rs1')
        offset = (
            (((i >> 10) & 0x7) << 3)
            | (((i >> 6) & 0x1) << 2)
            | (((i >> 5) & 0x1) << 6)
        )
        return _s_type(offset, _creg(rs2_s), _creg(rs1_s), 2, _OP_STORE)

    if funct3 == 0b111:  # C.SD (RV64) → SD rs2', offset[7:3](rs1')
        offset = (
            (((i >> 10) & 0x7) << 3)
            | (((i >> 5) & 0x3) << 6)
        )
        return _s_type(offset, _creg(rs2_s), _creg(rs1_s), 3, _OP_STORE)

    raise ValueError(f"Q0 reserved/illegal: {i:#06x}")


def _q1(i: int) -> int:
    """Q1: bits[1:0] = 01"""
    funct3  = (i >> 13) & 0x7
    rd      = (i >> 7) & 0x1F   # full 5-bit register
    rs1_s   = (i >> 7) & 0x7    # compressed rs1' (same bits, lower 3)

    # 6-bit signed immediate (CI format: bit[12] + bits[6:2])
    imm6_raw = (((i >> 12) & 0x1) << 5) | ((i >> 2) & 0x1F)
    imm6     = _sign_ext(imm6_raw, 6)

    if funct3 == 0b000:  # C.NOP (rd=0,imm=0) / C.ADDI → ADDI rd, rd, imm
        return _i_type(imm6 & 0xFFF, rd, 0, rd, _OP_IMM)

    if funct3 == 0b001:  # C.ADDIW (RV64) → ADDIW rd, rd, imm
        if rd == 0:
            raise ValueError(f"C.ADDIW rd=0: reserved {i:#06x}")
        return _i_type(imm6 & 0xFFF, rd, 0, rd, _OP_IMM32)

    if funct3 == 0b010:  # C.LI → ADDI rd, x0, imm
        return _i_type(imm6 & 0xFFF, 0, 0, rd, _OP_IMM)

    if funct3 == 0b011:
        if rd == 2:  # C.ADDI16SP → ADDI x2, x2, nzimm[9:4]
            nzimm = (
                (((i >> 12) & 0x1) << 9)   # bit[12] → nzimm[9]
                | (((i >> 6) & 0x1) << 4)  # bit[6]  → nzimm[4]
                | (((i >> 5) & 0x1) << 6)  # bit[5]  → nzimm[6]
                | (((i >> 3) & 0x3) << 7)  # bits[4:3] → nzimm[8:7]
                | (((i >> 2) & 0x1) << 5)  # bit[2]  → nzimm[5]
            )
            nzimm = _sign_ext(nzimm, 10)
            return _i_type(nzimm & 0xFFF, 2, 0, 2, _OP_IMM)
        elif rd != 0:  # C.LUI → LUI rd, nzimm[17:12]
            nzimm20 = _sign_ext(imm6_raw, 6) & 0xFFFFF
            return _u_type(nzimm20, rd, _OP_LUI)
        raise ValueError(f"Q1 funct3=011 rd=0: reserved {i:#06x}")

    if funct3 == 0b100:
        f2 = (i >> 10) & 0x3

        if f2 == 0b00:  # C.SRLI → SRLI rd', rd', shamt
            shamt = (((i >> 12) & 0x1) << 5) | ((i >> 2) & 0x1F)
            return _i_type(shamt & 0x3F, _creg(rs1_s), 5, _creg(rs1_s), _OP_IMM)

        if f2 == 0b01:  # C.SRAI → SRAI rd', rd', shamt
            shamt = (((i >> 12) & 0x1) << 5) | ((i >> 2) & 0x1F)
            return _i_type(0x400 | (shamt & 0x3F), _creg(rs1_s), 5, _creg(rs1_s), _OP_IMM)

        if f2 == 0b10:  # C.ANDI → ANDI rd', rd', imm
            return _i_type(imm6 & 0xFFF, _creg(rs1_s), 7, _creg(rs1_s), _OP_IMM)

        # f2 == 0b11: register-register ops
        bit12 = (i >> 12) & 0x1
        rs2_s = (i >> 2) & 0x7
        f2b   = (i >> 5) & 0x3
        if bit12 == 0:
            ops = {0b00: (0x20, 0), 0b01: (0x00, 4), 0b10: (0x00, 6), 0b11: (0x00, 7)}
            funct7, f3 = ops[f2b]
            return _r_type(funct7, _creg(rs2_s), _creg(rs1_s), f3, _creg(rs1_s), _OP_REG)
        else:
            if f2b == 0b00:  # C.SUBW
                return _r_type(0x20, _creg(rs2_s), _creg(rs1_s), 0, _creg(rs1_s), _OP_REG32)
            if f2b == 0b01:  # C.ADDW
                return _r_type(0x00, _creg(rs2_s), _creg(rs1_s), 0, _creg(rs1_s), _OP_REG32)
            raise ValueError(f"Q1 funct3=100 f2=11 bit12=1 f2b={f2b}: reserved {i:#06x}")

    if funct3 == 0b101:  # C.J → JAL x0, offset
        off = (
            (((i >> 12) & 0x1) << 11)   # bit[12] → offset[11]
            | (((i >> 11) & 0x1) << 4)  # bit[11] → offset[4]
            | (((i >> 9) & 0x3) << 8)   # bits[10:9] → offset[9:8]
            | (((i >> 8) & 0x1) << 10)  # bit[8]  → offset[10]
            | (((i >> 7) & 0x1) << 6)   # bit[7]  → offset[6]
            | (((i >> 6) & 0x1) << 7)   # bit[6]  → offset[7]
            | (((i >> 3) & 0x7) << 1)   # bits[5:3] → offset[3:1]
            | (((i >> 2) & 0x1) << 5)   # bit[2]  → offset[5]
        )
        return _j_type(_sign_ext(off, 12) & 0x1FFFFF, 0, _OP_JAL)

    if funct3 == 0b110:  # C.BEQZ → BEQ rs1', x0, offset
        rs1_cb = (i >> 7) & 0x7
        off = (
            (((i >> 12) & 0x1) << 8)    # bit[12]   → offset[8]
            | (((i >> 10) & 0x3) << 3)  # bits[11:10] → offset[4:3]
            | (((i >> 5) & 0x3) << 6)   # bits[6:5] → offset[7:6]
            | (((i >> 3) & 0x3) << 1)   # bits[4:3] → offset[2:1]
            | (((i >> 2) & 0x1) << 5)   # bit[2]    → offset[5]
        )
        return _b_type(_sign_ext(off, 9) & 0x1FFF, 0, _creg(rs1_cb), 0, _OP_BRANCH)

    if funct3 == 0b111:  # C.BNEZ → BNE rs1', x0, offset
        rs1_cb = (i >> 7) & 0x7
        off = (
            (((i >> 12) & 0x1) << 8)
            | (((i >> 10) & 0x3) << 3)
            | (((i >> 5) & 0x3) << 6)
            | (((i >> 3) & 0x3) << 1)
            | (((i >> 2) & 0x1) << 5)
        )
        return _b_type(_sign_ext(off, 9) & 0x1FFF, 0, _creg(rs1_cb), 1, _OP_BRANCH)

    raise ValueError(f"Q1 reserved: {i:#06x}")


def _q2(i: int) -> int:
    """Q2: bits[1:0] = 10"""
    funct3 = (i >> 13) & 0x7
    rd     = (i >> 7) & 0x1F
    rs2    = (i >> 2) & 0x1F
    bit12  = (i >> 12) & 0x1

    if funct3 == 0b000:  # C.SLLI → SLLI rd, rd, shamt
        shamt = (bit12 << 5) | ((i >> 2) & 0x1F)
        if rd == 0:
            raise ValueError(f"C.SLLI rd=0: reserved {i:#06x}")
        return _i_type(shamt & 0x3F, rd, 1, rd, _OP_IMM)

    if funct3 == 0b010:  # C.LWSP → LW rd, offset[7:2](x2)
        if rd == 0:
            raise ValueError(f"C.LWSP rd=0: reserved {i:#06x}")
        offset = (
            (bit12 << 5)
            | (((i >> 4) & 0x7) << 2)   # bits[6:4] → offset[4:2]
            | (((i >> 2) & 0x3) << 6)   # bits[3:2] → offset[7:6]
        )
        return _i_type(offset, 2, 2, rd, _OP_LOAD)

    if funct3 == 0b011:  # C.LDSP (RV64) → LD rd, offset[8:3](x2)
        if rd == 0:
            raise ValueError(f"C.LDSP rd=0: reserved {i:#06x}")
        offset = (
            (bit12 << 5)
            | (((i >> 5) & 0x3) << 3)   # bits[6:5] → offset[4:3]
            | (((i >> 2) & 0x7) << 6)   # bits[4:2] → offset[8:6]
        )
        return _i_type(offset, 2, 3, rd, _OP_LOAD)

    if funct3 == 0b100:
        if bit12 == 0:
            if rs2 == 0:  # C.JR → JALR x0, rd, 0
                if rd == 0:
                    raise ValueError(f"C.JR rd=0: reserved {i:#06x}")
                return _i_type(0, rd, 0, 0, _OP_JALR)
            else:         # C.MV → ADD rd, x0, rs2
                return _r_type(0, rs2, 0, 0, rd, _OP_REG)
        else:
            if rd == 0 and rs2 == 0:  # C.EBREAK
                return 0x00100073
            if rs2 == 0:              # C.JALR → JALR x1, rd, 0
                return _i_type(0, rd, 0, 1, _OP_JALR)
            else:                     # C.ADD → ADD rd, rd, rs2
                return _r_type(0, rs2, rd, 0, rd, _OP_REG)

    if funct3 == 0b110:  # C.SWSP → SW rs2, offset[7:2](x2)
        offset = (
            (((i >> 9) & 0xF) << 2)    # bits[12:9] → offset[5:2]
            | (((i >> 7) & 0x3) << 6)  # bits[8:7]  → offset[7:6]
        )
        return _s_type(offset, rs2, 2, 2, _OP_STORE)

    if funct3 == 0b111:  # C.SDSP (RV64) → SD rs2, offset[8:3](x2)
        offset = (
            (((i >> 10) & 0x7) << 3)   # bits[12:10] → offset[5:3]
            | (((i >> 7) & 0x7) << 6)  # bits[9:7]   → offset[8:6]
        )
        return _s_type(offset, rs2, 2, 3, _OP_STORE)

    raise ValueError(f"Q2 reserved/illegal: {i:#06x}")


# ── Public API ────────────────────────────────────────────────────────────────

def expand_rvc(instr16: int) -> int:
    """
    Expand a 16-bit RISC-V C (RVC) instruction to its 32-bit ISA equivalent.

    Targets RV64GC. Raises ValueError for illegal or reserved encodings.
    The caller is responsible for verifying that instr16 is indeed an RVC
    instruction (bits[1:0] != 0b11).
    """
    q = instr16 & 0x3
    if q == 0:
        return _q0(instr16)
    if q == 1:
        return _q1(instr16)
    if q == 2:
        return _q2(instr16)
    raise ValueError(f"Not RVC (bits[1:0]=11): {instr16:#06x}")

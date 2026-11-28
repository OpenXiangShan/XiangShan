// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.ifu.RVCDecoder

import RVCInstructions._
import RviInst._
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.ExpandedInstruction

/** RVC (compressed) -> RV-I expander.
  *
  * The top-level interface is identical to the original
  * `freechips.rocketchip.rocket.RVCDecoder`:
  *
  * {{{
  *   class RVCDecoder(x: UInt, fsIsOff: Bool, xLen: Int, fLen: Int, useAddiForMv: Boolean = false)
  *     def decode      : ExpandedInstruction // 32-bit RV-I encoding of the compressed instruction
  *     def ill         : Bool                // the code point is illegal / reserved
  *     def passthrough : ExpandedInstruction // the 32-bit instruction, returned as is
  * }}}
  *
  * The implementation is table driven and follows the style of `xiangshan.backend.decode`:
  *
  *   - [[RVCInstructions]] holds the BitPat patterns of every instruction, grouped by the nine
  *     RVC formats of the manual (CR/CI/CSS/CIW/CL/CS/CA/CB/CJ).
  *   - [[RVCFormat]] (and its subclasses) extracts the bit fields / immediates of each format.
  *   - [[RviInst]] knows how to splice those immediates into a 32-bit RV-I instruction.
  *   - This class has one expansion function per format (`expandCR`, `expandCI`, ... ), each a
  *     BitPat-keyed mux over the instructions of that format.
  *
  * Only `bits` of the returned [[ExpandedInstruction]] is consumed by XiangShan's IFU; the
  * `rd/rs1/rs2/rs3` fields are informational and are filled with the registers of the expanded
  * instruction. `decode.bits` and `ill` reproduce the original implementation for every code
  * point.
  *
  * @param x           the 32-bit fetch word; the compressed instruction is in its low 16 bits
  * @param fsIsOff     `mstatus.FS == 0` (floating-point state off)
  * @param xLen        XLEN (32 or 64)
  * @param fLen        FLEN (>= 64 enables the D extension)
  * @param useAddiForMv expand c.mv to `addi rd, rs2, 0` instead of `add rd, x0, rs2`
  */
class RVCDecoder(x: UInt, fsIsOff: Bool, xLen: Int, fLen: Int, useAddiForMv: Boolean = false) {
  require(xLen == 32 || xLen == 64, s"RVCDecoder only supports RV32/RV64, got xLen = $xLen")

  // -----------------------------------------------------------------------------------------------
  // The nine format views of the same fetch word. They are plain Scala objects, so they cost no
  // hardware; each one just renames the bit fields of `x` according to its format.
  // -----------------------------------------------------------------------------------------------
  private val cr  = new CRFormat(x)
  private val ci  = new CIFormat(x)
  private val css = new CSSFormat(x)
  private val ciw = new CIWFormat(x)
  private val cl  = new CLFormat(x)
  private val cs  = new CSFormat(x)
  private val ca  = new CAFormat(x)
  private val cb  = new CBFormat(x)
  private val cj  = new CJFormat(x)

  // -----------------------------------------------------------------------------------------------
  // Format-dispatch bits: the RVC encoding space is uniquely determined by the quadrant
  // (inst[1:0]) and funct3 (inst[15:13]); the CA/CB and Zcb sub-formats additionally use funct2.
  // -----------------------------------------------------------------------------------------------
  private val quadrant: UInt = x(1, 0)
  private val funct3:   UInt = x(15, 13)
  private val funct2:   UInt = x(11, 10)

  /** Build the expansion result. `bits` is the 32-bit RV-I encoding; the register fields are the
    * registers of the expanded instruction (informational only, see the class comment).
    */
  private def inst(bits: UInt, rd: UInt, rs1: UInt, rs2: UInt = 0.U(5.W), rs3: UInt = 0.U(5.W)): ExpandedInstruction = {
    val res = Wire(new ExpandedInstruction)
    res.bits := bits
    res.rd   := rd
    res.rs1  := rs1
    res.rs2  := rs2
    res.rs3  := rs3
    res
  }

  /** The 32-bit instruction (quadrant = 11): returned unchanged. */
  def passthrough: ExpandedInstruction =
    inst(x, x(11, 7), x(19, 15), x(24, 20), x(31, 27))

  // ===============================================================================================
  // One expansion function per RVC format.
  // ===============================================================================================

  /** CIW: c.addi4spn. */
  private def expandCIW: ExpandedInstruction = {
    val opcode = Mux(ciw.nzuimmIsZero, OP_RESERVED_1F, OP_IMM)
    inst(iType(ciw.addi4spnImm, ciw.sp, F3_ADD, ciw.rdp, opcode), ciw.rdp, ciw.sp)
  }

  /** CL: the 8-register-set loads (c.lw/c.ld/c.flw/c.fld and the Zcb byte/halfword loads). */
  private def expandCL: ExpandedInstruction = MuxCase(
    passthrough,
    Seq(
      (C_FLD === x)                              -> inst(loadFp(F3_LD, cl.rdp, cl.rs1p, cl.ldImm), cl.rdp, cl.rs1p),
      (C_LW === x)                               -> inst(load(F3_LW, cl.rdp, cl.rs1p, cl.lwImm), cl.rdp, cl.rs1p),
      (if (xLen == 64) C_LD === x else false.B)  -> inst(load(F3_LD, cl.rdp, cl.rs1p, cl.ldImm), cl.rdp, cl.rs1p),
      (if (xLen == 32) C_FLW === x else false.B) -> inst(loadFp(F3_LW, cl.rdp, cl.rs1p, cl.lwImm), cl.rdp, cl.rs1p),
      (C_LBU === x)                              -> inst(load(F3_LBU, cl.rdp, cl.rs1p, cl.byteImm), cl.rdp, cl.rs1p),
      (C_LHU === x)                              -> inst(load(F3_LHU, cl.rdp, cl.rs1p, cl.halfImm), cl.rdp, cl.rs1p),
      (C_LH === x)                               -> inst(load(F3_LH, cl.rdp, cl.rs1p, cl.halfImm), cl.rdp, cl.rs1p)
    )
  )

  /** CS: the 8-register-set stores (c.sw/c.sd/c.fsw/c.fsd and the Zcb byte/halfword stores). */
  private def expandCS: ExpandedInstruction = MuxCase(
    passthrough,
    Seq(
      (C_FSD === x) -> inst(storeFp(F3_SD, cs.rs2p, cs.rs1p, cs.ldImm), cs.x0, cs.rs1p, cs.rs2p),
      (C_SW === x)  -> inst(store(F3_SW, cs.rs2p, cs.rs1p, cs.lwImm), cs.x0, cs.rs1p, cs.rs2p),
      (if (xLen == 64) C_SD === x else false.B) -> inst(
        store(F3_SD, cs.rs2p, cs.rs1p, cs.ldImm),
        cs.x0,
        cs.rs1p,
        cs.rs2p
      ),
      (if (xLen == 32) C_FSW === x else false.B) -> inst(
        storeFp(F3_SW, cs.rs2p, cs.rs1p, cs.lwImm),
        cs.x0,
        cs.rs1p,
        cs.rs2p
      ),
      (C_SB === x) -> inst(store(F3_SB, cs.rs2p, cs.rs1p, cs.byteImm), cs.x0, cs.rs1p, cs.rs2p),
      (C_SH === x) -> inst(store(F3_SH, cs.rs2p, cs.rs1p, cs.halfImm), cs.x0, cs.rs1p, cs.rs2p)
    )
  )

  /** CR: c.mv, c.jr, c.add, c.jalr, c.ebreak. */
  private def expandCR: ExpandedInstruction = {
    // c.mv: addi rd, rs2, 0 (useAddiForMv) or add rd, x0, rs2
    val cMv =
      if (useAddiForMv) inst(opImm(F3_ADD, cr.rd, cr.rs2, 0.U(12.W)), cr.rd, cr.rs2, cr.x0)
      else inst(op(F7_ADD, cr.rs2, cr.x0, F3_ADD, cr.rd), cr.rd, cr.x0, cr.rs2)
    // c.jr: jalr x0, 0(rd); the rd = x0 code point is reserved, for which the original emitted a
    // 0x1F sentinel opcode (masked by `ill`, kept here for bit-exact equivalence)
    val cJr = Mux(
      cr.rd.orR,
      inst(jalr(0.U(12.W), cr.rd, cr.x0), cr.x0, cr.rd, cr.rs2),
      inst(iType(0.U(12.W), cr.rd, F3_ADD, cr.x0, OP_RESERVED_1F), cr.x0, cr.rd, cr.rs2)
    )
    // c.add: add rd, rd, rs2
    val cAdd = inst(op(F7_ADD, cr.rs2, cr.rd, F3_ADD, cr.rd), cr.rd, cr.rd, cr.rs2)
    // c.jalr: jalr ra, 0(rd)
    val cJalr = inst(jalr(0.U(12.W), cr.rd, cr.ra), cr.ra, cr.rd, cr.rs2)
    // c.ebreak: ebreak
    val cEbreak = inst(ebreak, cr.x0, cr.x0, cr.x0)

    MuxCase(
      passthrough,
      Seq(
        (C_EBREAK === x) -> cEbreak, // must precede C_JALR and C_ADD
        (C_JALR === x)   -> cJalr,   // must precede C_ADD
        (C_ADD === x)    -> cAdd,
        (C_JR === x)     -> cJr,     // must precede C_MV
        (C_MV === x)     -> cMv
      )
    )
  }

  /** CI: c.addi/c.nop/c.addiw/c.li/c.lui/c.addi16sp/c.mop.n/c.slli and the stack-relative loads. */
  private def expandCI: ExpandedInstruction = {
    val cAddiOrNop = Mux(
      ci.rd.orR,
      inst(opImm(F3_ADD, ci.rd, ci.rd, ci.imm12), ci.rd, ci.rd, ci.rs2),
      inst(nop, ci.x0, ci.x0, ci.x0)
    )
    val cAddiw = inst(
      iType(ci.imm12, ci.rd, F3_ADD, ci.rd, Mux(ci.rd.orR, OP_IMM_32, OP_RESERVED_1F)),
      ci.rd,
      ci.rd,
      ci.rs2
    )
    val cLi = inst(opImm(F3_ADD, ci.rd, ci.x0, ci.imm12), ci.rd, ci.x0, ci.rs2)
    val cLui = inst(
      uType(ci.luiImm20, ci.rd, Mux(ci.immIsZero, OP_RESERVED_3F, OP_LUI)),
      ci.rd,
      ci.rd,
      ci.rs2
    )
    val cAddi16sp = inst(
      iType(ci.addi16spImm, ci.sp, F3_ADD, ci.sp, Mux(ci.immIsZero, OP_RESERVED_1F, OP_IMM)),
      ci.sp,
      ci.sp,
      ci.rs2
    )
    val cSlli = inst(opImm(F3_SLL, ci.rd, ci.rd, ci.shamt6), ci.rd, ci.rd, ci.rs2)
    val cLwsp = inst(
      iType(ci.lwspImm, ci.sp, F3_LW, ci.rd, Mux(ci.rd.orR, OP_LOAD, OP_RESERVED_1F)),
      ci.rd,
      ci.sp,
      ci.rs2
    )
    val cLdsp = inst(
      iType(ci.ldspImm, ci.sp, F3_LD, ci.rd, Mux(ci.rd.orR, OP_LOAD, OP_RESERVED_1F)),
      ci.rd,
      ci.sp,
      ci.rs2
    )
    val cFlwsp = inst(loadFp(F3_LW, ci.rd, ci.sp, ci.lwspImm), ci.rd, ci.sp, ci.rs2)
    val cFldsp = inst(loadFp(F3_LD, ci.rd, ci.sp, ci.ldspImm), ci.rd, ci.sp, ci.rs2)

    MuxCase(
      passthrough,
      Seq(
        (C_ADDI === x)                               -> cAddiOrNop,
        (if (xLen == 64) C_ADDIW === x else false.B) -> cAddiw, // RV64; on RV32 this slot is C.JAL
        (C_LI === x)                                 -> cLi,
        (C_MOP_N === x)    -> inst(nop, ci.x0, ci.x0, ci.x0), // c.mop.n is a hint => nop
        (C_ADDI16SP === x) -> cAddi16sp,                      // must precede C_LUI
        (C_LUI === x)      -> cLui,
        (C_SLLI === x)     -> cSlli,
        (C_LWSP === x)     -> cLwsp,
        (if (xLen == 64) C_LDSP === x else false.B)  -> cLdsp, // RV64; on RV32 this slot is C.FLWSP
        (if (xLen == 32) C_FLWSP === x else false.B) -> cFlwsp,
        (C_FLDSP === x)                              -> cFldsp
      )
    )
  }

  /** CSS: c.swsp, c.sdsp, c.fswsp, c.fsdsp. */
  private def expandCSS: ExpandedInstruction = MuxCase(
    passthrough,
    Seq(
      (C_SWSP === x) -> inst(store(F3_SW, css.rs2, css.sp, css.swspImm), css.x0, css.sp, css.rs2),
      (if (xLen == 64) C_SDSP === x else false.B) -> inst(
        store(F3_SD, css.rs2, css.sp, css.sdspImm),
        css.x0,
        css.sp,
        css.rs2
      ),
      (if (xLen == 32) C_FSWSP === x else false.B) -> inst(
        storeFp(F3_SW, css.rs2, css.sp, css.swspImm),
        css.x0,
        css.sp,
        css.rs2
      ),
      (C_FSDSP === x) -> inst(storeFp(F3_SD, css.rs2, css.sp, css.sdspImm), css.x0, css.sp, css.rs2)
    )
  )

  /** CA: c.sub/c.xor/c.or/c.and/c.subw/c.addw/c.mul and the Zcb unary operations. */
  private def expandCA: ExpandedInstruction = {
    def rOp(funct7: UInt, funct3: UInt, opcode: UInt): ExpandedInstruction =
      inst(rType(funct7, ca.rs2c, ca.rsd, funct3, ca.rsd, opcode), ca.rsd, ca.rsd, ca.rs2c)

    val cSub  = rOp(F7_SUB, F3_ADD, OP)
    val cXor  = rOp(F7_ADD, F3_XOR, OP)
    val cOr   = rOp(F7_ADD, F3_OR, OP)
    val cAnd  = rOp(F7_ADD, F3_AND, OP)
    val cSubw = rOp(F7_SUB, F3_ADD, OP_32) // RV64
    val cAddw = rOp(F7_ADD, F3_ADD, OP_32) // RV64
    val cMul  = rOp(F7_MUL, F3_ADD, OP)

    val regReg = MuxCase(
      passthrough,
      Seq(
        (ca.funct6 === 0.U) -> cSub,
        (ca.funct6 === 1.U) -> cXor,
        (ca.funct6 === 2.U) -> cOr,
        (ca.funct6 === 3.U) -> cAnd,
        (ca.funct6 === 4.U) -> cSubw,
        (ca.funct6 === 5.U) -> cAddw,
        (ca.funct6 === 6.U) -> cMul
      )
    )

    // Zcb unary group (funct6 = 100111, inst[6:5] = 11), selected by inst[4:2].
    val cZextb = inst(opImm(F3_AND, ca.rsd, ca.rsd, IMM_ZEXT_B), ca.rsd, ca.rsd)
    val cSextb = inst(opImm(F3_SLL, ca.rsd, ca.rsd, IMM_SEXT_B), ca.rsd, ca.rsd)
    val cZexth = inst(
      rType(F7_ADD_UW, ca.x0, ca.rsd, F3_XOR, ca.rsd, if (xLen == 64) OP_32 else OP),
      ca.rsd,
      ca.rsd
    )
    val cSexth = inst(opImm(F3_SLL, ca.rsd, ca.rsd, IMM_SEXT_H), ca.rsd, ca.rsd)
    val cZextw =
      if (xLen == 64) inst(rType(F7_ADD_UW, ca.x0, ca.rsd, F3_ADD, ca.rsd, OP_32), ca.rsd, ca.rsd)
      else inst(unimp, ca.x0, ca.x0) // c.zext.w is illegal in RV32
    val cNot = inst(opImm(F3_XOR, ca.rsd, ca.rsd, IMM_NOT), ca.rsd, ca.rsd)

    val zcb = MuxCase(
      passthrough,
      Seq(
        (ca.zcbSel === 0.U) -> cZextb,
        (ca.zcbSel === 1.U) -> cSextb,
        (ca.zcbSel === 2.U) -> cZexth,
        (ca.zcbSel === 3.U) -> cSexth,
        (ca.zcbSel === 4.U) -> cZextw,
        (ca.zcbSel === 5.U) -> cNot
        // inst[4:2] = 110/111 are reserved (flagged by `ill`)
      )
    )

    Mux(ca.funct6 === 7.U, zcb, regReg)
  }

  /** CB: c.srli/c.srai/c.andi and c.beqz/c.bnez. */
  private def expandCB: ExpandedInstruction = MuxCase(
    passthrough,
    Seq(
      (C_SRLI === x) -> inst(opImm(F3_SR, cb.rs1p, cb.rs1p, cb.shamt6), cb.rs1p, cb.rs1p),
      (C_SRAI === x) -> inst(opImm(F3_SR, cb.rs1p, cb.rs1p, cb.shamt6 | IMM_SRAI), cb.rs1p, cb.rs1p),
      (C_ANDI === x) -> inst(opImm(F3_AND, cb.rs1p, cb.rs1p, cb.imm12), cb.rs1p, cb.rs1p),
      (C_BEQZ === x) -> inst(branch(F3_BEQ, cb.x0, cb.rs1p, cb.bImm), cb.x0, cb.rs1p),
      (C_BNEZ === x) -> inst(branch(F3_BNE, cb.x0, cb.rs1p, cb.bImm), cb.x0, cb.rs1p)
    )
  )

  /** CJ: c.j and (RV32 only) c.jal. */
  private def expandCJ: ExpandedInstruction = MuxCase(
    passthrough,
    Seq(
      (C_J === x)                                -> inst(jal(cj.jImm, cj.x0), cj.x0, cj.x0),
      (if (xLen == 32) C_JAL === x else false.B) -> inst(jal(cj.jImm, cj.ra), cj.ra, cj.x0)
    )
  )

  // ===============================================================================================
  // Dispatch: quadrant + funct3 (+ funct2) select the format.
  // ===============================================================================================

  private def decodeQuadrant0: ExpandedInstruction = MuxCase(
    passthrough,
    Seq(
      (funct3 === 0.U) -> expandCIW,
      (funct3 === 1.U) -> expandCL,
      (funct3 === 2.U) -> expandCL,
      (funct3 === 3.U) -> expandCL,
      // funct3 = 100 is the Zcb slot: funct2 = 00/01 are loads (CL), 10/11 are stores (CS)
      (funct3 === 4.U) -> Mux(funct2(1) === 1.U, expandCS, expandCL),
      (funct3 === 5.U) -> expandCS,
      (funct3 === 6.U) -> expandCS,
      (funct3 === 7.U) -> expandCS
    )
  )

  private def decodeQuadrant1: ExpandedInstruction = MuxCase(
    passthrough,
    Seq(
      (funct3 === 0.U) -> expandCI,
      // funct3 = 001 is C.JAL (CJ) in RV32 and C.ADDIW (CI) in RV64
      (funct3 === 1.U) -> (if (xLen == 32) expandCJ else expandCI),
      (funct3 === 2.U) -> expandCI,
      (funct3 === 3.U) -> expandCI,
      // funct3 = 100 is CA when funct2 = 11, otherwise CB (c.srli/c.srai/c.andi)
      (funct3 === 4.U) -> Mux(funct2 === 3.U, expandCA, expandCB),
      (funct3 === 5.U) -> expandCJ,
      (funct3 === 6.U) -> expandCB,
      (funct3 === 7.U) -> expandCB
    )
  )

  private def decodeQuadrant2: ExpandedInstruction = MuxCase(
    passthrough,
    Seq(
      (funct3 === 0.U) -> expandCI,
      (funct3 === 1.U) -> expandCI,
      (funct3 === 2.U) -> expandCI,
      (funct3 === 3.U) -> expandCI,
      (funct3 === 4.U) -> expandCR,
      (funct3 === 5.U) -> expandCSS,
      (funct3 === 6.U) -> expandCSS,
      (funct3 === 7.U) -> expandCSS
    )
  )

  /** Expand the compressed instruction. Quadrant 11 (a 32-bit instruction) is passed through. */
  def decode: ExpandedInstruction = MuxCase(
    passthrough,
    Seq(
      (quadrant === 0.U) -> decodeQuadrant0,
      (quadrant === 1.U) -> decodeQuadrant1,
      (quadrant === 2.U) -> decodeQuadrant2
    )
  )

  // ===============================================================================================
  // Illegal-instruction detection.
  // ===============================================================================================

  /** D accesses are illegal when there is no D extension (fLen < 64) or when FS is off. */
  private def dIsOff: Bool = if (fLen < 64) true.B else fsIsOff

  /** F accesses (RV32 only) are illegal when there is no F extension (fLen < 32) or when FS is off. */
  private def fIsOff: Bool = if (fLen < 32) true.B else fsIsOff

  private def isZcmop: Bool = C_MOP_N === x

  /** Zcb load/store slot: funct6 must be 10000x and, for c.sh, inst[6] must be 0. */
  private def zcbLdStReserved: Bool = x(12) || (x(11) && x(10) && x(6))

  private def illQuadrant0: Bool = MuxCase(
    false.B,
    Seq(
      (funct3 === 0.U) -> (RVCReserved.C_ADDI4SPN === x),        // c.addi4spn nzuimm = 0
      (funct3 === 1.U) -> dIsOff,                                // c.fld needs D and FS != Off
      (funct3 === 3.U) -> (if (xLen == 32) fIsOff else false.B), // c.flw (RV32)
      (funct3 === 4.U) -> zcbLdStReserved,                       // Zcb slot reserved codes
      (funct3 === 5.U) -> dIsOff,                                // c.fsd needs D and FS != Off
      (funct3 === 7.U) -> (if (xLen == 32) fIsOff else false.B)  // c.fsw (RV32)
    )
  )

  private def illQuadrant1: Bool = MuxCase(
    false.B,
    Seq(
      (funct3 === 1.U) -> (if (xLen == 64) RVCReserved.C_ADDIW === x else false.B), // c.addiw rd = 0 (RV64)
      (funct3 === 3.U) -> ((RVCReserved.C_LUI === x) && !isZcmop),                  // c.lui / c.addi16sp nzimm = 0
      (funct3 === 4.U) -> (RVCReserved.ZCB_UNIMP0 === x || RVCReserved.ZCB_UNIMP1 === x)
    )
  )

  private def illQuadrant2: Bool = MuxCase(
    false.B,
    Seq(
      (funct3 === 1.U) -> dIsOff,                                                 // c.fldsp needs D and FS != Off
      (funct3 === 2.U) -> (RVCReserved.C_LWSP === x),                             // c.lwsp rd = 0
      (funct3 === 3.U) -> (if (xLen == 64) RVCReserved.C_LDSP === x else fIsOff), // c.ldsp rd = 0 / c.flwsp (RV32)
      (funct3 === 4.U) -> (RVCReserved.C_JR === x),                               // c.jr x0 / c.jalr x0
      (funct3 === 5.U) -> dIsOff,                                                 // c.fsdsp needs D and FS != Off
      (funct3 === 7.U) -> (if (xLen == 64) false.B else fIsOff)                   // c.sdsp / c.fswsp (RV32)
    )
  )

  /** true when the code point is an illegal / reserved compressed instruction. */
  def ill: Bool = MuxCase(
    false.B,
    Seq(
      (quadrant === 0.U) -> illQuadrant0,
      (quadrant === 1.U) -> illQuadrant1,
      (quadrant === 2.U) -> illQuadrant2
    )
  )
}

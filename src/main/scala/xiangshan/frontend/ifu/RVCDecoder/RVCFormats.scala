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

import chisel3._
import chisel3.util._

/** The nine RVC instruction formats of the "C" extension manual, one class per format.
  *
  * A format object is a plain Scala class that merely renames the bit fields of the fetch word,
  * so it costs no hardware. The immediate accessors return the immediate "semantically aligned"
  * (e.g. `lwImm` is already shifted left by 2), ready to be placed into the RV-I encoding by
  * [[RviInst]].
  *
  * `x` is the 32-bit fetch word; the compressed instruction occupies its low 16 bits.
  */
sealed abstract class RVCFormat(val x: UInt) {
  require(x.getWidth == 32, "RVCFormat expects the 32-bit fetch word holding a 16-bit instruction")

  /** inst[1:0]: quadrant, 00/01/10 = compressed, 11 = 32-bit instruction. */
  final def quadrant: UInt = x(1, 0)
  /** inst[15:13]: funct3. */
  final def funct3: UInt = x(15, 13)
  /** inst[11:10]: funct2, used by the CA/CB sub-opcodes and the Zcb selector. */
  final def funct2: UInt = x(11, 10)
  /** inst[12]: the sub-opcode bit shared by CR/CI/CA/CB/CJ. */
  final def bit12: UInt = x(12)
  /** inst[11:7]: full-register rd. */
  final def rd: UInt = x(11, 7)
  /** inst[6:2]: full-register rs2 or the CI immediate. */
  final def rs2: UInt = x(6, 2)
  /** inst[9:7] + 8: rs1' of the 8-register set (CL/CS/CA/CB). */
  final def rs1p: UInt = Cat(1.U(2.W), x(9, 7))
  /** inst[4:2] + 8: rs2' of the 8-register set (CS/CA) or rd' (CIW/CL). */
  final def rs2p: UInt = Cat(1.U(2.W), x(4, 2))
  /** rd' = inst[4:2] + 8: destination of the 8-register-set loads and of c.addi4spn. */
  final def rdp: UInt = rs2p
  /** register constants */
  final def x0: UInt = 0.U(5.W)
  final def ra: UInt = 1.U(5.W)
  final def sp: UInt = 2.U(5.W)
}

/** CR: c.mv, c.jr, c.add, c.jalr, c.ebreak. */
final class CRFormat(x: UInt) extends RVCFormat(x) {
  /** rs2 = x0 selects c.jr / c.jalr instead of c.mv / c.add. */
  def rs2IsZero: Bool = !rs2.orR
}

/** CI: c.addi/c.nop/c.addiw/c.li/c.lui/c.addi16sp/c.mop.n/c.slli and the stack-relative loads. */
final class CIFormat(x: UInt) extends RVCFormat(x) {
  /** 12-bit sign-extended immediate {inst[12], inst[6:2]} of c.addi / c.li / c.addiw. */
  def imm12: UInt = Cat(Fill(7, x(12)), x(6, 2))
  /** 6-bit shift amount {inst[12], inst[6:2]} of c.slli. */
  def shamt6: UInt = Cat(x(12), x(6, 2))
  /** 20-bit immediate (inst[31:12]) of c.lui. */
  def luiImm20: UInt = Cat(Fill(15, x(12)), x(6, 2))
  /** 12-bit sign-extended immediate of c.addi16sp. */
  def addi16spImm: UInt = Cat(Fill(3, x(12)), x(4, 3), x(5), x(2), x(6), 0.U(4.W))
  /** 8-bit immediate of c.lwsp / c.flwsp. */
  def lwspImm: UInt = Cat(x(3, 2), x(12), x(6, 4), 0.U(2.W))
  /** 9-bit immediate of c.ldsp / c.fldsp. */
  def ldspImm: UInt = Cat(x(4, 2), x(12), x(6, 5), 0.U(3.W))
  /** true when nzimm is zero: the reserved code point of c.lui / c.addi16sp. */
  def immIsZero: Bool = !(x(12) | x(6, 2).orR)
}

/** CSS: c.swsp, c.sdsp, c.fswsp, c.fsdsp. */
final class CSSFormat(x: UInt) extends RVCFormat(x) {
  /** 8-bit immediate of c.swsp / c.fswsp. */
  def swspImm: UInt = Cat(x(8, 7), x(12, 9), 0.U(2.W))
  /** 9-bit immediate of c.sdsp / c.fsdsp. */
  def sdspImm: UInt = Cat(x(9, 7), x(12, 10), 0.U(3.W))
}

/** CIW: c.addi4spn. */
final class CIWFormat(x: UInt) extends RVCFormat(x) {
  /** 10-bit non-zero immediate {inst[10:7], inst[12:11], inst[5], inst[6], 00}. */
  def addi4spnImm: UInt = Cat(x(10, 7), x(12, 11), x(5), x(6), 0.U(2.W))
  /** c.addi4spn is valid only when nzuimm != 0. */
  def nzuimmIsZero: Bool = !x(12, 5).orR
}

/** CL: the 8-register-set loads (c.lw/c.ld/c.flw/c.fld and the Zcb byte/halfword loads). */
class CLFormat(x: UInt) extends RVCFormat(x) {
  /** 7-bit immediate of c.lw / c.flw. */
  def lwImm: UInt = Cat(x(5), x(12, 10), x(6), 0.U(2.W))
  /** 8-bit immediate of c.ld / c.fld. */
  def ldImm: UInt = Cat(x(6, 5), x(12, 10), 0.U(3.W))
  /** 2-bit byte immediate {inst[5], inst[6]} of c.lbu / c.sb. */
  def byteImm: UInt = Cat(x(5), x(6))
  /** 2-bit halfword immediate {inst[5], 0} of c.lhu / c.lh / c.sh. */
  def halfImm: UInt = Cat(x(5), 0.U(1.W))
  /** inst[6]: selects c.lh (1) over c.lhu (0). */
  def funct1: UInt = x(6)
}

/** CS: the 8-register-set stores; the bit layout is identical to CL. */
final class CSFormat(x: UInt) extends CLFormat(x)

/** CA: two 8-register-set operands (c.sub/... and the Zcb unary operations). */
final class CAFormat(x: UInt) extends RVCFormat(x) {
  /** rd' = rs1' = inst[9:7] + 8. */
  def rsd: UInt = rs1p
  /** rs2' = inst[4:2] + 8. */
  def rs2c: UInt = rs2p
  /** {inst[12], inst[6:5]}: the register-register sub-opcode / the Zcb group selector. */
  def funct6: UInt = Cat(x(12), x(6, 5))
  /** inst[4:2]: the Zcb unary-op selector. */
  def zcbSel: UInt = x(4, 2)
}

/** CB: c.beqz / c.bnez; c.srli / c.srai / c.andi reuse this layout. */
final class CBFormat(x: UInt) extends RVCFormat(x) {
  /** 13-bit signed branch offset (bit 0 is always zero). */
  def bImm: UInt = Cat(Fill(5, x(12)), x(6, 5), x(2), x(11, 10), x(4, 3), 0.U(1.W))
  /** 6-bit shift amount {inst[12], inst[6:2]} of c.srli / c.srai. */
  def shamt6: UInt = Cat(x(12), x(6, 2))
  /** 12-bit sign-extended immediate {inst[12], inst[6:2]} of c.andi. */
  def imm12: UInt = Cat(Fill(7, x(12)), x(6, 2))
}

/** CJ: c.j / c.jal. */
final class CJFormat(x: UInt) extends RVCFormat(x) {
  /** 21-bit signed jump offset (bit 0 is always zero). */
  def jImm: UInt = Cat(Fill(10, x(12)), x(8), x(10, 9), x(6), x(7), x(2), x(11), x(5, 3), 0.U(1.W))
}

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

/** A tiny encoder for the RV-I instruction formats (R/I/S/B/U/J).
  *
  * It exists so that the RVC assembly reads like the manual
  * (`opImm(F3_ADD, rd, rs1, imm)` instead of a hand-written `Cat(...)`), and so that the
  * "immediate scattering" (e.g. `Cat(imm >> 5, rs2, rs1, f3, imm(4, 0), opcode)`) is written
  * once instead of at every call site.
  *
  * Immediates are passed "semantically aligned" (bit 0 of the value is instruction bit 20 for
  * I-type, etc.), exactly as the format classes of [[RVCFormat]] return them.
  */
object RviInst {
  // ------------------------------------------------------------------------ opcode (bits 6:0)
  val OP_LOAD:     UInt = 0x03.U(7.W)
  val OP_LOAD_FP:  UInt = 0x07.U(7.W)
  val OP_IMM:      UInt = 0x13.U(7.W)
  val OP_IMM_32:   UInt = 0x1b.U(7.W)
  val OP_STORE:    UInt = 0x23.U(7.W)
  val OP_STORE_FP: UInt = 0x27.U(7.W)
  val OP:          UInt = 0x33.U(7.W)
  val OP_LUI:      UInt = 0x37.U(7.W)
  val OP_32:       UInt = 0x3b.U(7.W)
  val OP_BRANCH:   UInt = 0x63.U(7.W)
  val OP_JALR:     UInt = 0x67.U(7.W)
  val OP_JAL:      UInt = 0x6f.U(7.W)
  val OP_SYSTEM:   UInt = 0x73.U(7.W)

  /** reserved opcodes used as sentinels for the RVC code points that expand to nothing valid */
  val OP_RESERVED_1F: UInt = 0x1f.U(7.W)
  val OP_RESERVED_3F: UInt = 0x3f.U(7.W)

  // ------------------------------------------------------------------------ funct3
  val F3_ADD: UInt = 0x0.U(3.W)
  val F3_SLL: UInt = 0x1.U(3.W)
  val F3_XOR: UInt = 0x4.U(3.W)
  val F3_SR:  UInt = 0x5.U(3.W)
  val F3_OR:  UInt = 0x6.U(3.W)
  val F3_AND: UInt = 0x7.U(3.W)

  val F3_LB:  UInt = 0x0.U(3.W)
  val F3_LH:  UInt = 0x1.U(3.W)
  val F3_LW:  UInt = 0x2.U(3.W)
  val F3_LD:  UInt = 0x3.U(3.W)
  val F3_LBU: UInt = 0x4.U(3.W)
  val F3_LHU: UInt = 0x5.U(3.W)

  val F3_SB: UInt = 0x0.U(3.W)
  val F3_SH: UInt = 0x1.U(3.W)
  val F3_SW: UInt = 0x2.U(3.W)
  val F3_SD: UInt = 0x3.U(3.W)

  val F3_BEQ: UInt = 0x0.U(3.W)
  val F3_BNE: UInt = 0x1.U(3.W)

  // ------------------------------------------------------------------------ funct7
  val F7_ADD: UInt = 0x00.U(7.W)
  val F7_SUB: UInt = 0x20.U(7.W)
  val F7_MUL: UInt = 0x01.U(7.W)

  /** add.uw (RV64), and zext.h / pack when rs2 = x0 */
  val F7_ADD_UW: UInt = 0x04.U(7.W)

  // ------------------------------------------------------------------------ immediates
  /** srai sets imm[10] (the funct6 010000 field of the shift immediate). */
  val IMM_SRAI: UInt = 0x400.U(12.W)

  /** andi 0xff  == c.zext.b */
  val IMM_ZEXT_B: UInt = 0x0ff.U(12.W)

  /** xori -1    == c.not */
  val IMM_NOT: UInt = 0xfff.U(12.W)

  /** sext.b / sext.h */
  val IMM_SEXT_B: UInt = 0x604.U(12.W)
  val IMM_SEXT_H: UInt = 0x605.U(12.W)

  /** ebreak immediate */
  val IMM_EBREAK: UInt = 1.U(12.W)

  /** Low `width` bits of `imm`, zero-extending when narrower and truncating when wider. */
  private def low(imm: UInt, width: Int): UInt = imm.pad(width)(width - 1, 0)

  // ======================================================================== the six RV-I formats

  /** R-type: funct7 | rs2 | rs1 | funct3 | rd | opcode. */
  def rType(funct7: UInt, rs2: UInt, rs1: UInt, funct3: UInt, rd: UInt, opcode: UInt): UInt =
    Cat(funct7, rs2, rs1, funct3, rd, opcode)

  /** I-type: imm[11:0] | rs1 | funct3 | rd | opcode. */
  def iType(imm: UInt, rs1: UInt, funct3: UInt, rd: UInt, opcode: UInt): UInt =
    Cat(low(imm, 12), rs1, funct3, rd, opcode)

  /** S-type: imm[11:5] | rs2 | rs1 | funct3 | imm[4:0] | opcode. */
  def sType(imm: UInt, rs2: UInt, rs1: UInt, funct3: UInt, opcode: UInt): UInt = {
    val i = low(imm, 12)
    Cat(i(11, 5), rs2, rs1, funct3, i(4, 0), opcode)
  }

  /** B-type: imm[12] | imm[10:5] | rs2 | rs1 | funct3 | imm[4:1] | imm[11] | opcode. */
  def bType(imm: UInt, rs2: UInt, rs1: UInt, funct3: UInt, opcode: UInt): UInt = {
    val i = low(imm, 13)
    Cat(i(12), i(10, 5), rs2, rs1, funct3, i(4, 1), i(11), opcode)
  }

  /** U-type: imm[31:12] | rd | opcode. */
  def uType(imm20: UInt, rd: UInt, opcode: UInt): UInt =
    Cat(low(imm20, 20), rd, opcode)

  /** J-type: imm[20] | imm[10:1] | imm[11] | imm[19:12] | rd | opcode. */
  def jType(imm: UInt, rd: UInt, opcode: UInt): UInt = {
    val i = low(imm, 21)
    Cat(i(20), i(10, 1), i(11), i(19, 12), rd, opcode)
  }

  // ======================================================================== convenience builders
  def load(funct3:    UInt, rd:  UInt, rs1: UInt, imm: UInt): UInt = iType(imm, rs1, funct3, rd, OP_LOAD)
  def loadFp(funct3:  UInt, rd:  UInt, rs1: UInt, imm: UInt): UInt = iType(imm, rs1, funct3, rd, OP_LOAD_FP)
  def store(funct3:   UInt, rs2: UInt, rs1: UInt, imm: UInt): UInt = sType(imm, rs2, rs1, funct3, OP_STORE)
  def storeFp(funct3: UInt, rs2: UInt, rs1: UInt, imm: UInt): UInt = sType(imm, rs2, rs1, funct3, OP_STORE_FP)
  def opImm(funct3:   UInt, rd:  UInt, rs1: UInt, imm: UInt): UInt = iType(imm, rs1, funct3, rd, OP_IMM)
  def opImm32(funct3: UInt, rd:  UInt, rs1: UInt, imm: UInt): UInt = iType(imm, rs1, funct3, rd, OP_IMM_32)
  def op(funct7: UInt, rs2: UInt, rs1: UInt, funct3: UInt, rd: UInt): UInt = rType(funct7, rs2, rs1, funct3, rd, OP)
  def op32(funct7: UInt, rs2: UInt, rs1: UInt, funct3: UInt, rd: UInt): UInt =
    rType(funct7, rs2, rs1, funct3, rd, OP_32)
  def branch(funct3: UInt, rs2: UInt, rs1:    UInt, imm: UInt): UInt = bType(imm, rs2, rs1, funct3, OP_BRANCH)
  def jal(imm:       UInt, rd:  UInt): UInt = jType(imm, rd, OP_JAL)
  def jalr(imm:      UInt, rs1: UInt, rd:     UInt): UInt = iType(imm, rs1, F3_ADD, rd, OP_JALR)
  def lui(imm20:     UInt, rd:  UInt): UInt = uType(imm20, rd, OP_LUI)
  def system(imm:    UInt, rs1: UInt, funct3: UInt, rd:  UInt): UInt = iType(imm, rs1, funct3, rd, OP_SYSTEM)

  /** nop = addi x0, x0, 0 */
  def nop: UInt = opImm(F3_ADD, 0.U(5.W), 0.U(5.W), 0.U(12.W))

  /** ebreak */
  def ebreak: UInt = system(IMM_EBREAK, 0.U(5.W), F3_ADD, 0.U(5.W))

  /** the all-zero word is an illegal instruction encoding */
  def unimp: UInt = 0.U(32.W)
}

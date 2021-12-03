/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.decode

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.util.{UIntIsOneOf, uintToBitPat}
import xiangshan._
import utils._
import xiangshan.backend._
import xiangshan.backend.decode.BDecode.{N, Y}
import xiangshan.backend.decode.Instructions._

/**
 * Abstract trait giving defaults and other relevant values to different Decode constants/
 */
abstract trait DecodeConstants {
  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")

<<<<<<< HEAD
  def decodeDefault: List[BitPat] = { // illegal instruction
    //   srcType(0)   srcType(1)   srcType(2)   fuType      fuOpType    rfWen
=======
  def decodeDefault: List[BitPat] = // illegal instruction
    //   srcType(0)   srcType(1)   srcType(2)   fuType      fuOpType    rfWen  
>>>>>>> master
    //   |            |            |            |           |           |  fpWen
    //   |            |            |            |           |           |  |  isXSTrap
    //   |            |            |            |           |           |  |  |  noSpecExec
    //   |            |            |            |           |           |  |  |  |  blockBackward
    //   |            |            |            |           |           |  |  |  |  |  flushPipe
    //   |            |            |            |           |           |  |  |  |  |  |  isRVF
    //   |            |            |            |           |           |  |  |  |  |  |  |  selImm
    //   |            |            |            |           |           |  |  |  |  |  |  |  |                    isXsCC
    List(SrcType.DC, SrcType.DC, SrcType.DC, FuType.alu, ALUOpType.sll, N, N, N, N, N, N, N, SelImm.INVALID_INSTR, N)
  } // Use SelImm to indicate invalid instr

    val table: Array[(BitPat, List[BitPat])]
}

trait DecodeUnitConstants
{
  // abstract out instruction decode magic numbers
  val RD_MSB  = 11
  val RD_LSB  = 7
  val RS1_MSB = 19
  val RS1_LSB = 15
  val RS2_MSB = 24
  val RS2_LSB = 20
  val RS3_MSB = 31
  val RS3_LSB = 27
}

/**
 * Decoded control signals
 * See xiangshan/package.scala, xiangshan/backend/package.scala, Bundle.scala
 */

/**
 * Decode constants for RV64
 */
object X64Decode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    LD      -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.ld, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    LWU     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lwu, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    SD      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.stu, LSUOpType.sd, N, N, N, N, N, N, N, SelImm.IMM_S, N),

    SLLI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sll, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    SRLI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.srl, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    SRAI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sra, Y, N, N, N, N, N, N, SelImm.IMM_I, N),

    ADDIW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.addw, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    SLLIW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sllw, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    SRAIW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sraw, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    SRLIW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.srlw, Y, N, N, N, N, N, N, SelImm.IMM_I, N),

    ADDW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.addw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SUBW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.subw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SLLW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sllw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SRAW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sraw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SRLW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.srlw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

    RORW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.rorw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    RORIW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.rorw, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    ROLW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.rolw, Y, N, N, N, N, N, N, SelImm.IMM_X, N)
  )
}

/**
 * Overall Decode constants
 */
object XDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    LW      -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lw, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    LH      -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lh, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    LHU     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lhu, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    LB      -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lb, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    LBU     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lbu, Y, N, N, N, N, N, N, SelImm.IMM_I, N),

    SW      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.stu, LSUOpType.sw, N, N, N, N, N, N, N, SelImm.IMM_S, N),
    SH      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.stu, LSUOpType.sh, N, N, N, N, N, N, N, SelImm.IMM_S, N),
    SB      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.stu, LSUOpType.sb, N, N, N, N, N, N, N, SelImm.IMM_S, N),

    LUI     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.add, Y, N, N, N, N, N, N, SelImm.IMM_U, N),

    ADDI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.add, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    ANDI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.and, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    ORI     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.or, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    XORI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.xor, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    SLTI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.slt, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    SLTIU   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sltu, Y, N, N, N, N, N, N, SelImm.IMM_I, N),

    SLL     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sll, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    ADD     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.add, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SUB     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sub, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SLT     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.slt, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SLTU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sltu, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    AND     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.and, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    OR      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.or, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    XOR     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.xor, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SRA     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sra, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SRL     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.srl, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

    MUL     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mul, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    MULH    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mulh, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    MULHU   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mulhu, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    MULHSU  -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mulhsu, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    MULW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mulw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

    DIV     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.div, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    DIVU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.divu, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    REM     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.rem, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    REMU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.remu, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    DIVW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.divw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    DIVUW   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.divuw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    REMW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.remw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    REMUW   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.remuw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

    AUIPC   -> List(SrcType.pc , SrcType.imm, SrcType.DC, FuType.jmp, JumpOpType.auipc, Y, N, N, N, N, N, N, SelImm.IMM_U, N),
    JAL     -> List(SrcType.pc , SrcType.imm, SrcType.DC, FuType.jmp, JumpOpType.jal, Y, N, N, N, N, N, N, SelImm.IMM_UJ, N),
    JALR    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.jmp, JumpOpType.jalr, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    BEQ     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.beq, N, N, N, N, N, N, N, SelImm.IMM_SB, N),
    BNE     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bne, N, N, N, N, N, N, N, SelImm.IMM_SB, N),
    BGE     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bge, N, N, N, N, N, N, N, SelImm.IMM_SB, N),
    BGEU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bgeu, N, N, N, N, N, N, N, SelImm.IMM_SB, N),
    BLT     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.blt, N, N, N, N, N, N, N, SelImm.IMM_SB, N),
    BLTU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bltu, N, N, N, N, N, N, N, SelImm.IMM_SB, N),

    // I-type, the immediate12 holds the CSR register.
    CSRRW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.wrt, Y, N, N, Y, Y, N, N, SelImm.IMM_I, N),
    CSRRS   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.set, Y, N, N, Y, Y, N, N, SelImm.IMM_I, N),
    CSRRC   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.clr, Y, N, N, Y, Y, N, N, SelImm.IMM_I, N),

    CSRRWI  -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.wrti, Y, N, N, Y, Y, N, N, SelImm.IMM_Z, N),
    CSRRSI  -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.seti, Y, N, N, Y, Y, N, N, SelImm.IMM_Z, N),
    CSRRCI  -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.clri, Y, N, N, Y, Y, N, N, SelImm.IMM_Z, N),

<<<<<<< HEAD
    SFENCE_VMA->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.fence, FenceOpType.sfence, N, N, N, Y, Y, Y, N, SelImm.IMM_X, N),
    EBREAK  -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, Y, N, N, Y, Y, N, N, SelImm.IMM_I, N),
    ECALL   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, Y, N, N, Y, Y, N, N, SelImm.IMM_I, N),
    SRET    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, Y, N, N, Y, Y, N, N, SelImm.IMM_I, N),
    MRET    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, Y, N, N, Y, Y, N, N, SelImm.IMM_I, N),
    DRET    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, Y, N, N, Y, Y, N, N, SelImm.IMM_I, N),
=======
    SFENCE_VMA->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fence, FenceOpType.sfence, N, N, N, Y, Y, Y, N, SelImm.IMM_X),
    EBREAK  -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, Y, N, N, Y, Y, N, N, SelImm.IMM_I),
    ECALL   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, Y, N, N, Y, Y, N, N, SelImm.IMM_I),
    SRET    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, Y, N, N, Y, Y, N, N, SelImm.IMM_I),
    MRET    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, Y, N, N, Y, Y, N, N, SelImm.IMM_I),
    DRET    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, Y, N, N, Y, Y, N, N, SelImm.IMM_I),
>>>>>>> master

    WFI     -> List(SrcType.pc, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sll, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

    FENCE_I -> List(SrcType.pc, SrcType.imm, SrcType.DC, FuType.fence, FenceOpType.fencei, N, N, N, Y, Y, Y, N, SelImm.IMM_X, N),
    FENCE   -> List(SrcType.pc, SrcType.imm, SrcType.DC, FuType.fence, FenceOpType.fence, N, N, N, Y, Y, Y, N, SelImm.IMM_X, N),

    // A-type
    AMOADD_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoadd_w, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOXOR_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoxor_w, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOSWAP_W->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoswap_w, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOAND_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoand_w, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOOR_W -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoor_w, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOMIN_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomin_w, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOMINU_W->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amominu_w, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOMAX_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomax_w, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOMAXU_W->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomaxu_w, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),

    AMOADD_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoadd_d, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOXOR_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoxor_d, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOSWAP_D->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoswap_d, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOAND_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoand_d, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOOR_D -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoor_d, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOMIN_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomin_d, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOMINU_D->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amominu_d, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOMAX_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomax_d, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    AMOMAXU_D->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomaxu_d, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),

    LR_W    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.mou, LSUOpType.lr_w, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    LR_D    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.mou, LSUOpType.lr_d, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    SC_W    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.sc_w, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),
    SC_D    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.sc_d, Y, N, N, Y, Y, N, N, SelImm.IMM_X, N),

    ANDN    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.andn, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    ORN     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.orn, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    XNOR    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.xnor, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    ORC_B   -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.alu, ALUOpType.orcb, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

    MIN     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.min, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    MINU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.minu, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    MAX     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.max, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    MAXU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.maxu, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

    SEXT_B  -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.alu, ALUOpType.sextb, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    PACKH   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.packh, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SEXT_H  -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.alu, ALUOpType.sexth, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    PACKW   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.packw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    REVB    -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.alu, ALUOpType.revb, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    REV8    -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.alu, ALUOpType.rev8, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    PACK    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.pack, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

    BSET    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bset, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    BSETI   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.bset, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    BCLR    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bclr, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    BCLRI   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.bclr, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    BINV    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.binv, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    BINVI   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.binv, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    BEXT    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bext, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    BEXTI   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.bext, Y, N, N, N, N, N, N, SelImm.IMM_I, N),

    ROR     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.ror, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    RORI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.ror, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    ROL     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.rol, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

    SH1ADD  -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sh1add, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SH2ADD  -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sh2add, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SH3ADD  -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sh3add, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SH1ADDU_W   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sh1adduw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SH2ADDU_W   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sh2adduw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SH3ADDU_W   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sh3adduw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    ADDU_W      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.adduw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SLLIU_W     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.slliuw, Y, N, N, N, N, N, N, SelImm.IMM_I, N)
  )
}

/**
 * FP Decode constants
 */
object FDecode extends DecodeConstants{
  val table: Array[(BitPat, List[BitPat])] = Array(

  FLW     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lw, N, Y, N, N, N, N, Y, SelImm.IMM_I, N),
  FLD     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.ld, N, Y, N, N, N, N, N, SelImm.IMM_I, N),
  FSW     -> List(SrcType.reg, SrcType.fp, SrcType.DC, FuType.stu, LSUOpType.sw, N, N, N, N, N, N, Y, SelImm.IMM_S, N),
  FSD     -> List(SrcType.reg, SrcType.fp, SrcType.DC, FuType.stu, LSUOpType.sd, N, N, N, N, N, N, N, SelImm.IMM_S, N),

  FCLASS_S-> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, Y, SelImm.IMM_X, N),
  FCLASS_D-> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

  FMV_D_X -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),
  FMV_X_D -> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
  FMV_X_W -> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, Y, SelImm.IMM_X, N),
  FMV_W_X -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),

  FSGNJ_S -> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FSGNJ_D -> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),
  FSGNJX_S-> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FSGNJX_D-> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),
  FSGNJN_S-> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FSGNJN_D-> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),

  // FP to FP
  FCVT_S_D-> List(SrcType.fp, SrcType.imm, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FCVT_D_S-> List(SrcType.fp, SrcType.imm, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),

  // Int to FP
  FCVT_S_W-> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FCVT_S_WU->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FCVT_S_L-> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FCVT_S_LU->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),

  FCVT_D_W-> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),
  FCVT_D_WU->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),
  FCVT_D_L-> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),
  FCVT_D_LU->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),

  // FP to Int
  FCVT_W_S-> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, Y, SelImm.IMM_X, N),
  FCVT_WU_S->List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, Y, SelImm.IMM_X, N),
  FCVT_L_S-> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, Y, SelImm.IMM_X, N),
  FCVT_LU_S->List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, Y, SelImm.IMM_X, N),

  FCVT_W_D-> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
  FCVT_WU_D->List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
  FCVT_L_D-> List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
  FCVT_LU_D->List(SrcType.fp , SrcType.imm, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

  // "fp_single" is used for wb_data formatting (and debugging, N)
  FEQ_S    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, Y, SelImm.IMM_X, N),
  FLT_S    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, Y, SelImm.IMM_X, N),
  FLE_S    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, Y, SelImm.IMM_X, N),

  FEQ_D    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
  FLT_D    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
  FLE_D    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, X, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

  FMIN_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FMAX_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FMIN_D   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),
  FMAX_D   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),

  FADD_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FSUB_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FMUL_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FADD_D   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),
  FSUB_D   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),
  FMUL_D   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),

  FMADD_S  ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FMSUB_S  ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FNMADD_S ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FNMSUB_S ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FMADD_D  ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),
  FMSUB_D  ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),
  FNMADD_D ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),
  FNMSUB_D ->List(SrcType.fp,  SrcType.fp, SrcType.fp, FuType.fmac, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N)
  )
}

/**
  * Bit Manipulation Decode
  */
object BDecode extends DecodeConstants{
  val table: Array[(BitPat, List[BitPat])] = Array(
    // Basic bit manipulation
    CLZ     -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.bku, BKUOpType.clz, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    CTZ     -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.bku, BKUOpType.ctz, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    CPOP    -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.bku, BKUOpType.cpop, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    XPERM_B -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.xpermb, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    XPERM_N -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.xpermn, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

    CLZW    -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.bku, BKUOpType.clzw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    CTZW    -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.bku, BKUOpType.ctzw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    CPOPW   -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.bku, BKUOpType.cpopw, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

    CLMUL   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.clmul, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    CLMULH  -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.clmulh, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    CLMULR  -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.clmulr, Y, N, N, N, N, N, N, SelImm.IMM_X, N),

    AES64ES     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.aes64es, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    AES64ESM    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.aes64esm, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    AES64DS     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.aes64ds, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    AES64DSM    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.aes64dsm, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    AES64IM     -> List(SrcType.reg, SrcType.DC,  SrcType.DC, FuType.bku, BKUOpType.aes64im, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    AES64KS1I   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.bku, BKUOpType.aes64ks1i, Y, N, N, N, N, N, N, SelImm.IMM_I, N),
    AES64KS2    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.aes64ks2, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SHA256SUM0  -> List(SrcType.reg, SrcType.DC,  SrcType.DC, FuType.bku, BKUOpType.sha256sum0, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SHA256SUM1  -> List(SrcType.reg, SrcType.DC,  SrcType.DC, FuType.bku, BKUOpType.sha256sum1, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SHA256SIG0  -> List(SrcType.reg, SrcType.DC,  SrcType.DC, FuType.bku, BKUOpType.sha256sig0, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SHA256SIG1  -> List(SrcType.reg, SrcType.DC,  SrcType.DC, FuType.bku, BKUOpType.sha256sig1, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SHA512SUM0  -> List(SrcType.reg, SrcType.DC,  SrcType.DC, FuType.bku, BKUOpType.sha512sum0, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SHA512SUM1  -> List(SrcType.reg, SrcType.DC,  SrcType.DC, FuType.bku, BKUOpType.sha512sum1, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SHA512SIG0  -> List(SrcType.reg, SrcType.DC,  SrcType.DC, FuType.bku, BKUOpType.sha512sig0, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SHA512SIG1  -> List(SrcType.reg, SrcType.DC,  SrcType.DC, FuType.bku, BKUOpType.sha512sig1, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SM3P0       -> List(SrcType.reg, SrcType.DC,  SrcType.DC, FuType.bku, BKUOpType.sm3p0, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SM3P1       -> List(SrcType.reg, SrcType.DC,  SrcType.DC, FuType.bku, BKUOpType.sm3p1, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SM4KS0      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.sm4ks0, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SM4KS1      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.sm4ks1, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SM4KS2      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.sm4ks2, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SM4KS3      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.sm4ks3, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SM4ED0      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.sm4ed0, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SM4ED1      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.sm4ed1, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SM4ED2      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.sm4ed2, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
    SM4ED3      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.bku, BKUOpType.sm4ed3, Y, N, N, N, N, N, N, SelImm.IMM_X, N),
  )
}

/**
 * FP Divide SquareRoot Constants
 */
object FDivSqrtDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
  FDIV_S    ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FDIV_D    ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N),
  FSQRT_S   ->List(SrcType.fp,  SrcType.imm, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, Y, SelImm.IMM_X, N),
  FSQRT_D   ->List(SrcType.fp,  SrcType.imm, SrcType.DC, FuType.fmisc, X, N, Y, N, N, N, N, N, SelImm.IMM_X, N)
  )
}

/**
 * Svinval extension Constants
 */
object SvinvalDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
  /* sinval_vma is like sfence.vma , but sinval_vma can be dispatched and issued like normal instructions while sfence.vma 
   * must assure it is the ONLY instrucion executing in backend.
   */
  SINVAL_VMA        ->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fence, FenceOpType.sfence, N, N, N, N, N, N, N, SelImm.IMM_X),
  /* sfecne.w.inval is the begin instrucion of a TLB flush which set *noSpecExec* and *blockBackward* signals 
   * so when it comes to dispatch , it will block all instruction after itself until all instrucions ahead of it in rob commit 
   * then dispatch and issue this instrucion to flush sbuffer to dcache
   * after this instrucion commits , issue following sinval_vma instructions (out of order) to flush TLB
   */
  SFENCE_W_INVAL    ->List(SrcType.DC, SrcType.DC, SrcType.DC, FuType.fence, FenceOpType.nofence, N, N, N, Y, Y, N, N, SelImm.IMM_X),
  /* sfecne.inval.ir is the end instrucion of a TLB flush which set *noSpecExec* *blockBackward* and *flushPipe* signals 
   * so when it comes to dispatch , it will wait until all sinval_vma ahead of it in rob commit 
   * then dispatch and issue this instrucion
   * when it commit at the head of rob , flush the pipeline since some instrucions have been fetched to ibuffer using old TLB map 
   */
  SFENCE_INVAL_IR   ->List(SrcType.DC, SrcType.DC, SrcType.DC, FuType.fence, FenceOpType.nofence, N, N, N, Y, Y, Y, N, SelImm.IMM_X)
  /* what is Svinval extension ? 
   *                       ----->             sfecne.w.inval
   * sfence.vma   vpn1     ----->             sinval_vma   vpn1
   * sfence.vma   vpn2     ----->             sinval_vma   vpn2
   *                       ----->             sfecne.inval.ir
   * 
   * sfence.vma should be executed in-order and it flushes the pipeline after committing
   * we can parallel sfence instrucions with this extension 
   */
    )
}
/*
 * CBO decode
 */
object CBODecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    CBO_ZERO  -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.stu, LSUOpType.cbo_zero , N, N, N, N, N, N, N, SelImm.IMM_S),
    CBO_CLEAN -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.stu, LSUOpType.cbo_clean, N, N, N, N, N, N, N, SelImm.IMM_S),
    CBO_FLUSH -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.stu, LSUOpType.cbo_flush, N, N, N, N, N, N, N, SelImm.IMM_S),
    CBO_INVAL -> List(SrcType.reg, SrcType.DC, SrcType.DC, FuType.stu, LSUOpType.cbo_inval, N, N, N, N, N, N, N, SelImm.IMM_S)
  )
}

/**
 * XiangShan Trap Decode constants
 */
object XSTrapDecode extends DecodeConstants {
  // calculate as ADDI => addi zero, a0, 0
  // replace rs '?????' with '01010'(a0) in decode stage
  def lsrc1 = "b01010".U // $a0
  val table: Array[(BitPat, List[BitPat])] = Array(
    TRAP    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.add, Y, N, Y, Y, Y, N, N, SelImm.IMM_I, N)
  )
}

object CustomDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    CUSTOM0->           List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM0_RS1->       List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM0_RS1_RS2->   List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM0_RD->        List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM0_RD_RS1->    List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM0_RD_RS1_RS2->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM1->           List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM1_RS1->       List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM1_RS1_RS2->   List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM1_RD->        List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM1_RD_RS1->    List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM1_RD_RS1_RS2->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM2->           List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM2_RS1->       List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM2_RS1_RS2->   List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM2_RD->        List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM2_RD_RS1->    List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM2_RD_RS1_RS2->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM3->           List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM3_RS1->       List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM3_RS1_RS2->   List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM3_RD->        List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM3_RD_RS1->    List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
    CUSTOM3_RD_RS1_RS2->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.xscc, XsCCOpType.xscc, Y, N, N, Y, N, N, N, SelImm.IMM_X, Y),
   )
 }


//object Imm32Gen {
//  def apply(sel: UInt, inst: UInt) = {
//    val sign = Mux(sel === SelImm.IMM_Z, 0.S, inst(31).asSInt)
//    val b30_20 = Mux(sel === SelImm.IMM_U, inst(30,20).asSInt, sign)
//    val b19_12 = Mux(sel =/= SelImm.IMM_U && sel =/= SelImm.IMM_UJ, sign, inst(19,12).asSInt)
//    val b11 = Mux(sel === SelImm.IMM_U || sel === SelImm.IMM_Z, 0.S,
//              Mux(sel === SelImm.IMM_UJ, inst(20).asSInt,
//              Mux(sel === SelImm.IMM_SB, inst(7).asSInt, sign)))
//    val b10_5 = Mux(sel === SelImm.IMM_U || sel === SelImm.IMM_Z, 0.U(1.W), inst(30,25))
//    val b4_1 = Mux(sel === SelImm.IMM_U, 0.U(1.W),
//               Mux(sel === SelImm.IMM_S || sel === SelImm.IMM_SB, inst(11,8),
//               Mux(sel === SelImm.IMM_Z, inst(19,16), inst(24,21))))
//    val b0 = Mux(sel === SelImm.IMM_S, inst(7),
//             Mux(sel === SelImm.IMM_I, inst(20),
//             Mux(sel === SelImm.IMM_Z, inst(15), 0.U(1.W))))
//
//    Cat(sign, b30_20, b19_12, b11, b10_5, b4_1, b0)
//  }
//}

abstract class Imm(val len: Int) extends Bundle {
  def toImm32(minBits: UInt): UInt = do_toImm32(minBits(len - 1, 0))
  def do_toImm32(minBits: UInt): UInt
  def minBitsFromInstr(instr: UInt): UInt
}

case class Imm_I() extends Imm(12) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31, 20))
}

case class Imm_S() extends Imm(12) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31, 25), instr(11, 7))
}

case class Imm_B() extends Imm(12) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31), instr(7), instr(30, 25), instr(11, 8))
}

case class Imm_U() extends Imm(20){
  override def do_toImm32(minBits: UInt): UInt = Cat(minBits, 0.U(12.W))

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(31, 12)
  }
}

case class Imm_J() extends Imm(20){
  override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(31), instr(19, 12), instr(20), instr(30, 25), instr(24, 21))
  }
}

case class Imm_Z() extends Imm(12 + 5){
  override def do_toImm32(minBits: UInt): UInt = minBits

  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(19, 15), instr(31, 20))
  }
}

case class Imm_B6() extends Imm(6){
  override def do_toImm32(minBits: UInt): UInt = ZeroExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(25, 20)
  }
}

object ImmUnion {
  val I = Imm_I()
  val S = Imm_S()
  val B = Imm_B()
  val U = Imm_U()
  val J = Imm_J()
  val Z = Imm_Z()
  val B6 = Imm_B6()
  val imms = Seq(I, S, B, U, J, Z, B6)
  val maxLen = imms.maxBy(_.len).len
  val immSelMap = Seq(
    SelImm.IMM_I,
    SelImm.IMM_S,
    SelImm.IMM_SB,
    SelImm.IMM_U,
    SelImm.IMM_UJ,
    SelImm.IMM_Z,
    SelImm.IMM_B6
  ).zip(imms)
  println(s"ImmUnion max len: $maxLen")
}


/**
 * IO bundle for the Decode unit
 */
class DecodeUnitIO(implicit p: Parameters) extends XSBundle {
  val enq = new Bundle { val ctrl_flow = Input(new CtrlFlow) }
  val deq = new Bundle { val cf_ctrl = Output(new CfCtrl) }
  val csrCtrl = Input(new CustomCSRCtrlIO)
}

/**
 * Decode unit that takes in a single CtrlFlow and generates a CfCtrl.
 */
class DecodeUnit(implicit p: Parameters) extends XSModule with DecodeUnitConstants {
  val io = IO(new DecodeUnitIO)

  val ctrl_flow = Wire(new CtrlFlow) // input with RVC Expanded
  val cf_ctrl = Wire(new CfCtrl)

  ctrl_flow := io.enq.ctrl_flow

<<<<<<< HEAD
  val decode_table = XDecode.table ++ FDecode.table ++ FDivSqrtDecode.table ++ X64Decode.table ++ XSTrapDecode.table ++ BDecode.table ++ CustomDecode.table
=======
  val decode_table = XDecode.table ++ FDecode.table ++ FDivSqrtDecode.table ++ X64Decode.table ++ XSTrapDecode.table ++ BDecode.table ++ CBODecode.table ++ SvinvalDecode.table
>>>>>>> master

  // output
  cf_ctrl.cf := ctrl_flow
  val cs = Wire(new CtrlSignals()).decode(ctrl_flow.instr, decode_table)
  cs.singleStep := false.B
  cs.replayInst := false.B

  val fpDecoder = Module(new FPDecoder)
  fpDecoder.io.instr := ctrl_flow.instr
  cs.fpu := fpDecoder.io.fpCtrl

  val isMove = BitPat("b000000000000_?????_000_?????_0010011") === ctrl_flow.instr
  cs.isMove := isMove && ctrl_flow.instr(RD_MSB, RD_LSB) =/= 0.U

  // read src1~3 location
  cs.lsrc(0) := Mux(ctrl_flow.instr === LUI, 0.U, ctrl_flow.instr(RS1_MSB, RS1_LSB))
  cs.lsrc(1) := ctrl_flow.instr(RS2_MSB, RS2_LSB)
  cs.lsrc(2) := ctrl_flow.instr(RS3_MSB, RS3_LSB)
  // read dest location
  cs.ldest := ctrl_flow.instr(RD_MSB, RD_LSB)

  // fill in exception vector
  cf_ctrl.cf.exceptionVec := io.enq.ctrl_flow.exceptionVec
  cf_ctrl.cf.exceptionVec(illegalInstr) := cs.selImm === SelImm.INVALID_INSTR

  when (!io.csrCtrl.svinval_enable) {
    val base_ii = cs.selImm === SelImm.INVALID_INSTR
    val sinval = BitPat("b0001011_?????_?????_000_00000_1110011") === ctrl_flow.instr
    val w_inval = BitPat("b0001100_00000_00000_000_00000_1110011") === ctrl_flow.instr
    val inval_ir = BitPat("b0001100_00001_00000_000_00000_1110011") === ctrl_flow.instr
    val svinval_ii = sinval || w_inval || inval_ir
    cf_ctrl.cf.exceptionVec(illegalInstr) := base_ii || svinval_ii
    cs.flushPipe := false.B
  }

  // fix frflags
  //                           fflags    zero csrrs rd    csr
  val isFrflags = BitPat("b000000000001_00000_010_?????_1110011") === ctrl_flow.instr
  when (cs.fuType === FuType.csr && isFrflags) {
    cs.blockBackward := false.B
  }

  // fix isXSTrap
  when (cs.isXSTrap) {
    cs.lsrc(0) := XSTrapDecode.lsrc1
  }

  //to selectout prefetch.r/prefetch.w
  val isORI = BitPat("b?????????????????110?????0010011") === ctrl_flow.instr
  when(isORI) {
    // TODO: add CSR based Zicbop config
    when(cs.ldest === 0.U) {
      cs.selImm := SelImm.IMM_S
      cs.fuType := FuType.ldu
      when(cs.lsrc(1) === "b00001".U) {
        cs.fuOpType := LSUOpType.prefetch_r
      }.otherwise {
        cs.fuOpType := LSUOpType.prefetch_w
      }
    }
  }

  cs.imm := LookupTree(cs.selImm, ImmUnion.immSelMap.map(
    x => {
      val minBits = x._2.minBitsFromInstr(ctrl_flow.instr)
      require(minBits.getWidth == x._2.len)
      x._1 -> minBits
    }
  ))

  cf_ctrl.ctrl := cs

  // TODO: do we still need this?
  // fix ret and call
//  when (cs.fuType === FuType.jmp) {
//    def isLink(reg: UInt) = (reg === 1.U || reg === 5.U)
//    when (isLink(cs.ldest) && cs.fuOpType === JumpOpType.jal) { cf_ctrl.ctrl.fuOpType := JumpOpType.call }
//    when (cs.fuOpType === JumpOpType.jalr) {
//      when (isLink(cs.lsrc(0))) { cf_ctrl.ctrl.fuOpType := JumpOpType.ret  }
//      when (isLink(cs.ldest)) { cf_ctrl.ctrl.fuOpType := JumpOpType.call }
//    }
//  }

  io.deq.cf_ctrl := cf_ctrl

  //-------------------------------------------------------------
  // Debug Info
  XSDebug("in:  instr=%x pc=%x excepVec=%b intrVec=%b crossPageIPFFix=%d\n",
    io.enq.ctrl_flow.instr, io.enq.ctrl_flow.pc, io.enq.ctrl_flow.exceptionVec.asUInt,
    io.enq.ctrl_flow.intrVec.asUInt, io.enq.ctrl_flow.crossPageIPFFix)
  XSDebug("out: srcType(0)=%b srcType(1)=%b srcType(2)=%b lsrc(0)=%d lsrc(1)=%d lsrc(2)=%d ldest=%d fuType=%b fuOpType=%b\n",
    io.deq.cf_ctrl.ctrl.srcType(0), io.deq.cf_ctrl.ctrl.srcType(1), io.deq.cf_ctrl.ctrl.srcType(2),
    io.deq.cf_ctrl.ctrl.lsrc(0), io.deq.cf_ctrl.ctrl.lsrc(1), io.deq.cf_ctrl.ctrl.lsrc(2),
    io.deq.cf_ctrl.ctrl.ldest, io.deq.cf_ctrl.ctrl.fuType, io.deq.cf_ctrl.ctrl.fuOpType)
  XSDebug("out: rfWen=%d fpWen=%d isXSTrap=%d noSpecExec=%d isBlocked=%d flushPipe=%d isRVF=%d imm=%x\n",
    io.deq.cf_ctrl.ctrl.rfWen, io.deq.cf_ctrl.ctrl.fpWen, io.deq.cf_ctrl.ctrl.isXSTrap,
    io.deq.cf_ctrl.ctrl.noSpecExec, io.deq.cf_ctrl.ctrl.blockBackward, io.deq.cf_ctrl.ctrl.flushPipe,
    io.deq.cf_ctrl.ctrl.isRVF, io.deq.cf_ctrl.ctrl.imm)
  XSDebug("out: excepVec=%b intrVec=%b\n",
    io.deq.cf_ctrl.cf.exceptionVec.asUInt, io.deq.cf_ctrl.cf.intrVec.asUInt)
}

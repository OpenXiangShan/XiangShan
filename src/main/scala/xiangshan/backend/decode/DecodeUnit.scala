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
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util.uintToBitPat
import utility._
import utils._
import xiangshan.ExceptionNO.illegalInstr
import xiangshan._
import xiangshan.backend.fu.FuType
import xiangshan.backend.Bundles.{DecodedInst, DynInst, StaticInst}
import xiangshan.backend.decode.isa.bitfield.{InstVType, XSInstBitFields}
import xiangshan.backend.fu.vector.Bundles.{Category, VType}

/**
 * Abstract trait giving defaults and other relevant values to different Decode constants/
 */
abstract trait DecodeConstants {
  // This X should be used only in 1-bit signal. Otherwise, use BitPat("b???") to align with the width of UInt.
  def X = BitPat("b0")
  def N = BitPat("b0")
  def Y = BitPat("b1")
  def T = true
  def F = false

  def decodeDefault: List[BitPat] = // illegal instruction
    //   srcType(0) srcType(1) srcType(2) fuType    fuOpType    rfWen
    //   |          |          |          |         |           |  fpWen
    //   |          |          |          |         |           |  |  vecWen
    //   |          |          |          |         |           |  |  |  isXSTrap
    //   |          |          |          |         |           |  |  |  |  noSpecExec
    //   |          |          |          |         |           |  |  |  |  |  blockBackward
    //   |          |          |          |         |           |  |  |  |  |  |  flushPipe
    //   |          |          |          |         |           |  |  |  |  |  |  |  canRobCompress
    //   |          |          |          |         |           |  |  |  |  |  |  |  |  uopSplitType
    //   |          |          |          |         |           |  |  |  |  |  |  |  |  |             selImm
    List(SrcType.X, SrcType.X, SrcType.X, FuType.X, FuOpType.X, N, N, N, N, N, N, N, N, UopSplitType.X, SelImm.INVALID_INSTR) // Use SelImm to indicate invalid instr

  val decodeArray: Array[(BitPat, XSDecodeBase)]
  final def table: Array[(BitPat, List[BitPat])] = decodeArray.map(x => (x._1, x._2.generate()))
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

abstract class XSDecodeBase {
  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")
  def T = true
  def F = false
  def generate() : List[BitPat]
}

case class XSDecode(
  src1: BitPat, src2: BitPat, src3: BitPat,
  fu: FuType.OHType, fuOp: BitPat, selImm: BitPat,
  uopSplitType: BitPat = UopSplitType.X,
  xWen: Boolean = false,
  fWen: Boolean = false,
  vWen: Boolean = false,
  mWen: Boolean = false,
  xsTrap: Boolean = false,
  noSpec: Boolean = false,
  blockBack: Boolean = false,
  flushPipe: Boolean = false,
  canRobCompress: Boolean = false,
) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    List (src1, src2, src3, BitPat(fu.U(FuType.num.W)), fuOp, xWen.B, fWen.B, (vWen || mWen).B, xsTrap.B, noSpec.B, blockBack.B, flushPipe.B, canRobCompress.B, uopSplitType, selImm)
  }
}

case class FDecode(
  src1: BitPat, src2: BitPat, src3: BitPat,
  fu: FuType.OHType, fuOp: BitPat, selImm: BitPat = SelImm.X,
  uopSplitType: BitPat = UopSplitType.X,
  xWen: Boolean = false,
  fWen: Boolean = false,
  vWen: Boolean = false,
  mWen: Boolean = false,
  xsTrap: Boolean = false,
  noSpec: Boolean = false,
  blockBack: Boolean = false,
  flushPipe: Boolean = false,
  canRobCompress: Boolean = false,
) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    XSDecode(src1, src2, src3, fu, fuOp, selImm, uopSplitType, xWen, fWen, vWen, mWen, xsTrap, noSpec, blockBack, flushPipe, canRobCompress).generate()
  }
}

/**
 * Decode constants for RV64
 */
object X64Decode extends DecodeConstants {
  val decodeArray: Array[(BitPat, XSDecodeBase)] = Array(
    LD      -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.ldu, LSUOpType.ld  , SelImm.IMM_I, xWen = T),
    LWU     -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.ldu, LSUOpType.lwu , SelImm.IMM_I, xWen = T),
    SD      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.stu, LSUOpType.sd  , SelImm.IMM_S          ),

    SLLI    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.sll , SelImm.IMM_I, xWen = T, canRobCompress = T),
    SRLI    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.srl , SelImm.IMM_I, xWen = T, canRobCompress = T),
    SRAI    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.sra , SelImm.IMM_I, xWen = T, canRobCompress = T),

    ADDIW   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.addw, SelImm.IMM_I, xWen = T, canRobCompress = T),
    SLLIW   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.sllw, SelImm.IMM_I, xWen = T, canRobCompress = T),
    SRAIW   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.sraw, SelImm.IMM_I, xWen = T, canRobCompress = T),
    SRLIW   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.srlw, SelImm.IMM_I, xWen = T, canRobCompress = T),

    ADDW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.addw, SelImm.X    , xWen = T, canRobCompress = T),
    SUBW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.subw, SelImm.X    , xWen = T, canRobCompress = T),
    SLLW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sllw, SelImm.X    , xWen = T, canRobCompress = T),
    SRAW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sraw, SelImm.X    , xWen = T, canRobCompress = T),
    SRLW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.srlw, SelImm.X    , xWen = T, canRobCompress = T),

    RORW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.rorw, SelImm.X    , xWen = T, canRobCompress = T),
    RORIW   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.rorw, SelImm.IMM_I, xWen = T, canRobCompress = T),
    ROLW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.rolw, SelImm.X    , xWen = T, canRobCompress = T),
  )
}

/**
 * Overall Decode constants
 */
object XDecode extends DecodeConstants {
  val decodeArray: Array[(BitPat, XSDecodeBase)] = Array(
    LW      -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.ldu, LSUOpType.lw  , SelImm.IMM_I, xWen = T),
    LH      -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.ldu, LSUOpType.lh  , SelImm.IMM_I, xWen = T),
    LHU     -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.ldu, LSUOpType.lhu , SelImm.IMM_I, xWen = T),
    LB      -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.ldu, LSUOpType.lb  , SelImm.IMM_I, xWen = T),
    LBU     -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.ldu, LSUOpType.lbu , SelImm.IMM_I, xWen = T),
    SW      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.stu, LSUOpType.sw  , SelImm.IMM_S          ),
    SH      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.stu, LSUOpType.sh  , SelImm.IMM_S          ),
    SB      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.stu, LSUOpType.sb  , SelImm.IMM_S          ),
    LUI     -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.add , SelImm.IMM_U, xWen = T, canRobCompress = T),
    ADDI    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.add , SelImm.IMM_I, xWen = T, canRobCompress = T),
    ANDI    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.and , SelImm.IMM_I, xWen = T, canRobCompress = T),
    ORI     -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.or  , SelImm.IMM_I, xWen = T, canRobCompress = T),
    XORI    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.xor , SelImm.IMM_I, xWen = T, canRobCompress = T),
    SLTI    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.slt , SelImm.IMM_I, xWen = T, canRobCompress = T),
    SLTIU   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.sltu, SelImm.IMM_I, xWen = T, canRobCompress = T),
    SLL     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sll , SelImm.X    , xWen = T, canRobCompress = T),
    ADD     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.add , SelImm.X    , xWen = T, canRobCompress = T),
    SUB     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sub , SelImm.X    , xWen = T, canRobCompress = T),
    SLT     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.slt , SelImm.X    , xWen = T, canRobCompress = T),
    SLTU    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sltu, SelImm.X    , xWen = T, canRobCompress = T),
    AND     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.and , SelImm.X    , xWen = T, canRobCompress = T),
    OR      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.or  , SelImm.X    , xWen = T, canRobCompress = T),
    XOR     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.xor , SelImm.X    , xWen = T, canRobCompress = T),
    SRA     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sra , SelImm.X    , xWen = T, canRobCompress = T),
    SRL     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.srl , SelImm.X    , xWen = T, canRobCompress = T),

    MUL     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mul, MDUOpType.mul   , SelImm.X, xWen = T, canRobCompress = T),
    MULH    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mul, MDUOpType.mulh  , SelImm.X, xWen = T, canRobCompress = T),
    MULHU   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mul, MDUOpType.mulhu , SelImm.X, xWen = T, canRobCompress = T),
    MULHSU  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mul, MDUOpType.mulhsu, SelImm.X, xWen = T, canRobCompress = T),
    MULW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mul, MDUOpType.mulw  , SelImm.X, xWen = T, canRobCompress = T),

    DIV     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, MDUOpType.div   , SelImm.X, xWen = T, canRobCompress = T),
    DIVU    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, MDUOpType.divu  , SelImm.X, xWen = T, canRobCompress = T),
    REM     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, MDUOpType.rem   , SelImm.X, xWen = T, canRobCompress = T),
    REMU    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, MDUOpType.remu  , SelImm.X, xWen = T, canRobCompress = T),
    DIVW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, MDUOpType.divw  , SelImm.X, xWen = T, canRobCompress = T),
    DIVUW   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, MDUOpType.divuw , SelImm.X, xWen = T, canRobCompress = T),
    REMW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, MDUOpType.remw  , SelImm.X, xWen = T, canRobCompress = T),
    REMUW   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, MDUOpType.remuw , SelImm.X, xWen = T, canRobCompress = T),

    AUIPC   -> XSDecode(SrcType.pc , SrcType.imm, SrcType.X, FuType.

      jmp, JumpOpType.auipc, SelImm.IMM_U , xWen = T),
    JAL     -> XSDecode(SrcType.pc , SrcType.imm, SrcType.X, FuType.jmp, JumpOpType.jal  , SelImm.IMM_UJ, xWen = T),
    JALR    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.jmp, JumpOpType.jalr , SelImm.IMM_I , uopSplitType = UopSplitType.SCA_SIM, xWen = T),
    BEQ     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.brh, BRUOpType.beq   , SelImm.IMM_SB          ),
    BNE     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.brh, BRUOpType.bne   , SelImm.IMM_SB          ),
    BGE     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.brh, BRUOpType.bge   , SelImm.IMM_SB          ),
    BGEU    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.brh, BRUOpType.bgeu  , SelImm.IMM_SB          ),
    BLT     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.brh, BRUOpType.blt   , SelImm.IMM_SB          ),
    BLTU    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.brh, BRUOpType.bltu  , SelImm.IMM_SB          ),

    // I-type, the immediate12 holds the CSR register.
    CSRRW   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt , SelImm.IMM_I, xWen = T, noSpec = T, blockBack = T),
    CSRRS   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.set , SelImm.IMM_I, xWen = T, noSpec = T, blockBack = T),
    CSRRC   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.clr , SelImm.IMM_I, xWen = T, noSpec = T, blockBack = T),

    CSRRWI  -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrti, SelImm.IMM_Z, xWen = T, noSpec = T, blockBack = T),
    CSRRSI  -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.seti, SelImm.IMM_Z, xWen = T, noSpec = T, blockBack = T),
    CSRRCI  -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.clri, SelImm.IMM_Z, xWen = T, noSpec = T, blockBack = T),

    EBREAK  -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.jmp, SelImm.IMM_I, xWen = T, noSpec = T, blockBack = T),
    ECALL   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.jmp, SelImm.IMM_I, xWen = T, noSpec = T, blockBack = T),
    SRET    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.jmp, SelImm.IMM_I, xWen = T, noSpec = T, blockBack = T),
    MRET    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.jmp, SelImm.IMM_I, xWen = T, noSpec = T, blockBack = T),
    DRET    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.jmp, SelImm.IMM_I, xWen = T, noSpec = T, blockBack = T),
    WFI     -> XSDecode(SrcType.pc , SrcType.imm, SrcType.X, FuType.csr, CSROpType.wfi, SelImm.X    , xWen = T, noSpec = T, blockBack = T),

    SFENCE_VMA -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.fence, FenceOpType.sfence, SelImm.X, noSpec = T, blockBack = T, flushPipe = T),
    FENCE_I    -> XSDecode(SrcType.pc , SrcType.imm, SrcType.X, FuType.fence, FenceOpType.fencei, SelImm.X, noSpec = T, blockBack = T, flushPipe = T),
    FENCE      -> XSDecode(SrcType.pc , SrcType.imm, SrcType.X, FuType.fence, FenceOpType.fence , SelImm.X, noSpec = T, blockBack = T, flushPipe = T),

    // A-type
    AMOADD_W  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoadd_w , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOXOR_W  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoxor_w , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOSWAP_W -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoswap_w, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOAND_W  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoand_w , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOOR_W   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoor_w  , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMIN_W  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomin_w , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMINU_W -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amominu_w, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMAX_W  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomax_w , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMAXU_W -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomaxu_w, SelImm.X, xWen = T, noSpec = T, blockBack = T),

    AMOADD_D  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoadd_d,  SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOXOR_D  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoxor_d,  SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOSWAP_D -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoswap_d, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOAND_D  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoand_d,  SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOOR_D   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoor_d,   SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMIN_D  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomin_d,  SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMINU_D -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amominu_d, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMAX_D  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomax_d,  SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMAXU_D -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomaxu_d, SelImm.X, xWen = T, noSpec = T, blockBack = T),

    LR_W    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.mou, LSUOpType.lr_w, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    LR_D    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.mou, LSUOpType.lr_d, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    SC_W    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.sc_w, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    SC_D    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.sc_d, SelImm.X, xWen = T, noSpec = T, blockBack = T),

    ANDN    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.andn, SelImm.X, xWen = T, canRobCompress = T),
    ORN     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.orn , SelImm.X, xWen = T, canRobCompress = T),
    XNOR    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.xnor, SelImm.X, xWen = T, canRobCompress = T),
    ORC_B   -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.alu, ALUOpType.orcb, SelImm.X, xWen = T, canRobCompress = T),

    MIN     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.min , SelImm.X, xWen = T, canRobCompress = T),
    MINU    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.minu, SelImm.X, xWen = T, canRobCompress = T),
    MAX     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.max , SelImm.X, xWen = T, canRobCompress = T),
    MAXU    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.maxu, SelImm.X, xWen = T, canRobCompress = T),

    SEXT_B  -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.alu, ALUOpType.sextb, SelImm.X, xWen = T, canRobCompress = T),
    PACKH   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.packh, SelImm.X, xWen = T, canRobCompress = T),
    SEXT_H  -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.alu, ALUOpType.sexth, SelImm.X, xWen = T, canRobCompress = T),
    PACKW   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.packw, SelImm.X, xWen = T, canRobCompress = T),
    BREV8   -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.alu, ALUOpType.revb , SelImm.X, xWen = T, canRobCompress = T),
    REV8    -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.alu, ALUOpType.rev8 , SelImm.X, xWen = T, canRobCompress = T),
    PACK    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.pack , SelImm.X, xWen = T, canRobCompress = T),

    BSET    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.bset, SelImm.X    , xWen = T, canRobCompress = T),
    BSETI   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.bset, SelImm.IMM_I, xWen = T, canRobCompress = T),
    BCLR    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.bclr, SelImm.X    , xWen = T, canRobCompress = T),
    BCLRI   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.bclr, SelImm.IMM_I, xWen = T, canRobCompress = T),
    BINV    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.binv, SelImm.X    , xWen = T, canRobCompress = T),
    BINVI   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.binv, SelImm.IMM_I, xWen = T, canRobCompress = T),
    BEXT    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.bext, SelImm.X    , xWen = T, canRobCompress = T),
    BEXTI   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.bext, SelImm.IMM_I, xWen = T, canRobCompress = T),

    ROR     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.ror, SelImm.X     , xWen = T, canRobCompress = T),
    RORI    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.ror, SelImm.IMM_I , xWen = T, canRobCompress = T),
    ROL     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.rol, SelImm.X     , xWen = T, canRobCompress = T),

    SH1ADD    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sh1add  , SelImm.X    , xWen = T, canRobCompress = T),
    SH2ADD    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sh2add  , SelImm.X    , xWen = T, canRobCompress = T),
    SH3ADD    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sh3add  , SelImm.X    , xWen = T, canRobCompress = T),
    SH1ADD_UW -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sh1adduw, SelImm.X    , xWen = T, canRobCompress = T),
    SH2ADD_UW -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sh2adduw, SelImm.X    , xWen = T, canRobCompress = T),
    SH3ADD_UW -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sh3adduw, SelImm.X    , xWen = T, canRobCompress = T),
    ADD_UW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.adduw   , SelImm.X    , xWen = T, canRobCompress = T),
    SLLI_UW   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.slliuw  , SelImm.IMM_I, xWen = T, canRobCompress = T),
  )
}

/**
 * FP Decode constants
 */
object FpDecode extends DecodeConstants{
  val decodeArray: Array[(BitPat, XSDecodeBase)] = Array(
    FLW     -> FDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.ldu, LSUOpType.lw, selImm = SelImm.IMM_I, fWen = T),
    FLD     -> FDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.ldu, LSUOpType.ld, selImm = SelImm.IMM_I, fWen = T),
    FSW     -> FDecode(SrcType.reg, SrcType.fp,  SrcType.X, FuType.stu, LSUOpType.sw, selImm = SelImm.IMM_S          ),
    FSD     -> FDecode(SrcType.reg, SrcType.fp,  SrcType.X, FuType.stu, LSUOpType.sd, selImm = SelImm.IMM_S          ),

    FMV_X_D -> FDecode(SrcType.fp , SrcType.imm, SrcType.X, FuType.fmisc, FuOpType.X, xWen = T, canRobCompress = T),
    FMV_X_W -> FDecode(SrcType.fp , SrcType.imm, SrcType.X, FuType.fmisc, FuOpType.X, xWen = T, canRobCompress = T),

    FMV_D_X -> FDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.i2f,   FuOpType.X, fWen = T, canRobCompress = T),
    FMV_W_X -> FDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.i2f,   FuOpType.X, fWen = T, canRobCompress = T),

    // FP to FP
    FCVT_S_D -> FDecode(SrcType.fp, SrcType.imm, SrcType.X, FuType.fmisc, FuOpType.X, fWen = T, canRobCompress = T),
    FCVT_D_S -> FDecode(SrcType.fp, SrcType.imm, SrcType.X, FuType.fmisc, FuOpType.X, fWen = T, canRobCompress = T),

    // Int to FP
    FCVT_S_W  -> FDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.i2f, FuOpType.X, fWen = T, canRobCompress = T),
    FCVT_S_WU -> FDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.i2f, FuOpType.X, fWen = T, canRobCompress = T),
    FCVT_S_L  -> FDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.i2f, FuOpType.X, fWen = T, canRobCompress = T),
    FCVT_S_LU -> FDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.i2f, FuOpType.X, fWen = T, canRobCompress = T),

    FCVT_D_W  -> FDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.i2f, FuOpType.X, fWen = T, canRobCompress = T),
    FCVT_D_WU -> FDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.i2f, FuOpType.X, fWen = T, canRobCompress = T),
    FCVT_D_L  -> FDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.i2f, FuOpType.X, fWen = T, canRobCompress = T),
    FCVT_D_LU -> FDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.i2f, FuOpType.X, fWen = T, canRobCompress = T),

    // FP to Int
    FCVT_W_S  -> FDecode(SrcType.fp , SrcType.imm, SrcType.X, FuType.fmisc, FuOpType.X, xWen = T, canRobCompress = T),
    FCVT_WU_S -> FDecode(SrcType.fp , SrcType.imm, SrcType.X, FuType.fmisc, FuOpType.X, xWen = T, canRobCompress = T),
    FCVT_L_S  -> FDecode(SrcType.fp , SrcType.imm, SrcType.X, FuType.fmisc, FuOpType.X, xWen = T, canRobCompress = T),
    FCVT_LU_S -> FDecode(SrcType.fp , SrcType.imm, SrcType.X, FuType.fmisc, FuOpType.X, xWen = T, canRobCompress = T),

    FCVT_W_D  -> FDecode(SrcType.fp , SrcType.imm, SrcType.X, FuType.fmisc, FuOpType.X, xWen = T, canRobCompress = T),
    FCVT_WU_D -> FDecode(SrcType.fp , SrcType.imm, SrcType.X, FuType.fmisc, FuOpType.X, xWen = T, canRobCompress = T),
    FCVT_L_D  -> FDecode(SrcType.fp , SrcType.imm, SrcType.X, FuType.fmisc, FuOpType.X, xWen = T, canRobCompress = T),
    FCVT_LU_D -> FDecode(SrcType.fp , SrcType.imm, SrcType.X, FuType.fmisc, FuOpType.X, xWen = T, canRobCompress = T),

  )
}

/**
  * Bit Manipulation Decode
  */
object BDecode extends DecodeConstants{
  val decodeArray: Array[(BitPat, XSDecodeBase)] = Array(
    // Basic bit manipulation
    CLZ     -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.clz,    SelImm.X, xWen = T, canRobCompress = T),
    CTZ     -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.ctz,    SelImm.X, xWen = T, canRobCompress = T),
    CPOP    -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.cpop,   SelImm.X, xWen = T, canRobCompress = T),
    XPERM8  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.xpermb, SelImm.X, xWen = T, canRobCompress = T),
    XPERM4  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.xpermn, SelImm.X, xWen = T, canRobCompress = T),

    CLZW    -> XSDecode(SrcType.reg, SrcType.DC, SrcType.X, FuType.bku, BKUOpType.clzw,    SelImm.X, xWen = T, canRobCompress = T),
    CTZW    -> XSDecode(SrcType.reg, SrcType.DC, SrcType.X, FuType.bku, BKUOpType.ctzw,    SelImm.X, xWen = T, canRobCompress = T),
    CPOPW   -> XSDecode(SrcType.reg, SrcType.DC, SrcType.X, FuType.bku, BKUOpType.cpopw,   SelImm.X, xWen = T, canRobCompress = T),

    CLMUL   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.clmul,  SelImm.X, xWen = T, canRobCompress = T),
    CLMULH  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.clmulh, SelImm.X, xWen = T, canRobCompress = T),
    CLMULR  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.clmulr, SelImm.X, xWen = T, canRobCompress = T),

    AES64ES    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.aes64es,    SelImm.X    , xWen = T, canRobCompress = T),
    AES64ESM   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.aes64esm,   SelImm.X    , xWen = T, canRobCompress = T),
    AES64DS    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.aes64ds,    SelImm.X    , xWen = T, canRobCompress = T),
    AES64DSM   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.aes64dsm,   SelImm.X    , xWen = T, canRobCompress = T),
    AES64IM    -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.aes64im,    SelImm.X    , xWen = T, canRobCompress = T),
    AES64KS1I  -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.bku, BKUOpType.aes64ks1i,  SelImm.IMM_I, xWen = T, canRobCompress = T),
    AES64KS2   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.aes64ks2,   SelImm.X    , xWen = T, canRobCompress = T),
    SHA256SUM0 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha256sum0, SelImm.X    , xWen = T, canRobCompress = T),
    SHA256SUM1 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha256sum1, SelImm.X    , xWen = T, canRobCompress = T),
    SHA256SIG0 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha256sig0, SelImm.X    , xWen = T, canRobCompress = T),
    SHA256SIG1 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha256sig1, SelImm.X    , xWen = T, canRobCompress = T),
    SHA512SUM0 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha512sum0, SelImm.X    , xWen = T, canRobCompress = T),
    SHA512SUM1 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha512sum1, SelImm.X    , xWen = T, canRobCompress = T),
    SHA512SIG0 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha512sig0, SelImm.X    , xWen = T, canRobCompress = T),
    SHA512SIG1 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha512sig1, SelImm.X    , xWen = T, canRobCompress = T),
    SM3P0      -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sm3p0,      SelImm.X    , xWen = T, canRobCompress = T),
    SM3P1      -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sm3p1,      SelImm.X    , xWen = T, canRobCompress = T),
    SM4KS0     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.sm4ks0,     SelImm.X    , xWen = T, canRobCompress = T),
    SM4KS1     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.sm4ks1,     SelImm.X    , xWen = T, canRobCompress = T),
    SM4KS2     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.sm4ks2,     SelImm.X    , xWen = T, canRobCompress = T),
    SM4KS3     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.sm4ks3,     SelImm.X    , xWen = T, canRobCompress = T),
    SM4ED0     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.sm4ed0,     SelImm.X    , xWen = T, canRobCompress = T),
    SM4ED1     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.sm4ed1,     SelImm.X    , xWen = T, canRobCompress = T),
    SM4ED2     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.sm4ed2,     SelImm.X    , xWen = T, canRobCompress = T),
    SM4ED3     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.sm4ed3,     SelImm.X    , xWen = T, canRobCompress = T),
  )
}

/**
 * FP Divide SquareRoot Constants
 */
object FDivSqrtDecode extends DecodeConstants {
  val decodeArray: Array[(BitPat, XSDecodeBase)] = Array(
    FDIV_S  -> FDecode(SrcType.fp,  SrcType.fp,  SrcType.X, FuType.fDivSqrt, FuOpType.X, fWen = T, canRobCompress = T),
    FDIV_D  -> FDecode(SrcType.fp,  SrcType.fp,  SrcType.X, FuType.fDivSqrt, FuOpType.X, fWen = T, canRobCompress = T),
    FSQRT_S -> FDecode(SrcType.fp,  SrcType.imm, SrcType.X, FuType.fDivSqrt, FuOpType.X, fWen = T, canRobCompress = T),
    FSQRT_D -> FDecode(SrcType.fp,  SrcType.imm, SrcType.X, FuType.fDivSqrt, FuOpType.X, fWen = T, canRobCompress = T),
  )
}

/**
 * Svinval extension Constants
 */
object SvinvalDecode extends DecodeConstants {
  val decodeArray: Array[(BitPat, XSDecodeBase)] = Array(
    /* sinval_vma is like sfence.vma , but sinval_vma can be dispatched and issued like normal instructions while sfence.vma
     * must assure it is the ONLY instrucion executing in backend.
     */
    SINVAL_VMA        -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.fence, FenceOpType.sfence, SelImm.X),
    /* sfecne.w.inval is the begin instrucion of a TLB flush which set *noSpecExec* and *blockBackward* signals
     * so when it comes to dispatch , it will block all instruction after itself until all instrucions ahead of it in rob commit
     * then dispatch and issue this instrucion to flush sbuffer to dcache
     * after this instrucion commits , issue following sinval_vma instructions (out of order) to flush TLB
     */
    SFENCE_W_INVAL    -> XSDecode(SrcType.DC, SrcType.DC, SrcType.X, FuType.fence, FenceOpType.nofence, SelImm.X, noSpec = T, blockBack = T),
    /* sfecne.inval.ir is the end instrucion of a TLB flush which set *noSpecExec* *blockBackward* and *flushPipe* signals
     * so when it comes to dispatch , it will wait until all sinval_vma ahead of it in rob commit
     * then dispatch and issue this instrucion
     * when it commit at the head of rob , flush the pipeline since some instrucions have been fetched to ibuffer using old TLB map
     */
    SFENCE_INVAL_IR   -> XSDecode(SrcType.DC, SrcType.DC, SrcType.X, FuType.fence, FenceOpType.nofence, SelImm.X, noSpec = T, blockBack = T, flushPipe = T)
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
  val decodeArray: Array[(BitPat, XSDecodeBase)] = Array(
    CBO_ZERO  -> XSDecode(SrcType.reg, SrcType.DC, SrcType.X, FuType.stu, LSUOpType.cbo_zero , SelImm.IMM_S),
    CBO_CLEAN -> XSDecode(SrcType.reg, SrcType.DC, SrcType.X, FuType.stu, LSUOpType.cbo_clean, SelImm.IMM_S),
    CBO_FLUSH -> XSDecode(SrcType.reg, SrcType.DC, SrcType.X, FuType.stu, LSUOpType.cbo_flush, SelImm.IMM_S),
    CBO_INVAL -> XSDecode(SrcType.reg, SrcType.DC, SrcType.X, FuType.stu, LSUOpType.cbo_inval, SelImm.IMM_S)
  )
}

/**
 * XiangShan Trap Decode constants
 */
object XSTrapDecode extends DecodeConstants {
  def TRAP = BitPat("b000000000000?????000000001101011")
  val decodeArray: Array[(BitPat, XSDecodeBase)] = Array(
    TRAP    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.add, SelImm.IMM_I, xWen = T, xsTrap = T, noSpec = T, blockBack = T)
  )
}

abstract class Imm(val len: Int) extends Bundle {
  def toImm32(minBits: UInt): UInt = do_toImm32(minBits(len - 1, 0))
  def do_toImm32(minBits: UInt): UInt
  def minBitsFromInstr(instr: UInt): UInt
}

case class Imm_I() extends Imm(12) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits(len - 1, 0), 32)

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
  override def do_toImm32(minBits: UInt): UInt = Cat(minBits(len - 1, 0), 0.U(12.W))

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

case class Imm_OPIVIS() extends Imm(5){
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(19, 15)
  }
}

case class Imm_OPIVIU() extends Imm(5){
  override def do_toImm32(minBits: UInt): UInt = ZeroExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(19, 15)
  }
}

case class Imm_VSETVLI() extends Imm(11){
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(30, 20)
  }
}

case class Imm_VSETIVLI() extends Imm(13){
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    val rvInst: XSInstBitFields = instr.asTypeOf(new XSInstBitFields)
    val uimm5 = rvInst.UIMM_VSETIVLI
    val vtype8 = rvInst.ZIMM_VTYPE
    Cat(uimm5, vtype8)
  }
  /**
    * get VType from extended imm
    * @param extedImm
    * @return VType
    */
  def getVType(extedImm: UInt): InstVType = {
    val vtype = Wire(new InstVType)
    vtype := extedImm(7, 0).asTypeOf(new InstVType)
    vtype
  }

  def getAvl(extedImm: UInt): UInt = {
    extedImm(12, 8)
  }
}

case class Imm_LUI32() extends Imm(32){
  override def do_toImm32(minBits: UInt): UInt = minBits(31, 0)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(31, 0)
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
  val OPIVIS = Imm_OPIVIS()
  val OPIVIU = Imm_OPIVIU()
  val VSETVLI = Imm_VSETVLI()
  val VSETIVLI = Imm_VSETIVLI()
  val LUI32 = Imm_LUI32()

  val imms = Seq(I, S, B, U, J, Z, B6, OPIVIS, OPIVIU, VSETVLI, VSETIVLI)
  val maxLen = imms.maxBy(_.len).len
  val immSelMap = Seq(
    SelImm.IMM_I,
    SelImm.IMM_S,
    SelImm.IMM_SB,
    SelImm.IMM_U,
    SelImm.IMM_UJ,
    SelImm.IMM_Z,
    SelImm.IMM_B6,
    SelImm.IMM_OPIVIS,
    SelImm.IMM_OPIVIU,
    SelImm.IMM_VSETVLI,
    SelImm.IMM_VSETIVLI,
  ).zip(imms)
  println(s"ImmUnion max len: $maxLen")
}

case class Imm_LUI_LOAD() {
  def immFromLuiLoad(lui_imm: UInt, load_imm: UInt): UInt = {
    val loadImm = load_imm(Imm_I().len - 1, 0)
    Cat(lui_imm(ImmUnion.maxLen - loadImm.getWidth - 1, 0), loadImm)
  }
  def getLuiImm(uop: DynInst): UInt = {
    val loadImmLen = Imm_I().len
    val imm_u = Cat(uop.psrc(1), uop.psrc(0), uop.imm(ImmUnion.maxLen - 1, loadImmLen))
    Cat(Imm_U().toImm32(imm_u)(31, loadImmLen), uop.imm(loadImmLen - 1, 0))
  }
}

/**
 * IO bundle for the Decode unit
 */
class DecodeUnitIO(implicit p: Parameters) extends XSBundle {
  val enq = new Bundle {
    val ctrlFlow = Input(new StaticInst)
    val vtype = Input(new VType)
  }
//  val vconfig = Input(UInt(XLEN.W))
  val deq = new Bundle {
    val decodedInst = Output(new DecodedInst)
    val isComplex = Output(Bool())
    val uopInfo = Output(new UopInfo)
  }
  val csrCtrl = Input(new CustomCSRCtrlIO)
}

/**
 * Decode unit that takes in a single CtrlFlow and generates a CfCtrl.
 */
class DecodeUnit(implicit p: Parameters) extends XSModule with DecodeUnitConstants {
  val io = IO(new DecodeUnitIO)

  val ctrl_flow = io.enq.ctrlFlow // input with RVC Expanded

  private val inst: XSInstBitFields = io.enq.ctrlFlow.instr.asTypeOf(new XSInstBitFields)

  val decode_table: Array[(BitPat, List[BitPat])] = XDecode.table ++
    FpDecode.table ++
//    FDivSqrtDecode.table ++
    X64Decode.table ++
    XSTrapDecode.table ++
    BDecode.table ++
    CBODecode.table ++
    SvinvalDecode.table ++
    VecDecoder.table

  require(decode_table.map(_._2.length == 15).reduce(_ && _), "Decode tables have different column size")
  // assertion for LUI: only LUI should be assigned `selImm === SelImm.IMM_U && fuType === FuType.alu`
  val luiMatch = (t: Seq[BitPat]) => t(3).value == FuType.alu.ohid && t.reverse.head.value == SelImm.IMM_U.litValue
  val luiTable = decode_table.filter(t => luiMatch(t._2)).map(_._1).distinct
  assert(luiTable.length == 1 && luiTable.head == LUI, "Conflicts: LUI is determined by FuType and SelImm in Dispatch")

  // output
  val decodedInst: DecodedInst = Wire(new DecodedInst()).decode(ctrl_flow.instr, decode_table)

  val fpDecoder = Module(new FPDecoder)
  fpDecoder.io.instr := ctrl_flow.instr
  decodedInst.fpu := fpDecoder.io.fpCtrl

  decodedInst.connectStaticInst(io.enq.ctrlFlow)

  decodedInst.uopIdx := 0.U
  decodedInst.firstUop := true.B
  decodedInst.lastUop := true.B
  decodedInst.numUops := 1.U

  val isMove = BitPat("b000000000000_?????_000_?????_0010011") === ctrl_flow.instr
  decodedInst.isMove := isMove && inst.RD =/= 0.U

  private val v0Idx = 0
  private val vconfigIdx = VCONFIG_IDX

  // read src1~3 location
  decodedInst.lsrc(0) := inst.RS1
  decodedInst.lsrc(1) := inst.RS2
  decodedInst.lsrc(2) := inst.FS3
  decodedInst.lsrc(3) := v0Idx.U
  decodedInst.lsrc(4) := vconfigIdx.U
  decodedInst.srcType(3) := Mux(inst.VM.asBool, SrcType.DC, SrcType.vp) // mask src
  decodedInst.srcType(4) := SrcType.vp // vconfig

  // cs.lsrc(2) := Mux(FuType.isVecExu(cs.fuType), ctrl_flow.instr(RD_MSB, RD_LSB), ctrl_flow.instr(RS3_MSB, RS3_LSB))
  // read dest location
  decodedInst.ldest := inst.RD

  // fill in exception vector
  val vecException = Module(new VecExceptionGen)
  vecException.io.inst := io.enq.ctrlFlow.instr
  vecException.io.decodedInst := decodedInst
  vecException.io.vtype := decodedInst.vpu.vtype
  decodedInst.exceptionVec(illegalInstr) := decodedInst.selImm === SelImm.INVALID_INSTR || vecException.io.illegalInst

  when (!io.csrCtrl.svinval_enable) {
    val base_ii = decodedInst.selImm === SelImm.INVALID_INSTR || vecException.io.illegalInst
    val sinval = BitPat("b0001011_?????_?????_000_00000_1110011") === ctrl_flow.instr
    val w_inval = BitPat("b0001100_00000_00000_000_00000_1110011") === ctrl_flow.instr
    val inval_ir = BitPat("b0001100_00001_00000_000_00000_1110011") === ctrl_flow.instr
    val svinval_ii = sinval || w_inval || inval_ir
    decodedInst.exceptionVec(illegalInstr) := base_ii || svinval_ii
    decodedInst.flushPipe := false.B
  }

  // fix frflags
  //                           fflags    zero csrrs rd    csr
  val isFrflags = BitPat("b000000000001_00000_010_?????_1110011") === ctrl_flow.instr
  when (decodedInst.fuType === FuType.csr.U && isFrflags) {
    decodedInst.blockBackward := false.B
  }

  decodedInst.imm := LookupTree(decodedInst.selImm, ImmUnion.immSelMap.map(
    x => {
      val minBits = x._2.minBitsFromInstr(ctrl_flow.instr)
      require(minBits.getWidth == x._2.len)
      x._1 -> minBits
    }
  ))

  decodedInst.commitType := 0.U // Todo: remove it

  decodedInst.isVset := FuType.isVset(decodedInst.fuType)

  private val needReverseInsts = Seq(VRSUB_VI, VRSUB_VX, VREM_VV, VREM_VX, VREMU_VV, VFRDIV_VF, VFRSUB_VF, VFMV_F_S)
  private val vextInsts = Seq(VZEXT_VF2, VZEXT_VF4, VZEXT_VF8, VSEXT_VF2, VSEXT_VF4, VSEXT_VF8)
  private val narrowInsts = Seq(
    VNSRA_WV, VNSRA_WX, VNSRA_WI, VNSRL_WV, VNSRL_WX, VNSRL_WI,
    VNCLIP_WV, VNCLIP_WX, VNCLIP_WI, VNCLIPU_WV, VNCLIPU_WX, VNCLIPU_WI,
  )
  private val maskDstInsts = Seq(
    VMADC_VV, VMADC_VX,  VMADC_VI,  VMADC_VVM, VMADC_VXM, VMADC_VIM,
    VMSBC_VV, VMSBC_VX,  VMSBC_VVM, VMSBC_VXM,
    VMAND_MM, VMNAND_MM, VMANDN_MM, VMXOR_MM, VMOR_MM, VMNOR_MM, VMORN_MM, VMXNOR_MM,
    VMSEQ_VV, VMSEQ_VX, VMSEQ_VI, VMSNE_VV, VMSNE_VX, VMSNE_VI,
    VMSLE_VV, VMSLE_VX, VMSLE_VI, VMSLEU_VV, VMSLEU_VX, VMSLEU_VI,
    VMSLT_VV, VMSLT_VX, VMSLTU_VV, VMSLTU_VX,
    VMSGT_VX, VMSGT_VI, VMSGTU_VX, VMSGTU_VI,
    VMFEQ_VV, VMFEQ_VF, VMFNE_VV, VMFNE_VF, VMFLT_VV, VMFLT_VF, VMFLE_VV, VMFLE_VF, VMFGT_VF, VMFGE_VF,
  )
  private val maskOpInsts = Seq(
    VMAND_MM, VMNAND_MM, VMANDN_MM, VMXOR_MM, VMOR_MM, VMNOR_MM, VMORN_MM, VMXNOR_MM,
  )
  private val wfflagsInsts = Seq(
    // opfff
    FADD_S, FSUB_S, FADD_D, FSUB_D,
    FEQ_S, FLT_S, FLE_S, FEQ_D, FLT_D, FLE_D,
    FMIN_S, FMAX_S, FMIN_D, FMAX_D,
    FMUL_S, FMUL_D,
    FDIV_S, FDIV_D, FSQRT_S, FSQRT_D,
    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S, FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D,
    FSGNJ_S, FSGNJN_S, FSGNJX_S,
    // opfvv
    VFADD_VV, VFSUB_VV, VFWADD_VV, VFWSUB_VV, VFWADD_WV, VFWSUB_WV,
    VFMUL_VV, VFDIV_VV, VFWMUL_VV,
    VFMACC_VV, VFNMACC_VV, VFMSAC_VV, VFNMSAC_VV, VFMADD_VV, VFNMADD_VV, VFMSUB_VV, VFNMSUB_VV,
    VFWMACC_VV, VFWNMACC_VV, VFWMSAC_VV, VFWNMSAC_VV,
    VFSQRT_V,
    VFMIN_VV, VFMAX_VV,
    VMFEQ_VV, VMFNE_VV, VMFLT_VV, VMFLE_VV,
    VFSGNJ_VV, VFSGNJN_VV, VFSGNJX_VV,
    // opfvf
    VFADD_VF, VFSUB_VF, VFRSUB_VF, VFWADD_VF, VFWSUB_VF, VFWADD_WF, VFWSUB_WF,
    VFMUL_VF, VFDIV_VF, VFRDIV_VF, VFWMUL_VF,
    VFMACC_VF, VFNMACC_VF, VFMSAC_VF, VFNMSAC_VF, VFMADD_VF, VFNMADD_VF, VFMSUB_VF, VFNMSUB_VF,
    VFWMACC_VF, VFWNMACC_VF, VFWMSAC_VF, VFWNMSAC_VF,
    VFMIN_VF, VFMAX_VF,
    VMFEQ_VF, VMFNE_VF, VMFLT_VF, VMFLE_VF, VMFGT_VF, VMFGE_VF,
    VFSGNJ_VF, VFSGNJN_VF, VFSGNJX_VF,
    // fcvt & vfcvt
    FCVT_S_W, FCVT_S_WU, FCVT_S_L, FCVT_S_LU,
    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
    FCVT_D_W, FCVT_D_WU, FCVT_D_L, FCVT_D_LU,
    FCVT_W_D, FCVT_WU_D, FCVT_L_D, FCVT_LU_D, FCVT_S_D, FCVT_D_S,
    VFCVT_XU_F_V, VFCVT_X_F_V, VFCVT_RTZ_XU_F_V, VFCVT_RTZ_X_F_V, VFCVT_F_XU_V, VFCVT_F_X_V,
    VFWCVT_XU_F_V, VFWCVT_X_F_V, VFWCVT_RTZ_XU_F_V, VFWCVT_RTZ_X_F_V, VFWCVT_F_XU_V, VFWCVT_F_X_V, VFWCVT_F_F_V,
    VFNCVT_XU_F_W, VFNCVT_X_F_W, VFNCVT_RTZ_XU_F_W, VFNCVT_RTZ_X_F_W, VFNCVT_F_XU_W, VFNCVT_F_X_W, VFNCVT_F_F_W,
    VFNCVT_ROD_F_F_W, VFRSQRT7_V, VFREC7_V,
  )
  decodedInst.wfflags := wfflagsInsts.map(_ === inst.ALL).reduce(_ || _)
  val fpToVecDecoder = Module(new FPToVecDecoder())
  fpToVecDecoder.io.instr := inst.asUInt
  val isFpToVecInst = fpToVecDecoder.io.vpuCtrl.fpu.isFpToVecInst
  decodedInst.vpu := 0.U.asTypeOf(decodedInst.vpu) // Todo: Connect vpu decoder
  when(isFpToVecInst){
    decodedInst.vpu := fpToVecDecoder.io.vpuCtrl
  }.otherwise{
    decodedInst.vpu.vill := io.enq.vtype.illegal
    decodedInst.vpu.vma := io.enq.vtype.vma
    decodedInst.vpu.vta := io.enq.vtype.vta
    decodedInst.vpu.vsew := io.enq.vtype.vsew
    decodedInst.vpu.vlmul := io.enq.vtype.vlmul
    decodedInst.vpu.vm := inst.VM
    decodedInst.vpu.nf := inst.NF
    decodedInst.vpu.needScalaSrc := Category.needScalaSrc(inst.VCATEGORY)
    decodedInst.vpu.permImmTruncate := Category.permImmTruncate(inst.VCATEGORY)
    decodedInst.vpu.isReverse := needReverseInsts.map(_ === inst.ALL).reduce(_ || _)
    decodedInst.vpu.isExt := vextInsts.map(_ === inst.ALL).reduce(_ || _)
    decodedInst.vpu.isNarrow := narrowInsts.map(_ === inst.ALL).reduce(_ || _)
    decodedInst.vpu.isDstMask := maskDstInsts.map(_ === inst.ALL).reduce(_ || _)
    decodedInst.vpu.isOpMask := maskOpInsts.map(_ === inst.ALL).reduce(_ || _)
  }

  val uopInfoGen = Module(new UopInfoGen)
  uopInfoGen.io.in.preInfo.typeOfSplit := decodedInst.uopSplitType
  uopInfoGen.io.in.preInfo.vsew := decodedInst.vpu.vsew
  uopInfoGen.io.in.preInfo.vlmul := decodedInst.vpu.vlmul
  uopInfoGen.io.in.preInfo.vwidth := inst.RM
  io.deq.isComplex := uopInfoGen.io.out.isComplex
  io.deq.uopInfo.numOfUop := uopInfoGen.io.out.uopInfo.numOfUop
  io.deq.uopInfo.lmul := uopInfoGen.io.out.uopInfo.lmul

  io.deq.decodedInst := decodedInst

  //-------------------------------------------------------------
  // Debug Info
//  XSDebug("in:  instr=%x pc=%x excepVec=%b crossPageIPFFix=%d\n",
//    io.enq.ctrl_flow.instr, io.enq.ctrl_flow.pc, io.enq.ctrl_flow.exceptionVec.asUInt,
//    io.enq.ctrl_flow.crossPageIPFFix)
//  XSDebug("out: srcType(0)=%b srcType(1)=%b srcType(2)=%b lsrc(0)=%d lsrc(1)=%d lsrc(2)=%d ldest=%d fuType=%b fuOpType=%b\n",
//    io.deq.cf_ctrl.ctrl.srcType(0), io.deq.cf_ctrl.ctrl.srcType(1), io.deq.cf_ctrl.ctrl.srcType(2),
//    io.deq.cf_ctrl.ctrl.lsrc(0), io.deq.cf_ctrl.ctrl.lsrc(1), io.deq.cf_ctrl.ctrl.lsrc(2),
//    io.deq.cf_ctrl.ctrl.ldest, io.deq.cf_ctrl.ctrl.fuType, io.deq.cf_ctrl.ctrl.fuOpType)
//  XSDebug("out: rfWen=%d fpWen=%d isXSTrap=%d noSpecExec=%d isBlocked=%d flushPipe=%d imm=%x\n",
//    io.deq.cf_ctrl.ctrl.rfWen, io.deq.cf_ctrl.ctrl.fpWen, io.deq.cf_ctrl.ctrl.isXSTrap,
//    io.deq.cf_ctrl.ctrl.noSpecExec, io.deq.cf_ctrl.ctrl.blockBackward, io.deq.cf_ctrl.ctrl.flushPipe,
//    io.deq.cf_ctrl.ctrl.imm)
//  XSDebug("out: excepVec=%b\n", io.deq.cf_ctrl.cf.exceptionVec.asUInt)
}

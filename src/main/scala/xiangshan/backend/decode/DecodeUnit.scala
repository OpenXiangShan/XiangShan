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

import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.CustomInstructions._
import freechips.rocketchip.rocket.Instructions._
import freechips.rocketchip.util.uintToBitPat
import utility._
import xiangshan._
import xiangshan.backend.decode.Zabha._
import xiangshan.backend.decode.isa.CSRReadOnlyBlockInstructions._
import xiangshan.backend.decode.isa.bitfield.{InstVType, XSInstBitFields}
import xiangshan.backend.fu.FuType
import yunsuan.MULOpType

/**
 * Abstract trait giving defaults and other relevant values to different Decode constants/
 */
trait DecodeConstants {
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
    //   |          |          |          |         |           |  |  |  noSpecExec
    //   |          |          |          |         |           |  |  |  |  blockBackward
    //   |          |          |          |         |           |  |  |  |  |  flushPipe
    //   |          |          |          |         |           |  |  |  |  |  |  canRobCompress
    //   |          |          |          |         |           |  |  |  |  |  |  |  uopSplitType
    //   |          |          |          |         |           |  |  |  |  |  |  |  |               selImm
    //   |          |          |          |         |           |  |  |  |  |  |  |  |               |
    List(SrcType.X, SrcType.X, SrcType.X, FuType.X, FuOpType.X, N, N, N, N, N, N, N, UopSplitType.X, SelImm.X) // Use SelImm to indicate invalid instr

  val decodeArray: Array[(BitPat, XSDecodeBase)]
  final def table: Array[(BitPat, List[BitPat])] = decodeArray.map(x => (x._1, x._2.generate()))
}



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
  noSpec: Boolean = false,
  blockBack: Boolean = false,
  flushPipe: Boolean = false,
  canRobCompress: Boolean = false,
) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    List (src1, src2, src3, BitPat(fu.U(FuType.num.W)), fuOp, xWen.B, fWen.B, (vWen || mWen).B, noSpec.B, blockBack.B, flushPipe.B, canRobCompress.B, uopSplitType, selImm)
  }
}

/**
 * Overall Decode constants
 */
object XDecode extends DecodeConstants {
  val decodeArray: Array[(BitPat, XSDecodeBase)] = Array(
    // RV32I
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

    // RV64I （extend from RV32I)
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

    // RV64M
    MUL     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mul, MULOpType.mul   , SelImm.X, xWen = T, canRobCompress = T),
    MULH    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mul, MULOpType.mulh  , SelImm.X, xWen = T, canRobCompress = T),
    MULHU   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mul, MULOpType.mulhu , SelImm.X, xWen = T, canRobCompress = T),
    MULHSU  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mul, MULOpType.mulhsu, SelImm.X, xWen = T, canRobCompress = T),
    MULW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mul, MULOpType.mulw  , SelImm.X, xWen = T, canRobCompress = T),

    DIV     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, DIVOpType.div   , SelImm.X, xWen = T, canRobCompress = T),
    DIVU    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, DIVOpType.divu  , SelImm.X, xWen = T, canRobCompress = T),
    REM     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, DIVOpType.rem   , SelImm.X, xWen = T, canRobCompress = T),
    REMU    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, DIVOpType.remu  , SelImm.X, xWen = T, canRobCompress = T),
    DIVW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, DIVOpType.divw  , SelImm.X, xWen = T, canRobCompress = T),
    DIVUW   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, DIVOpType.divuw , SelImm.X, xWen = T, canRobCompress = T),
    REMW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, DIVOpType.remw  , SelImm.X, xWen = T, canRobCompress = T),
    REMUW   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.div, DIVOpType.remuw , SelImm.X, xWen = T, canRobCompress = T),

    AUIPC   -> XSDecode(SrcType.pc , SrcType.imm, SrcType.X, FuType.jmp, JumpOpType.auipc, SelImm.IMM_U , xWen = T),
    JAL     -> XSDecode(SrcType.pc , SrcType.imm, SrcType.X, FuType.jmp, JumpOpType.jal  , SelImm.IMM_UJ, xWen = T),
    JALR    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.jmp, JumpOpType.jalr , SelImm.IMM_I , xWen = T),
    BEQ     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.brh, BRUOpType.beq   , SelImm.IMM_SB          ),
    BNE     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.brh, BRUOpType.bne   , SelImm.IMM_SB          ),
    BGE     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.brh, BRUOpType.bge   , SelImm.IMM_SB          ),
    BGEU    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.brh, BRUOpType.bgeu  , SelImm.IMM_SB          ),
    BLT     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.brh, BRUOpType.blt   , SelImm.IMM_SB          ),
    BLTU    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.brh, BRUOpType.bltu  , SelImm.IMM_SB          ),

    // System, the immediate12 holds the CSR register.

    CSRRW   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt , SelImm.IMM_Z, xWen = T, noSpec = T, blockBack = T),
    CSRRS   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.set , SelImm.IMM_Z, xWen = T, noSpec = T, blockBack = T),
    CSRRC   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.clr , SelImm.IMM_Z, xWen = T, noSpec = T, blockBack = T),

    CSRRWI  -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrti, SelImm.IMM_Z, xWen = T, noSpec = T, blockBack = T),
    CSRRSI  -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.seti, SelImm.IMM_Z, xWen = T, noSpec = T, blockBack = T),
    CSRRCI  -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.clri, SelImm.IMM_Z, xWen = T, noSpec = T, blockBack = T),
    // CSROpType is useless in read only csrInstr, because Wen is false and wdata is dependent on CSROpType.
    CSRs_readOnly -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z),
    CSRs_fflags   -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T),
    CSRs_fcsr     -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T),
    CSRs_vxsat    -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T),
    CSRs_vcsr     -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T),
    CSRs_vstart   -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T),
    CSRs_sstatus  -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T),
    CSRs_vsstatus -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T),
    CSRs_mstatus  -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T),
    CSRs_hstatus  -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T),
    CSRs_mnstatus -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T),
    CSRs_dcsr     -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T),
    CSRs_vtype    -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T),
    CSRs_mireg    -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T, blockBack = T),
    CSRs_sireg    -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T, blockBack = T),
    CSRs_vsireg   -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T, blockBack = T),
    CSRs_mtopi    -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T, blockBack = T),
    CSRs_stopi    -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T, blockBack = T),
    CSRs_vstopi   -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T, blockBack = T),
    CSRs_mtopei   -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T, blockBack = T),
    CSRs_stopei   -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T, blockBack = T),
    CSRs_vstopei  -> XSDecode(SrcType.X, SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrt, SelImm.IMM_Z, noSpec = T, blockBack = T),

    EBREAK  -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.jmp, SelImm.IMM_I, xWen = T, noSpec = T, blockBack = T),
    ECALL   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.jmp, SelImm.IMM_I, xWen = T, noSpec = T, blockBack = T),
    SRET    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.jmp, SelImm.IMM_I, xWen = T, noSpec = T, blockBack = T),
    MRET    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.jmp, SelImm.IMM_I, xWen = T, noSpec = T, blockBack = T),
    MNRET   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.jmp, SelImm.IMM_I, xWen = T, noSpec = T, blockBack = T),
    DRET    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.csr, CSROpType.jmp, SelImm.IMM_I, xWen = T, noSpec = T, blockBack = T),
    WFI     -> XSDecode(SrcType.pc , SrcType.imm, SrcType.X, FuType.csr, CSROpType.wfi, SelImm.X    , xWen = T, noSpec = T, blockBack = T),

    SFENCE_VMA -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.fence, FenceOpType.sfence, SelImm.X, noSpec = T, blockBack = T, flushPipe = T),
    FENCE_I    -> XSDecode(SrcType.pc , SrcType.imm, SrcType.X, FuType.fence, FenceOpType.fencei, SelImm.X, noSpec = T, blockBack = T, flushPipe = T),
    FENCE      -> XSDecode(SrcType.pc , SrcType.imm, SrcType.X, FuType.fence, FenceOpType.fence , SelImm.X, noSpec = T, blockBack = T, flushPipe = T),
    PAUSE      -> XSDecode(SrcType.pc , SrcType.imm, SrcType.X, FuType.fence, FenceOpType.fence , SelImm.X, noSpec = T, blockBack = T, flushPipe = T),

    // Zawrs
    WRS_NTO -> XSDecode(SrcType.pc , SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrs_nto, SelImm.X , noSpec = T, blockBack = T),
    WRS_STO -> XSDecode(SrcType.pc , SrcType.imm, SrcType.X, FuType.csr, CSROpType.wrs_sto, SelImm.X , noSpec = T, blockBack = T),

    // RV64A
    AMOADD_B  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoadd_b , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOXOR_B  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoxor_b , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOSWAP_B -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoswap_b, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOAND_B  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoand_b , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOOR_B   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoor_b  , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMIN_B  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomin_b , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMINU_B -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amominu_b, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMAX_B  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomax_b , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMAXU_B -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomaxu_b, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOCAS_B  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amocas_b, SelImm.X, uopSplitType = UopSplitType.AMO_CAS_BHWD, xWen = T, noSpec = T, blockBack = T),

    AMOADD_H  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoadd_h , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOXOR_H  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoxor_h , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOSWAP_H -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoswap_h, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOAND_H  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoand_h , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOOR_H   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoor_h  , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMIN_H  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomin_h , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMINU_H -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amominu_h, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMAX_H  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomax_h , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMAXU_H -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomaxu_h, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOCAS_H  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amocas_h, SelImm.X, uopSplitType = UopSplitType.AMO_CAS_BHWD, xWen = T, noSpec = T, blockBack = T),

    AMOADD_W  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoadd_w , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOXOR_W  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoxor_w , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOSWAP_W -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoswap_w, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOAND_W  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoand_w , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOOR_W   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoor_w  , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMIN_W  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomin_w , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMINU_W -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amominu_w, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMAX_W  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomax_w , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMAXU_W -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomaxu_w, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOCAS_W  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amocas_w, SelImm.X, uopSplitType = UopSplitType.AMO_CAS_BHWD, xWen = T, noSpec = T, blockBack = T),

    AMOADD_D  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoadd_d , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOXOR_D  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoxor_d , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOSWAP_D -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoswap_d, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOAND_D  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoand_d , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOOR_D   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amoor_d  , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMIN_D  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomin_d , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMINU_D -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amominu_d, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMAX_D  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomax_d , SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOMAXU_D -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amomaxu_d, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    AMOCAS_D  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amocas_d, SelImm.X, uopSplitType = UopSplitType.AMO_CAS_BHWD, xWen = T, noSpec = T, blockBack = T),

    AMOCAS_Q  -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.amocas_q, SelImm.X, uopSplitType = UopSplitType.AMO_CAS_Q, xWen = T, noSpec = T, blockBack = T),

    LR_W    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.mou, LSUOpType.lr_w, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    LR_D    -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.mou, LSUOpType.lr_d, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    SC_W    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.sc_w, SelImm.X, xWen = T, noSpec = T, blockBack = T),
    SC_D    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.mou, LSUOpType.sc_d, SelImm.X, xWen = T, noSpec = T, blockBack = T),
  )
}

object BitmanipDecode extends DecodeConstants{
  /*
    Note: Some Bitmanip instruction may have different OP code between rv32 and rv64.
    Including pseudo instruction like zext.h, and different funct12 like rev8.
    If some day we need to support change XLEN via CSR, we should care about this.
   */
  val decodeArray: Array[(BitPat, XSDecodeBase)] = Array(
    // Zba
    ADD_UW    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.adduw   , SelImm.X    , xWen = T, canRobCompress = T),
    SH1ADD    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sh1add  , SelImm.X    , xWen = T, canRobCompress = T),
    SH1ADD_UW -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sh1adduw, SelImm.X    , xWen = T, canRobCompress = T),
    SH2ADD    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sh2add  , SelImm.X    , xWen = T, canRobCompress = T),
    SH2ADD_UW -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sh2adduw, SelImm.X    , xWen = T, canRobCompress = T),
    SH3ADD    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sh3add  , SelImm.X    , xWen = T, canRobCompress = T),
    SH3ADD_UW -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.sh3adduw, SelImm.X    , xWen = T, canRobCompress = T),
    SLLI_UW   -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.slliuw  , SelImm.IMM_I, xWen = T, canRobCompress = T),

    // Zbb
    ANDN      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.andn    , SelImm.X    , xWen = T, canRobCompress = T),
    ORN       -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.orn     , SelImm.X    , xWen = T, canRobCompress = T),
    XNOR      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.xnor    , SelImm.X    , xWen = T, canRobCompress = T),

    CLZ       -> XSDecode(SrcType.reg, SrcType.DC , SrcType.X, FuType.bku, BKUOpType.clz     , SelImm.X    , xWen = T, canRobCompress = T),
    CLZW      -> XSDecode(SrcType.reg, SrcType.DC , SrcType.X, FuType.bku, BKUOpType.clzw    , SelImm.X    , xWen = T, canRobCompress = T),
    CTZ       -> XSDecode(SrcType.reg, SrcType.DC , SrcType.X, FuType.bku, BKUOpType.ctz     , SelImm.X    , xWen = T, canRobCompress = T),
    CTZW      -> XSDecode(SrcType.reg, SrcType.DC , SrcType.X, FuType.bku, BKUOpType.ctzw    , SelImm.X    , xWen = T, canRobCompress = T),

    CPOP      -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.cpop    , SelImm.X    , xWen = T, canRobCompress = T),
    CPOPW     -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.cpopw   , SelImm.X    , xWen = T, canRobCompress = T),

    MAX       -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.max     , SelImm.X    , xWen = T, canRobCompress = T),
    MAXU      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.maxu    , SelImm.X    , xWen = T, canRobCompress = T),
    MIN       -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.min     , SelImm.X    , xWen = T, canRobCompress = T),
    MINU      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.minu    , SelImm.X    , xWen = T, canRobCompress = T),

    SEXT_B    -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.alu, ALUOpType.sextb   , SelImm.X    , xWen = T, canRobCompress = T),
    SEXT_H    -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.alu, ALUOpType.sexth   , SelImm.X    , xWen = T, canRobCompress = T),
    // zext.h in rv64 is shared with packw in Zbkb with rs2 = $0.
    // If we configured to have no Zbkb, we should add zext.h here.

    ROL       -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.rol     , SelImm.X    , xWen = T, canRobCompress = T),
    ROLW      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.rolw    , SelImm.X    , xWen = T, canRobCompress = T),
    ROR       -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.ror     , SelImm.X    , xWen = T, canRobCompress = T),
    RORI      -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.ror     , SelImm.IMM_I, xWen = T, canRobCompress = T),
    RORIW     -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.rorw    , SelImm.IMM_I, xWen = T, canRobCompress = T),
    RORW      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.rorw    , SelImm.X    , xWen = T, canRobCompress = T),

    ORC_B     -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.alu, ALUOpType.orcb    , SelImm.X    , xWen = T, canRobCompress = T),

    REV8      -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.alu, ALUOpType.rev8    , SelImm.X    , xWen = T, canRobCompress = T),

    // Zbc
    CLMUL     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.clmul   , SelImm.X    , xWen = T, canRobCompress = T),
    CLMULH    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.clmulh  , SelImm.X    , xWen = T, canRobCompress = T),
    CLMULR    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.clmulr  , SelImm.X    , xWen = T, canRobCompress = T),

    // Zbs
    BCLR      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.bclr    , SelImm.X    , xWen = T, canRobCompress = T),
    BCLRI     -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.bclr    , SelImm.IMM_I, xWen = T, canRobCompress = T),
    BEXT      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.bext    , SelImm.X    , xWen = T, canRobCompress = T),
    BEXTI     -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.bext    , SelImm.IMM_I, xWen = T, canRobCompress = T),
    BINV      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.binv    , SelImm.X    , xWen = T, canRobCompress = T),
    BINVI     -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.binv    , SelImm.IMM_I, xWen = T, canRobCompress = T),
    BSET      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.bset    , SelImm.X    , xWen = T, canRobCompress = T),
    BSETI     -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.bset    , SelImm.IMM_I, xWen = T, canRobCompress = T),

    // Zbkb
    // rol, rolw, ror,rori, roriw, rorw, andn, orn, xnor, rev8 is in Zbb
    PACK      -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.pack    , SelImm.X    , xWen = T, canRobCompress = T),
    PACKH     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.packh   , SelImm.X    , xWen = T, canRobCompress = T),
    PACKW     -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.alu, ALUOpType.packw   , SelImm.X    , xWen = T, canRobCompress = T),
    BREV8     -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.alu, ALUOpType.revb    , SelImm.X    , xWen = T, canRobCompress = T),
    // If configured to RV32, we should add zip and unzip.

    // Zbkc
    // clmul, clmulh is in Zbc

    // Zbkx
    XPERM4    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.xpermn  , SelImm.X    , xWen = T, canRobCompress = T),
    XPERM8    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.xpermb  , SelImm.X    , xWen = T, canRobCompress = T),
  )
}

object ScalarCryptoDecode extends DecodeConstants {
  val decodeArray: Array[(BitPat, XSDecodeBase)] = Array(
    // Zknd: NIST Suite: AES Decryption
    AES64DS    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.aes64ds   , SelImm.X    , xWen = T, canRobCompress = T),
    AES64DSM   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.aes64dsm  , SelImm.X    , xWen = T, canRobCompress = T),
    AES64IM    -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.aes64im   , SelImm.X    , xWen = T, canRobCompress = T),
    AES64KS1I  -> XSDecode(SrcType.reg, SrcType.imm, SrcType.X, FuType.bku, BKUOpType.aes64ks1i , SelImm.IMM_I, xWen = T, canRobCompress = T),
    AES64KS2   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.aes64ks2  , SelImm.X    , xWen = T, canRobCompress = T),

    // Zkne: NIST Suite: AES Encryption
    // aes64ks1i, aes64ks2 is in Zknd
    AES64ES    -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.aes64es   , SelImm.X    , xWen = T, canRobCompress = T),
    AES64ESM   -> XSDecode(SrcType.reg, SrcType.reg, SrcType.X, FuType.bku, BKUOpType.aes64esm  , SelImm.X    , xWen = T, canRobCompress = T),

    // Zknh: NIST Suite: Hash Function Instructions
    SHA256SIG0 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha256sig0, SelImm.X    , xWen = T, canRobCompress = T),
    SHA256SIG1 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha256sig1, SelImm.X    , xWen = T, canRobCompress = T),
    SHA256SUM0 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha256sum0, SelImm.X    , xWen = T, canRobCompress = T),
    SHA256SUM1 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha256sum1, SelImm.X    , xWen = T, canRobCompress = T),
    SHA512SIG0 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha512sig0, SelImm.X    , xWen = T, canRobCompress = T),
    SHA512SIG1 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha512sig1, SelImm.X    , xWen = T, canRobCompress = T),
    SHA512SUM0 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha512sum0, SelImm.X    , xWen = T, canRobCompress = T),
    SHA512SUM1 -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sha512sum1, SelImm.X    , xWen = T, canRobCompress = T),

    // Zksh: ShangMi Suite: SM3 Hash Function Instructions
    SM3P0      -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sm3p0     , SelImm.X    , xWen = T, canRobCompress = T),
    SM3P1      -> XSDecode(SrcType.reg, SrcType.DC,  SrcType.X, FuType.bku, BKUOpType.sm3p1     , SelImm.X    , xWen = T, canRobCompress = T),
  )
}

/**
 * XiangShan Debug Decode constants
 */
object XSDebugDecode extends DecodeConstants {
  def TRAP = BitPat("b000000000000?????000000001101011")
  def SIM_TRIG = BitPat("b11011111101010010010000000010011") // HINT, slti x0, x18, -518
  val decodeArray: Array[(BitPat, XSDecodeBase)] = Array(
    TRAP    -> XSDecode(SrcType.imm, SrcType.imm, SrcType.X, FuType.alu, ALUOpType.xstrap, SelImm.IMM_I, blockBack = T)
  )
}

abstract class Imm(val len: Int, val typEncode: UInt) {
  def toImm32(minBits: UInt): UInt = do_toImm32(minBits(len - 1, 0))
  def extract(width: Int)(minBits: UInt): UInt = ???
  def do_toImm32(minBits: UInt): UInt
  def minBitsFromInstr(instr: UInt): UInt
}

case class Imm_I() extends Imm(12, SelImm.IMM_I) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits(len - 1, 0), 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31, 20))
}

case class Imm_S() extends Imm(12, SelImm.IMM_S) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31, 25), instr(11, 7))
}

case class Imm_B() extends Imm(12, SelImm.IMM_SB) {
  override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)

  override def minBitsFromInstr(instr: UInt): UInt =
    Cat(instr(31), instr(7), instr(30, 25), instr(11, 8))
}

case class Imm_U() extends Imm(20, SelImm.IMM_U){
  override def do_toImm32(minBits: UInt): UInt = Cat(minBits(len - 1, 0), 0.U(12.W))

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(31, 12)
  }
}

case class Imm_J() extends Imm(20, SelImm.IMM_UJ){
  override def do_toImm32(minBits: UInt): UInt = SignExt(Cat(minBits, 0.U(1.W)), 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(31), instr(19, 12), instr(20), instr(30, 25), instr(24, 21))
  }
}

case class Imm_Z() extends Imm(12 + 5 + 5, SelImm.IMM_Z){
  override def do_toImm32(minBits: UInt): UInt = minBits

  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(11, 7), instr(19, 15), instr(31, 20))
  }

  def getCSRAddr(imm: UInt): UInt = {
    require(imm.getWidth == this.len)
    imm(11, 0)
  }

  def getRS1(imm: UInt): UInt = {
    require(imm.getWidth == this.len)
    imm(16, 12)
  }

  def getRD(imm: UInt): UInt = {
    require(imm.getWidth == this.len)
    imm(21, 17)
  }

  def getImm5(imm: UInt): UInt = {
    require(imm.getWidth == this.len)
    imm(16, 12)
  }
}

case class Imm_OPIVIS() extends Imm(5, SelImm.IMM_OPIVIS){
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def extract(width: Int)(imm: UInt): UInt = SignExt(imm.take(5), width)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(19, 15)
  }
}

case class Imm_OPIVIU() extends Imm(5, SelImm.IMM_OPIVIU){
  override def do_toImm32(minBits: UInt): UInt = ZeroExt(minBits, 32)

  override def extract(width: Int)(imm: UInt): UInt = ZeroExt(imm.take(5), width)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(19, 15)
  }
}

case class Imm_VSETVLI() extends Imm(11, SelImm.IMM_VSETVLI){
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(30, 20)
  }
  /**
   * get VType from extended imm
   * @param extedImm
   * @return VType
   */
  def getVType(extedImm: UInt): InstVType = {
    val vtype = Wire(new InstVType)
    vtype := extedImm(10, 0).asTypeOf(new InstVType)
    vtype
  }

  def getVTypei(imm: UInt): UInt = {
    imm(10, 0)
  }
}

case class Imm_VSETIVLI() extends Imm(15, SelImm.IMM_VSETIVLI){
  override def do_toImm32(minBits: UInt): UInt = SignExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    val rvInst: XSInstBitFields = instr.asTypeOf(new XSInstBitFields)
    val uimm5 = rvInst.UIMM_VSETIVLI
    val vtype8 = rvInst.ZIMM_VSETIVLI
    Cat(uimm5, vtype8)
  }
  /**
   * get VType from extended imm
   * @param extedImm
   * @return VType
   */
  def getVType(extedImm: UInt): InstVType = {
    val vtype = Wire(new InstVType)
    vtype := extedImm(9, 0).asTypeOf(new InstVType)
    vtype
  }

  def getVTypei(imm: UInt): UInt = {
    imm(9, 0)
  }

  def getAvl(extedImm: UInt): UInt = {
    extedImm(14, 10)
  }
}

case class Imm_LUI32() extends Imm(32, SelImm.IMM_LUI32){
  override def do_toImm32(minBits: UInt): UInt = minBits(31, 0)

  override def minBitsFromInstr(instr: UInt): UInt = {
    instr(31, 0)
  }
}

case class Imm_VRORVI() extends Imm(6, SelImm.IMM_VRORVI){
  override def do_toImm32(minBits: UInt): UInt = ZeroExt(minBits, 32)

  override def minBitsFromInstr(instr: UInt): UInt = {
    Cat(instr(26), instr(19, 15))
  }
}

object ImmUnion {
  val I = Imm_I()
  val S = Imm_S()
  val B = Imm_B()
  val U = Imm_U()
  val J = Imm_J()
  val Z = Imm_Z()
  val OPIVIS = Imm_OPIVIS()
  val OPIVIU = Imm_OPIVIU()
  val VSETVLI = Imm_VSETVLI()
  val VSETIVLI = Imm_VSETIVLI()
  val LUI32 = Imm_LUI32()
  val VRORVI = Imm_VRORVI()

  // do not add special type lui32 to this, keep ImmUnion max len being 20.
  val imms = Seq(I, S, B, U, J, Z, OPIVIS, OPIVIU, VSETVLI, VSETIVLI, VRORVI)
  val maxLen = imms.maxBy(_.len).len
  val immSelMap = Seq(
    SelImm.IMM_I,
    SelImm.IMM_S,
    SelImm.IMM_SB,
    SelImm.IMM_U,
    SelImm.IMM_UJ,
    SelImm.IMM_Z,
    SelImm.IMM_OPIVIS,
    SelImm.IMM_OPIVIU,
    SelImm.IMM_VSETVLI,
    SelImm.IMM_VSETIVLI,
    SelImm.IMM_VRORVI,
  ).zip(imms)
  println(s"ImmUnion max len: $maxLen")
}

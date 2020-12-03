package xiangshan.backend.decode

import chisel3._
import chisel3.util._

import freechips.rocketchip.config.Parameters
import freechips.rocketchip.rocket.{RVCDecoder, ExpandedInstruction}
import freechips.rocketchip.rocket.{CSR,Causes}
import freechips.rocketchip.util.{uintToBitPat,UIntIsOneOf}

import xiangshan._
import utils._
import xiangshan.backend._
import xiangshan.backend.decode.AltInstructions._
import xiangshan.backend.fu.fpu.FPUOpType
import freechips.rocketchip.tile.RocketTile

/**
 * Abstract trait giving defaults and other relevant values to different Decode constants/
 */
abstract trait DecodeConstants {
  // TODO move these constants to somewhere else?
  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")

  def decodeDefault: List[BitPat] = 
    //   src1Type     src2Type     src3Type     fuType      fuOpType       rfWen
    //   |            |            |            |           |              |  fpWen
    //   |            |            |            |           |              |  |  isXSTrap
    //   |            |            |            |           |              |  |  |  noSpecExec
    //   |            |            |            |           |              |  |  |  |  blockBackward
    //   |            |            |            |           |              |  |  |  |  |  flushPipe
    //   |            |            |            |           |              |  |  |  |  |  |  isRVF
    //   |            |            |            |           |              |  |  |  |  |  |  |
    List(SrcType.reg, SrcType.reg, SrcType.reg, FuType.alu, ALUOpType.sll, N, N, N, N, N, N, N)
  
    val table: Array[(BitPat, List[BitPat])]
}

trait RISCVConstants
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

  // TODO move constants above to somewhere better and remove useless constants below

  val CSR_ADDR_MSB = 31
  val CSR_ADDR_LSB = 20
  val CSR_ADDR_SZ = 12

  // location of the fifth bit in the shamt (for checking for illegal ops for SRAIW,etc.)
  val SHAMT_5_BIT = 25
  val LONGEST_IMM_SZ = 20
  val X0 = 0.U
  val RA = 1.U // return address register

  // memory consistency model
  // The C/C++ atomics MCM requires that two loads to the same address maintain program order.
  // The Cortex A9 does NOT enforce load/load ordering (which leads to buggy behavior).
  val MCM_ORDER_DEPENDENT_LOADS = true

  val jal_opc = (0x6f).U
  val jalr_opc = (0x67).U

  def GetUop(inst: UInt): UInt = inst(6,0)
  def GetRd (inst: UInt): UInt = inst(RD_MSB,RD_LSB)
  def GetRs1(inst: UInt): UInt = inst(RS1_MSB,RS1_LSB)

  // Note: Accepts only EXPANDED rvc instructions
  def ComputeBranchTarget(pc: UInt, inst: UInt, xlen: Int)(implicit p: Parameters): UInt = {
    val b_imm32 = Cat(Fill(20,inst(31)), inst(7), inst(30,25), inst(11,8), 0.U(1.W))
    ((pc.asSInt + b_imm32.asSInt).asSInt & (-2).S).asUInt
  }

  // Note: Accepts only EXPANDED rvc instructions
  def ComputeJALTarget(pc: UInt, inst: UInt, xlen: Int)(implicit p: Parameters): UInt = {
    val j_imm32 = Cat(Fill(12,inst(31)), inst(19,12), inst(20), inst(30,25), inst(24,21), 0.U(1.W))
    ((pc.asSInt + j_imm32.asSInt).asSInt & (-2).S).asUInt
  }
}

/**
 * Decoded control signals
 * See xiangshan/package.scala, xiangshan/backend/package.scala, Bundle.scala
 */
// FIXME Check sig from isXSTrap to isRVF
/**
 * Decode constants for RV64
 */
object X64Decode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    LD      -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.ld, Y, N, N, N, N, N, N),
    LWU     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lwu, Y, N, N, N, N, N, N),
    SD      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.stu, LSUOpType.sd, N, N, N, N, N, N, N),

    SLLI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sll, Y, N, N, N, N, N, N),
    SRLI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.srl, Y, N, N, N, N, N, N),
    SRAI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sra, Y, N, N, N, N, N, N),

    ADDIW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.addw, Y, N, N, N, N, N, N),
    SLLIW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sllw, Y, N, N, N, N, N, N),
    SRAIW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sraw, Y, N, N, N, N, N, N),
    SRLIW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.srlw, Y, N, N, N, N, N, N),

    ADDW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.addw, Y, N, N, N, N, N, N),
    SUBW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.subw, Y, N, N, N, N, N, N),
    SLLW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sllw, Y, N, N, N, N, N, N),
    SRAW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sraw, Y, N, N, N, N, N, N),
    SRLW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.srlw, Y, N, N, N, N, N, N)
  )
}

/**
 * Overall Decode constants
 */
object XDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
    LW      -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lw, Y, N, N, N, N, N, N),
    LH      -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lh, Y, N, N, N, N, N, N),
    LHU     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lhu, Y, N, N, N, N, N, N),
    LB      -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lb, Y, N, N, N, N, N, N),
    LBU     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.lbu, Y, N, N, N, N, N, N),

    SW      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.stu, LSUOpType.sw, N, N, N, N, N, N, N),
    SH      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.stu, LSUOpType.sh, N, N, N, N, N, N, N),
    SB      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.stu, LSUOpType.sb, N, N, N, N, N, N, N),

    LUI     -> List(SrcType.pc, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.add, Y, N, N, N, N, N, N),

    ADDI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.add, Y, N, N, N, N, N, N),
    ANDI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.and, Y, N, N, N, N, N, N),
    ORI     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.or, Y, N, N, N, N, N, N),
    XORI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.xor, Y, N, N, N, N, N, N),
    SLTI    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.slt, Y, N, N, N, N, N, N),
    SLTIU   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sltu, Y, N, N, N, N, N, N),

    SLL     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sll, Y, N, N, N, N, N, N),
    ADD     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.add, Y, N, N, N, N, N, N),
    SUB     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sub, Y, N, N, N, N, N, N),
    SLT     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.slt, Y, N, N, N, N, N, N),
    SLTU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sltu, Y, N, N, N, N, N, N),
    AND     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.and, Y, N, N, N, N, N, N),
    OR      -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.or, Y, N, N, N, N, N, N),
    XOR     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.xor, Y, N, N, N, N, N, N),
    SRA     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.sra, Y, N, N, N, N, N, N),
    SRL     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.srl, Y, N, N, N, N, N, N),

    MUL     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mul, Y, N, N, N, N, N, N),
    MULH    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mulh, Y, N, N, N, N, N, N),
    MULHU   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mulhu, Y, N, N, N, N, N, N),
    MULHSU  -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mulhsu, Y, N, N, N, N, N, N),
    MULW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mul, MDUOpType.mulw, Y, N, N, N, N, N, N),

    DIV     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.div, Y, N, N, N, N, N, N),
    DIVU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.divu, Y, N, N, N, N, N, N),
    REM     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.rem, Y, N, N, N, N, N, N),
    REMU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.remu, Y, N, N, N, N, N, N),
    DIVW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.divw, Y, N, N, N, N, N, N),
    DIVUW   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.divuw, Y, N, N, N, N, N, N),
    REMW    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.remw, Y, N, N, N, N, N, N),
    REMUW   -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.div, MDUOpType.remuw, Y, N, N, N, N, N, N),

    AUIPC   -> List(SrcType.pc, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.add, Y, N, N, N, N, N, N),
    JAL     -> List(SrcType.pc , SrcType.imm, SrcType.DC, FuType.jmp, JumpOpType.jal, Y, N, N, N, N, N, N),
    JALR    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.jmp, JumpOpType.jalr, Y, N, N, N, N, N, N),
    BEQ     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.beq, N, N, N, N, N, N, N),
    BNE     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bne, N, N, N, N, N, N, N),
    BGE     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bge, N, N, N, N, N, N, N),
    BGEU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bgeu, N, N, N, N, N, N, N),
    BLT     -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.blt, N, N, N, N, N, N, N),
    BLTU    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.alu, ALUOpType.bltu, N, N, N, N, N, N, N),

    // I-type, the immediate12 holds the CSR register.
    CSRRW   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.wrt, N, N, N, N, N, N, N),
    CSRRS   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.set, N, N, N, N, N, N, N),
    CSRRC   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.clr, N, N, N, N, N, N, N),

    CSRRWI  -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.wrti, N, N, N, N, N, N, N),
    CSRRSI  -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.seti, N, N, N, N, N, N, N),
    CSRRCI  -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.clri, N, N, N, N, N, N, N),

    SFENCE_VMA->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.fence, FenceOpType.sfence, N, N, N, N, N, N, N),
    SCALL   -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, N, N, N, N, N, N, N), // same as ECALL
    SRET    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, N, N, N, N, N, N, N),
    MRET    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.csr, CSROpType.jmp, N, N, N, N, N, N, N),

    WFI     -> List(SrcType.pc, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.sll, N, N, N, N, N, N, N),

    FENCE_I -> List(SrcType.pc, SrcType.imm, SrcType.DC, FuType.fence, FenceOpType.fencei, N, N, N, N, N, N, N),
    FENCE   -> List(SrcType.pc, SrcType.imm, SrcType.DC, FuType.fence, FenceOpType.fence, N, N, N, N, N, N, N),

    // A-type
    AMOADD_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoadd_w, N, N, N, N, N, N, N),
    AMOXOR_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoxor_w, N, N, N, N, N, N, N),
    AMOSWAP_W->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoswap_w, N, N, N, N, N, N, N),
    AMOAND_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoand_w, N, N, N, N, N, N, N),
    AMOOR_W -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoor_w, N, N, N, N, N, N, N),
    AMOMIN_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomin_w, N, N, N, N, N, N, N),
    AMOMINU_W->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amominu_w, N, N, N, N, N, N, N),
    AMOMAX_W-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomax_w, N, N, N, N, N, N, N),
    AMOMAXU_W->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomaxu_w, N, N, N, N, N, N, N),

    AMOADD_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoadd_d, N, N, N, N, N, N, N),
    AMOXOR_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoxor_d, N, N, N, N, N, N, N),
    AMOSWAP_D->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoswap_d, N, N, N, N, N, N, N),
    AMOAND_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoand_d, N, N, N, N, N, N, N),
    AMOOR_D -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amoor_d, N, N, N, N, N, N, N),
    AMOMIN_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomin_d, N, N, N, N, N, N, N),
    AMOMINU_D->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amominu_d, N, N, N, N, N, N, N),
    AMOMAX_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomax_d, N, N, N, N, N, N, N),
    AMOMAXU_D->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.amomaxu_d, N, N, N, N, N, N, N),

    LR_W    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.mou, LSUOpType.lr_w, N, N, N, N, N, N, N),
    LR_D    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.mou, LSUOpType.lr_d, N, N, N, N, N, N, N),
    SC_W    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.sc_w, N, N, N, N, N, N, N),
    SC_D    -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.mou, LSUOpType.sc_d, N, N, N, N, N, N, N)
  )
}

/**
 * FP Decode constants
 */
object FDecode extends DecodeConstants{
  val table: Array[(BitPat, List[BitPat])] = Array(
    // FIXME check Src3type below
  FLW     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.flw, N, N, N, N, N, N, N),
  FLD     -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.ldu, LSUOpType.ld, N, N, N, N, N, N, N),
  FSW     -> List(SrcType.reg, SrcType.fp, SrcType.DC, FuType.stu, LSUOpType.sw, N, N, N, N, N, N, N), // sort of a lie; broken into two micro-ops
  FSD     -> List(SrcType.reg, SrcType.fp, SrcType.DC, FuType.stu, LSUOpType.sd, N, N, N, N, N, N, N),

  FCLASS_S-> List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fclass, N, N, N, N, N, N, N),
  FCLASS_D-> List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fclass, N, N, N, N, N, N, N),

  FMV_D_X -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.fmv_i2f, N, N, N, N, N, N, N),
  FMV_X_D -> List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fmv_f2i, N, N, N, N, N, N, N),
  FMV_X_W -> List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fmv_f2i, N, N, N, N, N, N, N),
  FMV_W_X -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.fmv_i2f, N, N, N, N, N, N, N),

  FSGNJ_S -> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fsgnj, N, N, N, N, N, N, N),
  FSGNJ_D -> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmisc, FPUOpType.fsgnj, N, N, N, N, N, N, N),
  FSGNJX_S-> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fsgnjx, N, N, N, N, N, N, N),
  FSGNJX_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmisc, FPUOpType.fsgnjx, N, N, N, N, N, N, N),
  FSGNJN_S-> List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fsgnjn, N, N, N, N, N, N, N),
  FSGNJN_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmisc, FPUOpType.fsgnjn, N, N, N, N, N, N, N),

  // FP to FP
  FCVT_S_D-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmisc, FPUOpType.d2s, N, N, N, N, N, N, N),
  FCVT_D_S-> List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmisc, FPUOpType.s2d, N, N, N, N, N, N, N),

  // Int to FP
  FCVT_S_W-> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.w2f, N, N, N, N, N, N, N),
  FCVT_S_WU->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.wu2f, N, N, N, N, N, N, N),
  FCVT_S_L-> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.l2f, N, N, N, N, N, N, N),
  FCVT_S_LU->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.lu2f, N, N, N, N, N, N, N),

  FCVT_D_W-> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.w2f, N, N, N, N, N, N, N),
  FCVT_D_WU->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.wu2f, N, N, N, N, N, N, N),
  FCVT_D_L-> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.l2f, N, N, N, N, N, N, N),
  FCVT_D_LU->List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.i2f, FPUOpType.lu2f, N, N, N, N, N, N, N),

  // FP to Int
  FCVT_W_S-> List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.f2w, N, N, N, N, N, N, N),
  FCVT_WU_S->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.f2wu, N, N, N, N, N, N, N),
  FCVT_L_S-> List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.f2l, N, N, N, N, N, N, N),
  FCVT_LU_S->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.f2lu, N, N, N, N, N, N, N),

  FCVT_W_D-> List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.f2w, N, N, N, N, N, N, N),
  FCVT_WU_D->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.f2wu, N, N, N, N, N, N, N),
  FCVT_L_D-> List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.f2l, N, N, N, N, N, N, N),
  FCVT_LU_D->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.f2lu, N, N, N, N, N, N, N),

  // "fp_single" is used for wb_data formatting (and debugging)
  FEQ_S    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.feq, N, N, N, N, N, N, N),
  FLT_S    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.flt, N, N, N, N, N, N, N),
  FLE_S    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fle, N, N, N, N, N, N, N),

  FEQ_D    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.feq, N, N, N, N, N, N, N),
  FLT_D    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.flt, N, N, N, N, N, N, N),
  FLE_D    ->List(SrcType.fp , SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fle, N, N, N, N, N, N, N),

  FMIN_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fmin, N, N, N, N, N, N, N),
  FMAX_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fmax, N, N, N, N, N, N, N),
  FMIN_D   ->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmisc, FPUOpType.fmin, N, N, N, N, N, N, N),
  FMAX_D   ->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmisc, FPUOpType.fmax, N, N, N, N, N, N, N),

  FADD_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, FPUOpType.fadd, N, N, N, N, N, N, N),
  FSUB_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, FPUOpType.fsub, N, N, N, N, N, N, N),
  FMUL_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, FPUOpType.fmul, N, N, N, N, N, N, N),
  FADD_D   ->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmac, FPUOpType.fadd, N, N, N, N, N, N, N),
  FSUB_D   ->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmac, FPUOpType.fsub, N, N, N, N, N, N, N),
  FMUL_D   ->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmac, FPUOpType.fmul, N, N, N, N, N, N, N),

  FMADD_S  ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, FPUOpType.fmadd, N, N, N, N, N, N, N),
  FMSUB_S  ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, FPUOpType.fmsub, N, N, N, N, N, N, N),
  FNMADD_S ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, FPUOpType.fnmadd, N, N, N, N, N, N, N),
  FNMSUB_S ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmac, FPUOpType.fnmsub, N, N, N, N, N, N, N),
  FMADD_D  ->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmac, FPUOpType.fmadd, N, N, N, N, N, N, N),
  FMSUB_D  ->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmac, FPUOpType.fmsub, N, N, N, N, N, N, N),
  FNMADD_D ->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmac, FPUOpType.fnmadd, N, N, N, N, N, N, N),
  FNMSUB_D ->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmac, FPUOpType.fnmsub, N, N, N, N, N, N, N)

  )
}

/**
 * FP Divide SquareRoot Constants
 */
object FDivSqrtDecode extends DecodeConstants {
  val table: Array[(BitPat, List[BitPat])] = Array(
  FDIV_S    ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fdiv, N, N, N, N, N, N, N),
  FDIV_D    ->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmisc, FPUOpType.fdiv, N, N, N, N, N, N, N),
  FSQRT_S   ->List(SrcType.fp,  SrcType.fp, SrcType.DC, FuType.fmisc, FPUOpType.fsqrt, N, N, N, N, N, N, N),
  FSQRT_D   ->List(SrcType.reg, SrcType.reg, SrcType.DC, FuType.fmisc, FPUOpType.fsqrt, N, N, N, N, N, N, N)

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
    TRAP    -> List(SrcType.reg, SrcType.imm, SrcType.DC, FuType.alu, ALUOpType.add, N, N, N, N, N, N, N)
  )
}

class RVCExpander extends XSModule {
  val io = IO(new Bundle {
    val in = Input(UInt(32.W))
    val out = Output(new ExpandedInstruction)
    val rvc = Output(Bool())
  })

  if (HasCExtension) {
    io.rvc := io.in(1,0) =/= 3.U
    io.out := new RVCDecoder(io.in, XLEN).decode
  } else {
    io.rvc := false.B
    io.out := new RVCDecoder(io.in, XLEN).passthrough
  }
}

/**
 * IO bundle for the Decode unit
 */
class DecodeUnitIO extends XSBundle {
  val enq = new Bundle { val ctrl_flow = Input(new CtrlFlow) }
  val deq = new Bundle { val cf_ctrl = Output(new CfCtrl) }
}

/**
 * Decode unit that takes in a single CtrlFlow and generates a CfCtrl.
 */
class DecodeUnit extends XSModule with RISCVConstants {
  val io = IO(new DecodeUnitIO)

  val ctrl_flow = Wire(new CtrlFlow) // input with RVC Expanded
  val cf_ctrl = Wire(new CfCtrl)

  // FIXME add expander 
  val exp = Module(new RVCExpander()) // FIXME Is empty really worked here?
  exp.io.in := io.enq.ctrl_flow.instr
  ctrl_flow := io.enq.ctrl_flow
  when (exp.io.rvc) {
    ctrl_flow.instr := exp.io.out.bits
  }

  // save rvc decode info
  val rvc_info = Wire(new ExpandedInstruction())
  val is_rvc = Wire(Bool())
  rvc_info := exp.io.out
  is_rvc := exp.io.rvc

  var decode_table = XDecode.table ++ FDecode.table ++ FDivSqrtDecode.table ++ X64Decode.table ++ XSTrapDecode.table

  // output
  cf_ctrl.cf := ctrl_flow
  cf_ctrl.brTag := DontCare
  val cs = Wire(new CtrlSignals()).decode(ctrl_flow.instr, decode_table)

  when (is_rvc) {
    cs.lsrc1 := rvc_info.rs1
    cs.lsrc2 := rvc_info.rs2
    cs.lsrc3 := rvc_info.rs3

    cs.ldest := rvc_info.rd

  } .otherwise {
    cs.lsrc1 := ctrl_flow.instr(RS1_MSB,RS1_LSB)
    cs.lsrc2 := ctrl_flow.instr(RS2_MSB,RS2_LSB)
    cs.lsrc3 := ctrl_flow.instr(RS3_MSB,RS3_LSB)

    cs.ldest := ctrl_flow.instr(RD_MSB,RD_LSB)
  }

  // TODO fill exp, intr Vec in CtrlFlow
  io.deq.cf_ctrl.cf.exceptionVec := io.enq.ctrl_flow.exceptionVec
  io.deq.cf_ctrl.cf.intrVec := io.enq.ctrl_flow.intrVec
  
  // TODO fill imm
  cs.imm := DontCare

  cf_ctrl.ctrl := cs
  io.deq.cf_ctrl := cf_ctrl

  //-------------------------------------------------------------
  // Debug Info
  XSDebug("in:  instr=%x pc=%x excepVec=%b intrVec=%b crossPageIPFFix=%d\n",
    io.enq.ctrl_flow.instr, io.enq.ctrl_flow.pc, io.enq.ctrl_flow.exceptionVec.asUInt,
    io.enq.ctrl_flow.intrVec.asUInt, io.enq.ctrl_flow.crossPageIPFFix)
  XSDebug("out: src1Type=%b src2Type=%b src3Type=%b lsrc1=%d lsrc2=%d lsrc3=%d ldest=%d fuType=%b fuOpType=%b\n",
    io.deq.cf_ctrl.ctrl.src1Type, io.deq.cf_ctrl.ctrl.src2Type, io.deq.cf_ctrl.ctrl.src3Type,
    io.deq.cf_ctrl.ctrl.lsrc1, io.deq.cf_ctrl.ctrl.lsrc2, io.deq.cf_ctrl.ctrl.lsrc3,
    io.deq.cf_ctrl.ctrl.ldest, io.deq.cf_ctrl.ctrl.fuType, io.deq.cf_ctrl.ctrl.fuOpType)
  XSDebug("out: rfWen=%d fpWen=%d isXSTrap=%d noSpecExec=%d isBlocked=%d flushPipe=%d isRVF=%d imm=%x\n",
    io.deq.cf_ctrl.ctrl.rfWen, io.deq.cf_ctrl.ctrl.fpWen, io.deq.cf_ctrl.ctrl.isXSTrap,
    io.deq.cf_ctrl.ctrl.noSpecExec, io.deq.cf_ctrl.ctrl.blockBackward, io.deq.cf_ctrl.ctrl.flushPipe,
    io.deq.cf_ctrl.ctrl.isRVF, io.deq.cf_ctrl.ctrl.imm)
}

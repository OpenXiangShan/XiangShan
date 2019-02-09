package core

import chisel3._
import chisel3.util._

trait HasInstrType {
  private val InstrTypeNum = 7
  def InstrN = "b000".U
  def InstrI = "b100".U
  def InstrR = "b101".U
  def InstrS = "b010".U
  def InstrB = "b001".U
  def InstrU = "b110".U
  def InstrJ = "b111".U
  val InstrTypeWidth = log2Up(InstrTypeNum).W

  def isrfWen(instrType : UInt): Bool = instrType(2)
}

trait HasSrcType {
  /* src1 type */
  private val Src1TypeNum = 2
  def Src1Reg = "b0".U
  def Src1Pc  = "b1".U
  val Src1TypeWidth = log2Up(Src1TypeNum).W

  /* src2 type */
  private val Src2TypeNum = 2
  def Src2Imm = "b0".U
  def Src2Reg = "b1".U
  val Src2TypeWidth = log2Up(Src2TypeNum).W
}

trait HasFuType
 extends HasALUOpType
    with HasBRUOpType
    with HasLSUOpType
    with HasMDUOpType
    with HasCSROpType {
  private val FuTypeNum = 5
  def FuAlu = "b000".U
  def FuBru = "b001".U
  def FuLsu = "b010".U
  def FuMdu = "b011".U
  def FuCsr = "b100".U
  val FuTypeWidth = log2Up(FuTypeNum).W

  private val FuOpTypeMaxNum = List(AluOpTypeNum, BruOpTypeNum,
    LsuOpTypeNum, MduOpTypeNum, CsrOpTypeNum).reduce(math.max)
  val FuOpTypeWidth = log2Up(FuOpTypeMaxNum).W
}

trait HasDecodeConst extends HasInstrType with HasSrcType with HasFuType

object Instructions extends HasDecodeConst {
  val DecodeDefault = List(InstrN, FuAlu, AluAdd)
  val DecodeTable = ALUInstr.table ++ BRUInstr.table ++ LSUInstr.table ++
                    MDUInstr.table ++ CSRInstr.table ++ NOOPTrap.table
}

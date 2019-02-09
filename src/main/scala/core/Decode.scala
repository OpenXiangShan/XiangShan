package core

import chisel3._
import chisel3.util._

trait HasInstrType {
  private val InstrTypeNum = 7
  val InstrN = "b000".U
  val InstrI = "b100".U
  val InstrR = "b101".U
  val InstrS = "b010".U
  val InstrB = "b001".U
  val InstrU = "b110".U
  val InstrJ = "b111".U
  val InstrTypeWidth = log2Up(InstrTypeNum).W

  def isrfWen(instrType : UInt): Bool = instrType(2)
}

trait HasSrcType {
  /* src1 type */
  private val Src1TypeNum = 2
  val Src1Reg = "b0".U
  val Src1Pc  = "b1".U
  val Src1TypeWidth = log2Up(Src1TypeNum).W

  /* src2 type */
  private val Src2TypeNum = 2
  val Src2Imm = "b0".U
  val Src2Reg = "b1".U
  val Src2TypeWidth = log2Up(Src2TypeNum).W
}

trait HasFuType
 extends HasALUOpType
    with HasBRUOpType
    with HasLSUOpType
    with HasMDUOpType
    with HasCSROpType {
  private val FuTypeNum = 5
  val FuAlu = "b000".U
  val FuBru = "b001".U
  val FuLsu = "b010".U
  val FuMdu = "b011".U
  val FuCsr = "b100".U
  val FuTypeWidth = log2Up(FuTypeNum).W

  private val FuOpTypeMaxNum = List(AluOpTypeNum, BruOpTypeNum,
    LsuOpTypeNum, MduOpTypeNum, CsrOpTypeNum).reduce(math.max)
  val FuOpTypeWidth = log2Up(FuOpTypeMaxNum).W
}

trait HasDecodeConst extends HasInstrType with HasSrcType with HasFuType

object Instructions
 extends ALUInstr
    with BRUInstr
    with LSUInstr
    with MDUInstr
    with CSRInstr {
  val TRAP    = BitPat("b????????????_?????_000_?????_1101011")
  val TRAPDecode = (TRAP -> List(InstrI, FuAlu, AluAdd))

  val DecodeDefault = List(InstrN, FuAlu, AluAdd)
  val DecodeTable = ALUInstrTable ++ BRUInstrTable ++ LSUInstrTable ++
                    MDUInstrTable ++ CSRInstrTable :+ TRAPDecode
}

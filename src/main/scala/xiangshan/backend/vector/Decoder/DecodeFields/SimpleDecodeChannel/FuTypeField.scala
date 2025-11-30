package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.UInt
import chisel3.util.BitPat
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.fu.FuType
import xiangshan.backend.vector.Decoder.InstPattern.InstPattern
import xiangshan.backend.vector.Decoder.Uop.ScalaUopTable
import xiangshan.backend.vector.Decoder.util.DecodeField

import scala.language.implicitConversions


class FuTypeField(table: Map[BitPat, Opcode]) extends DecodeField[InstPattern, UInt]{
  override def name: String = "fuType"

  override def chiselType: UInt = FuType()

  override def genTable(op: InstPattern): BitPat = {
    val res: UInt =
      try
        table(op.bitPat) match {
          case Opcode.AluOpcode(uint@_*) => FuType.alu.U
          case Opcode.BruOpcode(uint@_*) => FuType.brh.U
          case Opcode.JmpOpcode(uint@_*) => FuType.jmp.U
          case Opcode.MulOpcode(uint@_*) => FuType.mul.U
          case Opcode.DivOpcode(uint@_*) => FuType.div.U
          case Opcode.LduOpcode(uint@_*) => FuType.ldu.U
          case Opcode.StuOpcode(uint@_*) => FuType.stu.U
          case Opcode.AmoOpcode(uint@_*) => FuType.mou.U
          case Opcode.BkuOpcode(uint@_*) => FuType.bku.U
          case Opcode.CsrOpcode(uint@_*) => FuType.csr.U
          case Opcode.FenceOpcode(uint@_*) => FuType.fence.U
          case Opcode.FmacOpcode(uint@_*) => FuType.fmac.U
          case Opcode.FdivOpcode(uint@_*) => FuType.fDivSqrt.U
          case Opcode.FmiscOpcode(uint@_*) => FuType.fcmp.U
          case Opcode.FcvtOpcode(uint@_*) => FuType.fcvt.U
        }
      catch {
        case e: NoSuchElementException =>
          println(s"inst ${op.name} is not in uop table")
          throw e
        case e: Throwable => throw e
      }

    res
  }

  implicit def UIntCastToBitPat(uint: UInt): BitPat = BitPat(uint)
}

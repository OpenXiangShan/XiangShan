package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.UInt
import chisel3.util.BitPat
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.decode.opcode.Opcode.Opcode
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
        table(op.bitPat).factory match {
          case _: Opcode.AluOpcodes.type => FuType.alu.U
          case _: Opcode.BruOpcodes.type => FuType.brh.U
          case _: Opcode.JmpOpcodes.type => FuType.jmp.U
          case _: Opcode.MulOpcodes.type => FuType.mul.U
          case _: Opcode.DivOpcodes.type => FuType.div.U
          case _: Opcode.LduOpcodes.type => FuType.ldu.U
          case _: Opcode.StuOpcodes.type => FuType.stu.U
          case _: Opcode.AmoOpcodes.type => FuType.mou.U
          case _: Opcode.BkuOpcodes.type => FuType.bku.U
          case _: Opcode.CsrOpcodes.type => FuType.csr.U
          case _: Opcode.FenceOpcodes.type => FuType.fence.U
          case _: Opcode.FMacOpcodes.type => FuType.fmac.U
          case _: Opcode.FDivOpcodes.type => FuType.fDivSqrt.U
          case _: Opcode.FCvtOpcodes.type => FuType.fcvt.U
          case _: Opcode.FMiscOpcodes.type => FuType.fcmp.U
        }
      catch {
        case e: NoSuchElementException =>
          println(s"inst ${op.name} is not in uop table")
          throw e
        case e: Throwable => throw e
      }

    res.pad(FuType.width)
  }

  implicit def UIntCastToBitPat(uint: UInt): BitPat = BitPat(uint)
}

package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.UInt
import chisel3.util.BitPat
import xiangshan.FuOpType
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.vector.Decoder.InstPattern.InstPattern
import xiangshan.backend.vector.Decoder.Uop.ScalaUopTable
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt.{BitPatToExt, UIntToUIntField}

class OpcodeField(table: Map[BitPat, Opcode]) extends DecodeField[InstPattern, UInt] {

  override def name: String = "opcode"

  override def chiselType: UInt = Opcode()

  override def genTable(op: InstPattern): BitPat = {
    val res = {
      try
        table(op.bitPat).encode.toBitPat.pad0To(FuOpType.width)
      catch {
        case e: NoSuchElementException =>
          println(s"inst ${op.name} is not in uop table")
          throw e
        case e: Throwable => throw e
      }
    }

    res
  }
}

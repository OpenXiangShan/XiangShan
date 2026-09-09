package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3._
import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.Types.NumUop
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField

object NumUopField extends DecodeField[InstPattern, UInt] {
  override def name: String = "numUop"

  override def chiselType: UInt = NumUop()

  override def genTable(op: InstPattern): BitPat = {
    val num = UopInfoFieldSimple.genUopSeq(op).length
    0.max(num - 1).U(NumUop.width.W).toBitPat
  }
}

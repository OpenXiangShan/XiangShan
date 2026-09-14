package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern.{InstPattern, VecInstPattern}
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

object IsVField extends BoolDecodeField[InstPattern] {
  override def name: String = "isV"

  override def genTable(op: InstPattern): BitPat = {
    op match {
      case _: VecInstPattern => y
      case _ => n
    }
  }
}

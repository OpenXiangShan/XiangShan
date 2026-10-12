package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern.{InstPattern, MopInstPattern}
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

object IsMopField extends BoolDecodeField[InstPattern] {
  override def name: String = "isMop"

  override def genTable(op: InstPattern): BitPat = op match {
    case _: MopInstPattern => y
    case _                 => n
  }
}

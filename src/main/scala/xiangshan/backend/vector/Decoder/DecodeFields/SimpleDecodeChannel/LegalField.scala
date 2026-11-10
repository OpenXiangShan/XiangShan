package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern.InstPattern
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

object LegalField extends BoolDecodeField[InstPattern] {

  override def default: BitPat = n

  override def name: String = "legal"

  override def genTable(op: InstPattern): BitPat = y
}

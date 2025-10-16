package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import chisel3.util.experimental.decode.BoolDecodeField
import xiangshan.backend.vector.Decoder.InstPattern.InstPattern

object LegalField extends BoolDecodeField[InstPattern] {

  override def default: BitPat = n

  override def name: String = "legal"

  override def genTable(op: InstPattern): BitPat = y
}

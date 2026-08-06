package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

object VpWenField extends BoolDecodeField[VecInstPattern] {
  override def name: String = "vpWen"

  override def genTable(op: VecInstPattern): BitPat = {
    op match {
      case pattern: VecLoadInstPattern =>
        y
      case pattern: VecArithInstPattern if !(GpWenField.usedNames ++ FpWenField.usedNames).contains(pattern.name) =>
        y
      case pattern: VecArithInstPattern =>
        n
      case pattern: VecStoreInstPattern =>
        n
      case _: VecConfigInstPattern =>
        n
    }
  }
}

package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.util.BoolDecodeField
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt

object DirtyVsField extends BoolDecodeField[VecInstPattern] {
  override def name: String = "dirtyVs"

  override def genTable(op: VecInstPattern): BitPat = {
    op match {
      case _: VecLoadInstPattern | _: VecStoreInstPattern | _: VecConfigInstPattern =>
        y
      case p: VecArithInstPattern =>
        (!(GpWenField.usedNames ++ FpWenField.usedNames).contains(p.name)).toBitPat
    }
  }
}

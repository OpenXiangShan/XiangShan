package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

object IsSrcdstOverlapStrictField extends BoolDecodeField[VecInstPattern] {
  override def name: String = "isSrcdstOverlapStrict"

  override def genTable(op: VecInstPattern): BitPat = {
    op match {
      case _: VecConfigInstPattern | _: ScaMultUopInstPattern => dc
      case _: OverlapStrict => y
      case _ => n
    }
  }
}

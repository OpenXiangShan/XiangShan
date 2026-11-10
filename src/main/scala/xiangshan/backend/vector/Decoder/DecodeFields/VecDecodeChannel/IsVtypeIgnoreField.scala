package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

object IsVtypeIgnoreField extends BoolDecodeField[VecInstPattern] {
  override def name: String = "isVtypeIgnore"

  override def genTable(op: VecInstPattern): BitPat = {
    op match {
      case _: VecMemWhole => y
      case _: VecConfigInstPattern | _: ScaMultUopInstPattern => dc
      case _ => n
    }
  }
}

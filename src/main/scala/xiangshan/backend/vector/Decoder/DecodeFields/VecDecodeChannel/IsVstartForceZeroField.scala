package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

object IsVstartForceZeroField extends BoolDecodeField[VecInstPattern] {
  override def name: String = "isVstartForceZero"

  override def genTable(op: VecInstPattern): BitPat = {
    op match {
      case _: VecArithInstPattern => y
      case _: VecConfigInstPattern | _: ScaMultUopInstPattern => dc
      case _ => n
    }
  }
}

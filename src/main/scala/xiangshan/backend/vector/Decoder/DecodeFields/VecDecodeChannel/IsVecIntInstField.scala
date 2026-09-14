package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

object IsVecIntInstField extends BoolDecodeField[VecInstPattern] {
  override def name: String = "isVecIntInst"

  override def genTable(op: VecInstPattern): BitPat = {
    op match {
      case _: ScaMultUopInstPattern => y
      case _ => n
    }
  }
}

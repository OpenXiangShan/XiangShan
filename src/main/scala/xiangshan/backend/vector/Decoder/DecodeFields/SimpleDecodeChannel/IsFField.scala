package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern.{FpInstPattern, InstPattern, IntInstPattern, VecArithInstPattern, VecConfigInstPattern, VecInstPattern, VecMemInstPattern}
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

object IsFField extends BoolDecodeField[InstPattern] {
  override def name: String = "isF"

  override def genTable(op: InstPattern): BitPat = {
    op match {
      case _: IntInstPattern => n
      case _: FpInstPattern => y
      case vi: VecInstPattern => ???
    }
  }
}
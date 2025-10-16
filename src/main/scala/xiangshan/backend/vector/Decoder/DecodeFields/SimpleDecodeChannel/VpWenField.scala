package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import chisel3.util.experimental.decode.BoolDecodeField
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt

object VpWenField extends BoolDecodeField[InstPattern] {
  override def name: String = "vpWen"

  override def genTable(op: InstPattern): BitPat = {
    op match {
      case _: IntInstPattern => n
      case _: FpInstPattern => n
      case vec: VecInstPattern => genTable(vec)
    }
  }

  def genTable(op: VecInstPattern): BitPat = {
    op match {
      case _: VecLoadInstPattern => y
      case p: VecArithInstPattern =>
        (!(GpWenField.vecInsts ++ FpWenField.vecInsts).contains(p.name)).toBitPat
      case _: VecStoreInstPattern => n
      case _: VecConfigInstPattern => n
    }
  }
}

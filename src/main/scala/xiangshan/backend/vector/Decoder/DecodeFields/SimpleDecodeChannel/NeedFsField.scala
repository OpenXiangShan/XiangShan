package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

/** Require enabled scalar FP state for every FpInstPattern, including FP memory ops. */
object NeedFsField extends BoolDecodeField[InstPattern] {
  override def name: String = "needFs"

  override def genTable(inst: InstPattern): BitPat = {
    inst match {
      case _: FpInstPattern => y
      case _ => n
    }
  }
}

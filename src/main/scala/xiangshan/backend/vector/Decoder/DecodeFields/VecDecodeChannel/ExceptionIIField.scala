package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

/**
 * This field is used to specify if an instruction is illegal
 */
object ExceptionIIField extends BoolDecodeField[VecInstPattern] {

  override def name: String = "exceptionCause"

  override def genTable(op: VecInstPattern): BitPat = {
    op match {
      case _: AmocasQIllInstPattern => y
      case _ => n
    }
  }
}

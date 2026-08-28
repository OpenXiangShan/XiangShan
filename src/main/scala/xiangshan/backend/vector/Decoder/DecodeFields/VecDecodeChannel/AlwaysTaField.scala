package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

/**
 * This field is used to specify if an instruction always uses tail-agnostic policy.
 */
object AlwaysTaField extends BoolDecodeField[VecInstPattern] {

  override def name: String = "alwaysTa"

  override def genTable(op: VecInstPattern): BitPat = {
    op match {
      case vai: VecArithInstPattern => vai match {
        case VecIntVVMPattern() => y
        case VecIntMMMPattern() => y
        case VecCarryMPattern() => y
        case VecS2MDMPattern() => y
        case _ => n
      }
      case VecConfigInstPattern() => n
      case vmi: VecMemInstPattern => vmi match {
        case VecLoadMask() => y
        case _ => n
      }
      case _: ScaMultUopInstPattern => dc
    }
  }
}

package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import chisel3.util.experimental.decode.BoolDecodeField
import xiangshan.backend.vector.Decoder.InstPattern._

object NoSpecField extends BoolDecodeField[InstPattern] {

  override def name: String = "noSpec"

  override def genTable(op: InstPattern): BitPat = {
    op match {
      case int: IntInstPattern => int match {
        case intR: IntRTypePattern => intR match {
          case AmoInstPattern() => y
          case _ => n
        }
        case intI: IntITypePattern => intI match {
          case SystemInstPattern() => y
          case CSRInstPattern() => y
          case AmoLrInstPattern() => y
          case FenceInstPattern() => y
          case FenceiInstPattern() => y
          case _ => n
        }
        case _ => n
      }
      case _: FpInstPattern => n
      case _: VecInstPattern => n
    }
  }
}

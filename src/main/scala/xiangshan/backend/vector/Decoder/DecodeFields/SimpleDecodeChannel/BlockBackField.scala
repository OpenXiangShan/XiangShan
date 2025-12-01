package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

object BlockBackField extends BoolDecodeField[InstPattern] {

  override def name: String = "blockBack"

  override def genTable(inst: InstPattern): BitPat = {
    inst match {
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
          case CustomTrapPattern() => y
          case _ => n
        }
        case _ => n
      }
      case _: FpInstPattern => n
      case _: VecInstPattern => n
    }
  }
}
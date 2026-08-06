package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions.VFMV_F_S
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.util.BoolDecodeField
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt
import xiangshan.macros.InstanceNameMacro.getVariableName

object FpWenField extends BoolDecodeField[InstPattern] {
  override def name: String = "fpWen"

  override def genTable(inst: InstPattern): BitPat = {
    inst match {
      case _: IntInstPattern => n
      case fp: FpInstPattern =>
        fp match {
          case fpI: FpITypeInstPattern => fpI match {
            case FpITypeF2fInstPattern() => y
            case FpITypeF2iInstPattern() => n
            case _: FpITypeI2fInstPattern => y
            case FpITypeLoadInstPattern() => y
          }
          case _: FpRTypeInstPattern => y
          case FpR4TypeInstPattern() => y
          case FpSTypeInstPattern() => n
        }
      case vec: VecInstPattern => vecInsts.contains(vec.name).toBitPat
    }
  }

  private[DecodeFields] def vecInsts: Seq[String] = Seq(getVariableName(VFMV_F_S))
}

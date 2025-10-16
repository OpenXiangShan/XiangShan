package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import chisel3.util.experimental.decode.BoolDecodeField
import freechips.rocketchip.rocket.Instructions.{VCPOP_M, VFIRST_M, VMV_X_S}
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

object GpWenField extends BoolDecodeField[InstPattern] {
  override def name: String = "gpWen"

  override def genTable(inst: InstPattern): BitPat = {
    inst match {
      case _: IntRTypePattern => y
      case _: IntITypePattern => y
      case _: IntSTypePattern => n
      case IntBTypePattern() => n
      case IntUTypePattern() => y
      case IntJTypePattern() => y
      case fp: FpInstPattern =>
        fp match {
          case fpIType :FpITypeInstPattern =>
            fpIType match {
              case FpITypeF2iInstPattern() => y
              case _ => n
            }
          case _: FpRTypeInstPattern => n
          case FpR4TypeInstPattern() => n
          case FpSTypeInstPattern() => n
        }
      case p: VecInstPattern => vecInsts.contains(p.name).toBitPat
    }
  }

  private[DecodeFields] def vecInsts: Seq[String] = getVariableNameSeq(VCPOP_M, VFIRST_M, VMV_X_S)
}

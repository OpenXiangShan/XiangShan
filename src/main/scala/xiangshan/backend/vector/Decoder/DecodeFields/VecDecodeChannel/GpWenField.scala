package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import chisel3.util.experimental.decode.BoolDecodeField
import freechips.rocketchip.rocket.Instructions.{VCPOP_M, VFIRST_M, VMV_X_S}
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

object GpWenField extends BoolDecodeField[VecInstPattern] {
  override def name: String = "gpWen"

  override def genTable(op: VecInstPattern): BitPat = {
    usedNames.contains(op.name).toBitPat
  }

  val usedNames: Seq[String] = getVariableNameSeq(VCPOP_M, VFIRST_M, VMV_X_S)
}

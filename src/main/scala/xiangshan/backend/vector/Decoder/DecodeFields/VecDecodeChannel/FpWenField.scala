package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions.VFMV_F_S
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern
import xiangshan.backend.vector.Decoder.util.BoolDecodeField
import xiangshan.macros.InstanceNameMacro.getVariableName

object FpWenField extends BoolDecodeField[VecInstPattern] {
  override def name: String = "fpWen"

  override def genTable(op: VecInstPattern): BitPat = {
    if (usedNames.contains(op.name)) {
      y
    } else {
      n
    }}

  val usedNames: Seq[String] = Seq(getVariableName(VFMV_F_S))
}

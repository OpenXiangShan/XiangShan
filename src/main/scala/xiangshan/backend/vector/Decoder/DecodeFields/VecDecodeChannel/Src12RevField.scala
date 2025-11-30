package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions.{VFRDIV_VF, VFRSUB_VF, VRSUB_VI, VRSUB_VX}
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

object Src12RevField extends BoolDecodeField[VecInstPattern] {
  override def name: String = "src12Rev"

  override def genTable(op: VecInstPattern): BitPat = {
    if (revInsts.contains(op.name)) {
      y
    } else {
      n
    }}

  val revInsts = getVariableNameSeq(
    VRSUB_VX,
    VRSUB_VI,
    VFRDIV_VF,
    VFRSUB_VF,
  )
}

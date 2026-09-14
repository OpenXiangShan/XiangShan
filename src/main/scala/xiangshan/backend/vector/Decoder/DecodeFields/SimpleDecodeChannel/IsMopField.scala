package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3._
import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions.{MOP_R_N, MOP_RR_N}
import xiangshan.backend.vector.Decoder.InstPattern.InstPattern
import xiangshan.backend.vector.Decoder.util.BoolDecodeField
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

object IsMopField extends BoolDecodeField[InstPattern] {
  override def name: String = "isMop"

  override def genTable(op: InstPattern): BitPat = {
    op match {
      case inst if insts.contains(inst.name) => y
      case _                                                => n
    }
  }

  private val insts: Set[String] = getVariableNameSeq(
    MOP_R_N,
    MOP_RR_N,
  ).toSet
}

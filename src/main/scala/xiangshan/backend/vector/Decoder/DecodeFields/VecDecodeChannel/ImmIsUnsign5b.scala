package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern.Category
import xiangshan.backend.vector.Decoder.InstPattern.{VecArithInstPattern, VecConfigInstPattern, VecInstPattern, VecMemInstPattern}
import xiangshan.backend.vector.Decoder.util.BoolDecodeField
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt
import xiangshan.macros.InstanceNameMacro.{getVariableName, getVariableNameSeq}

object ImmIsUnsign5b extends BoolDecodeField[VecInstPattern] {
  override def name: String = "immIsUnsign5b"

  override def genTable(op: VecInstPattern): BitPat = {
    op match {
      case vai: VecArithInstPattern => (
        vai.category.rawString == Category.OPIVI.str &&
        uimmInst.contains(vai.name)
      ).toBitPat
      case VecConfigInstPattern() => n
      case _: VecMemInstPattern => n
    }
  }

  val uimmInst: Set[String] = getVariableNameSeq(
    VMSGTU_VI,
    VMSLEU_VI,
    VSADDU_VI,
    VNCLIP_WI,
  ).toSet
}

package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern.Category
import xiangshan.backend.vector.Decoder.InstPattern.{VecArithInstPattern, VecConfigInstPattern, VecInstPattern, ScaMultUopInstPattern, VecMemInstPattern}
import xiangshan.backend.vector.Decoder.util.BoolDecodeField
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt
import xiangshan.macros.InstanceNameMacro.{getVariableName, getVariableNameSeq}

object ImmIsSign5b extends BoolDecodeField[VecInstPattern] {
  override def name: String = "immIsSign5b"

  override def genTable(op: VecInstPattern): BitPat = {
    op match {
      case vai: VecArithInstPattern => (
        vai.category.rawString == Category.OPIVI.str &&
        !ImmIsUnsign5b.uimmInst.contains(vai.name)
      ).toBitPat
      case VecConfigInstPattern() => n
      case _: VecMemInstPattern => n
      case _: ScaMultUopInstPattern => n
    }
  }
}

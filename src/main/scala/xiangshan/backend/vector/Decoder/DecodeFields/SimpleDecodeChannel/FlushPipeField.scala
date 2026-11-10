package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.decode.isa.PseudoInstructions.PAUSE
import xiangshan.backend.vector.Decoder.InstPattern.InstPattern
import xiangshan.backend.vector.Decoder.util.BoolDecodeField
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

object FlushPipeField extends BoolDecodeField[InstPattern] {
  import xiangshan.backend.decode.isa.Instructions._

  override def name: String = "flushPipe"

  override def genTable(op: InstPattern): BitPat = {
    insts.contains(op.name).toBitPat
  }

  val insts = getVariableNameSeq(
    SFENCE_VMA,
    FENCE_I,
    FENCE,
    PAUSE,
    SFENCE_INVAL_IR,
    HFENCE_GVMA,
    HFENCE_VVMA,
  )
}

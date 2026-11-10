package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.UInt
import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions.ZICBOType._
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.decode.opcode.Opcode.StuOpcodes._
import xiangshan.backend.vector.Decoder.InstPattern.InstPattern
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{BoolPattern, DecodePatternComb2}
import xiangshan.backend.vector.Decoder.Uop.ScalaUopTable
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt.BitPatToExt
import xiangshan.macros.InstanceNameMacro.getVariableName


object CboOpcodeField extends DecodeField[DecodePatternComb2[InstPattern, BoolPattern], UInt] {

  override def name: String = "cboOpcode"

  override def chiselType: UInt = Opcode()

  override def dc: BitPat = BitPat.N(this.width)

  override def genTable(op: DecodePatternComb2[InstPattern, BoolPattern]): BitPat = {
    val DecodePatternComb2(inst, cboI2F) = op

    val bp = if (cboI2F.value && inst.name == getVariableName(CBO_INVAL)) {
      cbo_flush.encode
    } else {
      ScalaUopTable.tableZicbo(inst.bitPat).head.encode
    }

    bp.pad0To(Opcode.getWidth)
  }
}

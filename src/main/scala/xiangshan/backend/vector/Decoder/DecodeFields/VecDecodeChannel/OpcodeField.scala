package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.UInt
import chisel3.util.BitPat
import xiangshan.backend.decode.opcode.Opcode
import xiangshan.backend.decode.opcode.Opcode.Opcode
import xiangshan.backend.vector.Decoder.DecodePatterns.InstSewLmulNfPattern
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt.{BitPatToExt, UIntToUIntField}

class OpcodeField(uopIdx: Int) extends DecodeField[InstSewLmulNfPattern, UInt] {

  override def name: String = s"opcode$uopIdx"

  override def chiselType: UInt = Opcode()

  override def genTable(op: InstSewLmulNfPattern): BitPat = {
    val uopSeq = UopInfoFieldVec.genUopSeq(op)
    if (uopSeq.isDefinedAt(uopIdx)) {
      Option(uopSeq(uopIdx)).map(_.encode.padDcTo(Opcode.getWidth)).getOrElse(default)
    } else {
      default
    }
  }
}

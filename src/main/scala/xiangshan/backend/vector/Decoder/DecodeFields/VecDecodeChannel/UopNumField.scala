package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode.DecodeField
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.UopNumOH
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField

object UopNumField extends DecodeField[
  DecodePatternComb4[VecInstPattern, SewPattern, NfPattern, LmulPattern],
  UInt,
] {
  override def name: String = "uopNum"

  override def chiselType: UInt = UopNumOH()

  override def genTable(op: DecodePatternComb4[VecInstPattern, SewPattern, NfPattern, LmulPattern]): BitPat = {
    val uopSeq = UopInfoField.genUopSeq(op)

    uopSeq.length match {
      // produce 1 uop to hold illegal instruction info
      case 0 => UopNumOH.N1.toBitPat
      case 1 => UopNumOH.N1.toBitPat
      case 2 => UopNumOH.N2.toBitPat
      case 3 | 4 => UopNumOH.N4.toBitPat
      case 5 | 6 | 7 | 8 => UopNumOH.N4.toBitPat
    }
  }
}

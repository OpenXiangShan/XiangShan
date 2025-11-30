package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.NumUopOH
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField

object NumUopOhField extends DecodeField[
  DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern],
  UInt,
] {
  override def name: String = "uopNum"

  override def chiselType: UInt = NumUopOH()

  override def genTable(op: DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]): BitPat = {
    val uopSeq = UopInfoField.genUopSeq(op)

    uopSeq.length match {
      // produce 1 uop to hold illegal instruction info
      case 0 => NumUopOH.N1.toBitPat
      case 1 => NumUopOH.N1.toBitPat
      case 2 => NumUopOH.N2.toBitPat
      case 3 | 4 => NumUopOH.N4.toBitPat
      case 5 | 6 | 7 | 8 => NumUopOH.N4.toBitPat
    }
  }
}

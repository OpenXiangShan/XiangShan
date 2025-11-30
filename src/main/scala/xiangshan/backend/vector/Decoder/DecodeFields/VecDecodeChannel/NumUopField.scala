package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util.BitPat
import org.chipsalliance.cde.config.Parameters
import xiangshan.backend.Bundles.NumWB
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb4, LmulPattern, NfPattern, SewPattern}
import xiangshan.backend.vector.Decoder.Types.NumUop
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField

object NumUopField extends DecodeField[
  DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern],
  UInt,
] {

  override def name: String = "numUop"

  override def chiselType: UInt = NumUop()

  override def genTable(op: DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]): BitPat = {
    val num = UopInfoField.genUopSeq(op).length
    0.max(num - 1).U(NumUop.width.W).toBitPat
  }
}

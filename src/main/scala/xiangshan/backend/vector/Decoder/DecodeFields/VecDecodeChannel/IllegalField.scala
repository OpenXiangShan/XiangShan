package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb4, LmulPattern, NfPattern, SewPattern}
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ScalaTypeExt._

object IllegalField extends DecodeField[
  DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern],
  Bool,
] {
  override def name: String = "illegal"

  override def chiselType: Bool = Bool()

  override def genTable(op: DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]): BitPat = {
    UopInfoField.genUopSeq(op).isEmpty.toBitPat
  }
}

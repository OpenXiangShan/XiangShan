package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern.{VecInstPattern, VecLoadInstPattern, VecMemInstPattern, VecStoreInstPattern}
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb, DecodePatternComb4, LmulPattern, NfPattern, SewPattern}
import xiangshan.backend.vector.Decoder.Types.NumWB
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField

object NumWbField extends DecodeField[
  DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern],
  UInt,
] {

  override def name: String = "numWb"

  override def chiselType: UInt = NumWB()

  override def genTable(op: DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]): BitPat = {
    val num = UopInfoFieldVec.genUopSeq(op).length
    val DecodePatternComb(instP, sewP, lmulP, nfP) = op
    instP match {
      case vmi: VecMemInstPattern => vmi match {
        case pattern: VecLoadInstPattern => 0.max(num - 1).U(NumWB.width.W).toBitPat
        case pattern: VecStoreInstPattern =>
          0.max(num * 2 - 1).U(NumWB.width.W).toBitPat // store need writeback double
      }
      case _ => 0.max(num - 1).U(NumWB.width.W).toBitPat
    }
  }
}

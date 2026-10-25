package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern.{VecInstPattern, VecMemInstPattern, VecMemIndex, VecMemTrait, VecConfigInstPattern, ScaMultUopInstPattern}
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb, DecodePatternComb4, LmulPattern, NfPattern, SewPattern}
import xiangshan.backend.vector.Decoder.util.DecodeField

/** Mark indexed memory with nf+1 > 1 for destination/index overlap checking. */
object IsSegmentField extends DecodeField[
  DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern],
  Bool,
] {
  override def name: String = "isSegment"

  override def chiselType: Bool = Bool()

  override def genTable(op: DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]): BitPat = {
    val DecodePatternComb(instP, sewP, lmulP, nfP) = op
    instP match {
      case _: VecConfigInstPattern | _: ScaMultUopInstPattern => dc
      case vmi: VecMemInstPattern =>
        vmi.asInstanceOf[VecMemTrait] match {
          case _: VecMemIndex =>
            if (nfP.segNum > 1) BitPat.Y(1) else BitPat.N(1)
          case _ => BitPat.N(1)
        }
      case _ => BitPat.N(1)
    }
  }
}

package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.DecodePatterns.InstSewLmulNfPattern
import xiangshan.backend.vector.Decoder.InstPattern.{
  VecGatherEI16Pattern,
  VecGatherIPattern,
  VecGatherVPattern,
  VecGatherXPattern,
  VecInstPattern,
  VecMemInstPattern,
  VecMemTrait,
  VecMemWhole
}
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.DecodePatternComb
import xiangshan.backend.vector.Decoder.util.BoolDecodeField
import xiangshan.backend.vector.util.ScalaTypeExt.BooleanToExt

object UseGatherField extends BoolDecodeField[InstSewLmulNfPattern] {

  override def name: String = "useGather"

  override def genTable(op: InstSewLmulNfPattern): BitPat = {
    val DecodePatternComb(instP, _, _, nfP) = op
    val isGather = instP.isInstanceOf[VecGatherVPattern] ||
      instP.isInstanceOf[VecGatherXPattern] ||
      instP.isInstanceOf[VecGatherIPattern] ||
      instP.isInstanceOf[VecGatherEI16Pattern]
    val isSegment = instP match {
      case vmem: VecMemInstPattern =>
        nfP.segNum > 1 && !vmem.asInstanceOf[VecMemTrait].isInstanceOf[VecMemWhole]
      case _ => false
    }

    (isGather || isSegment).toBitPat
  }
}

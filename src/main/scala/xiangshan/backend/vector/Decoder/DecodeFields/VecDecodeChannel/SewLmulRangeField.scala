package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb2, SewPattern}
import xiangshan.backend.vector.Decoder.DecodePatterns.SewLmulPattern

object SewLmulRangeField {

  private type SewLmul = (Int, Double)

  private lazy val allPairs: Seq[SewLmul] = SewLmulPattern.all.map { sl =>
    (sl.p1.sewValue, sl.p2.lmulValue)
  }

  def validPairs(op: VecInstPattern): Set[SewLmul] = op match {
    case _: VecArithInstPattern => allPairs.toSet.filter(sewlmulFilter(op))
    case _ => throw new IllegalArgumentException(s"$op is not vai!")
  }

  private def sewlmulFilter(op: VecInstPattern): SewLmul => Boolean = op match {
    case _: SL_All          => _ => true
    case _: SL_Widen        => { case (sew, lmul) => sew <= 32 && lmul <= 4 }
    case _: SL_Float        => { case (sew, _)    => sew >= 16 }
    case _: SL_FloatWiden   => { case (sew, lmul) => sew >= 16 && sew <= 32 && lmul <= 4 }
    case _: SL_Narrow8      => { case (sew, _)    => sew >= 64 }
    case _: SL_Narrow4      => { case (sew, _)    => sew >= 32 }
    case _: SL_Narrow2      => { case (sew, _)    => sew >= 16 }
    case _: SL_CLMUL        => { case (sew, _)    => sew == 64 }
    case _: SL_BF16Widen    => { case (sew, lmul) => sew == 16 && lmul <= 4 }
    case _: SL_CryptW128E32 => { case (sew, lmul) => sew == 32 && lmul >= 1 }
    case _: SL_CryptW256E64 => { case (sew, lmul) => sew == 32 && lmul >= 2 }
    case _: SL_CryptBoth    => { case (sew, lmul) => sew == 32 && lmul >= 1 || sew == 64 && lmul >= 2}
    case _: SL_Gather16     => { case (sew, lmul) => sew >= 16 || lmul <= 4 }
    case _ => throw new IllegalArgumentException(s"Unknown SewLmul type in $op")
  }
}

package xiangshan.backend.vector.Decoder

import xiangshan.backend.vector.Decoder.DecodeChannel.VsetDecoderUtil.BoolPattern
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb2, DecodePatternComb4, LmulPattern, NfPattern, SewPattern}

object DecodePatterns {
  case class RdZeroPattern(
    rdZero: Option[Boolean]
  ) extends BoolPattern(rdZero)

  case class Rs1ZeroPattern(
    rs1Zero: Option[Boolean]
  ) extends BoolPattern(rs1Zero)

  type SewLmulPattern = DecodePatternComb2[SewPattern, LmulPattern]

  object SewLmulPattern {
    import SewPattern._
    import LmulPattern._

    def dontCare = SewPattern.dontCare ## LmulPattern.dontCare

    val e8mf8  = e8 ## mf8
    val e8mf4  = e8 ## mf4
    val e8mf2  = e8 ## mf2
    val e8m1   = e8 ## m1
    val e8m2   = e8 ## m2
    val e8m4   = e8 ## m4
    val e8m8   = e8 ## m8
    val e16mf4 = e16 ## mf4
    val e16mf2 = e16 ## mf2
    val e16m1  = e16 ## m1
    val e16m2  = e16 ## m2
    val e16m4  = e16 ## m4
    val e16m8  = e16 ## m8
    val e32mf2 = e32 ## mf2
    val e32m1  = e32 ## m1
    val e32m2  = e32 ## m2
    val e32m4  = e32 ## m4
    val e32m8  = e32 ## m8
    val e64m1  = e64 ## m1
    val e64m2  = e64 ## m2
    val e64m4  = e64 ## m4
    val e64m8  = e64 ## m8

    val e8All = Seq(
      e8mf8,
      e8mf4,
      e8mf2,
      e8m1,
      e8m2,
      e8m4,
      e8m8,
    )

    val e16All = Seq(
      e16mf4,
      e16mf2,
      e16m1,
      e16m2,
      e16m4,
      e16m8,
    )

    val e32All = Seq(
      e32mf2,
      e32m1,
      e32m2,
      e32m4,
      e32m8,
    )

    val e64All = Seq(
      e64m1,
      e64m2,
      e64m4,
      e64m8,
    )

    val mf8All = Seq(e8mf8)
    val mf4All = Seq(e8mf4, e16mf4)
    val mf2All = Seq(e8mf2, e16mf2, e32mf2)
    val m1All = Seq(e8m1, e16m1, e32m1, e64m1)
    val m2All = Seq(e8m2, e16m2, e32m2, e64m2)
    val m4All = Seq(e8m4, e16m4, e32m4, e64m4)
    val m8All = Seq(e8m8, e16m8, e32m8, e64m8)

    val all = e8All ++ e16All ++ e32All ++ e64All

    val fpAll = e16All ++ e32All ++ e64All

    val widenAll = e8All ++ e16All ++ e32All

    val fpWidenAll = e16All ++ e32All
  }

  type InstSewLmulNfPattern = DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]
}

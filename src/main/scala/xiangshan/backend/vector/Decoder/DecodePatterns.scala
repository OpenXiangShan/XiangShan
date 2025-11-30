package xiangshan.backend.vector.Decoder

import xiangshan.backend.vector.Decoder.DecodeChannel.VsetDecoderUtil.BoolPattern

object DecodePatterns {
  case class RdZeroPattern(
    rdZero: Option[Boolean]
  ) extends BoolPattern(rdZero)

  case class Rs1ZeroPattern(
    rs1Zero: Option[Boolean]
  ) extends BoolPattern(rs1Zero)
}

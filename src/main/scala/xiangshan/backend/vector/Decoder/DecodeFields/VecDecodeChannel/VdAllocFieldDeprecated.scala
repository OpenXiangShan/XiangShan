package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode.DecodeField
import xiangshan.backend.vector.Decoder.DecodeChannel.SplitCtlDecoderUtil.{SplitTypeOHPattern, UopLmulNfSplitOHPattern}
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb3, LmulPattern, NfPattern}
import xiangshan.backend.vector.Decoder.Split.SplitType
import xiangshan.backend.vector.util.BString.BinaryStringHelper
import xiangshan.backend.vector.util.ChiselTypeExt.UIntToUIntField

@deprecated
object VdAllocFieldDeprecated extends DecodeField[
  DecodePatternComb3[LmulPattern, NfPattern, SplitTypeOHPattern],
  UInt
] {
  override def name: String = "vdAlloc"

  override def chiselType: UInt = UInt(8.W)

  override def genTable(op: DecodePatternComb3[LmulPattern, NfPattern, SplitTypeOHPattern]): BitPat = {
    val UopLmulNfSplitOHPattern(lmulP, nfP, splitTypeOHP) = op
    val splitTypeValue: Int = splitTypeOHP.value
    var res = b"1111_1111"

    if (splitTypeValue == SplitType.VVM.litValue) {
      res = b"0000_0001"
    } else if (splitTypeValue == SplitType.WVV.litValue) {
      res = b"0101_0101"
    } else if (Seq(
      SplitType.VREDU, SplitType.VREDO,
      SplitType.VWREDU, SplitType.VWREDO,
    ).map(_.litValue).contains(splitTypeValue)) {
      res = b"0000_0001"
    }

    res.toBitPat
  }
}

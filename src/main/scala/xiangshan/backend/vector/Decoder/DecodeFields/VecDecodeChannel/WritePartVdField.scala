package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern.{VecArithInstPattern, VecCarryMPattern, VecCarryPattern, VecCompressPattern, VecConfigInstPattern, VecDVPattern, VecGatherEI16Pattern, VecGatherIPattern, VecGatherVPattern, VecGatherXPattern, VecInstPattern, VecIntAvgVVVPattern, VecIntClipWVVPattern, VecIntMMMPattern, VecIntNarrowShiftWVVPattern, VecIntRedPattern, VecIntS1VDVPattern, VecIntS1XDVPattern, VecIntS2DVExtF2Pattern, VecIntS2DVExtF4Pattern, VecIntS2DVExtF8Pattern, VecIntS2DVPattern, VecIntSatMulVVVPattern, VecIntSatVVVPattern, VecIntScaleShiftVVVPattern, VecIntVVMPattern, VecIntVVVPattern, VecIntVVVVPattern, VecIntVVWPattern, VecIntVVWWPattern, VecIntWRedPattern, VecIntWVWPattern, VecLoadInstPattern, VecMemFF, VecMemIndex, VecMemInstPattern, VecMemMask, VecMemStrided, VecMemTrait, VecMemUnitStride, VecMemWhole, VecS1XDAPattern, VecS2ADXPattern, VecS2MDMPattern, VecS2MDVPattern, VecS2MDXPattern, VecSlide1Pattern, VecSlideIPattern, VecSlideXPattern, VecStoreInstPattern}
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb, DecodePatternComb3, DecodePatternComb4, LmulPattern, NfPattern, SewPattern}
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

/**
 * This field is used to specify if an instruction write part vd.
 */
object WritePartVdField extends BoolDecodeField[
  DecodePatternComb4[
    VecInstPattern,
    SewPattern,
    LmulPattern,
    NfPattern,
  ]
] {

  override def name: String = "isWritePartVd"

  override def genTable(op: DecodePatternComb4[VecInstPattern, SewPattern, LmulPattern, NfPattern]): BitPat = {
    val DecodePatternComb(viP, sewP, lmulP, nfP) = op

    val sew: Int = sewP.sewValue
    val lmul: Option[Double] =
      try Some(lmulP.lmulValue)
      catch {
        case _: Throwable => None
      }

    viP match {
      case vai: VecArithInstPattern => vai match {
        case VecIntVVMPattern() => y
        case VecCarryMPattern() => y
        case VecIntMMMPattern() => y
        case VecS1XDAPattern() => y
        case VecS2MDMPattern() => y
        case VecIntRedPattern() => y
        case VecIntWRedPattern() => y
        case _  =>
          if (lmul.get > 0 && lmul.get < 1)
            y
          else
            n
      }
      case VecConfigInstPattern() => n
      case vmi: VecMemInstPattern =>
        vmi match {
          case vli: VecLoadInstPattern => vli.asInstanceOf[VecMemTrait] match {
            case _: VecMemWhole => n
            case _: VecMemMask =>
              if (sew == 8 && lmul.get == 8)
                n
              else
                y
            case _ =>
              val eew: Int = vli.eewValue
              val emul: Double = eew * lmul.get / sew
              if (emul < 1)
                y
              else
                n
          }
          case _: VecStoreInstPattern => n
        }
    }
  }
}

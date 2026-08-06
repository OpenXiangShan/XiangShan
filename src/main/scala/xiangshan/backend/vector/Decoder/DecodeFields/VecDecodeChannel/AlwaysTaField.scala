package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern.{VecArithInstPattern, VecCarryMPattern, VecCarryPattern, VecCompressPattern, VecConfigInstPattern, VecDVPattern, VecGatherEI16Pattern, VecGatherIPattern, VecGatherVPattern, VecGatherXPattern, VecInstPattern, VecIntAvgVVVPattern, VecIntClipWVVPattern, VecIntMMMPattern, VecIntNarrowShiftWVVPattern, VecIntRedPattern, VecIntS1VDVPattern, VecIntS1XDVPattern, VecIntS2DVExtF2Pattern, VecIntS2DVExtF4Pattern, VecIntS2DVExtF8Pattern, VecIntS2DVPattern, VecIntSatMulVVVPattern, VecIntSatVVVPattern, VecIntScaleShiftVVVPattern, VecIntVVMPattern, VecIntVVVPattern, VecIntVVVVPattern, VecIntVVWPattern, VecIntVVWWPattern, VecIntWRedPattern, VecIntWVWPattern, VecLoadInstPattern, VecLoadMask, VecMemInstPattern, VecS1XDAPattern, VecS2ADXPattern, VecS2MDMPattern, VecS2MDVPattern, VecS2MDXPattern, VecSlide1Pattern, VecSlideIPattern, VecSlideXPattern, VecStoreInstPattern}
import xiangshan.backend.vector.Decoder.util.BoolDecodeField

/**
 * This field is used to specify if an instruction always uses tail-agnostic policy.
 */
object AlwaysTaField extends BoolDecodeField[VecInstPattern] {

  override def name: String = "alwaysTa"

  override def genTable(op: VecInstPattern): BitPat = {
    op match {
      case vai: VecArithInstPattern => vai match {
        case VecIntVVMPattern() => y
        case VecIntMMMPattern() => y
        case VecCarryMPattern() => y
        case VecS2MDMPattern() => y
        case _ => n
      }
      case VecConfigInstPattern() => n
      case vmi: VecMemInstPattern => vmi match {
        case VecLoadMask() => y
        case _ => n
      }
    }
  }
}

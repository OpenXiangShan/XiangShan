package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.UInt
import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.SrcRenType
import xiangshan.backend.vector.Decoder.Types.DecodeSrcType.{FP, GP}
import xiangshan.backend.vector.Decoder.util.DecodeField
import xiangshan.backend.vector.util.ChiselTypeExt._

object Src2Field extends DecodeField[InstPattern, SrcRenType] {
  override def name: String = "src2"

  override def chiselType: SrcRenType = new SrcRenType

  override def genTable(op: InstPattern): BitPat = {
    val srcType: UInt = op match {
      case int: IntInstPattern => int match {
        case _: IntRTypePattern => GP
        case _: IntITypePattern => null
        case _: IntSTypePattern => GP
        case IntBTypePattern() => GP
        case IntUTypePattern() => null
        case IntJTypePattern() => null
      }
      case fp: FpInstPattern => fp match {
        case _: FpITypeInstPattern => null
        case _: FpRTypeInstPattern => FP
        case FpR4TypeInstPattern() => FP
        case FpSTypeInstPattern() => FP
      }
      case _: VecInstPattern =>
        throw new IllegalArgumentException(s"VecInstPattern is not support $op in Src2Field")
    }

    srcType match {
      case null => BitPat.N() ## BitPat.dontCare(2)
      case src: UInt => BitPat.Y() ## src
    }
  }
}

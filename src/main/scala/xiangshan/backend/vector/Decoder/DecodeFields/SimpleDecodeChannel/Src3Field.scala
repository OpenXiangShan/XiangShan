package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.UInt
import chisel3.util.BitPat
import chisel3.util.experimental.decode.DecodeField
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.SrcRenType
import xiangshan.backend.vector.Decoder.Types.DecodeSrcType.FP
import xiangshan.backend.vector.util.ChiselTypeExt._

object Src3Field extends DecodeField[InstPattern, SrcRenType] {
  override def name: String = "src3"

  override def chiselType: SrcRenType = new SrcRenType

  override def genTable(op: InstPattern): BitPat = {
    val srcType: UInt = op match {
      case pattern: FpInstPattern => pattern match {
        case FpR4TypeInstPattern() => FP
        case _ => null
      }
      case _: VecInstPattern =>
        throw new IllegalArgumentException(s"not support VecInstPattern $op in Src3Field")
      case _ => null
    }

    srcType match {
      case null => BitPat.N() ## BitPat.dontCare(2)
      case src: UInt => BitPat.Y() ## src
    }
  }
}


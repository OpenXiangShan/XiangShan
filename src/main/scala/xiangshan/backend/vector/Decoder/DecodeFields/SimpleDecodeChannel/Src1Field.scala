package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode.DecodeField
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb2, SewPattern}
import xiangshan.backend.vector.Decoder.SrcRenType
import xiangshan.backend.vector.Decoder.Types.DecodeSrcType.{FP, GP}
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.ScalaTypeExt._

object Src1Field extends DecodeField[InstPattern, SrcRenType] {

  override def name: String = "src1"

  override def chiselType: SrcRenType = new SrcRenType

  override def genTable(op: InstPattern): BitPat = {
    val srcType: UInt = op match {
      case int: IntInstPattern => int match {
        case _: IntRTypePattern => GP
        case intI: IntITypePattern => intI match {
          case SystemInstPattern() | CboInstPattern() => null
          case _ => GP
        }
        case _: IntSTypePattern => GP
        case IntBTypePattern() => GP
        case IntUTypePattern() => null
        case IntJTypePattern() => null
      }
      case fp: FpInstPattern => fp match {
        case fpI: FpITypeInstPattern => fpI match {
          case FpITypeF2fInstPattern() => FP
          case FpITypeF2iInstPattern() => FP
          case FpITypeImmInstPattern() => null
          case i2f: FpITypeI2fInstPattern => i2f match {
            case FpITypeImmInstPattern() => null
            case _ => GP
          }
          case FpITypeLoadInstPattern() => GP
        }
        case _: FpRTypeInstPattern => FP
        case FpR4TypeInstPattern() => FP
        case FpSTypeInstPattern() => GP
      }
      case vec: VecInstPattern =>
        throw new IllegalArgumentException(s"VecInstPattern $vec is not supported in Src1Field")
    }

    srcType match {
      case null => BitPat.N() ## BitPat.dontCare(2)
      case src: UInt => BitPat.Y() ## src
    }
  }

  def useIMM(op: VecArithInstPattern): Boolean = {
    val suffixes = Seq(
      "_VI",
      "_VIM",
      "_WI",
      "_V_I",
    )
    op.name.endsWithThese(suffixes)
  }

  def useGP(op: VecArithInstPattern): Boolean = {
    val suffixes = Seq(
      "_VX",
      "_VXM",
      "_WX",
      "_S_X",
      "_V_X",
    )
    op.name.endsWithThese(suffixes)
  }

  def useVP(op: VecArithInstPattern): Boolean = {
    val suffixes = Seq(
      "_VV",
      "_VVM",
      "_WV",
      "_VS",
      "_MM",
      "_V_V",
      "_VM",
    )
    op.name.endsWithThese(suffixes)
  }

  def useFP(op: VecArithInstPattern): Boolean = {
    val suffixes = Seq(
      "_VF",
      "_S_F",
      "_V_F",
      "_VFM",
      "_WF",
    )
    op.name.endsWithThese(suffixes)
  }

  def useNO(op: VecArithInstPattern): Boolean = {
    val suffixes = Seq(
      "R_V", // VMVNR_V
      "_X_S",
      "_F_S",
      "_M",
      "_VF2",
      "_VF4",
      "_VF8",
      "VID_V",
    )
    val prefixes = Seq(
      "VFCVT",
      "VFNCVT",
      "VFWCVT",
      "VFCLASS",
      "VFREC7",
      "VFRSQRT7",
      "VFSQRT",
    )
    op.name.startsWithThese(prefixes) || op.name.endsWithThese(suffixes)
  }
}


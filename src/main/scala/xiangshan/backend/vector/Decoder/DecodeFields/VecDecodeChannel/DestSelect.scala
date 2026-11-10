package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb, DecodePatternComb2, SewPattern}
import xiangshan.backend.vector.Decoder.Sews
import xiangshan.backend.vector.Decoder.util.DecodeField

object DestSelectEnum extends Enumeration {
  import scala.language.implicitConversions

  case class DestVal(str: String) extends super.Val {
    def toUInt: UInt = {
      ("b" + str).U(str.length.W)
    }

    def toBitPat: BitPat = {
      BitPat("b" + str)
    }
  }
  protected implicit def valueToEnum(x: Value): DestVal = x.asInstanceOf[DestVal]
  protected implicit def valueToString(x: Value): String = x.str

  lazy val width: Int = this.values.map(_.str.length).max

  def UInt(): UInt = chisel3.UInt(width.W)

  // Todo: if treat CONST and NONE as the same
  val NONE       = DestVal("000") // no vs2
  val INC1       = DestVal("001") // vd|0,1,2,3,4,5,6,7
  val INCF2      = DestVal("010") // vd|0,0,1,1,2,2,3,3
  val INCF4      = DestVal("011") // vd|0,0,0,0,1,1,1,1
  val CONST      = DestVal("100") // vd|0
  val AMOCASQ    = DestVal("101") // for amocas instruction, rd|0,0,1,1
}

object DestSelectField extends DecodeField[
  DecodePatternComb2[VecInstPattern, SewPattern],
  UInt
] {
  import DestSelectEnum._

  override def name: String = "destSel"

  override def chiselType: UInt = DestSelectEnum.UInt()

  override def genTable(op: DecodePatternComb2[VecInstPattern, SewPattern]): BitPat = {
    val DecodePatternComb(instP, sewP) = op

    val destSel: DestVal = instP match {
      case vai: VecArithInstPattern =>
        vai match {
          case vci: VecCryptoInstPattern => INC1
          case vii: VecIntArithInstPattern =>
            vii match {
              case VecIntVVVPattern() => INC1
              case VecIntVVVVPattern() => INC1
              case VecIntVVMPattern() => CONST
              case VecIntMMMPattern() => CONST
              case VecIntVVWPattern() => INC1
              case VecIntVVWWPattern() => INC1
              case VecIntWVWPattern() => INC1
              case VecIntSatVVVPattern() => INC1
              case VecIntSatMulVVVPattern() => INC1
              case VecIntScaleShiftVVVPattern() => INC1
              case VecIntNarrowShiftWVVPattern() => INCF2
              case VecIntClipWVVPattern() => INCF2
              case VecIntAvgVVVPattern() => INC1
              case VecGatherVPattern() => INC1
              case VecGatherXPattern() => INC1
              case VecGatherIPattern() => INC1
              case VecGatherEI16Pattern() =>
                val sewVal = Sews.decodeValue(sewP.bitPat)
                sewVal match {
                  case 8 => INCF2
                  case 16 | 32 | 64 => INC1
                  case _ => throw new IllegalArgumentException("sew should be in Seq(8, 16, 32, 64)")
                }
              case VecCompressPattern() => INC1
              case VecSlideXPattern() => INC1
              case VecSlideIPattern() => INC1
              case VecSlide1Pattern() => INC1
              case VecCarryPattern() => INC1
              case VecCarryMPattern() => CONST
              case VecIntS1VDVPattern() => INC1
              case VecIntS1XDVPattern() => INC1
              case VecS1XDAPattern() => INC1
              case VecIntS2DVWholeMvPattern() => INC1
              case VecIntS2DVPattern() => INC1
              case VecIntS2DVExtF8Pattern() => INC1
              case VecIntS2DVExtF4Pattern() => INC1
              case VecIntS2DVExtF2Pattern() => INC1
              case VecS2ADXPattern() => CONST
              case VecS2MDXPattern() => CONST
              case VecS2MDMPattern() => CONST
              case VecS2MDVPattern() => INC1
              case VecDVPattern() => INC1
              case VecIntRedPattern() => CONST
              case VecIntWRedPattern() => CONST
            }
          case vfi: VecFpArithInstPattern =>
            vfi match {
              case VecFpOp2VVPattern() => INC1
              case VecFpOp2VMPattern() => CONST
              case VecFpOp3VVVPattern() => INC1
              case VecFpRedPattern() => CONST
              case VecFpWRedPattern() => CONST
              case VecFpOp2VVWPattern() => INC1
              case VecFpOp2WVWPattern() => INC1
              case VecFpOp3VVWPattern() => INC1
              case VecFpS2VPattern() => INC1
              case VecFpS2VVWPattern() => INC1
              case VecFpS2WVIntPattern() => INC1
              case VecFpS2WVFpPattern() => INC1
              case VecFpS2APattern() => CONST
              case VecFpS1VPattern() => INC1
            }
          case _ =>
            throw new IllegalArgumentException(s"Unsupported vector arith pattern $vai in DestSelectField")
        }
      case VecConfigInstPattern() => CONST
      case vmi: VecMemInstPattern =>
        vmi match {
          case _: VecMemUnitStride => INC1
          case _: VecMemStrided => INC1
          case _: VecMemIndex =>
            val eew: Double = vmi.eewValue
            val sew: Double = Sews.decodeValue(sewP.bitPat)
            val dEew = sew
            val iEew = eew
            val diRatio = dEew / iEew
            diRatio match {
              case 0.125 => CONST
              case 0.25  => INCF4
              case 0.5   => INCF2
              case 1     => INC1
              case 2     => INC1
              case 4     => INC1
              case 8     => INC1
            }
          case _: VecMemWhole => INC1
          case _: VecMemMask => CONST
        }
      case smui: ScaMultUopInstPattern =>
        smui match {
          case _: AmocasInstPattern => AMOCASQ
          case _ => CONST
        }
    }
    destSel.toBitPat
  }
}

package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb, DecodePatternComb2, SewPattern}
import xiangshan.backend.vector.Decoder.Sews
import xiangshan.backend.vector.Decoder.util.DecodeField

object Src2SelectEnum extends Enumeration {
  import scala.language.implicitConversions

  case class Src2Val(str: String) extends super.Val {
    def toUInt: UInt = {
      ("b" + str).U(str.length.W)
    }

    def toBitPat: BitPat = {
      BitPat("b" + str)
    }
  }
  protected implicit def valueToEnum(x: Value): Src2Val = x.asInstanceOf[Src2Val]
  protected implicit def valueToString(x: Value): String = x.str

  lazy val width: Int = this.values.map(_.str.length).max

  def UInt(): UInt = chisel3.UInt(width.W)

  // Todo: if treat CONST and NONE as the same
  val NONE       = Src2Val("000") // no vs2
  val INC1       = Src2Val("001") // vs2|0,1,2,3,4,5,6,7
  val INCF2      = Src2Val("010") // vs2|0,0,1,1,2,2,3,3
  val INCF4      = Src2Val("011") // vs2|0,0,0,0,1,1,1,1
  val CONST      = Src2Val("100") // vs2|0
  val INC1x7_S1  = Src2Val("110") // vs2|0,1,2,3,4,5,6,vs1|0
  val INCF2x7_S1 = Src2Val("111") // vs2|0,0,1,1,2,2,3,vs1|0
}

object Src2SelectField extends DecodeField[
  DecodePatternComb2[VecInstPattern, SewPattern],
  UInt
] {
  import Src2SelectEnum._

  override def name: String = "src2Sel"

  override def chiselType: UInt = Src2SelectEnum.UInt()

  override def genTable(op: DecodePatternComb2[VecInstPattern, SewPattern]): BitPat = {
    val DecodePatternComb(instP, sewP) = op

    val src2Sel: Src2Val = instP match {
      case vai: VecArithInstPattern =>
        vai match {
          case vii: VecIntArithInstPattern =>
            vii match {
              case VecIntVVVPattern() => INC1
              case VecIntVVVVPattern() => INC1
              case VecIntVVMPattern() => INC1
              case VecIntMMMPattern() => CONST
              case VecIntVVWPattern() => INCF2
              case VecIntVVWWPattern() => INCF2
              case VecIntWVWPattern() => INC1
              case VecIntSatVVVPattern() => INC1
              case VecIntSatMulVVVPattern() => INC1
              case VecIntScaleShiftVVVPattern() => INC1
              case VecIntNarrowShiftWVVPattern() => INC1
              case VecIntClipWVVPattern() => INC1
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
              case VecCarryMPattern() => INC1
              case VecIntS1VDVPattern() => NONE
              case VecIntS1XDVPattern() => NONE
              case VecS1XDAPattern() => NONE
              case VecIntS2DVWholeMvPattern() => INC1
              case VecIntS2DVPattern() => INC1
              case VecIntS2DVExtF8Pattern() => CONST
              case VecIntS2DVExtF4Pattern() => INCF4
              case VecIntS2DVExtF2Pattern() => INCF2
              case VecS2ADXPattern() => INC1
              case VecS2MDXPattern() => CONST
              case VecS2MDMPattern() => CONST
              case VecS2MDVPattern() => CONST
              case VecDVPattern() => NONE
              case VecIntRedPattern() => INC1x7_S1
              case VecIntWRedPattern() => INCF2x7_S1
            }
          case vfi: VecFpArithInstPattern => ???
        }
      case VecConfigInstPattern() => CONST
      case vmi: VecMemInstPattern =>
        vmi.asInstanceOf[VecMemTrait] match {
          case _: VecMemUnitStride => NONE
          case _: VecMemStrided => CONST
          case _: VecMemIndex =>
            val eew: Double = vmi.eewValue
            val sew: Double = Sews.decodeValue(sewP.bitPat)
            val dEew = sew
            val iEew = eew
            val diRatio = dEew / iEew
            diRatio match {
              case 0.125 => INC1
              case 0.25  => INC1
              case 0.5   => INC1
              case 1     => INC1
              case 2     => INCF2
              case 4     => INCF4
              case 8     => CONST
            }
          case _: VecMemWhole => NONE
          case _: VecMemMask => NONE
          case _: VecMemFF => NONE
        }
    }

    src2Sel.toBitPat
  }
}

package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb, DecodePatternComb2, SewPattern}
import xiangshan.backend.vector.Decoder.Sews
import xiangshan.backend.vector.Decoder.util.DecodeField

object Src1SelectEnum extends Enumeration {
  import scala.language.implicitConversions

  case class Src1Val(str: String) extends super.Val {
    def toUInt: UInt = {
      ("b" + str).U(str.length.W)
    }

    def toBitPat: BitPat = {
      BitPat("b" + str)
    }
  }
  protected implicit def valueToEnum(x: Value): Src1Val = x.asInstanceOf[Src1Val]
  protected implicit def valueToString(x: Value): String = x.str

  lazy val width: Int = this.values.map(_.str.length).max

  def UInt(): UInt = chisel3.UInt(width.W)

  // Todo: if treat CONST and NONE as the same
  val NONE             = Src1Val("000") // no vs1
  val INC1             = Src1Val("001") // vs1|0,1,2,3,4,5,6,7
  val INCF2            = Src1Val("010") // vs1|0,0,1,1,2,2,3,3
  val INCF4            = Src1Val("011") // vs1|0,0,0,0,1,1,1,1
  val CONST            = Src1Val("100") // vs1|0
  val S1MINx1_DCONST   = Src1Val("101") // vs1|0,vd|0,0,0,0,0,0,0
  val S2MAXx1_DCONST   = Src1Val("110") // vs2|7,vd|0,0,0,0,0,0,0
  val S2MAXF2x1_DCONST = Src1Val("111") // vs2|3,vd|0,0,0,0,0,0,0
}

object Src1SelectField extends DecodeField[
  DecodePatternComb2[VecInstPattern, SewPattern],
  UInt
] {
  import Src1SelectEnum._

  private def fpSrc1Sel(pattern: VecFpArithInstPattern, default: Src1Val): Src1Val = {
    pattern.category.rawString match {
      case VecInstPattern.Category.OPFVF.str => CONST
      case _ => default
    }
  }

  private def intSrc1Sel(pattern: VecIntArithInstPattern, default: Src1Val): Src1Val = {
    pattern.category.rawString match {
      case VecInstPattern.Category.OPIVX.str | VecInstPattern.Category.OPMVX.str => CONST
      case _ => default
    }
  }

  override def name: String = "src1Sel"

  override def chiselType: UInt = Src1SelectEnum.UInt()

  override def genTable(op: DecodePatternComb2[VecInstPattern, SewPattern]): BitPat = {
    val DecodePatternComb(instP, sewP) = op

    val src1Sel: Src1Val = instP match {
      case vai: VecArithInstPattern =>
        vai match {
          case vii: VecIntArithInstPattern =>
            vii match {
              case VecIntVVVPattern() => intSrc1Sel(vii, INC1)
              case VecIntVVVVPattern() | VecCryptoVVVVPattern() => intSrc1Sel(vii, INC1)
              case VecIntVVMPattern() => intSrc1Sel(vii, INC1)
              case VecIntMMMPattern() => CONST
              case VecIntVVWPattern() => intSrc1Sel(vii, INCF2)
              case VecIntVVWWPattern() => intSrc1Sel(vii, INCF2)
              case VecIntWVWPattern() => intSrc1Sel(vii, INCF2)
              case VecIntSatVVVPattern() => intSrc1Sel(vii, INC1)
              case VecIntSatMulVVVPattern() => intSrc1Sel(vii, INC1)
              case VecIntScaleShiftVVVPattern() => intSrc1Sel(vii, INC1)
              case VecIntNarrowShiftWVVPattern() => intSrc1Sel(vii, INCF2)
              case VecIntClipWVVPattern() => intSrc1Sel(vii, INCF2)
              case VecIntAvgVVVPattern() => intSrc1Sel(vii, INC1)
              case VecGatherVPattern() => INC1
              case VecGatherXPattern() => CONST
              case VecGatherIPattern() => NONE
              case VecGatherEI16Pattern() =>
                val sewVal = Sews.decodeValue(sewP.bitPat)
                sewVal match {
                  case 8 | 16 => INC1
                  case 32 => INCF2
                  case 64 => INCF4
                  case _ => throw new IllegalArgumentException("sew should be in Seq(8, 16, 32, 64)")
                }
              case VecCompressPattern() => CONST
              case VecSlideXPattern() => CONST
              case VecSlideIPattern() => NONE
              case VecSlide1Pattern() => CONST
              case VecCarryPattern() => intSrc1Sel(vii, INC1)
              case VecCarryMPattern() => intSrc1Sel(vii, INC1)
              case VecIntS1VDVPattern() => INC1
              case VecIntS1XDVPattern() => CONST
              case VecS1XDAPattern() => CONST
              case VecIntS2DVWholeMvPattern() => NONE
              case VecIntS2DVPattern() => NONE
              case VecIntS2DVExtF8Pattern() => NONE
              case VecIntS2DVExtF4Pattern() => NONE
              case VecIntS2DVExtF2Pattern() => NONE
              case VecS2ADXPattern() => NONE
              case VecS2MDXPattern() => NONE
              case VecS2MDMPattern() => NONE
              case VecS2MDVPattern() => NONE
              case VecDVPattern() => NONE
              case VecIntRedPattern() => S2MAXx1_DCONST
              case VecIntWRedPattern() => S2MAXF2x1_DCONST
            }
          case vfi: VecFpArithInstPattern =>
            vfi match {
              case VecFpOp2VVPattern() => fpSrc1Sel(vfi, INC1)
              case VecFpOp2VMPattern() => fpSrc1Sel(vfi, INC1)
              case VecFpOp3VVVPattern() => fpSrc1Sel(vfi, INC1)
              case VecFpRedPattern() => S2MAXx1_DCONST
              case VecFpWRedPattern() => S2MAXF2x1_DCONST
              case VecFpOp2VVWPattern() => fpSrc1Sel(vfi, INCF2)
              case VecFpOp2WVWPattern() => fpSrc1Sel(vfi, INCF2)
              case VecFpOp3VVWPattern() => fpSrc1Sel(vfi, INCF2)
              case VecFpS2VPattern() => NONE
              case VecFpS2VVWPattern() => NONE
              case VecFpS2WVIntPattern() => NONE
              case VecFpS2WVFpPattern() => NONE
              case VecFpS2APattern() => NONE
              case VecFpS1VPattern() => CONST
            }
          case _ =>
            throw new IllegalArgumentException(s"Unsupported vector arith pattern $vai in Src1SelectField")
        }
      case VecConfigInstPattern() => CONST
      case vmi: VecMemInstPattern => CONST
      case vii: VecIntInstPattern => CONST
    }
    src1Sel.toBitPat
  }
}
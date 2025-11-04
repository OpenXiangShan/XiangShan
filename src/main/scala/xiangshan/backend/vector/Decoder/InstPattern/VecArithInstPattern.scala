package xiangshan.backend.vector.Decoder.InstPattern

import chisel3.util.BitPat
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern.Category

import scala.util.matching.Regex

sealed class VecArithInstPattern()(
  implicit rawInst: BitPat,
) extends VecInstPattern() {
  def func6 : BitPat = rawInst(31, 26)
  def vm    : BitPat = rawInst(25)
  def category : BitPat = rawInst(14, 12)

  override def bitPat: BitPat = genPattern

  val genPattern = rawInst.ensuring(_.getWidth == 32)
}

object VecArithInstPattern extends VecInstFieldDefination {
  def apply()(implicit rawInst: BitPat): VecArithInstPattern = {
    import scala.language.implicitConversions

    val OPIVV = "000"
    val OPFVV = "001"
    val OPMVV = "010"
    val OPIVI = "011"
    val OPIVX = "100"
    val OPFVF = "101"
    val OPMVX = "110"
    val OPCFG = "111"

    category.rawString match {
      case OPIVV | OPIVX | OPIVI =>
        func6.rawString match {
          // vadd, vandn, vsub, vrsub
          case "000000" | "000001" | "000010" | "000011" => VecIntVVVPattern()
          // min/max
          case "000100" | "000101" | "000110" | "000111" => VecIntVVVPattern()
          // and,or,xor
          case "001001" | "001010" | "001011" => VecIntVVVPattern()
          case "001100" =>
            category.rawString match {
              case OPIVV => VecGatherVPattern()
              case OPIVX => VecGatherXPattern()
              case OPIVI => VecGatherIPattern()
            }
          case "001110" =>
            category.rawString match {
              case OPIVV => VecGatherEI16Pattern()
              case OPIVX => VecSlideXPattern()
              case OPIVI => VecSlideIPattern()
            }
          case "001111" =>
            category.rawString match {
              case OPIVX => VecSlideXPattern()
              case OPIVI => VecSlideIPattern()
            }
          // carry
          case "010000" | "010010" =>
            VecCarryPattern()
          // carry mask
          case "010001" | "010011" =>
            VecCarryMPattern()
          // rotate shift
          case "010100" | "010101" =>
            category.rawString match {
              case OPIVV | OPIVX => VecIntVVVPattern()
            }
          case "01010?" => VecIntVVVPattern()
          case "010111" =>
            vm.rawString match {
              // vmerge.v?m
              case "0" => VecIntVVVPattern()
              case "1" =>
                category.rawString match {
                  // vmv.v.v
                  case OPIVV => VecIntS1VDVPattern()
                  // vmv.v.[xi]
                  case _ => VecIntS1XDVPattern()
                }

            }
          // Vec Int Cmp
          case "011000" | "011001" | "011010" | "011011" | "011100" | "011101" | "011110" | "011111" => VecIntVVMPattern()
          // singal width saturate
          case "100000" | "100001" | "100010" | "100011" => VecIntSatVVVPattern()
          // vsll
          case "100101" => VecIntVVVPattern()
          case "100111" =>
            category.rawString match {
              // vsmul
              case OPIVV | OPIVX => VecIntSatMulVVVPattern()
              // vmvnr
              case OPIVI => VecIntS2DVPattern()
            }
          // vsrl, vsra
          case "101000" | "101001" => VecIntVVVPattern()
          // vssrl, vssra
          case "101010" | "101011" => VecIntScaleShiftVVVPattern()
          // vnsrl, vnsra
          case "101100" | "101101" => VecIntNarrowShiftWVVPattern()
          // vnclip{u}
          case "101110" | "101111" => VecIntClipWVVPattern()
          // vwredsum{u}
          case "110000" | "110001" => VecIntWRedPattern()
          // vwsll
          case "110101" => VecIntVVWPattern()
        }
      case OPMVV | OPMVX =>
        func6.rawString match {
          case "000000" | "000001" | "000010" | "000011" | "000100" | "000101" | "000110" | "000111" => VecIntRedPattern()
          case "001000" | "001001" | "001010" | "001011" => VecIntAvgVVVPattern()
          case "001100" | "001101" => VecIntVVVPattern()
          case "001110" | "001111" => VecSlide1Pattern()
          case "010000" =>
            category.rawString match {
              // VWXUNARY0
              case OPMVV =>
                vs1.rawString match {
                  // vmv.x.s
                  case "00000" => VecS2ADXPattern()
                  // vcpop.m, vfirst.m
                  case "10000" | "10001" => VecS2MDXPattern()
                }
              // VRXUNARY0
              case OPMVX =>
                vs2.rawString match {
                  // vmv.s.x
                  case "00000" => VecS1XDAPattern()
                }
            }
          // VXUNARY0
          case "010010" =>
            category.rawString match {
              case OPMVV =>
                vs1.rawString match {
                  // v{z,s}ext.vf{2,4,8}
                  case "00010" | "00011" => VecIntS2DVExtF8Pattern()
                  case "00100" | "00101" => VecIntS2DVExtF4Pattern()
                  case "00110" | "00111" => VecIntS2DVExtF2Pattern()
                  // vbrev8, vrev8, vbrev
                  case "01000" | "01001" | "01010" => VecIntS2DVPattern()
                  // vclz, vctz, vcpop
                  case "01100" | "01101" | "01110" => VecIntS2DVPattern()
                }
            }
          // VMUNARY
          case "010100" =>
            category.rawString match {
              case OPMVV =>
                vs1.rawString match {
                  // vmsbf, vmsof, vmsif
                  case "00001" | "00010" | "00011" => VecS2MDMPattern()
                  // viota
                  case "10000" => VecS2MDVPattern()
                  // vid
                  case "10001" => VecDVPattern()
                }
            }
          // vcompress
          case "010111" =>
            category.rawString match {
              case OPMVV => VecCompressPattern()
            }
          case s if s.startsWith("011") =>
            category.rawString match {
              case OPMVV => VecIntMMMPattern()
            }
          // vdiv{u}, vrem{u}, vmul{hu,hsu,h}
          case "100000" | "100001" | "100010" | "100011" | "100100" | "100101" | "100110" | "100111" => VecIntVVVPattern()
          // vmadd, vnmsub, vmacc, vnmsac
          case "101001" | "101011" | "101101" | "101111" => VecIntVVVVPattern()
          // vwaddu, ... vwsub
          case "110000" | "110001" | "110010" | "110011" => VecIntVVWPattern()
          // vwaddu.w, ... vwsub.w
          case "110100" | "110101" | "110110" | "110111" => VecIntWVWPattern()
          // vwmul
          case "111000" | "111010" | "111011" => VecIntVVWPattern()
          // vwmacc
          case s if s.startsWith("1111") => VecIntVVWWPattern()
        }
      case _ => null
    }
  }
}

case class VecIntVVVPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntVVVVPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntVVMPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntMMMPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntVVWPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntVVWWPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntWVWPattern()(implicit rawInst: BitPat) extends VecArithInstPattern

case class VecIntSatVVVPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntSatMulVVVPattern()(implicit rawInst: BitPat) extends VecArithInstPattern

case class VecIntScaleShiftVVVPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntNarrowShiftWVVPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntClipWVVPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntAvgVVVPattern()(implicit rawInst: BitPat) extends VecArithInstPattern

case class VecGatherVPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecGatherXPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecGatherIPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecGatherEI16Pattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecCompressPattern()(implicit rawInst: BitPat) extends VecArithInstPattern

case class VecSlideXPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecSlideIPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecSlide1Pattern()(implicit rawInst: BitPat) extends VecArithInstPattern

case class VecCarryPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecCarryMPattern()(implicit rawInst: BitPat) extends VecArithInstPattern

case class VecIntS1VDVPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntS1XDVPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecS1XDAPattern()(implicit rawInst: BitPat) extends VecArithInstPattern

case class VecIntS2DVPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntS2DVExtF8Pattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntS2DVExtF4Pattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntS2DVExtF2Pattern()(implicit rawInst: BitPat) extends VecArithInstPattern
// Todo: vmv.s.x treat 0 < idx < VLMAX as tail.
case class VecS2ADXPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecS2MDXPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecS2MDMPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecS2MDVPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecDVPattern()(implicit rawInst: BitPat) extends VecArithInstPattern

case class VecIntRedPattern()(implicit rawInst: BitPat) extends VecArithInstPattern
case class VecIntWRedPattern()(implicit rawInst: BitPat) extends VecArithInstPattern


trait VecInstFieldDefination {
  def func6(implicit rawInst: BitPat): BitPat = rawInst(31, 26)
  def vm(implicit rawInst: BitPat): BitPat = rawInst(25)
  def vs2(implicit rawInst: BitPat): BitPat = rawInst(24, 20)
  def vs1(implicit rawInst: BitPat): BitPat = rawInst(19, 15)
  def category(implicit rawInst: BitPat): BitPat = rawInst(14, 12)
  def vd(implicit rawInst: BitPat): BitPat = rawInst(11, 7)
}

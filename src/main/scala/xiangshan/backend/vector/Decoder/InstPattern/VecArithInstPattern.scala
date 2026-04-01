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

abstract class VecIntArithInstPattern()(
  implicit rawInst: BitPat,
) extends VecArithInstPattern

abstract class VecFpArithInstPattern()(
  implicit rawInst: BitPat,
) extends VecArithInstPattern

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
              case OPIVI => VecIntS2DVWholeMvPattern()
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
      case OPFVV | OPFVF =>
        func6.rawString match {
          // vfredusum/vfredosum/vfredmin/vfredmax
          case "000001" | "000011" | "000101" | "000111" =>
            category.rawString match {
              case OPFVV => VecFpRedPattern()
            }
          // vfadd/vfsub/vfmin/vfmax/vfsgnj/vfsgnjn/vfsgnjx/vfdiv/vfrdiv/vfmul/vfrsub
          case "000000" | "000010" | "000100" | "000110" |
               "001000" | "001001" | "001010" |
               "001110" | "001111" |
               "100000" | "100001" | "100100" | "100111" =>
            VecFpOp2VVPattern()
          // unary move/extract family around func6=010000:
          // opfvv + vs1=00000 -> vfmv.f.s, extracting one fp element from vector to scalar.
          // opfvf             -> vfmv.s.f, writing one scalar fp value into vd[0].
          case "010000" =>
            category.rawString match {
              case OPFVV =>
                vs1.rawString match {
                  case "00000" => VecFpS2APattern()
                }
              case OPFVF => VecFpS1VPattern()
            }
          // vfunary0 group. classification is refined by vs1 sub-op:
          // 01xxx -> widening fp convert from narrow source.
          // 10000/10001/10110/10111 -> narrowing from wide fp source to integer dest.
          // 1xxxx -> narrowing from wide fp source to fp dest.
          // others -> same-width unary fp op/conversion on vs2.
          case "010010" =>
            vs1.rawString match {
              case s if s.startsWith("01") => VecFpS2VVWPattern()
              case "10000" | "10001" | "10110" | "10111" => VecFpS2WVIntPattern()
              case s if s.startsWith("1") => VecFpS2WVFpPattern()
              case _ => VecFpS2VPattern()
            }
          // vfunary1 same-width single-source fp ops such as vfsqrt/vfrec7/vfrsqrt7/vfclass.
          case "010011" =>
            VecFpS2VPattern()
          // vm-dependent merge/move form:
          // vm=0 -> vfmerge.vfm behaves like a 2-operand op.
          // vm=1 -> vfmv.v.f is a scalar-to-vector move.
          case "010111" =>
            vm.rawString match {
              case "0" => VecFpOp2VVPattern()
              case "1" => VecFpS1VPattern()
            }
          // vmfeq/vmfle/vmflt/vmfne/vmfgt/vmfge
          case "011000" | "011001" | "011011" | "011100" | "011101" | "011111" =>
            VecFpOp2VMPattern()
          // vfmadd/vfnmadd/vfmsub/vfnmsub/vfmacc/vfnmacc/vfmsac/vfnmsac
          case "101000" | "101001" | "101010" | "101011" |
               "101100" | "101101" | "101110" | "101111" =>
            VecFpOp3VVVPattern()
          // vfwredusum/vfwredosum
          case "110001" | "110011" =>
            category.rawString match {
              case OPFVV => VecFpWRedPattern()
            }
          // vfwadd/vfwsub/vfwmul
          case "110000" | "110010" | "111000" =>
            VecFpOp2VVWPattern()
          // vfwadd.w/vfwsub.w
          case "110100" | "110110" =>
            VecFpOp2WVWPattern()
          // vfwmacc/vfwnmacc/vfwmsac/vfwnmsac
          case "111011" | "111100" | "111101" | "111110" | "111111" =>
            VecFpOp3VVWPattern()
          case _ => null
        }
      case _ => null
    }
  }
}

case class VecIntVVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntVVVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecIntVVMPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntMMMPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntVVWPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntVVWWPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntWVWPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecIntSatVVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntSatMulVVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecIntScaleShiftVVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntNarrowShiftWVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntClipWVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntAvgVVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecGatherVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecGatherXPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecGatherIPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecGatherEI16Pattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecCompressPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecSlideXPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecSlideIPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecSlide1Pattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecCarryPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecCarryMPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecIntS1VDVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntS1XDVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecS1XDAPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecIntS2DVWholeMvPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntS2DVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntS2DVExtF8Pattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntS2DVExtF4Pattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntS2DVExtF2Pattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
// Todo: vmv.s.x treat 0 < idx < VLMAX as tail.
case class VecS2ADXPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecS2MDXPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecS2MDMPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecS2MDVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecDVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecIntRedPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern
case class VecIntWRedPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

case class VecFpOp2VVPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpOp2VMPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpOp3VVVPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpRedPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpWRedPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpOp2VVWPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpOp2WVWPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpOp3VVWPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpS2VPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpS2VVWPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpS2WVIntPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpS2WVFpPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpS2APattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern
case class VecFpS1VPattern()(implicit rawInst: BitPat) extends VecFpArithInstPattern

case class VecCryptoVVVVPattern()(implicit rawInst: BitPat) extends VecIntArithInstPattern

trait VecInstFieldDefination {
  def func6(implicit rawInst: BitPat): BitPat = rawInst(31, 26)
  def vm(implicit rawInst: BitPat): BitPat = rawInst(25)
  def vs2(implicit rawInst: BitPat): BitPat = rawInst(24, 20)
  def vs1(implicit rawInst: BitPat): BitPat = rawInst(19, 15)
  def category(implicit rawInst: BitPat): BitPat = rawInst(14, 12)
  def vd(implicit rawInst: BitPat): BitPat = rawInst(11, 7)
}

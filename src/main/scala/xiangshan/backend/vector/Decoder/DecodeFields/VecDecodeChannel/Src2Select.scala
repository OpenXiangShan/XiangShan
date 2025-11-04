package xiangshan.backend.vector.Decoder.DecodeFields.VecDecodeChannel

import chisel3._
import chisel3.util.BitPat
import chisel3.util.experimental.decode.DecodeField
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil.{DecodePatternComb, DecodePatternComb2, RVVInstWithConfigPattern, SewPattern}
import xiangshan.backend.vector.Decoder.Sews

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

//
//  val prefixesMap: Map[Src2SelectEnum.Val, Seq[String]] = Map(
//    CONST ->  getVariableNameSeq(
//      VSETIVLI, VSETVLI, VSETVL,
//      VCPOP_M, VFIRST_M,
//      VMV_X_S,
//      VMSBF_M, VMSIF_M, VMSOF_M,
//      VFMV_F_S,
//      VZEXT_VF8,
//      VSEXT_VF8,
//    ),
//    INCF2 -> (
//      xiangshan.backend.decode.isa.instclass.RVV.vvw
//        :+ getVariableName(VFWREDOSUM_VS)
//
//    ),
//
//    INC1 -> (
//      getVariableNameSeq(
//        // 23 int
//        VADD_VV, VSUB_VV, VRSUB_VX,
//        VMINU_VV, VMIN_VV, VMAXU_VV, VMAX_VV,
//        VAND_VV, VOR_VV, VXOR_VV,
//        VADC_VVM, VSBC_VVM,
//        VMERGE_VVM,
//        VSADD_VV, VSADDU_VV, VSSUB_VV, VSSUBU_VV,
//        VSLL_VV, VSRL_VV, VSRA_VV,
//        VSSRL_VV, VSSRA_VV,
//        VSMUL_VV,
//        // 4 vmvnr
//        VMV1R_V, VMV2R_V, VMV4R_V, VMV8R_V,
//        // 4 avg
//        VAADD_VV, VAADDU_VV, VASUB_VV, VASUBU_VV,
//        //
//        VDIV_VV, VDIVU_VV, VREM_VV, VREMU_VV,
//        VMULHU_VV, VMUL_VV, VMULHSU_VV, VMULH_VV,
//        VMADD_VV, VNMSUB_VV, VMACC_VV, VNMSAC_VV,
//        VWADD_VV, VWADDU_VV, VWSUB_VV, VWSUBU_VV,
//        VWMUL_VV, VWMULU_VV, VWMULSU_VV,
//        VWMACC_VV, VWMACCU_VV, VWMACCUS_VX, VWMACCSU_VV,
//        // 0src op
//        VID_V,
//        // 1src op
//        VIOTA_M,
//        VFMV_S_F,
//        VMV_S_X,
//        // 20 fp
//        VFADD_VV, VFSUB_VV, VFRSUB_VF,
//        VFMIN_VV, VFMAX_VV,
//        VFSGNJ_VV, VFSGNJN_VV, VFSGNJX_VV,
//        VFMERGE_VFM,
//        VFDIV_VV, VFRDIV_VF, VFMUL_VV,
//        VFMADD_VV, VFNMADD_VV, VFMSUB_VV, VFNMSUB_VV, VFMACC_VV, VFNMACC_VV, VFMSAC_VV, VFNMSAC_VV,
//        // 4 1src op
//        VFCLASS_V,
//        VFREC7_V, VFRSQRT7_V,
//        VFSQRT_V,
//      ).map(_.split('_').head) ++ Seq(
//        "VMV_V_",
//        "VFMV_V_",
//        // 6 vfcvt
//        "VFCVT",
//      )
//      ),
////    SplitType.VVM -> getVariableNameSeq(
////      // 2 carry
////      VMADC_VV, VMSBC_VV,
////      // 8 int cmp
////      VMSEQ_VV, VMSNE_VV, VMSLE_VV, VMSLEU_VV, VMSGT_VX, VMSGTU_VX, VMSLT_VV, VMSLTU_VV,
////      // 6 fp cmp
////      VMFEQ_VV, VMFNE_VV, VMFLE_VV, VMFLT_VV, VMFGT_VF, VMFGE_VF,
////    ).map(_.split('_').head),
//  )
//
//  val suffixesMap: Map[Src2SelectEnum.Val, Seq[String]] = Map(
//    CONST -> Seq(
//      "_MM",
//    ),
//  )
}

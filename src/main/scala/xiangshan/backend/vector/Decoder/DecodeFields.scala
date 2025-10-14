package xiangshan.backend.vector.Decoder

import chisel3._
import chisel3.util._
import chisel3.util.experimental.decode.{BoolDecodeField, DecodeField}
import freechips.rocketchip.rocket.Instructions._
import xiangshan.SrcType
import xiangshan.backend.fu.vector.Bundles.VSew
import xiangshan.backend.vector.Decoder.DecodeChannel.SplitCtlDecoderUtil.{InstNfLmulSewPattern, UopLmulNfSplitOHPattern}
import xiangshan.backend.vector.Decoder.InstPattern._
import xiangshan.backend.vector.Decoder.RVVDecodeUtil._
import xiangshan.backend.vector.Decoder.Split.SplitTable._
import xiangshan.backend.vector.Decoder.Split.{SplitType, SplitTypeOH}
import xiangshan.backend.vector.Decoder.Uop.UopType.UopBase
import xiangshan.backend.vector.Decoder.Uop._
import xiangshan.backend.vector.util.BString._
import xiangshan.backend.vector.util.ChiselTypeExt._
import xiangshan.backend.vector.util.ScalaTypeExt._
import xiangshan.macros.InstanceNameMacro.{getVariableName, getVariableNameSeq}

import scala.collection.mutable.ArrayBuffer

object DecodeFields {
  object GpWenField extends BoolDecodeField[VecInstPattern] {
    override def name: String = "gpWen"

    override def genTable(op: VecInstPattern): BitPat = {
      if (usedNames.contains(op.name))
        y
      else
        n
    }

    val usedNames: Seq[String] = getVariableNameSeq(VCPOP_M, VFIRST_M, VMV_X_S)
  }

  object FpWenField extends BoolDecodeField[VecInstPattern] {
    override def name: String = "fpWen"

    override def genTable(op: VecInstPattern): BitPat = {
      if (usedNames.contains(op.name))
        y
      else
        n
    }

    val usedNames: Seq[String] = Seq(getVariableName(VFMV_F_S))
  }

  object VpWenField extends BoolDecodeField[VecInstPattern] {
    override def name: String = "vpWen"

    override def genTable(op: VecInstPattern): BitPat = {
      op match {
        case pattern: VecMemInstPattern if pattern.isLoad =>
          y
        case pattern: VecArithInstPattern if !(GpWenField.usedNames ++ FpWenField.usedNames).contains(pattern.name) =>
          y
        case pattern: VecArithInstPattern =>
          n
        case pattern: VecMemInstPattern =>
          n
        case _: VecConfigInstPattern =>
          n
      }
    }
  }

  object Src1TypeField extends DecodeField[VecInstPattern, UInt] {
    override def name: String = "src1Type"

    override def chiselType: UInt = SrcType()

    override def genTable(op: VecInstPattern): BitPat = {
      op match {
        case VecArithInstPattern() => genTable(op.asInstanceOf[VecArithInstPattern])
        case VecConfigInstPattern() => genTable(op.asInstanceOf[VecConfigInstPattern])
        case VecMemInstPattern() => genTable(op.asInstanceOf[VecMemInstPattern])
      }
    }

    def genTable(op: VecArithInstPattern): BitPat = {
      var isImm = false
      var isGp = false
      var isVp = false
      var isFp = false
      var isNo = false
      var res: BitPat = BitPat.N()
      if (op.name.endsWithThese(immSuffixes)) {
        isImm = true
        res = SrcType.imm.toFixWidthBitPat(4)
      }
      if (op.name.endsWithThese(gpSuffixes)) {
        isGp = true
        res = SrcType.xp.toFixWidthBitPat(4)
      }
      if (op.name.endsWithThese(noSuffixes) || op.name.startsWithThese(noPrefixes)) {
        isNo = true
        res = SrcType.no.toFixWidthBitPat(4)
      }
      if (op.name.endsWithThese(vpSuffixes)) {
        isVp = true
        res = SrcType.vp.toFixWidthBitPat(4)
      }
      if (op.name.endsWithThese(fpSuffixes)) {
        isFp = true
        res = SrcType.fp.toFixWidthBitPat(4)
      }
      require(Seq(isImm, isGp, isFp, isVp, isNo).map(x => if (x) 1 else 0).sum == 1,
        s"The inst ${op.name} not correct decoded, " +
        s"isImm=$isImm, isGp=$isGp, isVp=$isVp, isFp=$isFp, isNo=$isNo"
      )
      res
    }

    def genTable(op: VecMemInstPattern): BitPat = {
      SrcType.xp.toFixWidthBitPat(4)
    }

    def genTable(op: VecConfigInstPattern): BitPat = {
      if (getVariableNameSeq(VSETVLI, VSETVL).contains(op.name))
        SrcType.xp.toFixWidthBitPat(4)
      else
        SrcType.imm.toFixWidthBitPat(4)
    }


    val immSuffixes = Seq(
      "_VI",
      "_VIM",
      "_WI",
      "_V_I",
    )

    val gpSuffixes = Seq(
      "_VX",
      "_VXM",
      "_WX",
      "_S_X",
      "_V_X",
    )

    val vpSuffixes = Seq(
      "_VV",
      "_VVM",
      "_WV",
      "_VS",
      "_MM",
      "_V_V",
      "_VM",
    )

    val noSuffixes = Seq(
      "R_V",
      "_X_S",
      "_F_S",
      "_M",
      "_VF2",
      "_VF4",
      "_VF8",
      "VID_V",
    )

    val fpSuffixes = Seq(
      "_VF",
      "_S_F",
      "_V_F",
      "_VFM",
      "_WF",
    )

    val noPrefixes = Seq(
      "VFCVT",
      "VFNCVT",
      "VFWCVT",
      "VFCLASS",
      "VFREC7",
      "VFRSQRT7",
      "VFSQRT",
    )
  }

  object Src2TypeField extends DecodeField[VecInstPattern, UInt] {
    override def name: String = "src2Type"

    override def chiselType: UInt = SrcType()

    override def genTable(op: VecInstPattern): BitPat = {
      if (src2NoInsts.contains(op.name))
        SrcType.no.toFixWidthBitPat(4)
      else
        SrcType.vp.toFixWidthBitPat(4)
    }

    val src2NoInsts: Seq[String] = getVariableNameSeq(
      VMV_V_I, VMV_V_V, VMV_V_X,
      VID_V,
      VMV_S_X,
      VFMV_S_F, VFMV_V_F,
    )
  }

  object AlwaysReadVdField extends BoolDecodeField[VecInstPattern] {
    override def name: String = "alwaysReadVd"

    override def genTable(op: VecInstPattern): BitPat = {
      if (op.name.startsWithThese(vdRenInstPrefixes))
        y
      else
        n
    }

    val vdRenInstPrefixes: Seq[String] = getVariableNameSeq(
      VMACC_VV, VNMSAC_VV,
      VMADD_VV, VNMSUB_VV,
      VWMACC_VV,
      VFMACC_VV, VFNMACC_VV,
      VFMACC_VV, VFNMSAC_VV,
      VFMADD_VV, VFNMADD_VV,
      VFMSUB_VV, VFNMSUB_VV,
      VFWMACC_VV, VFWNMACC_VV,
      VFWMSAC_VV, VFWNMSAC_VV,
    ).map(_.split('_').head)
  }

  object VdEew1bField extends BoolDecodeField[VecInstPattern] {
    override def name: String = "vdEew1b"

    override def genTable(op: VecInstPattern): BitPat = {
      if (op.name.startsWithThese(prefixes) || op.name.endsWithThese(suffixes))
        y
      else
        n
    }

    val prefixes: Seq[String] = getVariableNameSeq(
      VMADC_VV, VMSBC_VV,
      VMSEQ_VV, VMSNE_VV, VMSLE_VV, VMSLEU_VV, VMSGT_VX, VMSGTU_VX, VMSLT_VV, VMSLTU_VV,
      VMSBF_M, VMSIF_M, VMSOF_M,
      VLM_V,
    ).map(_.split('_').head) ++ Seq(
      "VMF",
    )

    val suffixes: Seq[String] = Seq(
      "_MM",
    )
  }

  object VdWidenField extends BoolDecodeField[VecInstPattern] {
    override def name: String = "vdWiden"

    override def genTable(op: VecInstPattern): BitPat = {
      if (op.name.startsWithThese("VW", "VFW"))
        y
      else
        n
    }
  }

  object Vs2WidenField extends BoolDecodeField[VecInstPattern] {
    override def name: String = "vs2Widen"

    override def genTable(op: VecInstPattern): BitPat = {

      if (op.name.endsWithThese("_WV", "_WX", "_WI", "_WF", "_F_W", "_X_W", "_XU_W"))
        y
      else
        n
    }
  }

  object Src12RevField extends BoolDecodeField[VecInstPattern] {
    override def name: String = "src12Rev"

    override def genTable(op: VecInstPattern): BitPat = {
      if (revInsts.contains(op.name))
        y
      else
        n
    }

    val revInsts = getVariableNameSeq(
      VRSUB_VX,
      VRSUB_VI,
      VFRDIV_VF,
      VFRSUB_VF,
    )
  }

  /**
   * This field is used to specify if the uop write narrow mask, like vmseq.vv. <br/>
   * The mask operation uops are excluded, since they write whole register.
   */
  object NarrowMaskWenField extends BoolDecodeField[VecInstPattern] {
    override def name: String = "narrowMaskWen"

    override def genTable(op: VecInstPattern): BitPat = {
      if (op.name.startsWithThese(instPrefixes))
        y
      else
        n
    }

    val instPrefixes = Seq(
      "VMADC", "VMSBC",
      "VMS",
      "VMF",
    )
  }

  object VxsatWenField extends BoolDecodeField[VecInstPattern] {
    override def name: String = "vxsatWen"

    override def genTable(op: VecInstPattern): BitPat = {
      if (op.name.startsWithThese(instPrefixes))
        y
      else
        n
    }

    val instPrefixes = getVariableNameSeq(
      VSADD_VV,
      VSADDU_VV,
      VSSUB_VV,
      VSSUBU_VV,
      VNCLIP_WV,
      VNCLIPU_WV,
      VSMUL_VV,
    ).map(_.split('_').head)
  }

  object EewField extends DecodeField[VecInstPattern, UInt] {

    override def name: String = "eew"

    override def chiselType: UInt = VSew()

    override def genTable(op: VecInstPattern): BitPat = {
      val eew: UInt = op match {
        case x: VecMemInstPattern if x.mew.rawString == "0" =>
          x.width.rawString match {
            case "000" => VSew.e8
            case "101" => VSew.e16
            case "110" => VSew.e32
            case "111" => VSew.e64
          }
      }

      eew.toBitPat
    }
  }

  object SplitTypeField extends DecodeField[RVVInstWithConfigPattern, SplitType.Type] {
    override def name: String = "splitType"

    override def chiselType: SplitType.Type = SplitType()

    override def genTable(op: RVVInstWithConfigPattern): BitPat = {
      var splitType: SplitType.Type = SplitType()

      if (
        op.name.startsWithThese(prefixesMap(SplitType.NONE)) ||
        op.name.endsWithThese(suffixesMap(SplitType.NONE))
      ) {
        splitType = SplitType.NONE
      }
      else if (op.name.startsWithThese(prefixesMap(SplitType.VVV))) {
        splitType = SplitType.VVV
      }
      else if (op.name.startsWithThese(prefixesMap(SplitType.VVM))) {
        splitType = SplitType.VVM
      }
      else if (
        op.name.startsWithThese("VN") && op.name.endsWithThese("_WX", "_WI", "_WV") ||
          op.name.startsWithThese("VFNCVT")
      ) {
        splitType = SplitType.WVV
      }
      else if (
        op.name.startsWithThese("VW") && op.name.endsWithThese("_WX", "_WV") ||
        op.name.startsWithThese("VFW") && op.name.endsWithThese("_WF", "_WV")
      ) {
        splitType = SplitType.WVW
      }
      else if (
        op.name.startsWithThese("VW") && op.name.endsWithThese("_VX", "_VV") ||
        op.name.endsWithThese("_VF2") ||
        op.name.startsWithThese("VFW") && op.name.endsWithThese("_VF", "_VV") ||
        op.name.startsWithThese("VFWCVT")
      ) {
        splitType = SplitType.VVW
      }
      else if (op.name.endsWith("_VF4")) {
        splitType = SplitType.EXT4
      }
      else if (op.name.endsWith("_VF8")) {
        splitType = SplitType.EXT8
      }
      else if (op.name.startsWithThese("VRED", "VFREDU", "VFREDM")) {
        splitType = SplitType.VREDU
      }
      else if (op.name.startsWithThese("VFREDO")) {
        splitType = SplitType.VREDO
      }
      else if (op.name.startsWithThese("VFWREDU", "VWRED")) {
        splitType = SplitType.VWREDU
      }
      else if (op.name.startsWithThese("VFWREDO")) {
        splitType = SplitType.VWREDO
      }
      else if (op.name.startsWithThese("VSLIDEUP", "VSLIDE1UP", "VFSLIDE1UP")) {
        splitType = SplitType.SHUFFLE_SLIDE_UP
      }
      else if (op.name.startsWithThese("VSLIDEDOWN", "VSLIDE1DOWN", "VFSLIDE1DOWN", "VCOMPRESS")) {
        splitType = SplitType.SHUFFLE_COMPRESS_DOWN
      }
      else if (op.name.startsWithThese("VRGATHER_V")) {
        splitType = SplitType.SHUFFLE_GATHER
      }
      else if (op.name == getVariableName(VRGATHEREI16_VV)) {
        val sew: Double = Sews.decodeValue(op.sew)
        val iEew: Double = 16
        val dEew: Double = sew
        val diRatio = dEew / iEew
        splitType = diRatio match {
          case 4   => SplitType.VLSIDX_DI_RATIO_4
          case 2   => SplitType.VLSIDX_DI_RATIO_2
          case 1   => SplitType.VLSIDX_DI_RATIO_1
          case 0.5 => SplitType.VLSIDX_DI_RATIO_F2
        }
      }
      else op.instPattern match {
        case instPattern: VecMemInstPattern if instPattern.mop(0).rawString == "1" =>
          val eew: Double = instPattern.eewValue
          val sew: Double = Sews.decodeValue(op.sew)

          val dEew = sew
          val iEew = eew
          val diRatio = dEew / iEew
          splitType = diRatio match {
            case 8      => SplitType.VLSIDX_DI_RATIO_8
            case 4      => SplitType.VLSIDX_DI_RATIO_4
            case 2      => SplitType.VLSIDX_DI_RATIO_2
            case 1      => SplitType.VLSIDX_DI_RATIO_1
            case 0.5    => SplitType.VLSIDX_DI_RATIO_F2
            case 0.25   => SplitType.VLSIDX_DI_RATIO_F4
            case 0.125  => SplitType.VLSIDX_DI_RATIO_F8
          }
        case inst: VecMemInstPattern if inst.mop.rawString == "00" && inst.lumop.rawString == "10000" =>
          splitType = SplitType.VLSNONIDXFF
        case inst: VecMemInstPattern if inst.mop.rawString == "00" && inst.lumop.rawString == "01011" =>
          splitType = SplitType.NONE
        case inst: VecMemInstPattern =>
          splitType = SplitType.VLSNONIDX
      }
      splitType.toBitPat
    }


    val prefixesMap: Map[SplitType.Type, Seq[String]] = Map(
      SplitType.NONE -> getVariableNameSeq(
        VSETIVLI, VSETVLI, VSETVL,
        VCPOP_M, VFIRST_M,
        VMV_X_S,
        VMSBF_M, VMSIF_M, VMSOF_M,
        VFMV_F_S,
      ),
      SplitType.VVV -> (
        getVariableNameSeq(
          // 23 int
          VADD_VV, VSUB_VV, VRSUB_VX,
          VMINU_VV, VMIN_VV, VMAXU_VV, VMAX_VV,
          VAND_VV, VOR_VV, VXOR_VV,
          VADC_VVM, VSBC_VVM,
          VMERGE_VVM,
          VSADD_VV, VSADDU_VV, VSSUB_VV, VSSUBU_VV,
          VSLL_VV, VSRL_VV, VSRA_VV,
          VSSRL_VV, VSSRA_VV,
          VSMUL_VV,
          // 4 vmvnr
          VMV1R_V, VMV2R_V, VMV4R_V, VMV8R_V,
          // 4 avg
          VAADD_VV, VAADDU_VV, VASUB_VV, VASUBU_VV,
          //
          VDIV_VV, VDIVU_VV, VREM_VV, VREMU_VV,
          VMULHU_VV, VMUL_VV, VMULHSU_VV, VMULH_VV,
          VMADD_VV, VNMSUB_VV, VMACC_VV, VNMSAC_VV,
          VWADD_VV, VWADDU_VV, VWSUB_VV, VWSUBU_VV,
          VWMUL_VV, VWMULU_VV, VWMULSU_VV,
          VWMACC_VV, VWMACCU_VV, VWMACCUS_VX, VWMACCSU_VV,
          // 0src op
          VID_V,
          // 1src op
          VIOTA_M,
          VFMV_S_F,
          VMV_S_X,
          // 20 fp
          VFADD_VV, VFSUB_VV, VFRSUB_VF,
          VFMIN_VV, VFMAX_VV,
          VFSGNJ_VV, VFSGNJN_VV, VFSGNJX_VV,
          VFMERGE_VFM,
          VFDIV_VV, VFRDIV_VF, VFMUL_VV,
          VFMADD_VV, VFNMADD_VV, VFMSUB_VV, VFNMSUB_VV, VFMACC_VV, VFNMACC_VV, VFMSAC_VV, VFNMSAC_VV,
          // 4 1src op
          VFCLASS_V,
          VFREC7_V, VFRSQRT7_V,
          VFSQRT_V,
        ).map(_.split('_').head) ++ Seq(
          "VMV_V_",
          "VFMV_V_",
          // 6 vfcvt
          "VFCVT",
        )
      ),
      SplitType.VVM -> getVariableNameSeq(
        // 2 carry
        VMADC_VV, VMSBC_VV,
        // 8 int cmp
        VMSEQ_VV, VMSNE_VV, VMSLE_VV, VMSLEU_VV, VMSGT_VX, VMSGTU_VX, VMSLT_VV, VMSLTU_VV,
        // 6 fp cmp
        VMFEQ_VV, VMFNE_VV, VMFLE_VV, VMFLT_VV, VMFGT_VF, VMFGE_VF,
      ).map(_.split('_').head),
    )

    val suffixesMap: Map[SplitType.Type, Seq[String]] = Map(
      SplitType.NONE -> Seq(
        "_MM",
      ),
    )
  }

  object SplitTypeOHField extends DecodeField[RVVInstWithConfigPattern, SplitTypeOH.Type] {
    override def name: String = "splitTypeOH"

    override def chiselType: SplitTypeOH.Type = SplitTypeOH()

    override def genTable(op: RVVInstWithConfigPattern): BitPat = {
      var splitType: SplitType.Type = SplitType()

      if (
        op.name.startsWithThese(prefixesMap(SplitType.NONE)) ||
          op.name.endsWithThese(suffixesMap(SplitType.NONE))
      ) {
        splitType = SplitType.NONE
      }
      else if (op.name.startsWithThese(prefixesMap(SplitType.VVV))) {
        splitType = SplitType.VVV
      }
      else if (op.name.startsWithThese(prefixesMap(SplitType.VVM))) {
        splitType = SplitType.VVM
      }
      else if (
        op.name.startsWithThese("VN") && op.name.endsWithThese("_WX", "_WI", "_WV") ||
          op.name.startsWithThese("VFNCVT")
      ) {
        splitType = SplitType.WVV
      }
      else if (
        op.name.startsWithThese("VW") && op.name.endsWithThese("_WX", "_WV") ||
          op.name.startsWithThese("VFW") && op.name.endsWithThese("_WF", "_WV")
      ) {
        splitType = SplitType.WVW
      }
      else if (
        op.name.startsWithThese("VW") && op.name.endsWithThese("_VX", "_VV") ||
          op.name.endsWithThese("_VF2") ||
          op.name.startsWithThese("VFW") && op.name.endsWithThese("_VF", "_VV") ||
          op.name.startsWithThese("VFWCVT")
      ) {
        splitType = SplitType.VVW
      }
      else if (op.name.endsWith("_VF4")) {
        splitType = SplitType.EXT4
      }
      else if (op.name.endsWith("_VF8")) {
        splitType = SplitType.EXT8
      }
      else if (op.name.startsWithThese("VRED", "VFREDU", "VFREDM")) {
        splitType = SplitType.VREDU
      }
      else if (op.name.startsWithThese("VFREDO")) {
        splitType = SplitType.VREDO
      }
      else if (op.name.startsWithThese("VFWREDU", "VWRED")) {
        splitType = SplitType.VWREDU
      }
      else if (op.name.startsWithThese("VFWREDO")) {
        splitType = SplitType.VWREDO
      }
      else if (op.name.startsWithThese("VSLIDEUP", "VSLIDE1UP", "VFSLIDE1UP")) {
        splitType = SplitType.SHUFFLE_SLIDE_UP
      }
      else if (op.name.startsWithThese("VSLIDEDOWN", "VSLIDE1DOWN", "VFSLIDE1DOWN", "VCOMPRESS")) {
        splitType = SplitType.SHUFFLE_COMPRESS_DOWN
      }
      else if (op.name.startsWithThese("VRGATHER_V")) {
        splitType = SplitType.SHUFFLE_GATHER
      }
      else if (op.name == getVariableName(VRGATHEREI16_VV)) {
        val sew: Double = Sews.decodeValue(op.sew)
        val iEew: Double = 16
        val dEew: Double = sew
        val diRatio = dEew / iEew
        splitType = diRatio match {
          case 4   => SplitType.VLSIDX_DI_RATIO_4
          case 2   => SplitType.VLSIDX_DI_RATIO_2
          case 1   => SplitType.VLSIDX_DI_RATIO_1
          case 0.5 => SplitType.VLSIDX_DI_RATIO_F2
        }
      }
      else op.instPattern match {
        case instPattern: VecMemInstPattern if instPattern.mop(0).rawString == "1" =>
          val eew: Double = instPattern.eewValue
          val sew: Double = Sews.decodeValue(op.sew)

          val dEew = sew
          val iEew = eew
          val diRatio = dEew / iEew
          splitType = diRatio match {
            case 8      => SplitType.VLSIDX_DI_RATIO_8
            case 4      => SplitType.VLSIDX_DI_RATIO_4
            case 2      => SplitType.VLSIDX_DI_RATIO_2
            case 1      => SplitType.VLSIDX_DI_RATIO_1
            case 0.5    => SplitType.VLSIDX_DI_RATIO_F2
            case 0.25   => SplitType.VLSIDX_DI_RATIO_F4
            case 0.125  => SplitType.VLSIDX_DI_RATIO_F8
          }
        case inst: VecMemInstPattern if inst.mop.rawString == "00" && inst.lumop.rawString == "10000" =>
          splitType = SplitType.VLSNONIDXFF
        case inst: VecMemInstPattern if inst.mop.rawString == "00" && inst.lumop.rawString == "01011" =>
          splitType = SplitType.NONE
        case inst: VecMemInstPattern =>
          splitType = SplitType.VLSNONIDX
      }
      SplitTypeOH(splitType).toBitPat
    }


    val prefixesMap: Map[SplitType.Type, Seq[String]] = Map(
      SplitType.NONE -> getVariableNameSeq(
        VSETIVLI, VSETVLI, VSETVL,
        VCPOP_M, VFIRST_M,
        VMV_X_S,
        VMSBF_M, VMSIF_M, VMSOF_M,
        VFMV_F_S,
      ),
      SplitType.VVV -> (
        getVariableNameSeq(
          // 23 int
          VADD_VV, VSUB_VV, VRSUB_VX,
          VMINU_VV, VMIN_VV, VMAXU_VV, VMAX_VV,
          VAND_VV, VOR_VV, VXOR_VV,
          VADC_VVM, VSBC_VVM,
          VMERGE_VVM,
          VSADD_VV, VSADDU_VV, VSSUB_VV, VSSUBU_VV,
          VSLL_VV, VSRL_VV, VSRA_VV,
          VSSRL_VV, VSSRA_VV,
          VSMUL_VV,
          // 4 vmvnr
          VMV1R_V, VMV2R_V, VMV4R_V, VMV8R_V,
          // 4 avg
          VAADD_VV, VAADDU_VV, VASUB_VV, VASUBU_VV,
          //
          VDIV_VV, VDIVU_VV, VREM_VV, VREMU_VV,
          VMULHU_VV, VMUL_VV, VMULHSU_VV, VMULH_VV,
          VMADD_VV, VNMSUB_VV, VMACC_VV, VNMSAC_VV,
          VWADD_VV, VWADDU_VV, VWSUB_VV, VWSUBU_VV,
          VWMUL_VV, VWMULU_VV, VWMULSU_VV,
          VWMACC_VV, VWMACCU_VV, VWMACCUS_VX, VWMACCSU_VV,
          // 0src op
          VID_V,
          // 1src op
          VIOTA_M,
          VFMV_S_F,
          VMV_S_X,
          // 20 fp
          VFADD_VV, VFSUB_VV, VFRSUB_VF,
          VFMIN_VV, VFMAX_VV,
          VFSGNJ_VV, VFSGNJN_VV, VFSGNJX_VV,
          VFMERGE_VFM,
          VFDIV_VV, VFRDIV_VF, VFMUL_VV,
          VFMADD_VV, VFNMADD_VV, VFMSUB_VV, VFNMSUB_VV, VFMACC_VV, VFNMACC_VV, VFMSAC_VV, VFNMSAC_VV,
          // 4 1src op
          VFCLASS_V,
          VFREC7_V, VFRSQRT7_V,
          VFSQRT_V,
        ).map(_.split('_').head) ++ Seq(
          "VMV_V_",
          "VFMV_V_",
          // 6 vfcvt
          "VFCVT",
        )
        ),
      SplitType.VVM -> getVariableNameSeq(
        // 2 carry
        VMADC_VV, VMSBC_VV,
        // 8 int cmp
        VMSEQ_VV, VMSNE_VV, VMSLE_VV, VMSLEU_VV, VMSGT_VX, VMSGTU_VX, VMSLT_VV, VMSLTU_VV,
        // 6 fp cmp
        VMFEQ_VV, VMFNE_VV, VMFLE_VV, VMFLT_VV, VMFGT_VF, VMFGE_VF,
      ).map(_.split('_').head),
    )

    val suffixesMap: Map[SplitType.Type, Seq[String]] = Map(
      SplitType.NONE -> Seq(
        "_MM",
      ),
    )
  }

  object SrcSplitSelectField extends DecodeField[UopLmulNfSplitOHPattern, Vec[SrcDestSelect]] {
    override def name: String = "srcSplitSelect"

    override def chiselType: Vec[SrcDestSelect] = Vec(8, new SrcDestSelect)

    override def genTable(op: UopLmulNfSplitOHPattern): BitPat = {
      import xiangshan.backend.vector.Decoder.RVVDecodeUtil.SrcNumSelect._

      val UopLmulNfSplitOHPattern(lmulP, nfP, splitTypeOHP) = op

      //      val splitTypeValue: Int = op.splitType.value.toInt
      //      val lmulValue: Double = Lmuls.decodeValue(op.lmul)
      val splitTypeValue: Int = splitTypeOHP.value
      val lmulValue: Double = lmulP.lmulValue

      val uops: ArrayBuffer[BitPat] = ArrayBuffer.fill(8)(BitPat.dontCare(SrcDestSelect.width))

      if (splitTypeValue == SplitType.NONE.litValue) {
        uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
      }
      else if (splitTypeValue == SplitType.VVV.litValue) {
        for (i <- 0 until (1.0 max lmulValue).toInt) {
          uops(i) = genBitPat(_.VS2, i) ## genBitPat(_.VS1, i) ## genBitPat(_.VD, i)
        }
      }
      else if (splitTypeValue == SplitType.VVM.litValue) {
        for (i <- 0 until (1.0 max lmulValue).toInt) {
          uops(i) = genBitPat(_.VS2, i) ## genBitPat(_.VS1, i) ## genBitPat(_.VD, 0)
        }
      }
      else if (splitTypeValue == SplitType.WVV.litValue) {
        if (lmulValue * 2 <= 8) {
          for (i <- 0 until (1.0 max (lmulValue * 2)).toInt) {
            uops(i) = genBitPat(_.VS2, i) ## genBitPat(_.VS1, i / 2) ## genBitPat(_.VD, i / 2)
          }
        }
      }
      else if (splitTypeValue == SplitType.WVW.litValue) {
        if (lmulValue * 2 <= 8) {
          for (i <- 0 until (1.0 max (lmulValue * 2)).toInt) {
            uops(i) = genBitPat(_.VS2, i) ## genBitPat(_.VS1, i / 2) ## genBitPat(_.VD, i)
          }
        }
      }
      else if (splitTypeValue == SplitType.VVW.litValue) {
        if (lmulValue * 2 <= 8) {
          for (i <- 0 until (1.0 max (lmulValue * 2)).toInt) {
            uops(i) = genBitPat(_.VS2, i / 2) ## genBitPat(_.VS1, i / 2) ## genBitPat(_.VD, i)
          }
        }
      }
      else if (splitTypeValue == SplitType.EXT4.litValue) {
        if (lmulValue <= 8) {
          for (i <- 0 until (1.0 max lmulValue).toInt) {
            uops(i) = genBitPat(_.VS2, i / 4) ## genBitPat(_.NO, 0) ## genBitPat(_.VD, i)
          }
        }
      }
      else if (splitTypeValue == SplitType.EXT8.litValue) {
        if (lmulValue <= 8) {
          for (i <- 0 until (1.0 max lmulValue).toInt) {
            uops(i) = genBitPat(_.VS2, i / 8) ## genBitPat(_.NO, 0) ## genBitPat(_.VD, i)
          }
        }
      }
      else if (splitTypeValue == SplitType.VREDU.litValue) {
        lmulValue match {
          case 8 =>
            //       src2                   src1                   dest
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS2, 7) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VS2, 1) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(2) = genBitPat(_.VS2, 2) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(3) = genBitPat(_.VS2, 3) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(4) = genBitPat(_.VS2, 4) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(5) = genBitPat(_.VS2, 5) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(6) = genBitPat(_.VS2, 6) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(7) = genBitPat(_.VD , 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
          case 4 =>
            //       src2                   src1                   dest
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS2, 3) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VS2, 1) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(2) = genBitPat(_.VS2, 2) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(3) = genBitPat(_.VD , 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
          case 2 =>
            //       src2                   src1                   dest
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS2, 1) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VD , 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
          case _ =>
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
        }
      }
      else if (splitTypeValue == SplitType.VREDO.litValue) {
        lmulValue match {
          case 8 =>
            //       src2                   src1                   dest
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VS2, 1) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(2) = genBitPat(_.VS2, 2) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(3) = genBitPat(_.VS2, 3) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(4) = genBitPat(_.VS2, 4) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(5) = genBitPat(_.VS2, 5) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(6) = genBitPat(_.VS2, 6) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(7) = genBitPat(_.VS2, 7) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
          case 4 =>
            //       src2                   src1                   dest
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VS2, 1) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(2) = genBitPat(_.VS2, 2) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(3) = genBitPat(_.VS2, 3) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
          case 2 =>
            //       src2                   src1                   dest
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VS2, 1) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
          case _ =>
            //       src2                   src1                   dest
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
        }
      }
      else if (splitTypeValue == SplitType.VWREDU.litValue) {
        lmulValue match {
          case 8 =>
            // illegal
          case 4 =>
            //        src2                   src1                   dest
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS2, 3) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VS2, 0) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(2) = genBitPat(_.VS2, 1) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(3) = genBitPat(_.VS2, 1) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(4) = genBitPat(_.VS2, 2) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(5) = genBitPat(_.VS2, 2) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(6) = genBitPat(_.VS2, 3) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(7) = genBitPat(_.VD , 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
          case 2 =>
            //        src2                   src1                   dest
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS2, 1) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VS2, 0) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(2) = genBitPat(_.VS2, 1) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(3) = genBitPat(_.VD , 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
          case x if x <= 1 =>
            //       src2                   src1                   dest
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS2, 0) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VD , 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
        }
      }
      else if (splitTypeValue == SplitType.VWREDO.litValue) {
        lmulValue match {
          case 8 =>
            // illegal
          case 4 =>
            //       src2                   src1                   dest
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VS2, 0) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(2) = genBitPat(_.VS2, 1) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(3) = genBitPat(_.VS2, 1) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(4) = genBitPat(_.VS2, 2) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(5) = genBitPat(_.VS2, 2) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(6) = genBitPat(_.VS2, 3) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(7) = genBitPat(_.VS2, 3) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
          case 2 =>
            //       src2                   src1                   dest
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VS2, 0) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(2) = genBitPat(_.VS2, 1) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
            uops(3) = genBitPat(_.VS2, 1) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
          case x if x <= 1 =>
            //       src2                   src1                   dest
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VS2, 0) ## genBitPat(_.VD , 0) ## genBitPat(_.VD, 0)
        }
      }
      else if (splitTypeValue == SplitType.SHUFFLE_SLIDE_UP.litValue) {
        for (i <- 0 until (1.0 max lmulValue).toInt) {
          uops(i) = genBitPat(_.VS2, i) ## genBitPat(_.NO, 0) ## genBitPat(_.VD, i)
        }
      }
      else if (splitTypeValue == SplitType.SHUFFLE_COMPRESS_DOWN.litValue) {
        for (i <- 0 until (1.0 max lmulValue).toInt) {
          uops(i) = genBitPat(_.VS2, i) ## genBitPat(_.NO, 0) ## genBitPat(_.VD, i)
        }
      }
      else if (splitTypeValue == SplitType.SHUFFLE_GATHER.litValue) {
        lmulValue match {
          case 8 =>
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS2, 4) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VS2, 1) ## genBitPat(_.VS2, 5) ## genBitPat(_.VD, 1)
            uops(2) = genBitPat(_.VS2, 2) ## genBitPat(_.VS2, 6) ## genBitPat(_.VD, 2)
            uops(3) = genBitPat(_.VS2, 3) ## genBitPat(_.VS2, 7) ## genBitPat(_.VD, 3)
            uops(4) = genBitPat(_.VS1, 0) ## genBitPat(_.VS1, 4) ## genBitPat(_.VD, 4)
            uops(5) = genBitPat(_.VS1, 1) ## genBitPat(_.VS1, 5) ## genBitPat(_.VD, 5)
            uops(6) = genBitPat(_.VS1, 2) ## genBitPat(_.VS1, 6) ## genBitPat(_.VD, 6)
            uops(7) = genBitPat(_.VS1, 3) ## genBitPat(_.VS1, 7) ## genBitPat(_.VD, 7)
          case 4 =>
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS2, 2) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VS2, 1) ## genBitPat(_.VS2, 3) ## genBitPat(_.VD, 1)
            uops(2) = genBitPat(_.VS1, 0) ## genBitPat(_.VS1, 2) ## genBitPat(_.VD, 2)
            uops(3) = genBitPat(_.VS1, 1) ## genBitPat(_.VS1, 3) ## genBitPat(_.VD, 3)
          case 2 =>
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS2, 1) ## genBitPat(_.VD, 0)
            uops(1) = genBitPat(_.VS1, 0) ## genBitPat(_.VS1, 1) ## genBitPat(_.VD, 1)
          case _ =>
            uops(0) = genBitPat(_.VS2, 0) ## genBitPat(_.VS1, 0) ## genBitPat(_.VD, 0)
        }
        for (i <- 0 until (1.0 max lmulValue).toInt) {
          uops(i) = genBitPat(_.VS2, i) ## genBitPat(_.NO, 0) ## genBitPat(_.VD, i)
        }
      }
      else if (splitTypeValue == SplitType.VLSIDX_DI_RATIO_1.litValue) {
        val (vs2Step, vdStep) = getVs2VdStep(1)
        val numUop = getNumUop(1)
        if (numUop <= 8) {
          for (i <- 0 until (1 max numUop)) {
            uops(i) = genBitPat(_.VS2, math.floor(i * vs2Step).toInt) ## genBitPat(_.NO, 0) ## genBitPat(_.VD, math.floor(i * vdStep).toInt)
          }
        }
      }
      else if (splitTypeValue == SplitType.VLSIDX_DI_RATIO_2.litValue) {
        val (vs2Step, vdStep) = getVs2VdStep(2)
        val numUop = getNumUop(2)
        if (numUop <= 8) {
          for (i <- 0 until (1 max numUop)) {
            uops(i) = genBitPat(_.VS2, math.floor(i * vs2Step).toInt) ## genBitPat(_.NO, 0) ## genBitPat(_.VD, math.floor(i * vdStep).toInt)
          }
        }
      }
      else if (splitTypeValue == SplitType.VLSIDX_DI_RATIO_4.litValue) {
        val (vs2Step, vdStep) = getVs2VdStep(4)
        val numUop = getNumUop(4)
        if (numUop <= 8) {
          for (i <- 0 until (1 max numUop)) {
            uops(i) = genBitPat(_.VS2, math.floor(i * vs2Step).toInt) ## genBitPat(_.NO, 0) ## genBitPat(_.VD, math.floor(i * vdStep).toInt)
          }
        }
      }
      else if (splitTypeValue == SplitType.VLSIDX_DI_RATIO_8.litValue) {
        val (vs2Step, vdStep) = getVs2VdStep(8)
        val numUop = getNumUop(8)
        if (numUop <= 8) {
          for (i <- 0 until (1 max numUop)) {
            uops(i) = genBitPat(_.VS2, math.floor(i * vs2Step).toInt) ## genBitPat(_.NO, 0) ## genBitPat(_.VD, math.floor(i * vdStep).toInt)
          }
        }
      }
      else if (splitTypeValue == SplitType.VLSIDX_DI_RATIO_F8.litValue) {
        val (vs2Step, vdStep) = getVs2VdStep(0.125)
        val numUop = getNumUop(0.125)
        if (numUop <= 8) {
          for (i <- 0 until (1 max numUop)) {
            uops(i) = genBitPat(_.VS2, math.floor(i * vs2Step).toInt) ## genBitPat(_.NO, 0) ## genBitPat(_.VD, math.floor(i * vdStep).toInt)
          }
        }
      }
      else if (splitTypeValue == SplitType.VLSIDX_DI_RATIO_F4.litValue) {
        val (vs2Step, vdStep) = getVs2VdStep(0.25)
        val numUop = getNumUop(0.25)
        if (numUop <= 8) {
          for (i <- 0 until (1 max numUop)) {
            uops(i) = genBitPat(_.VS2, math.floor(i * vs2Step).toInt) ## genBitPat(_.NO, 0) ## genBitPat(_.VD, math.floor(i * vdStep).toInt)
          }
        }
      }
      else if (splitTypeValue == SplitType.VLSIDX_DI_RATIO_F2.litValue) {
        val (vs2Step, vdStep) = getVs2VdStep(0.5)
        val numUop = getNumUop(0.5)
        if (numUop <= 8) {
          for (i <- 0 until (1 max numUop)) {
            uops(i) = genBitPat(_.VS2, math.floor(i * vs2Step).toInt) ## genBitPat(_.NO, 0) ## genBitPat(_.VD, math.floor(i * vdStep).toInt)
          }
        }
      }
      else if (
        splitTypeValue == SplitType.VLSNONIDX.litValue ||
        splitTypeValue == SplitType.VLSNONIDXFF.litValue
      ) {
        for (i <- 0 until (1.0 max lmulValue).toInt) {
          uops(i) = genBitPat(_.VS2, i) ## genBitPat(_.NO, 0) ## genBitPat(_.VD, i)
        }
      }

      def getVs2VdStep(diRatio: Double): (Double, Double) = {
        val seg = nfP.segNum
        val vs2Step: Double = if (diRatio < 1) 1.0 else 1.0 / (seg * diRatio)
        val vdStep: Double = if (diRatio >= 1) 1.0 else diRatio
        (vs2Step, vdStep)
      }

      def getNumUop(diRatio: Double): Int = {
        val seg = nfP.segNum
        val dlmul = lmulP.lmulValue
        val ilmul = dlmul / diRatio
        val numUop = seg * (dlmul max ilmul)
        numUop.toInt
      }

      val res = uops.reverse.reduce(_ ## _)
      res
    }
  }

  object UopDependField extends DecodeField[UopLmulNfSplitOHPattern, UInt] {
    override def name: String = "uopDepend"

    override def chiselType: UInt = UInt(8.W)

    override def genTable(op: UopLmulNfSplitOHPattern): BitPat = {
      val UopLmulNfSplitOHPattern(lmulP, nfP, splitTypeOHP) = op
      val splitTypeValue: Int = splitTypeOHP.value
      var res = b"0000_0000"

      if (splitTypeValue == SplitType.VVM.litValue)
        res = b"1111_1110"
      else if (splitTypeValue == SplitType.WVV.litValue)
        res = b"1010_1010"
      else if (Seq(
        SplitType.VREDU, SplitType.VREDO,
        SplitType.VWREDU, SplitType.VWREDO,
        SplitType.SHUFFLE_SLIDE_UP, SplitType.SHUFFLE_COMPRESS_DOWN,
        SplitType.SHUFFLE_GATHER,
      ).map(_.litValue).contains(splitTypeValue)) {
        res = b"1111_1110"
      }
      else if (Seq(
        SplitType.VLSIDX_DI_RATIO_1, SplitType.VLSIDX_DI_RATIO_2,
        SplitType.VLSIDX_DI_RATIO_4, SplitType.VLSIDX_DI_RATIO_8,
        SplitType.VLSIDX_DI_RATIO_F2, SplitType.VLSIDX_DI_RATIO_F4,
        SplitType.VLSIDX_DI_RATIO_F8,
      ).map(_.litValue).contains(splitTypeValue)) {
        // TODO: make unorder uops independent
        res = b"1111_1110"
      }
      else if (splitTypeValue == SplitType.VLSNONIDXFF.litValue){
        res = b"1111_1111"
      }

      res.toBitPat
    }
  }

  object VdAllocField extends DecodeField[UopLmulNfSplitOHPattern, UInt] {
    override def name: String = "vdAlloc"

    override def chiselType: UInt = UInt(8.W)

    override def genTable(op: UopLmulNfSplitOHPattern): BitPat = {
      val UopLmulNfSplitOHPattern(lmulP, nfP, splitTypeOHP) = op
      val splitTypeValue: Int = splitTypeOHP.value
      var res = b"1111_1111"

      if (splitTypeValue == SplitType.VVM.litValue)
        res = b"0000_0001"
      else if (splitTypeValue == SplitType.WVV.litValue)
        res = b"0101_0101"
      else if (Seq(
        SplitType.VREDU, SplitType.VREDO,
        SplitType.VWREDU, SplitType.VWREDO,
      ).map(_.litValue).contains(splitTypeValue)) {
        res = b"0000_0001"
      }

      res.toBitPat
    }
  }

  object UopInfoField extends DecodeField[InstNfLmulSewPattern, Vec[ValidIO[UopInfoRenameWithIllegal]]] {
    import VecUopDefines._

    //                             valid          illegal
    lazy val emptyUopBitPat  : BitPat = BitPat.N(1) ## BitPat.dontCare(1) ## BitPat.dontCare(UopInfoRename.width)
    lazy val illegalUopBitPat: BitPat = BitPat.Y(1) ## BitPat.Y(1)        ## BitPat.dontCare(UopInfoRename.width)

//    override def default: BitPat = Seq.fill(8)(emptyUopBitPat).reduce(_ ## _)

    override def name: String = "uopInfo"

    override def chiselType: Vec[ValidIO[UopInfoRenameWithIllegal]] = Vec(8, ValidIO(new UopInfoRenameWithIllegal))

    override def genTable(op: InstNfLmulSewPattern): BitPat = {
      val uopSeq: Seq[UopBase] = genUopSeq(op)
      val InstNfLmulSewPattern(instP: VecInstPattern, nfP: NfPattern, lmulP: LmulPattern, sewP: SewPattern) = op

      {
        if (uopSeq.nonEmpty) {
          uopSeq.map {
            x: UopBase =>
              // valid = 1,  illegal = 0
              BitPat.Y(1) ## BitPat.N(1) ## x.genUopInfoRenameBitPat
          }.padTo(8, emptyUopBitPat)
        } else {
          illegalUopBitPat +: Seq.fill(7)(emptyUopBitPat)
        }
      }.reverse.reduce(_ ## _)
    }


    def genUopSeq(op: InstNfLmulSewPattern): Seq[UopBase] = {
      val InstNfLmulSewPattern(instP: VecInstPattern, nfP: NfPattern, lmulP: LmulPattern, sewP: SewPattern) = op

      instP match {
        case arith: VecArithInstPattern =>
          val instBitPat = arith.bitPat
          val lmul: Double = Lmuls.decodeValue(lmulP.bitPat)
          val uopSeq: Seq[_ <: UopBase] =
            try {
              lmul match {
                case 8 => lmul8Table.getOrElse(instBitPat, lmulUnrelatedTable.getOrElse(instBitPat, Seq.fill(8)(dupTable(instBitPat))))
                case 4 => lmul4Table.getOrElse(instBitPat, lmulUnrelatedTable.getOrElse(instBitPat, Seq.fill(4)(dupTable(instBitPat))))
                case 2 => lmul2Table.getOrElse(instBitPat, lmulUnrelatedTable.getOrElse(instBitPat, Seq.fill(2)(dupTable(instBitPat))))
                case 1 => lmul1Table.getOrElse(instBitPat, lmulUnrelatedTable.getOrElse(instBitPat, Seq.fill(1)(dupTable(instBitPat))))
                case x: Double if x > 0 && x < 1 =>
                  lmulF2Table.getOrElse(
                    instBitPat,
                    lmul1Table.getOrElse(
                      instBitPat,
                      lmulUnrelatedTable.getOrElse(
                        instBitPat,
                        Seq.fill(1)(dupTable(instBitPat))
                      )
                    )
                  )
              }
            } catch {
              case (e: java.util.NoSuchElementException) =>
                throw new IllegalArgumentException(s"key ${arith} not found in defaultTable and lmul${lmul.ceil.toInt}Table")
            }
          uopSeq

        case cfg: VecConfigInstPattern =>
          throw new IllegalArgumentException(s"inst ${cfg} pattern is not supported in UopInfoField")

        case mem: VecMemInstPattern if mem.isUnitStrideWhole =>
          val uop: UopBase = (mem.isLoad, mem.isStore) match {
            case (true, false) => vleWhole.x
            case (false, true) => vseWhole.x
          }
          val seg = NfPattern(mem.nf).segNum // will be 1,2,4,8
          Seq.fill(seg)(uop)

        case mem: VecMemInstPattern if mem.isUnitStrideMask =>
          val uop: UopBase = (mem.isLoad, mem.isStore) match {
            case (true, false) => vlm.x
            case (false, true) => vsm.x
          }
          Seq(uop)

        case mem: VecMemInstPattern if mem.isUnitStrideNormal || mem.isStrided || mem.isLoad && mem.isUnitStrideFF =>
          val uop: UopBase = (
            if (mem.isLoad) {
              if (mem.isUnitStrideNormal) vle.x
              else if (mem.isStrided) vlse.xx
              else if (mem.isUnitStrideFF) vleff.x
              else throw new IllegalArgumentException
            } else if (mem.isStore) {
              if (mem.isUnitStrideNormal) vse.x
              else if (mem.isStrided) vsse.xx
              else throw new IllegalArgumentException
            } else
              throw new IllegalArgumentException
          ).asInstanceOf[UopBase]

          val lmul: Double = lmulP.lmulValue
          val sew: Int = sewP.sewValue
          val eew: Int = mem.eewValue
          val emul: Int = (lmul * eew / sew).max(1.0).toInt
          val seg = nfP.segNum
          if (seg * emul <= 8) {
            Seq.fill(seg * emul)(uop)
          } else {
            Seq()
          }

        case mem: VecMemInstPattern if mem.isIndexedOrder || mem.isIndexedUnorder =>
          val uop: UopBase = {
            if (mem.isLoad) vlxe.vx
            else if (mem.isStore) vsxe.vx
            else {
              throw new IllegalArgumentException
            }
          }.asInstanceOf[UopBase]
          val lmul: Double = lmulP.lmulValue
          val sew: Int = sewP.sewValue
          val eew: Int = mem.eewValue
          val emul: Double = lmul * eew / sew
          val seg = nfP.segNum
          val dEmul = lmul
          val iEmul = emul
          val uopNum = (1.0 max iEmul max dEmul).toInt * seg
          if (iEmul >= 0.125 && uopNum <= 8)
            Seq.fill(uopNum)(uop)
          else
            Seq()
      }
    }
  }

}

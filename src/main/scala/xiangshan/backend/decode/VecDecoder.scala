package xiangshan.backend.decode

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util.BitPat.bitPatToUInt
import chisel3.util._
import freechips.rocketchip.util.uintToBitPat
import freechips.rocketchip.rocket.Instructions._
import utils._
import xiangshan.ExceptionNO.illegalInstr
import xiangshan._
import yunsuan.{VfpuType, VipuType, VpermType, VialuFixType}

abstract class VecDecode extends XSDecodeBase {
  def generate() : List[BitPat]
  def asOldDecodeOutput(): List[BitPat] = {
    val src1::src2::src3::fu::fuOp::xWen::fWen::vWen::mWen::vxsatWen::xsTrap::noSpec::blockBack::flushPipe::selImm::Nil = generate()
    List (src1, src2, src3, fu, fuOp, xWen, fWen, xsTrap, noSpec, blockBack, flushPipe, selImm)
  }
  def asFirstStageDecodeOutput(): List[BitPat] = {
    val src1::src2::src3::fu::fuOp::xWen::fWen::vWen::mWen::vxsatWen::xsTrap::noSpec::blockBack::flushPipe::selImm::Nil = generate()
    List (src1, src2, src3, fu, fuOp, xWen, fWen, bitPatToUInt(vWen) | bitPatToUInt(mWen), xsTrap, noSpec, blockBack, flushPipe, selImm)
  }
}

case class OPIVV(fu: BitPat, fuOp: BitPat, vWen: Boolean, mWen: Boolean, vxsatWen: Boolean, uopSplitType: BitPat = UopSplitType.VEC_VVV, src3: BitPat = SrcType.vp) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    XSDecode(SrcType.vp, SrcType.vp, src3, fu, fuOp, SelImm.X, uopSplitType,
      xWen = F, fWen = F, vWen = vWen, mWen = mWen, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class OPIVX(fu: BitPat, fuOp: BitPat, vWen: Boolean, mWen: Boolean, vxsatWen: Boolean, uopSplitType: BitPat = UopSplitType.VEC_VXV, src3: BitPat = SrcType.vp) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    XSDecode(SrcType.xp, SrcType.vp, src3, fu, fuOp, SelImm.X, uopSplitType,
      xWen = F, fWen = F, vWen = vWen, mWen = mWen, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class OPIVI(fu: BitPat, fuOp: BitPat, vWen: Boolean, mWen: Boolean, vxsatWen: Boolean, selImm: BitPat = SelImm.IMM_OPIVIS, uopSplitType: BitPat = UopSplitType.VEC_VVV, src3: BitPat = SrcType.vp) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    XSDecode(SrcType.imm, SrcType.vp, src3, fu, fuOp, selImm, uopSplitType,
      xWen = F, fWen = F, vWen = vWen, mWen = mWen, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class OPMVV(vdRen: Boolean, fu: BitPat, fuOp: BitPat, xWen: Boolean, vWen: Boolean, mWen: Boolean, uopSplitType: BitPat = UopSplitType.dummy) extends XSDecodeBase {
  private def src3: BitPat = if (vdRen) SrcType.vp else SrcType.X
  def generate() : List[BitPat] = {
    XSDecode(SrcType.vp, SrcType.vp, src3, fu, fuOp, SelImm.X, uopSplitType, xWen, F, vWen, mWen, F, F, F, F).generate()
  }
}

case class OPMVX(vdRen: Boolean, fu: BitPat, fuOp: BitPat, xWen: Boolean, vWen: Boolean, mWen: Boolean, uopSplitType: BitPat = UopSplitType.dummy) extends XSDecodeBase {
  private def src3: BitPat = if (vdRen) SrcType.vp else SrcType.X
  def generate() : List[BitPat] = {
    XSDecode(SrcType.xp, SrcType.vp, src3, fu, fuOp, SelImm.X, uopSplitType,
      xWen = xWen, fWen = F, vWen = vWen, mWen = mWen, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class OPFVV(src1:BitPat, src3:BitPat, fu: BitPat, fuOp: BitPat, fWen: Boolean, vWen: Boolean, mWen: Boolean, uopSplitType: BitPat = UopSplitType.dummy) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    XSDecode(src1, SrcType.vp, src3, fu, fuOp, SelImm.X, uopSplitType,
      xWen = F, fWen = fWen, vWen = vWen, mWen = mWen, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class OPFVF(src1:BitPat, src3:BitPat, fu: BitPat, fuOp: BitPat, fWen: Boolean, vWen: Boolean, mWen: Boolean, uopSplitType: BitPat = UopSplitType.dummy) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    XSDecode(src1, SrcType.vp, src3, fu, fuOp, SelImm.X, uopSplitType,
      xWen = F, fWen = fWen, vWen = vWen, mWen = mWen, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class VSET(vli: Boolean, vtypei: Boolean, fuOp: BitPat, flushPipe: Boolean, selImm: BitPat, uopSplitType: BitPat = UopSplitType.DIR) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    val src1 = if (vli) SrcType.imm else SrcType.xp
    val src2 = if (vtypei) SrcType.imm else SrcType.xp
    XSDecode(src1, src2, SrcType.X, FuType.alu, fuOp, selImm, uopSplitType,
      xWen = T, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = flushPipe).generate()
  }
}

case class VLD(src2: BitPat, fuOp: BitPat, strided: Boolean = false, indexed: Boolean = false, ff: Boolean = false,
  mask: Boolean = false, whole: Boolean = false, ordered: Boolean = false, uopSplitType: BitPat = UopSplitType.dummy) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    val fu = FuType.vldu
    val src1 = SrcType.xp
    val src3 = SrcType.X
    XSDecode(src1, src2, src3, fu, fuOp, SelImm.X, uopSplitType,
      xWen = F, fWen = F, vWen = T, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class VST(src2: BitPat, fuOp: BitPat, strided: Boolean = false, indexed: Boolean = false,
  mask: Boolean = false, whole: Boolean = false, ordered: Boolean = false, uopSplitType: BitPat = UopSplitType.dummy) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    val fu = FuType.vstu
    val src1 = SrcType.xp
    val src3 = SrcType.vp
    XSDecode(src1, src2, src3, fu, fuOp, SelImm.X, uopSplitType,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

object VecDecoder extends DecodeConstants {
  val opivv: Array[(BitPat, XSDecodeBase)] = Array(
    VADD_VV         -> OPIVV(FuType.vialuF, VialuFixType.vadd_vv, T, F, F),
    VSUB_VV         -> OPIVV(FuType.vialuF, VialuFixType.vsub_vv, T, F, F),

    VMINU_VV        -> OPIVV(FuType.vialuF, VialuFixType.vminu_vv, T, F, F),
    VMIN_VV         -> OPIVV(FuType.vialuF, VialuFixType.vmin_vv, T, F, F),
    VMAXU_VV        -> OPIVV(FuType.vialuF, VialuFixType.vmaxu_vv, T, F, F),
    VMAX_VV         -> OPIVV(FuType.vialuF, VialuFixType.vmax_vv, T, F, F),

    VAND_VV         -> OPIVV(FuType.vialuF, VialuFixType.vand_vv, T, F, F),
    VOR_VV          -> OPIVV(FuType.vialuF, VialuFixType.vor_vv, T, F, F),
    VXOR_VV         -> OPIVV(FuType.vialuF, VialuFixType.vxor_vv, T, F, F),

    VRGATHER_VV     -> OPIVV(FuType.vppu, VpermType.vrgather, T, F, F, UopSplitType.VEC_RGATHER),
    VRGATHEREI16_VV -> OPIVV(FuType.vppu, VpermType.vrgatherei16, T, F, F, UopSplitType.VEC_RGATHEREI16),

    VADC_VVM        -> OPIVV(FuType.vialuF, VialuFixType.vadc_vvm, T, F, F),
    VMADC_VVM       -> OPIVV(FuType.vialuF, VialuFixType.vmadc_vvm, F, T, F, UopSplitType.VEC_VVM),
    VMADC_VV        -> OPIVV(FuType.vialuF, VialuFixType.vmadc_vv, F, T, F, UopSplitType.VEC_VVM),

    VSBC_VVM        -> OPIVV(FuType.vialuF, VialuFixType.vsbc_vvm, T, F, F),
    VMSBC_VV        -> OPIVV(FuType.vialuF, VialuFixType.vmsbc_vv, F, T, F, UopSplitType.VEC_VVM),
    VMSBC_VVM       -> OPIVV(FuType.vialuF, VialuFixType.vmsbc_vvm, F, T, F, UopSplitType.VEC_VVM),

    VMERGE_VVM      -> OPIVV(FuType.vialuF, VialuFixType.vmerge_vvm, T, F, F),

    VMV_V_V      -> OPIVV(FuType.vialuF, VialuFixType.vmv_v_v, T, F, F),

    VMSEQ_VV        -> OPIVV(FuType.vialuF, VialuFixType.vmseq_vv, F, T, F, UopSplitType.VEC_VVM),
    VMSNE_VV        -> OPIVV(FuType.vialuF, VialuFixType.vmsne_vv, F, T, F, UopSplitType.VEC_VVM),
    VMSLTU_VV       -> OPIVV(FuType.vialuF, VialuFixType.vmsltu_vv, F, T, F, UopSplitType.VEC_VVM),
    VMSLT_VV        -> OPIVV(FuType.vialuF, VialuFixType.vmslt_vv, F, T, F, UopSplitType.VEC_VVM),
    VMSLEU_VV       -> OPIVV(FuType.vialuF, VialuFixType.vmsleu_vv, F, T, F, UopSplitType.VEC_VVM),
    VMSLE_VV        -> OPIVV(FuType.vialuF, VialuFixType.vmsle_vv, F, T, F, UopSplitType.VEC_VVM),

    VSLL_VV         -> OPIVV(FuType.vialuF, VialuFixType.vsll_vv, T, F, F),
    VSRL_VV         -> OPIVV(FuType.vialuF, VialuFixType.vsrl_vv, T, F, F),
    VSRA_VV         -> OPIVV(FuType.vialuF, VialuFixType.vsra_vv, T, F, F),
    VNSRL_WV        -> OPIVV(FuType.vialuF, VialuFixType.vnsrl_wv, T, F, F, UopSplitType.VEC_WVV),
    VNSRA_WV        -> OPIVV(FuType.vialuF, VialuFixType.vnsra_wv, T, F, F, UopSplitType.VEC_WVV),

    VSADDU_VV       -> OPIVV(FuType.vialuF, VialuFixType.vsaddu_vv, T, F, T),
    VSADD_VV        -> OPIVV(FuType.vialuF, VialuFixType.vsadd_vv, T, F, T),
    VSSUBU_VV       -> OPIVV(FuType.vialuF, VialuFixType.vssubu_vv, T, F, T),
    VSSUB_VV        -> OPIVV(FuType.vialuF, VialuFixType.vssub_vv, T, F, T),

    VSMUL_VV        -> OPIVV(FuType.vipu, VipuType.dummy, T, F, T),

    VSSRL_VV        -> OPIVV(FuType.vialuF, VialuFixType.vssrl_vv, T, F, F),
    VSSRA_VV        -> OPIVV(FuType.vialuF, VialuFixType.vssra_vv, T, F, F),

    VNCLIPU_WV      -> OPIVV(FuType.vialuF, VialuFixType.vnclipu_wv, T, F, T, UopSplitType.VEC_WVV),
    VNCLIP_WV       -> OPIVV(FuType.vialuF, VialuFixType.vnclip_wv, T, F, T, UopSplitType.VEC_WVV),

    VWREDSUMU_VS    -> OPIVV(FuType.vipu, VipuType.vwredsumu_vs, T, F, F, UopSplitType.VEC_VWW),
    VWREDSUM_VS     -> OPIVV(FuType.vipu, VipuType.vwredsum_vs, T, F, F, UopSplitType.VEC_VWW),
  )

  val opivx: Array[(BitPat, XSDecodeBase)] = Array(
    VADD_VX       -> OPIVX(FuType.vialuF, VialuFixType.vadd_vv, T, F, F),
    VSUB_VX       -> OPIVX(FuType.vialuF, VialuFixType.vsub_vv, T, F, F),
    VRSUB_VX      -> OPIVX(FuType.vialuF, VialuFixType.vrsub_vv, T, F, F),

    VMINU_VX      -> OPIVX(FuType.vialuF, VialuFixType.vminu_vv, T, F, F),
    VMIN_VX       -> OPIVX(FuType.vialuF, VialuFixType.vmin_vv, T, F, F),
    VMAXU_VX      -> OPIVX(FuType.vialuF, VialuFixType.vmaxu_vv, T, F, F),
    VMAX_VX       -> OPIVX(FuType.vialuF, VialuFixType.vmax_vv, T, F, F),

    VAND_VX       -> OPIVX(FuType.vialuF, VialuFixType.vand_vv, T, F, F),
    VOR_VX        -> OPIVX(FuType.vialuF, VialuFixType.vor_vv, T, F, F),
    VXOR_VX       -> OPIVX(FuType.vialuF, VialuFixType.vxor_vv, T, F, F),

    VRGATHER_VX   -> OPIVX(FuType.vppu, VpermType.vrgather_vx, T, F, F, UopSplitType.VEC_RGATHER_VX),

    VSLIDEUP_VX   -> OPIVX(FuType.vppu, VpermType.vslideup, T, F, F, UopSplitType.VEC_SLIDEUP),
    VSLIDEDOWN_VX -> OPIVX(FuType.vppu, VpermType.vslidedown, T, F, F, UopSplitType.VEC_SLIDEDOWN),

    VADC_VXM      -> OPIVX(FuType.vialuF, VialuFixType.vadc_vvm, T, F, F),
    VMADC_VXM      -> OPIVX(FuType.vialuF, VialuFixType.vmadc_vvm, F, T, F, UopSplitType.VEC_VXM),
    VMADC_VX      -> OPIVX(FuType.vialuF, VialuFixType.vmadc_vv, F, T, F, UopSplitType.VEC_VXM),
    VSBC_VXM      -> OPIVX(FuType.vialuF, VialuFixType.vsbc_vvm, T, F, F),
    VMSBC_VX      -> OPIVX(FuType.vialuF, VialuFixType.vmsbc_vv, F, T, F, UopSplitType.VEC_VXM),
    VMSBC_VXM     -> OPIVX(FuType.vialuF, VialuFixType.vmsbc_vvm, F, T, F, UopSplitType.VEC_VXM),

    VMERGE_VXM    -> OPIVX(FuType.vialuF, VialuFixType.vmerge_vvm, T, F, F),

    VMV_V_X    -> OPIVX(FuType.vialuF, VialuFixType.vmv_v_v, T, F, F),

    VMSEQ_VX      -> OPIVX(FuType.vialuF, VialuFixType.vmseq_vv, F, T, F, UopSplitType.VEC_VXM),
    VMSNE_VX      -> OPIVX(FuType.vialuF, VialuFixType.vmsne_vv, F, T, F, UopSplitType.VEC_VXM),
    VMSLTU_VX     -> OPIVX(FuType.vialuF, VialuFixType.vmsltu_vv, F, T, F, UopSplitType.VEC_VXM),
    VMSLT_VX      -> OPIVX(FuType.vialuF, VialuFixType.vmslt_vv, F, T, F, UopSplitType.VEC_VXM),
    VMSLEU_VX     -> OPIVX(FuType.vialuF, VialuFixType.vmsleu_vv, F, T, F, UopSplitType.VEC_VXM),
    VMSLE_VX      -> OPIVX(FuType.vialuF, VialuFixType.vmsle_vv, F, T, F, UopSplitType.VEC_VXM),
    VMSGTU_VX     -> OPIVX(FuType.vialuF, VialuFixType.vmsgtu_vv, F, T, F, UopSplitType.VEC_VXM),
    VMSGT_VX      -> OPIVX(FuType.vialuF, VialuFixType.vmsgt_vv, F, T, F, UopSplitType.VEC_VXM),

    VSLL_VX       -> OPIVX(FuType.vialuF, VialuFixType.vsll_vv, T, F, F),
    VSRL_VX       -> OPIVX(FuType.vialuF, VialuFixType.vsrl_vv, T, F, F),
    VSRA_VX       -> OPIVX(FuType.vialuF, VialuFixType.vsra_vv, T, F, F),
    VNSRL_WX      -> OPIVX(FuType.vialuF, VialuFixType.vnsrl_wv, T, F, F, UopSplitType.VEC_WXV),
    VNSRA_WX      -> OPIVX(FuType.vialuF, VialuFixType.vnsra_wv, T, F, F, UopSplitType.VEC_WXV),

    VSADDU_VX     -> OPIVX(FuType.vialuF, VialuFixType.vsaddu_vv, T, F, T),
    VSADD_VX      -> OPIVX(FuType.vialuF, VialuFixType.vsadd_vv, T, F, T),
    VSSUBU_VX     -> OPIVX(FuType.vialuF, VialuFixType.vssubu_vv, T, F, T),
    VSSUB_VX      -> OPIVX(FuType.vialuF, VialuFixType.vssub_vv, T, F, T),


    VSMUL_VX      -> OPIVX(FuType.vipu, VipuType.dummy, T, F, T),

    VSSRL_VX      -> OPIVX(FuType.vialuF, VialuFixType.vssrl_vv, T, F, F),
    VSSRA_VX      -> OPIVX(FuType.vialuF, VialuFixType.vssra_vv, T, F, F),

    VNCLIPU_WX    -> OPIVX(FuType.vialuF, VialuFixType.vnclipu_wv, T, F, T, UopSplitType.VEC_WXV),
    VNCLIP_WX     -> OPIVX(FuType.vialuF, VialuFixType.vnclip_wv, T, F, T, UopSplitType.VEC_WXV),
  )

  val opivi: Array[(BitPat, XSDecodeBase)] = Array(
    VADD_VI       -> OPIVI(FuType.vialuF, VialuFixType.vadd_vv,   T, F, F),
    VRSUB_VI      -> OPIVI(FuType.vialuF, VialuFixType.vrsub_vv, T, F, F),

    VAND_VI       -> OPIVI(FuType.vialuF, VialuFixType.vand_vv, T, F, F),
    VOR_VI        -> OPIVI(FuType.vialuF, VialuFixType.vor_vv, T, F, F),
    VXOR_VI       -> OPIVI(FuType.vialuF, VialuFixType.vxor_vv, T, F, F),

    VRGATHER_VI   -> OPIVI(FuType.vppu, VpermType.vrgather, T, F, F, selImm = SelImm.IMM_OPIVIU, uopSplitType = UopSplitType.VEC_RGATHER),

    VSLIDEUP_VI   -> OPIVI(FuType.vppu, VpermType.vslideup, T, F, F, selImm = SelImm.IMM_OPIVIU, uopSplitType = UopSplitType.VEC_ISLIDEUP),
    VSLIDEDOWN_VI -> OPIVI(FuType.vppu, VpermType.vslidedown, T, F, F, selImm = SelImm.IMM_OPIVIU, uopSplitType = UopSplitType.VEC_ISLIDEDOWN),

    VADC_VIM      -> OPIVI(FuType.vialuF, VialuFixType.vadc_vvm, T, F, F),
    VMADC_VIM     -> OPIVI(FuType.vialuF, VialuFixType.vmadc_vvm, T, F, F, uopSplitType = UopSplitType.VEC_VVM),
    VMADC_VI      -> OPIVI(FuType.vialuF, VialuFixType.vmadc_vv, T, F, F, uopSplitType = UopSplitType.VEC_VVM),

    VMERGE_VIM    -> OPIVI(FuType.vialuF, VialuFixType.vmerge_vvm, T, F, F),

    VMV_V_I    -> OPIVI(FuType.vialuF, VialuFixType.vmv_v_v, T, F, F),

    VMSEQ_VI      -> OPIVI(FuType.vialuF, VialuFixType.vmseq_vv, F, T, F, uopSplitType = UopSplitType.VEC_VVM),
    VMSNE_VI      -> OPIVI(FuType.vialuF, VialuFixType.vmsne_vv, F, T, F, uopSplitType = UopSplitType.VEC_VVM),
    VMSLEU_VI     -> OPIVI(FuType.vialuF, VialuFixType.vmsleu_vv, F, T, F, selImm = SelImm.IMM_OPIVIU, uopSplitType = UopSplitType.VEC_VVM),
    VMSLE_VI      -> OPIVI(FuType.vialuF, VialuFixType.vmsle_vv, F, T, F, uopSplitType = UopSplitType.VEC_VVM),
    VMSGTU_VI     -> OPIVI(FuType.vialuF, VialuFixType.vmsgtu_vv, F, T, F, selImm = SelImm.IMM_OPIVIU, uopSplitType = UopSplitType.VEC_VVM),
    VMSGT_VI      -> OPIVI(FuType.vialuF, VialuFixType.vmsgt_vv, F, T, F, uopSplitType = UopSplitType.VEC_VVM),

    VSLL_VI       -> OPIVI(FuType.vialuF, VialuFixType.vsll_vv, T, F, F, selImm = SelImm.IMM_OPIVIU),
    VSRL_VI       -> OPIVI(FuType.vialuF, VialuFixType.vsrl_vv, T, F, F, selImm = SelImm.IMM_OPIVIU),
    VSRA_VI       -> OPIVI(FuType.vialuF, VialuFixType.vsra_vv, T, F, F, selImm = SelImm.IMM_OPIVIU),
    VNSRL_WI      -> OPIVI(FuType.vialuF, VialuFixType.vnsrl_wv, T, F, F, selImm = SelImm.IMM_OPIVIU, uopSplitType = UopSplitType.VEC_WVV),
    VNSRA_WI      -> OPIVI(FuType.vialuF, VialuFixType.vnsra_wv, T, F, F, selImm = SelImm.IMM_OPIVIU, uopSplitType = UopSplitType.VEC_WVV),

    VSADDU_VI     -> OPIVI(FuType.vialuF, VialuFixType.vsaddu_vv, T, F, T, selImm = SelImm.IMM_OPIVIU),
    VSADD_VI      -> OPIVI(FuType.vialuF, VialuFixType.vsadd_vv, T, F, T),

    VSSRL_VI      -> OPIVI(FuType.vialuF, VialuFixType.vssrl_vv, T, F, F, selImm = SelImm.IMM_OPIVIU),
    VSSRA_VI      -> OPIVI(FuType.vialuF, VialuFixType.vssra_vv, T, F, F, selImm = SelImm.IMM_OPIVIU),

    VNCLIPU_WI    -> OPIVI(FuType.vialuF, VialuFixType.vnclipu_wv, T, F, T, selImm = SelImm.IMM_OPIVIU, uopSplitType = UopSplitType.VEC_WVV),
    VNCLIP_WI     -> OPIVI(FuType.vialuF, VialuFixType.vnclip_wv, T, F, T, uopSplitType = UopSplitType.VEC_WVV),

    VMV1R_V       -> OPIVI(FuType.vipu, VipuType.dummy, T, F, F),
    VMV2R_V       -> OPIVI(FuType.vipu, VipuType.dummy, T, F, F),
    VMV4R_V       -> OPIVI(FuType.vipu, VipuType.dummy, T, F, F),
    VMV8R_V       -> OPIVI(FuType.vipu, VipuType.dummy, T, F, F),
  )

  val opmvv: Array[(BitPat, XSDecodeBase)] = Array(
    VAADD_VV     -> OPMVV(T, FuType.vialuF, VialuFixType.vaadd_vv, F, T, F, UopSplitType.VEC_VVV),
    VAADDU_VV    -> OPMVV(T, FuType.vialuF, VialuFixType.vaaddu_vv, F, T, F, UopSplitType.VEC_VVV),
    VASUB_VV     -> OPMVV(T, FuType.vialuF, VialuFixType.vasub_vv, F, T, F, UopSplitType.VEC_VVV),
    VASUBU_VV    -> OPMVV(T, FuType.vialuF, VialuFixType.vasubu_vv, F, T, F, UopSplitType.VEC_VVV),
    VCOMPRESS_VM -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VCPOP_M      -> OPMVV(T, FuType.vipu, VipuType.vcpop_m, T, F, F, UopSplitType.VEC_M0X),
    VDIV_VV      -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VDIVU_VV     -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VFIRST_M     -> OPMVV(T, FuType.vipu, VipuType.vfirst_m, T, F, F, UopSplitType.VEC_M0X_VFIRST),
    VID_V        -> OPMVV(T, FuType.vipu, VipuType.vid_v, F, T, F, UopSplitType.VEC_MVV),
    VIOTA_M      -> OPMVV(T, FuType.vipu, VipuType.viota_m, F, T, F, UopSplitType.VEC_MVV),

    // VMACC_VV     -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),

    VMADD_VV     -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VMAND_MM     -> OPMVV(T, FuType.vialuF, VialuFixType.vmand_mm, F, T, F, UopSplitType.VEC_MMM),
    VMANDN_MM    -> OPMVV(T, FuType.vialuF, VialuFixType.vmandn_mm, F, T, F, UopSplitType.VEC_MMM),
    VMNAND_MM    -> OPMVV(T, FuType.vialuF, VialuFixType.vmnand_mm, F, T, F, UopSplitType.VEC_MMM),
    VMNOR_MM     -> OPMVV(T, FuType.vialuF, VialuFixType.vmnor_mm, F, T, F, UopSplitType.VEC_MMM),
    VMOR_MM      -> OPMVV(T, FuType.vialuF, VialuFixType.vmor_mm, F, T, F, UopSplitType.VEC_MMM),
    VMORN_MM     -> OPMVV(T, FuType.vialuF, VialuFixType.vmorn_mm, F, T, F, UopSplitType.VEC_MMM),
    VMXNOR_MM    -> OPMVV(T, FuType.vialuF, VialuFixType.vmxnor_mm, F, T, F, UopSplitType.VEC_MMM),
    VMXOR_MM     -> OPMVV(T, FuType.vialuF, VialuFixType.vmxor_mm, F, T, F, UopSplitType.VEC_MMM),
    VMSBF_M      -> OPMVV(T, FuType.vipu, VipuType.vmsbf_m, F, T, F, UopSplitType.VEC_M0M),
    VMSIF_M      -> OPMVV(T, FuType.vipu, VipuType.vmsif_m, F, T, F, UopSplitType.VEC_M0M),
    VMSOF_M      -> OPMVV(T, FuType.vipu, VipuType.vmsof_m, F, T, F, UopSplitType.VEC_M0M),
    VMUL_VV      -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VMULH_VV     -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VMULHSU_VV   -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VMULHU_VV    -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),

    VMV_X_S      -> OPMVV(T, FuType.vipu, VipuType.dummy, T, F, F),
    VNMSAC_VV    -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VNMSUB_VV    -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VREDAND_VS   -> OPMVV(T, FuType.vipu, VipuType.vredand_vs, F, T, F, UopSplitType.VEC_VRED),
    VREDMAX_VS   -> OPMVV(T, FuType.vipu, VipuType.vredmax_vs, F, T, F, UopSplitType.VEC_VRED),
    VREDMAXU_VS  -> OPMVV(T, FuType.vipu, VipuType.vredmaxu_vs, F, T, F, UopSplitType.VEC_VRED),
    VREDMIN_VS   -> OPMVV(T, FuType.vipu, VipuType.vredmin_vs, F, T, F, UopSplitType.VEC_VRED),
    VREDMINU_VS  -> OPMVV(T, FuType.vipu, VipuType.vredminu_vs, F, T, F, UopSplitType.VEC_VRED),
    VREDOR_VS    -> OPMVV(T, FuType.vipu, VipuType.vredor_vs, F, T, F, UopSplitType.VEC_VRED),
    VREDSUM_VS   -> OPMVV(T, FuType.vipu, VipuType.vredsum_vs, F, T, F, UopSplitType.VEC_VRED),
    VREDXOR_VS   -> OPMVV(T, FuType.vipu, VipuType.vredxor_vs, F, T, F, UopSplitType.VEC_VRED),
    VREM_VV      -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VREMU_VV     -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VSEXT_VF2    -> OPMVV(T, FuType.vialuF, VialuFixType.vsext_vf2, F, T, F, UopSplitType.VEC_EXT2),
    VSEXT_VF4    -> OPMVV(T, FuType.vialuF, VialuFixType.vsext_vf4, F, T, F, UopSplitType.VEC_EXT4),
    VSEXT_VF8    -> OPMVV(T, FuType.vialuF, VialuFixType.vsext_vf8, F, T, F, UopSplitType.VEC_EXT8),
    VZEXT_VF2    -> OPMVV(T, FuType.vialuF, VialuFixType.vzext_vf2, F, T, F, UopSplitType.VEC_EXT2),
    VZEXT_VF4    -> OPMVV(T, FuType.vialuF, VialuFixType.vzext_vf4, F, T, F, UopSplitType.VEC_EXT4),
    VZEXT_VF8    -> OPMVV(T, FuType.vialuF, VialuFixType.vzext_vf8, F, T, F, UopSplitType.VEC_EXT8),
    VWADD_VV     -> OPMVV(T, FuType.vialuF, VialuFixType.vwadd_vv, F, T, F, UopSplitType.VEC_VVW),
    VWADD_WV     -> OPMVV(T, FuType.vialuF, VialuFixType.vwadd_wv, F, T, F, UopSplitType.VEC_WVW),
    VWADDU_VV    -> OPMVV(T, FuType.vialuF, VialuFixType.vwaddu_vv, F, T, F, UopSplitType.VEC_VVW),
    VWADDU_WV    -> OPMVV(T, FuType.vialuF, VialuFixType.vwaddu_wv, F, T, F, UopSplitType.VEC_WVW),
    VWMACC_VV    -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMACCSU_VV  -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMACCU_VV   -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMUL_VV     -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMULSU_VV   -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMULU_VV    -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWSUB_VV     -> OPMVV(T, FuType.vialuF, VialuFixType.vwsub_vv, F, T, F, UopSplitType.VEC_VVW),
    VWSUB_WV     -> OPMVV(T, FuType.vialuF, VialuFixType.vwsub_wv, F, T, F, UopSplitType.VEC_WVW),
    VWSUBU_VV    -> OPMVV(T, FuType.vialuF, VialuFixType.vwsubu_vv, F, T, F, UopSplitType.VEC_VVW),
    VWSUBU_WV    -> OPMVV(T, FuType.vialuF, VialuFixType.vwsubu_wv, F, T, F, UopSplitType.VEC_WVW),
  )

  val opmvx: Array[(BitPat, XSDecodeBase)] = Array(
    VAADD_VX       -> OPMVX(T, FuType.vialuF, VialuFixType.vaadd_vv, F, T, F, UopSplitType.VEC_VXV),
    VAADDU_VX      -> OPMVX(T, FuType.vialuF, VialuFixType.vaaddu_vv, F, T, F, UopSplitType.VEC_VXV),
    VASUB_VX       -> OPMVX(T, FuType.vialuF, VialuFixType.vasub_vv, F, T, F, UopSplitType.VEC_VXV),
    VASUBU_VX      -> OPMVX(T, FuType.vialuF, VialuFixType.vasubu_vv, F, T, F, UopSplitType.VEC_VXV),
    VDIV_VX        -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VDIVU_VX       -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VMACC_VX       -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VMADD_VX       -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VMUL_VX        -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VMULH_VX       -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VMULHSU_VX     -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VMULHU_VX      -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VMV_S_X        -> OPMVX(T, FuType.vipu, VipuType.vmv_s_x, F, T, F, UopSplitType.VEC_0XV),

    VNMSAC_VX      -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VNMSUB_VX      -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VREM_VX        -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VREMU_VX       -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),

    VSLIDE1DOWN_VX -> OPMVX(T, FuType.vppu, VpermType.vslide1down, F, T, F, UopSplitType.VEC_SLIDE1DOWN),
    VSLIDE1UP_VX   -> OPMVX(T, FuType.vppu, VpermType.vslide1up, F, T, F, UopSplitType.VEC_SLIDE1UP),
    VWADD_VX       -> OPMVX(T, FuType.vialuF, VialuFixType.vwadd_vv, F, T, F, UopSplitType.VEC_VXW),
    VWADD_WX       -> OPMVX(T, FuType.vialuF, VialuFixType.vwadd_wv, F, T, F, UopSplitType.VEC_WXW),
    VWADDU_VX      -> OPMVX(T, FuType.vialuF, VialuFixType.vwaddu_vv, F, T, F, UopSplitType.VEC_VXW),
    VWADDU_WX      -> OPMVX(T, FuType.vialuF, VialuFixType.vwaddu_wv, F, T, F, UopSplitType.VEC_WXW),

    // OutOfMemoryError
    VWMACC_VX      -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMACCSU_VX    -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMACCU_VX     -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),

    VWMACCUS_VX    -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMUL_VX       -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMULSU_VX     -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    // Ok
    VWMULU_VX      -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWSUB_VX       -> OPMVX(T, FuType.vialuF, VialuFixType.vwsub_vv, F, T, F, UopSplitType.VEC_VXW),
    VWSUB_WX       -> OPMVX(T, FuType.vialuF, VialuFixType.vwsub_wv, F, T, F, UopSplitType.VEC_WXW),
    VWSUBU_VX      -> OPMVX(T, FuType.vialuF, VialuFixType.vwsubu_vv, F, T, F, UopSplitType.VEC_VXW),
    VWSUBU_WX      -> OPMVX(T, FuType.vialuF, VialuFixType.vwsubu_wv, F, T, F, UopSplitType.VEC_WXW),
  )

  val opfvv: Array[(BitPat, XSDecodeBase)] = Array(
    // 13.2. Vector Single-Width Floating-Point Add/Subtract Instructions
    VFADD_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfadd, F, T, F),
    VFSUB_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfsub, F, T, F),

    // 13.3. Vector Widening Floating-Point Add/Subtract Instructions
    VFWADD_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfadd, F, T, F),
    VFWSUB_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfsub, F, T, F),
    VFWADD_WV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfadd, F, T, F),
    VFWSUB_WV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfsub, F, T, F),

    // 13.4. Vector Single-Width Floating-Point Multiply/Divide Instructions
    VFMUL_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfmul, F, T, F),
    VFDIV_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfdiv , F, T, F),

    // 13.5. Vector Widening Floating-Point Multiply
    VFWMUL_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfmul, F, T, F),

    // 13.6. Vector Single-Width Floating-Point Fused Multiply-Add Instructions
    VFMACC_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.vfmacc , F, T, F),
    VFNMACC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.vfnmacc, F, T, F),
    VFMSAC_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.vfmsac , F, T, F),
    VFNMSAC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.vfnmsac, F, T, F),
    VFMADD_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.vfmadd , F, T, F),
    VFNMADD_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.vfnmadd, F, T, F),
    VFMSUB_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.vfmsub , F, T, F),
    VFNMSUB_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.vfnmsub, F, T, F),

    // 13.7. Vector Widening Floating-Point Fused Multiply-Add Instructions
    VFWMACC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.vfmacc , F, T, F),
    VFWNMACC_VV        -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.vfnmacc, F, T, F),
    VFWMSAC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.vfmsac , F, T, F),
    VFWNMSAC_VV        -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.vfnmsac, F, T, F),

    // 13.8. Vector Floating-Point Square-Root Instruction
    VFSQRT_V           -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.9. Vector Floating-Point Reciprocal Square-Root Estimate Instruction
    VFRSQRT7_V         -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.10. Vector Floating-Point Reciprocal Estimate Instruction
    VFREC7_V           -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.11. Vector Floating-Point MIN/MAX Instructions
    VFMIN_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfmin, F, T, F),
    VFMAX_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfmax, F, T, F),

    // 13.12. Vector Floating-Point Sign-Injection Instructions
    VFSGNJ_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfsgnj , F, T, F),
    VFSGNJN_VV         -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfsgnjn, F, T, F),
    VFSGNJX_VV         -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfsgnjx, F, T, F),

    // 13.13. Vector Floating-Point Compare Instructions
    VMFEQ_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfeq, F, T, F),
    VMFNE_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfne, F, T, F),
    VMFLT_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vflt, F, T, F),
    VMFLE_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfle, F, T, F),

    // 13.14. Vector Floating-Point Classify Instruction
    VFCLASS_V          -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.fclass, F, T, F),

    // 13.17. Single-Width Floating-Point/Integer Type-Convert Instructions
    VFCVT_XU_F_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFCVT_X_F_V        -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFCVT_RTZ_XU_F_V   -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFCVT_RTZ_X_F_V    -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFCVT_F_XU_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFCVT_F_X_V        -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.18. Widening Floating-Point/Integer Type-Convert Instructions
    VFWCVT_XU_F_V      -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWCVT_X_F_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWCVT_RTZ_XU_F_V  -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWCVT_RTZ_X_F_V   -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWCVT_F_XU_V      -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWCVT_F_X_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWCVT_F_F_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // !
    // 13.19. Narrowing Floating-Point/Integer Type-Convert Instructions
    VFNCVT_XU_F_W      -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFNCVT_X_F_W       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFNCVT_RTZ_XU_F_W  -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFNCVT_RTZ_X_F_W   -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFNCVT_F_XU_W      -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFNCVT_F_X_W       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFNCVT_F_F_W       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFNCVT_ROD_F_F_W   -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 14.3. Vector Single-Width Floating-Point Reduction Instructions
    VFREDOSUM_VS       -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFREDUSUM_VS       -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFREDMAX_VS        -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFREDMIN_VS        -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 14.4. Vector Widening Floating-Point Reduction Instructions
    VFWREDOSUM_VS      -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWREDUSUM_VS      -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 16.2. Floating-Point Scalar Move Instructions
    VFMV_F_S           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.vfmove, F, T, F),// f[rd] = vs2[0] (rs1=0)
  )

  val opfvf: Array[(BitPat, XSDecodeBase)] = Array(
    // 13.2. Vector Single-Width Floating-Point Add/Subtract Instructions
    VFADD_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.vfadd, F, T, F),
    VFSUB_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.vfsub, F, T, F),
    VFRSUB_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.3. Vector Widening Floating-Point Add/Subtract Instructions
    VFWADD_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWSUB_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWADD_WF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWSUB_WF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.4. Vector Single-Width Floating-Point Multiply/Divide Instructions
    VFMUL_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFDIV_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.vfdiv, F, T, F),
    VFRDIV_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.5. Vector Widening Floating-Point Multiply
    VFWMUL_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.6. Vector Single-Width Floating-Point Fused Multiply-Add Instructions
    VFMACC_VF          -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.vfmacc, F, T, F),
    VFNMACC_VF         -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFMSAC_VF          -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFNMSAC_VF         -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFMADD_VF          -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFNMADD_VF         -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFMSUB_VF          -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFNMSUB_VF         -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.7. Vector Widening Floating-Point Fused Multiply-Add Instructions
    VFWMACC_VF         -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWNMACC_VF        -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWMSAC_VF         -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWNMSAC_VF        -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.11. Vector Floating-Point MIN/MAX Instructions
    VFMIN_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.vfmin, F, T, F),
    VFMAX_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.vfmax, F, T, F),

    // 13.12. Vector Floating-Point Sign-Injection Instructions
    VFSGNJ_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFSGNJN_VF         -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFSGNJX_VF         -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.13. Vector Floating-Point Compare Instructions
    VMFEQ_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, F, T),
    VMFNE_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, F, T),
    VMFLT_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, F, T),
    VMFLE_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, F, T),
    VMFGT_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, F, T),
    VMFGE_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, F, T),

    // 13.15. Vector Floating-Point Merge Instruction
    VFMERGE_VFM        -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.16. Vector Floating-Point Move Instruction
    VFMV_V_F           -> OPFVF(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),// src2=SrcType.X

    // 16.2. Floating-Point Scalar Move Instructions
    VFMV_S_F           -> OPFVF(SrcType.fp, SrcType.vp, FuType.vppu, VpermType.vfmv_s_f, F, T, F),// vs2=0 // vs3 = vd

    // 16.3.3. Vector Slide1up
    VFSLIDE1UP_VF      -> OPFVF(SrcType.fp, SrcType.vp , FuType.vppu, VpermType.vfslide1up, F, T, F, UopSplitType.VEC_FSLIDE1UP),// vd[0]=f[rs1], vd[i+1] = vs2[i]

    // 16.3.4. Vector Slide1down Instruction
    // vslide1down.vx vd, vs2, rs1, vm # vd[i] = vs2[i+1], vd[vl-1]=x[rs1]
    VFSLIDE1DOWN_VF    -> OPFVF(SrcType.fp, SrcType.vp , FuType.vppu, VpermType.vfslide1down, F, T, F, UopSplitType.VEC_FSLIDE1DOWN),// vd[i] = vs2[i+1], vd[vl-1]=f[rs1]
  )

  val vset: Array[(BitPat, XSDecodeBase)] = Array(
    VSETVLI   -> VSET(F, T, ALUOpType.vsetvli1,  F, SelImm.IMM_VSETVLI),
    VSETIVLI  -> VSET(T, T, ALUOpType.vsetivli1, F, SelImm.IMM_VSETIVLI),
    VSETVL    -> VSET(F, F, ALUOpType.vsetvl1,   T, SelImm.X), // flush pipe
  )

  val vls: Array[(BitPat, XSDecodeBase)] = Array(
    // 7.4. Vector Unit-Stride Instructions
    VLE8_V        -> VLD(SrcType.X,   VlduType.dummy),
    VLE16_V       -> VLD(SrcType.X,   VlduType.dummy),
    VLE32_V       -> VLD(SrcType.X,   VlduType.dummy),
    VLE64_V       -> VLD(SrcType.X,   VlduType.dummy),
    VSE8_V        -> VST(SrcType.X,   VstuType.dummy),
    VSE16_V       -> VST(SrcType.X,   VstuType.dummy),
    VSE32_V       -> VST(SrcType.X,   VstuType.dummy),
    VSE64_V       -> VST(SrcType.X,   VstuType.dummy),
    VLM_V         -> VLD(SrcType.X,   VlduType.dummy, mask = T),
    VSM_V         -> VST(SrcType.X,   VstuType.dummy, mask = T),
    // 7.5. Vector Strided Instructions
    VLSE8_V       -> VLD(SrcType.xp,  VlduType.dummy, strided = T),
    VLSE16_V      -> VLD(SrcType.xp,  VlduType.dummy, strided = T),
    VLSE32_V      -> VLD(SrcType.xp,  VlduType.dummy, strided = T),
    VLSE64_V      -> VLD(SrcType.xp,  VlduType.dummy, strided = T),
    VSSE8_V       -> VST(SrcType.xp,  VstuType.dummy, strided = T),
    VSSE16_V      -> VST(SrcType.xp,  VstuType.dummy, strided = T),
    VSSE32_V      -> VST(SrcType.xp,  VstuType.dummy, strided = T),
    VSSE64_V      -> VST(SrcType.xp,  VstuType.dummy, strided = T),
    // 7.6. Vector Indexed Instructions
    VLUXEI8_V     -> VLD(SrcType.vp,  VlduType.dummy, indexed = T, ordered = F),
    VLUXEI16_V    -> VLD(SrcType.vp,  VlduType.dummy, indexed = T, ordered = F),
    VLUXEI32_V    -> VLD(SrcType.vp,  VlduType.dummy, indexed = T, ordered = F),
    VLUXEI64_V    -> VLD(SrcType.vp,  VlduType.dummy, indexed = T, ordered = F),
    VLOXEI8_V     -> VLD(SrcType.vp,  VlduType.dummy, indexed = T, ordered = T),
    VLOXEI16_V    -> VLD(SrcType.vp,  VlduType.dummy, indexed = T, ordered = T),
    VLOXEI32_V    -> VLD(SrcType.vp,  VlduType.dummy, indexed = T, ordered = T),
    VLOXEI64_V    -> VLD(SrcType.vp,  VlduType.dummy, indexed = T, ordered = T),
    VSUXEI8_V     -> VLD(SrcType.vp,  VstuType.dummy, indexed = T, ordered = F),
    VSUXEI16_V    -> VST(SrcType.vp,  VstuType.dummy, indexed = T, ordered = F),
    VSUXEI32_V    -> VST(SrcType.vp,  VstuType.dummy, indexed = T, ordered = F),
    VSUXEI64_V    -> VST(SrcType.vp,  VstuType.dummy, indexed = T, ordered = F),
    VSOXEI8_V     -> VST(SrcType.vp,  VstuType.dummy, indexed = T, ordered = T),
    VSOXEI16_V    -> VST(SrcType.vp,  VstuType.dummy, indexed = T, ordered = T),
    VSOXEI32_V    -> VST(SrcType.vp,  VstuType.dummy, indexed = T, ordered = T),
    VSOXEI64_V    -> VST(SrcType.vp,  VstuType.dummy, indexed = T, ordered = T),
    // 7.7. Unit-stride Fault-Only-First Loads
    VLE8FF_V      -> VLD(SrcType.X,   VlduType.dummy, ff = T),
    VLE16FF_V     -> VLD(SrcType.X,   VlduType.dummy, ff = T),
    VLE32FF_V     -> VLD(SrcType.X,   VlduType.dummy, ff = T),
    VLE64FF_V     -> VLD(SrcType.X,   VlduType.dummy, ff = T),
    // 7.8. Vector Load/Store Segment Instructions
    // 7.8.1. Vector Unit-Stride Segment Loads and Stores
    // TODO
    // 7.8.2. Vector Strided Segment Loads and Stores
    // TODO
    // 7.8.3. Vector Indexed Segment Loads and Stores
    // TODO
    // 7.9. Vector Load/Store Whole Register Instructions
    VL1RE8_V      -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL1RE16_V     -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL1RE32_V     -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL1RE64_V     -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL2RE8_V      -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL2RE16_V     -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL2RE32_V     -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL2RE64_V     -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL4RE8_V      -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL4RE16_V     -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL4RE32_V     -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL4RE64_V     -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL8RE8_V      -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL8RE16_V     -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL8RE32_V     -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VL8RE64_V     -> VLD(SrcType.X,   VlduType.dummy, whole = T),
    VS1R_V        -> VST(SrcType.X,   VlduType.dummy, whole = T),
    VS2R_V        -> VST(SrcType.X,   VlduType.dummy, whole = T),
    VS4R_V        -> VST(SrcType.X,   VlduType.dummy, whole = T),
    VS8R_V        -> VST(SrcType.X,   VlduType.dummy, whole = T),
  )

  override val decodeArray: Array[(BitPat, XSDecodeBase)] = vset ++ vls ++
    opivv ++ opivx ++ opivi ++ opmvv ++ opmvx ++ opfvv ++ opfvf
}

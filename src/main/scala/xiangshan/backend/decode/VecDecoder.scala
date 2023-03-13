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
import yunsuan.{VfpuType, VipuType, VppuType}

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

case class OPIVV(src3: BitPat, fu: BitPat, fuOp: BitPat, vWen: Boolean, mWen: Boolean, vxsatWen: Boolean, uopDivType: BitPat = UopDivType.VEC_LMUL) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    XSDecode(SrcType.vp, SrcType.vp, src3, fu, fuOp, SelImm.X, uopDivType,
      xWen = F, fWen = F, vWen = vWen, mWen = mWen, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class OPIVX(src3: BitPat, fu: BitPat, fuOp: BitPat, vWen: Boolean, mWen: Boolean, vxsatWen: Boolean, uopDivType: BitPat = UopDivType.VEC_MV_LMUL) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    XSDecode(SrcType.xp, SrcType.vp, src3, fu, fuOp, SelImm.X, uopDivType,
      xWen = F, fWen = F, vWen = vWen, mWen = mWen, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class OPIVI(src3: BitPat, fu: BitPat, fuOp: BitPat, vWen: Boolean, mWen: Boolean, vxsatWen: Boolean, selImm: BitPat, uopDivType: BitPat = UopDivType.VEC_LMUL) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    XSDecode(SrcType.imm, SrcType.vp, src3, fu, fuOp, selImm, uopDivType,
      xWen = F, fWen = F, vWen = vWen, mWen = mWen, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class OPMVV(vdRen: Boolean, fu: BitPat, fuOp: BitPat, xWen: Boolean, vWen: Boolean, mWen: Boolean, uopDivType: BitPat = UopDivType.dummy) extends XSDecodeBase {
  private def src3: BitPat = if (vdRen) SrcType.vp else SrcType.X
  def generate() : List[BitPat] = {
    XSDecode(SrcType.vp, SrcType.vp, src3, fu, fuOp, SelImm.X, uopDivType, xWen, F, vWen, mWen, F, F, F, F).generate()
  }
}

case class OPMVX(vdRen: Boolean, fu: BitPat, fuOp: BitPat, xWen: Boolean, vWen: Boolean, mWen: Boolean, uopDivType: BitPat = UopDivType.dummy) extends XSDecodeBase {
  private def src3: BitPat = if (vdRen) SrcType.vp else SrcType.X
  def generate() : List[BitPat] = {
    XSDecode(SrcType.xp, SrcType.vp, src3, fu, fuOp, SelImm.X, uopDivType,
      xWen = xWen, fWen = F, vWen = vWen, mWen = mWen, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class OPFVV(src1:BitPat, src3:BitPat, fu: BitPat, fuOp: BitPat, fWen: Boolean, vWen: Boolean, mWen: Boolean, uopDivType: BitPat = UopDivType.dummy) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    XSDecode(src1, SrcType.vp, src3, fu, fuOp, SelImm.X, uopDivType,
      xWen = F, fWen = fWen, vWen = vWen, mWen = mWen, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class OPFVF(src1:BitPat, src3:BitPat, fu: BitPat, fuOp: BitPat, fWen: Boolean, vWen: Boolean, mWen: Boolean, uopDivType: BitPat = UopDivType.dummy) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    XSDecode(src1, SrcType.vp, src3, fu, fuOp, SelImm.X, uopDivType,
      xWen = F, fWen = fWen, vWen = vWen, mWen = mWen, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class VSET(vli: Boolean, vtypei: Boolean, fuOp: BitPat, flushPipe: Boolean, selImm: BitPat, uopDivType: BitPat = UopDivType.DIR) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    val src1 = if (vli) SrcType.imm else SrcType.xp
    val src2 = if (vtypei) SrcType.imm else SrcType.xp
    XSDecode(src1, src2, SrcType.X, FuType.alu, fuOp, selImm, uopDivType,
      xWen = T, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = flushPipe).generate()
  }
}

case class VLD(src2: BitPat, fuOp: BitPat, strided: Boolean = false, indexed: Boolean = false, ff: Boolean = false,
  mask: Boolean = false, whole: Boolean = false, ordered: Boolean = false, uopDivType: BitPat = UopDivType.dummy) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    val fu = FuType.vldu
    val src1 = SrcType.xp
    val src3 = SrcType.X
    XSDecode(src1, src2, src3, fu, fuOp, SelImm.X, uopDivType,
      xWen = F, fWen = F, vWen = T, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

case class VST(src2: BitPat, fuOp: BitPat, strided: Boolean = false, indexed: Boolean = false,
  mask: Boolean = false, whole: Boolean = false, ordered: Boolean = false, uopDivType: BitPat = UopDivType.dummy) extends XSDecodeBase {
  def generate() : List[BitPat] = {
    val fu = FuType.vstu
    val src1 = SrcType.xp
    val src3 = SrcType.vp
    XSDecode(src1, src2, src3, fu, fuOp, SelImm.X, uopDivType,
      xWen = F, fWen = F, vWen = F, mWen = F, xsTrap = F, noSpec = F, blockBack = F, flushPipe = F).generate()
  }
}

object VecDecoder extends DecodeConstants {
  val opivv: Array[(BitPat, XSDecodeBase)] = Array(
    VADD_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.vadd_vv, T, F, F),
    VSUB_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.vsub_vv, T, F, F),

    VMINU_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.vminu_vv, T, F, F),
    VMIN_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.vmin_vv, T, F, F),
    VMAXU_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.vmaxu_vv, T, F, F),
    VMAX_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.vmax_vv, T, F, F),

    VAND_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.vand_vv, T, F, F),
    VOR_VV          -> OPIVV(SrcType.X, FuType.vipu, VipuType.vor_vv, T, F, F),
    VXOR_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.vxor_vv, T, F, F),

    VRGATHER_VV     -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F),
    VRGATHEREI16_VV -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F),

    VADC_VVM        -> OPIVV(SrcType.vp, FuType.vipu, VipuType.vadc_vvm, T, F, F),
    VMADC_VVM       -> OPIVV(SrcType.vp, FuType.vipu, VipuType.vmadc_vvm, F, T, F),
    VMADC_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.vmadc_vv, F, T, F),

    VSBC_VVM        -> OPIVV(SrcType.vp, FuType.vipu, VipuType.vsbc_vvm, T, F, F),
    VMSBC_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.vmsbc_vvm, F, T, F),
    VMSBC_VVM       -> OPIVV(SrcType.vp, FuType.vipu, VipuType.vmsbc_vv, F, T, F),

    VMERGE_VVM      -> OPIVV(SrcType.vp, FuType.vipu, VipuType.vmerge_vvm, T, F, F),

    VMV_V_V      -> OPIVV(SrcType.vp, FuType.vipu, VipuType.vmv_v_v, T, F, F),

    VMSEQ_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.vmseq_vv, F, T, F),
    VMSNE_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.vmsne_vv, F, T, F),
    VMSLTU_VV       -> OPIVV(SrcType.X, FuType.vipu, VipuType.vmsltu_vv, F, T, F),
    VMSLT_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.vmslt_vv, F, T, F),
    VMSLEU_VV       -> OPIVV(SrcType.X, FuType.vipu, VipuType.vmsleu_vv, F, T, F),
    VMSLE_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.vmsle_vv, F, T, F),

    VSLL_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.vsll_vv, T, F, F),
    VSRL_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.vsrl_vv, T, F, F),
    VSRA_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.vsra_vv, T, F, F),
    VNSRL_WV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.vnsrl_wv, T, F, F, UopDivType.VEC_NARROW),
    VNSRA_WV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.vnsra_wv, T, F, F, UopDivType.VEC_NARROW),

    VSADDU_VV       -> OPIVV(SrcType.X, FuType.vipu, VipuType.vsaddu_vv, T, F, T),
    VSADD_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.vsadd_vv, T, F, T),
    VSSUBU_VV       -> OPIVV(SrcType.X, FuType.vipu, VipuType.vssubu_vv, T, F, T),
    VSSUB_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.vssub_vv, T, F, T),

    VSMUL_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T),

    VSSRL_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.vssrl_vv, T, F, F),
    VSSRA_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.vssra_vv, T, F, F),

    VNCLIPU_WV      -> OPIVV(SrcType.X, FuType.vipu, VipuType.vnclipu_wv, T, F, T, UopDivType.VEC_NARROW),
    VNCLIP_WV       -> OPIVV(SrcType.X, FuType.vipu, VipuType.vnclip_wv, T, F, T, UopDivType.VEC_NARROW),

    VWREDSUMU_VS    -> OPIVV(SrcType.X, FuType.vipu, VipuType.vwredsumu_vs, T, F, F),
    VWREDSUM_VS     -> OPIVV(SrcType.X, FuType.vipu, VipuType.vwredsum_vs, T, F, F),
  )

  val opivx: Array[(BitPat, XSDecodeBase)] = Array(
    VADD_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.vadd_vv, T, F, F),
    VSUB_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.vsub_vv, T, F, F),
    VRSUB_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vrsub_vv, T, F, F),

    VMINU_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vminu_vv, T, F, F),
    VMIN_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.vmin_vv, T, F, F),
    VMAXU_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vmaxu_vv, T, F, F),
    VMAX_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.vmax_vv, T, F, F),

    VAND_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.vand_vv, T, F, F),
    VOR_VX        -> OPIVX(SrcType.X, FuType.vipu, VipuType.vor_vv, T, F, F),
    VXOR_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.vxor_vv, T, F, F),

    VRGATHER_VX   -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F),

    VSLIDEUP_VX   -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F),
    VSLIDEDOWN_VX -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F),

    VADC_VXM      -> OPIVX(SrcType.vp, FuType.vipu, VipuType.vadc_vvm, T, F, F),
    VMADC_VXM      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vmadc_vvm, F, T, F),
    VMADC_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vmadc_vv, F, T, F),
    VSBC_VXM      -> OPIVX(SrcType.vp, FuType.vipu, VipuType.vsbc_vvm, T, F, F),
    VMSBC_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vmsbc_vvm, F, T, F),
    VMSBC_VXM     -> OPIVX(SrcType.vp, FuType.vipu, VipuType.vmsbc_vv, F, T, F),

    VMERGE_VXM    -> OPIVX(SrcType.vp, FuType.vipu, VipuType.vmerge_vvm, T, F, F),
    
    VMV_V_X    -> OPIVX(SrcType.vp, FuType.vipu, VipuType.vmv_v_v, T, F, F),

    VMSEQ_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vmseq_vv, F, T, F),
    VMSNE_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vmsne_vv, F, T, F),
    VMSLTU_VX     -> OPIVX(SrcType.X, FuType.vipu, VipuType.vmsltu_vv, F, T, F),
    VMSLT_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vmslt_vv, F, T, F),
    VMSLEU_VX     -> OPIVX(SrcType.X, FuType.vipu, VipuType.vmsleu_vv, F, T, F),
    VMSLE_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vmsle_vv, F, T, F),
    VMSGTU_VX     -> OPIVX(SrcType.X, FuType.vipu, VipuType.vmsgtu_vv, F, T, F),
    VMSGT_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vmsgt_vv, F, T, F),

    VSLL_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.vsll_vv, T, F, F),
    VSRL_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.vsrl_vv, T, F, F),
    VSRA_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.vsra_vv, T, F, F),
    VNSRL_WX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vnsrl_wv, T, F, F, UopDivType.VEC_MV_NARROW),
    VNSRA_WX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vnsra_wv, T, F, F, UopDivType.VEC_MV_NARROW),

    VSADDU_VX     -> OPIVX(SrcType.X, FuType.vipu, VipuType.vsaddu_vv, T, F, T),
    VSADD_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vsadd_vv, T, F, T),
    VSSUBU_VX     -> OPIVX(SrcType.X, FuType.vipu, VipuType.vssubu_vv, T, F, T),
    VSSUB_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vssub_vv, T, F, T),


    VSMUL_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T),

    VSSRL_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vssrl_vv, T, F, F),
    VSSRA_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.vssra_vv, T, F, F),

    VNCLIPU_WX    -> OPIVX(SrcType.X, FuType.vipu, VipuType.vnclipu_wv, T, F, T, UopDivType.VEC_MV_NARROW),
    VNCLIP_WX     -> OPIVX(SrcType.X, FuType.vipu, VipuType.vnclip_wv, T, F, T, UopDivType.VEC_MV_NARROW),
  )

  val opivi: Array[(BitPat, XSDecodeBase)] = Array(
    VADD_VI       -> OPIVI(SrcType.X, FuType.vipu, VipuType.vadd_vv,   T, F, F, SelImm.IMM_OPIVIS),
    VRSUB_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.vrsub_vv, T, F, F, SelImm.IMM_OPIVIS),

    VAND_VI       -> OPIVI(SrcType.X, FuType.vipu, VipuType.vand_vv, T, F, F, SelImm.IMM_OPIVIS),
    VOR_VI        -> OPIVI(SrcType.X, FuType.vipu, VipuType.vor_vv, T, F, F, SelImm.IMM_OPIVIS),
    VXOR_VI       -> OPIVI(SrcType.X, FuType.vipu, VipuType.vxor_vv, T, F, F, SelImm.IMM_OPIVIS),

    VRGATHER_VI   -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIU),

    VSLIDEUP_VI   -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIU),
    VSLIDEDOWN_VI -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIU),

    VADC_VIM      -> OPIVI(SrcType.vp, FuType.vipu, VipuType.vadc_vvm, T, F, F, SelImm.IMM_OPIVIS),
    VMADC_VIM     -> OPIVI(SrcType.X, FuType.vipu, VipuType.vmadc_vvm, T, F, F, SelImm.IMM_OPIVIS),
    VMADC_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.vmadc_vv, T, F, F, SelImm.IMM_OPIVIS),

    VMERGE_VIM    -> OPIVI(SrcType.vp, FuType.vipu, VipuType.vmerge_vvm, T, F, F, SelImm.IMM_OPIVIS),
    
    VMV_V_I    -> OPIVI(SrcType.vp, FuType.vipu, VipuType.vmv_v_v, T, F, F, SelImm.IMM_OPIVIS),

    VMSEQ_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.vmseq_vv, F, T, F, SelImm.IMM_OPIVIS),
    VMSNE_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.vmsne_vv, F, T, F, SelImm.IMM_OPIVIS),
    VMSLEU_VI     -> OPIVI(SrcType.X, FuType.vipu, VipuType.vmsleu_vv, F, T, F, SelImm.IMM_OPIVIS),
    VMSLE_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.vmsle_vv, F, T, F, SelImm.IMM_OPIVIS),
    VMSGTU_VI     -> OPIVI(SrcType.X, FuType.vipu, VipuType.vmsgtu_vv, F, T, F, SelImm.IMM_OPIVIS),
    VMSGT_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.vmsgt_vv, F, T, F, SelImm.IMM_OPIVIS),

    VSLL_VI       -> OPIVI(SrcType.X, FuType.vipu, VipuType.vsll_vv, T, F, F, SelImm.IMM_OPIVIU),
    VSRL_VI       -> OPIVI(SrcType.X, FuType.vipu, VipuType.vsrl_vv, T, F, F, SelImm.IMM_OPIVIU),
    VSRA_VI       -> OPIVI(SrcType.X, FuType.vipu, VipuType.vsra_vv, T, F, F, SelImm.IMM_OPIVIU),
    VNSRL_WI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.vnsrl_wv, T, F, F, SelImm.IMM_OPIVIU, UopDivType.VEC_NARROW),
    VNSRA_WI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.vnsra_wv, T, F, F, SelImm.IMM_OPIVIU, UopDivType.VEC_NARROW),

    VSADDU_VI     -> OPIVI(SrcType.X, FuType.vipu, VipuType.vsaddu_vv, T, F, T, SelImm.IMM_OPIVIS),
    VSADD_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.vsadd_vv, T, F, T, SelImm.IMM_OPIVIS),

    VSSRL_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.vssrl_vv, T, F, F, SelImm.IMM_OPIVIU),
    VSSRA_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.vssra_vv, T, F, F, SelImm.IMM_OPIVIU),

    VNCLIPU_WI    -> OPIVI(SrcType.X, FuType.vipu, VipuType.vnclipu_wv, T, F, T, SelImm.IMM_OPIVIU, UopDivType.VEC_NARROW),
    VNCLIP_WI     -> OPIVI(SrcType.X, FuType.vipu, VipuType.vnclip_wv, T, F, T, SelImm.IMM_OPIVIU, UopDivType.VEC_NARROW),

    VMV1R_V       -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS),
    VMV2R_V       -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS),
    VMV4R_V       -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS),
    VMV8R_V       -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS),
  )

  val opmvv: Array[(BitPat, XSDecodeBase)] = Array(
    VAADD_VV     -> OPMVV(F, FuType.vipu, VipuType.vaadd_vv, F, T, F),
    VAADDU_VV    -> OPMVV(F, FuType.vipu, VipuType.vaaddu_vv, F, T, F),
    VASUB_VV     -> OPMVV(F, FuType.vipu, VipuType.vasub_vv, F, T, F),
    VASUBU_VV    -> OPMVV(F, FuType.vipu, VipuType.vasubu_vv, F, T, F),
    VCOMPRESS_VM -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F),
    VCPOP_M      -> OPMVV(F, FuType.vipu, VipuType.vcpop_m, T, F, F),
    VDIV_VV      -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F),
    VDIVU_VV     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F),
    VFIRST_M     -> OPMVV(F, FuType.vipu, VipuType.vfirst_m, T, F, F),
    VID_V        -> OPMVV(F, FuType.vipu, VipuType.vid_v, F, T, F),
    VIOTA_M      -> OPMVV(F, FuType.vipu, VipuType.viota_m, F, T, F),

    // VMACC_VV     -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),

    VMADD_VV     -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VMAND_MM     -> OPMVV(F, FuType.vipu, VipuType.vmand_mm, F, T, F),
    VMANDN_MM    -> OPMVV(F, FuType.vipu, VipuType.vmandn_mm, F, T, F),
    VMNAND_MM    -> OPMVV(F, FuType.vipu, VipuType.vmnand_mm, F, T, F),
    VMNOR_MM     -> OPMVV(F, FuType.vipu, VipuType.vmnor_mm, F, T, F),
    VMOR_MM      -> OPMVV(F, FuType.vipu, VipuType.vmor_mm, F, T, F),
    VMORN_MM     -> OPMVV(F, FuType.vipu, VipuType.vmorn_mm, F, T, F),
    VMXNOR_MM    -> OPMVV(F, FuType.vipu, VipuType.vmxnor_mm, F, T, F),
    VMXOR_MM     -> OPMVV(F, FuType.vipu, VipuType.vmxor_mm, F, T, F),
    VMSBF_M      -> OPMVV(F, FuType.vipu, VipuType.vmsbf_m, F, T, F),
    VMSIF_M      -> OPMVV(F, FuType.vipu, VipuType.vmsif_m, F, T, F),
    VMSOF_M      -> OPMVV(F, FuType.vipu, VipuType.vmsof_m, F, T, F),
    VMUL_VV      -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F),
    VMULH_VV     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F),
    VMULHSU_VV   -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F),
    VMULHU_VV    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F),

    VMV_X_S      -> OPMVV(F, FuType.vipu, VipuType.dummy, T, F, F),
    VNMSAC_VV    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F),
    VNMSUB_VV    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F),
    VREDAND_VS   -> OPMVV(F, FuType.vipu, VipuType.vredand_vs, F, T, F),
    VREDMAX_VS   -> OPMVV(F, FuType.vipu, VipuType.vredmax_vs, F, T, F),
    VREDMAXU_VS  -> OPMVV(F, FuType.vipu, VipuType.vredmaxu_vs, F, T, F),
    VREDMIN_VS   -> OPMVV(F, FuType.vipu, VipuType.vredmin_vs, F, T, F),
    VREDMINU_VS  -> OPMVV(F, FuType.vipu, VipuType.vredminu_vs, F, T, F),
    VREDOR_VS    -> OPMVV(F, FuType.vipu, VipuType.vredor_vs, F, T, F),
    VREDSUM_VS   -> OPMVV(F, FuType.vipu, VipuType.vredsum_vs, F, T, F),
    VREDXOR_VS   -> OPMVV(F, FuType.vipu, VipuType.vredxor_vs, F, T, F),
    VREM_VV      -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F),
    VREMU_VV     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F),
    VSEXT_VF2    -> OPMVV(F, FuType.vipu, VipuType.vsext_vf2, F, T, F, UopDivType.VEC_EXT2),
    VSEXT_VF4    -> OPMVV(F, FuType.vipu, VipuType.vsext_vf4, F, T, F, UopDivType.VEC_EXT4),
    VSEXT_VF8    -> OPMVV(F, FuType.vipu, VipuType.vsext_vf8, F, T, F, UopDivType.VEC_EXT8),
    VZEXT_VF2    -> OPMVV(F, FuType.vipu, VipuType.vzext_vf2, F, T, F, UopDivType.VEC_EXT2),
    VZEXT_VF4    -> OPMVV(F, FuType.vipu, VipuType.vzext_vf4, F, T, F, UopDivType.VEC_EXT4),
    VZEXT_VF8    -> OPMVV(F, FuType.vipu, VipuType.vzext_vf8, F, T, F, UopDivType.VEC_EXT8),
    VWADD_VV     -> OPMVV(F, FuType.vipu, VipuType.vwadd_vv, F, T, F, UopDivType.VEC_WIDE),
    VWADD_WV     -> OPMVV(F, FuType.vipu, VipuType.vwadd_wv, F, T, F, UopDivType.VEC_WIDE0),
    VWADDU_VV    -> OPMVV(F, FuType.vipu, VipuType.vwaddu_vv, F, T, F, UopDivType.VEC_WIDE),
    VWADDU_WV    -> OPMVV(F, FuType.vipu, VipuType.vwaddu_wv, F, T, F, UopDivType.VEC_WIDE0),
    VWMACC_VV    -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMACCSU_VV  -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMACCU_VV   -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMUL_VV     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F),
    VWMULSU_VV   -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F),
    VWMULU_VV    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F),
    VWSUB_VV     -> OPMVV(F, FuType.vipu, VipuType.vwsub_vv, F, T, F, UopDivType.VEC_WIDE),
    VWSUB_WV     -> OPMVV(F, FuType.vipu, VipuType.vwsub_wv, F, T, F, UopDivType.VEC_WIDE0),
    VWSUBU_VV    -> OPMVV(F, FuType.vipu, VipuType.vwsubu_vv, F, T, F, UopDivType.VEC_WIDE),
    VWSUBU_WV    -> OPMVV(F, FuType.vipu, VipuType.vwsubu_wv, F, T, F, UopDivType.VEC_WIDE0),
  )

  val opmvx: Array[(BitPat, XSDecodeBase)] = Array(
    VAADD_VX       -> OPMVX(F, FuType.vipu, VipuType.vaadd_vv, F, T, F),
    VAADDU_VX      -> OPMVX(F, FuType.vipu, VipuType.vaaddu_vv, F, T, F),
    VASUB_VX       -> OPMVX(F, FuType.vipu, VipuType.vasub_vv, F, T, F),
    VASUBU_VX      -> OPMVX(F, FuType.vipu, VipuType.vasubu_vv, F, T, F),
    VDIV_VX        -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),
    VDIVU_VX       -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),
    VMACC_VX       -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VMADD_VX       -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VMUL_VX        -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),
    VMULH_VX       -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),
    VMULHSU_VX     -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),
    VMULHU_VX      -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),
    VMV_S_X        -> OPMVX(F, FuType.vipu, VipuType.vmv_s_x, F, T, F, UopDivType.VEC_MV),

    VNMSAC_VX      -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),
    VNMSUB_VX      -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),
    VREM_VX        -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),
    VREMU_VX       -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),

    VSLIDE1DOWN_VX -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),
    VSLIDE1UP_VX   -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),
    VWADD_VX       -> OPMVX(F, FuType.vipu, VipuType.vwadd_vv, F, T, F, UopDivType.VEC_MV_WIDE),
    VWADD_WX       -> OPMVX(F, FuType.vipu, VipuType.vwadd_wv, F, T, F, UopDivType.VEC_MV_WIDE0),
    VWADDU_VX      -> OPMVX(F, FuType.vipu, VipuType.vwaddu_vv, F, T, F, UopDivType.VEC_MV_WIDE),
    VWADDU_WX      -> OPMVX(F, FuType.vipu, VipuType.vwaddu_wv, F, T, F, UopDivType.VEC_MV_WIDE0),

    // OutOfMemoryError
    VWMACC_VX      -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMACCSU_VX    -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMACCU_VX     -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),

    VWMACCUS_VX    -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F),
    VWMUL_VX       -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),
    VWMULSU_VX     -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),
    // Ok
    VWMULU_VX      -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F),
    VWSUB_VX       -> OPMVX(F, FuType.vipu, VipuType.vwsub_vv, F, T, F, UopDivType.VEC_MV_WIDE),
    VWSUB_WX       -> OPMVX(F, FuType.vipu, VipuType.vwsub_wv, F, T, F, UopDivType.VEC_MV_WIDE0),
    VWSUBU_VX      -> OPMVX(F, FuType.vipu, VipuType.vwsubu_vv, F, T, F),
    VWSUBU_WX      -> OPMVX(F, FuType.vipu, VipuType.vwsubu_wv, F, T, F),
  )

  val opfvv: Array[(BitPat, XSDecodeBase)] = Array(
    // 13.2. Vector Single-Width Floating-Point Add/Subtract Instructions
    VFADD_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.fadd , F, T, F),
    VFSUB_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.fsub, F, T, F),

    // 13.3. Vector Widening Floating-Point Add/Subtract Instructions
    VFWADD_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWSUB_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWADD_WV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWSUB_WV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.4. Vector Single-Width Floating-Point Multiply/Divide Instructions
    VFMUL_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFDIV_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.fdiv , F, T, F),

    // 13.5. Vector Widening Floating-Point Multiply
    VFWMUL_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.6. Vector Single-Width Floating-Point Fused Multiply-Add Instructions
    VFMACC_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.fmacc, F, T, F),
    VFNMACC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFMSAC_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFNMSAC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFMADD_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFNMADD_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFMSUB_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFNMSUB_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.7. Vector Widening Floating-Point Fused Multiply-Add Instructions
    VFWMACC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWNMACC_VV        -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWMSAC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWNMSAC_VV        -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.8. Vector Floating-Point Square-Root Instruction
    VFSQRT_V           -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.9. Vector Floating-Point Reciprocal Square-Root Estimate Instruction
    VFRSQRT7_V         -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.10. Vector Floating-Point Reciprocal Estimate Instruction
    VFREC7_V           -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.11. Vector Floating-Point MIN/MAX Instructions
    VFMIN_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFMAX_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.12. Vector Floating-Point Sign-Injection Instructions
    VFSGNJ_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFSGNJN_VV         -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFSGNJX_VV         -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.13. Vector Floating-Point Compare Instructions
    VMFEQ_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VMFNE_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VMFLT_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VMFLE_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.14. Vector Floating-Point Classify Instruction
    VFCLASS_V          -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

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
    VFMV_F_S           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),// f[rd] = vs2[0] (rs1=0)
  )

  val opfvf: Array[(BitPat, XSDecodeBase)] = Array(
    // 13.2. Vector Single-Width Floating-Point Add/Subtract Instructions
    VFADD_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.fadd , F, T, F),
    VFSUB_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.fsub, F, T, F),
    VFRSUB_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.3. Vector Widening Floating-Point Add/Subtract Instructions
    VFWADD_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWSUB_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWADD_WF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFWSUB_WF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.4. Vector Single-Width Floating-Point Multiply/Divide Instructions
    VFMUL_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFDIV_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.fdiv , F, T, F),
    VFRDIV_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.5. Vector Widening Floating-Point Multiply
    VFWMUL_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

    // 13.6. Vector Single-Width Floating-Point Fused Multiply-Add Instructions
    VFMACC_VF          -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.fmacc, F, T, F),
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
    VFMIN_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),
    VFMAX_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),

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
    VFMV_S_F           -> OPFVF(SrcType.fp, SrcType.vp, FuType.vppu, VppuType.f2s  , F, T, F),// vs2=0 // vs3 = vd

    // 16.3.3. Vector Slide1up
    VFSLIDE1UP_VF      -> OPFVF(SrcType.fp, SrcType.X , FuType.vppu, VppuType.vslide1up, F, T, F),// vd[0]=f[rs1], vd[i+1] = vs2[i]

    // 16.3.4. Vector Slide1down Instruction
    // vslide1down.vx vd, vs2, rs1, vm # vd[i] = vs2[i+1], vd[vl-1]=x[rs1]
    VFSLIDE1DOWN_VF    -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F),// vd[i] = vs2[i+1], vd[vl-1]=f[rs1]
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

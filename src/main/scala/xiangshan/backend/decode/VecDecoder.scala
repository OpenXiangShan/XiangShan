package xiangshan.backend.decode

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.util.uintToBitPat
import freechips.rocketchip.rocket.Instructions._
import utils._
import xiangshan.ExceptionNO.illegalInstr
import xiangshan._
import yunsuan.{VipuType, VfpuType}

abstract class VecType {
  def X = BitPat("b?")
  def N = BitPat("b0")
  def Y = BitPat("b1")
  def generate() : List[BitPat]
  def asOldDecodeOutput(): List[BitPat] = {
    val src1::src2::src3::fu::fuOp::xWen::fWen::vWen::mWen::xsTrap::noSpec::blockBack::flushPipe::selImm::Nil = generate()
    List (src1, src2, src3, fu, fuOp, xWen, fWen, xsTrap, noSpec, blockBack, flushPipe, selImm)
  }
}

case class OPIVV(op0: BitPat, fu: BitPat, fuOp: BitPat, vWen: Boolean, mWen: Boolean, vxsatWen: Boolean) extends VecType {
  def generate() : List[BitPat] = {
    List (SrcType.vp, SrcType.vp, op0, fu, fuOp, N, N, vWen.B, mWen.B, vxsatWen.B, N, N, N, SelImm.X)
  }
}

case class OPIVX(op0: BitPat, fu: BitPat, fuOp: BitPat, vWen: Boolean, mWen: Boolean, vxsatWen: Boolean) extends VecType {
  def generate() : List[BitPat] = {
    List (SrcType.xp, SrcType.vp, op0, fu, fuOp, N, N, vWen.B, mWen.B, vxsatWen.B, N, N, N, SelImm.X)
  }
}

case class OPIVI(op0: BitPat, fu: BitPat, fuOp: BitPat, vWen: Boolean, mWen: Boolean, vxsatWen: Boolean, selImm: BitPat) extends VecType {
  def generate() : List[BitPat] = {
    List (SrcType.imm, SrcType.vp, op0, fu, fuOp, N, N, vWen.B, mWen.B, vxsatWen.B, N, N, N, selImm)
  }
}

case class OPMVV(vdRen: Boolean, fu: BitPat, fuOp: BitPat, xWen: Boolean, vWen: Boolean, mWen: Boolean, others: Any) extends VecType {
  private def src3: BitPat = if (vdRen) SrcType.vp else SrcType.X
  def generate() : List[BitPat] = {
    List (SrcType.vp, SrcType.vp, src3, fu, fuOp, xWen.B, N, vWen.B, mWen.B, N, N, N, N, SelImm.X)
  }
}

case class OPMVX(vdRen: Boolean, fu: BitPat, fuOp: BitPat, xWen: Boolean, vWen: Boolean, mWen: Boolean, others: Any) extends VecType {
  private def src3: BitPat = if (vdRen) SrcType.vp else SrcType.X
  def generate() : List[BitPat] = {
    List (SrcType.xp, SrcType.vp, src3, fu, fuOp, xWen.B, N, vWen.B, mWen.B, N, N, N, N, SelImm.X)
  }
}

case class OPFVV(src1:BitPat, src3:BitPat, fu: BitPat, fuOp: BitPat,  fWen: Boolean, vWen: Boolean, mWen: Boolean) extends VecType {
  def generate() : List[BitPat] = {
    List (src1, SrcType.vp, src3, fu, fuOp, N, fWen.B, vWen.B, mWen.B, N, N, N, N, SelImm.X)
  }
}

case class OPFVF(src1:BitPat, src3:BitPat, fu: BitPat, fuOp: BitPat, fWen: Boolean, vWen: Boolean, mWen: Boolean) extends VecType {
  def generate() : List[BitPat] = {
    List (src1, SrcType.vp, src3, fu, fuOp, N, fWen.B, vWen.B, mWen.B, N, N, N, N, SelImm.X)
  }
}

case class VSET() extends VecType {
  def generate() : List[BitPat] = { null }
}

case class VLS() extends VecType {
  def generate() : List[BitPat] = { null }
}

object VecDecoder extends DecodeConstants {
  private def F = false
  private def T = true

  val opivvTable: Array[(BitPat, List[BitPat])] = Array(
    VADD_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VSUB_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VMINU_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VMIN_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VMAXU_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VMAX_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VAND_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VOR_VV          -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VXOR_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VRGATHER_VV     -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VRGATHEREI16_VV -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VADC_VVM        -> OPIVV(SrcType.vp, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VMADC_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMADC_VVM       -> OPIVV(SrcType.vp, FuType.vipu, VipuType.dummy, F, T, F).generate(),

    VSBC_VVM        -> OPIVV(SrcType.vp, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VMSBC_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMSBC_VVM       -> OPIVV(SrcType.vp, FuType.vipu, VipuType.dummy, F, T, F).generate(),

    VMERGE_VVM      -> OPIVV(SrcType.vp, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VMSEQ_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMSNE_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMSLTU_VV       -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMSLT_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMSLEU_VV       -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMSLE_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),

    VSLL_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VSRL_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VSRA_VV         -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VNSRL_WV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VNSRA_WV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VSADDU_VV       -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T).generate(),
    VSADD_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T).generate(),
    VSSUBU_VV       -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T).generate(),
    VSSUB_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T).generate(),

    VSMUL_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T).generate(),

    VSSRL_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VSSRA_VV        -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VNCLIPU_WV      -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T).generate(),
    VNCLIP_WV       -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T).generate(),

    VWREDSUMU_VS    -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VWREDSUM_VS     -> OPIVV(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
  )

  val opivxTable: Array[(BitPat, List[BitPat])] = Array(
    VADD_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VSUB_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VRSUB_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VMINU_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VMIN_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VMAXU_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VMAX_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VAND_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VOR_VX        -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VXOR_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VRGATHER_VX   -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VSLIDEUP_VX   -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VSLIDEDOWN_VX -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VADC_VXM      -> OPIVX(SrcType.vp, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VMADC_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VSBC_VXM      -> OPIVX(SrcType.vp, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VMSBC_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMSBC_VXM     -> OPIVX(SrcType.vp, FuType.vipu, VipuType.dummy, F, T, F).generate(),

    VMERGE_VXM    -> OPIVX(SrcType.vp, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VMSEQ_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMSNE_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMSLTU_VX     -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMSLT_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMSLEU_VX     -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMSLE_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMSGTU_VX     -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),
    VMSGT_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F).generate(),

    VSLL_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VSRL_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VSRA_VX       -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VNSRL_WX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VNSRA_WX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VSADDU_VX     -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T).generate(),
    VSADD_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T).generate(),
    VSSUBU_VX     -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T).generate(),
    VSSUB_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T).generate(),


    VSMUL_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T).generate(),

    VSSRL_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),
    VSSRA_VX      -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F).generate(),

    VNCLIPU_WV    -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T).generate(),
    VNCLIP_WV     -> OPIVX(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T).generate(),
  )

  val opiviTable: Array[(BitPat, List[BitPat])] = Array(
    VADD_VI       -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS).generate(),
    VRSUB_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS).generate(),

    VAND_VI       -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS).generate(),
    VOR_VI        -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS).generate(),
    VXOR_VI       -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS).generate(),

    VRGATHER_VI   -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIU).generate(),

    VSLIDEUP_VI   -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIU).generate(),
    VSLIDEDOWN_VI -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIU).generate(),

    VADC_VIM      -> OPIVI(SrcType.vp, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS).generate(),
    VMADC_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS).generate(),

    VMERGE_VIM    -> OPIVI(SrcType.vp, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS).generate(),

    VMSEQ_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F, SelImm.IMM_OPIVIS).generate(),
    VMSNE_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F, SelImm.IMM_OPIVIS).generate(),
    VMSLEU_VI     -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F, SelImm.IMM_OPIVIS).generate(),
    VMSLE_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F, SelImm.IMM_OPIVIS).generate(),
    VMSGTU_VI     -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F, SelImm.IMM_OPIVIS).generate(),
    VMSGT_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, F, T, F, SelImm.IMM_OPIVIS).generate(),

    VSLL_VI       -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIU).generate(),
    VSRL_VI       -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIU).generate(),
    VSRA_VI       -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIU).generate(),
    VNSRL_WI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIU).generate(),
    VNSRA_WI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIU).generate(),

    VSADDU_VI     -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T, SelImm.IMM_OPIVIS).generate(),
    VSADD_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T, SelImm.IMM_OPIVIS).generate(),

    VSSRL_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIU).generate(),
    VSSRA_VI      -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIU).generate(),

    VNCLIPU_WV    -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T, SelImm.IMM_OPIVIU).generate(),
    VNCLIP_WV     -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, T, SelImm.IMM_OPIVIU).generate(),

    VMV1R_V       -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS).generate(),
    VMV2R_V       -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS).generate(),
    VMV4R_V       -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS).generate(),
    VMV8R_V       -> OPIVI(SrcType.X, FuType.vipu, VipuType.dummy, T, F, F, SelImm.IMM_OPIVIS).generate(),
  )

  val opmvv: Array[(BitPat, OPMVV)] = Array(
    VAADD_VV     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VAADDU_VV    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VASUB_VV     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VASUBU_VV    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VCOMPRESS_VM -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VCPOP_M      -> OPMVV(F, FuType.vipu, VipuType.dummy, T, F, F, T),
    VDIV_VV      -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VDIVU_VV     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VFIRST_M     -> OPMVV(F, FuType.vipu, VipuType.dummy, T, F, F, T),
    VID_V        -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VIOTA_M      -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMACC_VV     -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMADD_VV     -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMAND_MM     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMANDN_MM    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMNAND_MM    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMNOR_MM     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMOR_MM      -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMORN_MM     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMXNOR_MM    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMXOR_MM     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMSBF_M      -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMSIF_M      -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMSOF_M      -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMUL_VV      -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMULH_VV     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMULHSU_VV   -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMULHU_VV    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMV_X_S      -> OPMVV(F, FuType.vipu, VipuType.dummy, T, F, F, T),
    VNMSAC_VV    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VNMSUB_VV    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VREDAND_VS   -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VREDMAX_VS   -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VREDMAXU_VS  -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VREDMIN_VS   -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VREDMINU_VS  -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VREDOR_VS    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VREDSUM_VS   -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VREDXOR_VS   -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VREM_VV      -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VREMU_VV     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VSEXT_VF2    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VSEXT_VF4    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VSEXT_VF8    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VZEXT_VF2    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VZEXT_VF4    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VZEXT_VF8    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWADD_VV     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWADD_WV     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWADDU_VV    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWADDU_WV    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWMACC_VV    -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWMACCSU_VV  -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWMACCU_VV   -> OPMVV(T, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWMUL_VV     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWMULSU_VV   -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWMULU_VV    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWSUB_VV     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWSUB_WV     -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWSUBU_VV    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWSUBU_WV    -> OPMVV(F, FuType.vipu, VipuType.dummy, F, T, F, T)
  )
  val opmvx: Array[(BitPat, OPMVX)] = Array(
    VAADD_VX       -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VAADDU_VX      -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VASUB_VX       -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VASUBU_VX      -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VDIV_VX        -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VDIVU_VX       -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMACC_VX       -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMADD_VX       -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMUL_VX        -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMULH_VX       -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMULHSU_VX     -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMULHU_VX      -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VMV_S_X        -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VNMSAC_VX      -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VNMSUB_VX      -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VREM_VX        -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VREMU_VX       -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VSLIDE1DOWN_VX -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VSLIDE1UP_VX   -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWADD_VX       -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWADD_WX       -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWADDU_VX      -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWADDU_WX      -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWMACC_VX      -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWMACCSU_VX    -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWMACCU_VX     -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWMACCUS_VX    -> OPMVX(T, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWMUL_VX       -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWMULSU_VX     -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWMULU_VX      -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWSUB_VX       -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWSUB_WX       -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWSUBU_VX      -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T),
    VWSUBU_WX      -> OPMVX(F, FuType.vipu, VipuType.dummy, F, T, F, T)
  )
  val opmvvTable: Array[(BitPat, List[BitPat])] = opmvv.map(x => (x._1, x._2.generate()))
  val opmvxTable: Array[(BitPat, List[BitPat])] = opmvx.map(x => (x._1, x._2.generate()))


  val opfvvTable: Array[(BitPat, List[BitPat])] = Array(
                       // OPFVV(fu: BitPat, fuOp: BitPat,  fWen: Boolean, vWen: Boolean, mWen: Boolean, others: Any)
// 13.2. Vector Single-Width Floating-Point Add/Subtract Instructions
VFADD_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFSUB_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.3. Vector Widening Floating-Point Add/Subtract Instructions
VFWADD_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWSUB_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWADD_WV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWSUB_WV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.4. Vector Single-Width Floating-Point Multiply/Divide Instructions
VFMUL_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFDIV_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.5. Vector Widening Floating-Point Multiply
VFWMUL_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.6. Vector Single-Width Floating-Point Fused Multiply-Add Instructions
VFMACC_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNMACC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFMSAC_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNMSAC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFMADD_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNMADD_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFMSUB_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNMSUB_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.7. Vector Widening Floating-Point Fused Multiply-Add Instructions
VFWMACC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWNMACC_VV        -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWMSAC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWNMSAC_VV        -> OPFVV(SrcType.vp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.8. Vector Floating-Point Square-Root Instruction
VFSQRT_V           -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.9. Vector Floating-Point Reciprocal Square-Root Estimate Instruction
VFRSQRT7_V         -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.10. Vector Floating-Point Reciprocal Estimate Instruction
VFREC7_V           -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.11. Vector Floating-Point MIN/MAX Instructions
VFMIN_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFMAX_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.12. Vector Floating-Point Sign-Injection Instructions
VFSGNJ_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFSGNJN_VV         -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFSGNJX_VV         -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.13. Vector Floating-Point Compare Instructions
VMFEQ_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VMFNE_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VMFLT_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VMFLE_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.14. Vector Floating-Point Classify Instruction
VFCLASS_V          -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.17. Single-Width Floating-Point/Integer Type-Convert Instructions
VFCVT_XU_F_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFCVT_X_F_V        -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFCVT_RTZ_XU_F_V   -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFCVT_RTZ_X_F_V    -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFCVT_F_XU_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFCVT_F_X_V        -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.18. Widening Floating-Point/Integer Type-Convert Instructions
VFWCVT_XU_F_V      -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWCVT_X_F_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWCVT_RTZ_XU_F_V  -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWCVT_RTZ_X_F_V   -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWCVT_F_XU_V      -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWCVT_F_X_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWCVT_F_F_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.19. Narrowing Floating-Point/Integer Type-Convert Instructions
VFNCVT_XU_F_W      -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNCVT_X_F_W       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNCVT_RTZ_XU_F_W  -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNCVT_RTZ_X_F_W   -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNCVT_F_XU_W      -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNCVT_F_X_W       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNCVT_F_F_W       -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNCVT_ROD_F_F_W   -> OPFVV(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 14.3. Vector Single-Width Floating-Point Reduction Instructions
VFREDOSUM_VS       -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFREDUSUM_VS       -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFREDMAX_VS        -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFREDMIN_VS        -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 14.4. Vector Widening Floating-Point Reduction Instructions
VFWREDOSUM_VS      -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWREDUSUM_VS      -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 16.2. Floating-Point Scalar Move Instructions
VFMV_F_S           -> OPFVV(SrcType.vp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),// f[rd] = vs2[0] (rs1=0)



  )

  val opfvfTable: Array[(BitPat, List[BitPat])] = Array(
// 13.2. Vector Single-Width Floating-Point Add/Subtract Instructions
VFADD_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFSUB_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFRSUB_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.3. Vector Widening Floating-Point Add/Subtract Instructions
VFWADD_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWSUB_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWADD_WF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWSUB_WF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.4. Vector Single-Width Floating-Point Multiply/Divide Instructions
VFMUL_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFDIV_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFRDIV_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.5. Vector Widening Floating-Point Multiply
VFWMUL_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.6. Vector Single-Width Floating-Point Fused Multiply-Add Instructions
VFMACC_VF          -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNMACC_VF         -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFMSAC_VF          -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNMSAC_VF         -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFMADD_VF          -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNMADD_VF         -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFMSUB_VF          -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFNMSUB_VF         -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.7. Vector Widening Floating-Point Fused Multiply-Add Instructions
VFWMACC_VF         -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWNMACC_VF        -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWMSAC_VF         -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFWNMSAC_VF        -> OPFVF(SrcType.fp, SrcType.vp, FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.11. Vector Floating-Point MIN/MAX Instructions
VFMIN_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFMAX_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.12. Vector Floating-Point Sign-Injection Instructions
VFSGNJ_VF          -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFSGNJN_VF         -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
VFSGNJX_VF         -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.13. Vector Floating-Point Compare Instructions
VMFEQ_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, F, T).generate(),
VMFNE_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, F, T).generate(),
VMFLT_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, F, T).generate(),
VMFLE_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, F, T).generate(),
VMFGT_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, F, T).generate(),
VMFGE_VF           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, F, T).generate(),

// 13.15. Vector Floating-Point Merge Instruction
VFMERGE_VFM        -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),

// 13.16. Vector Floating-Point Move Instruction
VFMV_V_F           -> OPFVF(SrcType.X , SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),// src2=SrcType.X

// 16.2. Floating-Point Scalar Move Instructions
VFMV_S_F           -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),// vs2=0

// 16.3.3. Vector Slide1up
// vslide1up.vx vd, vs2, rs1, vm # vd[0]=x[rs1], vd[i+1] = vs2[i]
VFSLIDE1UP_VF      -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),// vd[0]=f[rs1], vd[i+1] = vs2[i]

// 16.3.4. Vector Slide1down Instruction
// vslide1down.vx vd, vs2, rs1, vm # vd[i] = vs2[i+1], vd[vl-1]=x[rs1]
VFSLIDE1DOWN_VF    -> OPFVF(SrcType.fp, SrcType.X , FuType.vfpu, VfpuType.dummy, F, T, F).generate(),// vd[i] = vs2[i+1], vd[vl-1]=f[rs1]

  )

  val vsetTable: Array[(BitPat, List[BitPat])] = Array()
  val vlsTable: Array[(BitPat, List[BitPat])] = Array()

  val table = opivvTable ++ opivxTable ++ opiviTable ++
              opmvvTable ++ opmvxTable ++
              opfvvTable ++ opfvfTable ++
              vsetTable ++ vlsTable
}

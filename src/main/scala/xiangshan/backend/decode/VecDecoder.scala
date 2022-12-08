package xiangshan.backend.decode

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.util.uintToBitPat
import utils._
import xiangshan.ExceptionNO.illegalInstr
import xiangshan._
import freechips.rocketchip.rocket.Instructions._

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

case class OPMVV(fu: BitPat, fuOp: BitPat, xWen: Boolean, vWen: Boolean, mWen: Boolean, others: Any) extends VecType {
  def generate() : List[BitPat] = {
    List (SrcType.vp, SrcType.vp, SrcType.X, fu, fuOp, xWen.B, N, vWen.B, mWen.B, N, N, N, N, SelImm.X)
  }
}

case class OPMVX() extends VecType {
  def generate() : List[BitPat] = { null }
}

case class OPFVV() extends VecType {
  def generate() : List[BitPat] = { null }
}

case class OPFVF(fu: BitPat, fuOp: BitPat, fWen: Boolean, vWen: Boolean, mWen: Boolean) extends VecType {
  def generate() : List[BitPat] = {
    List (SrcType.vp, SrcType.fp, SrcType.X, fu, fuOp, N, fWen.B, vWen.B, mWen.B, N, N, N, N, SelImm.X)
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

  val opmvvTable: Array[(BitPat, List[BitPat])] = Array()
  val opmvxTable: Array[(BitPat, List[BitPat])] = Array()

  val opfvvTable: Array[(BitPat, List[BitPat])] = Array()

  val opfvfTable: Array[(BitPat, List[BitPat])] = Array(
    VFADD_VF  -> OPFVF(FuType.vfpu, VfpuType.dummy, F, T, F).generate(),
    VMFEQ_VF  -> OPFVF(FuType.vfpu, VfpuType.dummy, F, F, T).generate(),
  )

  val vsetTable: Array[(BitPat, List[BitPat])] = Array()
  val vlsTable: Array[(BitPat, List[BitPat])] = Array()

  val table = opivvTable ++ opivxTable ++ opiviTable ++
              opmvvTable ++ opmvxTable ++
              opfvvTable ++ opfvfTable ++
              vsetTable ++ vlsTable
}

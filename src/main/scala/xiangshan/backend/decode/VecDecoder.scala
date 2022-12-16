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

case class OPIVV(fu: BitPat, fuOp: BitPat, vWen: Boolean, mWen: Boolean) extends VecType {
  def generate() : List[BitPat] = {
    List (SrcType.vp, SrcType.vp, SrcType.X, fu, fuOp, N, N, vWen.B, mWen.B, N, N, N, N, SelImm.X)
  }
}

case class OPIVX() extends VecType {
  def generate() : List[BitPat] = { null }
}

case class OPIVI() extends VecType {
  def generate() : List[BitPat] = { null }
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
    VADD_VV   -> OPIVV(FuType.vipu, VipuType.dummy, T, F).generate(),

    VMSEQ_VV  -> OPIVV(FuType.vipu, VipuType.dummy, F, T).generate(),
  )

  val opivxTable: Array[(BitPat, List[BitPat])] = Array()
  val opiviTable: Array[(BitPat, List[BitPat])] = Array()

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

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

case class OPMVV(fu: BitPat, fuOp: BitPat, xWen: Boolean, vWen: Boolean, mWen: Boolean, others: Any) extends VecType {
  def generate() : List[BitPat] = {
    List (SrcType.vp, SrcType.vp, SrcType.X, fu, fuOp, xWen.B, N, vWen.B, mWen.B, N, N, N, N, SelImm.X)
  }
}

case class OPMVX() extends VecType {
  def generate() : List[BitPat] = { null }
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
    VADD_VV   -> OPIVV(FuType.vipu, VipuType.dummy, T, F).generate(),

    VMSEQ_VV  -> OPIVV(FuType.vipu, VipuType.dummy, F, T).generate(),
  )

  val opivxTable: Array[(BitPat, List[BitPat])] = Array()
  val opiviTable: Array[(BitPat, List[BitPat])] = Array()

  val opmvvTable: Array[(BitPat, List[BitPat])] = Array()
  val opmvxTable: Array[(BitPat, List[BitPat])] = Array()
 
  val opfvvTable: Array[(BitPat, List[BitPat])] = Array(
                       // OPFVV(fu: BitPat, fuOp: BitPat,  fWen: Boolean, vWen: Boolean, mWen: Boolean, others: Any)
// 13.2. Vector Single-Width Floating-Point Add/Subtract Instructions
VFADD_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFSUB_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.3. Vector Widening Floating-Point Add/Subtract Instructions
VFWADD_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFWSUB_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFWADD_WV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFWSUB_WV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.4. Vector Single-Width Floating-Point Multiply/Divide Instructions
VFMUL_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFDIV_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.5. Vector Widening Floating-Point Multiply
VFWMUL_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.6. Vector Single-Width Floating-Point Fused Multiply-Add Instructions
VFMACC_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFNMACC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFMSAC_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFNMSAC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFMADD_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFNMADD_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFMSUB_VV          -> OPFVV(SrcType.vp, SrcType.vp, FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFNMSUB_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.7. Vector Widening Floating-Point Fused Multiply-Add Instructions
VFWMACC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFWNMACC_VV        -> OPFVV(SrcType.vp, SrcType.vp, FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFWMSAC_VV         -> OPFVV(SrcType.vp, SrcType.vp, FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFWNMSAC_VV        -> OPFVV(SrcType.vp, SrcType.vp, FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.8. Vector Floating-Point Square-Root Instruction
VFSQRT_V           -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.9. Vector Floating-Point Reciprocal Square-Root Estimate Instruction
VFRSQRT7_V         -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.10. Vector Floating-Point Reciprocal Estimate Instruction
VFREC7_V           -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.11. Vector Floating-Point MIN/MAX Instructions
VFMIN_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFMAX_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.12. Vector Floating-Point Sign-Injection Instructions
VFSGNJ_VV          -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFSGNJN_VV         -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFSGNJX_VV         -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.13. Vector Floating-Point Compare Instructions
VMFEQ_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VMFNE_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VMFLT_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VMFLE_VV           -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.14. Vector Floating-Point Classify Instruction
VFCLASS_V          -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.17. Single-Width Floating-Point/Integer Type-Convert Instructions
VFCVT_XU_F_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFCVT_X_F_V        -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFCVT_RTZ_XU_F_V   -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFCVT_RTZ_X_F_V    -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFCVT_F_XU_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFCVT_F_X_V        -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.18. Widening Floating-Point/Integer Type-Convert Instructions
VFWCVT_XU_F_V      -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFWCVT_X_F_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFWCVT_RTZ_XU_F_V  -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFWCVT_RTZ_X_F_V   -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFWCVT_F_XU_V      -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFWCVT_F_X_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFWCVT_F_F_V       -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 13.19. Narrowing Floating-Point/Integer Type-Convert Instructions
VFNCVT_XU_F_W      -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFNCVT_X_F_W       -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFNCVT_RTZ_XU_F_W  -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFNCVT_RTZ_X_F_W   -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFNCVT_F_XU_W      -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFNCVT_F_X_W       -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFNCVT_F_F_W       -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFNCVT_ROD_F_F_W   -> OPFVV(SrcType.X , SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 14.3. Vector Single-Width Floating-Point Reduction Instructions
VFREDOSUM_VS       -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFREDUSUM_VS       -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFREDMAX_VS        -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFREDMIN_VS        -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 14.4. Vector Widening Floating-Point Reduction Instructions
VFWREDOSUM_VS      -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),
VFWREDUSUM_VS      -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),

// 16.2. Floating-Point Scalar Move Instructions
VFMV_F_S           -> OPFVV(SrcType.vp, SrcType.X , FuType.vvpu, VfpuType.dummy, F, T, F).generate(),// f[rd] = vs2[0] (rs1=0)



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

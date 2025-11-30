package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.vector.Decoder.InstPattern.{FpInstPattern, InstPattern, IntInstPattern, VecInstPattern}
import xiangshan.backend.vector.Decoder.util.BoolDecodeField
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

object FFlagsWenField extends BoolDecodeField[InstPattern] {

  override def name: String = "fflagsWen"

  override def genTable(op: InstPattern): BitPat = {
    op match {
      case ii: IntInstPattern => n
      case fpi: FpInstPattern if fpInsts.contains(fpi.name) => y
      case vi: VecInstPattern if vpInsts.contains(vi.name) => y
      case _ => n
    }
  }

  val fpInsts = getVariableNameSeq(
    FMADD_D, FMSUB_D, FNMSUB_D, FNMADD_D,
    FMADD_S, FMSUB_S, FNMSUB_S, FNMADD_S,
    FMADD_H, FMSUB_H, FNMSUB_H, FNMADD_H,
    FADD_D, FSUB_D, FMUL_D, FDIV_D, FSQRT_D,
    FADD_S, FSUB_S, FMUL_S, FDIV_S, FSQRT_S,
    FADD_H, FSUB_H, FMUL_H, FDIV_H, FSQRT_H,
    FMIN_D, FMAX_D, FMINM_D, FMAXM_D,
    FMIN_S, FMAX_S, FMINM_S, FMAXM_S,
    FMIN_H, FMAX_H, FMINM_H, FMAXM_H,
    FEQ_D, FLT_D, FLE_D, FLEQ_D, FLTQ_D,
    FEQ_S, FLT_S, FLE_S, FLEQ_S, FLTQ_S,
    FEQ_H, FLT_H, FLE_H, FLEQ_H, FLTQ_H,
    FROUND_D, FROUNDNX_D,
    FROUND_S, FROUNDNX_S,
    FROUND_H, FROUNDNX_H,
    // cvt
    FCVT_S_W, FCVT_S_WU, FCVT_S_L, FCVT_S_LU,
    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
    FCVT_D_W, FCVT_D_WU, FCVT_D_L, FCVT_D_LU,
    FCVT_W_D, FCVT_WU_D, FCVT_L_D, FCVT_LU_D,
    FCVT_H_W, FCVT_H_WU, FCVT_H_L, FCVT_H_LU,
    FCVT_W_H, FCVT_WU_H, FCVT_L_H, FCVT_LU_H,
    FCVT_S_D, FCVT_D_S,
    FCVT_S_H, FCVT_H_S,
    FCVT_H_D, FCVT_D_H,
    FCVTMOD_W_D,
  ).toSet

  val vpInsts = getVariableNameSeq(
    VFADD_VV, VFSUB_VV,
    VFADD_VF, VFSUB_VF, VFRSUB_VF,
    VFWADD_VV, VFWSUB_VV, VFWADD_WV, VFWSUB_WV,
    VFWADD_VF, VFWSUB_VF, VFWADD_WF, VFWSUB_WF,
    VFMUL_VV, VFDIV_VV, VFWMUL_VV,
    VFMUL_VF, VFDIV_VF, VFWMUL_VF, VFRDIV_VF,
    VFMACC_VV, VFNMACC_VV, VFMSAC_VV, VFNMSAC_VV, VFMADD_VV, VFNMADD_VV, VFMSUB_VV, VFNMSUB_VV,
    VFMACC_VF, VFNMACC_VF, VFMSAC_VF, VFNMSAC_VF, VFMADD_VF, VFNMADD_VF, VFMSUB_VF, VFNMSUB_VF,
    VFWMACC_VV, VFWNMACC_VV, VFWMSAC_VV, VFWNMSAC_VV,
    VFWMACC_VF, VFWNMACC_VF, VFWMSAC_VF, VFWNMSAC_VF,
    VFMIN_VV, VFMAX_VV,
    VFMIN_VF, VFMAX_VF,
    VMFEQ_VV, VMFNE_VV, VMFLT_VV, VMFLE_VV,
    VMFEQ_VF, VMFNE_VF, VMFLT_VF, VMFLE_VF, VMFGT_VF, VMFGE_VF,
    VFSQRT_V,
    VFREDMAX_VS, VFREDMIN_VS, VFREDOSUM_VS, VFREDUSUM_VS,
    VFWREDOSUM_VS, VFWREDUSUM_VS,
    // vfcvt
    VFCVT_F_XU_V, VFCVT_RTZ_XU_F_V,
    VFCVT_F_X_V,  VFCVT_RTZ_X_F_V,
    VFCVT_X_F_V,  VFCVT_XU_F_V,
    // vfncvt
    VFNCVT_F_F_W,  VFNCVT_ROD_F_F_W,
    VFNCVT_F_XU_W,
    VFNCVT_F_X_W,
    VFNCVT_X_F_W,  VFNCVT_RTZ_X_F_W,
    VFNCVT_XU_F_W, VFNCVT_RTZ_XU_F_W,
    // vwcnt
    VFWCVT_F_F_V,
    VFWCVT_F_XU_V,
    VFWCVT_F_X_V,
    VFWCVT_X_F_V,  VFWCVT_RTZ_X_F_V,
    VFWCVT_XU_F_V, VFWCVT_RTZ_XU_F_V,
  ).toSet
}

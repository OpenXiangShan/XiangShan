package xiangshan.backend.vector.Decoder.DecodeFields.SimpleDecodeChannel

import chisel3.util.BitPat
import xiangshan.backend.decode.isa.Instructions._
import xiangshan.backend.vector.Decoder.InstPattern.VecInstPattern.Category.{OPFVF, OPFVV}
import xiangshan.backend.vector.Decoder.InstPattern.{FpInstPattern, InstPattern, VecArithInstPattern, VecInstPattern}
import xiangshan.backend.vector.Decoder.util.BoolDecodeField
import xiangshan.macros.InstanceNameMacro.getVariableNameSeq

object FrmRenField extends BoolDecodeField[InstPattern] {
  override def name: String = "frmRen"

  override def genTable(inst: InstPattern): BitPat = {
    inst match {
      case fi: FpInstPattern if scalaInsts.contains(fi.name) => y
      case vi: VecInstPattern => vi match {
        // Ref: 10.1. Vector Arithmetic Instruction encoding
        // All vector floating-point code will rely on a valid value in frm. Implementations can
        // make all vector FP instructions report exceptions when the rounding mode is invalid
        // to simplify control logic.
        case vai: VecArithInstPattern if Seq(OPFVV, OPFVF).map(_.str).contains(vai.category.rawString) => y
        case _ => n
      }
      case _ => n
    }
  }

  val scalaInsts: Set[String] = getVariableNameSeq(
    FADD_D, FSUB_D, FMADD_D, FMSUB_D, FNMSUB_D, FNMADD_D, FMUL_D, FDIV_D, FSQRT_D,
    FADD_S, FSUB_S, FMADD_S, FMSUB_S, FNMSUB_S, FNMADD_S, FMUL_S, FDIV_S, FSQRT_S,
    FADD_H, FSUB_H, FMADD_H, FMSUB_H, FNMSUB_H, FNMADD_H, FMUL_H, FDIV_H, FSQRT_H,
    // f2i
    FCVT_L_D, FCVT_LU_D, FCVT_W_D, FCVT_WU_D,
    FCVT_L_S, FCVT_LU_S, FCVT_W_S, FCVT_WU_S,
    FCVT_L_H, FCVT_LU_H, FCVT_W_H, FCVT_WU_H,
    // i2f
    FCVT_D_L, FCVT_D_LU,                     // FCVT_D_W and FCVT_D_WU need not frm
    FCVT_S_L, FCVT_S_LU, FCVT_S_W, FCVT_S_WU,
    FCVT_H_L, FCVT_H_LU, FCVT_H_W, FCVT_H_WU,
    FCVT_S_D,
    FCVT_H_D,
    FCVT_H_S,
    FROUND_D, FROUNDNX_D,
    FROUND_S, FROUNDNX_S,
    FROUND_H, FROUNDNX_H,
  ).toSet
}

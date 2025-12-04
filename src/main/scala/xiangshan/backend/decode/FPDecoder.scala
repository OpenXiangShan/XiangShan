/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.decode

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.DecodeLogic
import freechips.rocketchip.rocket.Instructions._
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.fu.fpu.FPU
import xiangshan.backend.fu.vector.Bundles.{VSew, VLmul}
import xiangshan.backend.Bundles.VPUCtrlSignals
import xiangshan.{FPUCtrlSignals, XSModule}

class FPDecoder(implicit p: Parameters) extends XSModule{
  val io = IO(new Bundle() {
    val instr = Input(UInt(32.W))
    val fpCtrl = Output(new FPUCtrlSignals)
  })

  private val inst: XSInstBitFields = io.instr.asTypeOf(new XSInstBitFields)

  val ctrl = io.fpCtrl
  // TODO dirty code
  val isFP16Instrs = Seq(
    // zfh inst
    FADD_H, FSUB_H, FEQ_H, FLT_H, FLE_H, FMIN_H, FMAX_H,
    FMUL_H, FDIV_H, FSQRT_H,
    FMADD_H, FMSUB_H, FNMADD_H, FNMSUB_H,
    FCLASS_H, FSGNJ_H, FSGNJX_H, FSGNJN_H,
    // zfa inst
    FLEQ_H, FLTQ_H, FMINM_H, FMAXM_H,
    FROUND_H, FROUNDNX_H,
  )
  val isFP16Instr = isFP16Instrs.map(io.instr === _).reduce(_ || _)
  val isFP32Instrs = Seq(
    FADD_S, FSUB_S, FEQ_S, FLT_S, FLE_S, FMIN_S, FMAX_S,
    FMUL_S, FDIV_S, FSQRT_S,
    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S,
    FCLASS_S, FSGNJ_S, FSGNJX_S, FSGNJN_S,
    // zfa inst
    FLEQ_S, FLTQ_S, FMINM_S, FMAXM_S,
    FROUND_S, FROUNDNX_S,
  )
  val isFP32Instr = isFP32Instrs.map(io.instr === _).reduce(_ || _)
  val isFP64Instrs = Seq(
    FADD_D, FSUB_D, FEQ_D, FLT_D, FLE_D, FMIN_D, FMAX_D,
    FMUL_D, FDIV_D, FSQRT_D,
    FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D,
    FCLASS_D, FSGNJ_D, FSGNJX_D, FSGNJN_D,
  )
  val isFP64Instr = isFP64Instrs.map(io.instr === _).reduce(_ || _)
  // scalar cvt inst
  val isSew2Cvts = Seq(
    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
    FCVT_W_D, FCVT_WU_D, FCVT_S_D, FCVT_D_S,
    FMV_X_W,
    // zfa inst
    FCVTMOD_W_D,
  )
  /*
  The optype for FCVT_D_H and FCVT_H_D is the same,
  so the two instructions are distinguished by sew.
  FCVT_H_D:VSew.e64
  FCVT_D_H:VSew.e16
   */
  val isSew2Cvth = Seq(
    FCVT_S_H, FCVT_H_S, FCVT_D_H,
    FMV_X_H,
    FCVT_W_H, FCVT_L_H, FCVT_H_W,
    FCVT_H_L, FCVT_H_WU, FCVT_H_LU,
    FCVT_WU_H, FCVT_LU_H,
  )

  private val wfflagsInsts = Seq(
    // opfff
    FADD_S, FSUB_S, FADD_D, FSUB_D, FADD_H, FSUB_H,
    FEQ_S, FLT_S, FLE_S, FEQ_D, FLT_D, FLE_D, FEQ_H, FLT_H, FLE_H,
    FMIN_S, FMAX_S, FMIN_D, FMAX_D, FMIN_H, FMAX_H,
    FMUL_S, FMUL_D, FMUL_H,
    FDIV_S, FDIV_D, FSQRT_S, FSQRT_D, FDIV_H, FSQRT_H,
    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S, FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D, FMADD_H, FMSUB_H, FNMADD_H, FNMSUB_H,
    // opfvv
    VFADD_VV, VFSUB_VV, VFWADD_VV, VFWSUB_VV, VFWADD_WV, VFWSUB_WV,
    VFMUL_VV, VFDIV_VV, VFWMUL_VV,
    VFMACC_VV, VFNMACC_VV, VFMSAC_VV, VFNMSAC_VV, VFMADD_VV, VFNMADD_VV, VFMSUB_VV, VFNMSUB_VV,
    VFWMACC_VV, VFWNMACC_VV, VFWMSAC_VV, VFWNMSAC_VV,
    VFSQRT_V,
    VFMIN_VV, VFMAX_VV,
    VMFEQ_VV, VMFNE_VV, VMFLT_VV, VMFLE_VV,
    // opfvf
    VFADD_VF, VFSUB_VF, VFRSUB_VF, VFWADD_VF, VFWSUB_VF, VFWADD_WF, VFWSUB_WF,
    VFMUL_VF, VFDIV_VF, VFRDIV_VF, VFWMUL_VF,
    VFMACC_VF, VFNMACC_VF, VFMSAC_VF, VFNMSAC_VF, VFMADD_VF, VFNMADD_VF, VFMSUB_VF, VFNMSUB_VF,
    VFWMACC_VF, VFWNMACC_VF, VFWMSAC_VF, VFWNMSAC_VF,
    VFMIN_VF, VFMAX_VF,
    VMFEQ_VF, VMFNE_VF, VMFLT_VF, VMFLE_VF, VMFGT_VF, VMFGE_VF,
    // vfred
    VFREDOSUM_VS, VFREDUSUM_VS, VFREDMAX_VS, VFREDMIN_VS, VFWREDOSUM_VS, VFWREDUSUM_VS,
    // fcvt & vfcvt
    FCVT_S_W, FCVT_S_WU, FCVT_S_L, FCVT_S_LU,
    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
    FCVT_D_W, FCVT_D_WU, FCVT_D_L, FCVT_D_LU,
    FCVT_W_D, FCVT_WU_D, FCVT_L_D, FCVT_LU_D, FCVT_S_D, FCVT_D_S,
    FCVT_S_H, FCVT_H_S, FCVT_H_D, FCVT_D_H,
    FCVT_H_W, FCVT_H_WU, FCVT_H_L, FCVT_H_LU,
    FCVT_W_H, FCVT_WU_H, FCVT_L_H, FCVT_LU_H,
    VFCVT_XU_F_V, VFCVT_X_F_V, VFCVT_RTZ_XU_F_V, VFCVT_RTZ_X_F_V, VFCVT_F_XU_V, VFCVT_F_X_V,
    VFWCVT_XU_F_V, VFWCVT_X_F_V, VFWCVT_RTZ_XU_F_V, VFWCVT_RTZ_X_F_V, VFWCVT_F_XU_V, VFWCVT_F_X_V, VFWCVT_F_F_V,
    VFNCVT_XU_F_W, VFNCVT_X_F_W, VFNCVT_RTZ_XU_F_W, VFNCVT_RTZ_X_F_W, VFNCVT_F_XU_W, VFNCVT_F_X_W, VFNCVT_F_F_W,
    VFNCVT_ROD_F_F_W, VFRSQRT7_V, VFREC7_V,
    // zfa
    FLEQ_H, FLEQ_S, FLEQ_D, FLTQ_H, FLTQ_S, FLTQ_D,
    FMINM_H, FMINM_S, FMINM_D, FMAXM_H, FMAXM_S, FMAXM_D,
    FROUND_H, FROUND_S, FROUND_D, FROUNDNX_H, FROUNDNX_S, FROUNDNX_D,
    FCVTMOD_W_D,
  )

  val isSew2Cvt32 = isSew2Cvts.map(io.instr === _).reduce(_ || _)
  val isSew2Cvt16 = isSew2Cvth.map(io.instr === _).reduce(_ || _)
  ctrl.fmt := Mux(isFP32Instr || isSew2Cvt32, VSew.e32, Mux(isFP16Instr || isSew2Cvt16, VSew.e16, VSew.e64))
  ctrl.rm := inst.RM
  ctrl.wfflags := wfflagsInsts.map(_ === inst.ALL).reduce(_ || _)

}

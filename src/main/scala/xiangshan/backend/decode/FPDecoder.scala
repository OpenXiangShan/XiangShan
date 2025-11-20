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

//class FPToVecDecoder(implicit p: Parameters) extends XSModule {
//  val io = IO(new Bundle() {
//    val instr = Input(UInt(32.W))
//    val vpuCtrl = Output(new VPUCtrlSignals)
//  })
//
//  val inst = io.instr.asTypeOf(new XSInstBitFields)
//  val fpToVecInsts = Seq(
//    FADD_S, FSUB_S, FADD_D, FSUB_D, FADD_H, FSUB_H,
//    FEQ_S, FLT_S, FLE_S, FEQ_D, FLT_D, FLE_D, FEQ_H, FLT_H, FLE_H,
//    FMIN_S, FMAX_S, FMIN_D, FMAX_D, FMIN_H, FMAX_H,
//    FMUL_S, FMUL_D, FMUL_H,
//    FDIV_S, FDIV_D, FSQRT_S, FSQRT_D, FDIV_H, FSQRT_H,
//    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S, FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D, FMADD_H, FMSUB_H, FNMADD_H, FNMSUB_H,
//    FCLASS_S, FCLASS_D, FSGNJ_S, FSGNJ_D, FSGNJX_S, FSGNJX_D, FSGNJN_S, FSGNJN_D,
//    FCLASS_H, FSGNJ_H, FSGNJX_H, FSGNJN_H,
//    // scalar cvt inst
//    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
//    FCVT_W_D, FCVT_WU_D, FCVT_L_D, FCVT_LU_D, FCVT_S_D, FCVT_D_S,
//    FCVT_S_H, FCVT_H_S, FCVT_H_D, FCVT_D_H,
//    FMV_X_W, FMV_X_D, FMV_X_H,
//    FCVT_W_H, FCVT_WU_H, FCVT_L_H, FCVT_LU_H,
//    // zfa inst
//    FLEQ_H, FLEQ_S, FLEQ_D, FLTQ_H, FLTQ_S, FLTQ_D, FMINM_H, FMINM_S, FMINM_D, FMAXM_H, FMAXM_S, FMAXM_D,
//    FROUND_H, FROUND_S, FROUND_D, FROUNDNX_H, FROUNDNX_S, FROUNDNX_D, FCVTMOD_W_D,
//  )
//  val isFpToVecInst = fpToVecInsts.map(io.instr === _).reduce(_ || _)
//  val isFP16Instrs = Seq(
//    // zfh inst
//    FADD_H, FSUB_H, FEQ_H, FLT_H, FLE_H, FMIN_H, FMAX_H,
//    FMUL_H, FDIV_H, FSQRT_H,
//    FMADD_H, FMSUB_H, FNMADD_H, FNMSUB_H,
//    FCLASS_H, FSGNJ_H, FSGNJX_H, FSGNJN_H,
//    // zfa inst
//    FLEQ_H, FLTQ_H, FMINM_H, FMAXM_H,
//    FROUND_H, FROUNDNX_H,
//  )
//  val isFP16Instr = isFP16Instrs.map(io.instr === _).reduce(_ || _)
//  val isFP32Instrs = Seq(
//    FADD_S, FSUB_S, FEQ_S, FLT_S, FLE_S, FMIN_S, FMAX_S,
//    FMUL_S, FDIV_S, FSQRT_S,
//    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S,
//    FCLASS_S, FSGNJ_S, FSGNJX_S, FSGNJN_S,
//    // zfa inst
//    FLEQ_S, FLTQ_S, FMINM_S, FMAXM_S,
//    FROUND_S, FROUNDNX_S,
//  )
//  val isFP32Instr = isFP32Instrs.map(io.instr === _).reduce(_ || _)
//  val isFP64Instrs = Seq(
//    FADD_D, FSUB_D, FEQ_D, FLT_D, FLE_D, FMIN_D, FMAX_D,
//    FMUL_D, FDIV_D, FSQRT_D,
//    FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D,
//    FCLASS_D, FSGNJ_D, FSGNJX_D, FSGNJN_D,
//  )
//  val isFP64Instr = isFP64Instrs.map(io.instr === _).reduce(_ || _)
//  // scalar cvt inst
//  val isSew2Cvts = Seq(
//    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
//    FCVT_W_D, FCVT_WU_D, FCVT_S_D, FCVT_D_S,
//    FMV_X_W,
//    // zfa inst
//    FCVTMOD_W_D,
//  )
//  /*
//  The optype for FCVT_D_H and FCVT_H_D is the same,
//  so the two instructions are distinguished by sew.
//  FCVT_H_D:VSew.e64
//  FCVT_D_H:VSew.e16
//   */
//  val isSew2Cvth = Seq(
//    FCVT_S_H, FCVT_H_S, FCVT_D_H,
//    FMV_X_H,
//    FCVT_W_H, FCVT_L_H, FCVT_H_W,
//    FCVT_H_L, FCVT_H_WU, FCVT_H_LU,
//    FCVT_WU_H, FCVT_LU_H,
//  )
//  val isSew2Cvt32 = isSew2Cvts.map(io.instr === _).reduce(_ || _)
//  val isSew2Cvt16 = isSew2Cvth.map(io.instr === _).reduce(_ || _)
//  val isLmulMf4Cvts = Seq(
//    FCVT_W_S, FCVT_WU_S,
//    FMV_X_W,
//  )
//  val isLmulMf4Cvt = isLmulMf4Cvts.map(io.instr === _).reduce(_ || _)
//  val needReverseInsts = Seq(
//    FADD_S, FSUB_S, FADD_D, FSUB_D, FADD_H, FSUB_H,
//    FEQ_S, FLT_S, FLE_S, FEQ_D, FLT_D, FLE_D, FEQ_H, FLT_H, FLE_H,
//    FMIN_S, FMAX_S, FMIN_D, FMAX_D, FMIN_H, FMAX_H,
//    FMUL_S, FMUL_D, FMUL_H,
//    FDIV_S, FDIV_D, FSQRT_S, FSQRT_D, FDIV_H, FSQRT_H,
//    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S, FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D,
//    FMADD_H, FMSUB_H, FNMADD_H, FNMSUB_H,
//    FCLASS_S, FCLASS_D, FSGNJ_S, FSGNJ_D, FSGNJX_S, FSGNJX_D, FSGNJN_S, FSGNJN_D,
//    FCLASS_H, FSGNJ_H, FSGNJX_H, FSGNJN_H,
//    // zfa inst
//    FLEQ_H, FLEQ_S, FLEQ_D, FLTQ_H, FLTQ_S, FLTQ_D, FMINM_H, FMINM_S, FMINM_D, FMAXM_H, FMAXM_S, FMAXM_D,
//  )
//  val needReverseInst = needReverseInsts.map(_ === inst.ALL).reduce(_ || _)
//  io.vpuCtrl := 0.U.asTypeOf(io.vpuCtrl)
//  io.vpuCtrl.fpu.isFpToVecInst := isFpToVecInst
//  io.vpuCtrl.fpu.isFP32Instr   := isFP32Instr
//  io.vpuCtrl.fpu.isFP64Instr   := isFP64Instr
//  io.vpuCtrl.vill  := false.B
//  io.vpuCtrl.vma   := true.B
//  io.vpuCtrl.vta   := true.B
//  io.vpuCtrl.vsew  := Mux(isFP32Instr || isSew2Cvt32, VSew.e32, Mux(isFP16Instr || isSew2Cvt16, VSew.e16, VSew.e64))
//  io.vpuCtrl.vlmul := Mux(isFP32Instr || isLmulMf4Cvt, VLmul.mf4, VLmul.mf2)
//  io.vpuCtrl.vm    := inst.VM
//  io.vpuCtrl.nf    := inst.NF
//  io.vpuCtrl.veew := inst.WIDTH
//  io.vpuCtrl.isReverse := needReverseInst
//  io.vpuCtrl.isExt     := false.B
//  io.vpuCtrl.isNarrow  := false.B
//  io.vpuCtrl.isDstMask := false.B
//  io.vpuCtrl.isOpMask  := false.B
//  io.vpuCtrl.isDependOldVd := false.B
//  io.vpuCtrl.isWritePartVd := false.B
//}


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

  val isSew2Cvt32 = isSew2Cvts.map(io.instr === _).reduce(_ || _)
  val isSew2Cvt16 = isSew2Cvth.map(io.instr === _).reduce(_ || _)
  ctrl.fmt := Mux(isFP32Instr || isSew2Cvt32, VSew.e32, Mux(isFP16Instr || isSew2Cvt16, VSew.e16, VSew.e64))
  ctrl.rm := inst.RM
  ctrl.wflags := DontCare

}

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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.rocket.DecodeLogic
import freechips.rocketchip.rocket.Instructions._
import xiangshan.backend.decode.isa.bitfield.XSInstBitFields
import xiangshan.backend.fu.fpu.FPU
import xiangshan.backend.fu.vector.Bundles.{VSew, VLmul, Category}
import xiangshan.backend.Bundles.VPUCtrlSignals
import xiangshan.{FPUCtrlSignals, XSModule}

class FPToVecDecoder(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle() {
    val instr = Input(UInt(32.W))
    val vpuCtrl = Output(new VPUCtrlSignals)
  })

  val inst = io.instr.asTypeOf(new XSInstBitFields)
  val fpToVecInsts = Seq(
    FADD_S, FSUB_S, FADD_D, FSUB_D,
    FEQ_S, FLT_S, FLE_S, FEQ_D, FLT_D, FLE_D,
    FMIN_S, FMAX_S, FMIN_D, FMAX_D,
    FMUL_S, FMUL_D,
    FDIV_S, FDIV_D, FSQRT_S, FSQRT_D,
    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S, FMADD_D, FMSUB_D, FNMADD_D, FNMSUB_D,
    FCLASS_S, FCLASS_D, FSGNJ_S, FSGNJ_D, FSGNJX_S, FSGNJX_D, FSGNJN_S, FSGNJN_D,

    // scalar cvt inst
    FCVT_W_S, FCVT_WU_S, FCVT_L_S, FCVT_LU_S,
    FCVT_W_D, FCVT_WU_D, FCVT_L_D, FCVT_LU_D, FCVT_S_D, FCVT_D_S,
    FMV_X_W, FMV_X_D,
  )
  val isFpToVecInst = fpToVecInsts.map(io.instr === _).reduce(_ || _)
  val isFP32Instrs = Seq(
    FADD_S, FSUB_S, FEQ_S, FLT_S, FLE_S, FMIN_S, FMAX_S,
    FMUL_S, FDIV_S, FSQRT_S,
    FMADD_S, FMSUB_S, FNMADD_S, FNMSUB_S,
    FCLASS_S, FSGNJ_S, FSGNJX_S, FSGNJN_S,
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
  )
  val isSew2Cvt = isSew2Cvts.map(io.instr === _).reduce(_ || _)

  val isSew3Cvts = Seq(
    FCVT_L_D, FCVT_LU_D,
    FMV_X_D,
  )
  val isSew3Cvt = isSew3Cvts.map(io.instr === _).reduce(_ || _)

  val isLmulMf4Cvts = Seq(
    FCVT_W_S, FCVT_WU_S,
    FMV_X_W,
  )
  val isLmulMf4Cvt = isLmulMf4Cvts.map(io.instr === _).reduce(_ || _)

  val isLmulMf2Cvts = Seq(
    FCVT_L_S, FCVT_LU_S,
    FCVT_W_D, FCVT_WU_D, FCVT_L_D, FCVT_LU_D, FCVT_S_D, FCVT_D_S,
    FMV_X_D,
  )
  val isLmulMf2Cvt = isLmulMf2Cvts.map(io.instr === _).reduce(_ || _)

  val needReverseInsts = fpToVecInsts
  val needReverseInst = needReverseInsts.map(_ === inst.ALL).reduce(_ || _)
  io.vpuCtrl := 0.U.asTypeOf(io.vpuCtrl)
  io.vpuCtrl.fpu.isFpToVecInst := isFpToVecInst
  io.vpuCtrl.fpu.isFP32Instr   := isFP32Instr
  io.vpuCtrl.fpu.isFP64Instr   := isFP64Instr
  io.vpuCtrl.fpu.rmInst := inst.RM
  io.vpuCtrl.vill  := false.B
  io.vpuCtrl.vma   := true.B
  io.vpuCtrl.vta   := true.B
  io.vpuCtrl.vsew  := Mux(isFP32Instr || isSew2Cvt, VSew.e32, VSew.e64)
  io.vpuCtrl.vlmul := Mux(isFP32Instr || isLmulMf4Cvt, VLmul.mf4, VLmul.mf2)
  io.vpuCtrl.vm    := inst.VM
  io.vpuCtrl.nf    := inst.NF
  io.vpuCtrl.needScalaSrc := Category.needScalaSrc(inst.VCATEGORY)
  io.vpuCtrl.permImmTruncate := Category.permImmTruncate(inst.VCATEGORY)
  io.vpuCtrl.isReverse := needReverseInst
  io.vpuCtrl.isExt     := false.B
  io.vpuCtrl.isNarrow  := false.B
  io.vpuCtrl.isDstMask := false.B
  io.vpuCtrl.isOpMask  := false.B
}

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
package xiangshan.mem

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._


class VectorLoadWrapperIOBundle(implicit p: Parameters) extends XSBundle {
  val loadRegIn = Vec(VecLoadPipelineWidth, Flipped(Decoupled(new ExuInput(isVpu = true))))
  val loadPipleIn = Vec(VecLoadPipelineWidth, Flipped(Decoupled(new VecExuOutput())))
  val redirect    = Flipped(ValidIO(new Redirect))
  val loadPipeOut = Vec(VecLoadPipelineWidth, Decoupled(new VecLoadPipeBundle()))
  val vecFeedback = Vec(VecLoadPipelineWidth, ValidIO(Bool()))
  val vecLoadWriteback = Vec(VecLoadPipelineWidth, Decoupled(new ExuOutput(isVpu = true)))
}

class VectorLoadWrapper(implicit p: Parameters) extends XSModule with HasCircularQueuePtrHelper {

  val io = IO(new VectorLoadWrapperIOBundle())

  val loadInstDec = Wire(Vec(VecLoadPipelineWidth,new VecDecode()))
  val eew         = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val sew         = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val lmul        = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val emul        = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val isSegment   = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val instType    = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val uop_unit_stride_fof = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val uop_unit_whole_reg = Wire(Vec(VecLoadPipelineWidth, Bool()))
  val uop_segment_num = Wire(Vec(VecLoadPipelineWidth, UInt(3.W)))
  val realFlowNum     = Wire(Vec(VecLoadPipelineWidth, UInt(5.W)))



  for (i <- 0 until VecLoadPipelineWidth) {
    loadInstDec(i).apply(io.loadRegIn(i).bits.uop.cf.instr)
    eew(i)                 := loadInstDec(i).uop_eew
    sew(i)                 := io.loadRegIn(i).bits.uop.ctrl.vconfig.vtype.vsew
    lmul(i)                := io.loadRegIn(i).bits.uop.ctrl.vconfig.vtype.vlmul
    emul(i)                := EewLog2(eew(i)) - sew(i) + lmul(i)
    isSegment(i)           := loadInstDec(i).uop_segment_num =/= "b000".U && !loadInstDec(i).uop_unit_stride_whole_reg
    instType(i)            := Cat(isSegment(i), loadInstDec(i).uop_type)
    uop_unit_stride_fof(i) := loadInstDec(i).uop_unit_stride_fof
    uop_unit_whole_reg(i) := loadInstDec(i).uop_unit_stride_whole_reg
    uop_segment_num(i)     := loadInstDec(i).uop_segment_num
    realFlowNum(i)         := GenRealFlowNum(instType = instType(i), emul = emul(i), lmul = lmul(i), eew = eew(i), sew = sew(i))
  }

  val vlFlowQueue = Module(new VlFlowQueue())
  val vlUopQueue = Module(new VlUopQueue())

  vlUopQueue.io.redirect <> io.redirect
  vlFlowQueue.io.redirect <> io.redirect
  for (i <- 0 until VecLoadPipelineWidth) {
    io.loadRegIn(i).ready := vlUopQueue.io.loadRegIn(i).ready && vlFlowQueue.io.loadRegIn(i).ready
    io.vecFeedback(i).valid := vlUopQueue.io.uopVecFeedback(i).valid && vlFlowQueue.io.flowFeedback(i).valid
    io.vecFeedback(i).bits := vlUopQueue.io.uopVecFeedback(i).bits && vlFlowQueue.io.flowFeedback(i).bits

    vlUopQueue.io.loadRegIn(i).valid  := io.loadRegIn(i).valid && (vlFlowQueue.io.loadRegIn(i).ready || vlFlowQueue.io.flowFeedback(i).bits)
    vlUopQueue.io.loadRegIn(i).bits   := io.loadRegIn(i).bits
    vlFlowQueue.io.loadRegIn(i).valid := io.loadRegIn(i).valid && (vlUopQueue.io.loadRegIn(i).ready || vlUopQueue.io.uopVecFeedback(i).bits)
    vlFlowQueue.io.loadRegIn(i).bits  := io.loadRegIn(i).bits
  }

  vlUopQueue.io.instType  := instType
  vlUopQueue.io.emul      := emul
  vlUopQueue.io.realFlowNum := realFlowNum
  vlUopQueue.io.loadPipeIn <> io.loadPipleIn
  vlUopQueue.io.vecLoadWriteback <> io.vecLoadWriteback
  vlUopQueue.io.fof := uop_unit_stride_fof
  vlUopQueue.io.whole_reg := uop_unit_whole_reg
  vlFlowQueue.io.eew                 := eew
  vlFlowQueue.io.sew                 := sew
  vlFlowQueue.io.emul                := emul
  vlFlowQueue.io.instType            := instType
  vlFlowQueue.io.uop_unit_stride_fof := uop_unit_stride_fof
  vlFlowQueue.io.whole_reg := uop_unit_whole_reg
  vlFlowQueue.io.uop_segment_num     := uop_segment_num
  vlFlowQueue.io.realFlowNum         := realFlowNum
  vlFlowQueue.io.loadPipeOut          <> io.loadPipeOut

}
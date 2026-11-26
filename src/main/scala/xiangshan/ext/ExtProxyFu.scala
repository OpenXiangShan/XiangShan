/***************************************************************************************
* Copyright (c) 2024-2026 Beijing Institute of Open Source Chip (BOSC)
* Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.ext

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.ExceptionNO._
import xiangshan.backend.fu.{FuConfig, FuncUnit}

/**
 * Generic in-core proxy execution unit for core extension plugins.
 * Attached to IntSchd ALU0 alongside CSR/Fence, sharing scalar rs1/rs2/rd ports,
 * and bridges FuncUnitIO to the core-level plugin via FuncUnitIO.extExec.
 */
class ExtProxyFu(cfg: FuConfig)(implicit p: Parameters) extends FuncUnit(cfg) {
  private val exec = io.extExec.get

  // 3-state FSM: s_idle -> s_wait_ext -> s_resp_hold
  private val s_idle :: s_wait_ext :: s_resp_hold :: Nil = Enum(3)
  private val state   = RegInit(s_idle)
  private val seqId   = RegInit(0.U(2.W))
  private val uopCtrl = Reg(chiselTypeOf(io.in.bits.ctrl))
  private val respReg = Reg(new ExtExecResp)

  private val inFlush   = io.in.bits.ctrl.robIdx.needFlush(io.flush)
  private val holdFlush = uopCtrl.robIdx.needFlush(io.flush)

  // Request and abort channels
  io.in.ready            := (state === s_idle) && exec.req.ready
  exec.req.valid         := (state === s_idle) && io.in.valid && !inFlush
  exec.req.bits.fuOpType := io.in.bits.ctrl.fuOpType
  exec.req.bits.rs1Data  := io.in.bits.data.src(0)
  exec.req.bits.rs2Data  := io.in.bits.data.src(1)
  exec.req.bits.raw25    := io.in.bits.data.imm(24, 0)
  exec.req.bits.robIdx   := io.in.bits.ctrl.robIdx
  exec.req.bits.seqId    := seqId

  exec.abort := (state === s_wait_ext) && holdFlush

  switch(state) {
    is(s_idle) {
      when(io.in.fire && !inFlush) {
        uopCtrl := io.in.bits.ctrl
        when(exec.resp.valid && exec.resp.bits.seqId === seqId) {
          // Capture same-cycle combinational response from the extension unit
          respReg := exec.resp.bits
          seqId   := seqId + 1.U
          state   := s_resp_hold
        }.otherwise {
          state   := s_wait_ext
        }
      }
    }
    is(s_wait_ext) {
      when(holdFlush) {
        seqId := seqId + 1.U
        state := s_idle
      }.elsewhen(exec.resp.valid && exec.resp.bits.seqId === seqId) {
        respReg := exec.resp.bits
        seqId   := seqId + 1.U
        state   := s_resp_hold
      }
    }
    is(s_resp_hold) {
      when(holdFlush || io.out.fire) {
        state := s_idle
      }
    }
  }

  // Output writeback to standard XiangShan ROB/WB path
  io.out.valid                  := (state === s_resp_hold) && !holdFlush
  io.out.bits.ctrl.robIdx       := uopCtrl.robIdx
  io.out.bits.ctrl.pdest        := uopCtrl.pdest
  io.out.bits.ctrl.rfWen.foreach(_ := uopCtrl.rfWen.get)
  io.out.bits.ctrl.flushPipe.foreach(_ := uopCtrl.flushPipe.get)
  io.out.bits.perfDebugInfo.foreach(_ := RegEnable(io.in.bits.perfDebugInfo.get, io.in.fire))
  io.out.bits.debug_seqNum.foreach(_ := RegEnable(io.in.bits.debug_seqNum.get, io.in.fire))

  io.out.bits.res.data := respReg.rdData

  // Map precise exception types to standard XiangShan ExceptionNO
  io.out.bits.ctrl.exceptionVec.foreach(_ := false.B)
  if (cfg.exceptionOut.contains(illegalInstr)) {
    io.out.bits.ctrl.exceptionVec(illegalInstr) := respReg.illegalInstr
  }
  if (cfg.exceptionOut.contains(virtualInstr)) {
    io.out.bits.ctrl.exceptionVec(virtualInstr) := respReg.virtualInstr
  }
  if (cfg.exceptionOut.contains(loadAccessFault)) {
    io.out.bits.ctrl.exceptionVec(loadAccessFault) := respReg.accessFault
  }
}

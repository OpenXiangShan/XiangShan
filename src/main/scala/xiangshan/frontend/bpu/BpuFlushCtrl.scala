// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._

class BpuFlushCtrlIO(numPredictors: Int) extends Bundle {
  val flush      = Input(Bool())       // phase-1: io.flush (Bpu top-level Option port's .get)
  val redirectEn = Input(Bool())       // phase-2: io.fromFtq.redirect.valid
  val bpuFlushEn = Input(Bool())       // sticky enable: once set, sbpctl.BPU_FLUSH_EN stays high until reset
  val flushMask  = Input(UInt(numPredictors.W))  // per-predictor enables on the phase-1 cycle
  val resetDone  = Input(Bool())       // aggregated done: all predictors selected by activeFlushMask
  val contextFlush = Output(Bool())
  val bpuFlushing  = Output(Bool())
  val activeFlushMask = Output(UInt(numPredictors.W)) // mask latched for this transaction
}

class BpuFlushCtrl(numPredictors: Int) extends Module {
  require(numPredictors > 0)
  val io = IO(new BpuFlushCtrlIO(numPredictors))

  val s_idle :: s_waiting :: s_flushing :: s_done :: Nil = Enum(4)
  val flushState = RegInit(s_idle)
  val activeFlushMask = RegInit(0.U(numPredictors.W))

  val acceptFlush = (flushState === s_idle) && io.flush && io.bpuFlushEn
  when(acceptFlush) {
    activeFlushMask := io.flushMask // snapshot at transaction start, mid-transaction CSR writes take no effect
  }
  io.activeFlushMask := activeFlushMask

  // state transitions
  when(acceptFlush) {
    flushState := s_waiting
  }.elsewhen(flushState === s_waiting && io.redirectEn) {
    flushState := s_flushing   // contextFlush is high in this cycle, next cycle enters flushing
  }.elsewhen(flushState === s_flushing && io.resetDone) {
    flushState := s_done       // aggregated resetDone asserted, this transaction completes
  }.elsewhen(flushState === s_done) {
    flushState := s_idle       // stays for 1 cycle only, reserved for future completion handling
  }

  // contextFlush is high only on the waiting -> flushing transition cycle
  io.contextFlush := (flushState === s_waiting) && io.redirectEn

  // bpuFlushing covers the whole flush window: cycle T contextFlush ~ the state machine leaves s_flushing
  io.bpuFlushing := io.contextFlush || (flushState === s_flushing)
}

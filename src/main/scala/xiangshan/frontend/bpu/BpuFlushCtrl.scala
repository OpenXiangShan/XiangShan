// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._

class BpuFlushCtrlIO(numPredictors: Int) extends Bundle {
  val flush:           Bool = Input(Bool())
  val redirectEn:      Bool = Input(Bool())
  val bpuFlushEn:      Bool = Input(Bool())
  val flushMask:       UInt = Input(UInt(numPredictors.W))
  val resetDone:       Bool = Input(Bool())
  val contextFlush:    Bool = Output(Bool())
  val bpuFlushing:     Bool = Output(Bool())
  val activeFlushMask: UInt = Output(UInt(numPredictors.W))
}

class BpuFlushCtrl(numPredictors: Int) extends Module {
  require(numPredictors > 0)

  val io = IO(new BpuFlushCtrlIO(numPredictors))

  val s_idle :: s_waiting :: s_flushing :: s_done :: Nil = Enum(4)
  val flushState = RegInit(s_idle)
  val activeFlushMask = RegInit(0.U(numPredictors.W))
  val acceptFlush = (flushState === s_idle) && io.flush && io.bpuFlushEn

  when(acceptFlush) {
    activeFlushMask := io.flushMask
  }

  switch(flushState) {
    is(s_idle) {
      when(acceptFlush) { flushState := s_waiting }
    }
    is(s_waiting) {
      when(io.redirectEn) { flushState := s_flushing }
    }
    is(s_flushing) {
      when(io.resetDone) { flushState := s_done }
    }
    is(s_done) {
      flushState := s_idle
    }
  }

  io.contextFlush    := (flushState === s_waiting) && io.redirectEn
  io.bpuFlushing     := io.contextFlush || (flushState === s_flushing)
  io.activeFlushMask := activeFlushMask
}

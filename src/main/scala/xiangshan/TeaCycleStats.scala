// Copyright (c) 2026 Institute of Computing Technology, Chinese Academy of Sciences
// Licensed under Mulan PSL v2. See LICENSE.
package xiangshan

import chisel3._
import chisel3.util._

class TeaCycleSummary extends Bundle {
  val cycle = UInt(64.W) // exclusive end of the observed prefix
  val states = Vec(4, UInt(64.W))
  val retired = UInt(64.W)
  val gaps = Vec(5, UInt(64.W)) // complete intervals: 1-7, 8-31, 32-127, 128-511, 512+
  val censoredGaps = UInt(64.W)
  val pending = UInt(64.W)
  val cleared = Bool() // counters reset after this snapshot
  val closed = Bool() // enable fell: no unobserved tail in this collection window
}

class TeaNoCommit extends Bundle {
  val start = UInt(64.W)
  val endCycle = UInt(64.W) // exclusive
  val states = Vec(3, UInt(64.W)) // Stalled, Flushed, Drained
  val leftCensored = Bool()
  val rightCensored = Bool()
}

// Fixed-size counters only: no ROB-sized storage or per-instruction timestamps.
class TeaCycleStats extends Module {
  val io = IO(new Bundle {
    val enable = Input(Bool())
    val clean = Input(Bool())
    val dump = Input(Bool())
    val cycle = Input(UInt(64.W))
    val state = Input(UInt(4.W))
    val retired = Input(UInt(16.W))
    val gapThreshold = Input(UInt(64.W))
    val summary = Output(Valid(new TeaCycleSummary))
    val interval = Output(Valid(new TeaNoCommit))
  })
  val wasEnabled = RegNext(io.enable, false.B)
  val wasCleaned = RegNext(io.clean, false.B)
  val stopped = wasEnabled && !io.enable
  val states = RegInit(VecInit.fill(4)(0.U(64.W)))
  val retired = RegInit(0.U(64.W))
  val gaps = RegInit(VecInit.fill(5)(0.U(64.W)))
  val censoredGaps = RegInit(0.U(64.W))
  val pending = RegInit(0.U(64.W))
  val gapStates = RegInit(VecInit.fill(3)(0.U(64.W)))
  val leftCensored = RegInit(false.B)
  val tick = RegInit(0.U(12.W))
  val hasCommit = io.retired =/= 0.U
  val rightCensored = stopped || io.enable && io.clean && !hasCommit
  val gapLength = pending + (io.enable && !hasCommit && rightCensored)
  val closeGap = gapLength =/= 0.U && (rightCensored || io.enable && hasCommit)
  val gapLeftCensored = Mux(pending =/= 0.U, leftCensored, !wasEnabled || wasCleaned)
  val censored = gapLeftCensored || rightCensored
  val bucket = MuxCase(4.U, Seq(8, 32, 128, 512).zipWithIndex.map {
    case (limit, index) => (gapLength < limit.U) -> index.U
  })

  val nextStates = VecInit(states.zipWithIndex.map { case (count, state) =>
    count + (io.enable && io.state === state.U)
  })
  val nextRetired = retired + Mux(io.enable, io.retired, 0.U)
  val nextGaps = VecInit(gaps.zipWithIndex.map { case (count, index) =>
    count + (closeGap && !censored && bucket === index.U)
  })
  val nextCensored = censoredGaps + (closeGap && censored)
  val nextPending = Mux(io.enable && !hasCommit && !io.clean, pending + 1.U, 0.U)
  states := Mux(io.clean, 0.U.asTypeOf(states), nextStates)
  retired := Mux(io.clean, 0.U, nextRetired)
  gaps := Mux(io.clean, 0.U.asTypeOf(gaps), nextGaps)
  censoredGaps := Mux(io.clean, 0.U, nextCensored)
  pending := nextPending
  tick := Mux(io.enable && !io.clean, tick + 1.U, 0.U)
  when(io.enable) {
    assert(io.state < 4.U)
    assert((io.state === TeaState.computing) === hasCommit)
  }
  for (i <- 0 until 3) {
    gapStates(i) := Mux(io.enable && !hasCommit && !io.clean,
      gapStates(i) + (io.state === (i + 1).U), 0.U)
  }
  when(io.enable && !hasCommit && pending === 0.U) {
    leftCensored := !wasEnabled || wasCleaned
  }

  io.summary.valid := stopped || io.enable && (!wasEnabled || wasCleaned || tick.andR || io.dump || io.clean)
  io.summary.bits.cycle := io.cycle + io.enable
  io.summary.bits.states := nextStates
  io.summary.bits.retired := nextRetired
  io.summary.bits.gaps := nextGaps
  io.summary.bits.censoredGaps := nextCensored
  io.summary.bits.pending := nextPending
  io.summary.bits.cleared := io.clean
  io.summary.bits.closed := stopped

  io.interval.valid := closeGap && (pending >= io.gapThreshold || censored)
  io.interval.bits.start := Mux(rightCensored, io.cycle + io.enable, io.cycle) - gapLength
  io.interval.bits.endCycle := io.cycle + (rightCensored && io.enable)
  io.interval.bits.states := VecInit(gapStates.zipWithIndex.map { case (count, index) =>
    count + (rightCensored && io.enable && !hasCommit && io.state === (index + 1).U)
  })
  io.interval.bits.leftCensored := gapLeftCensored
  io.interval.bits.rightCensored := rightCensored
}

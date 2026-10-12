// Copyright (c) 2026 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2026 Institute of Computing Technology, Chinese Academy of Sciences
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
import org.chipsalliance.cde.config.Parameters
import utility.XSPerfAccumulate
import xiangshan.XSModule

class TrainingBufferEntry[T <: Data](gen: T, numBanks: Int) extends Bundle {
  val data:     T    = gen.cloneType
  val bankIdx:  UInt = UInt(log2Ceil(numBanks).W)
  val needRead: Bool = Bool()
}

class TrainingBuffer[T <: Data](gen: T, numBanks: Int, numEntries: Int)(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val predictReadValid   = Input(Bool())
    val predictReadBankIdx = Input(UInt(log2Ceil(numBanks).W))

    val enq = Flipped(Valid(new TrainingBufferEntry(gen, numBanks)))
    val deq = Valid(new TrainingBufferEntry(gen, numBanks))

    val forceTraining: Bool = Output(Bool())
  })

  require(numEntries > 0)

  private val entries = Reg(Vec(numEntries, new TrainingBufferEntry(gen, numBanks)))
  private val valid   = RegInit(VecInit.fill(numEntries)(false.B))
  private val full    = valid.asUInt.andR

  // Fixed slots avoid moving payloads on dequeue. No arrival order is guaranteed.
  private val candidates = VecInit(entries.zipWithIndex.map { case (entry, i) =>
    valid(i) && (!io.predictReadValid || entry.bankIdx =/= io.predictReadBankIdx)
  })
  private val canDeq = candidates.reduce(_ || _)
  private val deqIdx = PriorityEncoder(candidates)

  private val incomingConflict = io.enq.valid && io.enq.bits.needRead &&
    io.predictReadValid && io.enq.bits.bankIdx === io.predictReadBankIdx

  // When no slot can be freed, suppress the prediction and issue the incoming request without buffering it.
  private val forceTraining = full && incomingConflict && !canDeq
  io.forceTraining := forceTraining

  private val bypass = io.enq.valid && (!incomingConflict || forceTraining)
  private val deq    = !bypass && canDeq
  private val enq    = incomingConflict && !forceTraining
  // A full buffer can reuse the slot being dequeued on this cycle.
  private val enqIdx = Mux(full, deqIdx, PriorityEncoder(~valid.asUInt))

  for (i <- 0 until numEntries) {
    when(deq && deqIdx === i.U) {
      valid(i) := false.B
    }
    when(enq && enqIdx === i.U) {
      assert(!valid(i) || (deq && deqIdx === i.U), "must not overwrite valid training")
      entries(i) := io.enq.bits
      valid(i)   := true.B
    }
    when(valid(i)) {
      assert(entries(i).needRead, "only SRAM-read training requests may be buffered")
    }
  }

  io.deq.valid := bypass || deq
  io.deq.bits  := Mux(bypass, io.enq.bits, entries(deqIdx))

  assert(!(enq && full && !deq), "training buffer must not drop incoming training")
  when(io.enq.valid) {
    assert(bypass ^ enq, "incoming training must be issued or buffered exactly once")
  }
  assert(!(bypass && deq), "only one training request may issue per cycle")
  assert(
    !(io.deq.valid && io.deq.bits.needRead && io.predictReadValid && !forceTraining &&
      io.deq.bits.bankIdx === io.predictReadBankIdx),
    "prediction and training reads must not use the same bank"
  )

  XSPerfAccumulate("full", full)
  XSPerfAccumulate("enq", enq)
  XSPerfAccumulate("deq", deq)
  XSPerfAccumulate("forceTraining", forceTraining)
}

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

class TrainingBufferEntry[T <: Data](gen: T, numBanks: Int)(implicit p: Parameters) extends Bundle {
  val train:    Train = new Train
  val readReq:  T     = gen.cloneType
  val bankIdx:  UInt  = UInt(log2Ceil(numBanks).W)
  val needRead: Bool  = Bool()
}

class TrainingBuffer[T <: Data](gen: T, numBanks: Int, numEntries: Int)(
    implicit p: Parameters
) extends XSModule {
  val io = IO(new Bundle {
    val predictReadValid:   Bool = Input(Bool())
    val predictReadBankIdx: UInt = Input(UInt(log2Ceil(numBanks).W))

    val enq: Valid[TrainingBufferEntry[T]] = Flipped(Valid(new TrainingBufferEntry(gen, numBanks)))
    val deq: Valid[TrainingBufferEntry[T]] = Valid(new TrainingBufferEntry(gen, numBanks))

    val full: Bool = Output(Bool())
  })

  require(numEntries > 0)
  require(numBanks > 0 && isPow2(numBanks))

  private val entries = Reg(Vec(numEntries, new TrainingBufferEntry(gen, numBanks)))
  private val valid   = RegInit(VecInit.fill(numEntries)(false.B))
  private val full    = valid.asUInt.andR
  io.full := full

  // Preselect one eligible entry for every possible prediction bank. The
  // current prediction bank only selects one of these precomputed grants.
  private val deqOHByPredictBank = VecInit((0 until numBanks).map { bankIdx =>
    PriorityEncoderOH(VecInit(entries.zip(valid).map { case (entry, entryValid) =>
      entryValid && (!entry.needRead || entry.bankIdx =/= bankIdx.U)
    }).asUInt)
  })
  private val defaultDeqOH = PriorityEncoderOH(valid.asUInt)
  private val deqOH = Mux(
    io.predictReadValid,
    deqOHByPredictBank(io.predictReadBankIdx),
    defaultDeqOH
  )
  private val canDeq = deqOH.orR

  private val incomingConflict = io.enq.valid && io.enq.bits.needRead &&
    io.predictReadValid && io.enq.bits.bankIdx === io.predictReadBankIdx
  private val bypass = io.enq.valid && !incomingConflict
  private val deq    = !bypass && canDeq
  private val enq    = incomingConflict
  private val enqOH  = Mux(full, deqOH, PriorityEncoderOH(~valid.asUInt))

  for (i <- 0 until numEntries) {
    when(deq && deqOH(i)) {
      valid(i) := false.B
    }
    when(enq && enqOH(i)) {
      assert(!valid(i) || (deq && deqOH(i)), "must not overwrite valid training")
      entries(i) := io.enq.bits
      valid(i)   := true.B
    }
  }

  io.deq.valid := bypass || deq
  io.deq.bits  := Mux(bypass, io.enq.bits, Mux1H(deqOH, entries))

  assert(!(enq && full && !deq), "training buffer must not drop incoming training")
  when(io.enq.valid) {
    assert(bypass ^ enq, "incoming training must be issued or buffered exactly once")
  }
  assert(!(bypass && deq), "only one training request may issue per cycle")
  assert(
    !(io.deq.valid && io.deq.bits.needRead && io.predictReadValid &&
      io.deq.bits.bankIdx === io.predictReadBankIdx),
    "prediction and training reads must not use the same bank"
  )

  XSPerfAccumulate("full", full)
  XSPerfAccumulate("enq", enq)
  XSPerfAccumulate("deq", deq)
  XSPerfAccumulate("bypass", bypass)
}

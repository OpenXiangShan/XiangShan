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

package xiangshan.frontend.bpu.utage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import scala.math.min
import utility.XSPerfAccumulate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories
import yunsuan.vector.alu.VIntFixpTable.table

// BypassShadowBuffer: A shadow buffer for TAGE table entries that:
// - Holds entries pending write to SRAM (due to bandwidth/conflict)
// - Allows predictor to "bypass" SRAM write latency by reading directly from buffer
// - Maintains coherence: each logical index appears at most once
class BypassShadowBuffer(
    val numSets:  Int,
    val numWay:   Int,
    val numEntry: Int = 8,
    val tableId:  Int,
    val NumBanks: Int = 4
)(implicit p: Parameters) extends MicroTageModule with Helpers {
  class BypassBufferIO extends MicroTageBundle {
    class Req extends MicroTageBundle {
      val readIndex: UInt = UInt(log2Ceil(MaxNumSets).W)
    }
    class Resp extends MicroTageBundle {
      val hit:         Vec[Bool]           = Vec(numWay, Bool())
      val readEntries: Vec[MicroTageEntry] = Vec(numWay, new MicroTageEntry)
    }
    class WriteReq extends MicroTageBundle {
      val writeIndex: UInt           = UInt(log2Ceil(MaxNumSets).W)
      val writeData:  MicroTageEntry = new MicroTageEntry
      val forceWrite: Bool           = Bool()
      val way:        UInt           = UInt(log2Ceil(numWay).W)
    }
    val req:          Req             = Input(new Req)
    val resp:         Resp            = Output(new Resp)
    val train:        MicroTageTrain  = new MicroTageTrain(numWay, numSets)
    val tryWrite:     Valid[WriteReq] = Output(Valid(new WriteReq))
    val writeSuccess: Bool            = Input(Bool())
    val usefulReset:  Bool            = Input(Bool())
  }
  val io = IO(new BypassBufferIO)
  class BufferEntry extends Bundle {
    val valid:     Bool           = Bool() // Indicates if entry is still in buffer
    val entryData: MicroTageEntry = new MicroTageEntry
    val index:     UInt           = UInt(log2Ceil(MaxNumSets).W)
    val waitWrite: Bool           = Bool()
    val way:       UInt           = UInt(log2Ceil(numWay).W)
  }

  private val entries   = RegInit(VecInit(Seq.fill(numEntry)(0.U.asTypeOf(new BufferEntry))))
  private val enqPtrVec = RegInit(0.U.asTypeOf(Vec(numWay, UInt(log2Ceil(numEntry).W)))) // Next available position
  private val deqPtr = RegInit(0.U(log2Ceil(numEntry).W)) // Position ready for write-back

  // Banked useful registers
  private val usefulEntries = RegInit(VecInit.tabulate(NumBanks) { bankIdx =>
    VecInit(Seq.fill(numSets / NumBanks)(
      VecInit(Seq.fill(numWay)(0.U.asTypeOf(new SaturateCounter(UsefulWidth))))
    ))
  })

  // Prediction logic
  private val entryDataVec     = VecInit(entries.map(e => e.entryData))
  private val a0_entryWayHitOH = Wire(Vec(numWay, Vec(numEntry, Bool())))
  for (way <- 0 until numWay) {
    a0_entryWayHitOH(way) := entries.map(e => (e.index === io.req.readIndex) && e.valid && (e.way === way.U))
  }

  private val a1_entryWayHitOH = RegNext(a0_entryWayHitOH)
  private val a1_hitVec        = a1_entryWayHitOH.map(hitOH => hitOH.reduce(_ || _))
  private val a1_entryVec      = a1_entryWayHitOH.map(hitOH => Mux1H(hitOH, entryDataVec))
  io.resp.hit         := a1_hitVec
  io.resp.readEntries := a1_entryVec

  // Training logic - stage 0
  private val t0_trainIndex    = io.train.t0_trainIndex
  private val t0_entryWayHitOH = Wire(Vec(numWay, Vec(numEntry, Bool())))
  for (way <- 0 until numWay) {
    t0_entryWayHitOH(way) := entries.map(e => (e.index === t0_trainIndex) && e.valid && (e.way === way.U))
  }
  private val t0_hitVec           = VecInit(t0_entryWayHitOH.map(hitOH => hitOH.reduce(_ || _)))
  private val t0_entryVec         = VecInit(t0_entryWayHitOH.map(hitOH => Mux1H(hitOH, entryDataVec)))
  private val t0_trainReadEntries = t0_entryVec
  // Access useful registers for t0 stage
  private val t0_bankIdx         = getBankId(t0_trainIndex, NumBanks)
  private val t0_bankOffset      = getBankInnerIndex(t0_trainIndex, NumBanks, numSets)
  private val t0_trainReadUseful = usefulEntries(t0_bankIdx)(t0_bankOffset)
  private val t0_hitBufferIdVec  = VecInit(t0_entryWayHitOH.map(hitOH => OHToUInt(hitOH)))

  for (way <- 0 until numWay) {
    val entry  = t0_trainReadEntries(way)
    val useful = t0_trainReadUseful(way)
    io.train.t0_read(way).canGetPosition := t0_hitVec(way)
    io.train.t0_read(way).cfiPosition    := entry.cfiPosition
    io.train.t0_read(way).useful         := useful.value
  }

  // Training logic - stage 1
  private val t1_trainIndex       = RegNext(t0_trainIndex)
  private val t1_hitVec           = RegNext(t0_hitVec)
  private val t1_hitBufferIdVec   = RegNext(t0_hitBufferIdVec)
  private val t1_trainReadEntries = RegNext(t0_trainReadEntries)
  private val t1_bufferWriteId = VecInit(
    (t1_hitVec, t1_hitBufferIdVec, enqPtrVec).zipped.map {
      case (hit, id, enqPtr) => Mux(hit, id, enqPtr)
    }
  )

  // Access useful registers for t1 stage
  private val t1_bankIdx         = getBankId(t1_trainIndex, NumBanks)
  private val t1_bankOffset      = getBankInnerIndex(t1_trainIndex, NumBanks, numSets)
  private val t1_trainReadUseful = usefulEntries(t1_bankIdx)(t1_bankOffset)

  private val writeBufferValid = Wire(Vec(numWay, Bool()))

  for (way <- 0 until numWay) {
    val oldEntry       = t1_trainReadEntries(way)
    val oldTakenCtr    = oldEntry.takenCtr
    val oldUseful      = t1_trainReadUseful(way)
    val updateTakenCtr = io.train.t1_update(way).bits.updateTakenCtr

    // Update logic: either allocation or update
    val doAlloc  = io.train.t1_alloc.valid && io.train.t1_alloc.bits.wayMask(way)
    val doUpdate = io.train.t1_update(way).valid && io.train.t1_update(way).bits.updateValid
    writeBufferValid(way) := doAlloc || doUpdate

    // New entry values
    val newEntry = Wire(new MicroTageEntry)
    newEntry.valid := true.B
    newEntry.tag   := io.train.t1_tag
    newEntry.cfiPosition :=
      Mux(doAlloc, io.train.t1_alloc.bits.cfiPosition, io.train.t1_update(way).bits.updateCfiPosition)
    newEntry.takenCtr := Mux(
      doAlloc,
      Mux(io.train.t1_alloc.bits.taken, TakenCounter.WeakPositive, TakenCounter.WeakNegative),
      Mux(
        t1_hitVec(way),
        oldTakenCtr.getUpdate(io.train.t1_update(way).bits.updateTaken),
        updateTakenCtr.getUpdate(io.train.t1_update(way).bits.updateTaken)
      )
    )

    // Useful counter update
    val newUseful = Mux(
      doAlloc,
      if (tableId == 0) UsefulCounter.WeakNegative else UsefulCounter.WeakPositive,
      Mux(
        io.train.t1_update(way).bits.usefulValid,
        oldUseful.getUpdate(io.train.t1_update(way).bits.needUseful),
        oldUseful
      )
    )

    val newBufferEntry = Wire(new BufferEntry)
    newBufferEntry.valid     := true.B
    newBufferEntry.entryData := newEntry
    newBufferEntry.index     := t1_trainIndex
    newBufferEntry.waitWrite := true.B
    newBufferEntry.way       := way.U

    // Update buffer entry
    when(doAlloc || doUpdate) {
      entries(t1_bufferWriteId(way)) := newBufferEntry
    }

    // Update useful counter
    when(doAlloc || (io.train.t1_update(way).valid && io.train.t1_update(way).bits.usefulValid)) {
      t1_trainReadUseful(way) := newUseful
    }
  }

  // Useful counter reset logic
  when(io.usefulReset) {
    for (bankIdx <- 0 until NumBanks) {
      for (setIdx <- 0 until numSets / NumBanks) {
        for (wayIdx <- 0 until numWay) {
          val entry = usefulEntries(bankIdx)(setIdx)(wayIdx)
          if (tableId == 0) {
            usefulEntries(bankIdx)(setIdx)(wayIdx).value :=
              Mux(entry.value === 0.U, 0.U, entry.value - 1.U)
          } else {
            usefulEntries(bankIdx)(setIdx)(wayIdx).value := entry.value >> 1.U
          }
        }
      }
    }
  }

  // Buffer management logic
  private val bufferMask       = RegInit(VecInit(Seq.fill(numEntry)(false.B)))
  private val bufferMaskEnqVec = Wire(Vec(numWay, Vec(numEntry, Bool())))

  for (way <- 0 until numWay) {
    bufferMaskEnqVec(way) := VecInit(Seq.fill(numEntry)(false.B))
    when(writeBufferValid(way)) {
      bufferMaskEnqVec(way) := UIntToOH(t1_bufferWriteId(way), numEntry).asBools
    }
  }

  private val bufferMaskDeq = Wire(Vec(numEntry, Bool()))
  bufferMaskDeq := VecInit(Seq.fill(numEntry)(true.B))
  when(io.writeSuccess) {
    for (i <- 0 until numEntry) {
      bufferMaskDeq(i) := (i.U =/= deqPtr)
    }
  }

  val enqMask = VecInit((0 until numEntry).map {
    i => bufferMaskEnqVec.map(_(i)).reduce(_ || _) // Multi-way OR
  })

  val bufferMaskNext = Wire(Vec(numEntry, Bool()))
  for (i <- 0 until numEntry) {
    bufferMaskNext(i) := (bufferMask(i) || enqMask(i)) && bufferMaskDeq(i)
  }
  bufferMask := bufferMaskNext

  // Find two empty slots for enqueue
  private val (emptyIndex1, emptyIndex2, found1, found2, noZeros) = findTwoZeros(bufferMaskNext.asUInt)
  for (i <- 0 until numWay) {
    if (i == 0) {
      enqPtrVec(i) := emptyIndex1
    } else {
      enqPtrVec(i) := emptyIndex2
    }
  }

  when(io.writeSuccess || !bufferMask(deqPtr)) {
    deqPtr := deqPtr + 1.U
  }

  // Write-back control logic
  private val bufferCounter = RegNext(PopCount(bufferMaskNext), 0.U(log2Ceil(numEntry).W))
  private val forceWrite    = bufferCounter >= (numEntry - numWay).U

  io.tryWrite.valid           := bufferMask(deqPtr)
  io.tryWrite.bits.writeIndex := entries(deqPtr).index
  io.tryWrite.bits.writeData  := entries(deqPtr).entryData
  io.tryWrite.bits.forceWrite := forceWrite && bufferMask(deqPtr)
  io.tryWrite.bits.way        := entries(deqPtr).way
}

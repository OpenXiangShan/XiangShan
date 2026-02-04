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

class MicroTageTable(
    val numSets: Int,
    val numWay:  Int,
    val tableId: Int
)(implicit p: Parameters) extends MicroTageModule with Helpers {
  class MicroTageTableIO extends MicroTageBundle {
    class MicroTageReq extends Bundle {
      val readIndex: UInt = UInt(log2Ceil(numSets).W)
    }
    class MicroTageResp extends Bundle {
      val readEntries: Vec[MicroTageEntry]  = Vec(numWay, new MicroTageEntry)
      val readUsefuls: Vec[SaturateCounter] = Vec(numWay, new SaturateCounter(UsefulWidth))
    }
    class MicroTageUpdateInfo extends Bundle {
      val updateValid: Bool = Bool()
      val updateTaken: Bool = Bool()
      val usefulValid: Bool = Bool()
      val needUseful:  Bool = Bool()
    }
    class MicroTageAllocInfo extends Bundle {
      val taken:       Bool = Bool()
      val wayMask:     UInt = UInt(numWay.W)
      val cfiPosition: UInt = UInt(CfiPositionWidth.W)
      val tag:         UInt = UInt(MaxTagLen.W)
    }
    class MicroTageTrain extends Bundle {
      val t0_trainIndex: UInt                            = Input(UInt(log2Ceil(numSets).W))
      val t0_read:       Vec[MicroTageTrainRead]         = Output(Vec(numWay, new MicroTageTrainRead))
      val t1_update:     Vec[Valid[MicroTageUpdateInfo]] = Input(Vec(numWay, Valid(new MicroTageUpdateInfo)))
      val t1_alloc:      Valid[MicroTageAllocInfo]       = Input(Valid(new MicroTageAllocInfo))
    }
    val req:         Valid[MicroTageReq] = Input(Valid(new MicroTageReq))
    val resps:       MicroTageResp       = Output(new MicroTageResp)
    val train:       MicroTageTrain      = new MicroTageTrain
    val usefulReset: Bool                = Input(Bool())
  }
  val io       = IO(new MicroTageTableIO)
  val numBanks = 4
  require(numSets % numBanks == 0, s"numSets must be divisible by $numBanks")
  val setsPerBank   = numSets / numBanks
  val bankAddrWidth = log2Ceil(setsPerBank)

  private val entries = RegInit(VecInit(
    Seq.fill(numBanks)(
      VecInit(Seq.fill(setsPerBank)(
        VecInit(Seq.fill(numWay)(0.U.asTypeOf(new MicroTageEntry)))
      ))
    )
  ))

  private val useful = RegInit(VecInit(
    Seq.fill(numBanks)(
      VecInit(Seq.fill(setsPerBank)(
        VecInit(Seq.fill(numWay)(0.U.asTypeOf(new SaturateCounter(UsefulWidth))))
      ))
    )
  ))

  def getBankSel(addr:  UInt): UInt = addr(1, 0)
  def getBankAddr(addr: UInt): UInt = addr(bankAddrWidth + 1, 2)

  val predIndex       = io.req.bits.readIndex
  val a1_predBankSel  = RegNext(getBankSel(predIndex))
  val a1_predBankAddr = RegNext(getBankAddr(predIndex))

  val a1_predReadEntries = entries(a1_predBankSel)(a1_predBankAddr)
  val a1_predReadUseful  = useful(a1_predBankSel)(a1_predBankAddr)

  // io.resps.readEntries := RegNext(predReadEntries, 0.U.asTypeOf(Vec(numWay, new MicroTageEntry)))
  // io.resps.readUsefuls := RegNext(predReadUseful, 0.U.asTypeOf(Vec(numWay, new SaturateCounter(UsefulWidth))))
  io.resps.readEntries := a1_predReadEntries
  io.resps.readUsefuls := a1_predReadUseful

  val t0_trainIdx = io.train.t0_trainIndex
  val t0_bankSel  = getBankSel(t0_trainIdx)
  val t0_bankAddr = getBankAddr(t0_trainIdx)

  val t0_trainReadEntries = entries(t0_bankSel)(t0_bankAddr)
  val t0_trainReadUseful  = useful(t0_bankSel)(t0_bankAddr)

  for (way <- 0 until numWay) {
    io.train.t0_read(way).valid       := t0_trainReadEntries(way).valid
    io.train.t0_read(way).cfiPosition := t0_trainReadEntries(way).cfiPosition
    io.train.t0_read(way).useful      := t0_trainReadUseful(way).value
  }

  val t1_trainIdx                 = RegNext(t0_trainIdx)
  val t1_bankSel                  = RegNext(t0_bankSel)
  val t1_bankAddr                 = RegNext(t0_bankAddr)
  private val t1_trainReadEntries = entries(t1_bankSel)(t1_bankAddr)
  private val t1_trainReadUseful  = useful(t1_bankSel)(t1_bankAddr)

  // For each way, prepare updated entry and useful counter
  for (way <- 0 until numWay) {
    val oldEntry    = t1_trainReadEntries(way)
    val oldTakenCtr = oldEntry.takenCtr
    val oldUseful   = t1_trainReadUseful(way)

    // Update logic: either alloc or update
    val doAlloc  = io.train.t1_alloc.valid && io.train.t1_alloc.bits.wayMask(way)
    val doUpdate = io.train.t1_update(way).valid && io.train.t1_update(way).bits.updateValid

    // New entry values
    val newEntry = Wire(new MicroTageEntry)
    newEntry.valid       := true.B
    newEntry.tag         := Mux(doAlloc, io.train.t1_alloc.bits.tag, oldEntry.tag)
    newEntry.cfiPosition := Mux(doAlloc, io.train.t1_alloc.bits.cfiPosition, oldEntry.cfiPosition)
    newEntry.takenCtr := Mux(
      doAlloc,
      Mux(io.train.t1_alloc.bits.taken, TakenCounter.WeakPositive, TakenCounter.WeakNegative),
      oldTakenCtr.getUpdate(io.train.t1_update(way).bits.updateTaken)
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

    // New entry values
    when(doAlloc || doUpdate) {
      t1_trainReadEntries(way) := newEntry
    }
    when(doAlloc || (io.train.t1_update(way).valid && io.train.t1_update(way).bits.usefulValid)) {
      t1_trainReadUseful(way) := newUseful
    }
  }

  // Write back updated entry on valid update
  when(io.usefulReset) {
    for (bank <- 0 until numBanks) {
      for (setIdx <- 0 until setsPerBank) {
        for (way <- 0 until numWay) {
          val entry = useful(bank)(setIdx)(way)
          if (tableId == 0) {
            useful(bank)(setIdx)(way).value := Mux(entry.value === 0.U, 0.U, entry.value - 1.U)
          } else {
            useful(bank)(setIdx)(way).value := entry.value >> 1.U
          }
        }
      }
    }
  }
}

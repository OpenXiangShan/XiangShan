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

package xiangshan.frontend.bpu.sc

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.DataHoldBypass
import utility.XSPerfAccumulate
import utility.sram.SRAMTemplate
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.WriteBuffer
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

class ScTable(
    numSets:   Int,
    numWays:   Int,
    tableType: String,
    tableIdx:  Int
)(implicit p: Parameters)
    extends ScModule with HasScParameters with Helpers {
  class ScTableIO extends ScBundle {
    val predictReadReq:  Valid[ScTableReq] = Flipped(Valid(new ScTableReq(numSets, numWays)))
    val predictReadResp: Vec[ScEntry]      = Output(Vec(numWays, new ScEntry()))
    val trainReadReq:    Valid[ScTableReq] = Flipped(Valid(new ScTableReq(numSets, numWays)))
    val trainReadResp:   Vec[ScEntry]      = Output(Vec(numWays, new ScEntry()))
    val update:          ScTableTrain      = Input(new ScTableTrain(numSets, numWays))
    val sramResetDone:   Bool              = Output(Bool())
  }

  val io = IO(new ScTableIO())

  println(f"Sc$tableType[$tableIdx]:")
  println(f"  Size(set, bank, way): $numSets * $NumBanks * $numWays")
  println(f"  Address fields:")
  addrFields.show(indent = 4)

  def numRows: Int = numSets

  private val sram = Seq.fill(NumBanks)(
    Module(new SRAMTemplate(
      new ScEntry(),
      set = numRows,
      way = numWays,
      singlePort = true,
      shouldReset = true,
      withClockGate = true,
      hasMbist = hasMbist,
      hasSramCtl = hasSramCtl,
      suffix = Option("bpu_sc")
    ))
  )

  private val writeBuffer = Seq.tabulate(NumBanks)(bankIdx =>
    Module(new WriteBuffer(
      new ScTableSramWriteReq(numRows, numWays),
      WriteBufferSize,
      numPorts = 1,
      numWays = numWays,
      hasWayMask = true,
      hasPendingLookup = true,
      nameSuffix = s"sc${tableType}${tableIdx}_${bankIdx}"
    ))
  )

  private val perfPrefix = s"sc_${tableType}_${tableIdx}"

  // Prediction and training share the SRAM read port and pending lookup in each bank.
  sram.zip(writeBuffer).zipWithIndex.foreach { case ((bank, buffer), bankIdx) =>
    val predictReadValid = io.predictReadReq.valid && io.predictReadReq.bits.bankMask(bankIdx)
    val trainReadValid   = io.trainReadReq.valid && io.trainReadReq.bits.bankMask(bankIdx)
    val readSetIdx       = Mux(predictReadValid, io.predictReadReq.bits.setIdx, io.trainReadReq.bits.setIdx)
    bank.io.r.req.valid                := predictReadValid || trainReadValid
    bank.io.r.req.bits.setIdx          := readSetIdx
    buffer.io.pendingLookup.get.setIdx := readSetIdx
    assert(!(predictReadValid && trainReadValid), s"read conflict in sc${tableType}${tableIdx}_${bankIdx}")

    val writebackPending = buffer.io.read.head.valid
    XSPerfAccumulate(s"${perfPrefix}_bank${bankIdx}_wb_blocked_by_predict_read", writebackPending && predictReadValid)
    XSPerfAccumulate(s"${perfPrefix}_bank${bankIdx}_wb_blocked_by_train_read", writebackPending && trainReadValid)
  }

  io.sramResetDone := sram.map(_.io.resetDone).reduce(_ && _)

  private def lookupPending(bankMask: UInt): (UInt, Vec[ScEntry]) = {
    val pending     = Mux1H(bankMask, writeBuffer.map(_.io.pendingLookup.get.resp))
    val pendingMask = Mux(pending.valid, pending.bits.wayMask.get.asUInt, 0.U)
    val pendingData = VecInit(pending.bits.wayData.get.map(_.asTypeOf(new ScEntry())))
    (pendingMask, pendingData)
  }

  // Snapshot only buffered updates in S0. A same-cycle enqueue is visible to subsequent requests.
  private val s0_predictRead = io.predictReadReq
  private val (s0_predictPendingMask, s0_predictPendingData) = lookupPending(s0_predictRead.bits.bankMask)
  private val s1_predictReadValid   = RegNext(s0_predictRead.valid, init = false.B)
  private val s1_predictBankMask    = RegEnable(s0_predictRead.bits.bankMask, s0_predictRead.valid)
  private val s1_predictPendingMask = RegEnable(s0_predictPendingMask, s0_predictRead.valid)
  private val s1_predictPendingData = RegEnable(s0_predictPendingData, s0_predictRead.valid)
  private val s1_predictSramData    = Mux1H(s1_predictBankMask, sram.map(_.io.r.resp.data))
  private val s1_predictData = VecInit.tabulate(numWays) { way =>
    Mux(s1_predictPendingMask(way), s1_predictPendingData(way), s1_predictSramData(way))
  }
  // Hold the merged response across S1 stalls. Later training updates only affect new prediction requests.
  io.predictReadResp := DataHoldBypass(s1_predictData, s1_predictReadValid)

  // Count each prediction read once, even if its response is held across an S1 stall.
  XSPerfAccumulate(s"${perfPrefix}_predict_read", s1_predictReadValid)
  XSPerfAccumulate(s"${perfPrefix}_predict_wb_bypass", s1_predictReadValid && s1_predictPendingMask.orR)

  // update path table
  private val updateValid    = io.update.valid && io.update.wayMask.reduce(_ || _)
  private val updateIdx      = io.update.setIdx
  private val updateWayMask  = io.update.wayMask
  private val updateBankMask = io.update.bankMask

  private def matchesUpdate(req: ScTableReq): Bool =
    io.update.valid && updateIdx === req.setIdx && (updateBankMask & req.bankMask).orR

  // Training snapshots also include the update enqueued at the same edge to preserve the read-modify-write dependency.
  private val t0_trainRead = io.trainReadReq
  private val (t0_pendingMask, t0_pendingData) = lookupPending(t0_trainRead.bits.bankMask)
  private val t0_updateMask = Mux(matchesUpdate(t0_trainRead.bits), updateWayMask.asUInt, 0.U)
  private val t0_forwardedData = VecInit.tabulate(numWays) { way =>
    Mux(t0_updateMask(way), io.update.entryVec(way), t0_pendingData(way))
  }
  private val t1_trainReadValid = RegNext(t0_trainRead.valid, false.B)
  private val t1_trainRead      = RegEnable(t0_trainRead.bits, t0_trainRead.valid)
  private val t1_pendingMask    = RegEnable(t0_pendingMask | t0_updateMask, t0_trainRead.valid)
  private val t1_pendingData    = RegEnable(t0_forwardedData, t0_trainRead.valid)
  private val t1_sramData       = Mux1H(t1_trainRead.bankMask, sram.map(_.io.r.resp.data))

  // The immediately preceding training can reach T2 after the snapshot, so forward it again in T1.
  private val t1_updateHit = t1_trainReadValid && matchesUpdate(t1_trainRead)

  io.trainReadResp := VecInit.tabulate(numWays) { way =>
    val snapshotData = Mux(t1_pendingMask(way), t1_pendingData(way), t1_sramData(way))
    Mux(t1_updateHit && updateWayMask(way), io.update.entryVec(way), snapshotData)
  }

  writeBuffer.zip(updateBankMask.asBools).foreach {
    case (buffer, bankEnable) =>
      val writeValid = updateValid && bankEnable
      buffer.io.write.head.valid       := writeValid
      buffer.io.write.head.bits.setIdx := updateIdx
      buffer.io.write.head.bits.wayMask.get := Mux(
        writeValid,
        updateWayMask,
        VecInit.fill(numWays)(false.B)
      )
      buffer.io.write.head.bits.wayData.get := Mux(
        writeValid,
        VecInit(io.update.entryVec.map(_.asUInt)),
        VecInit.fill(numWays)(0.U.asTypeOf(new ScEntry()).asUInt)
      )
  }

  sram.zip(writeBuffer).zipWithIndex.foreach {
    case ((bank, buffer), bankIdx) =>
      bank.io.w.req.valid            := buffer.io.read.head.valid && !bank.io.r.req.valid
      bank.io.w.req.bits.setIdx      := buffer.io.read.head.bits.setIdx
      bank.io.w.req.bits.waymask.get := buffer.io.read.head.bits.wayMask.get.asUInt
      bank.io.w.req.bits.data        := VecInit(buffer.io.read.head.bits.wayData.get.map(_.asTypeOf(new ScEntry())))
      buffer.io.read.head.ready      := bank.io.w.req.ready && !bank.io.r.req.valid
      XSPerfAccumulate(s"${perfPrefix}_bank${bankIdx}_wb_writeback", bank.io.w.req.fire)
  }
}

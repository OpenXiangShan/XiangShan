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
import xiangshan.backend.datapath.DataConfig.VAddrBits
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.phr.PhrAllFoldedHistories

class MicroTageTable(
    val numSets: Int,
    val histLen: Int,
    val tagLen:  Int
)(implicit p: Parameters) extends MicroTageModule with Helpers {
  class MicroTageTableIO extends MicroTageBundle {
    class MicroTageReq extends Bundle {
      val startPc:        PrunedAddr            = new PrunedAddr(VAddrBits)
      val foldedPathHist: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    }
    class MicroTageResp extends Bundle {
      val taken:       Bool = Bool()
      val cfiPosition: UInt = UInt(CfiPositionWidth.W)
      val useful:      Bool = Bool()
    }
    class MicroTageUpdate extends Bundle {
      val startPc:                PrunedAddr            = new PrunedAddr(VAddrBits)
      val cfiPosition:            UInt                  = UInt(CfiPositionWidth.W)
      val alloc:                  Bool                  = Bool()
      val correct:                Bool                  = Bool()
      val foldedPathHistForTrain: PhrAllFoldedHistories = new PhrAllFoldedHistories(AllFoldedHistoryInfo)
      // val wayMask:   UInt         = UInt(NumWays.W)
    }
    val req:         MicroTageReq           = Input(new MicroTageReq)
    val resp:        Valid[MicroTageResp]   = Output(Valid(new MicroTageResp))
    val update:      Valid[MicroTageUpdate] = Input(Valid(new MicroTageUpdate))
    val usefulReset: Bool                   = Input(Bool())
  }
  class MicroTageEntry() extends MicroTageBundle {
    val valid:       Bool            = Bool()
    val tag:         UInt            = UInt(tagLen.W)
    val takenCtr:    SaturateCounter = new SaturateCounter(TakenCtrWidth)
    val cfiPosition: UInt            = UInt(CfiPositionWidth.W)
    val useful:      Bool            = Bool()
  }
  val io                    = IO(new MicroTageTableIO)
  private val entries       = RegInit(VecInit(Seq.fill(numSets)(0.U.asTypeOf(new MicroTageEntry))))
  private val usefulEntries = RegInit(VecInit(Seq.fill(numSets)(false.B)))

  val idxFhInfo    = new FoldedHistoryInfo(histLen, min(log2Ceil(numSets), histLen))
  val tagFhInfo    = new FoldedHistoryInfo(histLen, min(histLen, tagLen))
  val altTagFhInfo = new FoldedHistoryInfo(histLen, min(histLen, tagLen - 1))
  def getUnhashedIdx(pc: PrunedAddr): UInt = pc.toUInt(VAddrBits - 1, instOffsetBits)
  def getUnhashedTag(pc: PrunedAddr): UInt = pc.toUInt(VAddrBits - 1, log2Ceil(FetchBlockAlignSize))

  def computeHash(unhashedIdx: UInt, unhashedTag: UInt, allFh: PhrAllFoldedHistories): (UInt, UInt) = {
    val idxFh    = allFh.getHistWithInfo(idxFhInfo).foldedHist
    val tagFh    = allFh.getHistWithInfo(tagFhInfo).foldedHist
    val altTagFh = allFh.getHistWithInfo(altTagFhInfo).foldedHist
    val idx      = (unhashedIdx ^ idxFh)(log2Ceil(numSets) - 1, 0)
    val tag      = (unhashedTag ^ tagFh ^ (altTagFh << 1))(tagLen - 1, 0)
    (idx, tag)
  }

  // predict
  private val readUnhasedIdx   = getUnhashedIdx(io.req.startPc)
  private val readUnhasedTag   = getUnhashedTag(io.req.startPc)
  private val (s0_idx, s0_tag) = computeHash(readUnhasedIdx, readUnhasedTag, io.req.foldedPathHist)
  private val readEntry        = entries(s0_idx)
  private val readHit          = (readEntry.tag === s0_tag) && readEntry.valid

  io.resp.valid            := readHit
  io.resp.bits.taken       := readEntry.takenCtr.isPositive
  io.resp.bits.cfiPosition := readEntry.cfiPosition
  io.resp.bits.useful      := usefulEntries(s0_idx) // readEntry.useful

  // train
  private val trainUnhasedIdx = getUnhashedIdx(io.update.bits.startPc)
  private val trainUnhasedTag = getUnhashedTag(io.update.bits.startPc)
  private val (trainIdx, trainTag) =
    computeHash(trainUnhasedIdx, trainUnhasedTag, io.update.bits.foldedPathHistForTrain)
  private val oldCtr      = entries(trainIdx).takenCtr
  private val updateEntry = Wire(new MicroTageEntry)
  updateEntry.valid          := true.B
  updateEntry.tag            := trainTag
  updateEntry.takenCtr.value := Mux(io.update.bits.alloc, oldCtr.getNeutral, oldCtr.getUpdate(io.update.bits.correct))
  updateEntry.cfiPosition    := io.update.bits.cfiPosition
  updateEntry.useful         := Mux(io.update.bits.alloc || io.update.bits.correct, true.B, false.B)

  when(io.update.valid) {
    entries(trainIdx)       := updateEntry
    usefulEntries(trainIdx) := updateEntry.useful
  }

  when(io.usefulReset) {
    usefulEntries.foreach(_ := false.B)
  }
  // Per-index access distribution
  for (i <- 0 until numSets) {
    XSPerfAccumulate(f"update_idx_access_$i", (trainIdx === i.U) && io.update.valid)
  }
}

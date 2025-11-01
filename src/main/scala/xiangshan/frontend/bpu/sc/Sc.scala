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
import scala.math.min
import utility.XSPerfAccumulate
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BtbInfo
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.phr.PhrAllFoldedHistories

/**
 * This module is the implementation of the Statistical Corrector.
 */
class Sc(implicit p: Parameters) extends BasePredictor with HasScParameters with Helpers {

  class ScIO(implicit p: Parameters) extends BasePredictorIO with HasScParameters {
    val mbtbResult:          Vec[Valid[BtbInfo]]   = Input(Vec(NumBtbResultEntries, Valid(new BtbInfo)))
    val foldedPathHist:      PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val trainFoldedPathHist: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val takenMask:           Vec[Bool]             = Output(Vec(NumBtbResultEntries, Bool()))
    val meta:                ScMeta                = Output(new ScMeta())
  }
  val io: ScIO = IO(new ScIO)

  private val pathTable =
    PathTableInfos.map(tableInfo => Module(new ScPathTable(tableInfo.Size, tableInfo.HistoryLength)))

  private val scThreshold = RegInit(VecInit.tabulate(NumWays)(_ => ScThreshold(p)))

  private val resetDone = RegInit(false.B)
  when(pathTable.map(_.io.req.ready).reduce(_ && _)) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  /*
   *  predict pipeline stage 0
   */
  private val s0_fire       = Wire(Bool())
  private val s0_startVAddr = io.startVAddr
  s0_fire := io.stageCtrl.s0_fire && io.enable

  private val s0_idx: Seq[UInt] = PathTableInfos.map(info =>
    getIdx(
      s0_startVAddr,
      new FoldedHistoryInfo(info.HistoryLength, min(info.HistoryLength, log2Ceil(info.Size / NumWays))),
      io.foldedPathHist,
      info.Size / NumWays
    )
  )

  pathTable.zip(s0_idx).foreach { case (table, idx) =>
    table.io.req.valid := s0_fire
    table.io.req.bits  := idx
  }

  /*
   *  predict pipeline stage 1
   *  calculate eatch ctr's percsum
   */
  private val s1_fire       = io.stageCtrl.s1_fire && io.enable
  private val s1_startVAddr = RegEnable(io.startVAddr, s0_fire)
  private val s1_idx        = s0_idx.map(RegEnable(_, s0_fire))
  private val s1_resp: Seq[Vec[ScEntry]] = pathTable.map(_.io.resp)

  private val s1_pathPercsum: Vec[Vec[SInt]] = VecInit(s1_resp.map(entries =>
    VecInit(entries.map(entry => getPercsum(entry.ctr.value)))
  ))

  /*
   *  predict pipeline stage 2
   *  match entries and calculate final percSum
   */
  private val s2_fire       = io.stageCtrl.s2_fire && io.enable
  private val s2_startVAddr = RegEnable(s1_startVAddr, s1_fire)
  private val s2_resp       = VecInit(s1_resp.map(entries => VecInit(entries.map(RegEnable(_, s1_fire)))))
  private val s2_pathPercsum: Vec[Vec[SInt]] = VecInit(s1_pathPercsum.map(ps => VecInit(ps.map(RegEnable(_, s1_fire)))))

  private val s2_mbtbHitMask    = VecInit(io.mbtbResult.map(_.valid))
  private val s2_mbtbPositions  = VecInit(io.mbtbResult.map(_.bits.cfiPosition))
  private val s2_mbtbAttributes = VecInit(io.mbtbResult.map(_.bits.attribute))
  private val s2_totalPercsum: Vec[SInt] = WireInit(VecInit.fill(NumWays)(0.S(ctrWidth.W)))
  private val s2_hitMask:      Vec[Bool] = WireInit(VecInit.fill(NumWays)(false.B))
  require(NumWays == s2_mbtbHitMask.length, s"NumWays $NumWays != s2_mbtbHitMask.length ${s2_mbtbHitMask.length}")
  s2_mbtbHitMask.zip(s2_mbtbAttributes).zip(s2_mbtbPositions).zipWithIndex.map {
    case (((hit, attr), pos), i) =>
      when(hit && attr.isConditional) {
        val pathIdx = pos(log2Ceil(NumWays) - 1, 0)
        s2_totalPercsum(i) := s2_pathPercsum.map(v => v(pathIdx)).reduce(_ +& _)
        s2_hitMask(i)      := true.B
      }
  }

  private val s2_scPred: Vec[Bool] = VecInit(s2_totalPercsum.map(_ > 0.S))

  private val s2_thresholds    = scThreshold.map(entry => entry.thres.value)
  private val updateThresholds = VecInit(s2_thresholds.map(t => (t << 3) +& 21.U))

  private val s2_useScPred = WireInit(VecInit.fill(NumWays)(false.B))
  s2_useScPred.zip(s2_totalPercsum).zip(s2_thresholds).zip(s2_hitMask).map {
    case (((u, sum), thres), hit) =>
      val aboveThres = aboveThreshold(sum, thres)
      when(hit && aboveThres) {
        u := true.B
      }
  }
  io.takenMask      := s2_useScPred.zip(s2_scPred).map { case (u, p) => u && p }
  io.meta.scResp    := VecInit(s2_resp.map(v => VecInit(v.map(_.asUInt))))
  io.meta.scPred    := s2_scPred
  io.meta.useScPred := s2_useScPred

  dontTouch(s2_totalPercsum)
  dontTouch(s2_hitMask)
  dontTouch(s2_scPred)
  dontTouch(s2_useScPred)

  /*
   *  train pipeline stage 1
   */
  private val t1_trainValid = RegNext(io.train.valid, init = false.B)
  private val t1_train      = RegEnable(io.train.bits, io.train.valid)
  private val t1_branches   = t1_train.branches
  private val t1_setIdx = PathTableInfos.map(info =>
    getIdx(
      t1_train.startVAddr,
      new FoldedHistoryInfo(info.HistoryLength, min(info.HistoryLength, log2Ceil(info.Size / NumWays))),
      io.trainFoldedPathHist,
      info.Size / NumWays
    )
  )
  private val t1_meta    = t1_train.meta.sc
  private val t1_oldCtrs = VecInit(t1_meta.scResp.map(v => VecInit(v.map(r => r.asTypeOf(new ScEntry())))))
  private val t1_writeValidVec =
    VecInit(t1_branches.map(b => b.valid && b.bits.attribute.isConditional && t1_trainValid))
  private val t1_writeValid        = t1_writeValidVec.reduce(_ || _)
  private val t1_branchesTakenMask = VecInit(t1_branches.map(b => b.valid && b.bits.taken))
  private val t1_branchesWayIdxVec = VecInit(t1_branches.map(b => b.bits.cfiPosition(log2Ceil(NumWays) - 1, 0)))
  require(
    t1_branchesWayIdxVec(0).getWidth == log2Ceil(NumWays),
    s"t1_branchesWayIdxVec entry width: ${t1_branchesWayIdxVec(0).getWidth} should be the same as log2Ceil(NumWays): ${log2Ceil(NumWays)}"
  )

  private val t1_mismatchMask = WireInit(VecInit.fill(NumWays)(false.B))
  private val t1_writeThresVec = VecInit(scThreshold.indices.map { wayIdx =>
    val updated = t1_branchesWayIdxVec.zip(t1_branchesTakenMask).foldLeft(scThreshold(wayIdx)) {
      case (prevThres, (branchWayIdx, taken)) =>
        val shouldUpdate = branchWayIdx === wayIdx.U
        val mismatch     = taken =/= t1_meta.scPred(wayIdx)
        t1_mismatchMask(wayIdx) := mismatch && shouldUpdate
        val nextThres = prevThres.update(mismatch)
        Mux(shouldUpdate, nextThres, prevThres)
    }
    WireInit(updated)
  })
  dontTouch(t1_writeThresVec)

  private val t1_writeEntryVec = WireInit(
    VecInit.fill(PathTableSize)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  private val t1_writeWayMask = WireInit(VecInit.fill(PathTableSize)(VecInit.fill(NumWays)(false.B)))

  t1_oldCtrs.zip(t1_writeEntryVec).zip(t1_writeWayMask).foreach {
    case ((oldEntries: Vec[ScEntry], writeEntries: Vec[ScEntry]), writeWayMask: Vec[Bool]) =>
      oldEntries.zip(writeEntries).zipWithIndex.foreach { case ((oldEntry, newEntry), wayIdx) =>
        val newCtr = t1_branchesTakenMask.zip(t1_branchesWayIdxVec).zip(t1_writeValidVec).foldLeft(oldEntry.ctr) {
          case (prevCtr, ((writeTaken, writeWayIdx), writeValidalid)) =>
            val needUpdate = writeValidalid && writeWayIdx === wayIdx.U
            val nextValue  = prevCtr.getUpdate(writeTaken)
            val nextCtr    = WireInit(prevCtr)
            nextCtr.value             := nextValue
            writeWayMask(writeWayIdx) := needUpdate
            Mux(needUpdate, nextCtr, prevCtr)
        }
        dontTouch(newCtr)
        newEntry.ctr := WireInit(newCtr)
      }
  }

  dontTouch(t1_writeEntryVec)

  pathTable zip t1_setIdx zip t1_writeEntryVec zip t1_writeWayMask foreach {
    case (((table, idx), writeEntries), wayMask) =>
      table.io.update.valid    := t1_writeValid
      table.io.update.setIdx   := idx
      table.io.update.wayMask  := wayMask
      table.io.update.entryVec := writeEntries
  }

  when(t1_writeValid) {
    scThreshold := t1_writeThresVec
  }
  XSPerfAccumulate("sc_pred_wrong", t1_writeValid && t1_mismatchMask.reduce(_ || _))
  // TODO: add more performance counters
}

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
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.mbtb.MainBtbResult
import xiangshan.frontend.bpu.phr.PhrAllFoldedHistories

/**
 * This module is the implementation of the Statistical Corrector.
 */
class Sc(implicit p: Parameters) extends BasePredictor with HasScParameters with Helpers {

  class ScIO(implicit p: Parameters) extends BasePredictorIO with HasScParameters {
    val mbtbResult:          MainBtbResult         = Input(new MainBtbResult)
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
    VecInit(entries.map(entry => getPercsum(entry.ctrs.value)))
  ))

  /*
   *  predict pipeline stage 2
   *  match entries and calculate final percSum
   */
  private val s2_fire       = io.stageCtrl.s2_fire && io.enable
  private val s2_startVAddr = RegEnable(s1_startVAddr, s1_fire)
  private val s2_resp       = VecInit(s1_resp.map(entries => VecInit(entries.map(RegEnable(_, s1_fire)))))
  private val s2_pathPercsum: Vec[Vec[SInt]] = VecInit(s1_pathPercsum.map(ps => VecInit(ps.map(RegEnable(_, s1_fire)))))

  // filter out branches that behind the fetch block start address
  private val s2_mbtbHitMask = io.mbtbResult.hitMask.zip(io.mbtbResult.positions).map {
    case (hit, position) =>
      hit && position >= s2_startVAddr(FetchBlockSizeWidth - 1, 1)
  }
  private val s2_mbtbPositions  = io.mbtbResult.positions
  private val s2_mbtbAttributes = io.mbtbResult.attributes
  private val totalPercsum: Vec[SInt] = WireInit(VecInit.fill(NumWays)(0.S(ctrWidth.W)))
  private val totalHit:     Vec[Bool] = WireInit(VecInit.fill(NumWays)(false.B))
  require(NumWays == s2_mbtbHitMask.length, s"NumWays $NumWays != s2_mbtbHitMask.length ${s2_mbtbHitMask.length}")
  s2_mbtbHitMask.zip(s2_mbtbAttributes).zip(s2_mbtbPositions).zipWithIndex.map {
    case (((hit, attr), pos), i) =>
      when(hit && attr.isConditional) {
        val pathIdx = pos(log2Ceil(NumWays) - 1, 0)
        totalPercsum(i) := s2_pathPercsum.map(v => v(pathIdx)).reduce(_ +& _)
        totalHit(i)     := true.B
      }
  }

  private val s2_scPred: Vec[Bool] = VecInit(totalPercsum.map(_ > 0.S))

  private val totalThresholds  = scThreshold.map(entry => entry.thres)
  private val updateThresholds = VecInit(totalThresholds.map(t => (t << 3) +& 21.U))

  private val useScPred = WireInit(VecInit.fill(NumWays)(false.B))
  useScPred.zip(totalPercsum).zip(totalThresholds).zip(totalHit).map {
    case (((u, sum), thres), hit) =>
      val aboveThres = aboveThreshold(sum, thres)
      when(hit && aboveThres) {
        u := true.B
      }
  }
  io.takenMask      := useScPred.zip(s2_scPred).map { case (u, p) => u && p }
  io.meta.scResp    := s2_resp
  io.meta.scPred    := s2_scPred
  io.meta.useScPred := useScPred

  /*
   *  train pipeline stage 1
   */
  private val t1_trainValid = RegEnable(io.train.valid, io.enable)
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
  private val t1_meta  = RegEnable(io.meta, io.train.valid)
  private val t1_taken = t1_train.branches(0).bits.taken

  private val t1_writeValid =
    t1_trainValid && t1_train.branches(0).valid && t1_train.branches(0).bits.attribute.isConditional
  private val t1_oldCtrs  = t1_meta.scResp
  private val t1_newCtrs  = WireInit(VecInit.fill(PathTableSize)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry()))))
  private val t1_wayIdx   = t1_train.branches(0).bits.cfiPosition(log2Ceil(NumWays) - 1, 0)
  private val t1_newThres = scThreshold(t1_wayIdx).update(t1_taken =/= t1_meta.scPred(t1_wayIdx))
  t1_oldCtrs zip t1_newCtrs foreach {
    case (oldEntry: Vec[ScEntry], newEntries: Vec[ScEntry]) =>
      when(t1_writeValid) {
        newEntries(t1_wayIdx).ctrs.value := oldEntry(t1_wayIdx).ctrs.getUpdate(t1_taken)
      }.otherwise {
        newEntries.map(_ := 0.U.asTypeOf(newEntries(0)))
      }
  }

  pathTable zip t1_setIdx zip t1_newCtrs foreach {
    case ((table, idx), newEntries) =>
      table.io.update.valid  := t1_writeValid
      table.io.update.setIdx := idx
      table.io.update.wayIdx := t1_wayIdx
      table.io.update.entry  := newEntries(t1_wayIdx)
  }

  when(t1_writeValid && t1_meta.scPred(t1_wayIdx) =/= t1_taken) {
    scThreshold(t1_wayIdx) := t1_newThres
  }

  XSPerfAccumulate(
    "sc_pred_right",
    t1_writeValid && (t1_meta.scPred(t1_wayIdx) === t1_taken) && t1_meta.useScPred(t1_wayIdx)
  )
  // TODO： add more performance counters
}

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
import xiangshan.frontend.bpu.history.ghr.GhrEntry
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories
import xiangshan.frontend.bpu.mbtb.MainBtbResult

/**
 * This module is the implementation of the Statistical Corrector.
 */
class Sc(implicit p: Parameters) extends BasePredictor with HasScParameters with Helpers {

  class ScIO(implicit p: Parameters) extends BasePredictorIO with HasScParameters {
    val mbtbResult:          MainBtbResult         = Input(new MainBtbResult)
    val foldedPathHist:      PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val s3_override:         Bool                  = Input(Bool())
    val ghr:                 GhrEntry              = Input(new GhrEntry())
    val trainFoldedPathHist: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val takenMask:           Vec[Bool]             = Output(Vec(NumBtbResultEntries, Bool()))
    val meta:                ScMeta                = Output(new ScMeta())
  }
  val io: ScIO = IO(new ScIO)

  /*
   * stage control signals
   */
  private val s0_fire = io.stageCtrl.s0_fire && io.enable
  private val s1_fire = io.stageCtrl.s1_fire && io.enable
  private val s2_fire = io.stageCtrl.s2_fire && io.enable
  private val s3_fire = io.stageCtrl.s3_fire && io.enable

  private val pathTable =
    PathTableInfos.map(tableInfo => Module(new ScTable(tableInfo.Size, tableInfo.HistoryLength)))

  private val globalTable =
    GlobalTableInfos.map(tableInfo => Module(new ScTable(tableInfo.Size, tableInfo.HistoryLength)))

  private val scThreshold = RegInit(VecInit.tabulate(NumWays)(_ => ScThreshold(p)))

  private val resetDone = RegInit(false.B)
  when(pathTable.map(_.io.req.ready).reduce(_ && _) && globalTable.map(_.io.req.ready).reduce(_ && _)) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  /*
   * ghr stage ctrl signals
   */
  private val s0_ghr      = WireInit(0.U.asTypeOf(io.ghr))
  private val s1_ghr      = RegEnable(s0_ghr, s0_fire)
  private val s2_ghr      = RegEnable(s1_ghr, s1_fire)
  private val s3_override = io.s3_override && io.enable

  /*
   * Only available when an bp-override occurs in s3
   * if s3_override,  the current s0 should use s2,
   * the next cycle should use the current s1,
   * and the next cycle should use the current io.ghr
   * The state machine is used to store the above two historical records
   */
  private val idle :: stage1 :: stage2 :: stage3 :: Nil = Enum(4)

  private val ghrStateReg = RegInit(idle)
  private val stage2Ghr: GhrEntry = RegInit(0.U.asTypeOf(io.ghr))
  private val stage3Ghr: GhrEntry = RegInit(0.U.asTypeOf(io.ghr))
  switch(ghrStateReg) {
    is(idle) {
      when(!s3_override) {
        ghrStateReg := idle
        s0_ghr      := io.ghr
      }.elsewhen(s3_override) {
        ghrStateReg := stage1
        s0_ghr      := s2_ghr
        stage2Ghr   := s1_ghr
        stage3Ghr   := io.ghr
      }
    }
    is(stage1) {
      when(s0_fire) {
        ghrStateReg := stage2
        s0_ghr      := stage2Ghr
      }
    }
    is(stage2) {
      when(s0_fire) {
        ghrStateReg := stage3
        s0_ghr      := stage3Ghr
      }
    }
    is(stage3) {
      when(s0_fire && !s3_override) {
        ghrStateReg := idle
        s0_ghr      := io.ghr
      }.elsewhen(s3_override) {
        ghrStateReg := stage1
        s0_ghr      := s2_ghr
        stage2Ghr   := s1_ghr
        stage3Ghr   := io.ghr
      }
    }
  }

  /*
   *  predict pipeline stage 0
   */
  private val s0_startVAddr = io.startVAddr
  private val s0_pathIdx: Seq[UInt] = PathTableInfos.map(info =>
    getPathTableIdx(
      s0_startVAddr,
      new FoldedHistoryInfo(info.HistoryLength, min(info.HistoryLength, log2Ceil(info.Size / NumWays))),
      io.foldedPathHist,
      info.Size / NumWays
    )
  )

  private val s0_globalIdx: Seq[UInt] = GlobalTableInfos.map(info =>
    getGlobalTableIdx(
      s0_startVAddr,
      s0_ghr.value.asUInt,
      info.Size / NumWays
    )
  )

  pathTable.zip(s0_pathIdx).foreach { case (table, idx) =>
    table.io.req.valid := s0_fire
    table.io.req.bits  := idx
  }

  globalTable.zip(s0_globalIdx).foreach { case (table, idx) =>
    table.io.req.valid := s0_fire && s0_ghr.valid // if ghr invalid not request global table
    table.io.req.bits  := idx
  }

  /*
   *  predict pipeline stage 1
   *  calculate each ctr's percsum
   */
  private val s1_startVAddr = RegEnable(io.startVAddr, s0_fire)
  private val s1_pathResp:   Seq[Vec[ScEntry]] = pathTable.map(_.io.resp)
  private val s1_globalResp: Seq[Vec[ScEntry]] = globalTable.map(_.io.resp)
  private val s1_allResp:    Vec[Vec[ScEntry]] = VecInit(s1_pathResp ++ s1_globalResp)

  private val s1_allPercsum: Vec[Vec[SInt]] = VecInit(s1_allResp.map(entries =>
    VecInit(entries.map(entry => getPercsum(entry.ctr.value)))
  ))
  require(
    s1_allPercsum.length == PathTableInfos.length + GlobalTableInfos.length,
    s"s1_allPercsum length ${s1_allPercsum.length} != " +
      s"PathTableInfos.length + GlobalTableInfos.length ${PathTableInfos.length + GlobalTableInfos.length}"
  )
  // Calculate sumPercsum in advance
  private val s1_sumPercsum: Vec[SInt] = VecInit.tabulate(NumWays)(j => s1_allPercsum.map(_(j)).reduce(_ +& _))
  require(
    s1_sumPercsum.length == NumWays,
    s"s1_sumPercsum length ${s1_sumPercsum.length} != NumWays $NumWays"
  )

  /*
   *  predict pipeline stage 2
   *  match entries and calculate final percSum
   */
  private val s2_startVAddr = RegEnable(s1_startVAddr, s1_fire)
  private val s2_pathResp   = s1_pathResp.map(entries => VecInit(entries.map(RegEnable(_, s1_fire))))
  private val s2_globalResp = s1_globalResp.map(entries => VecInit(entries.map(RegEnable(_, s1_fire))))
  private val s2_sumPercsum: Vec[SInt] = VecInit(s1_sumPercsum.map(RegEnable(_, s1_fire)))

  private val s2_mbtbHitMask    = io.mbtbResult.hitMask
  private val s2_mbtbPositions  = io.mbtbResult.positions
  private val s2_mbtbAttributes = io.mbtbResult.attributes
  private val s2_totalPercsum: Vec[SInt] = WireInit(VecInit.fill(NumWays)(0.S(ctrWidth.W)))
  private val s2_hitMask:      Vec[Bool] = WireInit(VecInit.fill(NumWays)(false.B))
  require(NumWays == s2_mbtbHitMask.length, s"NumWays $NumWays != s2_mbtbHitMask.length ${s2_mbtbHitMask.length}")

  s2_mbtbHitMask.zip(s2_mbtbAttributes).zip(s2_mbtbPositions).zipWithIndex.map {
    case (((hit, attr), pos), i) =>
      when(hit && attr.isConditional) {
        val pathIdx = pos(log2Ceil(NumWays) - 1, 0)
        s2_totalPercsum(i) := s2_sumPercsum(pathIdx)
        s2_hitMask(i)      := true.B
      }
  }

  private val s2_scPred: Vec[Bool] = VecInit(s2_totalPercsum.map(_ > 0.S))

  private val s2_thresholds = scThreshold.map(entry => entry.thres.value)

  private val s2_useScPred = WireInit(VecInit.fill(NumWays)(false.B))
  s2_useScPred.zip(s2_totalPercsum).zip(s2_thresholds).zip(s2_hitMask).map {
    case (((u, sum), thres), hit) =>
      val aboveThres = aboveThreshold(sum, thres)
      when(hit && aboveThres) {
        u := true.B
      }
  }
  private val s2_pred = s2_useScPred.zip(s2_scPred).map { case (u, p) => u && p }

  io.takenMask := VecInit(s2_pred.map(RegEnable(_, s2_fire)))

  io.meta.scPathResp   := VecInit(s2_pathResp.map(v => VecInit(v.map(s => RegEnable(s.asUInt, s2_fire)))))
  io.meta.scGlobleResp := VecInit(s2_globalResp.map(v => VecInit(v.map(s => RegEnable(s.asUInt, s2_fire)))))
  io.meta.scPred       := RegEnable(s2_scPred, s2_fire)
  io.meta.scGhr        := RegEnable(s2_ghr.value.asUInt, s2_fire)
  io.meta.useScPred    := RegEnable(s2_useScPred, s2_fire)

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
  private val t1_meta       = t1_train.meta.sc

  private val t1_pathSetIdx = PathTableInfos.map(info =>
    getPathTableIdx(
      t1_train.startVAddr,
      new FoldedHistoryInfo(info.HistoryLength, min(info.HistoryLength, log2Ceil(info.Size / NumWays))),
      io.trainFoldedPathHist,
      info.Size / NumWays
    )
  )

  private val t1_globalSetIdx = GlobalTableInfos.map(info =>
    getGlobalTableIdx(
      t1_train.startVAddr,
      t1_meta.scGhr,
      info.Size / NumWays
    )
  )

  private val t1_oldPathCtrs   = VecInit(t1_meta.scPathResp.map(v => VecInit(v.map(r => r.asTypeOf(new ScEntry())))))
  private val t1_oldGlobalCtrs = VecInit(t1_meta.scGlobleResp.map(v => VecInit(v.map(r => r.asTypeOf(new ScEntry())))))
  private val t1_writeValidVec =
    VecInit(t1_branches.map(b => b.valid && b.bits.attribute.isConditional && t1_trainValid))
  private val t1_writeValid        = t1_writeValidVec.reduce(_ || _)
  private val t1_branchesTakenMask = VecInit(t1_branches.map(b => b.valid && b.bits.taken))
  private val t1_branchesWayIdxVec = VecInit(t1_branches.map(b => b.bits.cfiPosition(log2Ceil(NumWays) - 1, 0)))
  require(
    t1_branchesWayIdxVec(0).getWidth == log2Ceil(NumWays),
    s"t1_branchesWayIdxVec entry width: ${t1_branchesWayIdxVec(0).getWidth} " +
      s"should be the same as log2Ceil(NumWays): ${log2Ceil(NumWays)}"
  )

  // Accumulate update information for multiple branches using update methods
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

  private val t1_writePathEntryVec = WireInit(
    VecInit.fill(PathTableSize)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  private val t1_writeWayMask = WireInit(VecInit.fill(NumWays)(false.B))
  t1_branchesWayIdxVec.zip(t1_writeValidVec).foreach {
    case (wayIdx, writeValid) =>
      when(writeValid) {
        t1_writeWayMask(wayIdx) := true.B
      }
  }
  t1_oldPathCtrs.zip(t1_writePathEntryVec).foreach {
    case (oldEntries: Vec[ScEntry], writeEntries: Vec[ScEntry]) =>
      oldEntries.zip(writeEntries).zipWithIndex.foreach { case ((oldEntry, newEntry), wayIdx) =>
        val newCtr = t1_branchesTakenMask.zip(t1_branchesWayIdxVec).zip(t1_writeValidVec).foldLeft(oldEntry.ctr) {
          case (prevCtr, ((writeTaken, writeWayIdx), writeValid)) =>
            val needUpdate = writeValid && writeWayIdx === wayIdx.U
            val nextValue  = prevCtr.getUpdate(writeTaken)
            val nextCtr    = WireInit(prevCtr)
            nextCtr.value := nextValue
            Mux(needUpdate, nextCtr, prevCtr)
        }
        newEntry.ctr := WireInit(newCtr)
      }
  }

  pathTable zip t1_pathSetIdx zip t1_writePathEntryVec foreach {
    case ((table, idx), writeEntries) =>
      table.io.update.valid    := t1_writeValid
      table.io.update.setIdx   := idx
      table.io.update.wayMask  := t1_writeWayMask
      table.io.update.entryVec := writeEntries
  }

  private val t1_writeGlobalEntryVec = WireInit(
    VecInit.fill(GlobalTableSize)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )

  t1_oldGlobalCtrs.zip(t1_writeGlobalEntryVec).foreach {
    case (oldEntries: Vec[ScEntry], writeEntries: Vec[ScEntry]) =>
      oldEntries.zip(writeEntries).zipWithIndex.foreach { case ((oldEntry, newEntry), wayIdx) =>
        val newCtr = t1_branchesTakenMask.zip(t1_branchesWayIdxVec).zip(t1_writeValidVec).foldLeft(oldEntry.ctr) {
          case (prevCtr, ((writeTaken, writeWayIdx), writeValid)) =>
            val needUpdate = writeValid && writeWayIdx === wayIdx.U
            val nextValue  = prevCtr.getUpdate(writeTaken)
            val nextCtr    = WireInit(prevCtr)
            nextCtr.value := nextValue
            Mux(needUpdate, nextCtr, prevCtr)
        }
        newEntry.ctr := WireInit(newCtr)
      }
  }

  globalTable zip t1_globalSetIdx zip t1_writeGlobalEntryVec foreach {
    case ((table, idx), writeEntries) =>
      table.io.update.valid    := t1_writeValid
      table.io.update.setIdx   := idx
      table.io.update.wayMask  := t1_writeWayMask
      table.io.update.entryVec := writeEntries
  }

  when(t1_writeValid) {
    scThreshold := t1_writeThresVec
  }

  XSPerfAccumulate("sc_pred_wrong", t1_writeValid && t1_mismatchMask.reduce(_ || _))
  // TODO: add more performance counters
}

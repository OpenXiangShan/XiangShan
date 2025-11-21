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
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.history.ghr.GhrEntry
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

/**
 * This module is the implementation of the Statistical Corrector.
 */
class Sc(implicit p: Parameters) extends BasePredictor with HasScParameters with Helpers {

  class ScIO(implicit p: Parameters) extends BasePredictorIO with HasScParameters {
    val mbtbResult:          Vec[Valid[Prediction]] = Input(Vec(NumBtbResultEntries, Valid(new Prediction)))
    val foldedPathHist:      PhrAllFoldedHistories  = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val s3_override:         Bool                   = Input(Bool())
    val ghr:                 GhrEntry               = Input(new GhrEntry())
    val trainFoldedPathHist: PhrAllFoldedHistories  = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val takenMask:           Vec[Bool]              = Output(Vec(NumBtbResultEntries, Bool()))
    val meta:                ScMeta                 = Output(new ScMeta())
  }
  val io: ScIO = IO(new ScIO)

  /*
   * stage control signals
   */
  private val s0_fire = io.stageCtrl.s0_fire && io.enable
  private val s1_fire = io.stageCtrl.s1_fire && io.enable
  private val s2_fire = io.stageCtrl.s2_fire && io.enable
  private val s3_fire = io.stageCtrl.s3_fire && io.enable

  private val ctrl = WireInit(0.U.asTypeOf(new TableCtrl()))
  ctrl.pathEnable   := true.B
  ctrl.globalEnable := true.B
  ctrl.biasEnable   := true.B

  /*
   *  instantiate tables
   */
  private val pathTable = PathTableInfos.zipWithIndex.map { case (info, i) =>
    Module(new ScTable(info.Size, NumWays, "pathTable", i))
  }

  private val globalTable = GlobalTableInfos.zipWithIndex.map { case (info, i) =>
    Module(new ScTable(info.Size, NumWays, "globalTable", i))
  }

  private val biasTable = Module(new ScTable(BiasTableSize, BiasTableNumWays, "biasTable", 0))

  private val scThreshold = RegInit(VecInit.tabulate(NumWays)(_ => ScThreshold(p)))

  private val resetDone = RegInit(false.B)
  when(pathTable.map(_.io.req.ready).reduce(_ && _) &&
    globalTable.map(_.io.req.ready).reduce(_ && _) &&
    biasTable.io.req.ready) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  io.train.ready := true.B

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
  private val s0_startPc = io.startPc
  private val s0_pathIdx: Seq[UInt] = PathTableInfos.map(info =>
    getPathTableIdx(
      s0_startPc,
      new FoldedHistoryInfo(info.HistoryLength, min(info.HistoryLength, log2Ceil(info.Size / NumWays))),
      io.foldedPathHist,
      info.Size / NumWays
    )
  )

  private val s0_globalIdx: Seq[UInt] = GlobalTableInfos.map(info =>
    getGlobalTableIdx(
      s0_startPc,
      s0_ghr.value.asUInt,
      info.Size / NumWays
    )
  )

  private val s0_biasIdx: UInt = getBiasTableIdx(s0_startPc, BiasTableSize)

  pathTable.zip(s0_pathIdx).foreach { case (table, idx) =>
    table.io.req.valid := s0_fire && ctrl.pathEnable
    table.io.req.bits  := idx
  }

  globalTable.zip(s0_globalIdx).foreach { case (table, idx) =>
    table.io.req.valid := s0_fire && s0_ghr.valid && ctrl.globalEnable // if ghr invalid not request global table
    table.io.req.bits  := idx
  }

  biasTable.io.req.valid := s0_fire && ctrl.biasEnable
  biasTable.io.req.bits  := s0_biasIdx

  /*
   *  predict pipeline stage 1
   *  calculate each ctr's percsum
   */
  private val s1_startPc = RegEnable(io.startPc, s0_fire)
  private val s1_pathResp: Vec[Vec[ScEntry]] = Mux(
    ctrl.pathEnable,
    VecInit(pathTable.map(_.io.resp)),
    VecInit.fill(PathTableSize)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  // if s0_ghr invalid, global table resp is also invalid
  private val s1_globalResp: Vec[Vec[ScEntry]] =
    Mux(
      s1_ghr.valid && ctrl.globalEnable,
      VecInit(globalTable.map(_.io.resp)),
      VecInit.fill(GlobalTableSize)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
    )
  private val s1_biasResp: Vec[ScEntry] = Mux(
    ctrl.biasEnable,
    biasTable.io.resp,
    VecInit.fill(BiasTableNumWays)(0.U.asTypeOf(new ScEntry()))
  )
  private val s1_allResp: Vec[Vec[ScEntry]] = VecInit(s1_pathResp ++ s1_globalResp)

  private val s1_biasPercsum: Vec[SInt] = VecInit(s1_biasResp.map(entry => getPercsum(entry.ctr.value)))
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
  private val s2_startPc    = RegEnable(s1_startPc, s1_fire)
  private val s2_pathResp   = s1_pathResp.map(entries => VecInit(entries.map(RegEnable(_, s1_fire))))
  private val s2_globalResp = s1_globalResp.map(entries => VecInit(entries.map(RegEnable(_, s1_fire))))
  private val s2_biasResp   = VecInit(s1_biasResp.map(RegEnable(_, s1_fire)))
  private val s2_biasUsedResp: Vec[ScEntry] = WireInit(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  private val s2_biasPercsum:  Vec[SInt]    = VecInit(s1_biasPercsum.map(RegEnable(_, s1_fire)))
  private val s2_sumPercsum:   Vec[SInt]    = VecInit(s1_sumPercsum.map(RegEnable(_, s1_fire)))

  private val s2_mbtbResult    = io.mbtbResult
  private val s2_condTakenMask = io.tageInfo.condTakenMask
  private val s2_providerValid = VecInit(io.tageInfo.providerTakenCtr.map(_.valid))
  private val s2_providerCtr   = VecInit(io.tageInfo.providerTakenCtr.map(_.bits))

  private val s2_biasIdxLowBits = VecInit(s2_condTakenMask.zip(s2_providerValid).zip(s2_providerCtr).map {
    case ((taken, valid), ctr) => Cat(valid && ctr.isWeak, taken)
  })
  private val s2_totalPercsum: Vec[SInt] = WireInit(VecInit.fill(NumWays)(0.S(ctrWidth.W)))
  private val s2_hitMask:      Vec[Bool] = WireInit(VecInit.fill(NumWays)(false.B))
  require(NumWays == s2_mbtbResult.length, s"NumWays $NumWays != s2_mbtbHitMask.length ${s2_mbtbResult.length}")

  s2_mbtbResult.zip(s2_condTakenMask).zipWithIndex.map {
    case ((mbtbResult, taken), i) =>
      val hit      = mbtbResult.valid && mbtbResult.bits.attribute.isConditional
      val tableIdx = mbtbResult.bits.cfiPosition(log2Ceil(NumWays) - 1, 0)
      val biasIdx  = Cat(tableIdx, s2_biasIdxLowBits(i))
      s2_hitMask(i) := hit
      when(hit) {
        s2_biasUsedResp(i) := s2_biasResp(biasIdx)
        s2_totalPercsum(i) := s2_sumPercsum(tableIdx) +& s2_biasPercsum(biasIdx)
      }
  }

  private val s2_scPred: Vec[Bool] = VecInit(s2_totalPercsum.map(_ >= 0.S))
  private val s2_thresholds = scThreshold.map(entry => entry.thres.value >> 3)
  private val s2_useScPred  = WireInit(VecInit.fill(NumWays)(false.B))

  s2_useScPred.zip(s2_totalPercsum).zip(s2_thresholds).zip(s2_hitMask).zip(s2_providerCtr).zip(s2_providerValid).map {
    case (((((u, sum), thres), hit), ctr), providerValid) =>
      val tageConfHigh = ctr.isSaturatePositive || ctr.isSaturateNegative
      val tageConfMid  = ctr.isMid
      val tageConfLow  = ctr.isWeak
      when(hit && providerValid && tageConfHigh) {
        val conf = (sum > (thres >> 1).zext) && pos(sum) || (sum < -(thres >> 1).zext) && neg(sum)
        u := Mux(conf, true.B, false.B)
      }.elsewhen(hit && providerValid && tageConfMid) {
        val conf = (sum > (thres >> 2).zext) && pos(sum) || (sum < -(thres >> 2).zext) && neg(sum)
        u := Mux(conf, true.B, false.B)
      }.elsewhen(hit && providerValid && tageConfLow) {
        val conf = (sum > (thres >> 3).zext) && pos(sum) || (sum < -(thres >> 3).zext) && neg(sum)
        u := Mux(conf, true.B, false.B)
      }
  }
  private val s2_pred = s2_useScPred.zip(s2_condTakenMask).zip(s2_scPred).map { case ((use, tageTaken), scPred) =>
    Mux(use, scPred, tageTaken)
  }

  io.takenMask := s2_pred

  io.meta.scPathResp      := VecInit(s2_pathResp.map(v => VecInit(v.map(s => RegEnable(s.asUInt, s2_fire)))))
  io.meta.scGlobalResp    := VecInit(s2_globalResp.map(v => VecInit(v.map(s => RegEnable(s.asUInt, s2_fire)))))
  io.meta.scBiasLowerBits := RegEnable(s2_biasIdxLowBits, s2_fire)
  io.meta.scBiasResp      := VecInit(s2_biasUsedResp.map(v => RegEnable(v.asUInt, s2_fire)))
  io.meta.scPred          := RegEnable(s2_scPred, s2_fire)
  io.meta.scGhr           := RegEnable(s2_ghr.value.asUInt, s2_fire)
  io.meta.tagePred        := RegEnable(s2_condTakenMask, s2_fire)
  io.meta.tagePredValid   := RegEnable(s2_providerValid, s2_fire)
  io.meta.useScPred       := RegEnable(s2_useScPred, s2_fire)

  /*
   *  train pipeline stage 1
   */
  private val t1_trainValid = RegNext(io.train.fire, init = false.B)
  private val t1_train      = RegEnable(io.train.bits, io.train.fire)
  private val t1_branches   = t1_train.branches
  private val t1_meta       = t1_train.meta.sc

  private val t1_pathSetIdx = PathTableInfos.map(info =>
    getPathTableIdx(
      t1_train.startPc,
      new FoldedHistoryInfo(info.HistoryLength, min(info.HistoryLength, log2Ceil(info.Size / NumWays))),
      RegEnable(io.trainFoldedPathHist, io.train.valid),
      info.Size / NumWays
    )
  )

  private val t1_globalSetIdx = GlobalTableInfos.map(info =>
    getGlobalTableIdx(
      t1_train.startPc,
      t1_meta.scGhr,
      info.Size / NumWays
    )
  )

  private val t1_biasSetIdx: UInt = getBiasTableIdx(t1_train.startPc, BiasTableSize)

  private val t1_oldPathCtrs    = VecInit(t1_meta.scPathResp.map(v => VecInit(v.map(r => r.asTypeOf(new ScEntry())))))
  private val t1_oldGlobalCtrs  = VecInit(t1_meta.scGlobalResp.map(v => VecInit(v.map(r => r.asTypeOf(new ScEntry())))))
  private val t1_oldBiasCtrs    = VecInit(t1_meta.scBiasResp.map(v => v.asTypeOf(new ScEntry())))
  private val t1_oldBiasLowBits = t1_meta.scBiasLowerBits

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

  // calculate new thresholds
  private val t1_writeThresVec = VecInit(scThreshold.indices.map { wayIdx =>
    val updated = t1_branchesWayIdxVec.zip(t1_branchesTakenMask).foldLeft(scThreshold(wayIdx)) {
      case (prevThres, (branchWayIdx, taken)) =>
        val shouldUpdate =
          (branchWayIdx === wayIdx.U) && (t1_meta.scPred(wayIdx) =/= t1_meta.tagePred(wayIdx)) &&
            (t1_meta.scPred(wayIdx) =/= taken) && t1_meta.tagePredValid(wayIdx)
        val updateDir = taken =/= t1_meta.scPred(wayIdx)
        val nextThres = prevThres.update(updateDir)
        Mux(shouldUpdate, nextThres, prevThres)
    }
    WireInit(updated)
  })

  // calculate pathTable and globalTable write wayMask
  private val t1_writeWayMask = WireInit(VecInit.fill(NumWays)(false.B))
  t1_branchesWayIdxVec.zip(t1_writeValidVec).foreach {
    case (wayIdx, writeValid) =>
      when(writeValid) {
        t1_writeWayMask(wayIdx) := true.B
      }
  }

  // calculate new path table entries
  private val t1_writePathEntryVec = WireInit(
    VecInit.fill(PathTableSize)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  t1_oldPathCtrs.zip(t1_writePathEntryVec).foreach {
    case (oldEntries: Vec[ScEntry], writeEntries: Vec[ScEntry]) =>
      writeEntries := updateEntry(oldEntries, t1_writeValidVec, t1_branchesTakenMask, t1_branchesWayIdxVec, t1_meta)
  }

  // calculate new global table entries
  private val t1_writeGlobalEntryVec = WireInit(
    VecInit.fill(GlobalTableSize)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  t1_oldGlobalCtrs.zip(t1_writeGlobalEntryVec).foreach {
    case (oldEntries: Vec[ScEntry], writeEntries: Vec[ScEntry]) =>
      writeEntries := updateEntry(oldEntries, t1_writeValidVec, t1_branchesTakenMask, t1_branchesWayIdxVec, t1_meta)
  }

  // calculate bias table new entries and wayMask
  private val t1_writeBiasEntryVec = WireInit(VecInit.fill(BiasTableNumWays)(0.U.asTypeOf(new ScEntry())))
  private val t1_writeBiasWayMask  = WireInit(VecInit.fill(BiasTableNumWays)(false.B))
  t1_branchesWayIdxVec.zip(t1_writeValidVec).foreach {
    case (wayIdx, writeValid) =>
      when(writeValid) {
        val lowBits    = t1_oldBiasLowBits(wayIdx)
        val biasWayIdx = Cat(wayIdx, lowBits)
        t1_writeBiasWayMask(biasWayIdx) := true.B
      }
  }

  // prediction used entries update
  private val t1_writeBiasUsedEntryVec =
    updateEntry(t1_oldBiasCtrs, t1_writeValidVec, t1_branchesTakenMask, t1_branchesWayIdxVec, t1_meta)

  t1_writeBiasUsedEntryVec.zipWithIndex.foreach { case (entry, wayIdx) =>
    val lowBits    = t1_oldBiasLowBits(wayIdx)
    val biasWayIdx = Cat(wayIdx.U, lowBits)
    t1_writeBiasEntryVec(biasWayIdx) := entry
  }

  // new entries write back to tables
  pathTable zip t1_pathSetIdx zip t1_writePathEntryVec foreach {
    case ((table, idx), writeEntries) =>
      table.io.update.valid    := t1_writeValid && ctrl.pathEnable
      table.io.update.setIdx   := idx
      table.io.update.wayMask  := t1_writeWayMask
      table.io.update.entryVec := writeEntries
  }

  globalTable zip t1_globalSetIdx zip t1_writeGlobalEntryVec foreach {
    case ((table, idx), writeEntries) =>
      table.io.update.valid    := t1_writeValid && ctrl.globalEnable
      table.io.update.setIdx   := idx
      table.io.update.wayMask  := t1_writeWayMask
      table.io.update.entryVec := writeEntries
  }

  biasTable.io.update.valid    := t1_writeValid && ctrl.biasEnable
  biasTable.io.update.setIdx   := t1_biasSetIdx
  biasTable.io.update.wayMask  := t1_writeBiasWayMask
  biasTable.io.update.entryVec := t1_writeBiasEntryVec

  when(t1_writeValid) {
    scThreshold := t1_writeThresVec
  }
  if (EnableCommitGHistDiff) {
    val scCorrectVec = WireInit(VecInit.fill(NumWays)(false.B))
    val scWrongVec   = WireInit(VecInit.fill(NumWays)(false.B))
    val useTageVec   = WireInit(VecInit.fill(NumWays)(false.B))
    // val tagePredValid = t1_meta.tagePredValid

    t1_branchesWayIdxVec.zip(t1_branchesTakenMask).zip(t1_writeValidVec).zip(t1_branches).foreach {
      case (((wayIdx, taken), writeValid), branch) =>
        val scCorrect = taken === t1_meta.scPred(wayIdx) && t1_meta.useScPred(wayIdx)
        val scWrong   = taken =/= t1_meta.scPred(wayIdx) && t1_meta.useScPred(wayIdx)
        val useTage   = branch.bits.mispredict && branch.bits.attribute.isConditional && !t1_meta.useScPred(wayIdx)
        when(writeValid && scCorrect) {
          scCorrectVec(wayIdx) := true.B
        }
        when(writeValid && scWrong) {
          scWrongVec(wayIdx) := true.B
        }
        when(writeValid && useTage) {
          useTageVec(wayIdx) := true.B
        }
    }
    for (i <- 0 until NumWays) {
      XSPerfAccumulate(s"sc_correct${i}", t1_writeValid && scCorrectVec(i))
      XSPerfAccumulate(s"sc_wrong${i}", t1_writeValid && scWrongVec(i))
      XSPerfAccumulate(s"use_tage_wrong${i}", t1_writeValid && useTageVec(i))
      XSPerfAccumulate(s"use_sc${i}", t1_writeValid && t1_meta.useScPred(i))
    }
    dontTouch(scCorrectVec)
    dontTouch(scWrongVec)
    dontTouch(useTageVec)
    XSPerfAccumulate(s"sc_train", t1_writeValid)
  }
  dontTouch(s2_totalPercsum)
  dontTouch(s2_hitMask)
  dontTouch(s2_scPred)
  dontTouch(s2_useScPred)
  dontTouch(t1_writeThresVec)

  XSPerfAccumulate("sc_global_table_invalid", s0_fire && !s0_ghr.valid)
  XSPerfAccumulate("sc_global_table_valid", s0_fire && s0_ghr.valid)
}

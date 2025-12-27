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
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.history.ghr.GhrEntry
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories
import xiangshan.frontend.bpu.tage.{TakenCounter => TageTakenCounter}

/**
 * This module is the implementation of the Statistical Corrector.
 */
class Sc(implicit p: Parameters) extends BasePredictor with HasScParameters with Helpers {

  class ScIO(implicit p: Parameters) extends BasePredictorIO with HasScParameters {
    val mbtbResult: Vec[Valid[Prediction]] = Input(Vec(NumBtbResultEntries, Valid(new Prediction)))
    val providerTakenCtrs: Vec[Valid[SaturateCounter]] =
      Input(Vec(NumBtbResultEntries, Valid(TageTakenCounter()))) // s2 stage tage info
    val foldedPathHist:      PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val s3_override:         Bool                  = Input(Bool())
    val ghr:                 GhrEntry              = Input(new GhrEntry())
    val trainFoldedPathHist: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val scTakenMask:         Vec[Bool]             = Output(Vec(NumBtbResultEntries, Bool()))
    val scUsed:              Vec[Bool]             = Output(Vec(NumBtbResultEntries, Bool()))
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

  private val scThreshold = RegInit(ThresholdCounter.Init)

  private val resetDone = RegInit(false.B)
  when(pathTable.map(_.io.resetDone).reduce(_ && _) &&
    globalTable.map(_.io.resetDone).reduce(_ && _) &&
    biasTable.io.resetDone) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  io.trainReady := true.B

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
  private val stage2Ghr   = RegInit(0.U.asTypeOf(io.ghr))
  private val stage3Ghr   = RegInit(0.U.asTypeOf(io.ghr))
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
  private val s0_startPc  = io.startPc
  private val s0_bankMask = getBankMask(s0_startPc)
  private val s0_pathIdx = PathTableInfos.map(info =>
    getPathTableIdx(
      s0_startPc,
      new FoldedHistoryInfo(info.HistoryLength, min(info.HistoryLength, log2Ceil(info.Size / NumWays / NumBanks))),
      io.foldedPathHist,
      info.Size / NumWays / NumBanks
    )
  )

  private val s1_pathIdx = s0_pathIdx.map(RegEnable(_, s0_fire)) // for debug
  private val s2_pathIdx = s1_pathIdx.map(RegEnable(_, s1_fire)) // for debug

  private val s0_globalIdx = GlobalTableInfos.map(info =>
    getGlobalTableIdx(
      s0_startPc,
      s0_ghr.value.asUInt(info.HistoryLength - 1, 0),
      info.Size / NumWays / NumBanks,
      info.HistoryLength
    )
  )

  private val s1_globalIdx = s0_globalIdx.map(RegEnable(_, s0_fire)) // for debug
  private val s2_globalIdx = s1_globalIdx.map(RegEnable(_, s1_fire)) // for debug

  private val s0_biasIdx = getBiasTableIdx(s0_startPc, BiasTableSize / BiasTableNumWays / NumBanks)

  private val s1_biasIdx = RegEnable(s0_biasIdx, s0_fire)
  private val s2_biasIdx = RegEnable(s1_biasIdx, s1_fire)

  pathTable.zip(s0_pathIdx).foreach { case (table, idx) =>
    table.io.req.valid         := s0_fire && PathEnable.B
    table.io.req.bits.setIdx   := idx
    table.io.req.bits.bankMask := s0_bankMask
  }

  globalTable.zip(s0_globalIdx).foreach { case (table, idx) =>
    table.io.req.valid         := s0_fire && s0_ghr.valid && GlobalEnable.B // if ghr invalid not request global table
    table.io.req.bits.setIdx   := idx
    table.io.req.bits.bankMask := s0_bankMask
  }

  biasTable.io.req.valid         := s0_fire && BiasEnable.B
  biasTable.io.req.bits.setIdx   := s0_biasIdx
  biasTable.io.req.bits.bankMask := s0_bankMask

  /*
   *  predict pipeline stage 1
   *  calculate each ctr's percsum
   */
  private val s1_startPc = RegEnable(io.startPc, s0_fire)
  private val s1_pathResp = Mux(
    PathEnable.B,
    VecInit(pathTable.map(_.io.resp)),
    VecInit.fill(NumPathTables)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  // if s0_ghr invalid, global table resp is also invalid
  private val s1_globalResp =
    Mux(
      s1_ghr.valid && GlobalEnable.B,
      VecInit(globalTable.map(_.io.resp)),
      VecInit.fill(NumGlobalTables)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
    )
  private val s1_biasResp = Mux(
    BiasEnable.B,
    biasTable.io.resp,
    VecInit.fill(BiasTableNumWays)(0.U.asTypeOf(new ScEntry()))
  )
  private val s1_mergeResp = VecInit(s1_pathResp ++ s1_globalResp)

  private val s1_biasPercsum = VecInit(s1_biasResp.map(entry => getPercsum(entry.ctr.value)))
  private val s1_mergePercsum = VecInit(s1_mergeResp.map(entries =>
    VecInit(entries.map(entry => getPercsum(entry.ctr.value)))
  ))
  require(
    s1_mergePercsum.length == PathTableInfos.length + GlobalTableInfos.length,
    s"s1_mergePercsum length ${s1_mergePercsum.length} != " +
      s"PathTableInfos.length + GlobalTableInfos.length ${PathTableInfos.length + GlobalTableInfos.length}"
  )
  // Calculate sumPercsum without bias in advance
  private val s1_sumPercsum = VecInit.tabulate(NumWays)(j => s1_mergePercsum.map(_(j)).reduce(_ +& _))
  require(
    s1_sumPercsum.length == NumWays,
    s"s1_sumPercsum length ${s1_sumPercsum.length} != NumWays $NumWays"
  )

  /*
   *  predict pipeline stage 2
   *  match entries and calculate final percSum
   */
  private val s2_startPc     = RegEnable(s1_startPc, s1_fire)
  private val s2_pathResp    = s1_pathResp.map(entries => VecInit(entries.map(RegEnable(_, s1_fire))))
  private val s2_globalResp  = s1_globalResp.map(entries => VecInit(entries.map(RegEnable(_, s1_fire))))
  private val s2_biasResp    = VecInit(s1_biasResp.map(RegEnable(_, s1_fire)))
  private val s2_biasPercsum = VecInit(s1_biasPercsum.map(RegEnable(_, s1_fire)))
  private val s2_sumPercsum  = VecInit(s1_sumPercsum.map(RegEnable(_, s1_fire)))

  private val s2_mbtbResult        = io.mbtbResult
  private val s2_providerTakenMask = VecInit(io.providerTakenCtrs.map(_.bits.isPositive))
  private val s2_providerValid     = VecInit(io.providerTakenCtrs.map(_.valid))
  private val s2_providerCtr       = VecInit(io.providerTakenCtrs.map(_.bits))

  private val s2_biasIdxLowBits = VecInit(s2_providerTakenMask.zip(s2_providerValid).zip(s2_providerCtr).map {
    case ((taken, valid), ctr) => Cat(valid && ctr.isWeak, taken)
  })
  private val s2_totalPercsum = WireInit(VecInit.fill(NumWays)(0.S(TotalSumWidth.W)))
  private val s2_hitMask      = WireInit(VecInit.fill(NumWays)(false.B))
  require(NumWays == s2_mbtbResult.length, s"NumWays $NumWays != s2_mbtbHitMask.length ${s2_mbtbResult.length}")

  s2_mbtbResult.zip(s2_providerTakenMask).zipWithIndex.map {
    case ((mbtbResult, taken), i) =>
      val hit     = mbtbResult.valid && mbtbResult.bits.attribute.isConditional
      val wayIdx  = getWayIdx(mbtbResult.bits.cfiPosition)
      val biasIdx = Cat(wayIdx, s2_biasIdxLowBits(i))
      s2_hitMask(i) := hit
      when(hit) {
        s2_totalPercsum(i) := s2_sumPercsum(wayIdx) +& s2_biasPercsum(biasIdx)
      }
  }

  private val s2_scPred        = VecInit(s2_totalPercsum.map(_ >= 0.S))
  private val s2_thresholds    = scThreshold.value >> 3
  private val s2_useScPred     = WireInit(VecInit.fill(NumWays)(false.B))
  private val s2_sumAboveThres = WireInit(VecInit.fill(NumWays)(false.B))

  for (i <- 0 until NumWays) {
    val hit          = s2_hitMask(i)
    val valid        = s2_providerValid(i)
    val sum          = s2_totalPercsum(i)
    val thres        = s2_thresholds
    val tageConfHigh = (s2_providerCtr(i).isSaturatePositive || s2_providerCtr(i).isSaturateNegative) && valid
    val tageConfMid  = s2_providerCtr(i).isMid && valid
    val tageConfLow  = s2_providerCtr(i).isWeak && valid
    val conf         = WireInit(false.B)
    when(hit && valid && tageConfHigh) {
      conf            := aboveThreshold(sum, thres >> 1)
      s2_useScPred(i) := Mux(conf, true.B, false.B)
    }.elsewhen(hit && valid && tageConfMid) {
      conf            := aboveThreshold(sum, thres >> 2)
      s2_useScPred(i) := Mux(conf, true.B, false.B)
    }.elsewhen(hit && valid && tageConfLow) {
      conf            := aboveThreshold(sum, thres >> 3)
      s2_useScPred(i) := Mux(conf, true.B, false.B)
    }.otherwise {
      conf            := false.B
      s2_useScPred(i) := false.B
    }
    s2_sumAboveThres(i) := aboveThreshold(sum, thres)
    dontTouch(tageConfHigh)
    dontTouch(tageConfMid)
    dontTouch(tageConfLow)
    dontTouch(conf)
  }

  io.scTakenMask := s2_scPred
  io.scUsed      := s2_useScPred

  io.meta.scPathResp      := VecInit(s2_pathResp.map(v => VecInit(v.map(s => RegEnable(s.asUInt, s2_fire)))))
  io.meta.scGlobalResp    := VecInit(s2_globalResp.map(v => VecInit(v.map(s => RegEnable(s.asUInt, s2_fire)))))
  io.meta.scBiasLowerBits := RegEnable(s2_biasIdxLowBits, s2_fire)
  io.meta.scBiasResp      := VecInit(s2_biasResp.map(v => RegEnable(v.asUInt, s2_fire)))
  io.meta.scPred          := RegEnable(s2_scPred, s2_fire)
  io.meta.scGhr           := RegEnable(s2_ghr.value.asUInt, s2_fire)
  io.meta.tagePred        := RegEnable(s2_providerTakenMask, s2_fire)
  io.meta.tagePredValid   := RegEnable(s2_providerValid, s2_fire)
  io.meta.tagePredCtr     := VecInit(s2_providerCtr.map(ctr => RegEnable(ctr.value, s2_fire)))
  io.meta.useScPred       := RegEnable(s2_useScPred, s2_fire)
  io.meta.sumAboveThres   := RegEnable(s2_sumAboveThres, s2_fire)
  io.meta.predPathIdx     := RegEnable(VecInit(s2_pathIdx), s2_fire)
  io.meta.predGlobalIdx   := RegEnable(VecInit(s2_globalIdx), s2_fire)
  io.meta.predBiasIdx     := RegEnable(s2_biasIdx, s2_fire)

  /*
   *  train pipeline stage 0
   */
  private val t0_fire = io.stageCtrl.t0_fire

  /*
   *  train pipeline stage 1
   */
  private val t1_fire     = RegNext(t0_fire, false.B)
  private val t1_train    = RegEnable(io.train, t0_fire)
  private val t1_branches = t1_train.branches
  private val t1_meta     = t1_train.meta.sc

  private val t1_bankMask = getBankMask(t1_train.startPc)

  private val t1_pathSetIdx = PathTableInfos.map(info =>
    getPathTableIdx(
      t1_train.startPc,
      new FoldedHistoryInfo(info.HistoryLength, min(info.HistoryLength, log2Ceil(info.Size / NumWays / NumBanks))),
      RegEnable(io.trainFoldedPathHist, t0_fire),
      info.Size / NumWays / NumBanks
    )
  )
  private val t1_globalSetIdx = GlobalTableInfos.map(info =>
    getGlobalTableIdx(
      t1_train.startPc,
      t1_meta.scGhr(info.HistoryLength - 1, 0),
      info.Size / NumWays / NumBanks,
      info.HistoryLength
    )
  )

  private val t1_biasSetIdx = getBiasTableIdx(t1_train.startPc, BiasTableSize / BiasTableNumWays / NumBanks)

  private val t1_oldPathCtrs    = VecInit(t1_meta.scPathResp.map(v => VecInit(v.map(r => r.asTypeOf(new ScEntry())))))
  private val t1_oldGlobalCtrs  = VecInit(t1_meta.scGlobalResp.map(v => VecInit(v.map(r => r.asTypeOf(new ScEntry())))))
  private val t1_oldBiasCtrs    = VecInit(t1_meta.scBiasResp.map(v => v.asTypeOf(new ScEntry())))
  private val t1_oldBiasLowBits = t1_meta.scBiasLowerBits
  private val t1_mbtbPosition   = VecInit(t1_train.meta.mbtb.entries.flatten.map(e => e.position))

  private val t1_branchesWayIdxVec = VecInit(t1_branches.map(b => getWayIdx(b.bits.cfiPosition)))
  private val t1_branchesScIdxVec  = WireInit(VecInit.fill(ResolveEntryBranchNumber)(0.U(log2Ceil(NumWays).W)))
  private val t1_branchesScIdxHitVec =
    WireInit(VecInit.fill(ResolveEntryBranchNumber)(false.B)) // if the branch cfi not in mbtbResult, do not train
  private val t1_writeValidVec =
    VecInit(t1_branches.zip(t1_branchesScIdxHitVec).map { case (b, hit) =>
      b.valid && b.bits.attribute.isConditional && t1_fire && hit
    })
  private val t1_writeValid = t1_writeValidVec.reduce(_ || _)
  private val t1_writeTakenVec =
    VecInit(t1_branches.map(b => b.valid && b.bits.taken && b.bits.attribute.isConditional))

  // During training, find the predicted scPred and lowBits values in the order of the predicted mbtbResult
  t1_branches.zipWithIndex.foreach { case (branch, branchIdx) =>
    for (i <- 0 until NumWays) {
      when(t1_mbtbPosition(i) === branch.bits.cfiPosition) {
        t1_branchesScIdxHitVec(branchIdx) := true.B
        t1_branchesScIdxVec(branchIdx)    := i.U
      }
    }
  }

  require(
    t1_branchesWayIdxVec(0).getWidth == log2Ceil(NumWays),
    s"t1_branchesWayIdxVec entry width: ${t1_branchesWayIdxVec(0).getWidth} " +
      s"should be the same as log2Ceil(NumWays): ${log2Ceil(NumWays)}"
  )

  // calculate new thresholds
  private val t1_writeThresVec =
    t1_writeValidVec.zip(t1_branchesWayIdxVec).zip(t1_writeTakenVec).zip(t1_branchesScIdxVec).foldLeft(scThreshold) {
      case (prevThres, (((writeValid, writeWayIdx), taken), branchIdx)) =>
        val scWrong      = taken =/= t1_meta.scPred(branchIdx)
        val shouldUpdate = writeValid && (scWrong || !t1_meta.sumAboveThres(branchIdx)) && false.B
        prevThres.getUpdate(scWrong, shouldUpdate)
    }

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
    VecInit.fill(NumPathTables)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  t1_oldPathCtrs.zip(t1_writePathEntryVec).foreach {
    case (oldEntries: Vec[ScEntry], writeEntries: Vec[ScEntry]) =>
      writeEntries := updateEntry(oldEntries, t1_writeValidVec, t1_writeTakenVec, t1_branchesWayIdxVec, t1_meta)
  }

  // calculate new global table entries
  private val t1_writeGlobalEntryVec = WireInit(
    VecInit.fill(NumGlobalTables)(VecInit.fill(NumWays)(0.U.asTypeOf(new ScEntry())))
  )
  t1_oldGlobalCtrs.zip(t1_writeGlobalEntryVec).foreach {
    case (oldEntries: Vec[ScEntry], writeEntries: Vec[ScEntry]) =>
      writeEntries := updateEntry(oldEntries, t1_writeValidVec, t1_writeTakenVec, t1_branchesWayIdxVec, t1_meta)
  }

  // calculate bias table new entries and wayMask
  private val t1_writeBiasEntryVec = WireInit(VecInit.fill(BiasTableNumWays)(0.U.asTypeOf(new ScEntry())))
  private val t1_writeBiasWayMask  = WireInit(VecInit.fill(BiasTableNumWays)(false.B))
  t1_branchesWayIdxVec.zip(t1_writeValidVec).zip(t1_branchesScIdxVec).foreach {
    case ((wayIdx, writeValid), branchIdx) =>
      val biasWayIdx = Cat(wayIdx, t1_oldBiasLowBits(branchIdx))
      when(writeValid && t1_oldBiasCtrs(biasWayIdx).ctr =/= t1_writeBiasEntryVec(biasWayIdx).ctr) {
        t1_writeBiasWayMask(biasWayIdx) := true.B
      }
  }

  t1_oldBiasCtrs.zip(t1_writeBiasEntryVec).zipWithIndex.foreach { case ((oldEntry, newEntry), wayIdx) =>
    val newCtr = t1_writeValidVec.zip(t1_writeTakenVec).zip(t1_branchesWayIdxVec).zip(
      t1_branchesScIdxVec
    ).foldLeft(oldEntry.ctr) {
      case (prevCtr, (((writeValid, writeTaken), writeWayIdx), branchIdx)) =>
        val biasWayIdx = Cat(writeWayIdx, t1_oldBiasLowBits(branchIdx))
        val needUpdate = writeValid && biasWayIdx === wayIdx.U && (
          t1_meta.scPred(branchIdx) =/= writeTaken ||
            !t1_meta.sumAboveThres(branchIdx)
        )
        prevCtr.getUpdate(
          writeTaken,
          needUpdate
        )
    }
    newEntry.ctr := WireInit(newCtr)
  }

  // new entries write back to tables
  pathTable.zip(t1_pathSetIdx).zip(t1_writePathEntryVec).foreach {
    case ((table, idx), writeEntries) =>
      table.io.update.valid    := t1_writeValid && PathEnable.B
      table.io.update.setIdx   := idx
      table.io.update.bankMask := t1_bankMask
      table.io.update.wayMask  := t1_writeWayMask
      table.io.update.entryVec := writeEntries
  }

  globalTable.zip(t1_globalSetIdx).zip(t1_writeGlobalEntryVec).foreach {
    case ((table, idx), writeEntries) =>
      table.io.update.valid    := t1_writeValid && GlobalEnable.B
      table.io.update.setIdx   := idx
      table.io.update.bankMask := t1_bankMask
      table.io.update.wayMask  := t1_writeWayMask
      table.io.update.entryVec := writeEntries
  }

  biasTable.io.update.valid    := t1_writeValid && BiasEnable.B
  biasTable.io.update.setIdx   := t1_biasSetIdx
  biasTable.io.update.bankMask := t1_bankMask
  biasTable.io.update.wayMask  := t1_writeBiasWayMask
  biasTable.io.update.entryVec := t1_writeBiasEntryVec

  when(t1_writeValid) {
    scThreshold := t1_writeThresVec
  }

  private val scCorrectVec   = WireInit(VecInit.fill(NumWays)(false.B))
  private val scWrongVec     = WireInit(VecInit.fill(NumWays)(false.B))
  private val tageCorrectVec = WireInit(VecInit.fill(NumWays)(false.B))
  private val tageWrongVec   = WireInit(VecInit.fill(NumWays)(false.B))
  private val newBranchVec   = WireInit(VecInit.fill(NumWays)(false.B))
  private val scUsedVec      = WireInit(VecInit.fill(NumWays)(false.B))
  private val scNotUsedVec   = WireInit(VecInit.fill(NumWays)(false.B))
  private val changeVec      = VecInit.fill(NumWays)(false.B)
  // foreach train branches
  for (i <- 0 until ResolveEntryBranchNumber) {
    val branchWayIdx = t1_branchesScIdxVec(i)
    when(t1_meta.useScPred(branchWayIdx) && t1_writeValidVec(i)) {
      tageCorrectVec(branchWayIdx) := t1_writeTakenVec(i) === t1_meta.tagePred(branchWayIdx)
      tageWrongVec(branchWayIdx)   := t1_writeTakenVec(i) =/= t1_meta.tagePred(branchWayIdx)
      scCorrectVec(branchWayIdx)   := t1_writeTakenVec(i) === t1_meta.scPred(branchWayIdx)
      scWrongVec(branchWayIdx)     := t1_writeTakenVec(i) =/= t1_meta.scPred(branchWayIdx)
      scUsedVec(branchWayIdx)      := true.B
    }.otherwise {
      scNotUsedVec(branchWayIdx) := !t1_meta.useScPred(branchWayIdx) && t1_writeValidVec(i)
    }
  }
  // foreach write way
  for (i <- 0 until NumWays) {
    val pChange = t1_oldPathCtrs.zip(t1_writePathEntryVec).map {
      case (oldEntries, writeEntries) =>
        (oldEntries(i).ctr =/= writeEntries(i).ctr) && t1_writeWayMask(i)
    }.reduce(_ || _) && PathEnable.B
    val gChange = t1_oldGlobalCtrs.zip(t1_writeGlobalEntryVec).map {
      case (oldEntries, writeEntries) =>
        (oldEntries(i).ctr =/= writeEntries(i).ctr) && t1_writeWayMask(i)
    }.reduce(_ || _) && GlobalEnable.B
    // val bChange =
    //   (t1_oldBiasCtrs(i).ctr.value =/= t1_writeBiasEntryVec(i).ctr.value) && t1_writeBiasWayMask(i) && BiasEnable.B
    changeVec(i) := (pChange || gChange) && t1_writeWayMask(i)

    XSPerfAccumulate(s"sc_correct_tage_wrong${i}", scCorrectVec(i) && tageWrongVec(i))
    XSPerfAccumulate(s"sc_wrong_tage_correct${i}", scWrongVec(i) && tageCorrectVec(i))
    XSPerfAccumulate(s"sc_correct_tage_correct${i}", scCorrectVec(i) && tageCorrectVec(i))
    XSPerfAccumulate(s"sc_wrong_tage_wrong${i}", scWrongVec(i) && tageWrongVec(i))
    XSPerfAccumulate(s"t1_use_sc${i}", scUsedVec(i))
    XSPerfAccumulate(s"t1_not_use_sc${i}", scNotUsedVec(i))

    XSPerfAccumulate(s"s2_sumAbove10_${i}", s2_fire && s2_sumPercsum(i).abs.asUInt >= 15.U)
    XSPerfAccumulate(s"path_table_change${i}", t1_writeValid && pChange)
    XSPerfAccumulate(s"global_table_change${i}", t1_writeValid && gChange)
    // XSPerfAccumulate(s"bias_table_change${i}", t1_writeValid && bChange)
    XSPerfAccumulate(s"sc_train${i}", t1_writeValid && changeVec(i))
  }

  XSPerfAccumulate(s"total_sc_train", t1_writeValid && changeVec.reduce(_ || _))
  XSPerfAccumulate(s"total_pred_use_sc", t1_writeValid && t1_meta.useScPred.reduce(_ || _))

  dontTouch(s2_sumPercsum)
  dontTouch(s2_totalPercsum)
  dontTouch(s2_hitMask)
  dontTouch(s2_scPred)
  dontTouch(s2_useScPred)
  dontTouch(t1_branchesWayIdxVec)
  dontTouch(t1_writeThresVec)
  dontTouch(t1_meta)
  dontTouch(t1_mbtbPosition)
  dontTouch(scCorrectVec)
  dontTouch(scWrongVec)

  private val sc_path_predIdx_diff_trainIdx = t1_writeValid && (t1_meta.predPathIdx.zip(t1_pathSetIdx).map {
    case (predIdx, trainIdx) => predIdx =/= trainIdx
  }.reduce(_ || _))
  private val sc_global_predIdx_diff_trainIdx = t1_writeValid && (t1_meta.predGlobalIdx.zip(t1_globalSetIdx).map {
    case (predIdx, trainIdx) => predIdx =/= trainIdx
  }.reduce(_ || _))
  private val sc_bias_predIdx_diff_trainIdx = t1_writeValid && (t1_meta.predBiasIdx =/= t1_biasSetIdx)

  dontTouch(sc_path_predIdx_diff_trainIdx)
  dontTouch(sc_global_predIdx_diff_trainIdx)
  dontTouch(sc_bias_predIdx_diff_trainIdx)

  XSPerfAccumulate("sc_global_table_invalid", s0_fire && !s0_ghr.valid)
  XSPerfAccumulate("sc_global_table_valid", s0_fire && s0_ghr.valid)
  XSPerfAccumulate("sc_path_predIdx_diff_trainIdx", sc_path_predIdx_diff_trainIdx)
  XSPerfAccumulate("sc_global_predIdx_diff_trainIdx", sc_global_predIdx_diff_trainIdx)
  XSPerfAccumulate("sc_bias_predIdx_diff_trainIdx", sc_bias_predIdx_diff_trainIdx)

}

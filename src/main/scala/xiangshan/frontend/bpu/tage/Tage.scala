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

package xiangshan.frontend.bpu.tage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import scala.math.min
import utility.XSPerfAccumulate
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.mbtb.MainBtbResult
import xiangshan.frontend.bpu.phr.PhrAllFoldedHistories

/**
 * This module is the implementation of the TAGE (TAgged GEometric history length predictor).
 */
class Tage(implicit p: Parameters) extends BasePredictor with HasTageParameters with Helpers {
  class TageIO(implicit p: Parameters) extends BasePredictorIO {
    val mbtbResult:             MainBtbResult         = Input(new MainBtbResult)
    val foldedPathHist:         PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val foldedPathHistForTrain: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val takenMask:              Vec[Bool]             = Output(Vec(MainBtbResultNumEntries, Bool()))
    val meta:                   TageMeta              = Output(new TageMeta)
  }
  val io: TageIO = IO(new TageIO)

  /* *** submodules *** */
  private val baseTable = Module(new TageBaseTable)
  private val tables    = TableInfos.map(tableInfo => Module(new TageTable(tableInfo.NumSets, tableInfo.NumWays)))

  /* *** history information *** */
  private val histInfoForIdx = TableInfos.map { tableInfo =>
    new FoldedHistoryInfo(tableInfo.HistoryLength, min(tableInfo.HistoryLength, log2Ceil(tableInfo.NumSets / NumBanks)))
  }
  private val histInfoForTag = TableInfos.map { tableInfo =>
    new FoldedHistoryInfo(tableInfo.HistoryLength, min(tableInfo.HistoryLength, TagWidth))
  }
  private val anotherHistInfoForTag = TableInfos.map { tableInfo =>
    new FoldedHistoryInfo(tableInfo.HistoryLength, min(tableInfo.HistoryLength, TagWidth - 1))
  }

  /* *** reset *** */
  private val resetDone = RegInit(false.B)
  when(baseTable.io.resetDone && tables.map(_.io.resetDone).reduce(_ && _)) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 0
     - send read request to tables
     -------------------------------------------------------------------------------------------------------------- */

  private val s0_fire    = io.stageCtrl.s0_fire && io.enable
  private val s0_startPc = io.startVAddr

  baseTable.io.readReqValid := s0_fire
  baseTable.io.startPc      := s0_startPc

  private val s0_foldedHistForIdx  = histInfoForIdx.map(io.foldedPathHist.getHistWithInfo(_).foldedHist)
  private val s0_foldedHistForTag1 = histInfoForTag.map(io.foldedPathHist.getHistWithInfo(_).foldedHist)
  private val s0_foldedHistForTag2 = anotherHistInfoForTag.map(io.foldedPathHist.getHistWithInfo(_).foldedHist)

  tables.zipWithIndex.foreach {
    case (table, tableIdx) =>
      table.io.readReqValid := s0_fire
      table.io.readSetIdx   := getSetIndex(s0_startPc, s0_foldedHistForIdx(tableIdx), table.NumSets)
      table.io.readBankMask := getBankMask(s0_startPc)
  }

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 1
     - get result from tables
     - compute temp tag
     -------------------------------------------------------------------------------------------------------------- */

  private val s1_fire    = io.stageCtrl.s1_fire && io.enable
  private val s1_startPc = RegEnable(s0_startPc, s0_fire)

  private val s1_baseTableCtrs = baseTable.io.ctrs
  private val s1_tableResp     = tables.map(_.io.readEntries)

  private val s1_foldedHistForTag1 = s0_foldedHistForTag1.map(RegEnable(_, s0_fire))
  private val s1_foldedHistForTag2 = s0_foldedHistForTag2.map(RegEnable(_, s0_fire))

  private val s1_tempTag = (0 until NumTables).map { tableIdx =>
    getTag(s1_startPc, s1_foldedHistForTag1(tableIdx), s1_foldedHistForTag2(tableIdx))
  }

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 2
     - get results from mbtb
     - select provider for each btb entry
     - get takenMask
     -------------------------------------------------------------------------------------------------------------- */

  private val s2_fire = io.stageCtrl.s2_fire && io.enable

  private val s2_baseTableCtrs = RegEnable(s1_baseTableCtrs, s1_fire)
  private val s2_tableResp     = s1_tableResp.map(entries => RegEnable(entries, s1_fire))

  private val s2_tempTag = s1_tempTag.map(tag => RegEnable(tag, s1_fire))

  private val s2_mbtbHitMask    = io.mbtbResult.hitMask
  private val s2_mbtbPositions  = io.mbtbResult.positions
  private val s2_mbtbAttributes = io.mbtbResult.attributes

  private val s2_tablePredictions = s2_mbtbHitMask.zip(s2_mbtbAttributes).zip(s2_mbtbPositions).map {
    case ((mbtbHit, attribute), position) => // for each btb entry
      val allTableResults = s2_tableResp.zip(TableInfos).zipWithIndex.map {
        case ((tageEntries, tableInfo), tableIdx) => // for each table
          val tag        = s2_tempTag(tableIdx) ^ position // use position to distinguish different branches
          val hitWayMask = PriorityEncoderOH(tageEntries.map(entry => entry.valid && entry.tag === tag))

          val tableResult = Wire(new TageTableResult(tableInfo.NumWays))
          tableResult.hit              := mbtbHit && attribute.isConditional && hitWayMask.reduce(_ || _)
          tableResult.hitWayMask       := hitWayMask
          tableResult.takenCtr         := Mux1H(hitWayMask, tageEntries.map(_.takenCtr))
          tableResult.usefulCtr        := Mux1H(hitWayMask, tageEntries.map(_.usefulCtr))
          tableResult.notUsefulWayMask := tageEntries.map(_.usefulCtr.value === 0.U) // used for new entry allocation
          tableResult
      }

      val hitTableMask        = allTableResults.map(_.hit)
      val ctrNotWeakTableMask = allTableResults.map(tableResult => tableResult.hit && tableResult.takenCtr.isNotWeak)

      val hitWayMaskVec = VecInit(allTableResults.map(_.hitWayMask))
      val takenCtrVec   = VecInit(allTableResults.map(_.takenCtr))
      val usefulCtrVec  = VecInit(allTableResults.map(_.usefulCtr))

      // provider: the table with the longest history
      val providerIdx = PriorityEncoder(hitTableMask.reverse)
      // hcProvider: high confidence provider, the table with the longest history and ctr not weak
      // if provider's ctr is not weak then hcProvider is identical to provider
      // if provider's ctr is weak then hcProvider is distinct
      val hcProviderIdx = PriorityEncoder(ctrNotWeakTableMask.reverse)

      val tablePrediction = Wire(new TageTablePrediction)
      tablePrediction.hasProvider              := hitTableMask.reduce(_ || _)
      tablePrediction.providerTableIdx         := providerIdx
      tablePrediction.providerWayMask          := hitWayMaskVec(providerIdx)
      tablePrediction.providerTakenCtr         := takenCtrVec(providerIdx)
      tablePrediction.providerUsefulCtr        := usefulCtrVec(providerIdx)
      tablePrediction.hasHcProvider            := ctrNotWeakTableMask.reduce(_ || _)
      tablePrediction.hcProviderIdx            := hcProviderIdx
      tablePrediction.hcProviderTakenCtr       := takenCtrVec(hcProviderIdx)
      tablePrediction.allTableNotUsefulWayMask := allTableResults.map(_.notUsefulWayMask)
      tablePrediction
  }

  private val s2_takenMask = s2_mbtbHitMask.zip(s2_mbtbAttributes).zip(s2_mbtbPositions).zip(s2_tablePredictions).map {
    case (((hit, attribute), position), tablePrediction) =>
      Mux(
        hit, // is it a branch?
        Mux(
          attribute.isConditional, // is it a conditional branch?
          Mux(
            tablePrediction.hasProvider,                 // has provider?
            tablePrediction.providerTakenCtr.isPositive, // use provider's prediction
            s2_baseTableCtrs(position).isPositive        // use base table's prediction
          ),
          Mux(attribute.isDirect || attribute.isIndirect, true.B, false.B) // unconditional branch is always taken
        ),
        false.B // not a branch, not taken
      )
  }

  // FIXME: only used for debug
  private val s2_baseTableTakenMask = s2_mbtbHitMask.zip(s2_mbtbAttributes).zip(s2_mbtbPositions).map {
    case ((hit, attribute), position) =>
      Mux(
        hit,
        Mux(
          attribute.isConditional,
          s2_baseTableCtrs(position).isPositive,
          Mux(attribute.isDirect || attribute.isIndirect, true.B, false.B)
        ),
        false.B
      )
  }

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 3
     - output prediction
     -------------------------------------------------------------------------------------------------------------- */

  private val s3_takenMask = s2_baseTableTakenMask.map(RegEnable(_, s2_fire))

  private val s3_baseTableCtrs    = RegEnable(s2_baseTableCtrs, s2_fire)
  private val s3_tablePredictions = s2_tablePredictions.map(RegEnable(_, s2_fire))

  io.takenMask := s3_takenMask

  io.meta.baseTableCtrs    := s3_baseTableCtrs
  io.meta.tablePredictions := s3_tablePredictions

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 0
     - receive train request
     -------------------------------------------------------------------------------------------------------------- */

  private val t0_trainValid = io.train.valid && io.train.bits.attribute.isConditional && io.enable
  private val t0_train      = io.train.bits

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 1
     - train base table and tagged tables
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_trainValid = RegNext(t0_trainValid)
  private val t1_train      = RegEnable(t0_train, t0_trainValid)

  private val t1_tageMeta = t1_train.meta.tage
  private val t1_mbtbMeta = t1_train.meta.mbtb

  private val t1_takenEntryIdx = t1_train.meta.takenEntryIdx

  baseTable.io.train.valid := t1_trainValid
  baseTable.io.train.bits  := t1_train

  private val t1_foldedHistForIdx = histInfoForIdx.map(io.foldedPathHistForTrain.getHistWithInfo(_).foldedHist)
  private val t1_foldedHistForTag = histInfoForTag.map(io.foldedPathHistForTrain.getHistWithInfo(_).foldedHist)
  private val t1_anotherFoldedHistForTag =
    anotherHistInfoForTag.map(io.foldedPathHistForTrain.getHistWithInfo(_).foldedHist)

  private val t1_providerNewTakenCtr = Wire(Vec(MainBtbResultNumEntries, new SaturateCounter(TakenCtrWidth)))
  t1_tageMeta.tablePredictions.zip(t1_mbtbMeta.positions).zip(t1_providerNewTakenCtr).foreach {
    case ((tablePrediction, position), newCtr) =>
      val hasProvider = tablePrediction.hasProvider
      val oldCtr      = tablePrediction.providerTakenCtr

      when(hasProvider && position < t1_train.cfiPosition) {
        newCtr.value := oldCtr.getDecrease
      }.elsewhen(hasProvider && position === t1_train.cfiPosition) {
        newCtr.value := oldCtr.getUpdate(t1_train.taken)
      }.otherwise {
        newCtr.value := oldCtr.value
      }
  }

  // increase provider's useful ctr only when provider is right and hcProvider is wrong
  private val t1_providerNewUsefulCtr = Wire(Vec(MainBtbResultNumEntries, new SaturateCounter(UsefulCtrWidth)))
  t1_tageMeta.tablePredictions.zip(t1_mbtbMeta.positions).zip(t1_providerNewUsefulCtr).foreach {
    case ((tableResult, position), newCtr) =>
      val hasProvider = tableResult.hasProvider
      val predTaken   = tableResult.providerTakenCtr.isPositive
      val hcPredTaken = tableResult.hcProviderTakenCtr.isPositive
      val oldCtr      = tableResult.providerUsefulCtr

      when(hasProvider && position < t1_train.cfiPosition) {
        newCtr.value := Mux(
          !predTaken,
          Mux(hcPredTaken, oldCtr.getIncrease, oldCtr.value),
          oldCtr.getDecrease
        )
      }.elsewhen(hasProvider && position === t1_train.cfiPosition) {
        newCtr.value := Mux(
          predTaken === t1_train.taken,
          Mux(hcPredTaken =/= t1_train.taken, oldCtr.getIncrease, oldCtr.value),
          oldCtr.getDecrease
        )
      }.otherwise {
        newCtr.value := oldCtr.value
      }
  }

  private val t1_hcProviderNewTakenCtr = Wire(Vec(MainBtbResultNumEntries, new SaturateCounter(TakenCtrWidth)))
  t1_tageMeta.tablePredictions.zip(t1_mbtbMeta.positions).zip(t1_hcProviderNewTakenCtr).foreach {
    case ((tablePrediction, position), newCtr) =>
      val hasHcProvider = tablePrediction.hasHcProvider
      val oldCtr        = tablePrediction.providerTakenCtr

      when(hasHcProvider && position < t1_train.cfiPosition) {
        newCtr.value := oldCtr.getDecrease
      }.elsewhen(hasHcProvider && position === t1_train.cfiPosition) {
        newCtr.value := oldCtr.getUpdate(t1_train.taken)
      }.otherwise {
        newCtr.value := oldCtr.value
      }
  }

  private val t1_hcProviderNewUsefulCtr = Wire(Vec(MainBtbResultNumEntries, new SaturateCounter(UsefulCtrWidth)))
  t1_tageMeta.tablePredictions.zip(t1_mbtbMeta.positions).zip(t1_hcProviderNewUsefulCtr).foreach {
    case ((tableResult, position), newCtr) =>
      val hasHcProvider = tableResult.hasProvider
      val predTaken     = tableResult.providerTakenCtr.isPositive
      val hcPredTaken   = tableResult.hcProviderTakenCtr.isPositive
      val oldCtr        = tableResult.providerUsefulCtr

      when(hasHcProvider && position < t1_train.cfiPosition) {
        newCtr.value := Mux(
          !predTaken,
          Mux(hcPredTaken, oldCtr.getIncrease, oldCtr.value),
          oldCtr.getDecrease
        )
      }.elsewhen(hasHcProvider && position === t1_train.cfiPosition) {
        newCtr.value := Mux(
          predTaken === t1_train.taken,
          Mux(hcPredTaken =/= t1_train.taken, oldCtr.getIncrease, oldCtr.value),
          oldCtr.getDecrease
        )
      }.otherwise {
        newCtr.value := oldCtr.value
      }
  }

  private val t1_hasProvider = t1_tageMeta.tablePredictions(t1_takenEntryIdx).hasProvider
  private val t1_providerIdx = t1_tageMeta.tablePredictions(t1_takenEntryIdx).providerTableIdx

  private val t1_providerIdxOH          = UIntToOH(t1_providerIdx, NumTables)
  private val t1_longerHistoryTableMask = ~((t1_providerIdxOH - 1.U) | t1_providerIdxOH)

  private val t1_hasNotUsefulTableMask =
    VecInit(t1_tageMeta.tablePredictions(t1_takenEntryIdx).allTableNotUsefulWayMask.map {
      notUsefulWayMask => notUsefulWayMask.reduce(_ || _)
    })

  private val t1_canAllocateTableMask =
    Mux(
      t1_hasProvider,
      t1_longerHistoryTableMask.asUInt & t1_hasNotUsefulTableMask.asUInt,
      t1_hasNotUsefulTableMask.asUInt
    )

  // FIXME: currently only allocate one table
  private val t1_allocateTableMaskOH = PriorityEncoderOH(t1_canAllocateTableMask)
  private val t1_allocateWayMask =
    Mux1H(t1_allocateTableMaskOH, t1_tageMeta.tablePredictions(t1_takenEntryIdx).allTableNotUsefulWayMask)

  private val t1_providerMispredict =
    t1_hasProvider && t1_tageMeta.tablePredictions(t1_takenEntryIdx).providerTakenCtr.isPositive =/= t1_train.taken

  // TODO: count t1_hasNotUseful for each set
  //   when count > threshold, reset all useful ctrs in the set to 0
  private val t1_allocateValid = t1_trainValid && t1_providerMispredict && t1_canAllocateTableMask.andR

  private val t1_newEntry = Wire(new TageEntry)
  t1_newEntry.valid := true.B
  t1_newEntry.tag :=
    getTag(
      t1_train.startVAddr,
      Mux1H(t1_allocateTableMaskOH, t1_foldedHistForTag),
      Mux1H(t1_allocateTableMaskOH, t1_anotherFoldedHistForTag)
    ) ^ t1_train.cfiPosition
  t1_newEntry.takenCtr.resetWeak(t1_train.taken)
  t1_newEntry.usefulCtr.value := 0.U // TODO: reconsider the initial value of useful ctr

  tables.zipWithIndex.foreach {
    case (table, tableIdx) =>
      table.io.writeReqValid := t1_allocateValid && t1_allocateTableMaskOH(tableIdx)
      table.io.writeSetIdx   := getSetIndex(t1_train.startVAddr, t1_foldedHistForIdx(tableIdx), table.NumSets)
      table.io.writeBankMask := getBankMask(t1_train.startVAddr)
      table.io.writeWayMask  := t1_allocateWayMask
      table.io.writeEntry    := t1_newEntry
  }

  private val t1_debug_mbtbHitCfi =
    t1_mbtbMeta.hitMask.zip(t1_mbtbMeta.positions).zip(t1_mbtbMeta.attributes).map {
      case ((hit, position), attribute) =>
        hit && t1_train.taken && position === t1_train.cfiPosition && attribute === t1_train.attribute
    }.reduce(_ || _)

//  private val t1_debug_mbtbMissWrong =
//    t1_trainValid && !t1_debug_mbtbHitCfi && !t1_tageMeta.debug_taken && t1_train.taken
//
//  private val t1_predictTakenRight = t1_trainValid && t1_tageMeta.debug_taken && t1_train.taken &&
//    t1_tageMeta.debug_takenPosition === t1_train.cfiPosition &&
//    t1_tageMeta.debug_takenAttribute === t1_train.attribute
//
//  private val t1_debug_positionWrong = t1_trainValid && t1_tageMeta.debug_takenPosition =/= t1_train.cfiPosition
//
//  private val t1_debug_directionWrong = t1_trainValid && t1_tageMeta.debug_taken =/= t1_train.taken

  /* *** perf counters */
  XSPerfAccumulate("total_train", t1_trainValid)
//  XSPerfAccumulate("tage_predict_taken_right", t1_predictTakenRight)
//  XSPerfAccumulate("tage_position_wrong", t1_debug_positionWrong)
//  XSPerfAccumulate("tage_direction_wrong", t1_debug_directionWrong)
  XSPerfAccumulate("tage_mbtb_hit_cfi", t1_trainValid && t1_debug_mbtbHitCfi)
//  XSPerfAccumulate("tage_mbtb_miss_wrong", t1_debug_mbtbMissWrong)

}

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
import freechips.rocketchip.util.SeqToAugmentedSeq
import org.chipsalliance.cde.config.Parameters
import scala.math.min
import utility.XSPerfAccumulate
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BtbInfo
import xiangshan.frontend.bpu.FoldedHistoryInfo
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.phr.PhrAllFoldedHistories

/**
 * This module is the implementation of the TAGE (TAgged GEometric history length predictor).
 */
class Tage(implicit p: Parameters) extends BasePredictor with HasTageParameters with Helpers {
  class TageIO(implicit p: Parameters) extends BasePredictorIO {
    val mbtbResult:             Vec[Valid[BtbInfo]]   = Input(Vec(NumBtbResultEntries, Valid(new BtbInfo)))
    val foldedPathHist:         PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val foldedPathHistForTrain: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val condTakenMask:          Vec[Bool]             = Output(Vec(NumBtbResultEntries, Bool()))
    val readBankIdx:            UInt                  = Output(UInt(log2Ceil(NumBanks).W)) // to resolveQueue
    val meta:                   TageMeta              = Output(new TageMeta)
  }
  val io: TageIO = IO(new TageIO)

  /* *** submodules *** */
  private val baseTable = Module(new TageBaseTable)
  private val tables    = TableInfos.map(tableInfo => Module(new TageTable(tableInfo.NumSets)))

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

  private val s0_fire       = io.stageCtrl.s0_fire && io.enable
  private val s0_startVAddr = io.startVAddr

  private val s0_foldedHistForIdx        = histInfoForIdx.map(io.foldedPathHist.getHistWithInfo(_).foldedHist)
  private val s0_foldedHistForTag        = histInfoForTag.map(io.foldedPathHist.getHistWithInfo(_).foldedHist)
  private val s0_anotherFoldedHistForTag = anotherHistInfoForTag.map(io.foldedPathHist.getHistWithInfo(_).foldedHist)

  private val s0_setIdx = TableInfos.zip(s0_foldedHistForIdx).map {
    case (tableInfo, hist) =>
      getSetIndex(s0_startVAddr, hist, tableInfo.NumSets)
  }

  private val s0_bankIdx  = getBankIndex(s0_startVAddr)
  private val s0_bankMask = UIntToOH(s0_bankIdx, NumBanks)

  baseTable.io.readReqValid := s0_fire
  baseTable.io.startVAddr   := s0_startVAddr

  // to stall resolveQueue when bank conflict
  io.readBankIdx := s0_bankIdx

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 1
     - get read data from tables
     - compute temp tag
     -------------------------------------------------------------------------------------------------------------- */

  private val s1_fire       = io.stageCtrl.s1_fire && io.enable
  private val s1_startVAddr = RegEnable(s0_startVAddr, s0_fire)

  private val s1_baseTableCtrs   = baseTable.io.takenCtrs
  private val s1_allTableEntries = tables.map(_.io.readResp.entries)

  private val s1_foldedHistForTag        = s0_foldedHistForTag.map(RegEnable(_, s0_fire))
  private val s1_anotherFoldedHistForTag = s0_anotherFoldedHistForTag.map(RegEnable(_, s0_fire))

  private val s1_tempTag = (0 until NumTables).map { tableIdx =>
    getTag(s1_startVAddr, s1_foldedHistForTag(tableIdx), s1_anotherFoldedHistForTag(tableIdx))
  }

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 2
     - get results from mbtb
     - get hcPred for each btb entry
     - get takenMask
     -------------------------------------------------------------------------------------------------------------- */

  private val s2_baseTableCtrs   = RegEnable(s1_baseTableCtrs, s1_fire)
  private val s2_allTableEntries = s1_allTableEntries.map(RegEnable(_, s1_fire))

  private val s2_tempTag = s1_tempTag.map(RegEnable(_, s1_fire))

  private val s2_mbtbHitMask    = VecInit(io.mbtbResult.map(_.valid))
  private val s2_mbtbPositions  = VecInit(io.mbtbResult.map(_.bits.cfiPosition))
  private val s2_mbtbAttributes = VecInit(io.mbtbResult.map(_.bits.attribute))

  private val s2_mbtbHitCondMask = s2_mbtbHitMask.zip(s2_mbtbAttributes).map {
    case (hit, attribute) => hit && attribute.isConditional
  }

  private val s2_tableResult = s2_mbtbHitCondMask.zip(s2_mbtbPositions).map {
    case (mbtbHit, position) => // for each btb entry
      val result = s2_allTableEntries.zipWithIndex.map {
        case (entries, tableIdx) => // for each table
          val tag        = s2_tempTag(tableIdx) ^ position // use position to distinguish different branches
          val hitWayMask = entries.map(entry => entry.valid && entry.tag === tag)
          val hit        = mbtbHit && hitWayMask.reduce(_ || _)
          val takenCtr   = Mux1H(PriorityEncoderOH(hitWayMask), entries.map(_.takenCtr))
          (hit, takenCtr)
      }
      val hitTableMask = result.map(_._1)
      val hasProvider  = hitTableMask.reduce(_ || _)
      val pred         = Mux1H(PriorityEncoderOH(hitTableMask.reverse), result.map(_._2)).isPositive
      (hasProvider, pred)
  }

  private val s2_condTakenMask = s2_mbtbHitCondMask.zip(s2_mbtbPositions).zip(s2_tableResult).map {
    case ((hit, position), result) =>
      val hasProvider = result._1
      val pred        = result._2
      val altPred     = s2_baseTableCtrs(position).isPositive
//      hit && Mux(hasProvider, pred, altPred)
      hit && altPred // temporarily only use base table prediction
  }

  io.condTakenMask := s2_condTakenMask

  io.meta.baseTableCtrs := s2_baseTableCtrs

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 0
     - send train request to base table
     - send read request to tables
     -------------------------------------------------------------------------------------------------------------- */

  private val t0_hasConditionalBranch = io.train.bits.branches.map { branch =>
    branch.valid && branch.bits.attribute.isConditional
  }.reduce(_ || _)

  private val t0_trainValid       = io.train.valid && t0_hasConditionalBranch
  private val t0_startVAddr       = io.train.bits.startVAddr
  private val t0_branches         = io.train.bits.branches
  private val t0_mispredictBranch = io.train.bits.mispredictBranch

  private val t0_bankIdx  = getBankIndex(t0_startVAddr)
  private val t0_bankMask = UIntToOH(t0_bankIdx, NumBanks)

  // TODO: should block resolveQueue when bank conflict
  private val t0_readBankConflict = t0_trainValid && s0_fire && t0_bankMask === s0_bankMask

  private val t0_valid = t0_trainValid && !t0_readBankConflict && io.enable

  private val t0_foldedHistForIdx = histInfoForIdx.map(io.foldedPathHistForTrain.getHistWithInfo(_).foldedHist)
  private val t0_foldedHistForTag = histInfoForTag.map(io.foldedPathHistForTrain.getHistWithInfo(_).foldedHist)
  private val t0_anotherFoldedHistForTag =
    anotherHistInfoForTag.map(io.foldedPathHistForTrain.getHistWithInfo(_).foldedHist)

  private val t0_setIdx = TableInfos.zip(t0_foldedHistForIdx).map {
    case (tableInfo, hist) =>
      getSetIndex(t0_startVAddr, hist, tableInfo.NumSets)
  }

  baseTable.io.train.valid := t0_trainValid
  baseTable.io.train.bits  := io.train.bits

  tables.zipWithIndex.foreach {
    case (table, tableIdx) =>
      table.io.readReq.valid         := s0_fire || t0_valid
      table.io.readReq.bits.setIdx   := Mux(t0_valid, t0_setIdx(tableIdx), s0_setIdx(tableIdx))
      table.io.readReq.bits.bankMask := Mux(t0_valid, t0_bankMask, s0_bankMask)
  }

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 1
     - get read data from tables
     - compute temp tag
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_valid            = RegNext(t0_valid) && io.enable
  private val t1_startVAddr       = RegEnable(t0_startVAddr, t0_valid)
  private val t1_branches         = RegEnable(t0_branches, t0_valid)
  private val t1_mispredictBranch = RegEnable(t0_mispredictBranch, t0_valid)

  private val t1_setIdx   = t0_setIdx.map(RegEnable(_, t0_valid))
  private val t1_bankMask = RegEnable(t0_bankMask, t0_valid)

  private val t1_foldedHistForTag        = t0_foldedHistForTag.map(RegEnable(_, t0_valid))
  private val t1_anotherFoldedHistForTag = t0_anotherFoldedHistForTag.map(RegEnable(_, t0_valid))

  private val t1_allTableEntries = tables.map(_.io.readResp.entries)
  private val t1_allocFailCtr    = tables.map(_.io.readResp.allocFailCtr)

  private val t1_tempTag = (0 until NumTables).map { tableIdx =>
    getTag(t1_startVAddr, t1_foldedHistForTag(tableIdx), t1_anotherFoldedHistForTag(tableIdx))
  }

  private val t1_hasMispredictBranch = t1_mispredictBranch.valid && t1_mispredictBranch.bits.attribute.isConditional

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 2
     - update branches' takenCtr and usefulCtr
     - allocate a new entry when mispredict
     -------------------------------------------------------------------------------------------------------------- */

  private val t2_valid    = RegNext(t1_valid) && io.enable
  private val t2_branches = RegEnable(t1_branches, t1_valid)

  private val t2_setIdx   = t1_setIdx.map(RegEnable(_, t1_valid))
  private val t2_bankMask = RegEnable(t1_bankMask, t1_valid)

  private val t2_allTableEntries = t1_allTableEntries.map(RegEnable(_, t1_valid))
  private val t2_allocFailCtr    = t1_allocFailCtr.map(RegEnable(_, t1_valid))
  private val t2_tempTag         = t1_tempTag.map(RegEnable(_, t1_valid))

  private val t2_hasMispredictBranch = RegEnable(t1_hasMispredictBranch, t1_valid)
  private val t2_mispredictBranch    = RegEnable(t1_mispredictBranch, t1_valid)

  private val t2_newTakenCtr  = Wire(Vec(NumTables, Vec(NumWays, new SaturateCounter(TakenCtrWidth))))
  private val t2_newUsefulCtr = Wire(Vec(NumTables, Vec(NumWays, new SaturateCounter(UsefulCtrWidth))))
  private val t2_hitTag       = Wire(Vec(NumTables, Vec(NumWays, UInt(TagWidth.W))))

  // Seq[NumTables][NumWays]
  private val t2_updateMask = t2_allTableEntries.zipWithIndex.map {
    case (entries, tableIdx) => // for each table
      val hitWayMask = entries.zipWithIndex.map {
        case (entry, wayIdx) => // for each way
          val result = t2_branches.map {
            branch => // for each branch
              val taken        = branch.bits.taken
              val mispredict   = branch.bits.mispredict
              val attribute    = branch.bits.attribute
              val position     = branch.bits.cfiPosition
              val tag          = t2_tempTag(tableIdx) ^ position
              val hit          = entry.valid && branch.valid && entry.tag === tag && attribute.isConditional
              val newTakenCtr  = entry.takenCtr.getUpdate(taken)
              val newUsefulCtr = entry.usefulCtr.getUpdate(!mispredict)
              (hit, newTakenCtr, newUsefulCtr, tag)
          }
          val hitBranchMask = result.map(_._1)
          t2_newTakenCtr(tableIdx)(wayIdx).value  := Mux1H(PriorityEncoderOH(hitBranchMask), result.map(_._2))
          t2_newUsefulCtr(tableIdx)(wayIdx).value := Mux1H(PriorityEncoderOH(hitBranchMask), result.map(_._3))
          t2_hitTag(tableIdx)(wayIdx)             := Mux1H(PriorityEncoderOH(hitBranchMask), result.map(_._4))
          hitBranchMask.reduce(_ || _)
      }
      hitWayMask
  }

  private val t2_mispredictBranchHitTableMask = t2_allTableEntries.zipWithIndex.map {
    case (entries, tableIdx) => // for each table
      val tag        = t2_tempTag(tableIdx) ^ t2_mispredictBranch.bits.cfiPosition
      val hitWayMask = entries.map(entry => entry.valid && entry.tag === tag)
      hitWayMask.reduce(_ || _)
  }
  private val t2_hasProvider = t2_mispredictBranchHitTableMask.reduce(_ || _)

  // only allocate new entry to tables with longer history
  private val t2_providerIdxOH = PriorityEncoderOH(t2_mispredictBranchHitTableMask.reverse).reverse.asUInt

  private val t2_longerHistoryTableMask = (~((t2_providerIdxOH - 1.U) | t2_providerIdxOH)).asUInt

  // only allocate new entry to tables that have not useful entry
  private val t2_allTableNotUsefulWayMask = t2_allTableEntries.map { entries =>
    entries.map(entry => entry.usefulCtr.value === 0.U).asUInt
  }
  private val t2_hasNotUsefulTableMask = t2_allTableNotUsefulWayMask.map(_.orR).asUInt

  private val t2_canAllocateTableMask =
    Mux(
      t2_hasProvider,
      t2_longerHistoryTableMask & t2_hasNotUsefulTableMask,
      t2_hasNotUsefulTableMask
    )

  // FIXME: currently only allocate one table, maybe a dynamic number of allocation
  private val t2_needAllocate        = t2_hasMispredictBranch && t2_canAllocateTableMask.orR
  private val t2_allocateTableMaskOH = PriorityEncoderOH(t2_canAllocateTableMask) & Fill(NumTables, t2_needAllocate)
  private val t2_allocateWayMask     = Mux1H(t2_allocateTableMaskOH, t2_allTableNotUsefulWayMask)
  private val t2_allocateWayMaskOH   = PriorityEncoderOH(t2_allocateWayMask) & Fill(NumWays, t2_needAllocate)

  private val t2_allocFailTableMask =
    Mux(
      t2_hasProvider,
      t2_longerHistoryTableMask & (~t2_hasNotUsefulTableMask).asUInt,
      (~t2_hasNotUsefulTableMask).asUInt
    )

  private val t2_needResetUsefulCtr = t2_allocFailTableMask.asBools.zip(t2_allocFailCtr).map {
    case (allocFail, ctr) =>
      allocFail && ctr.isSaturatePositive
  }
  private val t2_needIncreaseAllocFailCtr = t2_allocFailTableMask.asBools.zip(t2_allocFailCtr).map {
    case (allocFail, ctr) =>
      allocFail && !ctr.isSaturatePositive
  }

  tables.zipWithIndex.foreach {
    case (table, tableIdx) =>
      table.io.writeSetIdx   := t2_setIdx(tableIdx)
      table.io.writeBankMask := t2_bankMask

      table.io.updateReq.valid := t2_valid && (t2_updateMask(tableIdx).reduce(_ || _)
        || t2_allocateTableMaskOH(tableIdx))

      val writeEntries    = Wire(Vec(NumWays, new TageEntry))
      val writeWayMask    = Wire(Vec(NumWays, Bool()))
      val allocateWayMask = t2_allocateWayMaskOH & Fill(NumWays, t2_allocateTableMaskOH(tableIdx))
      writeEntries.zip(t2_updateMask(tableIdx)).zip(allocateWayMask.asBools)
        .zip(writeWayMask).zipWithIndex.foreach {
          case ((((entry, update), allocate), writeWayEnable), wayIdx) =>
            entry.valid := update || allocate
            entry.tag := Mux(
              allocate,
              t2_tempTag(tableIdx) ^ t2_mispredictBranch.bits.cfiPosition,
              t2_hitTag(tableIdx)(wayIdx)
            )
            entry.takenCtr.value := Mux(
              allocate,
              Mux(t2_mispredictBranch.bits.taken, (1 << (TakenCtrWidth - 1)).U, (1 << (TakenCtrWidth - 1) - 1).U),
              t2_newTakenCtr(tableIdx)(wayIdx).value
            )
            entry.usefulCtr.value := Mux(
              allocate,
              Mux(t2_needResetUsefulCtr(tableIdx), 0.U, UsefulCtrInitValue.U),
              t2_newUsefulCtr(tableIdx)(wayIdx).value
            )
            writeWayEnable := update || allocate
        }
      table.io.updateReq.bits.wayMask := writeWayMask.asUInt
      table.io.updateReq.bits.entries := writeEntries

      table.io.needResetUsefulCtr       := t2_valid && t2_needAllocate && t2_needResetUsefulCtr(tableIdx)
      table.io.needIncreaseAllocFailCtr := t2_valid && t2_needAllocate && t2_needIncreaseAllocFailCtr(tableIdx)
      table.io.oldAllocFailCtr          := t2_allocFailCtr(tableIdx)
  }

  /* --------------------------------------------------------------------------------------------------------------
     performance counter
     -------------------------------------------------------------------------------------------------------------- */

  XSPerfAccumulate("total_train", t0_valid)
  XSPerfAccumulate("read_conflict", t0_readBankConflict)
  // TODO: add more
}

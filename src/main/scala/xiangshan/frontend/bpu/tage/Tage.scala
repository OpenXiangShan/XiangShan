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
    val resolveTrain:           Valid[ResolveTrain]   = Input(Valid(new ResolveTrain))     // TODO: temp, remove it
    val readBankIdx:            UInt                  = Output(UInt(log2Ceil(NumBanks).W)) // to resolveQueue
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

  private val s0_fire    = io.stageCtrl.s0_fire && io.enable
  private val s0_startPc = io.startVAddr

  private val s0_foldedHistForIdx        = histInfoForIdx.map(io.foldedPathHist.getHistWithInfo(_).foldedHist)
  private val s0_foldedHistForTag        = histInfoForTag.map(io.foldedPathHist.getHistWithInfo(_).foldedHist)
  private val s0_anotherFoldedHistForTag = anotherHistInfoForTag.map(io.foldedPathHist.getHistWithInfo(_).foldedHist)

  private val s0_setIdx = TableInfos.zip(s0_foldedHistForIdx).map {
    case (tableInfo, hist) =>
      getSetIndex(s0_startPc, hist, tableInfo.NumSets)
  }

  private val s0_bankIdx  = getBankIndex(s0_startPc)
  private val s0_bankMask = UIntToOH(s0_bankIdx, NumBanks)

  baseTable.io.readReqValid := s0_fire
  baseTable.io.startPc      := s0_startPc

  tables.zipWithIndex.foreach {
    case (table, tableIdx) =>
      table.io.readReqValid := s0_fire
      table.io.readSetIdx   := s0_setIdx(tableIdx)
      table.io.readBankMask := s0_bankMask
  }

  // to stall resolveQueue when bank conflict
  io.readBankIdx := s0_bankIdx

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 1
     - get read data from tables
     - compute temp tag
     -------------------------------------------------------------------------------------------------------------- */

  private val s1_fire    = io.stageCtrl.s1_fire && io.enable
  private val s1_startPc = RegEnable(s0_startPc, s0_fire)

  private val s1_baseTableCtrs = baseTable.io.takenCtrs
  private val s1_tableResp     = tables.map(_.io.readData)

  private val s1_foldedHistForTag        = s0_foldedHistForTag.map(RegEnable(_, s0_fire))
  private val s1_anotherFoldedHistForTag = s0_anotherFoldedHistForTag.map(RegEnable(_, s0_fire))

  private val s1_tempTag = (0 until NumTables).map { tableIdx =>
    getTag(s1_startPc, s1_foldedHistForTag(tableIdx), s1_anotherFoldedHistForTag(tableIdx))
  }

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 2
     - get results from mbtb
     - select provider for each btb entry
     - get takenMask
     -------------------------------------------------------------------------------------------------------------- */

  private val s2_fire    = io.stageCtrl.s2_fire && io.enable
  private val s2_startPc = RegEnable(s1_startPc, s1_fire)

  private val s2_baseTableCtrs = RegEnable(s1_baseTableCtrs, s1_fire)
  private val s2_tableResp     = s1_tableResp.map(RegEnable(_, s1_fire))

  private val s2_tempTag = s1_tempTag.map(RegEnable(_, s1_fire))

  // filter out branches that behind the fetch block start address
  private val s2_mbtbHitMask = io.mbtbResult.hitMask.zip(io.mbtbResult.positions).map {
    case (hit, position) =>
      hit && position >= s2_startPc(FetchBlockSizeWidth - 1, 1)
  }
  private val s2_mbtbPositions  = io.mbtbResult.positions
  private val s2_mbtbAttributes = io.mbtbResult.attributes

  private val s2_tableResults = s2_mbtbHitMask.zip(s2_mbtbAttributes).zip(s2_mbtbPositions).map {
    case ((mbtbHit, attribute), position) => // for each btb entry
      val takenCtrs = s2_tableResp.zipWithIndex.map {
        case (tageEntries, tableIdx) => // for each table
          val tag          = s2_tempTag(tableIdx) ^ position // use position to distinguish different branches
          val hitWayMaskOH = PriorityEncoderOH(tageEntries.map(entry => entry.valid && entry.tag === tag))
          val hit          = mbtbHit && attribute.isConditional && hitWayMaskOH.reduce(_ || _)
          val takenCtr     = Mux1H(hitWayMaskOH, tageEntries.map(_.takenCtr))
          (hit, takenCtr)
      }
      val hitTableMask  = takenCtrs.map(_._1)
      val providerIdxOH = PriorityEncoderOH(hitTableMask.reverse) // provider: the table with the longest history

      val result = Wire(new TableResult)
      result.hasProvider      := hitTableMask.reduce(_ || _)
      result.providerTakenCtr := Mux1H(providerIdxOH, takenCtrs.map(_._2))
      result
  }

  private val s2_takenMask = s2_mbtbHitMask.zip(s2_mbtbAttributes).zip(s2_mbtbPositions).zip(s2_tableResults).map {
    case (((hit, attribute), position), result) =>
      MuxCase(
        false.B,
        Seq(
          (hit && attribute.isConditional && result.hasProvider)       -> result.providerTakenCtr.isPositive,
          (hit && attribute.isConditional && !result.hasProvider)      -> s2_baseTableCtrs(position).isPositive,
          (hit && (attribute.isDirect || hit && attribute.isIndirect)) -> true.B
        )
      )
  }

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 3
     - output takenMask
     -------------------------------------------------------------------------------------------------------------- */

  private val s3_takenMask = s2_takenMask.map(RegEnable(_, s2_fire))
  io.takenMask := s3_takenMask

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 0
     - send train request to base table
     - send read request to tables
     -------------------------------------------------------------------------------------------------------------- */

  private val t0_hasConditionalBranch = io.resolveTrain.bits.branches.map { branch =>
    branch.valid && branch.bits.attribute.isConditional
  }.reduce(_ || _)

  private val t0_fire     = io.resolveTrain.valid && t0_hasConditionalBranch && io.enable
  private val t0_startPc  = io.resolveTrain.bits.startVAddr
  private val t0_branches = io.resolveTrain.bits.branches

  private val t0_foldedHistForIdx = histInfoForIdx.map(io.foldedPathHistForTrain.getHistWithInfo(_).foldedHist)
  private val t0_foldedHistForTag = histInfoForTag.map(io.foldedPathHistForTrain.getHistWithInfo(_).foldedHist)
  private val t0_anotherFoldedHistForTag =
    anotherHistInfoForTag.map(io.foldedPathHistForTrain.getHistWithInfo(_).foldedHist)

  private val t0_setIdx = TableInfos.zip(t0_foldedHistForIdx).map {
    case (tableInfo, hist) =>
      getSetIndex(t0_startPc, hist, tableInfo.NumSets)
  }

  private val t0_bankIdx  = getBankIndex(t0_startPc)
  private val t0_bankMask = UIntToOH(t0_bankIdx, NumBanks)

  baseTable.io.train.valid := t0_fire
  baseTable.io.train.bits  := io.resolveTrain.bits

  tables.zipWithIndex.foreach {
    case (table, tableIdx) =>
      table.io.readReqValid := t0_fire
      table.io.readSetIdx   := t0_setIdx(tableIdx)
      table.io.readBankMask := t0_bankMask
  }

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 1
     - get read data from tables
     - compute temp tag
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_fire     = RegNext(t0_fire) && io.enable
  private val t1_startPc  = RegEnable(t0_startPc, t0_fire)
  private val t1_branches = RegEnable(t0_branches, t0_fire)

  private val t1_setIdx   = t0_setIdx.map(RegEnable(_, t0_fire))
  private val t1_bankMask = RegEnable(t0_bankMask, t0_fire)

  private val t1_foldedHistForTag        = t0_foldedHistForTag.map(RegEnable(_, t0_fire))
  private val t1_anotherFoldedHistForTag = t0_anotherFoldedHistForTag.map(RegEnable(_, t0_fire))

  private val t1_mispredictBranchMask = t1_branches.map {
    branch => branch.valid && branch.bits.mispredict && branch.bits.attribute.isConditional
  }
  when(t1_fire) {
    assert(PopCount(t1_mispredictBranchMask) <= 1.U)
  }

  private val t1_hasMispredictBranch = t1_mispredictBranchMask.reduce(_ || _)
  private val t1_mispredictBranchIdx = PriorityEncoder(t1_mispredictBranchMask)
  private val t1_mispredictBranch    = t1_branches(t1_mispredictBranchIdx)

  private val t1_tableResp = tables.map(_.io.readData)

  private val t1_tempTag = (0 until NumTables).map { tableIdx =>
    getTag(t1_startPc, t1_foldedHistForTag(tableIdx), t1_anotherFoldedHistForTag(tableIdx))
  }

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 2
     - update branches' takenCtr and usefulCtr
     - allocate a new entry when mispredict
     -------------------------------------------------------------------------------------------------------------- */

  private val t2_fire     = RegNext(t1_fire) && io.enable
  private val t2_branches = RegEnable(t1_branches, t1_fire)

  private val t2_setIdx   = t1_setIdx.map(RegEnable(_, t1_fire))
  private val t2_bankMask = RegEnable(t1_bankMask, t1_fire)

  private val t2_hasMispredictBranch = RegEnable(t1_hasMispredictBranch, t1_fire)
  private val t2_mispredictBranch    = RegEnable(t1_mispredictBranch, t1_fire)

  private val t2_tableResp = t1_tableResp.map(RegEnable(_, t1_fire))
  private val t2_tempTag   = t1_tempTag.map(RegEnable(_, t1_fire))

  private val t2_newTakenCtr  = Wire(Vec(NumTables, Vec(NumWays, new SaturateCounter(TakenCtrWidth))))
  private val t2_newUsefulCtr = Wire(Vec(NumTables, Vec(NumWays, new SaturateCounter(UsefulCtrWidth))))

  // Seq[NumTables][NumWays]
  // mask a way that need update counter
  private val t2_updateMask = t2_tableResp.zipWithIndex.map {
    case (entries, tableIdx) => // for each table
      val hitWayMask = entries.zipWithIndex.map {
        case (entry, wayIdx) => // for each way
          val hitBranchResult = t2_branches.map {
            branch => // for each branch
              val taken        = branch.bits.taken
              val mispredict   = branch.bits.mispredict
              val attribute    = branch.bits.attribute
              val tag          = t2_tempTag(tableIdx) ^ branch.bits.position
              val hit          = entry.valid && entry.tag === tag && attribute.isConditional
              val newTakenCtr  = entry.takenCtr.getUpdate(taken)
              val newUsefulCtr = entry.usefulCtr.getUpdate(!mispredict)
              (hit, newTakenCtr, newUsefulCtr)
          }
          val hitBranchMask = hitBranchResult.map(_._1)
          t2_newTakenCtr(tableIdx)(wayIdx).value  := Mux1H(hitBranchMask, hitBranchResult.map(_._2))
          t2_newUsefulCtr(tableIdx)(wayIdx).value := Mux1H(hitBranchMask, hitBranchResult.map(_._3))
          when(t2_fire) {
            assert(PopCount(hitBranchMask) <= 1.U)
          }
          hitBranchMask.reduce(_ || _)
      }
      hitWayMask
  }

  private val t2_mispredictBranchHitTableMask = t2_tableResp.zipWithIndex.map {
    case (entries, tableIdx) => // for each table
      val tag        = t2_tempTag(tableIdx) ^ t2_mispredictBranch.bits.position
      val hitWayMask = entries.map(entry => entry.valid && entry.tag === tag)
      hitWayMask.reduce(_ || _)
  }
  private val t2_mispredictBranchHasProvider = t2_mispredictBranchHitTableMask.reduce(_ || _)

  // only allocate new entry to tables with longer history than that of provider
  private val t2_mispredictBranchProviderIdxOH = PriorityEncoderOH(t2_mispredictBranchHitTableMask.reverse).asUInt
  private val t2_longerHistoryTableMask =
    (~((t2_mispredictBranchProviderIdxOH - 1.U) | t2_mispredictBranchProviderIdxOH)).asUInt

  // only allocate new entry to tables that have not useful entry
  private val t2_allTableNotUsefulWayMask = t2_tableResp.map { entries =>
    entries.map(entry => entry.usefulCtr.value === 0.U).asUInt
  }
  private val t2_hasNotUsefulTableMask = t2_allTableNotUsefulWayMask.map(_.orR).asUInt

  private val t2_canAllocateTableMask =
    Mux(
      t2_mispredictBranchHasProvider,
      t2_longerHistoryTableMask & t2_hasNotUsefulTableMask,
      t2_hasNotUsefulTableMask
    )

  private val t2_allocFailTableMask =
    Mux(
      t2_mispredictBranchHasProvider,
      t2_longerHistoryTableMask & (~t2_hasNotUsefulTableMask).asUInt,
      (~t2_hasNotUsefulTableMask).asUInt
    )

  // FIXME: currently only allocate one table, maybe a dynamic number of allocation
  private val t2_allocateTableMaskOH = PriorityEncoderOH(t2_canAllocateTableMask).asBools
  private val t2_allocateWayMask     = Mux1H(t2_allocateTableMaskOH, t2_allTableNotUsefulWayMask)
  private val t2_allocateMask = t2_canAllocateTableMask.asBools.map { tableSel =>
    t2_allocateWayMask.asBools.map(waySel => tableSel && waySel)
  }

  private val t2_needAllocate = t2_hasMispredictBranch && t2_canAllocateTableMask.orR

  tables.zipWithIndex.foreach {
    case (table, tableIdx) =>
      val writeData = Wire(Vec(NumWays, new TageEntry))
      writeData.zipWithIndex.foreach {
        case (entry, wayIdx) =>
          entry.valid := true.B
          entry.tag   := t2_tempTag(tableIdx) ^ t2_mispredictBranch.bits.position
          entry.takenCtr.value :=
            Mux(
              t2_allocateMask(tableIdx)(wayIdx),
              getTakenCtrInitValue(t2_mispredictBranch.bits.taken, TakenCtrWidth),
              t2_newTakenCtr(tableIdx)(wayIdx).value
            )
          entry.usefulCtr.value :=
            Mux(
              t2_allocateMask(tableIdx)(wayIdx),
              0.U, // TODO: reconsider the initial value of usefulCtr
              t2_newUsefulCtr(tableIdx)(wayIdx).value
            )
      }
      table.io.writeReqValid := t2_fire && (
        t2_needAllocate && t2_allocateTableMaskOH(tableIdx) || t2_updateMask(tableIdx).reduce(_ || _)
      )
      table.io.writeSetIdx   := t2_setIdx(tableIdx)
      table.io.writeBankMask := t2_bankMask
      table.io.writeWayMask  := t2_updateMask(tableIdx).asUInt | t2_allocateMask(tableIdx).asUInt
      table.io.writeData     := writeData
      table.io.allocFail     := t2_fire && t2_allocFailTableMask(tableIdx)
  }

  /* --------------------------------------------------------------------------------------------------------------
     performance counter
     -------------------------------------------------------------------------------------------------------------- */

  XSPerfAccumulate("total_train", t0_fire)
  // TODO: add more
}

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
import utility.DataHoldBypass
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
    val condTakenMask:          Vec[Bool]             = Output(Vec(NumBtbResultEntries, Bool()))
    val meta:                   TageMeta              = Output(new TageMeta)
    val trainReady:             Bool                  = Bool()
  }
  val io: TageIO = IO(new TageIO)

  /* *** submodules *** */
  private val baseTable = Module(new TageBaseTable)
  private val tables    = TableInfos.map(tableInfo => Module(new TageTable(tableInfo.NumSets)))

  // reset usefulCtr of all entries when saturated
  private val usefulResetCtr = RegInit(0.U.asTypeOf(new SaturateCounter(UsefulResetCtrWidth)))

  private val useAltCtrVec = RegInit(VecInit.fill(NumUseAltCtrs)(0.U.asTypeOf(new SaturateCounter(UseAltCtrWidth))))

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

  private val s0_foldedHistForIdx = VecInit(histInfoForIdx.map(io.foldedPathHist.getHistWithInfo(_).foldedHist))
  private val s0_foldedHistForTag = VecInit(histInfoForTag.map(io.foldedPathHist.getHistWithInfo(_).foldedHist))
  private val s0_anotherFoldedHistForTag =
    VecInit(anotherHistInfoForTag.map(io.foldedPathHist.getHistWithInfo(_).foldedHist))

  dontTouch(s0_foldedHistForIdx)
  dontTouch(s0_foldedHistForTag)
  dontTouch(s0_anotherFoldedHistForTag)

  private val s0_setIdx = VecInit(TableInfos.zip(s0_foldedHistForIdx).map {
    case (tableInfo, hist) =>
      getSetIndex(s0_startVAddr, hist, tableInfo.NumSets)
  })
  dontTouch(s0_setIdx)

  private val s0_bankIdx  = getBankIndex(s0_startVAddr)
  private val s0_bankMask = UIntToOH(s0_bankIdx, NumBanks)

  baseTable.io.readReqValid := s0_fire
  baseTable.io.startPc      := s0_startVAddr

  tables.zipWithIndex.foreach { case (table, tableIdx) =>
    table.io.predictReadReq.valid         := s0_fire
    table.io.predictReadReq.bits.setIdx   := s0_setIdx(tableIdx)
    table.io.predictReadReq.bits.bankMask := s0_bankMask
  }

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 1
     - get read data from tables
     - compute temp tag
     -------------------------------------------------------------------------------------------------------------- */

  private val s1_fire       = io.stageCtrl.s1_fire && io.enable
  private val s1_startVAddr = RegEnable(s0_startVAddr, s0_fire)
  private val s1_setIdx     = RegEnable(s0_setIdx, s0_fire)

  private val s1_baseTableCtrs   = baseTable.io.takenCtrs
  private val s1_allTableEntries = DataHoldBypass(VecInit(tables.map(_.io.predictReadResp.entries)), RegNext(s0_fire))
  private val s1_allTableUsefulCtrs =
    DataHoldBypass(VecInit(tables.map(_.io.predictReadResp.usefulCtrs)), RegNext(s0_fire))

  private val s1_foldedHistForTag        = RegEnable(s0_foldedHistForTag, s0_fire)
  private val s1_anotherFoldedHistForTag = RegEnable(s0_anotherFoldedHistForTag, s0_fire)

  private val s1_tempTag = VecInit((0 until NumTables).map { tableIdx =>
    getTag(s1_startVAddr, s1_foldedHistForTag(tableIdx), s1_anotherFoldedHistForTag(tableIdx))
  })

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 2
     - get results from mbtb
     - get hcPred for each btb entry
     - get takenMask
     -------------------------------------------------------------------------------------------------------------- */

  private val s2_baseTableCtrs      = RegEnable(s1_baseTableCtrs, s1_fire)
  private val s2_allTableEntries    = RegEnable(s1_allTableEntries, s1_fire)
  private val s2_allTableUsefulCtrs = RegEnable(s1_allTableUsefulCtrs, s1_fire)

  private val s2_startVAddr = RegEnable(s1_startVAddr, s1_fire)
  dontTouch(s2_startVAddr)

  private val s2_setIdx  = RegEnable(s1_setIdx, s1_fire)
  private val s2_tempTag = RegEnable(s1_tempTag, s1_fire)

  private val s2_mbtbHitMask    = io.mbtbResult.hitMask
  private val s2_mbtbPositions  = io.mbtbResult.positions
  private val s2_mbtbAttributes = io.mbtbResult.attributes

  private val s2_branchesVAddr     = VecInit(s2_mbtbPositions.map(getBranchVAddr(s2_startVAddr, _)))
  private val s2_branchesUseAltIdx = s2_branchesVAddr.map(getUseAltIndex)
  private val s2_useAlt            = s2_branchesUseAltIdx.map(idx => useAltCtrVec(idx).isPositive)
  dontTouch(s2_branchesVAddr)

  private val s2_mbtbHitCondMask = s2_mbtbHitMask.zip(s2_mbtbAttributes).map {
    case (hit, attribute) => hit && attribute.isConditional
  }

  private val s2_allBranchPrediction = s2_mbtbHitCondMask.zip(s2_mbtbPositions).map {
    case (mbtbHit, position) => // for each btb entry
      val allTableTagMatchResults = s2_allTableEntries.zip(s2_allTableUsefulCtrs).zipWithIndex.map {
        case ((entriesPerTable, usefulCtrsPerTable), tableIdx) => // for each table
          val tag          = s2_tempTag(tableIdx) ^ position // use position to distinguish different branches
          val hitWayMask   = entriesPerTable.map(entry => entry.valid && entry.tag === tag)
          val hitWayMaskOH = PriorityEncoderOH(hitWayMask)

          val result = Wire(new TagMatchResultPerTable)
          result.hit          := mbtbHit && hitWayMask.reduce(_ || _)
          result.takenCtr     := Mux1H(hitWayMaskOH, entriesPerTable.map(_.takenCtr))
          result.usefulCtr    := Mux1H(hitWayMaskOH, usefulCtrsPerTable)
          result.hitWayMaskOH := DontCare
          result.tag          := DontCare
          result
      }
      val hitTableMask           = allTableTagMatchResults.map(_.hit)
      val hasProvider            = hitTableMask.reduce(_ || _)
      val providerOH             = getLongestHistTableOH(hitTableMask)
      val hitTableMaskNoProvider = hitTableMask.zip(providerOH).map { case (a, b) => a && !b }
      val hasAlt                 = hasProvider && hitTableMaskNoProvider.reduce(_ || _)
      val altOH                  = getLongestHistTableOH(hitTableMaskNoProvider)

      val prediction = Wire(new PredictionPerBranch)
      prediction.hasProvider        := hasProvider
      prediction.providerIsWeak     := Mux1H(providerOH, allTableTagMatchResults.map(_.usefulCtr)).isWeak
      prediction.pred               := Mux1H(providerOH, allTableTagMatchResults.map(_.takenCtr)).isPositive
      prediction.hasAlt             := hasAlt
      prediction.altPred            := Mux1H(altOH, allTableTagMatchResults.map(_.takenCtr)).isPositive
      prediction.debug_providerOH   := providerOH.asUInt
      prediction.debug_altOH        := altOH.asUInt
      prediction.debug_hitTableMask := hitTableMask.asUInt
      prediction
  }

  // used for debug
  private val s2_hitTableMaskPerBranch = VecInit(s2_allBranchPrediction.map(_.debug_hitTableMask))
  private val s2_providerOHPerBranch   = VecInit(s2_allBranchPrediction.map(_.debug_providerOH))
  dontTouch(s2_hitTableMaskPerBranch)
  dontTouch(s2_providerOHPerBranch)

  private val s2_condTakenMask =
    s2_mbtbHitCondMask.zip(s2_mbtbPositions).zip(s2_allBranchPrediction).zip(s2_useAlt).map {
      case (((hit, position), prediction), useAlt) =>
        val hasProvider    = prediction.hasProvider
        val providerIsWeak = prediction.providerIsWeak
        val pred           = prediction.pred
        val hasAlt         = prediction.hasAlt
        val altPred        = prediction.altPred
        val basePred       = s2_baseTableCtrs(position).isPositive

        hit && MuxCase(
          basePred,
          Seq(
            (hasProvider && !providerIsWeak)                    -> pred,
            (hasProvider && providerIsWeak && hasAlt && useAlt) -> altPred
          )
        )
    }

  // only for performance counter
  private val s2_condHitTableMask = s2_mbtbHitCondMask.zip(s2_allBranchPrediction).map { case (hit, prediction) =>
    hit && prediction.hasProvider
  }

  io.condTakenMask := s2_condTakenMask

  io.meta.baseTableCtrs    := s2_baseTableCtrs
  io.meta.debug_useTable   := s2_condHitTableMask
  io.meta.debug_useTableOH := s2_allBranchPrediction.map(_.debug_providerOH)
  io.meta.debug_setIdx     := s2_setIdx
  io.meta.debug_tempTag    := s2_tempTag

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 0
     - send train request to base table
     - send read request to tables
     -------------------------------------------------------------------------------------------------------------- */

  private val t0_hasConditionalBranch = io.train.bits.branches.map { branch =>
    branch.valid && branch.bits.attribute.isConditional
  }.reduce(_ || _)
  dontTouch(t0_hasConditionalBranch)

  private val t0_metaSame = Wire(Bool())

  private val t0_fire = io.train.valid && io.trainReady && t0_hasConditionalBranch && io.enable && t0_metaSame

  private val t0_wrongPathTrain = io.train.valid && io.trainReady && t0_hasConditionalBranch && !t0_metaSame

  private val t0_startVAddr = io.train.bits.startVAddr
  private val t0_branches   = io.train.bits.branches

  private val t0_bankIdx  = getBankIndex(t0_startVAddr)
  private val t0_bankMask = UIntToOH(t0_bankIdx, NumBanks)

  private val t0_readBankConflict = io.train.valid && t0_hasConditionalBranch && s0_fire && t0_bankIdx === s0_bankIdx
  dontTouch(t0_readBankConflict)
  io.trainReady := !t0_readBankConflict

  private val t0_foldedHistForIdx = VecInit(histInfoForIdx.map(io.foldedPathHistForTrain.getHistWithInfo(_).foldedHist))
  private val t0_foldedHistForTag = VecInit(histInfoForTag.map(io.foldedPathHistForTrain.getHistWithInfo(_).foldedHist))
  private val t0_anotherFoldedHistForTag =
    VecInit(anotherHistInfoForTag.map(io.foldedPathHistForTrain.getHistWithInfo(_).foldedHist))

  dontTouch(t0_foldedHistForIdx)
  dontTouch(t0_foldedHistForTag)
  dontTouch(t0_anotherFoldedHistForTag)

  private val t0_setIdxUseHist = VecInit(TableInfos.zip(t0_foldedHistForIdx).map {
    case (tableInfo, hist) =>
      getSetIndex(t0_startVAddr, hist, tableInfo.NumSets)
  })
  private val t0_setIdx = io.train.bits.meta.tage.debug_setIdx
  dontTouch(t0_setIdx)

//  when(t0_fire) {
//    assert(t0_setIdx === io.train.bits.meta.tage.debug_setIdx, "predict setIdx != train setIdx")
//  }

  private val t0_tempTag = io.train.bits.meta.tage.debug_tempTag
  dontTouch(t0_tempTag)

  private val t0_tempTagUseHist = VecInit((0 until NumTables).map { tableIdx =>
    getTag(t0_startVAddr, t0_foldedHistForTag(tableIdx), t0_anotherFoldedHistForTag(tableIdx))
  })

  t0_metaSame := t0_setIdxUseHist === t0_setIdx && t0_tempTagUseHist === t0_tempTag

  baseTable.io.train.valid := t0_fire
  baseTable.io.train.bits  := io.train.bits

  tables.zipWithIndex.foreach { case (table, tableIdx) =>
    table.io.trainReadReq.valid         := t0_fire
    table.io.trainReadReq.bits.setIdx   := t0_setIdx(tableIdx)
    table.io.trainReadReq.bits.bankMask := t0_bankMask
  }

  when(t0_fire) {
    assert(!(s0_fire && s0_bankIdx === t0_bankIdx), "TageTable: predictReadReq and trainReadReq conflict")
  }

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 1
     - get read data from tables
     - compute temp tag
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_fire       = RegNext(t0_fire) && io.enable
  private val t1_startVAddr = RegEnable(t0_startVAddr, t0_fire)
  private val t1_branches   = RegEnable(t0_branches, t0_fire)

  private val t1_setIdx   = RegEnable(t0_setIdx, t0_fire)
  private val t1_bankMask = RegEnable(t0_bankMask, t0_fire)

  private val t1_foldedHistForTag        = RegEnable(t0_foldedHistForTag, t0_fire)
  private val t1_anotherFoldedHistForTag = RegEnable(t0_anotherFoldedHistForTag, t0_fire)

  private val t1_allTableEntries    = VecInit(tables.map(_.io.trainReadResp.entries))
  private val t1_allTableUsefulCtrs = VecInit(tables.map(_.io.trainReadResp.usefulCtrs))

//  private val t1_tempTag = (0 until NumTables).map { tableIdx =>
//    getTag(t1_startVAddr, t1_foldedHistForTag(tableIdx), t1_anotherFoldedHistForTag(tableIdx))
//  }
  private val t1_tempTag = RegEnable(t0_tempTag, t0_fire)

//  private val t1_debugTempTag = RegEnable(io.train.bits.meta.tage.debug_tempTag, t0_fire)
//  when(t1_fire) {
//    assert(VecInit(t1_tempTag) === t1_debugTempTag, "predict tag != train tag")
//  }

  private val t1_branchesVAddr =
    VecInit(t1_branches.map(branch => getBranchVAddr(s2_startVAddr, branch.bits.cfiPosition)))
  private val t1_branchesUseAltIdx = VecInit(t1_branchesVAddr.map(getUseAltIndex))

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 2
     - update branches' takenCtr and usefulCtr
     - allocate a new entry when mispredict
     -------------------------------------------------------------------------------------------------------------- */

  private val t2_fire       = RegNext(t1_fire) && io.enable
  private val t2_branches   = RegEnable(t1_branches, t1_fire)
  private val t2_startVAddr = RegEnable(t1_startVAddr, t1_fire)
  dontTouch(t2_startVAddr)

  private val t2_setIdx   = RegEnable(t1_setIdx, t1_fire)
  private val t2_bankMask = RegEnable(t1_bankMask, t1_fire)

  private val t2_allTableEntries    = RegEnable(t1_allTableEntries, t1_fire)
  private val t2_allTableUsefulCtrs = RegEnable(t1_allTableUsefulCtrs, t1_fire)
  private val t2_tempTag            = RegEnable(t1_tempTag, t1_fire)

  private val t2_branchesVAddr     = RegEnable(t1_branchesVAddr, t1_fire)
  private val t2_branchesUseAltIdx = RegEnable(t1_branchesUseAltIdx, t1_fire)
  dontTouch(t2_branchesVAddr)

  private val t2_allBranchUpdateInfo = t2_branches.map { branch =>
    val allTableTagMatchResults = t2_allTableEntries.zip(t2_allTableUsefulCtrs).zipWithIndex.map {
      case ((entriesPerTable, usefulCtrsPerTable), tableIdx) =>
        val tag          = t2_tempTag(tableIdx) ^ branch.bits.cfiPosition
        val hitWayMask   = entriesPerTable.map(entry => entry.valid && entry.tag === tag)
        val hitWayMaskOH = PriorityEncoderOH(hitWayMask)

        val result = Wire(new TagMatchResultPerTable)
        result.hit          := branch.valid && branch.bits.attribute.isConditional && hitWayMask.reduce(_ || _)
        result.takenCtr     := Mux1H(hitWayMaskOH, entriesPerTable.map(_.takenCtr))
        result.usefulCtr    := Mux1H(hitWayMaskOH, usefulCtrsPerTable)
        result.hitWayMaskOH := hitWayMaskOH.asUInt
        result.tag          := tag
        result
    }
    val hitTableMask           = allTableTagMatchResults.map(_.hit)
    val hasProvider            = hitTableMask.reduce(_ || _)
    val providerOH             = getLongestHistTableOH(hitTableMask)
    val hitTableMaskNoProvider = hitTableMask.zip(providerOH).map { case (a, b) => a && !b }
    val hasAlt                 = hasProvider && hitTableMaskNoProvider.reduce(_ || _)
    val altOH                  = getLongestHistTableOH(hitTableMaskNoProvider)

    val providerOldTakenCtr  = Mux1H(providerOH, allTableTagMatchResults.map(_.takenCtr))
    val providerOldUsefulCtr = Mux1H(providerOH, allTableTagMatchResults.map(_.usefulCtr))
    val altOldTakenCtr       = Mux1H(altOH, allTableTagMatchResults.map(_.takenCtr))
    val pred                 = providerOldTakenCtr.isPositive
    val altPred              = altOldTakenCtr.isPositive
    val taken                = branch.bits.taken
    val providerNewTakenCtr  = providerOldTakenCtr.getUpdate(taken)
    val altNewTakenCtr       = altOldTakenCtr.getUpdate(taken)
    val providerNewUsefulCtr = providerOldUsefulCtr.getIncrease(pred === taken && pred =/= altPred)

    val increaseUseAlt = hasProvider && providerOldTakenCtr.isWeak && altPred === taken
    val decreaseUseAlt = hasProvider && providerOldTakenCtr.isWeak && altPred =/= taken

    val updateInfo = Wire(new UpdateInfoPerBranch)
    updateInfo.providerOH                 := providerOH.asUInt & Fill(NumTables, hasProvider)
    updateInfo.providerHitWayMaskOH       := Mux1H(providerOH, allTableTagMatchResults.map(_.hitWayMaskOH))
    updateInfo.providerNewTakenCtr.value  := providerNewTakenCtr
    updateInfo.providerNewUsefulCtr.value := providerNewUsefulCtr
    updateInfo.providerTag                := Mux1H(providerOH, allTableTagMatchResults.map(_.tag))
    updateInfo.altOH                      := altOH.asUInt & Fill(NumTables, hasAlt)
    updateInfo.altHitWayMaskOH            := Mux1H(altOH, allTableTagMatchResults.map(_.hitWayMaskOH))
    updateInfo.altNewTakenCtr.value       := altNewTakenCtr
    updateInfo.altOldUsefulCtr            := Mux1H(altOH, allTableTagMatchResults.map(_.usefulCtr))
    updateInfo.altTag                     := Mux1H(altOH, allTableTagMatchResults.map(_.tag))
    updateInfo.needAllocate   := branch.valid && branch.bits.mispredict && branch.bits.attribute.isConditional
    updateInfo.increaseUseAlt := increaseUseAlt
    updateInfo.decreaseUseAlt := decreaseUseAlt
    updateInfo
  }

  useAltCtrVec.zipWithIndex.map { case (ctr, i) =>
    val increase = t2_branchesUseAltIdx.zip(t2_allBranchUpdateInfo).map { case (useAltIdx, updateInfo) =>
      useAltIdx === i.U && updateInfo.increaseUseAlt
    }.reduce(_ || _) && t2_fire
    val decrease = t2_branchesUseAltIdx.zip(t2_allBranchUpdateInfo).map { case (useAltIdx, updateInfo) =>
      useAltIdx === i.U && updateInfo.decreaseUseAlt
    }.reduce(_ || _) && t2_fire

    when(increase)(ctr.increase())
      .elsewhen(decrease)(ctr.decrease())
  }

  private val t2_updateMask            = Wire(Vec(NumTables, Vec(NumWays, Bool())))
  private val t2_updateWriteEntries    = Wire(Vec(NumTables, Vec(NumWays, new TageEntry)))
  private val t2_updateWriteUsefulCtrs = Wire(Vec(NumTables, Vec(NumWays, new SaturateCounter(UsefulCtrWidth))))

  dontTouch(t2_updateWriteEntries)
  dontTouch(t2_updateWriteUsefulCtrs)

  t2_updateMask.zip(t2_updateWriteEntries).zip(t2_updateWriteUsefulCtrs).zipWithIndex.map {
    case (((updateEnPerTable, entriesPerTable), usefulCtrsPerTable), tableIdx) =>
      updateEnPerTable.zip(entriesPerTable).zip(usefulCtrsPerTable).zipWithIndex.map {
        case (((updateEn, entry), usefulCtr), wayIdx) =>
          val hitBranchProviderMask = t2_allBranchUpdateInfo.map { branch =>
            branch.providerOH(tableIdx) && branch.providerHitWayMaskOH(wayIdx)
          }
          val hitBranchAltMask = t2_allBranchUpdateInfo.map { branch =>
            branch.altOH(tableIdx) && branch.altHitWayMaskOH(wayIdx)
          }
          val hitBranchProvider = hitBranchProviderMask.reduce(_ || _)
          val hitBranchAlt      = hitBranchAltMask.reduce(_ || _)
          updateEn    := hitBranchProvider || hitBranchAlt
          entry.valid := true.B
          entry.tag := Mux(
            hitBranchProvider,
            Mux1H(hitBranchProviderMask, t2_allBranchUpdateInfo.map(_.providerTag)),
            Mux1H(hitBranchAltMask, t2_allBranchUpdateInfo.map(_.altTag))
          )
          entry.takenCtr := Mux(
            hitBranchProvider,
            Mux1H(hitBranchProviderMask, t2_allBranchUpdateInfo.map(_.providerNewTakenCtr)),
            Mux1H(hitBranchAltMask, t2_allBranchUpdateInfo.map(_.altNewTakenCtr))
          )
          usefulCtr := Mux(
            hitBranchProvider,
            Mux1H(hitBranchProviderMask, t2_allBranchUpdateInfo.map(_.providerNewUsefulCtr)),
            Mux1H(hitBranchAltMask, t2_allBranchUpdateInfo.map(_.altOldUsefulCtr))
          )
      }
  }

  private val t2_needAllocateBranchMask = t2_allBranchUpdateInfo.map(_.needAllocate)
  when(t2_fire)(assert(PopCount(t2_needAllocateBranchMask) <= 1.U))
  private val t2_needAllocate             = t2_needAllocateBranchMask.reduce(_ || _)
  private val t2_allocateBranch           = Mux1H(t2_needAllocateBranchMask, t2_branches)
  private val t2_allocateBranchProviderOH = Mux1H(t2_needAllocateBranchMask, t2_allBranchUpdateInfo.map(_.providerOH))
  private val t2_longerHistoryTableMask = (~((t2_allocateBranchProviderOH - 1.U) | t2_allocateBranchProviderOH)).asUInt

  dontTouch(t2_allocateBranchProviderOH)
  dontTouch(t2_longerHistoryTableMask)

  // only allocate new entry to tables that have not useful entry
  private val t2_allTableNotUsefulWayMask = t2_allTableUsefulCtrs.map { ctrs =>
    ctrs.map(ctr => ctr.value === 0.U).asUInt
  }
  private val t2_hasNotUsefulTableMask = t2_allTableNotUsefulWayMask.map(_.orR).asUInt

  private val t2_canAllocateTableMask =
    Mux(
      t2_allocateBranchProviderOH.orR,
      t2_longerHistoryTableMask & t2_hasNotUsefulTableMask,
      t2_hasNotUsefulTableMask
    )

  private val t2_resetUseful = t2_fire && usefulResetCtr.isSaturatePositive
  when(t2_resetUseful) {
    usefulResetCtr.resetZero()
  }.elsewhen(t2_fire && t2_needAllocate && !t2_canAllocateTableMask.orR) {
    usefulResetCtr.increase()
  }

  private val t2_allocate            = t2_needAllocate && t2_canAllocateTableMask.orR
  private val t2_allocateTableMaskOH = PriorityEncoderOH(t2_canAllocateTableMask) & Fill(NumTables, t2_allocate)
  private val t2_allocateWayMaskOH   = PriorityEncoderOH(Mux1H(t2_allocateTableMaskOH, t2_allTableNotUsefulWayMask))
  private val t2_allocateMask = t2_allocateTableMaskOH.asBools.map { tableEn =>
    t2_allocateWayMaskOH.asBools.map(wayEn => tableEn && wayEn)
  }

  dontTouch(t2_allocateTableMaskOH)
  dontTouch(t2_allocateWayMaskOH)

  private val t2_allocateWriteEntry = Wire(new TageEntry)
  t2_allocateWriteEntry.valid := true.B
  t2_allocateWriteEntry.tag   := Mux1H(t2_allocateTableMaskOH, t2_tempTag) ^ t2_allocateBranch.bits.cfiPosition
  t2_allocateWriteEntry.takenCtr.value := Mux(
    t2_allocateBranch.bits.taken,
    (1 << (TakenCtrWidth - 1)).U,    // weak taken
    (1 << (TakenCtrWidth - 1) - 1).U // weak not taken
  )

  tables.zipWithIndex.foreach { case (table, tableIdx) =>
    table.io.writeReq.valid := t2_fire && (t2_updateMask(tableIdx).reduce(_ || _) || t2_allocateTableMaskOH(tableIdx))
    table.io.writeReq.bits.setIdx   := t2_setIdx(tableIdx)
    table.io.writeReq.bits.bankMask := t2_bankMask

    val writeEntries    = Wire(Vec(NumWays, new TageEntry))
    val writeUsefulCtrs = Wire(Vec(NumWays, new SaturateCounter(UsefulCtrWidth)))
    val writeWayMask    = Wire(Vec(NumWays, Bool()))

    writeEntries.zip(writeUsefulCtrs).zip(t2_updateMask(tableIdx)).zip(t2_allocateMask(tableIdx)).zipWithIndex.foreach {
      case ((((entry, usefulCtr), update), allocate), wayIdx) =>
        entry                := Mux(allocate, t2_allocateWriteEntry, t2_updateWriteEntries(tableIdx)(wayIdx))
        usefulCtr.value      := Mux(allocate, UsefulCtrInitValue.U, t2_updateWriteUsefulCtrs(tableIdx)(wayIdx).value)
        writeWayMask(wayIdx) := allocate || update
    }

    table.io.writeReq.bits.wayMask    := writeWayMask.asUInt
    table.io.writeReq.bits.entries    := writeEntries
    table.io.writeReq.bits.usefulCtrs := writeUsefulCtrs

    table.io.resetUseful := t2_fire && t2_resetUseful
  }

  /* --------------------------------------------------------------------------------------------------------------
     performance counter
     -------------------------------------------------------------------------------------------------------------- */

  XSPerfAccumulate("predict_cond", Mux(io.stageCtrl.s2_fire, PopCount(s2_mbtbHitCondMask), 0.U))
  XSPerfAccumulate("predict_cond_use_table", Mux(io.stageCtrl.s2_fire, PopCount(s2_condHitTableMask), 0.U))

  XSPerfAccumulate("wrong_path_train", t0_wrongPathTrain)

  XSPerfAccumulate("total_train", io.train.valid)
  XSPerfAccumulate("total_train_has_cond", io.train.valid && t0_hasConditionalBranch)
  XSPerfAccumulate("actual_train", t0_fire)

  XSPerfAccumulate("mispredict_branch_has_provider", t2_fire && t2_allocateBranchProviderOH.orR)
  for (i <- 0 until NumTables) {
    XSPerfAccumulate(
      s"mispredict_branch_provider_is_table_${i}",
      t2_fire && t2_allocateBranchProviderOH.orR && t2_allocateBranchProviderOH(i)
    )
  }

  XSPerfAccumulate("read_conflict", t0_readBankConflict)
  XSPerfAccumulate("reset_useful", t2_fire && t2_resetUseful)
  XSPerfAccumulate("need_allocate", t2_fire && t2_needAllocate)
  XSPerfAccumulate("allocate_success", t2_fire && t2_allocateTableMaskOH.orR)
  for (i <- 0 until NumTables) {
    XSPerfAccumulate(s"table_${i}_allocate", t2_fire && t2_allocateTableMaskOH(i))
  }
  // TODO: add more
}

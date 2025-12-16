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
import utility.ChiselDB
import utility.DataHoldBypass
import utility.XSPerfAccumulate
import utility.XSPerfHistogram
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.HalfAlignHelper
import xiangshan.frontend.bpu.Prediction
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.TageTableInfo
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

/**
 * This module is the implementation of the TAGE (TAgged GEometric history length predictor).
 */
class Tage(implicit p: Parameters) extends BasePredictor with HasTageParameters with TopHelper with HalfAlignHelper {
  class TageIO(implicit p: Parameters) extends BasePredictorIO {
    val foldedPathHist:         PhrAllFoldedHistories  = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val foldedPathHistForTrain: PhrAllFoldedHistories  = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val mbtbResult:             Vec[Valid[Prediction]] = Input(Vec(NumBtbResultEntries, Valid(new Prediction)))

    val takenMask:   Vec[Bool] = Output(Vec(NumBtbResultEntries, Bool()))
    val hasProvided: Vec[Bool] = Output(Vec(NumBtbResultEntries, Bool()))
    val providerTakenCtrVec: Vec[Valid[SaturateCounter]] =
      Output(Vec(NumBtbResultEntries, Valid(new SaturateCounter(TakenCtrWidth))))

    val meta: TageMeta = Output(new TageMeta)
  }
  val io: TageIO = IO(new TageIO)

  /* *** submodules *** */
  private val tables = TableInfos.zipWithIndex.map { case (info, i) => Module(new TageTable(i, info)) }

  // reset usefulCtr of all entries when usefulResetCtr saturated
  private val usefulResetCtr = RegInit(0.U.asTypeOf(new SaturateCounter(UsefulResetCtrWidth)))

  // use altPred when useAltCtr is positive
  private val useAltCtrVec = RegInit(VecInit.fill(NumUseAltCtrs)(0.U.asTypeOf(new SaturateCounter(UseAltCtrWidth))))

  /* *** reset *** */
  private val resetDone = RegInit(false.B)
  when(tables.map(_.io.resetDone).reduce(_ && _)) {
    resetDone := true.B
  }
  io.resetDone := resetDone

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 0
     - send read request to tables
     -------------------------------------------------------------------------------------------------------------- */

  private val s0_fire    = io.stageCtrl.s0_fire && io.enable
  private val s0_startPc = io.startPc

  private val s0_foldedHist = getFoldedHist(io.foldedPathHist)
  private val s0_setIdx = VecInit((tables zip s0_foldedHist).map { case (table, hist) =>
    table.getSetIndex(s0_startPc, hist.forIdx)
  })

  // currently all tables share the same bank index
  private val s0_bankIdx  = tables.head.getBankIndex(s0_startPc)
  private val s0_bankMask = UIntToOH(s0_bankIdx, NumBanks)

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

  private val s1_fire    = io.stageCtrl.s1_fire && io.enable
  private val s1_startPc = RegEnable(s0_startPc, s0_fire)

  // TODO: remove it
  private val s1_setIdx = RegEnable(s0_setIdx, s0_fire)

  private val s1_allTableEntries = DataHoldBypass(VecInit(tables.map(_.io.predictReadResp.entries)), RegNext(s0_fire))
  private val s1_allTableUsefulCtrs =
    DataHoldBypass(VecInit(tables.map(_.io.predictReadResp.usefulCtrs)), RegNext(s0_fire))

  private val s1_foldedHist = RegEnable(s0_foldedHist, s0_fire)
  // A tag without branch position, position will be hashed into after BTB result
  private val s1_rawTag = VecInit((tables zip s1_foldedHist).map { case (table, hist) =>
    table.getRawTag(s1_startPc, hist.forTag)
  })

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 2
     - get results from mbtb
     - get prediction for each branch
     -------------------------------------------------------------------------------------------------------------- */

  private val s2_valid              = RegNext(s1_fire) && io.enable
  private val s2_allTableEntries    = RegEnable(s1_allTableEntries, s1_fire)
  private val s2_allTableUsefulCtrs = RegEnable(s1_allTableUsefulCtrs, s1_fire)

  private val s2_startPc = RegEnable(s1_startPc, s1_fire)
  dontTouch(s2_startPc)

  private val s2_setIdx = RegEnable(s1_setIdx, s1_fire)
  private val s2_rawTag = RegEnable(s1_rawTag, s1_fire)

  private val s2_branches = io.mbtbResult

  // generate prediction
  private val s2_takenMask   = Wire(Vec(NumBtbResultEntries, Bool()))
  private val s2_hasProvided = Wire(Vec(NumBtbResultEntries, Bool()))
  // to sc
  private val s2_providerTakenCtrVec = Wire(Vec(NumBtbResultEntries, Valid(new SaturateCounter(TakenCtrWidth))))

  s2_branches.zipWithIndex.foreach { case (branch, i) =>
    val isCond    = branch.valid && branch.bits.attribute.isConditional
    val position  = branch.bits.cfiPosition
    val cfiPc     = getCfiPcFromPosition(s2_startPc, position)
    val useAltIdx = getUseAltIndex(cfiPc)

    // compare tags of each branch with all tables
    val allTableTagMatchResults = s2_allTableEntries.zip(s2_allTableUsefulCtrs).zipWithIndex.map {
      case ((entriesPerTable, usefulCtrsPerTable), tableIdx) =>
        val tag          = s2_rawTag(tableIdx) ^ position
        val hitWayMask   = entriesPerTable.map(entry => entry.valid && entry.tag === tag)
        val hitWayMaskOH = PriorityEncoderOH(hitWayMask)

        val result = Wire(new TagMatchResult).suggestName(s"s2_branch_${i}_table_${tableIdx}_result")
        result.hit          := isCond && hitWayMask.reduce(_ || _)
        result.entry        := Mux1H(hitWayMaskOH, entriesPerTable)
        result.usefulCtr    := Mux1H(hitWayMaskOH, usefulCtrsPerTable)
        result.hitWayMaskOH := hitWayMaskOH.asUInt
        result.hitWayMask   := hitWayMask.asUInt
        result
    }
    // find the provider, the table with the longest history among the hit tables
    val hitTableMask     = allTableTagMatchResults.map(_.hit)
    val hasProvider      = hitTableMask.reduce(_ || _)
    val providerTableOH  = getLongestHistTableOH(hitTableMask)
    val providerTakenCtr = Mux1H(providerTableOH, allTableTagMatchResults.map(_.entry.takenCtr))

    s2_providerTakenCtrVec(i).valid      := hasProvider
    s2_providerTakenCtrVec(i).bits.value := providerTakenCtr.value

    // find the alt, the table with the second longest history among the hit tables
    val hitTableMaskNoProvider = hitTableMask.zip(providerTableOH).map { case (a, b) => a && !b }
    val hasAlt                 = hasProvider && hitTableMaskNoProvider.reduce(_ || _)
    val altTableOH             = getLongestHistTableOH(hitTableMaskNoProvider)
    val altTakenCtr            = Mux1H(altTableOH, allTableTagMatchResults.map(_.entry.takenCtr))

    val providerIsWeak = providerTakenCtr.isWeak
    val pred           = providerTakenCtr.isPositive
    val altPred        = altTakenCtr.isPositive
    val useAlt         = providerIsWeak && hasAlt && useAltCtrVec(useAltIdx).isPositive

    XSPerfAccumulate(
      s"s2_branch_${i}_multihit_on_same_way",
      allTableTagMatchResults.map(e => (s2_valid && PopCount(e.hitWayMask) > 1.U).asUInt).reduce(_ +& _)
    )

    // get prediction for each branch
    s2_takenMask(i)   := Mux(useAlt, altPred, pred)
    s2_hasProvided(i) := hasProvider
  }

  io.takenMask           := s2_takenMask
  io.hasProvided         := s2_hasProvided
  io.providerTakenCtrVec := s2_providerTakenCtrVec

  io.meta.debug_setIdx  := s2_setIdx
  io.meta.debug_tempTag := s2_rawTag

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 0
     - send train request to base table
     - send read request to tables
     -------------------------------------------------------------------------------------------------------------- */

  private val t0_startPc  = io.train.bits.startPc
  private val t0_branches = io.train.bits.branches

  // currently all tables share the same bank index
  private val t0_bankIdx  = tables.head.getBankIndex(t0_startPc)
  private val t0_bankMask = UIntToOH(t0_bankIdx, NumBanks)

  private val t0_condMask = VecInit(t0_branches.map(branch => branch.valid && branch.bits.attribute.isConditional))
  private val t0_hasCond  = t0_condMask.reduce(_ || _)

  private val t0_readBankConflict = io.train.valid && t0_hasCond && s0_fire && t0_bankIdx === s0_bankIdx
  io.train.ready := true.B

  private val t0_valid = io.train.fire && t0_hasCond && io.enable

  private val t0_mbtbMeta = io.train.bits.meta.mbtb.entries.flatten
  private val t0_basePred = VecInit(t0_branches.map { branch =>
    Mux1H(t0_mbtbMeta.map(_.hit(branch.bits)), t0_mbtbMeta.map(_.counter.isPositive))
  })

  private val t0_foldedHist = getFoldedHist(io.foldedPathHistForTrain)
  private val t0_setIdx = VecInit((tables zip t0_foldedHist).map { case (table, hist) =>
    table.getSetIndex(t0_startPc, hist.forIdx)
  })
  dontTouch(t0_setIdx)

  // only for perf
  private val t0_readBankConflictReg     = RegNext(t0_readBankConflict)
  private val t0_readBankConflictPos     = t0_readBankConflict && (!t0_readBankConflictReg)
  private val t0_readBankConflictNeg     = !t0_readBankConflict && t0_readBankConflictReg
  private val t0_readBankConflictDistCnt = RegInit(0.U(4.W))
  private val perf_s0AlignedPc           = getAlignedPc(s0_startPc)
  private val perf_s1AlignedPc           = getAlignedPc(s1_startPc)
  private val perf_s1BankIdx             = RegEnable(s0_bankIdx, s0_fire)
  // pred target within align 64B,and not blocked by s2
  private val t0_readBankConflictShortLoop = t0_readBankConflictReg && s1_fire &&
    (perf_s1BankIdx === s0_bankIdx) &&
    (perf_s0AlignedPc.toUInt - perf_s1AlignedPc.toUInt <= FetchBlockSize.U ||
      perf_s1AlignedPc.toUInt - perf_s0AlignedPc.toUInt <= FetchBlockSize.U) && s0_fire
  private val t0_readBankConflictShortLoopReg     = RegNext(t0_readBankConflictShortLoop)
  private val t0_readBankConflictShortLoopNeg     = !t0_readBankConflictShortLoop & t0_readBankConflictShortLoopReg
  private val t0_readBankConflictShortLoopdistCnt = RegInit(0.U(4.W))
  // dist cnt
  t0_readBankConflictShortLoopdistCnt := Mux(
    t0_readBankConflictShortLoopNeg,
    0.U,
    Mux(t0_readBankConflictShortLoop, t0_readBankConflictShortLoopdistCnt + 1.U, t0_readBankConflictShortLoopdistCnt)
  )

  t0_readBankConflictDistCnt := Mux(
    t0_readBankConflictNeg,
    0.U,
    Mux(t0_readBankConflict, t0_readBankConflictDistCnt + 1.U, t0_readBankConflictDistCnt)
  )

//  when(t0_valid) {
//    assert(t0_setIdx === io.train.bits.meta.tage.debug_setIdx, "predict setIdx != train setIdx")
//  }

  tables.zipWithIndex.foreach { case (table, tableIdx) =>
    table.io.trainReadReq.valid         := t0_valid
    table.io.trainReadReq.bits.setIdx   := t0_setIdx(tableIdx)
    table.io.trainReadReq.bits.bankMask := t0_bankMask
  }

//  when(t0_valid) {
//    assert(!(s0_fire && s0_bankIdx === t0_bankIdx), "TageTable: predictReadReq and trainReadReq conflict")
//  }

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 1
     - get read data from tables
     - compute temp tag
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_valid    = RegNext(t0_valid) && io.enable
  private val t1_startPc  = RegEnable(t0_startPc, t0_valid)
  private val t1_branches = RegEnable(t0_branches, t0_valid)
  private val t1_condMask = RegEnable(t0_condMask, t0_valid)

  private val t1_setIdx   = RegEnable(t0_setIdx, t0_valid)
  private val t1_bankMask = RegEnable(t0_bankMask, t0_valid)

  private val t1_allTableEntries    = VecInit(tables.map(_.io.trainReadResp.entries))
  private val t1_allTableUsefulCtrs = VecInit(tables.map(_.io.trainReadResp.usefulCtrs))

  private val t1_basePred = RegEnable(t0_basePred, t0_valid)

  private val t1_foldedHist = RegEnable(t0_foldedHist, t0_valid)
  private val t1_rawTag = VecInit((tables zip t1_foldedHist).map { case (table, hist) =>
    table.getRawTag(t1_startPc, hist.forTag)
  })

  private val t1_debugTempTag = RegEnable(io.train.bits.meta.tage.debug_tempTag, t0_valid)
//  when(t1_valid) {
//    assert(t1_rawTag === t1_debugTempTag, "predict tag != train tag")
//  }

  private val t1_cfiPcVec =
    VecInit(t1_branches.map(branch => getCfiPcFromPosition(t1_startPc, branch.bits.cfiPosition)))
  private val t1_cfiUseAltIdxVec = VecInit(t1_cfiPcVec.map(getUseAltIndex))

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 2
     - update branches' takenCtr and usefulCtr
     - allocate a new entry when mispredict
     -------------------------------------------------------------------------------------------------------------- */

  private val t2_valid    = RegNext(t1_valid) && io.enable
  private val t2_branches = RegEnable(t1_branches, t1_valid)
  private val t2_condMask = RegEnable(t1_condMask, t1_valid)

  private val t2_startPc = RegEnable(t1_startPc, t1_valid)
  dontTouch(t2_startPc)

  private val t2_setIdx   = RegEnable(t1_setIdx, t1_valid)
  private val t2_bankMask = RegEnable(t1_bankMask, t1_valid)

  private val t2_allTableEntries    = RegEnable(t1_allTableEntries, t1_valid)
  private val t2_allTableUsefulCtrs = RegEnable(t1_allTableUsefulCtrs, t1_valid)

  private val t2_rawTag = RegEnable(t1_rawTag, t1_valid)

  private val t2_basePred = RegEnable(t1_basePred, t1_valid)

  private val t2_cfiPcVec        = RegEnable(t1_cfiPcVec, t1_valid)
  private val t2_cfiUseAltIdxVec = RegEnable(t1_cfiUseAltIdxVec, t1_valid)
  dontTouch(t2_cfiPcVec)

  private val t2_allBranchUpdateInfo = t2_branches.zipWithIndex.map { case (branch, brIdx) =>
    val isCond = t2_condMask(brIdx)
    val allTableTagMatchResults = t2_allTableEntries.zip(t2_allTableUsefulCtrs).zipWithIndex.map {
      case ((entriesPerTable, usefulCtrsPerTable), tableIdx) =>
        val tag          = t2_rawTag(tableIdx) ^ branch.bits.cfiPosition
        val hitWayMask   = entriesPerTable.map(entry => entry.valid && entry.tag === tag)
        val hitWayMaskOH = PriorityEncoderOH(hitWayMask)
        dontTouch(tag.suggestName(s"branch_${brIdx}_table_${tableIdx}_tag"))

        val result = Wire(new TagMatchResult).suggestName(s"t2_branch_${brIdx}_table_${tableIdx}_result")
        result.hit          := isCond && hitWayMask.reduce(_ || _)
        result.entry        := Mux1H(hitWayMaskOH, entriesPerTable)
        result.usefulCtr    := Mux1H(hitWayMaskOH, usefulCtrsPerTable)
        result.hitWayMaskOH := hitWayMaskOH.asUInt
        result.hitWayMask   := hitWayMask.asUInt
        result
    }
    val hitTableMask    = allTableTagMatchResults.map(_.hit)
    val hasProvider     = hitTableMask.reduce(_ || _)
    val providerTableOH = getLongestHistTableOH(hitTableMask)
    val providerInfo    = Mux1H(providerTableOH, allTableTagMatchResults)
    dontTouch(hitTableMask.asUInt.suggestName(s"t2_branch_${brIdx}_hitTableMask"))

    val hitTableMaskNoProvider = hitTableMask.zip(providerTableOH).map { case (a, b) => a && !b }
    val hasAlt                 = hasProvider && hitTableMaskNoProvider.reduce(_ || _)
    val altTableOH             = getLongestHistTableOH(hitTableMaskNoProvider)
    val altInfo                = Mux1H(altTableOH, allTableTagMatchResults)
    dontTouch(hitTableMaskNoProvider.asUInt.suggestName(s"t2_branch_${brIdx}_hitTableMaskNoProvider"))

    val pred           = providerInfo.entry.takenCtr.isPositive
    val providerIsWeak = providerInfo.entry.takenCtr.isWeak
    val altPred        = altInfo.entry.takenCtr.isPositive
    val basePred       = t2_basePred(brIdx)
    val useAlt         = providerIsWeak && hasAlt && useAltCtrVec(t2_cfiUseAltIdxVec(brIdx)).isPositive
    val finalPred      = Mux(useAlt, altPred, Mux(hasProvider, pred, basePred))
    val actualTaken    = branch.bits.taken

    XSPerfAccumulate(
      s"t2_branch_${brIdx}_mispredict_diff",
      t2_valid && branch.valid && isCond && ((finalPred =/= actualTaken) =/= branch.bits.mispredict)
    )

    val providerNewTakenCtr       = providerInfo.entry.takenCtr.getUpdate(actualTaken)
    val increaseProviderUsefulCtr = hasProvider && pred === actualTaken && pred =/= Mux(hasAlt, altPred, basePred)
    val providerNewUsefulCtr = Mux(
      increaseProviderUsefulCtr,
      providerInfo.usefulCtr.getIncrease,
      providerInfo.usefulCtr.value
    )
    val altNewTakenCtr = altInfo.entry.takenCtr.getUpdate(actualTaken)

    val increaseUseAlt = hasProvider && providerIsWeak && Mux(hasAlt, altPred, basePred) === actualTaken
    val decreaseUseAlt = hasProvider && providerIsWeak && Mux(hasAlt, altPred, basePred) =/= actualTaken

    val updateInfo = Wire(new UpdateInfo).suggestName(s"branch_${brIdx}_updateInfo")
    updateInfo.valid                        := isCond // Only consider update if conditional branch
    updateInfo.providerTableOH              := providerTableOH.asUInt & Fill(NumTables, hasProvider)
    updateInfo.providerWayOH                := providerInfo.hitWayMaskOH
    updateInfo.providerEntry.valid          := true.B
    updateInfo.providerEntry.tag            := providerInfo.entry.tag
    updateInfo.providerEntry.takenCtr.value := providerNewTakenCtr
    updateInfo.providerOldUsefulCtr         := providerInfo.usefulCtr
    updateInfo.providerNewUsefulCtr.value   := providerNewUsefulCtr

    updateInfo.altTableOH              := altTableOH.asUInt & Fill(NumTables, useAlt)
    updateInfo.altWayOH                := altInfo.hitWayMaskOH
    updateInfo.altEntry.valid          := true.B
    updateInfo.altEntry.tag            := altInfo.entry.tag
    updateInfo.altEntry.takenCtr.value := altNewTakenCtr
    updateInfo.altOldUsefulCtr         := altInfo.usefulCtr

    updateInfo.useAlt    := useAlt
    updateInfo.finalPred := finalPred

    updateInfo.needAllocate := isCond && finalPred =/= actualTaken && !(hasProvider && providerTableOH(NumTables - 1))
    updateInfo.notNeedUpdate := providerInfo.entry.takenCtr.shouldHold(actualTaken) &&
      providerInfo.usefulCtr.isSaturatePositive && increaseProviderUsefulCtr &&
      (!useAlt || altInfo.entry.takenCtr.shouldHold(actualTaken))

    updateInfo.increaseUseAlt := increaseUseAlt
    updateInfo.decreaseUseAlt := decreaseUseAlt
    updateInfo.hitTableMask   := hitTableMask.asUInt
    updateInfo.mispredicted   := finalPred =/= actualTaken
    updateInfo
  }

  useAltCtrVec.zipWithIndex.map { case (ctr, i) =>
    val idxMatchMask = t2_cfiUseAltIdxVec.map(_ === i.U)
    val increase = idxMatchMask.zip(t2_allBranchUpdateInfo).map { case (idxMatch, updateInfo) =>
      idxMatch && updateInfo.increaseUseAlt
    }.reduce(_ || _)
    val decrease = idxMatchMask.zip(t2_allBranchUpdateInfo).map { case (idxMatch, updateInfo) =>
      idxMatch && updateInfo.decreaseUseAlt
    }.reduce(_ || _)

    when(t2_valid && increase) {
      ctr.increase()
    }.elsewhen(t2_valid && decrease) {
      ctr.decrease()
    }
  }

  private val t2_needAllocate         = t2_allBranchUpdateInfo.map(_.needAllocate).reduce(_ || _)
  private val t2_mispredictBranchOH   = PriorityEncoderOH(t2_allBranchUpdateInfo.map(b => b.valid && b.mispredicted))
  private val t2_needAllocateBranchOH = PriorityEncoderOH(t2_allBranchUpdateInfo.map(_.needAllocate))
  private val t2_allocateBranch       = Mux1H(t2_needAllocateBranchOH, t2_branches)
  private val t2_mispredictBranchUpdateInfo = Mux1H(t2_mispredictBranchOH, t2_allBranchUpdateInfo)
  private val t2_allocateBranchProviderTableOH =
    Mux1H(t2_needAllocateBranchOH, t2_allBranchUpdateInfo.map(_.providerTableOH))

  // allocate new entry to the table with a longer history
  private val t2_longerHistoryTableMask = Mux(
    t2_allocateBranchProviderTableOH.orR, // has provider
    (~((t2_allocateBranchProviderTableOH - 1.U) | t2_allocateBranchProviderTableOH)).asUInt,
    Fill(NumTables, true.B)
  )
  dontTouch(t2_longerHistoryTableMask)

  private val t2_allTableCanAllocateWayMask =
    t2_allTableEntries.zip(t2_allTableUsefulCtrs).map { case (entriesPerTable, ctrsPerTable) =>
      entriesPerTable.zip(ctrsPerTable).map { case (entry, usefulCtr) =>
        !entry.valid || entry.valid && entry.takenCtr.isWeak && usefulCtr.value === 0.U
      }.asUInt
    }
  private val t2_canAllocateTableMask = t2_longerHistoryTableMask & t2_allTableCanAllocateWayMask.map(_.orR).asUInt
  private val t2_canAllocate          = t2_canAllocateTableMask.orR
  private val t2_allocate             = t2_needAllocate && t2_canAllocate
  private val t2_usefulReset          = t2_valid && usefulResetCtr.isSaturatePositive

  private val t2_allocateTableMaskOH = PriorityEncoderOH(t2_canAllocateTableMask) & Fill(NumTables, t2_allocate)
  private val t2_allocateWayMaskOH   = PriorityEncoderOH(Mux1H(t2_allocateTableMaskOH, t2_allTableCanAllocateWayMask))
  dontTouch(t2_allocateTableMaskOH)
  dontTouch(t2_allocateWayMaskOH)

  private val t2_allocateEntry = Wire(new TageEntry)
  t2_allocateEntry.valid := true.B
  t2_allocateEntry.tag   := Mux1H(t2_allocateTableMaskOH, t2_rawTag) ^ t2_allocateBranch.bits.cfiPosition
  t2_allocateEntry.takenCtr.value := Mux(
    t2_allocateBranch.bits.taken,
    (1 << (TakenCtrWidth - 1)).U,      // weak taken
    ((1 << (TakenCtrWidth - 1)) - 1).U // weak not taken
  )

  tables.zipWithIndex.foreach { case (table, tableIdx) =>
    implicit val info: TageTableInfo = TableInfos(tableIdx) // used by NumWays

    val updateMask       = Wire(Vec(NumWays, Bool()))
    val updateEntries    = Wire(Vec(NumWays, new TageEntry))
    val updateUsefulCtrs = Wire(Vec(NumWays, new SaturateCounter(UsefulCtrWidth)))

    updateMask.zip(updateEntries).zip(updateUsefulCtrs).zipWithIndex.foreach {
      case (((updateEn, entry), usefulCtr), wayIdx) =>
        val hitBranchProviderMask = t2_allBranchUpdateInfo.map { branch =>
          !branch.notNeedUpdate && branch.providerTableOH(tableIdx) && branch.providerWayOH(wayIdx)
        }
        val hitBranchAltMask = t2_allBranchUpdateInfo.map { branch =>
          !branch.notNeedUpdate && branch.altTableOH(tableIdx) && branch.altWayOH(wayIdx)
        }
        val hitBranchProvider  = hitBranchProviderMask.reduce(_ || _)
        val hitBranchAlt       = hitBranchAltMask.reduce(_ || _)
        val providerUpdateInfo = Mux1H(hitBranchProviderMask, t2_allBranchUpdateInfo)
        val altUpdateInfo      = Mux1H(hitBranchAltMask, t2_allBranchUpdateInfo)
        updateEn  := hitBranchProvider || hitBranchAlt
        entry     := Mux(hitBranchProvider, providerUpdateInfo.providerEntry, altUpdateInfo.altEntry)
        usefulCtr := Mux(hitBranchProvider, providerUpdateInfo.providerNewUsefulCtr, altUpdateInfo.altOldUsefulCtr)
    }

    val thisTableNeedUpdate   = updateMask.reduce(_ || _)
    val thisTableNeedAllocate = t2_allocateTableMaskOH(tableIdx)
    table.io.writeReq.valid         := t2_valid && (thisTableNeedUpdate || thisTableNeedAllocate)
    table.io.writeReq.bits.setIdx   := t2_setIdx(tableIdx)
    table.io.writeReq.bits.bankMask := t2_bankMask

    val writeEntries    = Wire(Vec(NumWays, new TageEntry))
    val writeUsefulCtrs = Wire(Vec(NumWays, new SaturateCounter(UsefulCtrWidth)))
    val writeWayMask    = Wire(Vec(NumWays, Bool()))

    writeEntries.zip(writeUsefulCtrs).zipWithIndex.foreach { case ((entry, usefulCtr), wayIdx) =>
      val thisWayNeedUpdate   = updateMask(wayIdx)
      val thisWayNeedAllocate = thisTableNeedAllocate && t2_allocateWayMaskOH(wayIdx)
      entry                := Mux(thisWayNeedAllocate, t2_allocateEntry, updateEntries(wayIdx))
      usefulCtr.value      := Mux(thisWayNeedAllocate, UsefulCtrInitValue.U, updateUsefulCtrs(wayIdx).value)
      writeWayMask(wayIdx) := thisWayNeedUpdate || thisWayNeedAllocate
    }

    table.io.writeReq.bits.wayMask    := writeWayMask.asUInt
    table.io.writeReq.bits.entries    := writeEntries
    table.io.writeReq.bits.usefulCtrs := writeUsefulCtrs

    table.io.resetUseful := t2_usefulReset
  }

  when(t2_usefulReset) {
    usefulResetCtr.resetZero()
  }.elsewhen(t2_valid && t2_needAllocate && !t2_canAllocate) {
    usefulResetCtr.increase()
  }

  /* --------------------------------------------------------------------------------------------------------------
     TAGE Trace
     -------------------------------------------------------------------------------------------------------------- */

  private val condTraceVec = Wire(Vec(ResolveEntryBranchNumber, Valid(new ConditionalBranchTrace)))
  condTraceVec.zipWithIndex.foreach { case (trace, i) =>
    trace.valid                  := t2_condMask(i)
    trace.bits.startPc           := t2_startPc
    trace.bits.cfiPc             := t2_cfiPcVec(i)
    trace.bits.hasProvider       := t2_allBranchUpdateInfo(i).providerTableOH.orR
    trace.bits.providerTableIdx  := OHToUInt(t2_allBranchUpdateInfo(i).providerTableOH)
    trace.bits.providerSetIdx    := t2_setIdx(trace.bits.providerTableIdx)
    trace.bits.providerWayIdx    := OHToUInt(t2_allBranchUpdateInfo(i).providerWayOH)
    trace.bits.providerTakenCtr  := t2_allBranchUpdateInfo(i).providerEntry.takenCtr
    trace.bits.providerUsefulCtr := t2_allBranchUpdateInfo(i).providerOldUsefulCtr
    trace.bits.hasAlt            := t2_allBranchUpdateInfo(i).altTableOH.orR
    trace.bits.altTableIdx       := OHToUInt(t2_allBranchUpdateInfo(i).altTableOH)
    trace.bits.altSetIdx         := t2_setIdx(trace.bits.altTableIdx)
    trace.bits.altWayIdx         := OHToUInt(t2_allBranchUpdateInfo(i).altWayOH)
    trace.bits.altTakenCtr       := t2_allBranchUpdateInfo(i).altEntry.takenCtr
    trace.bits.altUsefulCtr      := t2_allBranchUpdateInfo(i).altOldUsefulCtr
    trace.bits.useAlt            := t2_allBranchUpdateInfo(i).useAlt
    trace.bits.finalPred         := t2_allBranchUpdateInfo(i).finalPred
    trace.bits.actualTaken       := t2_branches(i).bits.taken
    trace.bits.mispredict        := t2_branches(i).bits.mispredict
    trace.bits.allocSuccess      := t2_allBranchUpdateInfo(i).needAllocate && t2_allocateTableMaskOH.orR
    trace.bits.allocTableIdx     := OHToUInt(t2_allocateTableMaskOH)
    trace.bits.allocateSetIdx    := t2_setIdx(trace.bits.allocTableIdx)
    trace.bits.allocWayIdx       := OHToUInt(t2_allocateWayMaskOH)
  }

  private val tageTraceDBTables = (0 until NumTables).map { i =>
    ChiselDB.createTable(s"CondTrace_${i}", new ConditionalBranchTrace, EnableTageTrace)
  }
  tageTraceDBTables.zip(condTraceVec).foreach { case (dbTable, condTrace) =>
    dbTable.log(
      data = condTrace.bits,
      en = t2_valid && condTrace.valid,
      clock = clock,
      reset = reset
    )
  }

  /* --------------------------------------------------------------------------------------------------------------
     performance counter
     -------------------------------------------------------------------------------------------------------------- */

  private val s2_condMask = s2_branches.map(branch => branch.valid && branch.bits.attribute.isConditional)
  XSPerfAccumulate("predict_cond", Mux(io.stageCtrl.s2_fire, PopCount(s2_condMask), 0.U))

  XSPerfAccumulate("total_train", io.train.fire)
  XSPerfAccumulate("train_has_cond", t0_valid)

  XSPerfAccumulate(
    "total_condbr_mispredicted",
    t2_allBranchUpdateInfo.map(e =>
      (t2_valid && e.valid && e.mispredicted).asUInt
    ).reduce(_ +& _)
  )
  XSPerfAccumulate(
    "total_first_condbr_mispredicted",
    t2_allBranchUpdateInfo.map(e =>
      t2_valid && e.valid && e.mispredicted
    ).reduce(_ || _)
  )
  XSPerfAccumulate(
    "total_allbr_mispredicted",
    io.train.bits.branches.map(b => (io.train.valid && b.valid && b.bits.mispredict).asUInt).reduce(_ +& _)
  )
  XSPerfAccumulate(
    "mispredict_branch_use_basetable",
    t2_allBranchUpdateInfo.map(e =>
      (t2_valid && e.valid && e.mispredicted && !e.providerTableOH.orR).asUInt
    ).reduce(_ +& _)
  )
  XSPerfAccumulate(
    "mispredict_branch_has_provider",
    t2_allBranchUpdateInfo.map(e =>
      (t2_valid && e.valid && e.mispredicted && e.providerTableOH.orR).asUInt
    ).reduce(_ +& _)
  )
  XSPerfAccumulate(
    "resolve_branch_use_basetable",
    t2_allBranchUpdateInfo.map(e =>
      (t2_valid && e.valid && !e.providerTableOH.orR).asUInt
    ).reduce(_ +& _)
  )
  XSPerfAccumulate(
    "resolve_branch_has_provider",
    t2_allBranchUpdateInfo.map(e =>
      (t2_valid && e.valid && e.providerTableOH.orR).asUInt
    ).reduce(_ +& _)
  )
  XSPerfAccumulate(
    "resolve_total_use_alt",
    t2_allBranchUpdateInfo.map(e =>
      (t2_valid && e.valid && e.useAlt).asUInt
    ).reduce(_ +& _)
  )
  for (i <- 0 until NumTables) {
    XSPerfAccumulate(
      s"allocate_branch_provider_is_table_${i}",
      t2_valid && t2_allocateBranchProviderTableOH.orR && t2_allocateBranchProviderTableOH(i)
    )
    XSPerfAccumulate(
      s"resolve_branch_hit_table_${i}",
      t2_allBranchUpdateInfo.map(e =>
        (t2_valid && e.valid && e.hitTableMask(i)).asUInt
      ).reduce(_ +& _)
    )
    XSPerfAccumulate(
      s"resolve_branch_provider_is_table_${i}",
      t2_allBranchUpdateInfo.map(e =>
        (t2_valid && e.valid && e.providerTableOH(i)).asUInt
      ).reduce(_ +& _)
    )
  }

  /*
  sum -> total bubbles caused by read bank conflict
  sampled -> total times of read bank conflict happened
   */
  XSPerfHistogram(
    "read_conflict_bubble_dist",
    t0_readBankConflictDistCnt,
    t0_readBankConflictNeg,
    0,
    16
  )
  /*
  sum -> total bubbles caused by read bank conflict within aligned 64B loop
  sampled -> total times of read bank conflict within aligned 64B loop happened
  Currently, there is an error in the short branch jump dist. It is approximately 1 time.
  e.g. The value obtained from the 7th loop of statistics may contain part of the 6th loop.
   */
  XSPerfHistogram(
    "read_conflict_loop_dist",
    t0_readBankConflictShortLoopdistCnt,
    t0_readBankConflictShortLoopNeg,
    0,
    16
  )
  XSPerfAccumulate("read_conflict", t0_readBankConflict)
  XSPerfAccumulate("reset_useful", t2_usefulReset)
  XSPerfAccumulate(
    "allocate_not_needed_due_to_already_on_highest_table",
    t2_mispredictBranchUpdateInfo.valid && t2_mispredictBranchUpdateInfo.mispredicted &&
      !t2_mispredictBranchUpdateInfo.needAllocate
  )
  XSPerfAccumulate("allocate_needed", t2_valid && t2_needAllocate)
  XSPerfAccumulate("allocate_success", t2_valid && t2_needAllocate && t2_canAllocate)
  XSPerfAccumulate("allocate_failure", t2_valid && t2_needAllocate && !t2_canAllocate)
  for (i <- 0 until NumTables) {
    XSPerfAccumulate(s"table_${i}_allocate", t2_valid && t2_allocateTableMaskOH(i))
  }
}

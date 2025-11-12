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
import utility.DataHoldBypass
import utility.XSPerfAccumulate
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BtbInfo
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

/**
 * This module is the implementation of the TAGE (TAgged GEometric history length predictor).
 */
class Tage(implicit p: Parameters) extends BasePredictor with HasTageParameters with Helpers {
  class TageIO(implicit p: Parameters) extends BasePredictorIO {
    val foldedPathHist:         PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val foldedPathHistForTrain: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val branchesFromMainBtb:    Vec[Valid[BtbInfo]]   = Input(Vec(NumBtbResultEntries, Valid(new BtbInfo)))
    val condTakenMask:          Vec[Bool]             = Output(Vec(NumBtbResultEntries, Bool()))
    val meta:                   TageMeta              = Output(new TageMeta)
    val providerTakenCtrVec: Vec[Valid[SaturateCounter]] =
      Output(Vec(NumBtbResultEntries, Valid(new SaturateCounter(TakenCtrWidth))))
  }
  val io: TageIO = IO(new TageIO)

  /* *** submodules *** */
  private val baseTable = Module(new TageBaseTable)
  private val tables    = TableInfos.map(tableInfo => Module(new TageTable(tableInfo.NumSets)))

  // reset usefulCtr of all entries when usefulResetCtr saturated
  private val usefulResetCtr = RegInit(0.U.asTypeOf(new SaturateCounter(UsefulResetCtrWidth)))

  // use altPred when useAltCtr is positive
  private val useAltCtrVec = RegInit(VecInit.fill(NumUseAltCtrs)(0.U.asTypeOf(new SaturateCounter(UseAltCtrWidth))))

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

  private val s0_foldedHist = getFoldedHist(io.foldedPathHist)
  private val s0_setIdx = VecInit(TableInfos.zip(s0_foldedHist).map { case (tableInfo, hist) =>
    getSetIndex(s0_startVAddr, hist.forIdx, tableInfo.NumSets)
  })

  private val s0_bankIdx  = getBankIndex(s0_startVAddr)
  private val s0_bankMask = UIntToOH(s0_bankIdx, NumBanks)

  baseTable.io.readReqValid := s0_fire
  baseTable.io.startVAddr   := s0_startVAddr

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

  // TODO: remove it
  private val s1_setIdx = RegEnable(s0_setIdx, s0_fire)

  private val s1_baseTableCtrs   = baseTable.io.takenCtrs
  private val s1_allTableEntries = DataHoldBypass(VecInit(tables.map(_.io.predictReadResp.entries)), RegNext(s0_fire))
  private val s1_allTableUsefulCtrs =
    DataHoldBypass(VecInit(tables.map(_.io.predictReadResp.usefulCtrs)), RegNext(s0_fire))

  private val s1_foldedHist = RegEnable(s0_foldedHist, s0_fire)
  private val s1_tempTag    = VecInit(s1_foldedHist.map(hist => getTag(s1_startVAddr, hist.forTag)))

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 2
     - get results from mbtb
     - get prediction for each branch
     -------------------------------------------------------------------------------------------------------------- */

  private val s2_baseTableCtrs      = RegEnable(s1_baseTableCtrs, s1_fire)
  private val s2_allTableEntries    = RegEnable(s1_allTableEntries, s1_fire)
  private val s2_allTableUsefulCtrs = RegEnable(s1_allTableUsefulCtrs, s1_fire)

  private val s2_startVAddr = RegEnable(s1_startVAddr, s1_fire)
  dontTouch(s2_startVAddr)

  private val s2_setIdx  = RegEnable(s1_setIdx, s1_fire)
  private val s2_tempTag = RegEnable(s1_tempTag, s1_fire)

  private val s2_branches  = io.branchesFromMainBtb
  private val s2_positions = VecInit(s2_branches.map(_.bits.cfiPosition))
  private val s2_condMask  = VecInit(s2_branches.map(branch => branch.valid && branch.bits.attribute.isConditional))
  dontTouch(s2_condMask)

  private val s2_branchesVAddr     = VecInit(s2_positions.map(getBranchVAddr(s2_startVAddr, _)))
  private val s2_branchesUseAltIdx = VecInit(s2_branchesVAddr.map(getUseAltIndex))
  dontTouch(s2_branchesVAddr)

  // to sc
  private val s2_providerTakenCtrVec = Wire(Vec(NumBtbResultEntries, Valid(new SaturateCounter(TakenCtrWidth))))

  private val s2_condTakenMask = s2_condMask.zip(s2_positions).zipWithIndex.map {
    case ((isCond, position), brIdx) =>
      // compare tags of each branch with all tables
      val allTableTagMatchResults = s2_allTableEntries.zip(s2_allTableUsefulCtrs).zipWithIndex.map {
        case ((entriesPerTable, usefulCtrsPerTable), tableIdx) =>
          val tag          = s2_tempTag(tableIdx) ^ position
          val hitWayMask   = entriesPerTable.map(entry => entry.valid && entry.tag === tag)
          val hitWayMaskOH = PriorityEncoderOH(hitWayMask)

          val result = Wire(new TagMatchResult).suggestName(s"branch_${brIdx}_table_${tableIdx}_result")
          result.hit          := isCond && hitWayMask.reduce(_ || _)
          result.entry        := Mux1H(hitWayMaskOH, entriesPerTable)
          result.usefulCtr    := Mux1H(hitWayMaskOH, usefulCtrsPerTable)
          result.hitWayMaskOH := DontCare
          result
      }
      // find the provider, the table with the longest history among the hit tables
      val hitTableMask     = allTableTagMatchResults.map(_.hit)
      val hasProvider      = hitTableMask.reduce(_ || _)
      val providerTableOH  = getLongestHistTableOH(hitTableMask)
      val providerTakenCtr = Mux1H(providerTableOH, allTableTagMatchResults.map(_.entry.takenCtr))

      s2_providerTakenCtrVec(brIdx).valid      := hasProvider
      s2_providerTakenCtrVec(brIdx).bits.value := providerTakenCtr.value

      // find the alt, the table with the second longest history among the hit tables
      val hitTableMaskNoProvider = hitTableMask.zip(providerTableOH).map { case (a, b) => a && !b }
      val hasAlt                 = hasProvider && hitTableMaskNoProvider.reduce(_ || _)
      val altTableOH             = getLongestHistTableOH(hitTableMaskNoProvider)
      val altTakenCtr            = Mux1H(altTableOH, allTableTagMatchResults.map(_.entry.takenCtr))

      val providerIsWeak = providerTakenCtr.isWeak
      val pred           = providerTakenCtr.isPositive
      val altPred        = altTakenCtr.isPositive
      val basePred       = s2_baseTableCtrs(position).isPositive
      val useAlt         = providerIsWeak && hasAlt && useAltCtrVec(s2_branchesUseAltIdx(brIdx)).isPositive

      // get prediction for each branch
      isCond && Mux(
        useAlt,
        altPred,
        Mux(hasProvider, pred, basePred)
      )
  }

  io.condTakenMask       := s2_condTakenMask
  io.providerTakenCtrVec := s2_providerTakenCtrVec

  io.meta.baseTableCtrs := s2_baseTableCtrs
  io.meta.debug_setIdx  := s2_setIdx
  io.meta.debug_tempTag := s2_tempTag

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 0
     - send train request to base table
     - send read request to tables
     -------------------------------------------------------------------------------------------------------------- */

  private val t0_startVAddr = io.train.bits.startVAddr
  private val t0_branches   = io.train.bits.branches

  private val t0_bankIdx  = getBankIndex(t0_startVAddr)
  private val t0_bankMask = UIntToOH(t0_bankIdx, NumBanks)

  private val t0_condMask = VecInit(t0_branches.map(branch => branch.valid && branch.bits.attribute.isConditional))
  private val t0_hasCond  = t0_condMask.reduce(_ || _)

  private val t0_readBankConflict = io.train.valid && t0_hasCond && s0_fire && t0_bankIdx === s0_bankIdx
  io.train.ready := !t0_readBankConflict

  private val t0_valid = io.train.fire && t0_hasCond && io.enable

  // TODO: dont save base table meta
  private val t0_baseTableCtrs = io.train.bits.meta.tage.baseTableCtrs

  private val t0_foldedHist = getFoldedHist(io.foldedPathHistForTrain)
  private val t0_setIdx = VecInit(TableInfos.zip(t0_foldedHist).map { case (tableInfo, hist) =>
    getSetIndex(t0_startVAddr, hist.forIdx, tableInfo.NumSets)
  })
  dontTouch(t0_setIdx)

//  when(t0_valid) {
//    assert(t0_setIdx === io.train.bits.meta.tage.debug_setIdx, "predict setIdx != train setIdx")
//  }

  baseTable.io.train.valid := t0_valid
  baseTable.io.train.bits  := io.train.bits

  tables.zipWithIndex.foreach { case (table, tableIdx) =>
    table.io.trainReadReq.valid         := t0_valid
    table.io.trainReadReq.bits.setIdx   := t0_setIdx(tableIdx)
    table.io.trainReadReq.bits.bankMask := t0_bankMask
  }

  when(t0_valid) {
    assert(!(s0_fire && s0_bankIdx === t0_bankIdx), "TageTable: predictReadReq and trainReadReq conflict")
  }

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 1
     - get read data from tables
     - compute temp tag
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_valid      = RegNext(t0_valid) && io.enable
  private val t1_startVAddr = RegEnable(t0_startVAddr, t0_valid)
  private val t1_branches   = RegEnable(t0_branches, t0_valid)
  private val t1_condMask   = RegEnable(t0_condMask, t0_valid)

  private val t1_setIdx   = RegEnable(t0_setIdx, t0_valid)
  private val t1_bankMask = RegEnable(t0_bankMask, t0_valid)

  private val t1_allTableEntries    = VecInit(tables.map(_.io.trainReadResp.entries))
  private val t1_allTableUsefulCtrs = VecInit(tables.map(_.io.trainReadResp.usefulCtrs))

  private val t1_baseTableCtrs = RegEnable(t0_baseTableCtrs, t0_valid)

  private val t1_foldedHist = RegEnable(t0_foldedHist, t0_valid)
  private val t1_tempTag    = VecInit(t1_foldedHist.map(hist => getTag(t1_startVAddr, hist.forTag)))

  private val t1_debugTempTag = RegEnable(io.train.bits.meta.tage.debug_tempTag, t0_valid)
//  when(t1_valid) {
//    assert(t1_tempTag === t1_debugTempTag, "predict tag != train tag")
//  }

  private val t1_branchesVAddr =
    VecInit(t1_branches.map(branch => getBranchVAddr(s2_startVAddr, branch.bits.cfiPosition)))
  private val t1_branchesUseAltIdx = VecInit(t1_branchesVAddr.map(getUseAltIndex))

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 2
     - update branches' takenCtr and usefulCtr
     - allocate a new entry when mispredict
     -------------------------------------------------------------------------------------------------------------- */

  private val t2_valid    = RegNext(t1_valid) && io.enable
  private val t2_branches = RegEnable(t1_branches, t1_valid)
  private val t2_condMask = RegEnable(t1_condMask, t1_valid)

  private val t2_startVAddr = RegEnable(t1_startVAddr, t1_valid)
  dontTouch(t2_startVAddr)

  private val t2_setIdx   = RegEnable(t1_setIdx, t1_valid)
  private val t2_bankMask = RegEnable(t1_bankMask, t1_valid)

  private val t2_allTableEntries    = RegEnable(t1_allTableEntries, t1_valid)
  private val t2_allTableUsefulCtrs = RegEnable(t1_allTableUsefulCtrs, t1_valid)

  private val t2_tempTag = RegEnable(t1_tempTag, t1_valid)

  private val t2_baseTableCtrs = RegEnable(t1_baseTableCtrs, t1_valid)

  private val t2_branchesVAddr     = RegEnable(t1_branchesVAddr, t1_valid)
  private val t2_branchesUseAltIdx = RegEnable(t1_branchesUseAltIdx, t1_valid)
  dontTouch(t2_branchesVAddr)

  private val t2_allBranchUpdateInfo = t2_branches.zipWithIndex.map { case (branch, brIdx) =>
    val isCond = t2_condMask(brIdx)
    val allTableTagMatchResults = t2_allTableEntries.zip(t2_allTableUsefulCtrs).zipWithIndex.map {
      case ((entriesPerTable, usefulCtrsPerTable), tableIdx) =>
        val tag          = t2_tempTag(tableIdx) ^ branch.bits.cfiPosition
        val hitWayMask   = entriesPerTable.map(entry => entry.valid && entry.tag === tag)
        val hitWayMaskOH = PriorityEncoderOH(hitWayMask)

        val result = Wire(new TagMatchResult).suggestName(s"branch_${brIdx}_table_${tableIdx}_result")
        result.hit          := isCond && hitWayMask.reduce(_ || _)
        result.entry        := Mux1H(hitWayMaskOH, entriesPerTable)
        result.usefulCtr    := Mux1H(hitWayMaskOH, usefulCtrsPerTable)
        result.hitWayMaskOH := hitWayMaskOH.asUInt
        result
    }
    val hitTableMask    = allTableTagMatchResults.map(_.hit)
    val hasProvider     = hitTableMask.reduce(_ || _)
    val providerTableOH = getLongestHistTableOH(hitTableMask)
    val providerInfo    = Mux1H(providerTableOH, allTableTagMatchResults)

    val hitTableMaskNoProvider = hitTableMask.zip(providerTableOH).map { case (a, b) => a && !b }
    val hasAlt                 = hasProvider && hitTableMaskNoProvider.reduce(_ || _)
    val altTableOH             = getLongestHistTableOH(hitTableMaskNoProvider)
    val altInfo                = Mux1H(altTableOH, allTableTagMatchResults)

    val pred           = providerInfo.entry.takenCtr.isPositive
    val providerIsWeak = providerInfo.entry.takenCtr.isWeak
    val altPred        = altInfo.entry.takenCtr.isPositive
    val basePred       = t2_baseTableCtrs(branch.bits.cfiPosition).isPositive
    val actualTaken    = branch.bits.taken

    val providerNewTakenCtr = providerInfo.entry.takenCtr.getUpdate(actualTaken)
    val providerNewUsefulCtr = Mux(
      hasProvider && pred === actualTaken && pred =/= Mux(hasAlt, altPred, basePred),
      providerInfo.usefulCtr.getIncrease,
      providerInfo.usefulCtr.value
    )

    val updateAlt      = providerIsWeak && hasAlt && useAltCtrVec(t2_branchesUseAltIdx(brIdx)).isPositive
    val altNewTakenCtr = altInfo.entry.takenCtr.getUpdate(actualTaken)

    val increaseUseAlt = hasProvider && providerIsWeak && Mux(hasAlt, altPred, basePred) === actualTaken
    val decreaseUseAlt = hasProvider && providerIsWeak && Mux(hasAlt, altPred, basePred) =/= actualTaken

    val updateInfo = Wire(new UpdateInfo).suggestName(s"branch_${brIdx}_updateInfo")
    updateInfo.providerTableOH              := providerTableOH.asUInt & Fill(NumTables, hasProvider)
    updateInfo.providerWayOH                := providerInfo.hitWayMaskOH
    updateInfo.providerEntry.valid          := true.B
    updateInfo.providerEntry.tag            := providerInfo.entry.tag
    updateInfo.providerEntry.takenCtr.value := providerNewTakenCtr
    updateInfo.providerNewUsefulCtr.value   := providerNewUsefulCtr

    updateInfo.altTableOH              := altTableOH.asUInt & Fill(NumTables, updateAlt)
    updateInfo.altWayOH                := altInfo.hitWayMaskOH
    updateInfo.altEntry.valid          := true.B
    updateInfo.altEntry.tag            := altInfo.entry.tag
    updateInfo.altEntry.takenCtr.value := altNewTakenCtr
    updateInfo.altOldUsefulCtr         := altInfo.usefulCtr

    updateInfo.needAllocate := isCond && branch.bits.mispredict && !(hasProvider && providerTableOH(NumTables - 1))

    updateInfo.increaseUseAlt := increaseUseAlt
    updateInfo.decreaseUseAlt := decreaseUseAlt
    updateInfo
  }

  useAltCtrVec.zipWithIndex.map { case (ctr, i) =>
    val idxMatchMask = t2_branchesUseAltIdx.map(_ === i.U)
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

  private val t2_updateMask       = Wire(Vec(NumTables, Vec(NumWays, Bool())))
  private val t2_updateEntries    = Wire(Vec(NumTables, Vec(NumWays, new TageEntry)))
  private val t2_updateUsefulCtrs = Wire(Vec(NumTables, Vec(NumWays, new SaturateCounter(UsefulCtrWidth))))
  dontTouch(t2_updateEntries)
  dontTouch(t2_updateUsefulCtrs)

  t2_updateMask.zip(t2_updateEntries).zip(t2_updateUsefulCtrs).zipWithIndex.map {
    case (((updateEnPerTable, entriesPerTable), usefulCtrsPerTable), tableIdx) =>
      updateEnPerTable.zip(entriesPerTable).zip(usefulCtrsPerTable).zipWithIndex.map {
        case (((updateEn, entry), usefulCtr), wayIdx) =>
          val hitBranchProviderMask = t2_allBranchUpdateInfo.map { branch =>
            branch.providerTableOH(tableIdx) && branch.providerWayOH(wayIdx)
          }
          val hitBranchAltMask = t2_allBranchUpdateInfo.map { branch =>
            branch.altTableOH(tableIdx) && branch.altWayOH(wayIdx)
          }
          val hitBranchProvider  = hitBranchProviderMask.reduce(_ || _)
          val hitBranchAlt       = hitBranchAltMask.reduce(_ || _)
          val providerUpdateInfo = Mux1H(hitBranchProviderMask, t2_allBranchUpdateInfo)
          val altUpdateInfo      = Mux1H(hitBranchAltMask, t2_allBranchUpdateInfo)
          updateEn  := hitBranchProvider || hitBranchAlt
          entry     := Mux(hitBranchProvider, providerUpdateInfo.providerEntry, altUpdateInfo.altEntry)
          usefulCtr := Mux(hitBranchProvider, providerUpdateInfo.providerNewUsefulCtr, altUpdateInfo.altOldUsefulCtr)
      }
  }

  private val t2_needAllocateBranchMask = t2_allBranchUpdateInfo.map(_.needAllocate)
  when(t2_valid) {
    assert(PopCount(t2_needAllocateBranchMask) <= 1.U)
  }
  private val t2_needAllocate   = t2_needAllocateBranchMask.reduce(_ || _)
  private val t2_allocateBranch = Mux1H(t2_needAllocateBranchMask, t2_branches)
  private val t2_allocateBranchProviderTableOH =
    Mux1H(t2_needAllocateBranchMask, t2_allBranchUpdateInfo.map(_.providerTableOH))

  // allocate new entry to the table with a longer history
  private val t2_longerHistoryTableMask = Mux(
    t2_allocateBranchProviderTableOH.orR, // has provider
    (~((t2_allocateBranchProviderTableOH - 1.U) | t2_allocateBranchProviderTableOH)).asUInt,
    Fill(NumTables, true.B)
  )
  dontTouch(t2_longerHistoryTableMask)

  private val t2_allTableNotUsefulWayMask = t2_allTableUsefulCtrs.map { ctrsPerTable =>
    ctrsPerTable.map(_.value === 0.U).asUInt
  }
  private val t2_hasNotUsefulEntryTableMask = t2_allTableNotUsefulWayMask.map(_.orR).asUInt
  private val t2_canAllocateTableMask       = t2_longerHistoryTableMask & t2_hasNotUsefulEntryTableMask
  private val t2_canAllocate                = t2_canAllocateTableMask.orR

  private val t2_allocateTableMaskOH =
    PriorityEncoderOH(t2_canAllocateTableMask) & Fill(NumTables, t2_needAllocate && t2_canAllocate)
  private val t2_allocateWayMaskOH = PriorityEncoderOH(Mux1H(t2_allocateTableMaskOH, t2_allTableNotUsefulWayMask))
  dontTouch(t2_allocateTableMaskOH)
  dontTouch(t2_allocateWayMaskOH)

  private val t2_allocateEntry = Wire(new TageEntry)
  t2_allocateEntry.valid := true.B
  t2_allocateEntry.tag   := Mux1H(t2_allocateTableMaskOH, t2_tempTag) ^ t2_allocateBranch.bits.cfiPosition
  t2_allocateEntry.takenCtr.value := Mux(
    t2_allocateBranch.bits.taken,
    (1 << (TakenCtrWidth - 1)).U,    // weak taken
    (1 << (TakenCtrWidth - 1) - 1).U // weak not taken
  )

  tables.zipWithIndex.foreach { case (table, tableIdx) =>
    val thisTableNeedUpdate   = t2_updateMask(tableIdx).reduce(_ || _)
    val thisTableNeedAllocate = t2_allocateTableMaskOH(tableIdx)
    table.io.writeReq.valid         := t2_valid && (thisTableNeedUpdate || thisTableNeedAllocate)
    table.io.writeReq.bits.setIdx   := t2_setIdx(tableIdx)
    table.io.writeReq.bits.bankMask := t2_bankMask

    val writeEntries    = Wire(Vec(NumWays, new TageEntry))
    val writeUsefulCtrs = Wire(Vec(NumWays, new SaturateCounter(UsefulCtrWidth)))
    val writeWayMask    = Wire(Vec(NumWays, Bool()))

    writeEntries.zip(writeUsefulCtrs).zipWithIndex.foreach { case ((entry, usefulCtr), wayIdx) =>
      val thisWayNeedUpdate   = t2_updateMask(tableIdx)(wayIdx)
      val thisWayNeedAllocate = thisTableNeedAllocate && t2_allocateWayMaskOH(wayIdx)
      entry           := Mux(thisWayNeedAllocate, t2_allocateEntry, t2_updateEntries(tableIdx)(wayIdx))
      usefulCtr.value := Mux(thisWayNeedAllocate, UsefulCtrInitValue.U, t2_updateUsefulCtrs(tableIdx)(wayIdx).value)
      writeWayMask(wayIdx) := thisWayNeedUpdate || thisWayNeedAllocate
    }

    table.io.writeReq.bits.wayMask    := writeWayMask.asUInt
    table.io.writeReq.bits.entries    := writeEntries
    table.io.writeReq.bits.usefulCtrs := writeUsefulCtrs

    table.io.resetUseful := t2_valid && usefulResetCtr.isSaturatePositive
  }

  when(t2_valid && usefulResetCtr.isSaturatePositive) {
    usefulResetCtr.resetZero()
  }.elsewhen(t2_valid && t2_needAllocate && !t2_canAllocate) {
    usefulResetCtr.increase()
  }

  /* --------------------------------------------------------------------------------------------------------------
     performance counter
     -------------------------------------------------------------------------------------------------------------- */

  XSPerfAccumulate("predict_cond", Mux(io.stageCtrl.s2_fire, PopCount(s2_condMask), 0.U))

  XSPerfAccumulate("total_train", io.train.fire)
  XSPerfAccumulate("train_has_cond", t0_valid)

  XSPerfAccumulate("mispredict_branch_has_provider", t2_valid && t2_allocateBranchProviderTableOH.orR)
  for (i <- 0 until NumTables) {
    XSPerfAccumulate(
      s"mispredict_branch_provider_is_table_${i}",
      t2_valid && t2_allocateBranchProviderTableOH.orR && t2_allocateBranchProviderTableOH(i)
    )
  }

  XSPerfAccumulate("read_conflict", t0_readBankConflict)
  XSPerfAccumulate("reset_useful", t2_valid && usefulResetCtr.isSaturatePositive)
  XSPerfAccumulate("need_allocate", t2_valid && t2_needAllocate)
  XSPerfAccumulate("allocate_success", t2_valid && t2_needAllocate && t2_canAllocate)
  XSPerfAccumulate("allocate_failure", t2_valid && t2_needAllocate && !t2_canAllocate)
  for (i <- 0 until NumTables) {
    XSPerfAccumulate(s"table_${i}_allocate", t2_valid && t2_allocateTableMaskOH(i))
  }
  // TODO: add more
}

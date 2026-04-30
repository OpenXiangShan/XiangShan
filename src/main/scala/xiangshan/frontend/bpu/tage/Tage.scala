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
import xiangshan.frontend.bpu.TageTableInfo

/**
 * This module is the implementation of the TAGE (TAgged GEometric history length predictor).
 */
class Tage(implicit p: Parameters) extends BasePredictor with HasTageParameters with TopHelper with HalfAlignHelper {
  class TageIO(implicit p: Parameters) extends BasePredictorIO {
    val fromPhr:     PhrToTageIO         = new PhrToTageIO
    val fromMainBtb: MainBtbToTageIO     = new MainBtbToTageIO
    val toSc:        TageToScIO          = new TageToScIO
    val prediction:  Vec[TagePrediction] = Output(Vec(NumBtbResultEntries, new TagePrediction))
    val meta:        TageMeta            = Output(new TageMeta)

    val debug_trainValid: Bool = Input(Bool())
  }
  val io: TageIO = IO(new TageIO)

  /* *** submodules *** */
  private val tables = TableInfos.zipWithIndex.map { case (info, i) => Module(new TageTable(i, info)) }

  // reset all usefulCtr when usefulResetCtr saturated
  private val usefulResetCtr      = RegInit(UsefulResetCounter.Zero)
  private val usefulResetInFlight = RegInit(false.B)

  // use the alternate prediction when counter is positive
  private val useAltOnNaVec = RegInit(VecInit.fill(NumUseAltOnNa)(UseAltOnNaCounter.Zero))

  /* *** reset *** */
  io.sramResetDone := tables.map(_.io.sramResetDone).reduce(_ && _)

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 0
     - send read request to tables
     -------------------------------------------------------------------------------------------------------------- */

  private val s0_fire    = io.stageCtrl.s0_fire && io.enable
  private val s0_startPc = io.startPc

  private val s0_foldedHist = getFoldedHist(io.fromPhr.foldedPathHist)
  private val s0_setIdx = VecInit((tables zip s0_foldedHist).map { case (table, hist) =>
    table.getSetIndex(s0_startPc, hist.forIdx)
  })

  // currently all tables share the same bank index
  private val s0_bankIdx  = tables.head.getBankIndex(s0_startPc)
  private val s0_bankMask = UIntToOH(s0_bankIdx, NumBanks)

  tables.zipWithIndex.foreach { case (table, tableIdx) =>
    table.io.readReq(0).valid         := s0_fire
    table.io.readReq(0).bits.setIdx   := s0_setIdx(tableIdx)
    table.io.readReq(0).bits.bankMask := s0_bankMask
  }

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 1
     - get read resp from tables
     - compute tag
     -------------------------------------------------------------------------------------------------------------- */

  private val s1_fire       = io.stageCtrl.s1_fire
  private val s1_startPc    = RegEnable(s0_startPc, s0_fire)
  private val s1_foldedHist = RegEnable(s0_foldedHist, s0_fire)

  // Vec[NumBtbResultEntries][NumTables]
  private val s1_tag = VecInit(io.fromMainBtb.s1_positions.map { position =>
    VecInit((tables zip s1_foldedHist).map { case (table, hist) =>
      table.getTag(s1_startPc, hist.forTag, position)
    })
  })

  private val s1_readResp = DataHoldBypass(VecInit(tables.map(_.io.readResp(0))), RegNext(s0_fire))

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 2
     - get results from mbtb
     - get prediction for each branch
     -------------------------------------------------------------------------------------------------------------- */

  private val s2_fire     = io.stageCtrl.s2_fire
  private val s2_startPc  = RegEnable(s1_startPc, s1_fire)
  private val s2_tag      = RegEnable(s1_tag, s1_fire)
  private val s2_readResp = RegEnable(s1_readResp, s1_fire)

  private val s2_branches = io.fromMainBtb.result

  s2_branches.zipWithIndex.foreach { case (branch, i) =>
    val position      = branch.bits.cfiPosition
    val cfiPc         = getCfiPcFromPosition(s2_startPc, position)
    val useAltOnNaIdx = getUseAltOnNaIdx(cfiPc)
    val useAltOnNa    = useAltOnNaVec(useAltOnNaIdx).isPositive

    // compare tags of each branch with all tables
    val allTableTagMatchResults = s2_readResp.zipWithIndex.map { case (tableReadResp, tableIdx) =>
      val tag          = s2_tag(i)(tableIdx)
      val hitWayMask   = tableReadResp.entries.map(entry => entry.valid && entry.tag === tag)
      val hitWayMaskOH = PriorityEncoderOH(hitWayMask)

      val result = Wire(new PredictTagMatchResult).suggestName(s"s2_branch_${i}_table_${tableIdx}_result")
      result.hit          := hitWayMask.reduce(_ || _)
      result.hitWayMaskOH := hitWayMaskOH.asUInt
      result.takenCtr     := Mux1H(hitWayMaskOH, tableReadResp.entries.map(_.takenCtr))
      result.usefulCtr    := Mux1H(hitWayMaskOH, tableReadResp.usefulCtrs)
      result.hitWayMask   := hitWayMask.asUInt
      result
    }
    // find the provider, the table with the longest history among the hit tables
    val hitTableMask    = allTableTagMatchResults.map(_.hit)
    val hasProvider     = hitTableMask.reduce(_ || _)
    val providerTableOH = getLongestHistTableOH(hitTableMask)
    val provider        = Mux1H(providerTableOH, allTableTagMatchResults)

    // find the alt, the table with the second longest history among the hit tables
    val hitTableMaskNoProvider = hitTableMask.zip(providerTableOH).map { case (a, b) => a && !b }
    val hasAlt                 = hasProvider && hitTableMaskNoProvider.reduce(_ || _)
    val altTableOH             = getLongestHistTableOH(hitTableMaskNoProvider)
    val alt                    = Mux1H(altTableOH, allTableTagMatchResults)

    val useProvider = hasProvider && !(useAltOnNa && provider.takenCtr.isWeak)

    // get prediction for each branch
    io.prediction(i).useProvider  := useProvider
    io.prediction(i).providerPred := provider.takenCtr.isPositive
    io.prediction(i).hasAlt       := hasAlt
    io.prediction(i).altPred      := alt.takenCtr.isPositive

    io.toSc.providerTakenCtrVec(i).valid := hasProvider && branch.valid
    io.toSc.providerTakenCtrVec(i).bits  := provider.takenCtr

    io.meta.entries(i).useProvider       := useProvider
    io.meta.entries(i).providerTableIdx  := OHToUInt(providerTableOH)
    io.meta.entries(i).providerWayIdx    := OHToUInt(provider.hitWayMaskOH)
    io.meta.entries(i).providerTakenCtr  := provider.takenCtr
    io.meta.entries(i).providerUsefulCtr := provider.usefulCtr
    io.meta.entries(i).altOrBasePred     := Mux(hasAlt, alt.takenCtr.isPositive, branch.bits.taken)

    XSPerfAccumulate(
      s"s2_branch_${i}_multihit_on_same_table",
      allTableTagMatchResults.map(e => (s2_fire && PopCount(e.hitWayMask) > 1.U).asUInt).reduce(_ +& _)
    )
  }

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 0
     - send train request to base table
     - send read request to tables
     -------------------------------------------------------------------------------------------------------------- */

  private val t0_startPc  = io.train.startPc
  private val t0_branches = io.train.branches

  // currently all tables share the same bank index
  private val t0_bankIdx  = tables.head.getBankIndex(t0_startPc)
  private val t0_bankMask = UIntToOH(t0_bankIdx, NumBanks)

  private val t0_condMask = VecInit(t0_branches.map(branch => branch.valid && branch.bits.attribute.isConditional))
  private val t0_hasCond  = t0_condMask.reduce(_ || _)

  private val t0_fire = io.stageCtrl.t0_fire && t0_hasCond && io.enable

  private val (t0_mbtbHitMask, t0_basePred, t0_meta) = t0_branches.map { branch =>
    val mbtbMeta  = io.train.meta.mbtb.entries.flatten
    val tageMeta  = io.train.meta.tage.entries
    val hitMask   = mbtbMeta.map(_.hit(branch.bits))
    val hitMaskOH = PriorityEncoderOH(hitMask)
    val mbtbHit   = hitMask.reduce(_ || _)
    val basePred  = Mux1H(hitMaskOH, mbtbMeta.map(_.counter.isPositive))
    val meta      = Mux1H(hitMaskOH, tageMeta)
    (mbtbHit, basePred, meta)
  }.unzip3

  // if all hit conditional branches use provider and no mispredict, use meta to train
  private val t0_useMeta = t0_branches.zipWithIndex.map { case (branch, i) =>
    val mbtbHit      = t0_mbtbHitMask(i)
    val isCond       = t0_condMask(i)
    val useProvider  = t0_meta(i).useProvider
    val mispredicted = branch.bits.mispredict
    !(mbtbHit && isCond) || (useProvider && !mispredicted)
  }.reduce(_ && _)
  private val t0_needRead = !t0_useMeta

  private val t0_readBankConflict = t0_hasCond && t0_needRead && s0_fire && t0_bankIdx === s0_bankIdx
  io.trainReady := !t0_readBankConflict

  // t0_readBankConflict can be high even there's no train.valid, causing perf counters to be inaccurate
  // so we use a debug_ signal for perf counters
  private val debug_readBankConflict = io.debug_trainValid && t0_readBankConflict

  private val t0_foldedHist = getFoldedHist(io.fromPhr.foldedPathHistForTrain)
  private val t0_setIdx = VecInit((tables zip t0_foldedHist).map { case (table, hist) =>
    table.getSetIndex(t0_startPc, hist.forIdx)
  })
  dontTouch(t0_setIdx)

  tables.zipWithIndex.foreach { case (table, tableIdx) =>
    table.io.readReq(1).valid         := t0_fire && t0_needRead
    table.io.readReq(1).bits.setIdx   := t0_setIdx(tableIdx)
    table.io.readReq(1).bits.bankMask := t0_bankMask
  }

  // only for perf
  private val debug_readBankConflictReg     = RegNext(debug_readBankConflict)
  private val debug_readBankConflictPos     = debug_readBankConflict && (!debug_readBankConflictReg)
  private val debug_readBankConflictNeg     = !debug_readBankConflict && debug_readBankConflictReg
  private val debug_readBankConflictDistCnt = RegInit(0.U(4.W))
  private val debug_s0AlignedPc             = getAlignedPc(s0_startPc)
  private val debug_s1AlignedPc             = getAlignedPc(s1_startPc)
  private val debug_s1BankIdx               = RegEnable(s0_bankIdx, s0_fire)
  // pred target within align 64B,and not blocked by s2
  private val debug_readBankConflictShortLoop = debug_readBankConflictReg && s1_fire &&
    (debug_s1BankIdx === s0_bankIdx) &&
    (debug_s0AlignedPc.toUInt - debug_s1AlignedPc.toUInt <= FetchBlockSize.U ||
      debug_s1AlignedPc.toUInt - debug_s0AlignedPc.toUInt <= FetchBlockSize.U) && s0_fire
  private val debug_readBankConflictShortLoopReg = RegNext(debug_readBankConflictShortLoop)
  private val debug_readBankConflictShortLoopNeg = !debug_readBankConflictShortLoop & debug_readBankConflictShortLoopReg
  private val debug_readBankConflictShortLoopDistCnt = RegInit(0.U(4.W))
  // dist cnt
  debug_readBankConflictShortLoopDistCnt := Mux(
    debug_readBankConflictShortLoopNeg,
    0.U,
    Mux(
      debug_readBankConflictShortLoop,
      debug_readBankConflictShortLoopDistCnt + 1.U,
      debug_readBankConflictShortLoopDistCnt
    )
  )

  debug_readBankConflictDistCnt := Mux(
    debug_readBankConflictNeg,
    0.U,
    Mux(debug_readBankConflict, debug_readBankConflictDistCnt + 1.U, debug_readBankConflictDistCnt)
  )

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 1
     - get read data from tables
     - compute temp tag
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_fire     = RegNext(t0_fire, init = false.B)
  private val t1_startPc  = RegEnable(t0_startPc, t0_fire)
  private val t1_branches = RegEnable(t0_branches, t0_fire)

  private val t1_setIdx   = RegEnable(t0_setIdx, t0_fire)
  private val t1_bankMask = RegEnable(t0_bankMask, t0_fire)

  private val t1_useMeta     = RegEnable(t0_useMeta, t0_fire)
  private val t1_meta        = RegEnable(VecInit(t0_meta), t0_fire)
  private val t1_basePred    = RegEnable(VecInit(t0_basePred), t0_fire)
  private val t1_mbtbHitMask = RegEnable(VecInit(t0_mbtbHitMask), t0_fire)

  private val t1_foldedHist = RegEnable(t0_foldedHist, t0_fire)
  private val t1_rawTag = VecInit((tables zip t1_foldedHist).map { case (table, hist) =>
    table.getRawTag(t1_startPc, hist.forTag)
  })

  private val t1_readResp = VecInit(tables.map(_.io.readResp(1)))

  /* --------------------------------------------------------------------------------------------------------------
    train pipeline stage 2
    - generate train info for each branch
     -------------------------------------------------------------------------------------------------------------- */

  private val t2_fire     = RegNext(t1_fire, init = false.B)
  private val t2_branches = RegEnable(t1_branches, t1_fire)
  private val t2_startPc  = RegEnable(t1_startPc, t1_fire)
  dontTouch(t2_startPc)

  private val t2_setIdx   = RegEnable(t1_setIdx, t1_fire)
  private val t2_bankMask = RegEnable(t1_bankMask, t1_fire)
  private val t2_rawTag   = RegEnable(t1_rawTag, t1_fire)
  private val t2_readResp = RegEnable(t1_readResp, t1_fire)

  private val t2_useMeta     = RegEnable(t1_useMeta, t1_fire)
  private val t2_meta        = RegEnable(t1_meta, t1_fire)
  private val t2_basePred    = RegEnable(t1_basePred, t1_fire)
  private val t2_mbtbHitMask = RegEnable(t1_mbtbHitMask, t1_fire)

  private val t2_cfiUseAltOnNaIdxVec = VecInit(t2_branches.map { branch =>
    val cfiPc = getCfiPcFromPosition(t2_startPc, branch.bits.cfiPosition)
    getUseAltOnNaIdx(cfiPc)
  })

  private val t2_trainInfoVec = VecInit(t2_branches.zipWithIndex.map { case (branch, i) =>
    val isCond        = branch.valid && branch.bits.attribute.isConditional
    val mbtbHit       = t2_mbtbHitMask(i)
    val meta          = t2_meta(i)
    val position      = branch.bits.cfiPosition
    val actualTaken   = branch.bits.taken
    val useAltOnNaIdx = t2_cfiUseAltOnNaIdxVec(i)
    val useAltOnNa    = useAltOnNaVec(useAltOnNaIdx).isPositive

    val allTableTagMatchResults = t2_readResp.zipWithIndex.map { case (tableReadResp, tableIdx) =>
      val tag          = t2_rawTag(tableIdx) ^ position
      val hitWayMask   = tableReadResp.entries.map(entry => entry.valid && entry.tag === tag)
      val hitWayMaskOH = PriorityEncoderOH(hitWayMask)
      dontTouch(tag.suggestName(s"t2_branch_${i}_table_${tableIdx}_tag"))

      val result = Wire(new TrainTagMatchResult).suggestName(s"t2_branch_${i}_table_${tableIdx}_result")
      result.hit          := hitWayMask.reduce(_ || _)
      result.hitWayMaskOH := hitWayMaskOH.asUInt
      result.tag          := tag
      result.takenCtr     := Mux1H(hitWayMaskOH, tableReadResp.entries.map(_.takenCtr))
      result.usefulCtr    := Mux1H(hitWayMaskOH, tableReadResp.usefulCtrs)
      result
    }
    val hitTableMask = allTableTagMatchResults.map(_.hit)
    dontTouch(hitTableMask.asUInt.suggestName(s"t2_branch_${i}_hitTableMask"))

    val hasProvider     = Wire(Bool())
    val providerTableOH = Wire(UInt(NumTables.W))
    val provider        = Wire(new TrainTagMatchResult)

    val hasAlt     = Wire(Bool())
    val altTableOH = Wire(UInt(NumTables.W))
    val alt        = Wire(new TrainTagMatchResult)

    val useProvider   = Wire(Bool())
    val useAlt        = Wire(Bool())
    val altOrBasePred = Wire(Bool())

    when(t2_useMeta) {
      hasProvider     := true.B
      providerTableOH := UIntToOH(meta.providerTableIdx, NumTables)

      provider.hit          := true.B
      provider.hitWayMaskOH := UIntToOH(meta.providerWayIdx, MaxNumWays)
      provider.tag          := t2_rawTag(meta.providerTableIdx) ^ position
      provider.takenCtr     := meta.providerTakenCtr
      provider.usefulCtr    := meta.providerUsefulCtr

      hasAlt     := false.B
      altTableOH := 0.U
      alt        := 0.U.asTypeOf(new TrainTagMatchResult)

      useProvider   := true.B
      useAlt        := false.B
      altOrBasePred := meta.altOrBasePred
    }.otherwise { // use result from sram read resp
      hasProvider     := hitTableMask.reduce(_ || _)
      providerTableOH := getLongestHistTableOH(hitTableMask).asUInt
      provider        := Mux1H(providerTableOH, allTableTagMatchResults)

      val hitTableMaskNoProvider = hitTableMask.zip(providerTableOH.asBools).map { case (a, b) => a && !b }
      hasAlt     := hasProvider && hitTableMaskNoProvider.reduce(_ || _)
      altTableOH := getLongestHistTableOH(hitTableMaskNoProvider).asUInt
      alt        := Mux1H(altTableOH, allTableTagMatchResults)

      useProvider   := hasProvider && (!useAltOnNa || !provider.takenCtr.isWeak)
      useAlt        := !useProvider && hasAlt
      altOrBasePred := Mux(hasAlt, alt.takenCtr.isPositive, t2_basePred(i))
    }

    val providerPred = provider.takenCtr.isPositive
    val finalPred    = Mux(useProvider, providerPred, altOrBasePred)

    val providerNewTakenCtr = provider.takenCtr.getUpdate(actualTaken)
    val altNewTakenCtr      = alt.takenCtr.getUpdate(actualTaken)

    val incProviderUsefulCtr = hasProvider && providerPred === actualTaken && providerPred =/= altOrBasePred
    val providerNewUsefulCtr = provider.usefulCtr.getIncrease(en = incProviderUsefulCtr)

    // allocate when mispredict, but except when:
    // 1. already on the highest table
    // 2. providerPred is not used, providerPred is right and provider is weak
    val needAllocate = branch.bits.mispredict && (finalPred =/= actualTaken) &&
      !(hasProvider && providerTableOH(NumTables - 1)) &&
      !(hasProvider && !useProvider && providerPred === actualTaken && provider.takenCtr.isWeak)

    val needUpdateProviderCtr    = !provider.takenCtr.shouldHold(actualTaken) && hasProvider
    val needUpdateProviderUseful = !provider.usefulCtr.isSaturatePositive && incProviderUsefulCtr && hasProvider

    val needUpdateAltCtr = !alt.takenCtr.shouldHold(actualTaken) && useAlt

    val incUseAltOnNa = hasProvider && provider.takenCtr.isWeak && altOrBasePred === actualTaken
    val decUseAltOnNa = hasProvider && provider.takenCtr.isWeak && altOrBasePred =/= actualTaken

    val trainInfo = Wire(new TrainInfo).suggestName(s"t2_branch_${i}_trainInfo")
    trainInfo.valid := isCond && mbtbHit // Only consider update if conditional branch

    trainInfo.hasProvider            := hasProvider
    trainInfo.useProvider            := useProvider
    trainInfo.providerTableOH        := providerTableOH.asUInt
    trainInfo.providerWayOH          := provider.hitWayMaskOH
    trainInfo.providerEntry.valid    := true.B
    trainInfo.providerEntry.tag      := provider.tag
    trainInfo.providerEntry.takenCtr := providerNewTakenCtr
    trainInfo.providerOldUsefulCtr   := provider.usefulCtr
    trainInfo.providerNewUsefulCtr   := providerNewUsefulCtr

    trainInfo.hasAlt            := hasAlt
    trainInfo.useAlt            := useAlt
    trainInfo.altTableOH        := altTableOH.asUInt
    trainInfo.altWayOH          := alt.hitWayMaskOH
    trainInfo.altEntry.valid    := true.B
    trainInfo.altEntry.tag      := alt.tag
    trainInfo.altEntry.takenCtr := altNewTakenCtr
    trainInfo.altOldUsefulCtr   := alt.usefulCtr

    trainInfo.needAllocate             := needAllocate
    trainInfo.needUpdateProviderCtr    := needUpdateProviderCtr
    trainInfo.needUpdateProviderUseful := needUpdateProviderUseful
    trainInfo.needUpdateAltCtr         := needUpdateAltCtr

    trainInfo.incUseAltOnNa := incUseAltOnNa
    trainInfo.decUseAltOnNa := decUseAltOnNa

    trainInfo.finalPred   := finalPred
    trainInfo.actualTaken := actualTaken

    trainInfo.hitTableMask     := hitTableMask.asUInt
    trainInfo.mispredicted     := branch.bits.mispredict
    trainInfo.newestMispredict := finalPred =/= actualTaken
    trainInfo
  })

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 3
     - update branches' takenCtr and usefulCtr
     - allocate a new entry when mispredict
     -------------------------------------------------------------------------------------------------------------- */

  private val t3_fire                = RegNext(t2_fire, init = false.B)
  private val t3_branches            = RegEnable(t2_branches, t2_fire)
  private val t3_startPc             = RegEnable(t2_startPc, t2_fire)
  private val t3_setIdx              = RegEnable(t2_setIdx, t2_fire)
  private val t3_bankMask            = RegEnable(t2_bankMask, t2_fire)
  private val t3_rawTag              = RegEnable(t2_rawTag, t2_fire)
  private val t3_readResp            = RegEnable(t2_readResp, t2_fire)
  private val t3_useMeta             = RegEnable(t2_useMeta, t2_fire)
  private val t3_mbtbHitMask         = RegEnable(t2_mbtbHitMask, t2_fire)
  private val t3_cfiUseAltOnNaIdxVec = RegEnable(t2_cfiUseAltOnNaIdxVec, t2_fire)
  private val t3_trainInfoVec        = RegEnable(t2_trainInfoVec, t2_fire)

  private val t3_needAllocateBranchOH = t3_trainInfoVec.map(info => info.valid && info.needAllocate)
  when(t3_fire) {
    assert(PopCount(t3_needAllocateBranchOH) <= 1.U)
  }
  private val t3_needAllocate            = t3_needAllocateBranchOH.reduce(_ || _)
  private val t3_allocateBranch          = Mux1H(t3_needAllocateBranchOH, t3_branches)
  private val t3_allocateBranchTrainInfo = Mux1H(t3_needAllocateBranchOH, t3_trainInfoVec)

  // allocate new entry to the table with a longer history
  private val t3_longerHistoryTableMask = {
    val hasProvider     = t3_allocateBranchTrainInfo.hasProvider
    val providerTableOH = t3_allocateBranchTrainInfo.providerTableOH
    Mux(
      hasProvider,
      (~((providerTableOH - 1.U) | providerTableOH)).asUInt,
      Fill(NumTables, true.B)
    )
  }
  dontTouch(t3_longerHistoryTableMask)

  private val t3_allTableCanAllocateWayMask = t3_readResp.map { tableReadResp =>
    val notValidMask  = tableReadResp.entries.map(!_.valid).asUInt
    val notUsefulMask = tableReadResp.usefulCtrs.map(_.isSaturateNegative).asUInt
    val ctrWeakAndNotUsefulMask = tableReadResp.entries.zip(tableReadResp.usefulCtrs).map { case (entry, usefulCtr) =>
      entry.takenCtr.isWeak && usefulCtr.isSaturateNegative
    }.asUInt
    MuxCase(
      notUsefulMask,
      Seq(
        notValidMask.orR            -> notValidMask,
        ctrWeakAndNotUsefulMask.orR -> ctrWeakAndNotUsefulMask
      )
    )
  }
  private val t3_canAllocateTableMask = t3_longerHistoryTableMask & t3_allTableCanAllocateWayMask.map(_.orR).asUInt
  private val t3_canAllocate          = t3_canAllocateTableMask.orR
  private val t3_allocate             = t3_needAllocate && t3_canAllocate

  private val t3_allocateTableOH = PriorityEncoderOH(t3_canAllocateTableMask)
  private val t3_allocateWayMask = Mux1H(t3_allocateTableOH, t3_allTableCanAllocateWayMask)
  private val t3_allocateWayOH   = PriorityEncoderOH(t3_allocateWayMask)
  dontTouch(t3_allocateTableOH)
  dontTouch(t3_allocateWayOH)

  private val t3_allocateEntry = {
    val rawTag      = Mux1H(t3_allocateTableOH, t3_rawTag)
    val position    = t3_allocateBranch.bits.cfiPosition
    val actualTaken = t3_allocateBranch.bits.taken
    val entry       = Wire(new TageEntry)
    entry.valid := true.B
    entry.tag   := rawTag ^ position
    entry.takenCtr := Mux(
      actualTaken,
      TakenCounter.WeakPositive,
      TakenCounter.WeakNegative
    )
    entry
  }

  private val t3_usefulResetStart = t3_fire && usefulResetCtr.isSaturatePositive && !usefulResetInFlight

  tables.zipWithIndex.foreach { case (table, tableIdx) =>
    implicit val info: TageTableInfo = TableInfos(tableIdx)

    val writeWayMask    = Wire(Vec(NumWays, Bool()))
    val writeEntryEn    = Wire(Vec(NumWays, Bool()))
    val writeUsefulEn   = Wire(Vec(NumWays, Bool()))
    val writeEntries    = Wire(Vec(NumWays, new TageEntry))
    val writeUsefulCtrs = Wire(Vec(NumWays, UsefulCounter()))
    val actualTakenMask = Wire(Vec(NumWays, Bool()))

    (0 until NumWays).foreach { wayIdx =>
      val (providerWriteCtr, providerWriteUseful, altWriteCtr) = t3_trainInfoVec.map { info =>
        val providerNeedUpdateCtr =
          info.valid && info.needUpdateProviderCtr && info.providerTableOH(tableIdx) && info.providerWayOH(wayIdx)
        val providerNeedUpdateUseful =
          info.valid && info.needUpdateProviderUseful && info.providerTableOH(tableIdx) && info.providerWayOH(wayIdx)
        val altNeedUpdateCtr = info.valid && info.needUpdateAltCtr && info.altTableOH(tableIdx) && info.altWayOH(wayIdx)
        (providerNeedUpdateCtr, providerNeedUpdateUseful, altNeedUpdateCtr)
      }.unzip3

      val hitProvider = providerWriteCtr.reduce(_ || _) || providerWriteUseful.reduce(_ || _)
      val hitProviderMask = (providerWriteCtr zip providerWriteUseful).map {
        case (writeCtr, writeUseful) =>
          writeCtr || writeUseful
      }
      val hitAlt = altWriteCtr.reduce(_ || _)
      when(t3_fire) {
        assert(PopCount(hitProviderMask) <= 1.U)
        assert(PopCount(altWriteCtr) <= 1.U)
        assert(!(hitProvider && hitAlt))
      }

      val providerInfo = Mux1H(hitProviderMask, t3_trainInfoVec)
      val altInfo      = Mux1H(altWriteCtr, t3_trainInfoVec)

      val updateEn                = hitProvider || hitAlt
      val updateEntry             = Mux(hitProvider, providerInfo.providerEntry, altInfo.altEntry)
      val updateUsefulCtr         = Mux(hitProvider, providerInfo.providerNewUsefulCtr, altInfo.altOldUsefulCtr)
      val updateBranchActualTaken = Mux(hitProvider, providerInfo.actualTaken, altInfo.actualTaken)

      val allocateEn = t3_allocate && t3_allocateTableOH(tableIdx) && t3_allocateWayOH(wayIdx)

      writeWayMask(wayIdx)    := updateEn || allocateEn
      writeEntryEn(wayIdx)    := providerWriteCtr.reduce(_ || _) || hitAlt || allocateEn
      writeUsefulEn(wayIdx)   := providerWriteUseful.reduce(_ || _) || allocateEn
      writeEntries(wayIdx)    := Mux(allocateEn, t3_allocateEntry, updateEntry)
      writeUsefulCtrs(wayIdx) := Mux(allocateEn, UsefulCounter.Init, updateUsefulCtr)
      actualTakenMask(wayIdx) := Mux(allocateEn, t3_allocateBranch.bits.taken, updateBranchActualTaken)
    }

    table.io.writeReq.valid                := t3_fire && writeWayMask.reduce(_ || _)
    table.io.writeReq.bits.setIdx          := t3_setIdx(tableIdx)
    table.io.writeReq.bits.bankMask        := t3_bankMask
    table.io.writeReq.bits.wayMask         := writeWayMask.asUInt
    table.io.writeReq.bits.writeEntryEn    := writeEntryEn
    table.io.writeReq.bits.writeUsefulEn   := writeUsefulEn
    table.io.writeReq.bits.entries         := writeEntries
    table.io.writeReq.bits.usefulCtrs      := writeUsefulCtrs
    table.io.writeReq.bits.actualTakenMask := actualTakenMask

    table.io.usefulResetStart := t3_usefulResetStart
  }

  when(t3_usefulResetStart) {
    usefulResetInFlight := true.B
  }.elsewhen(usefulResetInFlight && !tables.map(_.io.usefulResetInFlight).reduce(_ || _)) {
    usefulResetInFlight := false.B
  }

  when(t3_usefulResetStart) {
    usefulResetCtr.resetZero()
  }.elsewhen(t3_fire && t3_needAllocate && !t3_canAllocate && !usefulResetInFlight) {
    usefulResetCtr.selfIncrease()
  }

  useAltOnNaVec.zipWithIndex.map { case (ctr, i) =>
    val idxMatchMask = t3_cfiUseAltOnNaIdxVec.map(_ === i.U)
    val increaseMask = idxMatchMask.zip(t3_trainInfoVec).map { case (idxMatch, updateInfo) =>
      idxMatch && updateInfo.valid && updateInfo.incUseAltOnNa
    }
    val decreaseMask = idxMatchMask.zip(t3_trainInfoVec).map { case (idxMatch, updateInfo) =>
      idxMatch && updateInfo.valid && updateInfo.decUseAltOnNa
    }
    val increase = increaseMask.reduce(_ || _)
    val decrease = decreaseMask.reduce(_ || _)

    when(t3_fire) {
      assert(PopCount(increaseMask) <= 1.U)
      assert(PopCount(decreaseMask) <= 1.U)
      assert(!(increase && decrease))

      when(increase) {
        ctr.selfIncrease()
      }.elsewhen(decrease) {
        ctr.selfDecrease()
      }
    }
  }

  /* --------------------------------------------------------------------------------------------------------------
     TAGE Trace
     -------------------------------------------------------------------------------------------------------------- */

  private val condTraceVec = Wire(Vec(ResolveEntryBranchNumber, new ConditionalBranchTrace))
  condTraceVec.zipWithIndex.foreach { case (trace, i) =>
    trace.isCond  := t3_branches(i).valid && t3_branches(i).bits.attribute.isConditional
    trace.mbtbHit := t3_mbtbHitMask(i)
    trace.useMeta := t3_useMeta

    trace.startPc := t3_startPc
    trace.cfiPc   := t3_branches(i).bits.debug_realCfiPc.getOrElse(0.U(VAddrBits.W))

    trace.hasProvider       := t3_trainInfoVec(i).hasProvider
    trace.useProvider       := t3_trainInfoVec(i).useProvider
    trace.providerTableIdx  := OHToUInt(t3_trainInfoVec(i).providerTableOH)
    trace.providerSetIdx    := t3_setIdx(trace.providerTableIdx)
    trace.providerWayIdx    := OHToUInt(t3_trainInfoVec(i).providerWayOH)
    trace.providerTakenCtr  := t3_trainInfoVec(i).providerEntry.takenCtr
    trace.providerUsefulCtr := t3_trainInfoVec(i).providerOldUsefulCtr

    trace.hasAlt       := t3_trainInfoVec(i).hasAlt
    trace.useAlt       := t3_trainInfoVec(i).useAlt
    trace.altTableIdx  := OHToUInt(t3_trainInfoVec(i).altTableOH)
    trace.altSetIdx    := t3_setIdx(trace.altTableIdx)
    trace.altWayIdx    := OHToUInt(t3_trainInfoVec(i).altWayOH)
    trace.altTakenCtr  := t3_trainInfoVec(i).altEntry.takenCtr
    trace.altUsefulCtr := t3_trainInfoVec(i).altOldUsefulCtr

    trace.finalPred   := t3_trainInfoVec(i).finalPred
    trace.actualTaken := t3_branches(i).bits.taken
    trace.mispredict  := t3_branches(i).bits.mispredict

    trace.needAllocate     := t3_trainInfoVec(i).needAllocate
    trace.allocateSuccess  := t3_trainInfoVec(i).needAllocate && t3_canAllocate
    trace.allocateFailure  := t3_trainInfoVec(i).needAllocate && !t3_canAllocate
    trace.allocateTableIdx := OHToUInt(t3_allocateTableOH)
    trace.allocateSetIdx   := t3_setIdx(trace.allocateTableIdx)
    trace.allocateWayIdx   := OHToUInt(t3_allocateWayOH)
  }

  private val tageTraceDBTables = (0 until ResolveEntryBranchNumber).map { i =>
    ChiselDB.createTable(s"CondTrace_${i}", new ConditionalBranchTrace, EnableTageTrace)
  }
  tageTraceDBTables.zip(condTraceVec).foreach { case (dbTable, condTrace) =>
    dbTable.log(
      data = condTrace,
      en = t3_fire && condTrace.isCond,
      clock = clock,
      reset = reset
    )
  }

  /* --------------------------------------------------------------------------------------------------------------
     performance counter
     -------------------------------------------------------------------------------------------------------------- */

  XSPerfAccumulate(
    "predict_cond_num", {
      val condMask = s2_branches.map(branch => branch.valid && branch.bits.attribute.isConditional)
      Mux(io.stageCtrl.s2_fire, PopCount(condMask), 0.U)
    }
  )
  XSPerfAccumulate("total_train", io.stageCtrl.t0_fire)
  XSPerfAccumulate("train_has_cond", t0_fire)
  XSPerfAccumulate("read_conflict", debug_readBankConflict)
  XSPerfAccumulate("reset_useful", t3_usefulResetStart)
  XSPerfAccumulate(
    "allocate_not_needed_due_to_already_on_highest_table", {
      val mispredictBranchOH = PriorityEncoderOH(t3_trainInfoVec.map(b => b.valid && b.mispredicted))
      val trainInfo          = Mux1H(mispredictBranchOH, t3_trainInfoVec)
      t3_fire && trainInfo.valid && trainInfo.mispredicted &&
      trainInfo.hasProvider && trainInfo.providerTableOH(NumTables - 1)
    }
  )
  XSPerfAccumulate("allocate_needed", t3_fire && t3_needAllocate)
  XSPerfAccumulate("allocate_success", t3_fire && t3_allocate)
  XSPerfAccumulate("allocate_failure", t3_fire && t3_needAllocate && !t3_canAllocate)
  for (i <- 0 until NumTables) {
    XSPerfAccumulate(s"table_${i}_allocate", t3_fire && t3_allocate && t3_allocateTableOH(i))
    XSPerfAccumulate(
      s"allocate_branch_provider_is_table_${i}",
      t3_fire && t3_allocateBranchTrainInfo.hasProvider && t3_allocateBranchTrainInfo.providerTableOH(i)
    )
  }
  XSPerfAccumulate(
    "mispredict_diff",
    Mux(
      t3_fire,
      PopCount(t3_trainInfoVec.map(info => info.valid && (info.mispredicted =/= info.newestMispredict))),
      0.U
    )
  )
  XSPerfAccumulate(
    "total_cond_mispredicted",
    t3_fire && t3_trainInfoVec.map(e => e.valid && e.mispredicted).reduce(_ || _)
  )
  XSPerfAccumulate(
    "total_all_br_mispredicted",
    t0_branches.map(b => io.stageCtrl.t0_fire && b.valid && b.bits.mispredict).reduce(_ || _)
  )
  XSPerfAccumulate(
    "mispredict_branch_has_provider",
    t3_trainInfoVec.map(e => t3_fire && e.valid && e.mispredicted && e.hasProvider).reduce(_ || _)
  )
  XSPerfAccumulate(
    "mispredict_branch_use_provider",
    t3_trainInfoVec.map(e => t3_fire && e.valid && e.mispredicted && e.useProvider).reduce(_ || _)
  )
  XSPerfAccumulate(
    "mispredict_branch_has_alt",
    t3_trainInfoVec.map(e => t3_fire && e.valid && e.mispredicted && e.hasAlt).reduce(_ || _)
  )
  XSPerfAccumulate(
    "mispredict_branch_use_alt",
    t3_trainInfoVec.map(e => t3_fire && e.valid && e.mispredicted && e.useAlt).reduce(_ || _)
  )
  XSPerfAccumulate(
    "mispredict_branch_use_base_table",
    t3_trainInfoVec.map(e => t3_fire && e.valid && e.mispredicted && !e.useProvider && !e.useAlt).reduce(_ || _)
  )
  XSPerfAccumulate(
    "resolve_branch_has_provider",
    t3_trainInfoVec.map(e => (t3_fire && e.valid && e.hasProvider).asUInt).reduce(_ +& _)
  )
  XSPerfAccumulate(
    "resolve_branch_use_provider",
    t3_trainInfoVec.map(e => (t3_fire && e.valid && e.useProvider).asUInt).reduce(_ +& _)
  )
  XSPerfAccumulate(
    "resolve_branch_has_alt",
    t3_trainInfoVec.map(e => (t3_fire && e.valid && e.hasAlt).asUInt).reduce(_ +& _)
  )
  XSPerfAccumulate(
    "resolve_branch_use_alt",
    t3_trainInfoVec.map(e => (t3_fire && e.valid && e.useAlt).asUInt).reduce(_ +& _)
  )
  XSPerfAccumulate(
    "resolve_branch_use_base_table",
    t3_trainInfoVec.map(e => (t3_fire && e.valid && !e.useProvider && !e.useAlt).asUInt).reduce(_ +& _)
  )

  /*
  sum -> total bubbles caused by read bank conflict
  sampled -> total times of read bank conflict happened
   */
  XSPerfHistogram(
    "read_conflict_bubble_dist",
    debug_readBankConflictDistCnt,
    debug_readBankConflictNeg,
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
    debug_readBankConflictShortLoopDistCnt,
    debug_readBankConflictShortLoopNeg,
    0,
    16
  )
}

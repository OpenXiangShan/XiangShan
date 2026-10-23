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
import utility.ParallelORR
import utility.XSPerfAccumulate
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.HalfAlignHelper
import xiangshan.frontend.bpu.TageTableInfo
import xiangshan.frontend.bpu.TrainingBuffer

/**
 * This module is the implementation of the TAGE (TAgged GEometric history length predictor).
 */
class Tage(implicit p: Parameters) extends BasePredictor with HasTageParameters with TopHelper with HalfAlignHelper {
  class TageIO(implicit p: Parameters) extends BasePredictorIO {
    val fromPhr:     PhrToTageIO     = new PhrToTageIO
    val fromMainBtb: MainBtbToTageIO = new MainBtbToTageIO
    val toSc:        TageToScIO      = new TageToScIO
    val prediction:  TagePrediction  = Output(new TagePrediction)
    val meta:        TageMeta        = Output(new TageMeta)
    val holdPredict: Bool            = Output(Bool())
    val s0_valid:    Bool            = Input(Bool())
  }
  val io: TageIO = IO(new TageIO)

  /* *** submodules *** */
  private val tables = TableInfos.zipWithIndex.map { case (info, i) => Module(new TageTable(i, info)) }

  // reset all usefulCtr when usefulResetCtr saturated
  private val usefulResetCtr      = RegInit(UsefulResetCounter.Zero)
  private val usefulResetInFlight = RegInit(false.B)

  // use the alternate prediction when counter is positive
  private val useAltOnNaVec = RegInit(VecInit.fill(NumUseAltOnNa)(UseAltOnNaCounter.WeakNegative))

  /* *** reset *** */
  io.sramResetDone := tables.map(_.io.sramResetDone).reduce(_ && _)

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 0
     - send read request to tables
     -------------------------------------------------------------------------------------------------------------- */

  private val s0_fire    = io.stageCtrl.s0_fire && io.enable
  private val s0_startPc = io.startPc.unGuard

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
     - compute tags
     -------------------------------------------------------------------------------------------------------------- */

  private val s1_fire       = io.stageCtrl.s1_fire && io.enable
  private val s1_startPc    = RegEnable(s0_startPc, s0_fire)
  private val s1_foldedHist = RegEnable(s0_foldedHist, s0_fire)
  private val s1_readResp   = tables.map(table => DataHoldBypass(table.io.readResp(0), RegNext(s0_fire)))

  private val s1_tags = VecInit(io.fromMainBtb.s1_positions.map { position =>
    VecInit(tables.zip(s1_foldedHist).map { case (table, hist) =>
      table.getTag(s1_startPc, hist.forTag, position)
    })
  })

  private val s1_hits = VecInit(s1_tags.map { branchTags =>
    VecInit(s1_readResp.zip(branchTags).map { case (tableReadResp, tag) =>
      tableReadResp.entries.map(entry => entry.valid && entry.tag === tag).asUInt
    })
  })

  /* --------------------------------------------------------------------------------------------------------------
     predict pipeline stage 2
     - get results from mbtb
     - generate the fast provider candidate for each table way
     - select the hit way and provider
     - select the alternate and complete the prediction for each branch
     -------------------------------------------------------------------------------------------------------------- */

  private val s2_fire     = io.stageCtrl.s2_fire && io.enable
  private val s2_hits     = RegEnable(s1_hits, s1_fire)
  private val s2_readResp = s1_readResp.map(resp => RegEnable(resp, s1_fire))
  private val s2_branches = io.fromMainBtb.result

  s2_branches.zipWithIndex.foreach { case (branch, i) =>
    val allTableTagMatchResults = s2_readResp.zipWithIndex.map { case (tableReadResp, tableIdx) =>
      val hitWayMask   = s2_hits(i)(tableIdx)
      val hitWayMaskOH = PriorityEncoderOH(hitWayMask)

      val result = Wire(new TagMatchResult)
      result.hit          := hitWayMask.orR
      result.hitWayMaskOH := hitWayMaskOH.asUInt
      result.tag          := 0.U
      result.takenCtr     := Mux1H(hitWayMaskOH, tableReadResp.entries.map(_.takenCtr))
      result.usefulCtr    := Mux1H(hitWayMaskOH, tableReadResp.usefulCtrs)
      result.hitWayMask   := hitWayMask.asUInt
      result
    }

    // find the provider, the table with the longest history among the hit tables
    val hitTableMask    = VecInit(allTableTagMatchResults.map(_.hit)).asUInt
    val hasProvider     = ParallelORR(hitTableMask)
    val providerTableOH = Reverse(PriorityEncoderOH(Reverse(hitTableMask)))
    val provider        = Mux1H(providerTableOH, allTableTagMatchResults)
    val providerTaken   = provider.takenCtr.isPositive

    // find the alt, the table with the second longest history among the hit tables
    val hitTableMaskNoProvider = hitTableMask & (~providerTableOH)
    val hasAlt                 = hasProvider && ParallelORR(hitTableMaskNoProvider)
    val altTableOH             = Reverse(PriorityEncoderOH(Reverse(hitTableMaskNoProvider)))
    val alt                    = Mux1H(altTableOH, allTableTagMatchResults)
    val altTaken               = alt.takenCtr.isPositive

    val altConf          = hasAlt && !alt.takenCtr.isWeak
    val providerTableIdx = OHToUInt(providerTableOH)
    val useAltOnNaIdx    = Cat(providerTableIdx, altConf)
    val useAltOnNa       = useAltOnNaVec(useAltOnNaIdx).isPositive

    // If the entry is recognized as a newly allocated entry and USE_ALT_ON_NA is positive,
    // use the alternate prediction.
    val useProvider = hasProvider && !(useAltOnNa && provider.takenCtr.isWeak)
    val tageTaken = MuxCase(
      false.B,
      Seq(
        useProvider -> providerTaken,
        hasAlt      -> altTaken
      )
    )

    io.prediction.fastTakenVec(i).valid := useProvider && io.enable
    io.prediction.fastTakenVec(i).bits  := providerTaken

    io.prediction.takenVec(i).valid := (useProvider || hasAlt) && io.enable
    io.prediction.takenVec(i).bits  := tageTaken

    io.toSc.providerTakenCtrVec(i).valid := (hasProvider && branch.valid) && io.enable
    io.toSc.providerTakenCtrVec(i).bits  := provider.takenCtr

    io.meta.entries(i).useProvider       := useProvider
    io.meta.entries(i).hasProvider       := hasProvider
    io.meta.entries(i).hasAlt            := hasAlt
    io.meta.entries(i).providerTableIdx  := providerTableIdx
    io.meta.entries(i).providerWayIdx    := OHToUInt(provider.hitWayMaskOH)
    io.meta.entries(i).providerTakenCtr  := provider.takenCtr
    io.meta.entries(i).providerUsefulCtr := provider.usefulCtr
    io.meta.entries(i).altTableIdx       := OHToUInt(altTableOH)
    io.meta.entries(i).altWayIdx         := OHToUInt(alt.hitWayMaskOH)
    io.meta.entries(i).altTakenCtr       := alt.takenCtr

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

  private val incomingFoldedHist = getFoldedHist(io.fromPhr.foldedPathHistForTrain)
  private val incomingBranchMeta = VecInit(io.train.bits.branches.map { branch =>
    val mbtbMeta  = io.train.bits.meta.mbtb.entries.flatten
    val hitMask   = mbtbMeta.map(_.hit(branch.bits))
    val hitMaskOH = PriorityEncoderOH(hitMask)
    val result    = Wire(new TrainBranchMeta)
    result.mbtbHit := hitMask.reduce(_ || _)
    result.baseCtr := Mux1H(hitMaskOH, mbtbMeta.map(_.counter))
    result.meta    := Mux1H(hitMaskOH, io.train.bits.meta.tage.entries)
    result
  })
  private val incomingReadReq = Wire(new Bundle {
    val setIdx     = MixedVec(TableInfos.map(info => UInt(log2Ceil(info.getNumSets(NumBanks)).W)))
    val rawTag     = Vec(NumTables, UInt(TagWidth.W))
    val branchMeta = Vec(ResolveEntryBranchNumber, new TrainBranchMeta)
  })
  incomingReadReq.setIdx := MixedVecInit(tables.zip(incomingFoldedHist).map { case (table, hist) =>
    table.getSetIndex(io.train.bits.startPc, hist.forIdx)
  })
  incomingReadReq.rawTag := VecInit(tables.zip(incomingFoldedHist).map { case (table, hist) =>
    table.getRawTag(io.train.bits.startPc, hist.forTag)
  })
  incomingReadReq.branchMeta := incomingBranchMeta
  private val trainingBuffer = Module(new TrainingBuffer(
    chiselTypeOf(incomingReadReq),
    NumBanks,
    TrainingBufferSize
  )).suggestName("TageTrainingBuffer")
  private val incomingIsCondMask = io.train.bits.branches.map(b => b.valid && b.bits.attribute.isConditional)
  private val incomingHasCond    = incomingIsCondMask.reduce(_ || _)

  private val incomingNeedRead = io.train.bits.branches.zipWithIndex.map { case (branch, i) =>
    incomingIsCondMask(i) && branch.bits.mispredict
  }.reduce(_ || _)

  trainingBuffer.io.enq.valid          := io.enable && io.train.valid && incomingHasCond
  trainingBuffer.io.enq.bits.train     := io.train.bits
  trainingBuffer.io.enq.bits.readReq   := incomingReadReq
  trainingBuffer.io.enq.bits.bankIdx   := tables.head.getBankIndex(io.train.bits.startPc)
  trainingBuffer.io.enq.bits.needRead  := incomingNeedRead
  trainingBuffer.io.predictReadValid   := s0_fire
  trainingBuffer.io.predictReadBankIdx := s0_bankIdx

  private val t0_fire     = trainingBuffer.io.deq.valid
  private val t0_train    = trainingBuffer.io.deq.bits.train
  private val t0_startPc  = t0_train.startPc
  private val t0_branches = t0_train.branches
  // currently all tables share the same bank index
  private val t0_bankIdx  = trainingBuffer.io.deq.bits.bankIdx
  private val t0_bankMask = UIntToOH(t0_bankIdx, NumBanks)

  private val t0_branchMeta = trainingBuffer.io.deq.bits.readReq.branchMeta

  private val t0_needRead = trainingBuffer.io.deq.bits.needRead
  private val t0_useMeta  = !t0_needRead

  private val t0_setIdx = trainingBuffer.io.deq.bits.readReq.setIdx
  private val t0_rawTag = trainingBuffer.io.deq.bits.readReq.rawTag

  tables.zipWithIndex.foreach { case (table, tableIdx) =>
    table.io.readReq(1).valid         := t0_fire && t0_needRead
    table.io.readReq(1).bits.setIdx   := t0_setIdx(tableIdx)
    table.io.readReq(1).bits.bankMask := t0_bankMask
  }

  // Decide whether to hold prediction from the buffer state, before read arbitration.
  io.holdPredict := trainingBuffer.io.full
  XSPerfAccumulate("tage_force_train", io.s0_valid && io.holdPredict)

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 1
     - get read data from tables
     - compute temp tag
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_fire     = RegNext(t0_fire, init = false.B)
  private val t1_startPc  = RegEnable(t0_startPc, t0_fire)
  private val t1_branches = RegEnable(t0_branches, t0_fire)

  private val t1_setIdx   = RegEnable(t0_setIdx, t0_fire)
  private val t1_rawTag   = RegEnable(t0_rawTag, t0_fire)
  private val t1_bankMask = RegEnable(t0_bankMask, t0_fire)

  private val t1_useMeta    = RegEnable(t0_useMeta, t0_fire)
  private val t1_branchMeta = RegEnable(t0_branchMeta, t0_fire)

  private val t1_readResp = tables.map(_.io.readResp(1))

  /* --------------------------------------------------------------------------------------------------------------
    train pipeline stage 2
    - generate train info for each branch
     -------------------------------------------------------------------------------------------------------------- */

  private val t2_fire     = RegNext(t1_fire, init = false.B)
  private val t2_branches = RegEnable(t1_branches, t1_fire)
  private val t2_startPc  = RegEnable(t1_startPc, t1_fire)

  private val t2_setIdx   = RegEnable(t1_setIdx, t1_fire)
  private val t2_bankMask = RegEnable(t1_bankMask, t1_fire)
  private val t2_rawTag   = RegEnable(t1_rawTag, t1_fire)
  private val t2_readResp = t1_readResp.map(resp => RegEnable(resp, t1_fire))

  private val t2_useMeta    = RegEnable(t1_useMeta, t1_fire)
  private val t2_branchMeta = RegEnable(t1_branchMeta, t1_fire)

  private val t2_trainInfoVec = VecInit(t2_branches.zipWithIndex.map { case (branch, i) =>
    val isCond      = branch.valid && branch.bits.attribute.isConditional
    val meta        = t2_branchMeta(i).meta
    val position    = branch.bits.cfiPosition
    val actualTaken = branch.bits.taken

    val allTableTagMatchResults = t2_readResp.zipWithIndex.map { case (tableReadResp, tableIdx) =>
      val tag          = t2_rawTag(tableIdx) ^ position
      val hitWayMask   = tableReadResp.entries.map(entry => entry.valid && entry.tag === tag)
      val hitWayMaskOH = PriorityEncoderOH(hitWayMask)

      val result = Wire(new TagMatchResult).suggestName(s"t2_branch_${i}_table_${tableIdx}_result")
      result.hit          := hitWayMask.reduce(_ || _)
      result.hitWayMaskOH := hitWayMaskOH.asUInt
      result.tag          := tag
      result.takenCtr     := Mux1H(hitWayMaskOH, tableReadResp.entries.map(_.takenCtr))
      result.usefulCtr    := Mux1H(hitWayMaskOH, tableReadResp.usefulCtrs)
      result.hitWayMask   := 0.U
      result
    }
    val hitTableMask = allTableTagMatchResults.map(_.hit).asUInt

    val hasProvider     = Wire(Bool())
    val providerTableOH = Wire(UInt(NumTables.W))
    val provider        = Wire(new TagMatchResult)

    val hasAlt     = Wire(Bool())
    val altTableOH = Wire(UInt(NumTables.W))
    val alt        = Wire(new TagMatchResult)

    when(t2_useMeta) {
      hasProvider     := meta.hasProvider
      providerTableOH := Mux(meta.hasProvider, UIntToOH(meta.providerTableIdx, NumTables), 0.U)

      provider.hit          := meta.hasProvider
      provider.hitWayMaskOH := Mux(meta.hasProvider, UIntToOH(meta.providerWayIdx, MaxNumWays), 0.U)
      provider.tag          := t2_rawTag(meta.providerTableIdx) ^ position
      provider.takenCtr     := meta.providerTakenCtr
      provider.usefulCtr    := meta.providerUsefulCtr
      provider.hitWayMask   := 0.U

      hasAlt           := meta.hasAlt
      altTableOH       := Mux(meta.hasAlt, UIntToOH(meta.altTableIdx, NumTables), 0.U)
      alt.hit          := meta.hasAlt
      alt.hitWayMaskOH := Mux(meta.hasAlt, UIntToOH(meta.altWayIdx, MaxNumWays), 0.U)
      alt.tag          := t2_rawTag(meta.altTableIdx) ^ position
      alt.takenCtr     := meta.altTakenCtr
      alt.usefulCtr    := UsefulCounter.Zero
      alt.hitWayMask   := 0.U
    }.otherwise { // use result from sram read resp
      hasProvider     := ParallelORR(hitTableMask)
      providerTableOH := Reverse(PriorityEncoderOH(Reverse(hitTableMask)))
      provider        := Mux1H(providerTableOH, allTableTagMatchResults)

      val hitTableMaskNoProvider = hitTableMask & (~providerTableOH)
      hasAlt     := hasProvider && ParallelORR(hitTableMaskNoProvider)
      altTableOH := Reverse(PriorityEncoderOH(Reverse(hitTableMaskNoProvider)))
      alt        := Mux1H(altTableOH, allTableTagMatchResults)
    }

    val providerTaken = provider.takenCtr.isPositive
    val altTaken      = alt.takenCtr.isPositive

    val altConf       = hasAlt && !alt.takenCtr.isWeak
    val useAltOnNaIdx = Cat(OHToUInt(providerTableOH), altConf)
    val useAltOnNa    = useAltOnNaVec(useAltOnNaIdx).isPositive
    val useProvider = Mux(
      t2_useMeta,
      meta.useProvider,
      hasProvider && !(useAltOnNa && provider.takenCtr.isWeak)
    )
    val useAlt = !useProvider && hasAlt
    val tageTaken = MuxCase(
      false.B,
      Seq(
        useProvider -> providerTaken,
        hasAlt      -> altTaken
      )
    )

    val providerNewTakenCtr = provider.takenCtr.getUpdate(actualTaken)
    val altNewTakenCtr      = alt.takenCtr.getUpdate(actualTaken)

    val incProviderUsefulCtr = hasProvider && providerTaken === actualTaken && providerTaken =/= altTaken
    val providerNewUsefulCtr = provider.usefulCtr.getIncrease(en = incProviderUsefulCtr)

    // Allocate on a misprediction, unless:
    //   1. the provider is already in the table with the longest history, or
    //   2. the provider predicted correctly but was not used because it is weak.
    val needAllocate = branch.bits.mispredict && (tageTaken =/= actualTaken) &&
      !(hasProvider && providerTableOH(NumTables - 1)) &&
      !(hasProvider && !useProvider && providerTaken === actualTaken && provider.takenCtr.isWeak)

    val needUpdateProviderCtr    = !provider.takenCtr.shouldHold(actualTaken) && hasProvider
    val needUpdateProviderUseful = !provider.usefulCtr.isSaturatePositive && incProviderUsefulCtr && hasProvider

    val needUpdateAltCtr = !alt.takenCtr.shouldHold(actualTaken) && useAlt

    // Train the selector only when it can change the prediction.
    // When the weak provider and alternate/base disagree, exactly one of them is correct.
    val trainUseAltOnNa = hasProvider && hasAlt && provider.takenCtr.isWeak && providerTaken =/= altTaken
    val incUseAltOnNa   = trainUseAltOnNa && altTaken === actualTaken
    val decUseAltOnNa   = trainUseAltOnNa && altTaken =/= actualTaken

    val trainInfo = Wire(new TrainInfo).suggestName(s"t2_branch_${i}_trainInfo")
    trainInfo.valid := isCond

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
    trainInfo.useAltOnNaIdx := useAltOnNaIdx

    trainInfo.tageTaken   := tageTaken
    trainInfo.actualTaken := actualTaken

    trainInfo.hitTableMask     := hitTableMask.asUInt
    trainInfo.mispredicted     := branch.bits.mispredict
    trainInfo.newestMispredict := tageTaken =/= actualTaken
    trainInfo
  })

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 3
     - update branches' takenCtr and usefulCtr
     - allocate a new entry when mispredict
     -------------------------------------------------------------------------------------------------------------- */

  private val t3_fire         = RegNext(t2_fire, init = false.B)
  private val t3_branches     = RegEnable(t2_branches, t2_fire)
  private val t3_startPc      = RegEnable(t2_startPc, t2_fire)
  private val t3_setIdx       = RegEnable(t2_setIdx, t2_fire)
  private val t3_bankMask     = RegEnable(t2_bankMask, t2_fire)
  private val t3_rawTag       = RegEnable(t2_rawTag, t2_fire)
  private val t3_readResp     = t2_readResp.map(resp => RegEnable(resp, t2_fire))
  private val t3_useMeta      = RegEnable(t2_useMeta, t2_fire)
  private val t3_mbtbHitMask  = RegEnable(VecInit(t2_branchMeta.map(_.mbtbHit)), t2_fire)
  private val t3_trainInfoVec = RegEnable(t2_trainInfoVec, t2_fire)

  private val t3_needAllocateBranchOH = t3_trainInfoVec.map(info => info.valid && info.needAllocate)
  when(t3_fire) {
    assert(PopCount(t3_needAllocateBranchOH) <= 1.U)
  }
  private val t3_needAllocate            = t3_needAllocateBranchOH.reduce(_ || _)
  private val t3_allocateBranch          = Mux1H(t3_needAllocateBranchOH, t3_branches)
  private val t3_allocateBranchTrainInfo = Mux1H(t3_needAllocateBranchOH, t3_trainInfoVec)
  when(t3_fire) {
    assert(!(t3_needAllocate && t3_useMeta), "allocate requires a training SRAM read")
  }

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

  // Keep a 15-bit maximal-period LFSR (period 2^15 - 1) even though only the low NumTables bits are used.
  // The longer period avoids short-period correlation with allocation/training events at negligible hardware cost.
  // Its taps are {15, 14}, requiring only one feedback XOR; a shorter 8-bit maximal-period LFSR uses four taps.
  // Randomly filter allocation candidates, then prefer the shorter-history table among the remaining candidates.
  // Fall back to the original candidate mask when the random mask filters out every candidate.
  require(NumTables <= 15, s"TAGE NumTables ($NumTables) must be less than the 15-bit LFSR width")
  private val t3_allocateTableRandomMask    = random.LFSR(width = 15)(NumTables - 1, 0)
  private val t3_randomCanAllocateTableMask = t3_canAllocateTableMask & t3_allocateTableRandomMask
  private val t3_preferredAllocateTableMask = Mux(
    t3_randomCanAllocateTableMask.orR,
    t3_randomCanAllocateTableMask,
    t3_canAllocateTableMask
  )
  private val t3_allocateTableOH = PriorityEncoderOH(t3_preferredAllocateTableMask)
  private val t3_allocateWayMask = Mux1H(t3_allocateTableOH, t3_allTableCanAllocateWayMask)
  private val t3_allocateWayOH   = PriorityEncoderOH(t3_allocateWayMask)

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

  useAltOnNaVec.zipWithIndex.foreach { case (ctr, i) =>
    val idxMatchMask = t3_trainInfoVec.map(updateInfo => updateInfo.valid && updateInfo.useAltOnNaIdx === i.U)
    val increaseMask = idxMatchMask.zip(t3_trainInfoVec).map { case (idxMatch, updateInfo) =>
      idxMatch && updateInfo.incUseAltOnNa
    }
    val decreaseMask = idxMatchMask.zip(t3_trainInfoVec).map { case (idxMatch, updateInfo) =>
      idxMatch && updateInfo.decUseAltOnNa
    }
    val increaseCount = PopCount(increaseMask)
    val decreaseCount = PopCount(decreaseMask)
    val increase      = increaseCount >= decreaseCount
    val step          = Mux(increase, increaseCount - decreaseCount, decreaseCount - increaseCount)
    val updatedCtr = Mux(
      increase,
      ctr.getIncrease(step, en = step.orR),
      ctr.getDecrease(step, en = step.orR)
    )

    when(t3_fire) {
      ctr := updatedCtr
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

    trace.tageTaken   := t3_trainInfoVec(i).tageTaken
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
  XSPerfAccumulate(
    "fast_final_prediction_mismatch", {
      val mismatchMask = s2_branches.zipWithIndex.map { case (branch, i) =>
        val fastTaken  = Mux(io.prediction.fastTakenVec(i).valid, io.prediction.fastTakenVec(i).bits, branch.bits.taken)
        val finalTaken = Mux(io.prediction.takenVec(i).valid, io.prediction.takenVec(i).bits, branch.bits.taken)
        branch.valid && branch.bits.attribute.isConditional && fastTaken =/= finalTaken
      }
      Mux(s2_fire, PopCount(mismatchMask), 0.U)
    }
  )
  XSPerfAccumulate("total_train", io.train.valid)
  XSPerfAccumulate("train_has_cond", t0_fire)
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
    io.train.bits.branches.map(b => io.train.valid && b.valid && b.bits.mispredict).reduce(_ || _)
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
}

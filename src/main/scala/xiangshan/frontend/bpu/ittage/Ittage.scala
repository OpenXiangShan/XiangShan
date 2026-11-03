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
//
// Acknowledgement
//
// This implementation is inspired by several key papers:
// [1] André Seznec. "[A 64-Kbytes ITTAGE indirect branch predictor.](https://inria.hal.science/hal-00639041)" The
// Journal of Instruction-Level Parallelism (JILP) 2nd JILP Workshop on Computer Architecture Competitions (JWAC):
// Championship Branch Prediction (CBP). 2011.

package xiangshan.frontend.bpu.ittage

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import utility.GTimer
import utility.LowerMask
import utility.ParallelORR
import utility.ParallelSelectTwo
import utility.SelectTwoInterRes
import utility.XSDebug
import utility.XSPerfAccumulate
import utility.XSPerfSeqAccumulate
import xiangshan.XSModule
import xiangshan.frontend.GuardedPc
import xiangshan.frontend.GuardedPcInit
import xiangshan.frontend.PcInit
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.SaturateCounter
import xiangshan.frontend.bpu.Train
import xiangshan.frontend.bpu.WriteBuffer
import xiangshan.frontend.bpu.history.phr.PhrAllFoldedHistories

class Ittage(implicit p: Parameters) extends BasePredictor with HasIttageParameters with Helpers {
  class IttageIO extends BasePredictorIO {
    // prediction bundles
    val s1_foldedPhr:   PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    val trainFoldedPhr: PhrAllFoldedHistories = Input(new PhrAllFoldedHistories(AllFoldedHistoryInfo))
    // Performance-only inputs; neither participates in functional training control.
    val debug_trainValid:      Bool = Input(Bool())
    val debug_trainOtherReady: Bool = Input(Bool())

    val prediction: IttagePrediction = Output(new IttagePrediction)
    val meta:       IttageMeta       = Output(new IttageMeta)
  }

  val io: IttageIO = IO(new IttageIO)

  private val s0_startPc = io.startPc
  private val s0_fire    = io.stageCtrl.s0_fire && io.enable
  private val s1_fire    = io.stageCtrl.s1_fire && io.enable
  private val s2_fire    = io.stageCtrl.s2_fire && io.enable
  private val s3_fire    = io.stageCtrl.s3_fire && io.enable

  private val s1_startPc = RegEnable(s0_startPc, s0_fire)
  private val s2_startPc = RegEnable(s1_startPc, s1_fire)

  // Each ITTAGE table manages its own banking (NumBanks parameter); top-level only supplies PC/history.
  private val tables = TableInfos.zipWithIndex.map {
    case (info, i) =>
      val t = Module(new IttageTable(i, info))
      t
  }

  io.sramResetDone := tables.map(_.io.sramResetDone).reduce(_ && _)

  private val useAltOnNa = RegInit((1 << (UseAltOnNaWidth - 1)).U(UseAltOnNaWidth.W))
  private val tickCnt    = RegInit(TickCounter.Zero)

  private val rTable = Module(new RegionWays)

  // uftb miss or hasIndirect
  // TODO: for low power: use ubtb&abtb and remove this
  // private val s1_uftbHit         = io.fromFtb.s1_uftbHit
  // private val s1_uftbHasIndirect = io.fromFtb.s1_uftbHasIndirect
  private val s1_isIndirect = true.B // (!s1_uftbHit && !io.fromFtb.s1_ftbCloseReq) || s1_uftbHasIndirect

  // Keep the table responses to process in s2

  private val s2_resps = VecInit(tables.map(t => t.io.resp))

  private val s2_ittageTarget = Wire(GuardedPc())
  private val s2_provided     = Wire(Bool())
  private val s2_provider     = Wire(UInt(log2Ceil(NumTables).W))
  private val s2_altProvided  = Wire(Bool())
  private val s2_altProvider  = Wire(UInt(log2Ceil(NumTables).W))
  private val s2_providerCnt  = Wire(ConfidenceCounter())

  private val s3_ittageTarget = RegEnable(s2_ittageTarget, s2_fire)
  private val s3_provided     = RegEnable(s2_provided, s2_fire)
  private val s3_provider     = RegEnable(s2_provider, s2_fire)

  private val ittageMeta = WireDefault(0.U.asTypeOf(new IttageMeta))
  io.meta := ittageMeta

  // Predict
  tables.foreach { t =>
    t.io.req.valid           := s1_fire && s1_isIndirect // TODO: s1_isIndirect for low power
    t.io.req.bits.startPc    := s1_startPc.unGuard
    t.io.req.bits.foldedHist := io.s1_foldedPhr
  }

  // access tag tables and output meta info
  class IttageTableInfo extends Bundle {
    val cnt:          SaturateCounter = ConfidenceCounter()
    val usefulCnt:    SaturateCounter = UsefulCounter()
    val targetOffset: IttageOffset    = new IttageOffset
    val tableIdx:     UInt            = UInt(log2Ceil(NumTables).W)
    val maskTarget:   Vec[UInt]       = Vec(NumTables, UInt(GuardedVAddrBits.W))
  }

  private val inputRes = VecInit(s2_resps.zipWithIndex.map {
    case (r, i) =>
      val tableInfo = Wire(new IttageTableInfo)
      tableInfo.usefulCnt     := r.bits.usefulCnt
      tableInfo.cnt           := r.bits.cnt
      tableInfo.targetOffset  := r.bits.targetOffset
      tableInfo.tableIdx      := i.U(log2Ceil(NumTables).W)
      tableInfo.maskTarget    := VecInit(Seq.fill(NumTables)(0.U(GuardedVAddrBits.W)))
      tableInfo.maskTarget(i) := "hffff_ffff_ffff_ffff".U
      SelectTwoInterRes(r.valid, tableInfo)
  })

  private val selectedInfo = ParallelSelectTwo(inputRes.reverse)
  private val provided     = selectedInfo.hasOne
  private val altProvided  = selectedInfo.hasTwo

  private val providerInfo    = selectedInfo.first
  private val altProviderInfo = selectedInfo.second

  private val s2_useAltPred  = altProvided && providerInfo.cnt.isSaturateNegative
  private val s2_useProvider = provided && !s2_useAltPred

  private val regionReadTargetOffset = VecInit(s2_resps.map(r => r.bits.targetOffset))

  rTable.io.reqPointer.zipWithIndex.foreach { case (req_pointer, i) =>
    req_pointer := regionReadTargetOffset(i).pointer
  }

  // When the entry corresponding to the pointer is valid and does not use PCRegion, use rTable region.
  private val regionTargets = Wire(Vec(NumTables, GuardedPc()))
  for (i <- 0 until NumTables) {
    regionTargets(i) := GuardedPcInit(Mux(
      rTable.io.respHit(i) && !regionReadTargetOffset(i).usePcRegion,
      Cat(
        s2_startPc(GuardedVAddrBits - 1),
        rTable.io.respRegion(i),
        regionReadTargetOffset(i).offset.toUInt
      ),
      Cat(targetGetRegion(s2_startPc), regionReadTargetOffset(i).offset.toUInt)
    ))
  }

  private val providerCatTarget = GuardedPcInit(providerInfo.maskTarget.zipWithIndex.map {
    case (mask, i) => mask & regionTargets(i).toUInt
  }.reduce(_ | _))

  private val altProviderCatTarget = GuardedPcInit(altProviderInfo.maskTarget.zipWithIndex.map {
    case (mask, i) => mask & regionTargets(i).toUInt
  }.reduce(_ | _))

  s2_ittageTarget := MuxCase(
    0.U.asTypeOf(GuardedPc()),
    Seq(
      s2_useProvider -> providerCatTarget,
      s2_useAltPred  -> altProviderCatTarget
    )
  )

  // TODO: for low power: use mbtb and remove this
  // private val s2_predictionValid = io.fromFtb.s2_isJalr &&
  //   (s2_useProvider || s2_useAltPred)
  // private val s2_predictionValid = s2_useProvider || s2_useAltPred

  // TODO: for low power: use mbtb and remove this
  // io.predictionValid := RegEnable(s2_predictionValid, s2_fire)
  io.prediction.hit    := s3_fire && s3_provided
  io.prediction.target := s3_ittageTarget
//  when(io.prediction.hit) {
//    assert(io.prediction.target =/= 0.U.asTypeOf(GuardedPc()))
//  }

  s2_provided    := provided
  s2_provider    := providerInfo.tableIdx
  s2_altProvided := altProvided
  s2_altProvider := altProviderInfo.tableIdx
  s2_providerCnt := providerInfo.cnt

  XSDebug(s2_fire, p"hit_taken_jalr:")

  ittageMeta.provider.valid := s3_provided
  ittageMeta.provider.bits  := s3_provider
  ittageMeta.debug_predCycle.foreach(_ := GTimer())

  /* *** train/update pipeline *** */
  // Select the indirect branch used by ITTAGE training before issuing a
  // training read.  FTQ guarantees that this vector is at most one-hot.
  private val t0_startPc = io.train.startPc
  private val t0_trainBranchMask = VecInit(io.train.branches.map(b =>
    b.valid && b.bits.attribute.needIttage && b.bits.taken
  ))
  private val t0_hasTrainBranch = ParallelORR(t0_trainBranchMask)
  private val t0_meta           = io.train.meta.ittage
  // The mask is at most one-hot, so reduce the selected mispredict bits directly
  // instead of serializing a priority encoder and a dynamic mux on trainReady.
  private val t0_updateMisPred = ParallelORR(t0_trainBranchMask.zip(io.train.branches).map {
    case (selected, branch) => selected && branch.bits.mispredict
  })

  when(io.enable && io.stageCtrl.t0_fire) {
    assert(
      PopCount(t0_trainBranchMask) <= 1.U,
      "ITTAGE training accepts at most one valid, taken indirect branch without a RAS pop"
    )
  }

  // Re-read for every provider update and for allocation after a misprediction.
  // Training selects providers and allocation candidates from the current entries.
  private val t0_needRead = (t0_hasTrainBranch && t0_meta.provider.valid) || t0_updateMisPred

  // ITTAGE prediction reads occur in s1, rather than TAGE's s0.  A bank
  // conflict blocks the FTQ training handshake; prediction keeps priority.
  private val s1_bankIdx = tables.head.getBankIndex(s1_startPc.unGuard)
  private val t0_bankIdx = tables.head.getBankIndex(t0_startPc)
  private val t0_readBankConflict =
    t0_needRead && s1_fire && s1_isIndirect && t0_bankIdx === s1_bankIdx
  io.trainReady := !t0_readBankConflict

  private val t0_fire = io.enable && io.stageCtrl.t0_fire && t0_hasTrainBranch

  tables.foreach { table =>
    table.io.trainReq.valid           := t0_fire && t0_needRead
    table.io.trainReq.bits.startPc    := t0_startPc
    table.io.trainReq.bits.foldedHist := io.trainFoldedPhr
  }

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 1
     - select the provider and alternate from the training read responses
     - collect allocation eligibility independently of provider selection
     -------------------------------------------------------------------------------------------------------------- */

  private val t1_fire            = RegNext(t0_fire, init = false.B)
  private val t1_needRead        = RegEnable(t0_needRead, false.B, t0_fire)
  private val t1_train           = RegEnable(io.train, 0.U.asTypeOf(new Train), t0_fire)
  private val t1_trainFoldedPhr  = RegEnable(io.trainFoldedPhr, t0_fire)
  private val t1_trainBranchMask = RegEnable(t0_trainBranchMask, t0_fire)

  // The highest-numbered matching table has the longest history. Select the
  // provider and alternate again, since entries may have changed since prediction.
  private val t1_resps = VecInit(tables.map(_.io.trainResp))
  private val t1_inputRes = t1_resps.zipWithIndex.map { case (resp, i) =>
    val info = Wire(new IttageTableInfo)
    info              := 0.U.asTypeOf(new IttageTableInfo)
    info.tableIdx     := i.U
    info.cnt          := resp.bits.cnt
    info.usefulCnt    := resp.bits.usefulCnt
    info.targetOffset := resp.bits.targetOffset
    SelectTwoInterRes(resp.valid, info)
  }
  private val t1_selectedInfo    = ParallelSelectTwo(t1_inputRes.reverse)
  private val t1_providerInfo    = t1_selectedInfo.first
  private val t1_altProviderInfo = t1_selectedInfo.second

  private val t1_hasProvider    = t1_needRead && t1_selectedInfo.hasOne
  private val t1_hasAltProvider = t1_needRead && t1_selectedInfo.hasTwo
  private val t1_allocatableMask = VecInit(t1_resps.map(r =>
    !r.valid && r.bits.usefulCnt.isSaturateNegative
  )).asUInt

  /* --------------------------------------------------------------------------------------------------------------
     train pipeline stage 2
     - reconstruct targets and select an allocation candidate
     - generate updates and send them directly to tables
     -------------------------------------------------------------------------------------------------------------- */

  private val t2_fire            = RegNext(t1_fire, init = false.B)
  private val t2_needRead        = RegEnable(t1_needRead, false.B, t1_fire)
  private val t2_train           = RegEnable(t1_train, 0.U.asTypeOf(new Train), t1_fire)
  private val t2_trainFoldedPhr  = RegEnable(t1_trainFoldedPhr, t1_fire)
  private val t2_trainBranchMask = RegEnable(t1_trainBranchMask, t1_fire)
  private val t2_providerInfo    = RegEnable(t1_providerInfo, t1_fire)
  private val t2_altProviderInfo = RegEnable(t1_altProviderInfo, t1_fire)
  private val t2_hasProvider     = RegEnable(t1_hasProvider, false.B, t1_fire)
  private val t2_hasAltProvider  = RegEnable(t1_hasAltProvider, false.B, t1_fire)
  private val t2_allocatableMask = RegEnable(t1_allocatableMask, t1_fire)
  // Keep the allocation random sample aligned with the response transaction.
  private val allocLFSR    = random.LFSR(width = 15)(NumTables - 1, 0)
  private val t2_allocLFSR = RegEnable(allocLFSR, t1_fire)

  private val t2_updateStartPc   = t2_train.startPc
  private val t2_updateFoldedPhr = t2_trainFoldedPhr

  // Dedicated combinational region reads allow training and prediction to
  // reconstruct their targets concurrently, using their own start PCs.
  private val t2_targetOffsets = Seq(t2_providerInfo.targetOffset, t2_altProviderInfo.targetOffset)
  private val t2_targets = t2_targetOffsets.zipWithIndex.map { case (offset, i) =>
    rTable.io.trainReqPointer(i) := offset.pointer
    PcInit(Cat(
      Mux(
        rTable.io.trainRespHit(i) && !offset.usePcRegion,
        rTable.io.trainRespRegion(i),
        targetGetRegion(t2_updateStartPc)
      ),
      offset.offset.toUInt
    ))
  }

  private val t2_providerTarget    = t2_targets.head
  private val t2_altProviderTarget = t2_targets(1)
  private val t2_altDiffers        = t2_hasAltProvider && t2_providerTarget =/= t2_altProviderTarget

  // Allocation also uses the fresh hit/useful state, and only considers tables
  // with a longer history than the newly selected provider.
  private val t2_allocatableSlots = t2_allocatableMask &
    (~(LowerMask(UIntToOH(t2_providerInfo.tableIdx), NumTables) & Fill(NumTables, t2_hasProvider.asUInt))).asUInt
  private val t2_firstEntry    = PriorityEncoder(t2_allocatableSlots)
  private val t2_maskedEntry   = PriorityEncoder(t2_allocatableSlots & t2_allocLFSR)
  private val t2_allocateValid = t2_needRead && t2_allocatableSlots.orR
  private val t2_allocateIdx   = Mux(t2_allocatableSlots(t2_maskedEntry), t2_maskedEntry, t2_firstEntry)

  // Reuse the branch selected in t0, aligned with the training bundle.
  when(t2_fire) {
    assert(
      PopCount(t2_trainBranchMask) === 1.U,
      "An active ITTAGE training transaction must contain exactly one selected branch"
    )
  }
  private val t2_trainBranchIdx: UInt = PriorityEncoder(t2_trainBranchMask)

  // Use aliases for clarity
  private val t2_updateValid = t2_fire

  private val t2_updateMask            = WireInit(0.U.asTypeOf(Vec(NumTables, Bool())))
  private val t2_updateUsefulCntMask   = WireInit(0.U.asTypeOf(Vec(NumTables, Bool())))
  private val t2_updateResetUsefulCnt  = WireInit(false.B)
  private val t2_updateCorrect         = WireInit(0.U.asTypeOf(Vec(NumTables, Bool())))
  private val t2_updateAlloc           = WireInit(0.U.asTypeOf(Vec(NumTables, Bool())))
  private val t2_updateOldCnt          = WireInit(0.U.asTypeOf(Vec(NumTables, ConfidenceCounter())))
  private val t2_updateUsefulCnt       = WireInit(0.U.asTypeOf(Vec(NumTables, UsefulCounter())))
  private val t2_updateTargetOffset    = WireInit(0.U.asTypeOf(Vec(NumTables, new IttageOffset)))
  private val t2_updateOldTargetOffset = WireInit(0.U.asTypeOf(Vec(NumTables, new IttageOffset)))

  // Use the misprediction bits from the training bundle so allocation and
  // certain updates (e.g. alt-provider penalty) only happen on actual mispreds.
  private val t2_updateMisPred = t2_train.branches(t2_trainBranchIdx).bits.mispredict

  // Update in loop
  private val t2_updateRealTarget       = t2_train.branches(t2_trainBranchIdx).bits.target
  private val t2_updatePCRegion         = targetGetRegion(t2_train.startPc)
  private val t2_updateRealTargetRegion = targetGetRegion(t2_updateRealTarget)
  private val t2_updateRealTargetOffset = WireInit(0.U.asTypeOf(new IttageOffset))
  t2_updateRealTargetOffset.offset := targetGetOffset(t2_updateRealTarget)
  private val t2_updateRealUsePCRegion = t2_updateRealTargetRegion === t2_updatePCRegion
  // A zero-confidence provider/alternate replaces its target just like an
  // allocation. Both cases need a valid region pointer for a remote target.
  private val t2_updateWritesTarget = t2_updateMask.indices.map { i =>
    t2_updateMask(i) && (t2_updateAlloc(i) || t2_updateOldCnt(i).isSaturateNegative)
  }.reduce(_ || _)
  t2_updateRealTargetOffset.usePcRegion := t2_updateRealUsePCRegion
  rTable.io.writeValid                  := !t2_updateRealUsePCRegion && t2_updateWritesTarget
  rTable.io.writeRegion                 := t2_updateRealTargetRegion
  t2_updateRealTargetOffset.pointer     := rTable.io.writePointer

  // Retain the original SRAM target encoding when the target does not change.
  rTable.io.updateRegion := VecInit(t2_targets.map(targetGetRegion))

  private val t2_provider    = t2_providerInfo.tableIdx
  private val t2_altProvider = t2_altProviderInfo.tableIdx
  private val t2_useAltPred  = t2_hasAltProvider && t2_providerInfo.cnt.isSaturateNegative
  when(t2_updateValid) {
    when(t2_hasProvider) {
      when(t2_useAltPred && t2_updateMisPred) { // Train the freshly selected alternate.
        t2_updateMask(t2_altProvider)            := true.B
        t2_updateUsefulCntMask(t2_altProvider)   := false.B
        t2_updateCorrect(t2_altProvider)         := t2_altProviderTarget === t2_updateRealTarget
        t2_updateOldCnt(t2_altProvider)          := t2_altProviderInfo.cnt
        t2_updateAlloc(t2_altProvider)           := false.B
        t2_updateTargetOffset(t2_altProvider)    := t2_updateRealTargetOffset
        t2_updateOldTargetOffset(t2_altProvider) := t2_altProviderInfo.targetOffset
      }

      t2_updateMask(t2_provider)          := true.B
      t2_updateUsefulCntMask(t2_provider) := true.B

      t2_updateUsefulCnt(t2_provider) := Mux(
        !t2_altDiffers,
        t2_providerInfo.usefulCnt,
        (t2_providerTarget === t2_updateRealTarget).asTypeOf(UsefulCounter())
      )
      t2_updateCorrect(t2_provider)         := t2_providerTarget === t2_updateRealTarget
      t2_updateOldCnt(t2_provider)          := t2_providerInfo.cnt
      t2_updateAlloc(t2_provider)           := false.B
      t2_updateTargetOffset(t2_provider)    := t2_updateRealTargetOffset
      t2_updateOldTargetOffset(t2_provider) := t2_providerInfo.targetOffset
    }
  }
  XSDebug(
    t2_updateValid && t2_hasProvider,
    p"t2_train provider $t2_provider, pred cycle ${t2_train.meta.ittage.debug_predCycle.getOrElse(0.U)}\n"
  )
  XSDebug(
    t2_updateValid && t2_hasProvider && t2_useAltPred && t2_updateMisPred,
    p"t2_train altprovider $t2_altProvider, pred cycle ${t2_train.meta.ittage.debug_predCycle.getOrElse(0.U)}\n"
  )

  // if mispredicted and not the case that
  // provider offered correct target but used altpred due to unconfident
  private val providerCorrect = t2_hasProvider && t2_providerTarget === t2_updateRealTarget
  private val providerUnconf  = t2_providerInfo.cnt.isSaturateNegative
  // Check whether the target selected from the training re-read is correct.
  // Suppress allocation if the current entries already provide the actual target.
  private val t2_updatePredictionCorrect = t2_hasProvider && Mux(
    t2_useAltPred,
    t2_altProviderTarget === t2_updateRealTarget,
    providerCorrect
  )
  private val needAllocate = t2_updateValid && t2_updateMisPred && !t2_updatePredictionCorrect &&
    !(providerCorrect && providerUnconf)

  when(needAllocate) {
    tickCnt.selfUpdate(!t2_allocateValid)
    when(t2_allocateValid) {
      t2_updateMask(t2_allocateIdx)          := true.B
      t2_updateCorrect(t2_allocateIdx)       := false.B // useless for alloc
      t2_updateAlloc(t2_allocateIdx)         := true.B
      t2_updateUsefulCntMask(t2_allocateIdx) := true.B
      t2_updateUsefulCnt(t2_allocateIdx)     := UsefulCounter.SaturateNegative
      t2_updateTargetOffset(t2_allocateIdx)  := t2_updateRealTargetOffset
    }
  }

  XSDebug(
    needAllocate && t2_allocateValid,
    p"allocate new table entry, pred cycle ${t2_train.meta.ittage.debug_predCycle.getOrElse(0.U)}\n"
  )

  when(tickCnt.isSaturatePositive) {
    tickCnt.resetZero()
    t2_updateResetUsefulCnt := true.B
  }

  // The t1-to-t2 registers already isolate the SRAM response path. Tables
  // accept these requests directly into their per-bank write buffers.
  for (i <- 0 until NumTables) {
    tables(i).io.update.valid           := t2_updateMask(i)
    tables(i).io.update.resetUsefulCnt  := t2_updateResetUsefulCnt
    tables(i).io.update.correct         := t2_updateCorrect(i)
    tables(i).io.update.alloc           := t2_updateAlloc(i)
    tables(i).io.update.oldCnt          := t2_updateOldCnt(i)
    tables(i).io.update.targetOffset    := t2_updateTargetOffset(i)
    tables(i).io.update.oldTargetOffset := t2_updateOldTargetOffset(i)

    tables(i).io.update.usefulCntValid := t2_updateUsefulCntMask(i)
    tables(i).io.update.usefulCnt      := t2_updateUsefulCnt(i)
    tables(i).io.update.startPc        := t2_updateStartPc
    tables(i).io.update.foldedHist     := t2_updateFoldedPhr
  }

  // Prediction-time coverage and selection within ITTAGE. These counters do
  // not imply that the BPU ultimately used the ITTAGE target.
  private def predPerf(name: String, cond: Bool): Unit =
    XSPerfAccumulate(s"ittage_${name}_at_pred", cond && s2_fire)

  predPerf("total", true.B)
  predPerf("provided", s2_provided)
  predPerf("alt_provided", s2_altProvided)
  predPerf("use_provider", s2_useProvider)
  predPerf("use_altpred", s2_useAltPred)
  predPerf("provider_useful", s2_provided && !providerInfo.usefulCnt.isSaturateNegative)

  for (cnt <- 0 until (1 << ConfidenceCntWidth)) {
    predPerf(s"provider_cnt_${cnt}", s2_provided && s2_providerCnt.value === cnt.U)
  }

  for (i <- 0 until NumTables) {
    predPerf(s"table_${i}_provided", s2_provided && s2_provider === i.U)
    predPerf(s"table_${i}_final_provided", s2_useProvider && s2_provider === i.U)
    predPerf(s"table_${i}_alt_provider_as_final_pred", s2_useAltPred && s2_altProvider === i.U)
  }

  // Compare the freshly re-read targets with the actual target. These describe
  // training-time table state, not the accuracy of the original prediction.
  // Transactions without a training read have no table state to classify.
  private def trainPerf(name: String, cond: Bool): Unit =
    XSPerfAccumulate(s"ittage_${name}_at_train", cond && t2_updateValid && t2_needRead)

  private val perf_altProviderCorrect = t2_hasAltProvider && t2_altProviderTarget === t2_updateRealTarget
  trainPerf("provided", t2_hasProvider)
  trainPerf("alt_provided", t2_hasAltProvider)
  trainPerf("provider_correct", providerCorrect)
  trainPerf("altprovider_correct", perf_altProviderCorrect)
  trainPerf("altprovider_better", t2_hasProvider && !providerCorrect && perf_altProviderCorrect)
  trainPerf("provider_better", t2_hasAltProvider && providerCorrect && !perf_altProviderCorrect)

  // Keep only the accepted training/read totals for read overhead. Do not count
  // waiting cycles here; BPU.train_stall_ittage already tracks training stalls.
  XSPerfSeqAccumulate(
    "ittage_train",
    t0_fire,
    Seq(
      ("total", true.B),
      ("need_read", t0_needRead)
    )
  )

  // Compare against the training-read snapshot, not a prediction-time counter
  // or the current WriteBuffer/SRAM contents. These reasons may overlap.
  private val perf_providerCntChange = t2_hasProvider && !t2_providerInfo.cnt.shouldHold(providerCorrect)
  private val perf_providerUsefulChange = t2_hasProvider && t2_altDiffers &&
    t2_providerInfo.usefulCnt.value =/= providerCorrect.asUInt
  private val perf_providerTargetChange = t2_hasProvider && t2_providerInfo.cnt.isSaturateNegative &&
    t2_providerInfo.targetOffset.asUInt =/= t2_updateRealTargetOffset.asUInt
  private val perf_updateAlt    = t2_hasProvider && t2_useAltPred && t2_updateMisPred
  private val perf_altCorrect   = t2_altProviderTarget === t2_updateRealTarget
  private val perf_altCntChange = perf_updateAlt && !t2_altProviderInfo.cnt.shouldHold(perf_altCorrect)
  private val perf_altTargetChange = perf_updateAlt && t2_altProviderInfo.cnt.isSaturateNegative &&
    t2_altProviderInfo.targetOffset.asUInt =/= t2_updateRealTargetOffset.asUInt
  private val perf_entryChange =
    (needAllocate && t2_allocateValid) || perf_providerCntChange || perf_providerUsefulChange ||
      perf_providerTargetChange || perf_altCntChange || perf_altTargetChange
  // A failed allocation still trains tickCnt; a region write is also useful work.
  // Independent background useful resets are excluded from this transaction.
  private val perf_readNoop = !perf_entryChange && !needAllocate && !rTable.io.writeValid
  private val perf_providerNoChange = providerCorrect && t2_providerInfo.cnt.isSaturatePositive &&
    !perf_providerUsefulChange

  // FTQ keeps a pending payload stable until acceptance, or withdraws valid
  // when flushing it. Reset on either event so waits cannot leak across requests.
  private val perf_t0ReadStall            = io.enable && io.debug_trainValid && t0_readBankConflict
  private val perf_t0ExclusiveReadStall   = perf_t0ReadStall && io.debug_trainOtherReady
  private val perf_t0StallCycles          = RegInit(0.U(XLEN.W))
  private val perf_t0ExclusiveStallCycles = RegInit(0.U(XLEN.W))
  when(!io.debug_trainValid || io.stageCtrl.t0_fire) {
    perf_t0StallCycles          := 0.U
    perf_t0ExclusiveStallCycles := 0.U
  }.otherwise {
    when(perf_t0ReadStall)(perf_t0StallCycles                   := perf_t0StallCycles + 1.U)
    when(perf_t0ExclusiveReadStall)(perf_t0ExclusiveStallCycles := perf_t0ExclusiveStallCycles + 1.U)
  }
  private val perf_t1StallCycles          = RegEnable(perf_t0StallCycles, t0_fire)
  private val perf_t2StallCycles          = RegEnable(perf_t1StallCycles, t1_fire)
  private val perf_t1ExclusiveStallCycles = RegEnable(perf_t0ExclusiveStallCycles, t0_fire)
  private val perf_t2ExclusiveStallCycles = RegEnable(perf_t1ExclusiveStallCycles, t1_fire)
  XSPerfAccumulate("ittage_train_read_stall_cycles", perf_t0ReadStall)
  XSPerfAccumulate("ittage_train_read_stall_cycles_exclusive", perf_t0ExclusiveReadStall)

  XSPerfSeqAccumulate(
    "ittage_train_read",
    t2_updateValid && t2_needRead,
    Seq(
      ("provider_cnt_change", perf_providerCntChange),
      ("provider_useful_change", perf_providerUsefulChange),
      ("provider_target_change", perf_providerTargetChange),
      ("altprovider_cnt_change", perf_altCntChange),
      ("altprovider_target_change", perf_altTargetChange),
      ("entry_change", perf_entryChange),
      ("no_entry_change", !perf_entryChange),
      ("noop", perf_readNoop),
      ("noop_correct", perf_readNoop && !t2_updateMisPred),
      ("noop_mispredict", perf_readNoop && t2_updateMisPred),
      ("accepted_stall_cycles", true.B, perf_t2StallCycles),
      ("noop_stall_cycles", perf_readNoop, perf_t2StallCycles),
      ("noop_correct_stall_cycles", perf_readNoop && !t2_updateMisPred, perf_t2StallCycles),
      ("accepted_stall_cycles_exclusive", true.B, perf_t2ExclusiveStallCycles),
      ("noop_stall_cycles_exclusive", perf_readNoop, perf_t2ExclusiveStallCycles),
      ("noop_correct_stall_cycles_exclusive", perf_readNoop && !t2_updateMisPred, perf_t2ExclusiveStallCycles)
    )
  )

  // Mutually exclusive reasons for rereading a non-mispredicted branch.
  // Saturated/correct providers can still need a useful-counter update to 1.
  XSPerfSeqAccumulate(
    "ittage_train_read_correct",
    t2_updateValid && t2_needRead && !t2_updateMisPred,
    Seq(
      ("total", true.B),
      ("no_provider", !t2_hasProvider),
      ("provider_wrong", t2_hasProvider && !providerCorrect),
      ("provider_correct_unsaturated", providerCorrect && !t2_providerInfo.cnt.isSaturatePositive),
      ("provider_useful_only", providerCorrect && t2_providerInfo.cnt.isSaturatePositive && perf_providerUsefulChange),
      ("provider_no_change", perf_providerNoChange)
    )
  )

  // Updates count requests to the table write buffers, even if the stored value
  // stays unchanged. Background useful-counter clears are counted separately.
  XSPerfSeqAccumulate(
    "ittage_train",
    t2_updateValid,
    Seq(
      ("update", t2_updateMask.asUInt.orR),
      ("update_provider", t2_hasProvider && t2_updateMask(t2_provider)),
      ("update_altprovider", t2_hasAltProvider && t2_updateMask(t2_altProvider)),
      ("update_allocate", needAllocate && t2_allocateValid),
      ("allocate_failed", needAllocate && !t2_allocateValid)
    )
  )

  XSPerfAccumulate("ittage_reset_u", t2_updateResetUsefulCnt)

  if (debug) {
    val s2_respsRegs = RegEnable(s2_resps, s2_fire)
    XSDebug("req: v=%d, pc=0x%x\n", s0_fire, s0_startPc.toUInt)
    XSDebug("s1_fire:%d, resp: pc=%x\n", s1_fire, s1_startPc.toUInt)
    XSDebug(
      "s2_fireOnLastCycle: resp: pc=%x, hit=%b\n",
      s2_startPc.toUInt,
      s2_provided
    )
    for (i <- 0 until NumTables) {
      XSDebug(
        "TageTable(%d): valids:%b, resp_ctrs:%b, resp_us:%b, target:%x\n",
        i.U,
        VecInit(s2_respsRegs(i).valid).asUInt,
        s2_respsRegs(i).bits.cnt.value,
        s2_respsRegs(i).bits.usefulCnt.value,
        s2_respsRegs(i).bits.targetOffset.offset.toUInt
      )
    }
  }
  XSDebug(
    t2_updateValid,
    p"pc: ${Hexadecimal(t2_updateStartPc.toUInt)}, target: ${Hexadecimal(t2_train.branches(t2_trainBranchIdx).bits.target.toUInt)}\n"
  )
  XSDebug(
    t2_updateValid,
    p"pvdr(v:$t2_hasProvider num:$t2_provider ctr:${t2_providerInfo.cnt} " +
      p"u:${t2_providerInfo.usefulCnt} tar:${Hexadecimal(t2_providerTarget.toUInt)}), " +
      p"altpvdr(v:$t2_hasAltProvider num:$t2_altProvider ctr:${t2_altProviderInfo.cnt} " +
      p"tar:${Hexadecimal(t2_altProviderTarget.toUInt)})\n"
  )
  XSDebug(t2_updateValid, p"correct(${!t2_updateMisPred})\n")
}

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
  private val providerNull    = providerInfo.cnt.isSaturateNegative

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
      (provided && !(providerNull && altProvided)) -> providerCatTarget,
      (providerNull && altProvided)                -> altProviderCatTarget
    )
  )

  // TODO: for low power: use mbtb and remove this
  // private val s2_predictionValid = io.fromFtb.s2_isJalr &&
  //   (provided && !(providerNull && altProvided)) || (providerNull && altProvided)
  // private val s2_predictionValid = (provided && !(providerNull && altProvided)) || (providerNull && altProvided)

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

  private val t1_fire           = RegNext(t0_fire, init = false.B)
  private val t1_needRead       = RegEnable(t0_needRead, false.B, t0_fire)
  private val t1_train          = RegEnable(io.train, 0.U.asTypeOf(new Train), t0_fire)
  private val t1_trainFoldedPhr = RegEnable(io.trainFoldedPhr, t0_fire)

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
  private val t2_providerInfo    = RegEnable(t1_providerInfo, t1_fire)
  private val t2_altProviderInfo = RegEnable(t1_altProviderInfo, t1_fire)
  private val t2_hasProvider     = RegEnable(t1_hasProvider, false.B, t1_fire)
  private val t2_hasAltProvider  = RegEnable(t1_hasAltProvider, false.B, t1_fire)
  private val t2_allocatableMask = RegEnable(t1_allocatableMask, t1_fire)
  // Keep the allocation random sample aligned with the response transaction.
  private val allocLFSR    = random.LFSR(width = 15)(NumTables - 1, 0)
  private val t2_allocLFSR = RegEnable(allocLFSR, t1_fire)

  private val updateStartPc   = t2_train.startPc
  private val updateFoldedPhr = t2_trainFoldedPhr

  // Dedicated combinational region reads allow training and prediction to
  // reconstruct their targets concurrently, using their own start PCs.
  private val t2_targetOffsets = Seq(t2_providerInfo.targetOffset, t2_altProviderInfo.targetOffset)
  private val t2_targets = t2_targetOffsets.zipWithIndex.map { case (offset, i) =>
    rTable.io.trainReqPointer(i) := offset.pointer
    PcInit(Cat(
      Mux(
        rTable.io.trainRespHit(i) && !offset.usePcRegion,
        rTable.io.trainRespRegion(i),
        targetGetRegion(updateStartPc)
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

  // Select the branch needed for training.
  private val trainBranchIdxVec: Vec[Bool] = VecInit(t2_train.branches.map(b =>
    b.valid && b.bits.attribute.needIttage && b.bits.taken
  ))
  when(t2_fire) {
    assert(
      PopCount(trainBranchIdxVec) === 1.U,
      "An active ITTAGE training transaction must contain exactly one selected branch"
    )
  }
  private val trainBranchIdx: UInt = PriorityEncoder(trainBranchIdxVec)
  private val hasTrainBranch: Bool = trainBranchIdxVec.asUInt.orR

  private val updateValid = hasTrainBranch && t2_fire

  private val updateMask            = WireInit(0.U.asTypeOf(Vec(NumTables, Bool())))
  private val updateUsefulCntMask   = WireInit(0.U.asTypeOf(Vec(NumTables, Bool())))
  private val updateResetUsefulCnt  = WireInit(false.B)
  private val updateCorrect         = Wire(Vec(NumTables, Bool()))
  private val updateAlloc           = Wire(Vec(NumTables, Bool()))
  private val updateOldCnt          = Wire(Vec(NumTables, ConfidenceCounter()))
  private val updateUsefulCnt       = Wire(Vec(NumTables, UsefulCounter()))
  private val updateTargetOffset    = Wire(Vec(NumTables, new IttageOffset))
  private val updateOldTargetOffset = Wire(Vec(NumTables, new IttageOffset))
  updateCorrect         := DontCare
  updateAlloc           := DontCare
  updateOldCnt          := DontCare
  updateUsefulCnt       := DontCare
  updateTargetOffset    := DontCare
  updateOldTargetOffset := DontCare

  // Use the misprediction bits from the training bundle so allocation and
  // certain updates (e.g. alt-provider penalty) only happen on actual mispreds.
  private val updateMisPred = hasTrainBranch && t2_train.branches(trainBranchIdx).bits.mispredict

  // Update in loop
  private val updateRealTarget       = t2_train.branches(trainBranchIdx).bits.target
  private val updatePCRegion         = targetGetRegion(t2_train.startPc)
  private val updateRealTargetRegion = targetGetRegion(updateRealTarget)
  private val updateRealTargetOffset = WireInit(0.U.asTypeOf(new IttageOffset))
  updateRealTargetOffset.offset := targetGetOffset(updateRealTarget)
  private val updateRealUsePCRegion = updateRealTargetRegion === updatePCRegion
  // A zero-confidence provider/alternate replaces its target just like an
  // allocation. Both cases need a valid region pointer for a remote target.
  private val updateWritesTarget = updateMask.indices.map { i =>
    updateMask(i) && (updateAlloc(i) || updateOldCnt(i).isSaturateNegative)
  }.reduce(_ || _)
  updateRealTargetOffset.usePcRegion := updateRealUsePCRegion
  rTable.io.writeValid               := !updateRealUsePCRegion && updateWritesTarget
  rTable.io.writeRegion              := updateRealTargetRegion
  updateRealTargetOffset.pointer     := rTable.io.writePointer

  // Retain the original SRAM target encoding when the target does not change.
  rTable.io.updateRegion := VecInit(t2_targets.map(targetGetRegion))

  private val provider    = t2_providerInfo.tableIdx
  private val altProvider = t2_altProviderInfo.tableIdx
  private val usedAltPred = t2_hasAltProvider && t2_providerInfo.cnt.isSaturateNegative
  when(updateValid) {
    when(t2_hasProvider) {
      when(usedAltPred && updateMisPred) { // Train the freshly selected alternate.
        updateMask(altProvider)            := true.B
        updateUsefulCntMask(altProvider)   := false.B
        updateCorrect(altProvider)         := t2_altProviderTarget === updateRealTarget
        updateOldCnt(altProvider)          := t2_altProviderInfo.cnt
        updateAlloc(altProvider)           := false.B
        updateTargetOffset(altProvider)    := updateRealTargetOffset
        updateOldTargetOffset(altProvider) := t2_altProviderInfo.targetOffset
      }

      updateMask(provider)          := true.B
      updateUsefulCntMask(provider) := true.B

      updateUsefulCnt(provider) := Mux(
        !t2_altDiffers,
        t2_providerInfo.usefulCnt,
        (t2_providerTarget === updateRealTarget).asTypeOf(UsefulCounter())
      )
      updateCorrect(provider)         := t2_providerTarget === updateRealTarget
      updateOldCnt(provider)          := t2_providerInfo.cnt
      updateAlloc(provider)           := false.B
      updateTargetOffset(provider)    := updateRealTargetOffset
      updateOldTargetOffset(provider) := t2_providerInfo.targetOffset
    }
  }
  XSDebug(
    updateValid && t2_hasProvider,
    p"t2_train provider $provider, pred cycle ${t2_train.meta.ittage.debug_predCycle.getOrElse(0.U)}\n"
  )
  XSDebug(
    updateValid && t2_hasProvider && usedAltPred && updateMisPred,
    p"t2_train altprovider $altProvider, pred cycle ${t2_train.meta.ittage.debug_predCycle.getOrElse(0.U)}\n"
  )

  // if mispredicted and not the case that
  // provider offered correct target but used altpred due to unconfident
  private val providerCorrect = t2_hasProvider && t2_providerTarget === updateRealTarget
  private val providerUnconf  = t2_providerInfo.cnt.isSaturateNegative
  private val updatePredictionCorrect = t2_hasProvider && Mux(
    usedAltPred,
    t2_altProviderTarget === updateRealTarget,
    providerCorrect
  )
  private val needAllocate = updateValid && updateMisPred && !updatePredictionCorrect &&
    !(providerCorrect && providerUnconf)

  when(needAllocate) {
    tickCnt.selfUpdate(!t2_allocateValid)
    when(t2_allocateValid) {
      updateMask(t2_allocateIdx)          := true.B
      updateCorrect(t2_allocateIdx)       := false.B // useless for alloc
      updateAlloc(t2_allocateIdx)         := true.B
      updateUsefulCntMask(t2_allocateIdx) := true.B
      updateUsefulCnt(t2_allocateIdx)     := UsefulCounter.SaturateNegative
      updateTargetOffset(t2_allocateIdx)  := updateRealTargetOffset
    }
  }

  XSDebug(
    needAllocate && t2_allocateValid,
    p"allocate new table entry, pred cycle ${t2_train.meta.ittage.debug_predCycle.getOrElse(0.U)}\n"
  )

  when(tickCnt.isSaturatePositive) {
    tickCnt.resetZero()
    updateResetUsefulCnt := true.B
  }

  // The t1-to-t2 registers already isolate the SRAM response path. Tables
  // accept these requests directly into their per-bank write buffers.
  for (i <- 0 until NumTables) {
    tables(i).io.update.valid           := updateMask(i)
    tables(i).io.update.resetUsefulCnt  := updateResetUsefulCnt
    tables(i).io.update.correct         := updateCorrect(i)
    tables(i).io.update.alloc           := updateAlloc(i)
    tables(i).io.update.oldCnt          := updateOldCnt(i)
    tables(i).io.update.targetOffset    := updateTargetOffset(i)
    tables(i).io.update.oldTargetOffset := updateOldTargetOffset(i)

    tables(i).io.update.usefulCntValid := updateUsefulCntMask(i)
    tables(i).io.update.usefulCnt      := updateUsefulCnt(i)
    tables(i).io.update.startPc        := updateStartPc
    tables(i).io.update.foldedHist     := updateFoldedPhr
  }

  // Debug and perf info
  // Count accepted training transactions, not cycles spent waiting for a bank.
  // The three read reasons are mutually exclusive and sum to need_read.
  XSPerfSeqAccumulate(
    "ittage_train",
    t0_fire,
    Seq(
      ("total", true.B),
      ("need_read", t0_needRead),
      ("no_read", !t0_needRead),
      ("need_read_provider_only", t0_meta.provider.valid && !t0_updateMisPred),
      ("need_read_mispredict_only", !t0_meta.provider.valid && t0_updateMisPred),
      ("need_read_provider_and_mispredict", t0_meta.provider.valid && t0_updateMisPred)
    )
  )

  // Correlate each t0 read decision with its own t2 update mask. An update
  // means a table update request (table.io.update.valid in this cycle),
  // rather than merely entering training or changing the stored data value.
  // Useful-counter reset sweeps are independent and are excluded here.
  private val perf_hasUpdate = updateMask.asUInt.orR
  private val perf_hasAlloc = VecInit(updateMask.zip(updateAlloc).map { case (valid, alloc) =>
    valid && alloc
  }).asUInt.orR
  private val perf_metaProvider = t2_train.meta.ittage.provider
  XSPerfSeqAccumulate(
    "ittage_train",
    updateValid,
    Seq(
      ("read_update", t2_needRead && perf_hasUpdate),
      ("read_no_update", t2_needRead && !perf_hasUpdate),
      ("no_read_update", !t2_needRead && perf_hasUpdate),
      ("no_read_no_update", !t2_needRead && !perf_hasUpdate),
      ("update", perf_hasUpdate),
      ("update_provider", t2_hasProvider && updateMask(provider)),
      ("update_altprovider", t2_hasAltProvider && updateMask(altProvider)),
      ("update_allocate", perf_hasAlloc),
      ("update_entries", true.B, PopCount(updateMask)),
      ("allocate_failed", needAllocate && !t2_allocateValid),
      ("read_provider_lost", t2_needRead && perf_metaProvider.valid && !t2_hasProvider),
      ("read_provider_found", t2_needRead && !perf_metaProvider.valid && t2_hasProvider),
      (
        "read_provider_changed",
        t2_needRead && perf_metaProvider.valid && t2_hasProvider &&
          perf_metaProvider.bits =/= provider
      )
    )
  )

  XSPerfAccumulate("ittage_reset_u", updateResetUsefulCnt)
  XSPerfAccumulate("ittage_used", s1_fire && s1_isIndirect)
  XSPerfAccumulate("ittage_closed_due_to_uftb_info", s1_fire && !s1_isIndirect)
  XSPerfAccumulate("ittage_allocate", updateValid && perf_hasAlloc)
  XSPerfAccumulate("ittage_hit", io.prediction.hit)

  private def predPerf(name: String, cond: Bool): Unit =
    XSPerfAccumulate(s"${name}_at_pred", cond && s2_fire)

  private def commitPerf(name: String, cond: Bool): Unit =
    XSPerfAccumulate(s"${name}_at_commit", cond && updateValid)

  private def ittagePerf(name: String, predCond: Bool, commitCond: Bool): Unit = {
    predPerf(s"ittage_${name}", predCond)
    commitPerf(s"ittage_${name}", commitCond)
  }

  private val predUseProvider     = s2_provided && !s2_providerCnt.isSaturateNegative
  private val predUseAltPred      = s2_provided && s2_providerCnt.isSaturateNegative
  private val predUseHtAsAltPred  = predUseAltPred && s2_altProvided
  private val predUseBimAsAltPred = predUseAltPred && !s2_altProvided
  private val predUseBimAsPred    = !s2_provided

  private val commitUseProvider     = t2_hasProvider && !t2_providerInfo.cnt.isSaturateNegative
  private val commitUseAltPred      = t2_hasProvider && t2_providerInfo.cnt.isSaturateNegative
  private val commitUseHtAsAltPred  = commitUseAltPred && t2_hasAltProvider
  private val commitUseFtbAsAltPred = commitUseAltPred && !t2_hasAltProvider
  private val commitUseFtbAsPred    = !t2_hasProvider

  for (i <- 0 until NumTables) {
    val predThisIsProvider   = s2_provider === i.U
    val predThisIsAltPred    = s2_altProvider === i.U
    val commitThisIsProvider = t2_providerInfo.tableIdx === i.U
    val commitThisIsAltPred  = t2_altProviderInfo.tableIdx === i.U
    ittagePerf(
      s"table_${i}_final_provided",
      predUseProvider && predThisIsProvider,
      commitUseProvider && commitThisIsProvider
    )
    ittagePerf(
      s"table_${i}_provided_not_used",
      predUseAltPred && predThisIsProvider,
      commitUseAltPred && commitThisIsProvider
    )
    ittagePerf(
      s"table_${i}_alt_provider_as_final_pred",
      predUseHtAsAltPred && predThisIsAltPred,
      commitUseHtAsAltPred && commitThisIsAltPred
    )
    ittagePerf(
      s"table_${i}_alt_provider_not_used",
      predUseProvider && predThisIsAltPred,
      commitUseProvider && commitThisIsAltPred
    )
  }

  ittagePerf("provided", s2_provided, t2_hasProvider)
  ittagePerf("use_provider", predUseProvider, commitUseProvider)
  ittagePerf("use_altpred", predUseAltPred, commitUseAltPred)
  ittagePerf("use_ht_as_altpred", predUseHtAsAltPred, commitUseHtAsAltPred)
  ittagePerf("use_ftb_when_no_provider", predUseBimAsPred, commitUseFtbAsPred)
  ittagePerf("use_ftb_as_alt_provider", predUseBimAsAltPred, commitUseFtbAsAltPred)
  // Historical counter: counts training transactions, including those without updates.
  XSPerfAccumulate("updated", updateValid)

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
    updateValid,
    p"pc: ${Hexadecimal(updateStartPc.toUInt)}, target: ${Hexadecimal(t2_train.branches(trainBranchIdx).bits.target.toUInt)}\n"
  )
  XSDebug(
    updateValid,
    p"pvdr(v:$t2_hasProvider num:$provider ctr:${t2_providerInfo.cnt} " +
      p"u:${t2_providerInfo.usefulCnt} tar:${Hexadecimal(t2_providerTarget.toUInt)}), " +
      p"altpvdr(v:$t2_hasAltProvider num:$altProvider ctr:${t2_altProviderInfo.cnt} " +
      p"tar:${Hexadecimal(t2_altProviderTarget.toUInt)})\n"
  )
  XSDebug(updateValid, p"correct(${!updateMisPred})\n")
}

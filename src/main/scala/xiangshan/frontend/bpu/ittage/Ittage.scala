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
import utility.ParallelSelectTwo
import utility.SelectTwoInterRes
import utility.XSDebug
import utility.XSPerfAccumulate
import xiangshan.XSModule
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.bpu.BasePredictor
import xiangshan.frontend.bpu.BasePredictorIO
import xiangshan.frontend.bpu.BpuTrain
import xiangshan.frontend.bpu.SaturateCounter
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

  io.resetDone := true.B // FIXME: sram read ready

  private val s0_pc   = io.startVAddr
  private val s0_fire = io.stageCtrl.s0_fire && io.enable
  private val s1_fire = io.stageCtrl.s1_fire && io.enable
  private val s2_fire = io.stageCtrl.s2_fire && io.enable
  private val s3_fire = io.stageCtrl.s3_fire && io.enable

  private val s1_pc = RegEnable(s0_pc, s0_fire)
  private val s2_pc = RegEnable(s1_pc, s1_fire)

  private val tables = TableInfos.zipWithIndex.map {
    case (info, i) =>
      val t = Module(new IttageTable(info.Size, info.HistoryLength, TagWidth, i))
      t
  }

  private val useAltOnNa = RegInit((1 << (UseAltOnNaWidth - 1)).U(UseAltOnNaWidth.W))
  private val tickCnt    = RegInit(0.U.asTypeOf(new SaturateCounter(TickWidth)))

  private val rTable = Module(new RegionWays)

  // uftb miss or hasIndirect
  // TODO: for low power: use ubtb&abtb and remove this
  // private val s1_uftbHit         = io.fromFtb.s1_uftbHit
  // private val s1_uftbHasIndirect = io.fromFtb.s1_uftbHasIndirect
  private val s1_isIndirect = true.B // (!s1_uftbHit && !io.fromFtb.s1_ftbCloseReq) || s1_uftbHasIndirect

  // Keep the table responses to process in s2

  private val s2_resps = VecInit(tables.map(t => t.io.resp))

  private val s2_ittageTarget      = Wire(PrunedAddr(VAddrBits))
  private val s2_providerTarget    = Wire(PrunedAddr(VAddrBits))
  private val s2_altProviderTarget = Wire(PrunedAddr(VAddrBits))
  private val s2_provided          = Wire(Bool())
  private val s2_provider          = Wire(UInt(log2Ceil(NumTables).W))
  private val s2_altProvided       = Wire(Bool())
  private val s2_altProvider       = Wire(UInt(log2Ceil(NumTables).W))
  private val s2_providerUsefulCnt = Wire(new SaturateCounter(UsefulCntWidth))
  private val s2_providerCnt       = Wire(new SaturateCounter(ConfidenceCntWidth))
  private val s2_altProviderCnt    = Wire(new SaturateCounter(ConfidenceCntWidth))

  private val s3_ittageTarget      = RegEnable(s2_ittageTarget, s2_fire)
  private val s3_providerTarget    = RegEnable(s2_providerTarget, s2_fire)
  private val s3_altProviderTarget = RegEnable(s2_altProviderTarget, s2_fire)
  private val s3_provided          = RegEnable(s2_provided, s2_fire)
  private val s3_provider          = RegEnable(s2_provider, s2_fire)
  private val s3_altProvided       = RegEnable(s2_altProvided, s2_fire)
  private val s3_altProvider       = RegEnable(s2_altProvider, s2_fire)
  private val s3_providerUsefulCnt = RegEnable(s2_providerUsefulCnt, s2_fire)
  private val s3_providerCnt       = RegEnable(s2_providerCnt, s2_fire)
  private val s3_altProviderCnt    = RegEnable(s2_altProviderCnt, s2_fire)

  private val ittageMeta = WireDefault(0.U.asTypeOf(new IttageMeta))
  io.meta := ittageMeta

  private val t1_train = Wire(new BpuTrain)
  t1_train := RegEnable(io.train.bits, 0.U.asTypeOf(new BpuTrain), io.train.valid)

  private val t1_meta = Wire(new IttageMeta)
  t1_train.meta.ittage := t1_meta

  // The pc register has been moved outside of predictor
  // pc field of t1_train bundle and other t1_train data are not in the same stage
  // so io.t1_train.bits.pc is used directly here
  private val updatePc = io.train.bits.startVAddr

  // To improve Clock Gating Efficiency
  private val t0_meta = io.train.bits.meta.ittage
  t1_meta := RegEnable(t0_meta, io.train.valid)
  t1_meta.provider.bits := RegEnable(
    t0_meta.provider.bits,
    io.train.valid && t0_meta.provider.valid
  )
  t1_meta.providerTarget := RegEnable(
    t0_meta.providerTarget,
    0.U.asTypeOf(t0_meta.providerTarget),
    io.train.valid && t0_meta.provider.valid
  )
  t1_meta.allocate.bits := RegEnable(
    t0_meta.allocate.bits,
    io.train.valid && t0_meta.allocate.valid
  )
  t1_meta.altProvider.bits := RegEnable(
    t0_meta.altProvider.bits,
    io.train.valid && t0_meta.altProvider.valid
  )
  t1_meta.altProviderTarget := RegEnable(
    t0_meta.altProviderTarget,
    0.U.asTypeOf(t0_meta.altProviderTarget),
    io.train.valid && t0_meta.provider.valid && t0_meta.altProvider.valid && t0_meta.providerCnt.isSaturateNegative
  )

  // Select the branch needed for training
  val trainBranchIdxOH: Vec[Bool] = VecInit(t1_train.branches.map(b =>
    b.valid && b.bits.attribute.isOtherIndirect && b.bits.taken
  ))
  assert(
    PopCount(trainBranchIdxOH) <= 1.U,
    "At most one branch in branches should be valid and isOtherIndirect for ITTAGE update"
  )
  val trainBranchIdx: UInt = OHToUInt(trainBranchIdxOH)
  val hasTrainBranch: Bool = trainBranchIdxOH.asUInt.orR

  // Update condition for ittage
  private val updateValid = hasTrainBranch && RegNext(io.train.valid, init = false.B)

  private val updateMask            = WireInit(0.U.asTypeOf(Vec(NumTables, Bool())))
  private val updateUsefulCntMask   = WireInit(0.U.asTypeOf(Vec(NumTables, Bool())))
  private val updateResetUsefulCnt  = WireInit(false.B)
  private val updateCorrect         = Wire(Vec(NumTables, Bool()))
  private val updateAlloc           = Wire(Vec(NumTables, Bool()))
  private val updateOldCnt          = Wire(Vec(NumTables, new SaturateCounter(ConfidenceCntWidth)))
  private val updateUsefulCnt       = Wire(Vec(NumTables, new SaturateCounter(UsefulCntWidth)))
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
  private val updateMisPred = hasTrainBranch && t1_train.branches(trainBranchIdx).bits.mispredict

  // Predict
  tables.foreach { t =>
    t.io.req.valid           := s1_fire && s1_isIndirect // TODO: s1_isIndirect for low power
    t.io.req.bits.pc         := s1_pc
    t.io.req.bits.foldedHist := io.s1_foldedPhr
  }

  // access tag tables and output meta info
  class IttageTableInfo extends Bundle {
    val cnt:          SaturateCounter = new SaturateCounter(ConfidenceCntWidth)
    val usefulCnt:    SaturateCounter = new SaturateCounter(UsefulCntWidth)
    val targetOffset: IttageOffset    = new IttageOffset
    val tableIdx:     UInt            = UInt(log2Ceil(NumTables).W)
    val maskTarget:   Vec[UInt]       = Vec(NumTables, UInt(VAddrBits.W))
  }

  private val inputRes = VecInit(s2_resps.zipWithIndex.map {
    case (r, i) =>
      val tableInfo = Wire(new IttageTableInfo)
      tableInfo.usefulCnt     := r.bits.usefulCnt
      tableInfo.cnt           := r.bits.cnt
      tableInfo.targetOffset  := r.bits.targetOffset
      tableInfo.tableIdx      := i.U(log2Ceil(NumTables).W)
      tableInfo.maskTarget    := VecInit(Seq.fill(NumTables)(0.U(VAddrBits.W)))
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
  private val regionTargets = Wire(Vec(NumTables, PrunedAddr(VAddrBits)))
  for (i <- 0 until NumTables) {
    regionTargets(i) := PrunedAddrInit(Mux(
      rTable.io.respHit(i) && !regionReadTargetOffset(i).usePcRegion,
      Cat(rTable.io.respRegion(i), regionReadTargetOffset(i).offset.toUInt),
      Cat(targetGetRegion(s2_pc), regionReadTargetOffset(i).offset.toUInt)
    ))
  }

  private val providerCatTarget = PrunedAddrInit(providerInfo.maskTarget.zipWithIndex.map {
    case (mask, i) => mask & regionTargets(i).toUInt
  }.reduce(_ | _))

  private val altProviderCatTarget = PrunedAddrInit(altProviderInfo.maskTarget.zipWithIndex.map {
    case (mask, i) => mask & regionTargets(i).toUInt
  }.reduce(_ | _))

  s2_ittageTarget := MuxCase(
    0.U.asTypeOf(PrunedAddr(VAddrBits)),
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
//    assert(io.prediction.target =/= 0.U.asTypeOf(PrunedAddr(VAddrBits)))
//  }

  s2_provided          := provided
  s2_provider          := providerInfo.tableIdx
  s2_altProvided       := altProvided
  s2_altProvider       := altProviderInfo.tableIdx
  s2_providerUsefulCnt := providerInfo.usefulCnt
  s2_providerCnt       := providerInfo.cnt
  s2_altProviderCnt    := altProviderInfo.cnt
  s2_providerTarget    := providerCatTarget
  s2_altProviderTarget := altProviderCatTarget

  XSDebug(s2_fire, p"hit_taken_jalr:")

  ittageMeta.valid             := s3_fire
  ittageMeta.provider.valid    := s3_provided
  ittageMeta.provider.bits     := s3_provider
  ittageMeta.altProvider.valid := s3_altProvided
  ittageMeta.altProvider.bits  := s3_altProvider
  ittageMeta.altDiffers        := s3_providerTarget =/= s3_altProviderTarget
  ittageMeta.providerUsefulCnt := s3_providerUsefulCnt
  ittageMeta.providerCnt       := s3_providerCnt
  ittageMeta.altProviderCnt    := s3_altProviderCnt
  ittageMeta.providerTarget    := s3_providerTarget
  ittageMeta.altProviderTarget := s3_altProviderTarget
  ittageMeta.debug_predCycle.foreach(_ := GTimer())
  // Create a mask fo tables which did not hit our query, and also contain useless entries
  // and also uses a longer history than the provider
  private val s2_allocatableSlots = VecInit(s2_resps.map(r => !r.valid && r.bits.usefulCnt.isSaturateNegative)).asUInt &
    (~(LowerMask(UIntToOH(s2_provider), NumTables) & Fill(NumTables, s2_provided.asUInt))).asUInt
  private val s2_allocLFSR   = random.LFSR(width = 15)(NumTables - 1, 0)
  private val s2_firstEntry  = PriorityEncoder(s2_allocatableSlots)
  private val s2_maskedEntry = PriorityEncoder(s2_allocatableSlots & s2_allocLFSR)
  private val s2_allocEntry  = Mux(s2_allocatableSlots(s2_maskedEntry), s2_maskedEntry, s2_firstEntry)
  ittageMeta.allocate.valid := RegEnable(s2_allocatableSlots =/= 0.U, s2_fire)
  ittageMeta.allocate.bits  := RegEnable(s2_allocEntry, s2_fire)

  // Update in loop
  private val updateRealTarget       = t1_train.branches(trainBranchIdx).bits.target
  private val updatePCRegion         = targetGetRegion(t1_train.startVAddr)
  private val updateRealTargetRegion = targetGetRegion(updateRealTarget)
  private val metaProviderTargetOffset, metaAltProviderTargetOffset, updateRealTargetOffset =
    WireInit(0.U.asTypeOf(new IttageOffset))
  updateRealTargetOffset.offset := targetGetOffset(updateRealTarget)
  private val updateRealUsePCRegion = updateRealTargetRegion === updatePCRegion
  // If rTable is not written in Region, the pointer value will be invalid.
  // At this time, it is necessary to raise usePCRegion.
  // The t1_train mechanism of the usePCRegion bit requires further consideration.
  updateRealTargetOffset.usePcRegion := updateRealUsePCRegion || !updateAlloc.reduce(_ || _)
  rTable.io.writeValid               := !updateRealUsePCRegion && updateAlloc.reduce(_ || _)
  rTable.io.writeRegion              := updateRealTargetRegion
  updateRealTargetOffset.pointer     := rTable.io.writePointer

  private val metaProviderTargetRegion    = targetGetRegion(t1_meta.providerTarget)
  private val metaAltProviderTargetRegion = targetGetRegion(t1_meta.altProviderTarget)

  rTable.io.updateRegion               := VecInit(metaProviderTargetRegion, metaAltProviderTargetRegion)
  metaProviderTargetOffset.offset      := targetGetOffset(t1_meta.providerTarget)
  metaProviderTargetOffset.pointer     := rTable.io.updatePointer(0)
  metaProviderTargetOffset.usePcRegion := !rTable.io.updateHit(0)

  metaAltProviderTargetOffset.offset      := targetGetOffset(t1_meta.altProviderTarget)
  metaAltProviderTargetOffset.pointer     := rTable.io.updatePointer(1)
  metaAltProviderTargetOffset.usePcRegion := !rTable.io.updateHit(1)

  private val provider    = t1_meta.provider.bits
  private val altProvider = t1_meta.altProvider.bits
  private val usedAltPred = t1_meta.altProvider.valid && t1_meta.providerCnt.isSaturateNegative
  when(updateValid) {
    when(t1_meta.provider.valid) {
      when(usedAltPred && updateMisPred) { // t1_train altpred if used as pred
        updateMask(altProvider)            := true.B
        updateUsefulCntMask(altProvider)   := false.B
        updateCorrect(altProvider)         := false.B
        updateOldCnt(altProvider)          := t1_meta.altProviderCnt
        updateAlloc(altProvider)           := false.B
        updateTargetOffset(altProvider)    := updateRealTargetOffset
        updateOldTargetOffset(altProvider) := metaAltProviderTargetOffset
      }

      updateMask(provider)          := true.B
      updateUsefulCntMask(provider) := true.B

      updateUsefulCnt(provider) := Mux(
        !t1_meta.altDiffers,
        t1_meta.providerUsefulCnt,
        (t1_meta.providerTarget === updateRealTarget).asTypeOf(new SaturateCounter(UsefulCntWidth))
      )
      updateCorrect(provider)         := t1_meta.providerTarget === updateRealTarget
      updateOldCnt(provider)          := t1_meta.providerCnt
      updateAlloc(provider)           := false.B
      updateTargetOffset(provider)    := updateRealTargetOffset
      updateOldTargetOffset(provider) := metaProviderTargetOffset
    }
  }
  XSDebug(
    updateValid && t1_meta.provider.valid,
    p"t1_train provider $provider, pred cycle ${t1_meta.debug_predCycle.getOrElse(0.U)}\n"
  )
  XSDebug(
    updateValid && t1_meta.provider.valid && usedAltPred && updateMisPred,
    p"t1_train altprovider $altProvider, pred cycle ${t1_meta.debug_predCycle.getOrElse(0.U)}\n"
  )

  // if mispredicted and not the case that
  // provider offered correct target but used altpred due to unconfident
  private val providerCorrect = t1_meta.provider.valid && t1_meta.providerTarget === updateRealTarget
  private val providerUnconf  = t1_meta.providerCnt.isSaturateNegative
  private val allocate        = t1_meta.allocate

  when(updateValid && updateMisPred && !(providerCorrect && providerUnconf)) {
    tickCnt.update(!allocate.valid)
    when(allocate.valid) {
      updateMask(allocate.bits)          := true.B
      updateCorrect(allocate.bits)       := false.B // useless for alloc
      updateAlloc(allocate.bits)         := true.B
      updateUsefulCntMask(allocate.bits) := true.B
      updateUsefulCnt(allocate.bits)     := false.B.asTypeOf(new SaturateCounter(UsefulCntWidth))
      updateTargetOffset(allocate.bits)  := updateRealTargetOffset
    }
  }

  XSDebug(
    updateValid && updateMisPred && !(providerCorrect && providerUnconf) && allocate.valid,
    p"allocate new table entry, pred cycle ${t1_meta.debug_predCycle.getOrElse(0.U)}\n"
  )

  when(tickCnt.isSaturatePositive) {
    tickCnt.resetZero()
    updateResetUsefulCnt := true.B
  }

  for (i <- 0 until NumTables) {
    tables(i).io.update.valid           := RegNext(updateMask(i), init = false.B)
    tables(i).io.update.resetUsefulCnt  := RegNext(updateResetUsefulCnt, init = false.B)
    tables(i).io.update.correct         := RegEnable(updateCorrect(i), updateMask(i))
    tables(i).io.update.alloc           := RegEnable(updateAlloc(i), updateMask(i))
    tables(i).io.update.oldCnt          := RegEnable(updateOldCnt(i), updateMask(i))
    tables(i).io.update.targetOffset    := RegEnable(updateTargetOffset(i), updateMask(i))
    tables(i).io.update.oldTargetOffset := RegEnable(updateOldTargetOffset(i), updateMask(i))

    tables(i).io.update.usefulCntValid := RegEnable(updateUsefulCntMask(i), false.B, updateMask(i))
    tables(i).io.update.usefulCnt      := RegEnable(updateUsefulCnt(i), updateMask(i))
    tables(i).io.update.pc             := RegEnable(updatePc, updateMask(i))
    tables(i).io.update.foldedHist     := RegEnable(io.trainFoldedPhr, updateMask(i))
  }

  // Debug and perf info
  XSPerfAccumulate("ittage_reset_u", updateResetUsefulCnt)
  XSPerfAccumulate("ittage_used", s1_fire && s1_isIndirect)
  XSPerfAccumulate("ittage_closed_due_to_uftb_info", s1_fire && !s1_isIndirect)
  XSPerfAccumulate("ittage_allocate", updateAlloc.reduce(_ || _))
  XSPerfAccumulate("ittage_hit", io.prediction.hit)

  private def predPerf(name: String, cond: Bool): Unit =
    XSPerfAccumulate(s"${name}_at_pred", cond && s2_fire)

  private def commitPerf(name: String, cond: Bool): Unit =
    XSPerfAccumulate(s"${name}_at_commit", cond && updateValid)

  private def ittagePerf(name: String, predCond: Bool, commitCond: Bool): Unit = {
    predPerf(s"ittage_${name}", predCond)
    commitPerf(s"ittage_${name}", commitCond)
  }

  def ctrNull(ctr: SaturateCounter): Bool = ctr.isSaturateNegative

  private val predUseProvider     = s2_provided && !ctrNull(s2_providerCnt)
  private val predUseAltPred      = s2_provided && ctrNull(s2_providerCnt)
  private val predUseHtAsAltPred  = predUseAltPred && s2_altProvided
  private val predUseBimAsAltPred = predUseAltPred && !s2_altProvided
  private val predUseBimAsPred    = !s2_provided

  private val commitUseProvider     = t1_meta.provider.valid && !ctrNull(t1_meta.providerCnt)
  private val commitUseAltPred      = t1_meta.provider.valid && ctrNull(t1_meta.providerCnt)
  private val commitUseHtAsAltPred  = commitUseAltPred && t1_meta.altProvider.valid
  private val commitUseFtbAsAltPred = commitUseAltPred && !t1_meta.altProvider.valid
  private val commitUseFtbAsPred    = !t1_meta.provider.valid

  for (i <- 0 until NumTables) {
    val predThisIsProvider   = s2_provider === i.U
    val predThisIsAltPred    = s2_altProvider === i.U
    val commitThisIsProvider = t1_meta.provider.bits === i.U
    val commitThisIsAltPred  = t1_meta.altProvider.bits === i.U
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

  ittagePerf("provided", s2_provided, t1_meta.provider.valid)
  ittagePerf("use_provider", predUseProvider, commitUseProvider)
  ittagePerf("use_altpred", predUseAltPred, commitUseAltPred)
  ittagePerf("use_ht_as_altpred", predUseHtAsAltPred, commitUseHtAsAltPred)
  ittagePerf("use_ftb_when_no_provider", predUseBimAsPred, commitUseFtbAsPred)
  ittagePerf("use_ftb_as_alt_provider", predUseBimAsAltPred, commitUseFtbAsAltPred)
  XSPerfAccumulate("updated", updateValid)

  if (debug) {
    val s2_respsRegs = RegEnable(s2_resps, s2_fire)
    XSDebug("req: v=%d, pc=0x%x\n", s0_fire, s0_pc.toUInt)
    XSDebug("s1_fire:%d, resp: pc=%x\n", s1_fire, s1_pc.toUInt)
    XSDebug(
      "s2_fireOnLastCycle: resp: pc=%x, hit=%b\n",
      s2_pc.toUInt,
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
    p"pc: ${Hexadecimal(updatePc.toUInt)}, target: ${Hexadecimal(t1_train.branches(trainBranchIdx).bits.target.toUInt)}\n"
  )
  XSDebug(updateValid, t1_meta.toPrintable + p"\n")
  XSDebug(updateValid, p"correct(${!updateMisPred})\n")
}

// Copyright (c) 2024 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2024 Institute of Computing Technology, Chinese Academy of Sciences
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

package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import scala.math.min
import utility.DelayN
import utility.GTimer
import utility.HasCircularQueuePtrHelper
import utility.HasPerfEvents
import utility.LowerMask
import utility.ParallelXOR
import utility.PhyPriorityMuxGenerator
import utility.RegNextWithEnable
import utility.SegmentedAddrNext
import utility.XSDebug
import utility.XSError
import utility.XSPerfAccumulate
import utility.XSWarn
import xiangshan.HasXSParameter
import xiangshan.TopDownCounters
import xiangshan.XSBundle
import xiangshan.XSModule
import xiangshan.frontend.AllAheadFoldedHistoryOldestBits
import xiangshan.frontend.AllFoldedHistories
import xiangshan.frontend.BpuToFtqIO
import xiangshan.frontend.BranchPredictionBundle
import xiangshan.frontend.BranchPredictionResp
import xiangshan.frontend.BranchPredictionUpdate
import xiangshan.frontend.CGHPtr
import xiangshan.frontend.FrontendTopDownBundle
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.PrunedAddrInit
import xiangshan.frontend.ftq.FtqToBpuIO
import xiangshan.frontend.selectByTaken

trait HasBPUConst extends HasXSParameter {
  val MaxMetaBaseLength = if (!env.FPGAPlatform) 512 else 256 // TODO: Reduce meta length
  val MaxMetaLength     = if (HasHExtension) MaxMetaBaseLength + 4 else MaxMetaBaseLength
  val MaxBasicBlockSize = 32
  val LHistoryLength    = 32
  // val numBr = 2
  val useBPD    = true
  val useLHist  = true
  val numBrSlot = numBr - 1
  val totalSlot = numBrSlot + 1

  val numDup = 4

  // Used to gate PC higher parts
  val pcSegments = Seq(VAddrBits - 24, 12, 12)

  def BP_STAGES = (0 until 3).map(_.U(2.W))
  def BP_S1     = BP_STAGES(0)
  def BP_S2     = BP_STAGES(1)
  def BP_S3     = BP_STAGES(2)

  def dup_seq[T](src:          T, num: Int = numDup) = Seq.tabulate(num)(n => src)
  def dup[T <: Data](src:      T, num: Int = numDup) = VecInit(Seq.tabulate(num)(n => src))
  def dup_wire[T <: Data](src: T, num: Int = numDup) = Wire(Vec(num, src.cloneType))
  def dup_idx     = Seq.tabulate(numDup)(n => n.toString())
  val numBpStages = BP_STAGES.length

  val debug = true
  // TODO: Replace log2Up by log2Ceil
}

trait HasBPUParameter extends HasXSParameter with HasBPUConst {
  val BPUDebug            = true && !env.FPGAPlatform && env.EnablePerfDebug
  val EnableCFICommitLog  = true
  val EnbaleCFIPredLog    = true
  val EnableBPUTimeRecord = (EnableCFICommitLog || EnbaleCFIPredLog) && !env.FPGAPlatform
  val EnableCommit        = false
}

class BPUCtrl(implicit p: Parameters) extends XSBundle {
  val ubtb_enable = Bool()
  val btb_enable  = Bool()
  val bim_enable  = Bool()
  val tage_enable = Bool()
  val sc_enable   = Bool()
  val ras_enable  = Bool()
  val loop_enable = Bool()
}

trait BPUUtils extends HasXSParameter {
  // circular shifting
  def circularShiftLeft(source: UInt, len: Int, shamt: UInt): UInt = {
    val res    = Wire(UInt(len.W))
    val higher = source << shamt
    val lower  = source >> (len.U - shamt)
    res := higher | lower
    res
  }

  def circularShiftRight(source: UInt, len: Int, shamt: UInt): UInt = {
    val res    = Wire(UInt(len.W))
    val higher = source << (len.U - shamt)
    val lower  = source >> shamt
    res := higher | lower
    res
  }

  // To be verified
  def satUpdate(old: UInt, len: Int, taken: Bool): UInt = {
    val oldSatTaken    = old === ((1 << len) - 1).U
    val oldSatNotTaken = old === 0.U
    Mux(oldSatTaken && taken, ((1 << len) - 1).U, Mux(oldSatNotTaken && !taken, 0.U, Mux(taken, old + 1.U, old - 1.U)))
  }

  def signedSatUpdate(old: SInt, len: Int, taken: Bool): SInt = {
    val oldSatTaken    = old === ((1 << (len - 1)) - 1).S
    val oldSatNotTaken = old === (-(1 << (len - 1))).S
    Mux(
      oldSatTaken && taken,
      ((1 << (len - 1)) - 1).S,
      Mux(oldSatNotTaken && !taken, (-(1 << (len - 1))).S, Mux(taken, old + 1.S, old - 1.S))
    )
  }

  def getFallThroughAddr(start: PrunedAddr, carry: Bool, pft: UInt): PrunedAddr = {
    val higher = start(VAddrBits - 1, log2Ceil(PredictWidth) + instOffsetBits)
    PrunedAddrInit(Cat(Mux(carry, higher + 1.U, higher), pft, 0.U(instOffsetBits.W)))
  }

  def foldTag(tag: UInt, l: Int): UInt = {
    val nChunks = (tag.getWidth + l - 1) / l
    val chunks  = (0 until nChunks).map(i => tag(min((i + 1) * l, tag.getWidth) - 1, i * l))
    ParallelXOR(chunks)
  }
}

class PredictorMeta(implicit p: Parameters) extends XSBundle {
  val uftbMeta   = new MicroFtbMeta
  val ftbMeta    = new FtbMeta
  val tageMeta   = new TageMeta
  val ittageMeta = new IttageMeta
  val rasMeta    = new RasMeta
}

trait HasPredictorCommonSignals extends HasXSParameter {
  val s0_pc = PrunedAddr(VAddrBits)

  val s0_fire = Bool()
  val s1_fire = Bool()
  val s2_fire = Bool()
  val s3_fire = Bool()

  val ctrl   = new BPUCtrl
  val update = Valid(new BranchPredictionUpdate)
}

class BpuIO(implicit p: Parameters) extends XSBundle {
  val toFtq        = new BpuToFtqIO
  val fromFtq      = Flipped(new FtqToBpuIO)
  val ctrl         = Input(new BPUCtrl)
  val reset_vector = Input(PrunedAddr(PAddrBits))
}

class Bpu(implicit p: Parameters) extends XSModule with HasBPUConst with HasCircularQueuePtrHelper with HasPerfEvents {
  val io = IO(new BpuIO)

  val modified_reset = RegInit(true.B)
  when(modified_reset)(modified_reset := false.B)

  val uftb   = Module(new MicroFtb)
  val ftb    = Module(new Ftb)
  val tage   = Module(new Tage)
  val ittage = Module(new Ittage)
  val ras    = Module(new Ras)

  ftb.io.in.fromFauFtb        := uftb.io.out.toFtb
  ftb.io.in.fromTage          := tage.io.out.toFtb
  ftb.io.in.isRedirectFromIfu := RegNext(io.fromFtq.redirectFromIFU, init = false.B)
  ittage.io.in.fromFtb        := ftb.io.out.toIttage
  ras.io.in.fromFtb           := ftb.io.out.toRas

  val s0_stall = Wire(Bool()) // For some reason s0 stalled, usually FTQ Full

  val s0_fire = Wire(Bool())
  val s1_fire = Wire(Bool())
  val s2_fire = Wire(Bool())
  val s3_fire = Wire(Bool())

  val s1_ready = Wire(Bool())
  val s2_ready = Wire(Bool())
  val s3_ready = Wire(Bool())

  val s1_flush = Wire(Bool())
  val s2_flush = Wire(Bool())
  val s3_flush = Wire(Bool())

  val s1_valid = RegInit(false.B)
  val s2_valid = RegInit(false.B)
  val s3_valid = RegInit(false.B)

  val s2_override = WireDefault(false.B)
  val s3_override = WireDefault(false.B)

  val s0_pc     = WireDefault(0.U.asTypeOf(PrunedAddr(VAddrBits)))
  val s0_pc_reg = RegEnable(s0_pc, !s0_stall)
  when(modified_reset) {
    s0_pc_reg := io.reset_vector
  }

  val s1_pc = RegEnable(s0_pc, s0_fire)
  val s2_pc = RegEnable(s1_pc, s1_fire)
  val s3_pc = RegEnable(s2_pc, s2_fire)

  val predictorsIn = Seq(uftb.io.in, tage.io.in, ftb.io.in, ittage.io.in, ras.io.in)
  predictorsIn.foreach { in =>
    in.ctrl := DelayN(io.ctrl, 2)
    // TODO: duplicate pc and fire to solve high fan-out issue
    in.s0_pc   := s0_pc
    in.s0_fire := s0_fire
    in.s1_fire := s1_fire
    in.s2_fire := s2_fire
    in.s3_fire := s3_fire
    in.update  := io.fromFtq.update
    // low power: gate pc higher bits
    in.update.bits.pc := PrunedAddrInit(SegmentedAddrNext(
      io.fromFtq.update.bits.pc.toUInt,
      pcSegments,
      io.fromFtq.update.valid,
      Some("predictors_io_update_pc")
    ).getAddr())
  }

  val s0_folded_gh     = Wire(new AllFoldedHistories(foldedGHistInfos))
  val s0_folded_gh_reg = RegEnable(s0_folded_gh, 0.U.asTypeOf(s0_folded_gh), !s0_stall)

  val s1_folded_gh = RegEnable(s0_folded_gh, 0.U.asTypeOf(s0_folded_gh), s0_fire)
  val s2_folded_gh = RegEnable(s1_folded_gh, 0.U.asTypeOf(s0_folded_gh), s1_fire)
  val s3_folded_gh = RegEnable(s2_folded_gh, 0.U.asTypeOf(s0_folded_gh), s2_fire)

  val s0_last_br_num_oh     = Wire(UInt((numBr + 1).W))
  val s0_last_br_num_oh_reg = RegEnable(s0_last_br_num_oh, 0.U, !s0_stall)

  val s1_last_br_num_oh = RegEnable(s0_last_br_num_oh, 0.U.asTypeOf(s0_last_br_num_oh), s0_fire)
  val s2_last_br_num_oh = RegEnable(s1_last_br_num_oh, 0.U.asTypeOf(s0_last_br_num_oh), s1_fire)
  val s3_last_br_num_oh = RegEnable(s2_last_br_num_oh, 0.U.asTypeOf(s0_last_br_num_oh), s2_fire)

  val s0_ahead_fh_oldest_bits     = Wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  val s0_ahead_fh_oldest_bits_reg = RegEnable(s0_ahead_fh_oldest_bits, 0.U.asTypeOf(s0_ahead_fh_oldest_bits), !s0_stall)

  val s1_ahead_fh_oldest_bits = RegEnable(s0_ahead_fh_oldest_bits, 0.U.asTypeOf(s0_ahead_fh_oldest_bits), s0_fire)
  val s2_ahead_fh_oldest_bits = RegEnable(s1_ahead_fh_oldest_bits, 0.U.asTypeOf(s0_ahead_fh_oldest_bits), s1_fire)
  val s3_ahead_fh_oldest_bits = RegEnable(s2_ahead_fh_oldest_bits, 0.U.asTypeOf(s0_ahead_fh_oldest_bits), s2_fire)

  val npcGen         = new PhyPriorityMuxGenerator[PrunedAddr]
  val foldedGhGen    = new PhyPriorityMuxGenerator[AllFoldedHistories]
  val ghistPtrGen    = new PhyPriorityMuxGenerator[CGHPtr]
  val lastBrNumOHGen = new PhyPriorityMuxGenerator[UInt]
  val aheadFhObGen   = new PhyPriorityMuxGenerator[AllAheadFoldedHistoryOldestBits]

  val ghvBitWriteGens = Seq.tabulate(HistoryLength)(n => new PhyPriorityMuxGenerator[Bool])
  // val ghistGen = new PhyPriorityMuxGenerator[UInt]

  val ghv      = RegInit(0.U.asTypeOf(Vec(HistoryLength, Bool())))
  val ghv_wire = WireInit(ghv)

  val s0_ghist = WireInit(0.U.asTypeOf(UInt(HistoryLength.W)))

  println(f"history buffer length ${HistoryLength}")
  val ghv_write_datas = Wire(Vec(HistoryLength, Bool()))
  val ghv_wens        = Wire(Vec(HistoryLength, Bool()))

  val s0_ghist_ptr     = Wire(new CGHPtr)
  val s0_ghist_ptr_reg = RegEnable(s0_ghist_ptr, 0.U.asTypeOf(new CGHPtr), !s0_stall)

  val s1_ghist_ptr = RegEnable(s0_ghist_ptr, 0.U.asTypeOf(s0_ghist_ptr), s0_fire)
  val s2_ghist_ptr = RegEnable(s1_ghist_ptr, 0.U.asTypeOf(s0_ghist_ptr), s1_fire)
  val s3_ghist_ptr = RegEnable(s2_ghist_ptr, 0.U.asTypeOf(s0_ghist_ptr), s2_fire)

  def getHist(ptr: CGHPtr): UInt = (Cat(ghv_wire.asUInt, ghv_wire.asUInt) >> (ptr.value + 1.U))(HistoryLength - 1, 0)
  s0_ghist := getHist(s0_ghist_ptr)

  predictorsIn.foreach(_.update.bits.ghist := getHist(io.fromFtq.update.bits.spec_info.histPtr))

  tage.io.in.ghist            := s0_ghist
  tage.io.in.folded_hist      := s0_folded_gh
  ittage.io.in.ghist          := s0_ghist
  ittage.io.in.folded_hist    := s0_folded_gh
  ittage.io.in.s1_folded_hist := s1_folded_gh

  val redirect_req = io.fromFtq.redirect
  val do_redirect  = RegNextWithEnable(redirect_req) // TODO: reduce one cycle delay

  ras.io.in.redirect := do_redirect

  s3_flush := redirect_req.valid // flush when redirect comes
  s2_flush := s3_flush || s3_override
  s1_flush := s2_flush || s2_override

  s1_ready := s1_fire || !s1_valid
  s2_ready := s2_fire || !s2_valid
  s3_ready := s3_fire || !s3_valid

  val s1_predictorsReady = ftb.io.out.s1_ready && tage.io.out.s1_ready && ittage.io.out.s1_ready

  s0_fire := s1_predictorsReady && s1_ready
  s1_fire := s1_valid && s2_ready && io.toFtq.resp.ready
  s2_fire := s2_valid && s3_ready
  s3_fire := s3_valid

  when(redirect_req.valid)(s1_valid := false.B)
    .elsewhen(s0_fire)(s1_valid := true.B)
    .elsewhen(s1_flush)(s1_valid := false.B)
    .elsewhen(s1_fire)(s1_valid := false.B)

  when(s2_flush)(s2_valid := false.B)
    .elsewhen(s1_fire)(s2_valid := !s1_flush)
    .elsewhen(s2_fire)(s2_valid := false.B)

  when(s3_flush)(s3_valid := false.B)
    .elsewhen(s2_fire)(s3_valid := !s2_flush)
    .elsewhen(s3_fire)(s3_valid := false.B)

  val s2_ftq_idx = RegEnable(io.fromFtq.bpuPtr, s1_fire)
  val s3_ftq_idx = RegEnable(s2_ftq_idx, s2_fire)

  val bpuResp = Wire(new BranchPredictionResp)

  bpuResp.s1.valid       := s1_fire && !s1_flush
  bpuResp.s1.pc          := s1_pc
  bpuResp.s1.hasRedirect := false.B
  bpuResp.s1.ftq_idx     := DontCare
  bpuResp.s1.full_pred   := uftb.io.out.s1_fullPred

  bpuResp.s2.valid       := s2_fire && !s2_flush
  bpuResp.s2.pc          := s2_pc
  bpuResp.s2.hasRedirect := s2_override
  bpuResp.s2.ftq_idx     := s2_ftq_idx
  bpuResp.s2.full_pred   := ftb.io.out.s2_fullPred

  bpuResp.s3.valid       := s3_fire && !s3_flush
  bpuResp.s3.pc          := s3_pc
  bpuResp.s3.hasRedirect := s3_override
  bpuResp.s3.ftq_idx     := s3_ftq_idx
  bpuResp.s3.full_pred   := ftb.io.out.s3_fullPred

  // jalr target selection
  bpuResp.s3.full_pred.targets.last := MuxCase(
    ftb.io.out.s3_fullPred.targets.last, // default
    Seq(
      ras.io.out.predictionValid    -> ras.io.out.s3_returnAddress,
      ittage.io.out.predictionValid -> ittage.io.out.s3_jalrTarget
    )
  )

  // assign pred cycle for profiling
  if (bpuResp.s1.full_pred.predCycle.isDefined) bpuResp.s1.full_pred.predCycle.get := GTimer()
  if (bpuResp.s2.full_pred.predCycle.isDefined) bpuResp.s2.full_pred.predCycle.get := GTimer()
  if (bpuResp.s3.full_pred.predCycle.isDefined) bpuResp.s3.full_pred.predCycle.get := GTimer()

  bpuResp.s3_ftbEntry := ftb.io.out.s3_ftbEntry

  bpuResp.s3_specInfo.histPtr     := s3_ghist_ptr
  bpuResp.s3_specInfo.rasSpecInfo := ras.io.out.s3_rasSpecInfo
//  if (bpuResp.s3_specInfo.sc_disagree.isDefined && tage.io.out.sc_disagree.isDefined) {
//    bpuResp.s3_specInfo.sc_disagree.get := tage.io.out.sc_disagree.get
//  }

  bpuResp.s3_meta.uftbMeta   := uftb.io.out.s3_meta
  bpuResp.s3_meta.tageMeta   := tage.io.out.s3_meta
  bpuResp.s3_meta.ftbMeta    := ftb.io.out.s3_meta
  bpuResp.s3_meta.ittageMeta := ittage.io.out.s3_meta
  bpuResp.s3_meta.rasMeta    := ras.io.out.s3_meta

  val totalMetaSize = (new PredictorMeta).getWidth
  println(s"total meta size: $totalMetaSize\n")

  io.toFtq.resp.valid := s1_valid && s2_ready || s2_fire && s2_override || s3_fire && s3_override
  io.toFtq.resp.bits  := bpuResp

  // s0_stall should be exclusive with any other PC source
  s0_stall := !(s1_valid || s2_override || s3_override || do_redirect.valid)

  // Power-on reset
  val powerOnResetState = RegInit(true.B)
  when(s0_fire) {
    // When BPU pipeline first time fire, we consider power-on reset is done
    powerOnResetState := false.B
  }
  XSError(
    !powerOnResetState && s0_stall && s0_pc =/= s0_pc_reg,
    "s0_stall but s0_pc is differenct from s0_pc_reg"
  )

  npcGen.register(true.B, s0_pc_reg, Some("stallPC"), 0)
  foldedGhGen.register(true.B, s0_folded_gh_reg, Some("stallFGH"), 0)
  ghistPtrGen.register(true.B, s0_ghist_ptr_reg, Some("stallGHPtr"), 0)
  lastBrNumOHGen.register(true.B, s0_last_br_num_oh_reg, Some("stallBrNumOH"), 0)
  aheadFhObGen.register(true.B, s0_ahead_fh_oldest_bits_reg, Some("stallAFHOB"), 0)

  // History manage
  // s1
  val s1_possible_predicted_ghist_ptrs = (0 to numBr).map(s1_ghist_ptr - _.U)

  val s1_predicted_ghist_ptr = Mux1H(bpuResp.s1.lastBrPosOH, s1_possible_predicted_ghist_ptrs)

  val s1_possible_predicted_fhs = (0 to numBr).map(i =>
    s1_folded_gh.update(s1_ahead_fh_oldest_bits, s1_last_br_num_oh, i, bpuResp.s1.brTaken & bpuResp.s1.lastBrPosOH(i))
  )

  val s1_predicted_fh = Mux1H(bpuResp.s1.lastBrPosOH, s1_possible_predicted_fhs)

  val s1_ahead_fh_ob_src = Wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  s1_ahead_fh_ob_src.read(ghv, s1_ghist_ptr)

  if (EnableGHistDiff) {
    val s1_predicted_ghist = WireInit(getHist(s1_predicted_ghist_ptr).asTypeOf(Vec(HistoryLength, Bool())))
    for (i <- 0 until numBr) {
      when(bpuResp.s1.shouldShiftVec(i)) {
        s1_predicted_ghist(i) := bpuResp.s1.brTaken && (i == 0).B
      }
    }
    when(s1_valid) {
      s0_ghist := s1_predicted_ghist.asUInt
    }
  }

  val s1_ghv_wens = (0 until HistoryLength).map(n =>
    (0 until numBr).map(b =>
      s1_ghist_ptr.value === (CGHPtr(false.B, n.U) + b.U).value &&
        bpuResp.s1.shouldShiftVec(b) && s1_valid
    )
  )
  val s1_ghv_wdatas = (0 until HistoryLength).map(n =>
    Mux1H(
      (0 until numBr).map(b =>
        (
          s1_ghist_ptr.value === (CGHPtr(false.B, n.U) + b.U).value && bpuResp.s1.shouldShiftVec(b),
          bpuResp.s1.brTaken && bpuResp.s1.lastBrPosOH(b + 1)
        )
      )
    )
  )

  npcGen.register(s1_valid, bpuResp.s1.getTarget, Some("s1_target"), 4)
  foldedGhGen.register(s1_valid, s1_predicted_fh, Some("s1_FGH"), 4)
  ghistPtrGen.register(s1_valid, s1_predicted_ghist_ptr, Some("s1_GHPtr"), 4)
  lastBrNumOHGen.register(s1_valid, bpuResp.s1.lastBrPosOH.asUInt, Some("s1_BrNumOH"), 4)
  aheadFhObGen.register(s1_valid, s1_ahead_fh_ob_src, Some("s1_AFHOB"), 4)

  ghvBitWriteGens.zip(s1_ghv_wens).zipWithIndex.map { case ((b, w), i) =>
    b.register(w.reduce(_ || _), s1_ghv_wdatas(i), Some(s"s1_new_bit_$i"), 4)
  }

  class PreviousPredInfo extends Bundle {
    val hit         = Bool()
    val target      = PrunedAddr(VAddrBits)
    val lastBrPosOH = Vec(numBr + 1, Bool())
    val taken       = Bool()
    val takenMask   = Vec(numBr, Bool())
    val cfiIndex    = UInt(log2Ceil(PredictWidth).W)
  }

  def preds_needs_redirect_vec(x: PreviousPredInfo, y: BranchPredictionBundle) = {
    // Timing optimization
    // We first compare all target with previous stage target,
    // then select the difference by taken & hit
    // Usually target is generated quicker than taken, so do target compare before select can help timing
    val targetDiffVec: Vec[Bool] = VecInit(y.getAllTargets.map(_ =/= x.target))

    val targetDiff: Bool = selectByTaken(x.takenMask, x.hit, targetDiffVec)

    val lastBrPosOHDiff: Bool = x.lastBrPosOH.asUInt =/= y.lastBrPosOH.asUInt

    val takenDiff: Bool = x.taken =/= y.taken

    val takenOffsetDiff: Bool = x.taken && y.taken && x.cfiIndex =/= y.cfiIndex.bits

    VecInit(targetDiff, lastBrPosOHDiff, takenDiff, takenOffsetDiff)
    // x.shouldShiftVec.asUInt =/= y.shouldShiftVec.asUInt,
    // x.brTaken =/= y.brTaken
  }

  // s2
  val s2_possible_predicted_ghist_ptrs = (0 to numBr).map(s2_ghist_ptr - _.U)
  val s2_predicted_ghist_ptr           = Mux1H(bpuResp.s2.lastBrPosOH, s2_possible_predicted_ghist_ptrs)

  val s2_possible_predicted_fhs = (0 to numBr).map(i =>
    s2_folded_gh.update(
      s2_ahead_fh_oldest_bits,
      s2_last_br_num_oh,
      i,
      if (i > 0) bpuResp.s2.full_pred.br_taken_mask(i - 1) else false.B
    )
  )

  val s2_predicted_fh = Mux1H(bpuResp.s2.lastBrPosOH, s2_possible_predicted_fhs)

  val s2_ahead_fh_ob_src = Wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  s2_ahead_fh_ob_src.read(ghv, s2_ghist_ptr)

  if (EnableGHistDiff) {
    val s2_predicted_ghist = WireInit(getHist(s2_predicted_ghist_ptr).asTypeOf(Vec(HistoryLength, Bool())))
    for (i <- 0 until numBr) {
      when(bpuResp.s2.shouldShiftVec(i)) {
        s2_predicted_ghist(i) := bpuResp.s2.brTaken && (i == 0).B
      }
    }
    when(s2_override) {
      s0_ghist := s2_predicted_ghist.asUInt
    }
  }

  val s2_ghv_wens = (0 until HistoryLength).map(n =>
    (0 until numBr).map(b =>
      s2_ghist_ptr.value === (CGHPtr(false.B, n.U) + b.U).value &&
        bpuResp.s2.shouldShiftVec(b) && s2_override
    )
  )
  val s2_ghv_wdatas = (0 until HistoryLength).map(n =>
    Mux1H(
      (0 until numBr).map(b =>
        (
          s2_ghist_ptr.value === (CGHPtr(false.B, n.U) + b.U).value && bpuResp.s2.shouldShiftVec(b),
          bpuResp.s2.full_pred.real_br_taken_mask()(b)
        )
      )
    )
  )

  val s1_pred_info = Wire(new PreviousPredInfo)
  s1_pred_info.hit         := bpuResp.s1.full_pred.hit
  s1_pred_info.target      := bpuResp.s1.getTarget
  s1_pred_info.lastBrPosOH := bpuResp.s1.lastBrPosOH
  s1_pred_info.taken       := bpuResp.s1.taken
  s1_pred_info.takenMask   := bpuResp.s1.full_pred.taken_mask_on_slot
  s1_pred_info.cfiIndex    := bpuResp.s1.cfiIndex.bits

  val previous_s1_pred_info = RegEnable(s1_pred_info, 0.U.asTypeOf(new PreviousPredInfo), s1_fire)

  val s2_redirect_s1_last_pred_vec = preds_needs_redirect_vec(previous_s1_pred_info, bpuResp.s2)

  s2_override := s2_fire && s2_redirect_s1_last_pred_vec.reduce(_ || _)

  npcGen.register(s2_override, bpuResp.s2.getTarget, Some("s2_target"), 5)
  foldedGhGen.register(s2_override, s2_predicted_fh, Some("s2_FGH"), 5)
  ghistPtrGen.register(s2_override, s2_predicted_ghist_ptr, Some("s2_GHPtr"), 5)
  lastBrNumOHGen.register(s2_override, bpuResp.s2.lastBrPosOH.asUInt, Some("s2_BrNumOH"), 5)
  aheadFhObGen.register(s2_override, s2_ahead_fh_ob_src, Some("s2_AFHOB"), 5)

  ghvBitWriteGens.zip(s2_ghv_wens).zipWithIndex.map { case ((b, w), i) =>
    b.register(w.reduce(_ || _), s2_ghv_wdatas(i), Some(s"s2_new_bit_$i"), 5)
  }

  XSPerfAccumulate("s2_redirect_because_target_diff", s2_fire && s2_redirect_s1_last_pred_vec(0))
  XSPerfAccumulate("s2_redirect_because_branch_num_diff", s2_fire && s2_redirect_s1_last_pred_vec(1))
  XSPerfAccumulate("s2_redirect_because_direction_diff", s2_fire && s2_redirect_s1_last_pred_vec(2))
  XSPerfAccumulate("s2_redirect_because_cfi_idx_diff", s2_fire && s2_redirect_s1_last_pred_vec(3))
  // XSPerfAccumulate("s2_redirect_because_shouldShiftVec_diff", s2_fire && s2_redirect_s1_last_pred_vec(4))
  // XSPerfAccumulate("s2_redirect_because_brTaken_diff", s2_fire && s2_redirect_s1_last_pred_vec(5))
  XSPerfAccumulate("s2_redirect_because_fallThroughError", s2_fire && bpuResp.s2.fallThruError)

  XSPerfAccumulate("s2_redirect_when_taken", s2_override && bpuResp.s2.taken && bpuResp.s2.full_pred.hit)
  XSPerfAccumulate("s2_redirect_when_not_taken", s2_override && !bpuResp.s2.taken && bpuResp.s2.full_pred.hit)
  XSPerfAccumulate("s2_redirect_when_not_hit", s2_override && !bpuResp.s2.full_pred.hit)

  // s3
  val s3_possible_predicted_ghist_ptrs = (0 to numBr).map(s3_ghist_ptr - _.U)
  val s3_predicted_ghist_ptr           = Mux1H(bpuResp.s3.lastBrPosOH, s3_possible_predicted_ghist_ptrs)

  val s3_possible_predicted_fhs = (0 to numBr).map(i =>
    s3_folded_gh.update(
      s3_ahead_fh_oldest_bits,
      s3_last_br_num_oh,
      i,
      if (i > 0) bpuResp.s3.full_pred.br_taken_mask(i - 1) else false.B
    )
  )

  val s3_predicted_fh = Mux1H(bpuResp.s3.lastBrPosOH, s3_possible_predicted_fhs)

  val s3_ahead_fh_ob_src = Wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  s3_ahead_fh_ob_src.read(ghv, s3_ghist_ptr)

  if (EnableGHistDiff) {
    val s3_predicted_ghist = WireInit(getHist(s3_predicted_ghist_ptr).asTypeOf(Vec(HistoryLength, Bool())))
    for (i <- 0 until numBr) {
      when(bpuResp.s3.shouldShiftVec(i)) {
        s3_predicted_ghist(i) := bpuResp.s3.brTaken && (i == 0).B
      }
    }
    when(s3_override) {
      s0_ghist := s3_predicted_ghist.asUInt
    }
  }

  val s3_ghv_wens = (0 until HistoryLength).map(n =>
    (0 until numBr).map(b =>
      s3_ghist_ptr.value === (CGHPtr(false.B, n.U) + b.U).value && bpuResp.s3.shouldShiftVec(b) && s3_override
    )
  )
  val s3_ghv_wdatas = (0 until HistoryLength).map(n =>
    Mux1H(
      (0 until numBr).map(b =>
        (
          s3_ghist_ptr.value === (CGHPtr(false.B, n.U) + b.U).value && bpuResp.s3.shouldShiftVec(b),
          bpuResp.s3.full_pred.real_br_taken_mask()(b)
        )
      )
    )
  )

  // To optimize Clock Gating Efficiency of previous_s2_*
  val previous_s2_pred = Wire(new BranchPredictionBundle(isNotS3 = true))
  previous_s2_pred.pc := RegEnable(bpuResp.s2.pc, 0.U.asTypeOf(bpuResp.s2.pc), s2_fire).suggestName(
    s"previous_s2_pred_pc"
  )
  previous_s2_pred.valid := RegEnable(bpuResp.s2.valid, 0.U.asTypeOf(bpuResp.s2.valid), s2_fire).suggestName(
    s"previous_s2_pred_valid"
  )
  previous_s2_pred.hasRedirect := RegEnable(
    bpuResp.s2.hasRedirect,
    0.U.asTypeOf(bpuResp.s2.hasRedirect),
    s2_fire
  ).suggestName(s"previous_s2_pred_hasRedirect")
  previous_s2_pred.ftq_idx := RegEnable(bpuResp.s2.ftq_idx, 0.U.asTypeOf(bpuResp.s2.ftq_idx), s2_fire).suggestName(
    s"previous_s2_pred_ftq_idx"
  )
  previous_s2_pred.full_pred := RegEnable(
    bpuResp.s2.full_pred,
    0.U.asTypeOf(bpuResp.s2.full_pred),
    s2_fire
  ).suggestName(s"previous_s2_pred_full_pred")

  previous_s2_pred.full_pred.targets.zip(bpuResp.s2.full_pred.taken_mask_on_slot.zipWithIndex).map {
    case (target, (taken_mask, slotIdx)) =>
      // This enable signal can better improve CGE, but it may lead to timing violations:
      //    s2_fire_dup(0) && !new_fp.taken_mask_on_slot.take(slotIdx).fold(false.B)(_||_) && taken_mask && new_fp.hit
      target := RegEnable(
        bpuResp.s2.full_pred.targets(slotIdx),
        0.U.asTypeOf(bpuResp.s2.full_pred.targets(slotIdx)),
        s2_fire && taken_mask
      )
  }
  // This enable signal can better improve CGE, but it may lead to timing violations:
  //    s2_fire_dup(0) && new_fp.hit && !new_fp.taken_mask_on_slot.reduce(_||_)
  previous_s2_pred.full_pred.fallThroughAddr := RegEnable(
    bpuResp.s2.full_pred.fallThroughAddr,
    0.U.asTypeOf(bpuResp.s2.full_pred.fallThroughAddr),
    s2_fire && bpuResp.s2.full_pred.hit && !bpuResp.s2.full_pred.taken_mask_on_slot(0)
  )

  val s3_redirect_on_br_taken =
    bpuResp.s3.full_pred.real_br_taken_mask().asUInt =/= previous_s2_pred.full_pred.real_br_taken_mask().asUInt

  val s3_both_first_taken =
    bpuResp.s3.full_pred.real_br_taken_mask()(0) && previous_s2_pred.full_pred.real_br_taken_mask()(0)
  val s3_redirect_on_target = bpuResp.s3.getTarget =/= previous_s2_pred.getTarget
  val s3_redirect_on_jalr_target =
    bpuResp.s3.full_pred.hit_taken_on_jalr && bpuResp.s3.full_pred.jalr_target =/= previous_s2_pred.full_pred.jalr_target

  val s3_redirect_on_fall_thru_error = bpuResp.s3.fallThruError
  val s3_redirect_on_ftb_multi_hit   = bpuResp.s3.ftbMultiHit

  s3_override := s3_fire && (
    (s3_redirect_on_br_taken && !s3_both_first_taken) ||
      s3_redirect_on_target || s3_redirect_on_fall_thru_error || s3_redirect_on_ftb_multi_hit
  )

  XSPerfAccumulate(f"s3_redirect_on_br_taken", s3_fire && s3_redirect_on_br_taken)
  XSPerfAccumulate(f"s3_redirect_on_jalr_target", s3_fire && s3_redirect_on_jalr_target)
  XSPerfAccumulate(
    f"s3_redirect_on_others",
    s3_override && !(s3_redirect_on_br_taken || s3_redirect_on_jalr_target)
  )

  npcGen.register(s3_override, bpuResp.s3.getTarget, Some("s3_target"), 3)
  foldedGhGen.register(s3_override, s3_predicted_fh, Some("s3_FGH"), 3)
  ghistPtrGen.register(s3_override, s3_predicted_ghist_ptr, Some("s3_GHPtr"), 3)
  lastBrNumOHGen.register(s3_override, bpuResp.s3.lastBrPosOH.asUInt, Some("s3_BrNumOH"), 3)
  aheadFhObGen.register(s3_override, s3_ahead_fh_ob_src, Some("s3_AFHOB"), 3)

  ghvBitWriteGens.zip(s3_ghv_wens).zipWithIndex.map { case ((b, w), i) =>
    b.register(w.reduce(_ || _), s3_ghv_wdatas(i), Some(s"s3_new_bit_$i"), 3)
  }

  val redirectBits = do_redirect.bits

  // Redirect logic
  val shift       = redirectBits.cfiUpdate.shift
  val addIntoHist = redirectBits.cfiUpdate.addIntoHist
  // TODO: remove these below
  val shouldShiftVec =
    Mux(
      shift === 0.U,
      VecInit(0.U((1 << (log2Ceil(numBr) + 1)).W).asBools),
      VecInit(LowerMask(1.U << (shift - 1.U)).asBools)
    )

  // TODO end

  val taken              = redirectBits.cfiUpdate.taken
  val real_br_taken_mask = (0 until numBr).map(i => shift === (i + 1).U && taken && addIntoHist)

  val oldPtr      = redirectBits.cfiUpdate.histPtr
  val updated_ptr = oldPtr - shift
  def computeFoldedHist(hist: UInt, compLen: Int)(histLen: Int): UInt =
    if (histLen > 0) {
      val nChunks     = (histLen + compLen - 1) / compLen
      val hist_chunks = (0 until nChunks) map { i => hist(min((i + 1) * compLen, histLen) - 1, i * compLen) }
      ParallelXOR(hist_chunks)
    } else 0.U

  val oldFh = WireInit(0.U.asTypeOf(new AllFoldedHistories(foldedGHistInfos)))
  foldedGHistInfos.foreach { case (histLen, compLen) =>
    oldFh.getHistWithInfo((histLen, compLen)).folded_hist := computeFoldedHist(getHist(oldPtr), compLen)(histLen)
  }

  val updated_fh    = VecInit((0 to numBr).map(i => oldFh.update(ghv, oldPtr, i, taken && addIntoHist)))(shift)
  val thisBrNumOH   = UIntToOH(shift, numBr + 1)
  val thisAheadFhOb = Wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  thisAheadFhOb.read(ghv, oldPtr)
  val redirect_ghv_wens = (0 until HistoryLength).map(n =>
    (0 until numBr).map(b =>
      oldPtr.value === (CGHPtr(false.B, n.U) + b.U).value && shouldShiftVec(b) && do_redirect.valid
    )
  )
  val redirect_ghv_wdatas = (0 until HistoryLength).map(n =>
    Mux1H(
      (0 until numBr).map(b => oldPtr.value === (CGHPtr(false.B, n.U) + b.U).value && shouldShiftVec(b)),
      real_br_taken_mask
    )
  )

  if (EnableGHistDiff) {
    val updated_ghist = WireInit(getHist(updated_ptr).asTypeOf(Vec(HistoryLength, Bool())))
    for (i <- 0 until numBr) {
      when(shift >= (i + 1).U) {
        updated_ghist(i) := taken && addIntoHist && (i == 0).B
      }
    }
    when(do_redirect.valid) {
      s0_ghist := updated_ghist.asUInt
    }
  }

  // Commit time history checker
  if (EnableCommitGHistDiff) {
    val commitGHist    = RegInit(0.U.asTypeOf(Vec(HistoryLength, Bool())))
    val commitGHistPtr = RegInit(0.U.asTypeOf(new CGHPtr))
    def getCommitHist(ptr: CGHPtr): UInt =
      (Cat(commitGHist.asUInt, commitGHist.asUInt) >> (ptr.value + 1.U))(HistoryLength - 1, 0)

    val updateValid:         Bool      = io.fromFtq.update.valid
    val branchValidMask:     UInt      = io.fromFtq.update.bits.ftb_entry.brValids.asUInt
    val branchCommittedMask: Vec[Bool] = io.fromFtq.update.bits.br_committed
    val misPredictMask:      UInt      = io.fromFtq.update.bits.mispred_mask.asUInt
    val takenMask: UInt = io.fromFtq.update.bits.br_taken_mask.asUInt |
      io.fromFtq.update.bits.ftb_entry.strong_bias.asUInt // Always taken branch is recorded in history
    val takenIdx:      UInt = (PriorityEncoder(takenMask) + 1.U((log2Ceil(numBr) + 1).W)).asUInt
    val misPredictIdx: UInt = (PriorityEncoder(misPredictMask) + 1.U((log2Ceil(numBr) + 1).W)).asUInt
    val shouldShiftMask: UInt = Mux(takenMask.orR, LowerMask(takenIdx).asUInt, ((1 << numBr) - 1).asUInt) &
      Mux(misPredictMask.orR, LowerMask(misPredictIdx).asUInt, ((1 << numBr) - 1).asUInt) &
      branchCommittedMask.asUInt
    val updateShift: UInt =
      Mux(updateValid && branchValidMask.orR, PopCount(branchValidMask & shouldShiftMask), 0.U)

    // Maintain the commitGHist
    for (i <- 0 until numBr) {
      when(updateShift >= (i + 1).U) {
        val ptr: CGHPtr = commitGHistPtr - i.asUInt
        commitGHist(ptr.value) := takenMask(i)
      }
    }
    when(updateValid) {
      commitGHistPtr := commitGHistPtr - updateShift
    }

    // Calculate true history using Parallel XOR
    // Do differential
    TageTableInfos.map {
      case (nRows, histLen, _) => {
        val nRowsPerBr      = nRows / numBr
        val predictGHistPtr = io.fromFtq.update.bits.spec_info.histPtr
        val commitTrueHist: UInt = computeFoldedHist(getCommitHist(commitGHistPtr), log2Ceil(nRowsPerBr))(histLen)
        val predictFHist:   UInt = computeFoldedHist(getHist(predictGHistPtr), log2Ceil(nRowsPerBr))(histLen)
        XSWarn(
          updateValid && predictFHist =/= commitTrueHist,
          p"predict time ghist: ${predictFHist} is different from commit time: ${commitTrueHist}\n"
        )
      }
    }
  }

  npcGen.register(do_redirect.valid, PrunedAddrInit(do_redirect.bits.cfiUpdate.target), Some("redirect_target"), 2)
  foldedGhGen.register(do_redirect.valid, updated_fh, Some("redirect_FGHT"), 2)
  ghistPtrGen.register(do_redirect.valid, updated_ptr, Some("redirect_GHPtr"), 2)
  lastBrNumOHGen.register(do_redirect.valid, thisBrNumOH, Some("redirect_BrNumOH"), 2)
  aheadFhObGen.register(do_redirect.valid, thisAheadFhOb, Some("redirect_AFHOB"), 2)

  ghvBitWriteGens.zip(redirect_ghv_wens).zipWithIndex.map { case ((b, w), i) =>
    b.register(w.reduce(_ || _), redirect_ghv_wdatas(i), Some(s"redirect_new_bit_$i"), 2)
  }
  // no need to assign s0_last_pred

  // val need_reset = RegNext(reset.asBool) && !reset.asBool

  // Reset
  // npcGen.register(need_reset, resetVector.U, Some("reset_pc"), 1)
  // foldedGhGen.register(need_reset, 0.U.asTypeOf(s0_folded_gh), Some("reset_FGH"), 1)
  // ghistPtrGen.register(need_reset, 0.U.asTypeOf(new CGHPtr), Some("reset_GHPtr"), 1)

  s0_pc                   := npcGen()
  s0_folded_gh            := foldedGhGen()
  s0_ghist_ptr            := ghistPtrGen()
  s0_ahead_fh_oldest_bits := aheadFhObGen()
  s0_last_br_num_oh       := lastBrNumOHGen()

  (ghv_write_datas zip ghvBitWriteGens).map { case (wd, d) => wd := d() }
  for (i <- 0 until HistoryLength) {
    ghv_wens(i) := Seq(s1_ghv_wens, s2_ghv_wens, s3_ghv_wens, redirect_ghv_wens).map(_(i).reduce(_ || _)).reduce(_ || _)
    when(ghv_wens(i)) {
      ghv(i) := ghv_write_datas(i)
    }
  }

  // ========================= Topdown ========================= //
  def numOfStage = 3
  require(numOfStage > 1, "BPU numOfStage must be greater than 1")
  val topdown_stages = RegInit(VecInit(Seq.fill(numOfStage)(0.U.asTypeOf(new FrontendTopDownBundle))))

  // following can only happen on s1
  val controlRedirectBubble = Wire(Bool())
  val ControlBTBMissBubble  = Wire(Bool())
  val TAGEMissBubble        = Wire(Bool())
  val SCMissBubble          = Wire(Bool())
  val ITTAGEMissBubble      = Wire(Bool())
  val RASMissBubble         = Wire(Bool())

  val memVioRedirectBubble = Wire(Bool())
  val otherRedirectBubble  = Wire(Bool())
  val btbMissBubble        = Wire(Bool())
  otherRedirectBubble  := false.B
  memVioRedirectBubble := false.B

  // override can happen between s1-s2 and s2-s3
  val overrideBubble = Wire(Vec(numOfStage - 1, Bool()))
  def overrideStage  = 1
  // ftq update block can happen on s1, s2 and s3
  val ftqUpdateBubble = Wire(Vec(numOfStage, Bool()))
  def ftqUpdateStage  = 0
  // ftq full stall only happens on s3 (last stage)
  val ftqFullStall = Wire(Bool())

  // by default, no bubble event
  topdown_stages(0) := 0.U.asTypeOf(new FrontendTopDownBundle)
  // event movement driven by clock only
  for (i <- 0 until numOfStage - 1) {
    topdown_stages(i + 1) := topdown_stages(i)
  }

  // TODO: signals for memVio and other Redirects
  controlRedirectBubble := do_redirect.valid && do_redirect.bits.ControlRedirectBubble
  ControlBTBMissBubble  := do_redirect.bits.ControlBTBMissBubble
  TAGEMissBubble        := do_redirect.bits.TAGEMissBubble
  SCMissBubble          := do_redirect.bits.SCMissBubble
  ITTAGEMissBubble      := do_redirect.bits.ITTAGEMissBubble
  RASMissBubble         := do_redirect.bits.RASMissBubble

  memVioRedirectBubble := do_redirect.valid && do_redirect.bits.MemVioRedirectBubble
  otherRedirectBubble  := do_redirect.valid && do_redirect.bits.OtherRedirectBubble
  btbMissBubble        := do_redirect.valid && do_redirect.bits.BTBMissBubble
  overrideBubble(0)    := s2_override
  overrideBubble(1)    := s3_override
  ftqUpdateBubble(0)   := !s1_predictorsReady
  ftqUpdateBubble(1)   := false.B
  ftqUpdateBubble(2)   := false.B
  ftqFullStall         := !io.toFtq.resp.ready
  bpuResp.topdown_info := topdown_stages(numOfStage - 1)

  // topdown handling logic here
  when(controlRedirectBubble) {
    /*
    for (i <- 0 until numOfStage)
      topdown_stages(i).reasons(TopDownCounters.ControlRedirectBubble.id) := true.B
    bpuResp.topdown_info.reasons(TopDownCounters.ControlRedirectBubble.id) := true.B
     */
    when(ControlBTBMissBubble) {
      for (i <- 0 until numOfStage)
        topdown_stages(i).reasons(TopDownCounters.BTBMissBubble.id)  := true.B
      bpuResp.topdown_info.reasons(TopDownCounters.BTBMissBubble.id) := true.B
    }.elsewhen(TAGEMissBubble) {
      for (i <- 0 until numOfStage)
        topdown_stages(i).reasons(TopDownCounters.TAGEMissBubble.id)  := true.B
      bpuResp.topdown_info.reasons(TopDownCounters.TAGEMissBubble.id) := true.B
    }.elsewhen(SCMissBubble) {
      for (i <- 0 until numOfStage)
        topdown_stages(i).reasons(TopDownCounters.SCMissBubble.id)  := true.B
      bpuResp.topdown_info.reasons(TopDownCounters.SCMissBubble.id) := true.B
    }.elsewhen(ITTAGEMissBubble) {
      for (i <- 0 until numOfStage)
        topdown_stages(i).reasons(TopDownCounters.ITTAGEMissBubble.id)  := true.B
      bpuResp.topdown_info.reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
    }.elsewhen(RASMissBubble) {
      for (i <- 0 until numOfStage)
        topdown_stages(i).reasons(TopDownCounters.RASMissBubble.id)  := true.B
      bpuResp.topdown_info.reasons(TopDownCounters.RASMissBubble.id) := true.B
    }
  }
  when(memVioRedirectBubble) {
    for (i <- 0 until numOfStage)
      topdown_stages(i).reasons(TopDownCounters.MemVioRedirectBubble.id)  := true.B
    bpuResp.topdown_info.reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
  }
  when(otherRedirectBubble) {
    for (i <- 0 until numOfStage)
      topdown_stages(i).reasons(TopDownCounters.OtherRedirectBubble.id)  := true.B
    bpuResp.topdown_info.reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
  }
  when(btbMissBubble) {
    for (i <- 0 until numOfStage)
      topdown_stages(i).reasons(TopDownCounters.BTBMissBubble.id)  := true.B
    bpuResp.topdown_info.reasons(TopDownCounters.BTBMissBubble.id) := true.B
  }

  for (i <- 0 until numOfStage) {
    if (i < numOfStage - overrideStage) {
      when(overrideBubble(i)) {
        for (j <- 0 to i)
          topdown_stages(j).reasons(TopDownCounters.OverrideBubble.id) := true.B
      }
    }
    if (i < numOfStage - ftqUpdateStage) {
      when(ftqUpdateBubble(i)) {
        topdown_stages(i).reasons(TopDownCounters.FtqUpdateBubble.id) := true.B
      }
    }
  }
  when(ftqFullStall) {
    topdown_stages(0).reasons(TopDownCounters.FtqFullStall.id) := true.B
  }

  // ========================== Debug ========================= //
  XSError(
    isBefore(redirectBits.cfiUpdate.histPtr, s3_ghist_ptr) && do_redirect.valid,
    p"s3_ghist_ptr ${s3_ghist_ptr} exceeds redirect histPtr ${redirectBits.cfiUpdate.histPtr}\n"
  )
  XSError(
    isBefore(redirectBits.cfiUpdate.histPtr, s2_ghist_ptr) && do_redirect.valid,
    p"s2_ghist_ptr ${s2_ghist_ptr} exceeds redirect histPtr ${redirectBits.cfiUpdate.histPtr}\n"
  )
  XSError(
    isBefore(redirectBits.cfiUpdate.histPtr, s1_ghist_ptr) && do_redirect.valid,
    p"s1_ghist_ptr ${s1_ghist_ptr} exceeds redirect histPtr ${redirectBits.cfiUpdate.histPtr}\n"
  )

  XSDebug(RegNext(reset.asBool) && !reset.asBool, "Reseting...\n")
  XSDebug(io.fromFtq.update.valid, p"Update from ftq\n")
  XSDebug(io.fromFtq.redirect.valid, p"Redirect from ftq\n")

  XSDebug("[BP0]                 fire=%d                      pc=%x\n", s0_fire, s0_pc.toUInt)
  XSDebug(
    "[BP1] v=%d r=%d cr=%d fire=%d             flush=%d pc=%x\n",
    s1_valid,
    s1_ready,
    s1_predictorsReady,
    s1_fire,
    s1_flush,
    s1_pc.toUInt
  )
  XSDebug(
    "[BP2] v=%d r=%d cr=%d fire=%d redirect=%d flush=%d pc=%x\n",
    s2_valid,
    s2_ready,
    true.B,
    s2_fire,
    s2_override,
    s2_flush,
    s2_pc.toUInt
  )
  XSDebug(
    "[BP3] v=%d r=%d cr=%d fire=%d redirect=%d flush=%d pc=%x\n",
    s3_valid,
    s3_ready,
    true.B,
    s3_fire,
    s3_override,
    s3_flush,
    s3_pc.toUInt
  )
  XSDebug("[FTQ] ready=%d\n", io.toFtq.resp.ready)
  XSDebug("resp.s1.target=%x\n", bpuResp.s1.getTarget.toUInt)
  XSDebug("resp.s2.target=%x\n", bpuResp.s2.getTarget.toUInt)
  // XSDebug("s0_ghist: %b\n", s0_ghist.predHist)
  // XSDebug("s1_ghist: %b\n", s1_ghist.predHist)
  // XSDebug("s2_ghist: %b\n", s2_ghist.predHist)
  // XSDebug("s2_predicted_ghist: %b\n", s2_predicted_ghist.predHist)
  XSDebug(p"s0_ghist_ptr: $s0_ghist_ptr\n")
  XSDebug(p"s1_ghist_ptr: $s1_ghist_ptr\n")
  XSDebug(p"s2_ghist_ptr: $s2_ghist_ptr\n")
  XSDebug(p"s3_ghist_ptr: $s3_ghist_ptr\n")

  io.fromFtq.update.bits.display(io.fromFtq.update.valid)
  io.fromFtq.redirect.bits.display(io.fromFtq.redirect.valid)

  XSPerfAccumulate("s2_redirect", s2_override)
  XSPerfAccumulate("s3_redirect", s3_override)
  XSPerfAccumulate("s1_not_valid", !s1_valid)

  // ========================= Performance Counter ========================= //
  val perfEvents = uftb.getPerfEvents ++ ftb.getPerfEvents
  generatePerfEvent()
}

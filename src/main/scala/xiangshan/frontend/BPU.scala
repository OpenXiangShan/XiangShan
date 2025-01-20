/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import scala.math.min
import utility._
import xiangshan._

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

  def getFallThroughAddr(start: UInt, carry: Bool, pft: UInt) = {
    val higher = start.head(VAddrBits - log2Ceil(PredictWidth) - instOffsetBits)
    Cat(Mux(carry, higher + 1.U, higher), pft, 0.U(instOffsetBits.W))
  }

  def foldTag(tag: UInt, l: Int): UInt = {
    val nChunks = (tag.getWidth + l - 1) / l
    val chunks  = (0 until nChunks).map(i => tag(min((i + 1) * l, tag.getWidth) - 1, i * l))
    ParallelXOR(chunks)
  }
}

class BasePredictorInput(implicit p: Parameters) extends XSBundle with HasBPUConst {
  def nInputs = 1

  val s0_pc = Vec(numDup, UInt(VAddrBits.W))

  val folded_hist    = Vec(numDup, new AllFoldedHistories(foldedGHistInfos))
  val s1_folded_hist = Vec(numDup, new AllFoldedHistories(foldedGHistInfos))
  val ghist          = UInt(HistoryLength.W)

  val resp_in = Vec(nInputs, new BranchPredictionResp)

  // val final_preds = Vec(numBpStages, new)
  // val toFtq_fire = Bool()

  // val s0_all_ready = Bool()
}

class BasePredictorOutput(implicit p: Parameters) extends BranchPredictionResp {}

class BasePredictorIO(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val reset_vector = Input(UInt(PAddrBits.W))
  val in           = Flipped(DecoupledIO(new BasePredictorInput)) // TODO: Remove DecoupledIO
  // val out = DecoupledIO(new BasePredictorOutput)
  val out = Output(new BasePredictorOutput)
  // val flush_out = Valid(UInt(VAddrBits.W))

  val fauftb_entry_in      = Input(new FTBEntry)
  val fauftb_entry_hit_in  = Input(Bool())
  val fauftb_entry_out     = Output(new FTBEntry)
  val fauftb_entry_hit_out = Output(Bool())

  val ctrl = Input(new BPUCtrl)

  val s0_fire = Input(Vec(numDup, Bool()))
  val s1_fire = Input(Vec(numDup, Bool()))
  val s2_fire = Input(Vec(numDup, Bool()))
  val s3_fire = Input(Vec(numDup, Bool()))

  val s2_redirect = Input(Vec(numDup, Bool()))
  val s3_redirect = Input(Vec(numDup, Bool()))

  val s1_ready = Output(Bool())
  val s2_ready = Output(Bool())
  val s3_ready = Output(Bool())

  val update          = Flipped(Valid(new BranchPredictionUpdate))
  val redirect        = Flipped(Valid(new BranchPredictionRedirect))
  val redirectFromIFU = Input(Bool())
}

abstract class BasePredictor(implicit p: Parameters) extends XSModule
    with HasBPUConst with BPUUtils with HasPerfEvents {
  val meta_size      = 0
  val spec_meta_size = 0
  val is_fast_pred   = false
  val io             = IO(new BasePredictorIO())

  io.out := io.in.bits.resp_in(0)

  io.fauftb_entry_out     := io.fauftb_entry_in
  io.fauftb_entry_hit_out := io.fauftb_entry_hit_in

  io.out.last_stage_meta := 0.U

  io.in.ready := !io.redirect.valid

  io.s1_ready := true.B
  io.s2_ready := true.B
  io.s3_ready := true.B

  val s0_pc_dup = WireInit(io.in.bits.s0_pc) // fetchIdx(io.f0_pc)
  val s1_pc_dup = s0_pc_dup.zip(io.s0_fire).map { case (s0_pc, s0_fire) => RegEnable(s0_pc, s0_fire) }
  val s2_pc_dup = s1_pc_dup.zip(io.s1_fire).map { case (s1_pc, s1_fire) =>
    SegmentedAddrNext(s1_pc, pcSegments, s1_fire, Some("s2_pc"))
  }
  val s3_pc_dup = s2_pc_dup.zip(io.s2_fire).map { case (s2_pc, s2_fire) =>
    SegmentedAddrNext(s2_pc, s2_fire, Some("s3_pc"))
  }

  when(RegNext(RegNext(reset.asBool) && !reset.asBool)) {
    s1_pc_dup.map { case s1_pc => s1_pc := io.reset_vector }
  }

  io.out.s1.pc := s1_pc_dup
  io.out.s2.pc := s2_pc_dup.map(_.getAddr())
  io.out.s3.pc := s3_pc_dup.map(_.getAddr())

  val perfEvents: Seq[(String, UInt)] = Seq()

  def getFoldedHistoryInfo: Option[Set[FoldedHistoryInfo]] = None
}

class FakePredictor(implicit p: Parameters) extends BasePredictor {
  io.in.ready            := true.B
  io.out.last_stage_meta := 0.U
  io.out                 := io.in.bits.resp_in(0)
}

class BpuToFtqIO(implicit p: Parameters) extends XSBundle {
  val resp = DecoupledIO(new BpuToFtqBundle())
}

class BpuIO(implicit p: Parameters) extends XSBundle {
  val bpu_to_ftq   = new BpuToFtqIO()
  val ftq_to_bpu   = Flipped(new FtqToBpuIO)
  val ctrl         = Input(new BPUCtrl)
  val reset_vector = Input(UInt(PAddrBits.W))
}

class Bpu(implicit p: Parameters) extends XSModule with HasBPUConst with HasPerfEvents
    with HasCircularQueuePtrHelper {
  val io = IO(new BpuIO)

  val ctrl       = DelayN(io.ctrl, 1)
  val predictors = Module(if (useBPD) new Composer else new FakePredictor)
  val history    = Module(new History)

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

  // ctrl signal
  predictors.io.ctrl         := ctrl
  predictors.io.reset_vector := io.reset_vector

  val s0_stall_dup = dup_wire(Bool()) // For some reason s0 stalled, usually FTQ Full
  val s0_fire_dup, s1_fire_dup, s2_fire_dup, s3_fire_dup                        = dup_wire(Bool())
  val s1_valid_dup, s2_valid_dup, s3_valid_dup                                  = dup_seq(RegInit(false.B))
  val s1_ready_dup, s2_ready_dup, s3_ready_dup                                  = dup_wire(Bool())
  val s1_components_ready_dup, s2_components_ready_dup, s3_components_ready_dup = dup_wire(Bool())

  val s0_pc_dup     = dup(WireInit(0.U.asTypeOf(UInt(VAddrBits.W))))
  val s0_pc_reg_dup = s0_pc_dup.zip(s0_stall_dup).map { case (s0_pc, s0_stall) => RegEnable(s0_pc, !s0_stall) }
  when(RegNext(RegNext(reset.asBool) && !reset.asBool)) {
    s0_pc_reg_dup.map { case s0_pc => s0_pc := io.reset_vector }
  }
  val s1_pc = RegEnable(s0_pc_dup(0), s0_fire_dup(0))
  val s2_pc = RegEnable(s1_pc, s1_fire_dup(0))
  val s3_pc = RegEnable(s2_pc, s2_fire_dup(0))

  val npcGen_dup = Seq.tabulate(numDup)(n => new PhyPriorityMuxGenerator[UInt])

  val ghv_wire = history.io.ghv_wire
  def getHist(ptr: CGHPtr): UInt = (Cat(ghv_wire.asUInt, ghv_wire.asUInt) >> (ptr.value + 1.U))(HistoryLength - 1, 0)

  val resp = predictors.io.out

  val s1_flush_dup, s2_flush_dup, s3_flush_dup = dup_wire(Bool())
  val s2_redirect_dup, s3_redirect_dup         = dup_wire(Bool())

  history.io.resp            := resp
  history.io.s1_valid_dup    := s1_valid_dup
  history.io.s0_fire_dup     := s0_fire_dup
  history.io.s1_fire_dup     := s1_fire_dup
  history.io.s2_fire_dup     := s2_fire_dup
  history.io.s0_stall_dup    := s0_stall_dup
  history.io.s2_redirect_dup := s2_redirect_dup
  history.io.s3_redirect_dup := s3_redirect_dup
  history.io.ftq_to_bpu      := io.ftq_to_bpu

  val s0_ghist         = history.io.s0_ghist
  val s0_folded_gh_dup = history.io.s0_folded_gh_dup
  val s1_folded_gh_dup = history.io.s1_folded_gh_dup
  val s3_ghist_ptr_dup = history.io.s3_ghist_ptr_dup

  // predictors.io := DontCare
  predictors.io.in.valid               := s0_fire_dup(0)
  predictors.io.in.bits.s0_pc          := s0_pc_dup
  predictors.io.in.bits.ghist          := s0_ghist
  predictors.io.in.bits.folded_hist    := s0_folded_gh_dup
  predictors.io.in.bits.s1_folded_hist := s1_folded_gh_dup
  predictors.io.in.bits.resp_in(0)     := 0.U.asTypeOf(new BranchPredictionResp)
  predictors.io.fauftb_entry_in        := 0.U.asTypeOf(new FTBEntry)
  predictors.io.fauftb_entry_hit_in    := false.B
  predictors.io.redirectFromIFU        := RegNext(io.ftq_to_bpu.redirctFromIFU, init = false.B)

  val redirect_req    = io.ftq_to_bpu.redirect
  val do_redirect_dup = dup_seq(RegNextWithEnable(redirect_req))

  // Pipeline logic
  s2_redirect_dup.map(_ := false.B)
  s3_redirect_dup.map(_ := false.B)

  s3_flush_dup.map(_ := redirect_req.valid) // flush when redirect comes
  for (((s2_flush, s3_flush), s3_redirect) <- s2_flush_dup zip s3_flush_dup zip s3_redirect_dup)
    s2_flush := s3_flush || s3_redirect
  for (((s1_flush, s2_flush), s2_redirect) <- s1_flush_dup zip s2_flush_dup zip s2_redirect_dup)
    s1_flush := s2_flush || s2_redirect

  s1_components_ready_dup.map(_ := predictors.io.s1_ready)
  for (((s1_ready, s1_fire), s1_valid) <- s1_ready_dup zip s1_fire_dup zip s1_valid_dup)
    s1_ready := s1_fire || !s1_valid
  for (((s0_fire, s1_components_ready), s1_ready) <- s0_fire_dup zip s1_components_ready_dup zip s1_ready_dup)
    s0_fire             := s1_components_ready && s1_ready
  predictors.io.s0_fire := s0_fire_dup

  s2_components_ready_dup.map(_ := predictors.io.s2_ready)
  for (((s2_ready, s2_fire), s2_valid) <- s2_ready_dup zip s2_fire_dup zip s2_valid_dup)
    s2_ready := s2_fire || !s2_valid
  for (
    (((s1_fire, s2_components_ready), s2_ready), s1_valid) <-
      s1_fire_dup zip s2_components_ready_dup zip s2_ready_dup zip s1_valid_dup
  )
    s1_fire := s1_valid && s2_components_ready && s2_ready && io.bpu_to_ftq.resp.ready

  s3_components_ready_dup.map(_ := predictors.io.s3_ready)
  for (((s3_ready, s3_fire), s3_valid) <- s3_ready_dup zip s3_fire_dup zip s3_valid_dup)
    s3_ready := s3_fire || !s3_valid
  for (
    (((s2_fire, s3_components_ready), s3_ready), s2_valid) <-
      s2_fire_dup zip s3_components_ready_dup zip s3_ready_dup zip s2_valid_dup
  )
    s2_fire := s2_valid && s3_components_ready && s3_ready

  for ((((s0_fire, s1_flush), s1_fire), s1_valid) <- s0_fire_dup zip s1_flush_dup zip s1_fire_dup zip s1_valid_dup) {
    when(redirect_req.valid)(s1_valid := false.B)
      .elsewhen(s0_fire)(s1_valid := true.B)
      .elsewhen(s1_flush)(s1_valid := false.B)
      .elsewhen(s1_fire)(s1_valid := false.B)
  }
  predictors.io.s1_fire := s1_fire_dup

  for (
    ((((s1_fire, s2_flush), s2_fire), s2_valid), s1_flush) <-
      s1_fire_dup zip s2_flush_dup zip s2_fire_dup zip s2_valid_dup zip s1_flush_dup
  ) {

    when(s2_flush)(s2_valid := false.B)
      .elsewhen(s1_fire)(s2_valid := !s1_flush)
      .elsewhen(s2_fire)(s2_valid := false.B)
  }

  predictors.io.s2_fire     := s2_fire_dup
  predictors.io.s2_redirect := s2_redirect_dup

  s3_fire_dup := s3_valid_dup

  for (
    ((((s2_fire, s3_flush), s3_fire), s3_valid), s2_flush) <-
      s2_fire_dup zip s3_flush_dup zip s3_fire_dup zip s3_valid_dup zip s2_flush_dup
  ) {

    when(s3_flush)(s3_valid := false.B)
      .elsewhen(s2_fire)(s3_valid := !s2_flush)
      .elsewhen(s3_fire)(s3_valid := false.B)
  }

  predictors.io.s3_fire     := s3_fire_dup
  predictors.io.s3_redirect := s3_redirect_dup

  io.bpu_to_ftq.resp.valid :=
    s1_valid_dup(2) && s2_components_ready_dup(2) && s2_ready_dup(2) ||
      s2_fire_dup(2) && s2_redirect_dup(2) ||
      s3_fire_dup(2) && s3_redirect_dup(2)
  io.bpu_to_ftq.resp.bits                              := predictors.io.out
  io.bpu_to_ftq.resp.bits.last_stage_spec_info.histPtr := s3_ghist_ptr_dup(2)

  val full_pred_diff        = WireInit(false.B)
  val full_pred_diff_stage  = WireInit(0.U)
  val full_pred_diff_offset = WireInit(0.U)
  for (i <- 0 until numDup - 1) {
    when(io.bpu_to_ftq.resp.valid &&
      ((io.bpu_to_ftq.resp.bits.s1.full_pred(i).asTypeOf(UInt()) =/= io.bpu_to_ftq.resp.bits.s1.full_pred(
        i + 1
      ).asTypeOf(UInt()) && io.bpu_to_ftq.resp.bits.s1.full_pred(i).hit) ||
        (io.bpu_to_ftq.resp.bits.s2.full_pred(i).asTypeOf(UInt()) =/= io.bpu_to_ftq.resp.bits.s2.full_pred(
          i + 1
        ).asTypeOf(UInt()) && io.bpu_to_ftq.resp.bits.s2.full_pred(i).hit) ||
        (io.bpu_to_ftq.resp.bits.s3.full_pred(i).asTypeOf(UInt()) =/= io.bpu_to_ftq.resp.bits.s3.full_pred(
          i + 1
        ).asTypeOf(UInt()) && io.bpu_to_ftq.resp.bits.s3.full_pred(i).hit))) {
      full_pred_diff        := true.B
      full_pred_diff_offset := i.U
      when(io.bpu_to_ftq.resp.bits.s1.full_pred(i).asTypeOf(UInt()) =/= io.bpu_to_ftq.resp.bits.s1.full_pred(
        i + 1
      ).asTypeOf(UInt())) {
        full_pred_diff_stage := 1.U
      }.elsewhen(io.bpu_to_ftq.resp.bits.s2.full_pred(i).asTypeOf(UInt()) =/= io.bpu_to_ftq.resp.bits.s2.full_pred(
        i + 1
      ).asTypeOf(UInt())) {
        full_pred_diff_stage := 2.U
      }.otherwise {
        full_pred_diff_stage := 3.U
      }
    }
  }
  XSError(full_pred_diff, "Full prediction difference detected!")

  // s0_stall should be exclusive with any other PC source
  s0_stall_dup.zip(s1_valid_dup).zip(s2_redirect_dup).zip(s3_redirect_dup).zip(do_redirect_dup).foreach {
    case ((((s0_stall, s1_valid), s2_redirect), s3_redirect), do_redirect) => {
      s0_stall := !(s1_valid || s2_redirect || s3_redirect || do_redirect.valid)
    }
  }
  // Power-on reset
  val powerOnResetState = RegInit(true.B)
  when(s0_fire_dup(0)) {
    // When BPU pipeline first time fire, we consider power-on reset is done
    powerOnResetState := false.B
  }
  XSError(
    !powerOnResetState && s0_stall_dup(0) && s0_pc_dup(0) =/= s0_pc_reg_dup(0),
    "s0_stall but s0_pc is differenct from s0_pc_reg"
  )

  npcGen_dup.zip(s0_pc_reg_dup).map { case (gen, reg) =>
    gen.register(true.B, reg, Some("stallPC"), 0)
  }

  // assign pred cycle for profiling
  io.bpu_to_ftq.resp.bits.s1.full_pred.map(_.predCycle.map(_ := GTimer()))
  io.bpu_to_ftq.resp.bits.s2.full_pred.map(_.predCycle.map(_ := GTimer()))
  io.bpu_to_ftq.resp.bits.s3.full_pred.map(_.predCycle.map(_ := GTimer()))

  for (((npcGen, s1_valid), s1_target) <- npcGen_dup zip s1_valid_dup zip resp.s1.getTarget)
    npcGen.register(s1_valid, s1_target, Some("s1_target"), 4)

  class PreviousPredInfo extends Bundle {
    val hit         = Vec(numDup, Bool())
    val target      = Vec(numDup, UInt(VAddrBits.W))
    val lastBrPosOH = Vec(numDup, Vec(numBr + 1, Bool()))
    val taken       = Vec(numDup, Bool())
    val takenMask   = Vec(numDup, Vec(numBr, Bool()))
    val cfiIndex    = Vec(numDup, UInt(log2Ceil(PredictWidth).W))
  }

  def preds_needs_redirect_vec_dup(x: PreviousPredInfo, y: BranchPredictionBundle) = {
    // Timing optimization
    // We first compare all target with previous stage target,
    // then select the difference by taken & hit
    // Usually target is generated quicker than taken, so do target compare before select can help timing
    val targetDiffVec: IndexedSeq[Vec[Bool]] =
      x.target.zip(y.getAllTargets).map {
        case (xTarget, yAllTarget) => VecInit(yAllTarget.map(_ =/= xTarget))
      } // [numDup][all Target comparison]
    val targetDiff: IndexedSeq[Bool] =
      targetDiffVec.zip(x.hit).zip(x.takenMask).map {
        case ((diff, hit), takenMask) => selectByTaken(takenMask, hit, diff)
      } // [numDup]

    val lastBrPosOHDiff: IndexedSeq[Bool] = x.lastBrPosOH.zip(y.lastBrPosOH).map { case (oh1, oh2) =>
      oh1.asUInt =/= oh2.asUInt
    }
    val takenDiff: IndexedSeq[Bool] = x.taken.zip(y.taken).map { case (t1, t2) => t1 =/= t2 }
    val takenOffsetDiff: IndexedSeq[Bool] = x.cfiIndex.zip(y.cfiIndex).zip(x.taken).zip(y.taken).map {
      case (((i1, i2), xt), yt) => xt && yt && i1 =/= i2.bits
    }
    VecInit(
      for (
        (((tgtd, lbpohd), tkd), tod) <-
          targetDiff zip lastBrPosOHDiff zip takenDiff zip takenOffsetDiff
      )
        yield VecInit(tgtd, lbpohd, tkd, tod)
      // x.shouldShiftVec.asUInt =/= y.shouldShiftVec.asUInt,
      // x.brTaken =/= y.brTaken
    )
  }

  val s1_pred_info = Wire(new PreviousPredInfo)
  s1_pred_info.hit         := resp.s1.full_pred.map(_.hit)
  s1_pred_info.target      := resp.s1.getTarget
  s1_pred_info.lastBrPosOH := resp.s1.lastBrPosOH
  s1_pred_info.taken       := resp.s1.taken
  s1_pred_info.takenMask   := resp.s1.full_pred.map(_.taken_mask_on_slot)
  s1_pred_info.cfiIndex    := resp.s1.cfiIndex.map { case x => x.bits }

  val previous_s1_pred_info = RegEnable(s1_pred_info, 0.U.asTypeOf(new PreviousPredInfo), s1_fire_dup(0))

  val s2_redirect_s1_last_pred_vec_dup = preds_needs_redirect_vec_dup(previous_s1_pred_info, resp.s2)

  for (
    ((s2_redirect, s2_fire), s2_redirect_s1_last_pred_vec) <-
      s2_redirect_dup zip s2_fire_dup zip s2_redirect_s1_last_pred_vec_dup
  )
    s2_redirect := s2_fire && s2_redirect_s1_last_pred_vec.reduce(_ || _)

  for (((npcGen, s2_redirect), s2_target) <- npcGen_dup zip s2_redirect_dup zip resp.s2.getTarget)
    npcGen.register(s2_redirect, s2_target, Some("s2_target"), 5)

  XSPerfAccumulate("s2_redirect_because_target_diff", s2_fire_dup(0) && s2_redirect_s1_last_pred_vec_dup(0)(0))
  XSPerfAccumulate("s2_redirect_because_branch_num_diff", s2_fire_dup(0) && s2_redirect_s1_last_pred_vec_dup(0)(1))
  XSPerfAccumulate("s2_redirect_because_direction_diff", s2_fire_dup(0) && s2_redirect_s1_last_pred_vec_dup(0)(2))
  XSPerfAccumulate("s2_redirect_because_cfi_idx_diff", s2_fire_dup(0) && s2_redirect_s1_last_pred_vec_dup(0)(3))
  // XSPerfAccumulate("s2_redirect_because_shouldShiftVec_diff", s2_fire && s2_redirect_s1_last_pred_vec(4))
  // XSPerfAccumulate("s2_redirect_because_brTaken_diff", s2_fire && s2_redirect_s1_last_pred_vec(5))
  XSPerfAccumulate("s2_redirect_because_fallThroughError", s2_fire_dup(0) && resp.s2.fallThruError(0))

  XSPerfAccumulate("s2_redirect_when_taken", s2_redirect_dup(0) && resp.s2.taken(0) && resp.s2.full_pred(0).hit)
  XSPerfAccumulate("s2_redirect_when_not_taken", s2_redirect_dup(0) && !resp.s2.taken(0) && resp.s2.full_pred(0).hit)
  XSPerfAccumulate("s2_redirect_when_not_hit", s2_redirect_dup(0) && !resp.s2.full_pred(0).hit)

  // To optimize Clock Gating Efficiency of previous_s2_*
  val previous_s2_pred = Wire(new BranchPredictionBundle(isNotS3 = true))
  previous_s2_pred.pc := RegEnable(resp.s2.pc, 0.U.asTypeOf(resp.s2.pc), s2_fire_dup(0)).suggestName(
    s"previous_s2_pred_pc"
  )
  previous_s2_pred.valid := RegEnable(resp.s2.valid, 0.U.asTypeOf(resp.s2.valid), s2_fire_dup(0)).suggestName(
    s"previous_s2_pred_valid"
  )
  previous_s2_pred.hasRedirect := RegEnable(
    resp.s2.hasRedirect,
    0.U.asTypeOf(resp.s2.hasRedirect),
    s2_fire_dup(0)
  ).suggestName(s"previous_s2_pred_hasRedirect")
  previous_s2_pred.ftq_idx := RegEnable(resp.s2.ftq_idx, 0.U.asTypeOf(resp.s2.ftq_idx), s2_fire_dup(0)).suggestName(
    s"previous_s2_pred_ftq_idx"
  )
  previous_s2_pred.full_pred := RegEnable(
    resp.s2.full_pred,
    0.U.asTypeOf(resp.s2.full_pred),
    s2_fire_dup(0)
  ).suggestName(s"previous_s2_pred_full_pred")
  previous_s2_pred.full_pred.zip(resp.s2.full_pred.zipWithIndex).map { case (prev_fp, (new_fp, dupIdx)) =>
    prev_fp.targets.zip(new_fp.taken_mask_on_slot.zipWithIndex).map { case (target, (taken_mask, slotIdx)) =>
      // This enable signal can better improve CGE, but it may lead to timing violations:
      //    s2_fire_dup(0) && !new_fp.taken_mask_on_slot.take(slotIdx).fold(false.B)(_||_) && taken_mask && new_fp.hit
      target := RegEnable(new_fp.targets(slotIdx), 0.U.asTypeOf(new_fp.targets(slotIdx)), s2_fire_dup(0) && taken_mask)
    }
    // This enable signal can better improve CGE, but it may lead to timing violations:
    //    s2_fire_dup(0) && new_fp.hit && !new_fp.taken_mask_on_slot.reduce(_||_)
    prev_fp.fallThroughAddr := RegEnable(
      new_fp.fallThroughAddr,
      0.U.asTypeOf(new_fp.fallThroughAddr),
      s2_fire_dup(0) && resp.s2.full_pred(0).hit && !resp.s2.full_pred(0).taken_mask_on_slot(0)
    )
  }

  val s3_redirect_on_br_taken_dup = resp.s3.full_pred.zip(previous_s2_pred.full_pred).map { case (fp1, fp2) =>
    fp1.real_br_taken_mask().asUInt =/= fp2.real_br_taken_mask().asUInt
  }
  val s3_both_first_taken_dup = resp.s3.full_pred.zip(previous_s2_pred.full_pred).map { case (fp1, fp2) =>
    fp1.real_br_taken_mask()(0) && fp2.real_br_taken_mask()(0)
  }
  val s3_redirect_on_target_dup = resp.s3.getTarget.zip(previous_s2_pred.getTarget).map { case (t1, t2) => t1 =/= t2 }
  val s3_redirect_on_jalr_target_dup = resp.s3.full_pred.zip(previous_s2_pred.full_pred).map { case (fp1, fp2) =>
    fp1.hit_taken_on_jalr && fp1.jalr_target =/= fp2.jalr_target
  }
  val s3_redirect_on_fall_thru_error_dup = resp.s3.fallThruError
  val s3_redirect_on_ftb_multi_hit_dup   = resp.s3.ftbMultiHit

  for (
    (
      (
        ((((s3_redirect, s3_fire), s3_redirect_on_br_taken), s3_redirect_on_target), s3_redirect_on_fall_thru_error),
        s3_redirect_on_ftb_multi_hit
      ),
      s3_both_first_taken
    ) <-
      s3_redirect_dup zip s3_fire_dup zip s3_redirect_on_br_taken_dup zip s3_redirect_on_target_dup zip s3_redirect_on_fall_thru_error_dup zip s3_redirect_on_ftb_multi_hit_dup zip s3_both_first_taken_dup
  ) {

    s3_redirect := s3_fire && (
      (s3_redirect_on_br_taken && !s3_both_first_taken) || s3_redirect_on_target || s3_redirect_on_fall_thru_error || s3_redirect_on_ftb_multi_hit
    )
  }

  XSPerfAccumulate(f"s3_redirect_on_br_taken", s3_fire_dup(0) && s3_redirect_on_br_taken_dup(0))
  XSPerfAccumulate(f"s3_redirect_on_jalr_target", s3_fire_dup(0) && s3_redirect_on_jalr_target_dup(0))
  XSPerfAccumulate(
    f"s3_redirect_on_others",
    s3_redirect_dup(0) && !(s3_redirect_on_br_taken_dup(0) || s3_redirect_on_jalr_target_dup(0))
  )

  for (((npcGen, s3_redirect), s3_target) <- npcGen_dup zip s3_redirect_dup zip resp.s3.getTarget)
    npcGen.register(s3_redirect, s3_target, Some("s3_target"), 3)

  // Send signal tell Ftq override
  val s2_ftq_idx = RegEnable(io.ftq_to_bpu.enq_ptr, s1_fire_dup(0))
  val s3_ftq_idx = RegEnable(s2_ftq_idx, s2_fire_dup(0))

  for (((to_ftq_s1_valid, s1_fire), s1_flush) <- io.bpu_to_ftq.resp.bits.s1.valid zip s1_fire_dup zip s1_flush_dup) {
    to_ftq_s1_valid := s1_fire && !s1_flush
  }
  io.bpu_to_ftq.resp.bits.s1.hasRedirect.map(_ := false.B)
  io.bpu_to_ftq.resp.bits.s1.ftq_idx := DontCare
  for (((to_ftq_s2_valid, s2_fire), s2_flush) <- io.bpu_to_ftq.resp.bits.s2.valid zip s2_fire_dup zip s2_flush_dup) {
    to_ftq_s2_valid := s2_fire && !s2_flush
  }
  io.bpu_to_ftq.resp.bits.s2.hasRedirect.zip(s2_redirect_dup).map { case (hr, r) => hr := r }
  io.bpu_to_ftq.resp.bits.s2.ftq_idx := s2_ftq_idx
  for (((to_ftq_s3_valid, s3_fire), s3_flush) <- io.bpu_to_ftq.resp.bits.s3.valid zip s3_fire_dup zip s3_flush_dup) {
    to_ftq_s3_valid := s3_fire && !s3_flush
  }
  io.bpu_to_ftq.resp.bits.s3.hasRedirect.zip(s3_redirect_dup).map { case (hr, r) => hr := r }
  io.bpu_to_ftq.resp.bits.s3.ftq_idx := s3_ftq_idx

  predictors.io.update            := io.ftq_to_bpu.update
  predictors.io.update.bits.ghist := getHist(io.ftq_to_bpu.update.bits.spec_info.histPtr)
  // Move the update pc registers out of predictors.
  predictors.io.update.bits.pc := SegmentedAddrNext(
    io.ftq_to_bpu.update.bits.pc,
    pcSegments,
    io.ftq_to_bpu.update.valid,
    Some("predictors_io_update_pc")
  ).getAddr()

  val redirect_dup = do_redirect_dup.map(_.bits)
  predictors.io.redirect := do_redirect_dup(0)

  // val updatedGh = oldGh.update(shift, taken && addIntoHist)
  for ((npcGen, do_redirect) <- npcGen_dup zip do_redirect_dup)
    npcGen.register(do_redirect.valid, do_redirect.bits.cfiUpdate.target, Some("redirect_target"), 2)
  // no need to assign s0_last_pred

  // val need_reset = RegNext(reset.asBool) && !reset.asBool

  // Reset
  // npcGen.register(need_reset, resetVector.U, Some("reset_pc"), 1)
  // foldedGhGen.register(need_reset, 0.U.asTypeOf(s0_folded_gh), Some("reset_FGH"), 1)
  // ghistPtrGen.register(need_reset, 0.U.asTypeOf(new CGHPtr), Some("reset_GHPtr"), 1)

  s0_pc_dup.zip(npcGen_dup).map { case (s0_pc, npcGen) => s0_pc := npcGen() }

  // TODO: signals for memVio and other Redirects
  controlRedirectBubble := do_redirect_dup(0).valid && do_redirect_dup(0).bits.ControlRedirectBubble
  ControlBTBMissBubble  := do_redirect_dup(0).bits.ControlBTBMissBubble
  TAGEMissBubble        := do_redirect_dup(0).bits.TAGEMissBubble
  SCMissBubble          := do_redirect_dup(0).bits.SCMissBubble
  ITTAGEMissBubble      := do_redirect_dup(0).bits.ITTAGEMissBubble
  RASMissBubble         := do_redirect_dup(0).bits.RASMissBubble

  memVioRedirectBubble                 := do_redirect_dup(0).valid && do_redirect_dup(0).bits.MemVioRedirectBubble
  otherRedirectBubble                  := do_redirect_dup(0).valid && do_redirect_dup(0).bits.OtherRedirectBubble
  btbMissBubble                        := do_redirect_dup(0).valid && do_redirect_dup(0).bits.BTBMissBubble
  overrideBubble(0)                    := s2_redirect_dup(0)
  overrideBubble(1)                    := s3_redirect_dup(0)
  ftqUpdateBubble(0)                   := !s1_components_ready_dup(0)
  ftqUpdateBubble(1)                   := !s2_components_ready_dup(0)
  ftqUpdateBubble(2)                   := !s3_components_ready_dup(0)
  ftqFullStall                         := !io.bpu_to_ftq.resp.ready
  io.bpu_to_ftq.resp.bits.topdown_info := topdown_stages(numOfStage - 1)

  // topdown handling logic here
  when(controlRedirectBubble) {
    /*
    for (i <- 0 until numOfStage)
      topdown_stages(i).reasons(TopDownCounters.ControlRedirectBubble.id) := true.B
    io.bpu_to_ftq.resp.bits.topdown_info.reasons(TopDownCounters.ControlRedirectBubble.id) := true.B
     */
    when(ControlBTBMissBubble) {
      for (i <- 0 until numOfStage)
        topdown_stages(i).reasons(TopDownCounters.BTBMissBubble.id)                  := true.B
      io.bpu_to_ftq.resp.bits.topdown_info.reasons(TopDownCounters.BTBMissBubble.id) := true.B
    }.elsewhen(TAGEMissBubble) {
      for (i <- 0 until numOfStage)
        topdown_stages(i).reasons(TopDownCounters.TAGEMissBubble.id)                  := true.B
      io.bpu_to_ftq.resp.bits.topdown_info.reasons(TopDownCounters.TAGEMissBubble.id) := true.B
    }.elsewhen(SCMissBubble) {
      for (i <- 0 until numOfStage)
        topdown_stages(i).reasons(TopDownCounters.SCMissBubble.id)                  := true.B
      io.bpu_to_ftq.resp.bits.topdown_info.reasons(TopDownCounters.SCMissBubble.id) := true.B
    }.elsewhen(ITTAGEMissBubble) {
      for (i <- 0 until numOfStage)
        topdown_stages(i).reasons(TopDownCounters.ITTAGEMissBubble.id)                  := true.B
      io.bpu_to_ftq.resp.bits.topdown_info.reasons(TopDownCounters.ITTAGEMissBubble.id) := true.B
    }.elsewhen(RASMissBubble) {
      for (i <- 0 until numOfStage)
        topdown_stages(i).reasons(TopDownCounters.RASMissBubble.id)                  := true.B
      io.bpu_to_ftq.resp.bits.topdown_info.reasons(TopDownCounters.RASMissBubble.id) := true.B
    }
  }
  when(memVioRedirectBubble) {
    for (i <- 0 until numOfStage)
      topdown_stages(i).reasons(TopDownCounters.MemVioRedirectBubble.id)                  := true.B
    io.bpu_to_ftq.resp.bits.topdown_info.reasons(TopDownCounters.MemVioRedirectBubble.id) := true.B
  }
  when(otherRedirectBubble) {
    for (i <- 0 until numOfStage)
      topdown_stages(i).reasons(TopDownCounters.OtherRedirectBubble.id)                  := true.B
    io.bpu_to_ftq.resp.bits.topdown_info.reasons(TopDownCounters.OtherRedirectBubble.id) := true.B
  }
  when(btbMissBubble) {
    for (i <- 0 until numOfStage)
      topdown_stages(i).reasons(TopDownCounters.BTBMissBubble.id)                  := true.B
    io.bpu_to_ftq.resp.bits.topdown_info.reasons(TopDownCounters.BTBMissBubble.id) := true.B
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

  XSDebug(RegNext(reset.asBool) && !reset.asBool, "Reseting...\n")
  XSDebug(io.ftq_to_bpu.update.valid, p"Update from ftq\n")
  XSDebug(io.ftq_to_bpu.redirect.valid, p"Redirect from ftq\n")

  XSDebug("[BP0]                 fire=%d                      pc=%x\n", s0_fire_dup(0), s0_pc_dup(0))
  XSDebug(
    "[BP1] v=%d r=%d cr=%d fire=%d             flush=%d pc=%x\n",
    s1_valid_dup(0),
    s1_ready_dup(0),
    s1_components_ready_dup(0),
    s1_fire_dup(0),
    s1_flush_dup(0),
    s1_pc
  )
  XSDebug(
    "[BP2] v=%d r=%d cr=%d fire=%d redirect=%d flush=%d pc=%x\n",
    s2_valid_dup(0),
    s2_ready_dup(0),
    s2_components_ready_dup(0),
    s2_fire_dup(0),
    s2_redirect_dup(0),
    s2_flush_dup(0),
    s2_pc
  )
  XSDebug(
    "[BP3] v=%d r=%d cr=%d fire=%d redirect=%d flush=%d pc=%x\n",
    s3_valid_dup(0),
    s3_ready_dup(0),
    s3_components_ready_dup(0),
    s3_fire_dup(0),
    s3_redirect_dup(0),
    s3_flush_dup(0),
    s3_pc
  )
  XSDebug("[FTQ] ready=%d\n", io.bpu_to_ftq.resp.ready)
  XSDebug("resp.s1.target=%x\n", resp.s1.getTarget(0))
  XSDebug("resp.s2.target=%x\n", resp.s2.getTarget(0))
  // XSDebug("s0_ghist: %b\n", s0_ghist.predHist)
  // XSDebug("s1_ghist: %b\n", s1_ghist.predHist)
  // XSDebug("s2_ghist: %b\n", s2_ghist.predHist)
  // XSDebug("s2_predicted_ghist: %b\n", s2_predicted_ghist.predHist)

  io.ftq_to_bpu.update.bits.display(io.ftq_to_bpu.update.valid)
  io.ftq_to_bpu.redirect.bits.display(io.ftq_to_bpu.redirect.valid)

  XSPerfAccumulate("s2_redirect", s2_redirect_dup(0))
  XSPerfAccumulate("s3_redirect", s3_redirect_dup(0))
  XSPerfAccumulate("s1_not_valid", !s1_valid_dup(0))

  val perfEvents = predictors.asInstanceOf[Composer].getPerfEvents
  generatePerfEvent()
}

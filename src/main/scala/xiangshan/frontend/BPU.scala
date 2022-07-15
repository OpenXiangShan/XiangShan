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

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.chiselName
import chisel3.util._
import xiangshan._
import utils._

import scala.math.min

trait HasBPUConst extends HasXSParameter {
  val MaxMetaLength = 512 // TODO: Reduce meta length
  val MaxBasicBlockSize = 32
  val LHistoryLength = 32
  // val numBr = 2
  val useBPD = true
  val useLHist = true
  val numBrSlot = numBr-1
  val totalSlot = numBrSlot + 1

  def BP_STAGES = (0 until 3).map(_.U(2.W))
  def BP_S1 = BP_STAGES(0)
  def BP_S2 = BP_STAGES(1)
  def BP_S3 = BP_STAGES(2)
  val numBpStages = BP_STAGES.length

  val debug = true
  // TODO: Replace log2Up by log2Ceil
}

trait HasBPUParameter extends HasXSParameter with HasBPUConst {
  val BPUDebug = true && !env.FPGAPlatform && env.EnablePerfDebug
  val EnableCFICommitLog = true
  val EnbaleCFIPredLog = true
  val EnableBPUTimeRecord = (EnableCFICommitLog || EnbaleCFIPredLog) && !env.FPGAPlatform
  val EnableCommit = false
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
    val res = Wire(UInt(len.W))
    val higher = source << shamt
    val lower = source >> (len.U - shamt)
    res := higher | lower
    res
  }

  def circularShiftRight(source: UInt, len: Int, shamt: UInt): UInt = {
    val res = Wire(UInt(len.W))
    val higher = source << (len.U - shamt)
    val lower = source >> shamt
    res := higher | lower
    res
  }

  // To be verified
  def satUpdate(old: UInt, len: Int, taken: Bool): UInt = {
    val oldSatTaken = old === ((1 << len)-1).U
    val oldSatNotTaken = old === 0.U
    Mux(oldSatTaken && taken, ((1 << len)-1).U,
      Mux(oldSatNotTaken && !taken, 0.U,
        Mux(taken, old + 1.U, old - 1.U)))
  }

  def signedSatUpdate(old: SInt, len: Int, taken: Bool): SInt = {
    val oldSatTaken = old === ((1 << (len-1))-1).S
    val oldSatNotTaken = old === (-(1 << (len-1))).S
    Mux(oldSatTaken && taken, ((1 << (len-1))-1).S,
      Mux(oldSatNotTaken && !taken, (-(1 << (len-1))).S,
        Mux(taken, old + 1.S, old - 1.S)))
  }

  def getFallThroughAddr(start: UInt, carry: Bool, pft: UInt) = {
    val higher = start.head(VAddrBits-log2Ceil(PredictWidth)-instOffsetBits)
    Cat(Mux(carry, higher+1.U, higher), pft, 0.U(instOffsetBits.W))
  }

  def foldTag(tag: UInt, l: Int): UInt = {
    val nChunks = (tag.getWidth + l - 1) / l
    val chunks = (0 until nChunks).map { i =>
      tag(min((i+1)*l, tag.getWidth)-1, i*l)
    }
    ParallelXOR(chunks)
  }
}

// class BranchPredictionUpdate(implicit p: Parameters) extends XSBundle with HasBPUConst {
//   val pc = UInt(VAddrBits.W)
//   val br_offset = Vec(num_br, UInt(log2Up(MaxBasicBlockSize).W))
//   val br_mask = Vec(MaxBasicBlockSize, Bool())
//
//   val jmp_valid = Bool()
//   val jmp_type = UInt(3.W)
//
//   val is_NextMask = Vec(FetchWidth*2, Bool())
//
//   val cfi_idx = Valid(UInt(log2Ceil(MaxBasicBlockSize).W))
//   val cfi_mispredict = Bool()
//   val cfi_is_br = Bool()
//   val cfi_is_jal = Bool()
//   val cfi_is_jalr = Bool()
//
//   val ghist = new ShiftingGlobalHistory()
//
//   val target = UInt(VAddrBits.W)
//
//   val meta = UInt(MaxMetaLength.W)
//   val spec_meta = UInt(MaxMetaLength.W)
//
//   def taken = cfi_idx.valid
// }


class BasePredictorInput (implicit p: Parameters) extends XSBundle with HasBPUConst {
  def nInputs = 1

  val s0_pc = UInt(VAddrBits.W)

  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  val ghist = UInt(HistoryLength.W)

  val resp_in = Vec(nInputs, new BranchPredictionResp)

  // val final_preds = Vec(numBpStages, new)
  // val toFtq_fire = Bool()

  // val s0_all_ready = Bool()
}

class BasePredictorOutput (implicit p: Parameters) extends XSBundle with HasBPUConst {
  val last_stage_meta = UInt(MaxMetaLength.W) // This is use by composer
  val resp = new BranchPredictionResp

  // These store in meta, extract in composer
  // val rasSp = UInt(log2Ceil(RasSize).W)
  // val rasTop = new RASEntry
  // val specCnt = Vec(PredictWidth, UInt(10.W))
}

class BasePredictorIO (implicit p: Parameters) extends XSBundle with HasBPUConst {
  val reset_vector = Input(UInt(PAddrBits.W))
  val in  = Flipped(DecoupledIO(new BasePredictorInput)) // TODO: Remove DecoupledIO
  // val out = DecoupledIO(new BasePredictorOutput)
  val out = Output(new BasePredictorOutput)
  // val flush_out = Valid(UInt(VAddrBits.W))

  val ctrl = Input(new BPUCtrl)

  val s0_fire = Input(Bool())
  val s1_fire = Input(Bool())
  val s2_fire = Input(Bool())
  val s3_fire = Input(Bool())

  val s2_redirect = Input(Bool())
  val s3_redirect = Input(Bool())

  val s1_ready = Output(Bool())
  val s2_ready = Output(Bool())
  val s3_ready = Output(Bool())

  val update = Flipped(Valid(new BranchPredictionUpdate))
  val redirect = Flipped(Valid(new BranchPredictionRedirect))
}

abstract class BasePredictor(implicit p: Parameters) extends XSModule 
  with HasBPUConst with BPUUtils with HasPerfEvents {
  val meta_size = 0
  val spec_meta_size = 0
  val io = IO(new BasePredictorIO())

  io.out.resp := io.in.bits.resp_in(0)

  io.out.last_stage_meta := 0.U

  io.in.ready := !io.redirect.valid

  io.s1_ready := true.B
  io.s2_ready := true.B
  io.s3_ready := true.B

  val reset_vector = DelayN(io.reset_vector, 5)
  val s0_pc       = WireInit(io.in.bits.s0_pc) // fetchIdx(io.f0_pc)
  val s1_pc       = RegEnable(s0_pc, io.s0_fire)
  val s2_pc       = RegEnable(s1_pc, io.s1_fire)
  val s3_pc       = RegEnable(s2_pc, io.s2_fire)

  when (RegNext(RegNext(reset.asBool) && !reset.asBool)) {
    s1_pc := reset_vector
  }

  io.out.resp.s1.pc := s1_pc
  io.out.resp.s2.pc := s2_pc
  io.out.resp.s3.pc := s3_pc
  
  val perfEvents: Seq[(String, UInt)] = Seq()


  def getFoldedHistoryInfo: Option[Set[FoldedHistoryInfo]] = None
}

class FakePredictor(implicit p: Parameters) extends BasePredictor {
  io.in.ready                 := true.B
  io.out.last_stage_meta      := 0.U
  io.out.resp := io.in.bits.resp_in(0)
}

class BpuToFtqIO(implicit p: Parameters) extends XSBundle {
  val resp = DecoupledIO(new BpuToFtqBundle())
}

class PredictorIO(implicit p: Parameters) extends XSBundle {
  val bpu_to_ftq = new BpuToFtqIO()
  val ftq_to_bpu = Flipped(new FtqToBpuIO())
  val ctrl = Input(new BPUCtrl)
  val reset_vector = Input(UInt(PAddrBits.W))
}

@chiselName
class Predictor(implicit p: Parameters) extends XSModule with HasBPUConst with HasPerfEvents with HasCircularQueuePtrHelper {
  val io = IO(new PredictorIO)

  val ctrl = DelayN(io.ctrl, 1)
  val predictors = Module(if (useBPD) new Composer else new FakePredictor)

  // ctrl signal
  predictors.io.ctrl := ctrl
  predictors.io.reset_vector := io.reset_vector

  val s0_fire, s1_fire, s2_fire, s3_fire = Wire(Bool())
  val s1_valid, s2_valid, s3_valid = RegInit(false.B)
  val s1_ready, s2_ready, s3_ready = Wire(Bool())
  val s1_components_ready, s2_components_ready, s3_components_ready = Wire(Bool())

  val reset_vector = DelayN(io.reset_vector, 5)
  val s0_pc = Wire(UInt(VAddrBits.W))
  val s0_pc_reg = RegNext(s0_pc)
  when (RegNext(RegNext(reset.asBool) && !reset.asBool)) {
    s0_pc_reg := reset_vector
  }
  val s1_pc = RegEnable(s0_pc, s0_fire)
  val s2_pc = RegEnable(s1_pc, s1_fire)
  val s3_pc = RegEnable(s2_pc, s2_fire)

  val s0_folded_gh = Wire(new AllFoldedHistories(foldedGHistInfos))
  val s0_folded_gh_reg = RegNext(s0_folded_gh, 0.U.asTypeOf(s0_folded_gh))
  val s1_folded_gh = RegEnable(s0_folded_gh, 0.U.asTypeOf(s0_folded_gh), s0_fire)
  val s2_folded_gh = RegEnable(s1_folded_gh, 0.U.asTypeOf(s0_folded_gh), s1_fire)
  val s3_folded_gh = RegEnable(s2_folded_gh, 0.U.asTypeOf(s0_folded_gh), s2_fire)

  val s0_last_br_num_oh = Wire(UInt((numBr+1).W))
  val s0_last_br_num_oh_reg = RegNext(s0_last_br_num_oh, 0.U)
  val s1_last_br_num_oh = RegEnable(s0_last_br_num_oh, 0.U, s0_fire)
  val s2_last_br_num_oh = RegEnable(s1_last_br_num_oh, 0.U, s1_fire)
  val s3_last_br_num_oh = RegEnable(s2_last_br_num_oh, 0.U, s2_fire)

  val s0_ahead_fh_oldest_bits = Wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  val s0_ahead_fh_oldest_bits_reg = RegNext(s0_ahead_fh_oldest_bits, 0.U.asTypeOf(s0_ahead_fh_oldest_bits))
  val s1_ahead_fh_oldest_bits = RegEnable(s0_ahead_fh_oldest_bits, 0.U.asTypeOf(s0_ahead_fh_oldest_bits), s0_fire)
  val s2_ahead_fh_oldest_bits = RegEnable(s1_ahead_fh_oldest_bits, 0.U.asTypeOf(s0_ahead_fh_oldest_bits), s1_fire)
  val s3_ahead_fh_oldest_bits = RegEnable(s2_ahead_fh_oldest_bits, 0.U.asTypeOf(s0_ahead_fh_oldest_bits), s2_fire)

  val npcGen   = new PhyPriorityMuxGenerator[UInt]
  val foldedGhGen = new PhyPriorityMuxGenerator[AllFoldedHistories]
  val ghistPtrGen = new PhyPriorityMuxGenerator[CGHPtr]
  val lastBrNumOHGen = new PhyPriorityMuxGenerator[UInt]
  val aheadFhObGen = new PhyPriorityMuxGenerator[AllAheadFoldedHistoryOldestBits]

  val ghvBitWriteGens = Seq.tabulate(HistoryLength)(n => new PhyPriorityMuxGenerator[Bool])
  // val ghistGen = new PhyPriorityMuxGenerator[UInt]

  val ghv = RegInit(0.U.asTypeOf(Vec(HistoryLength, Bool())))
  val ghv_wire = WireInit(ghv)

  val s0_ghist = WireInit(0.U.asTypeOf(UInt(HistoryLength.W)))


  println(f"history buffer length ${HistoryLength}")
  val ghv_write_datas = Wire(Vec(HistoryLength, Bool()))
  val ghv_wens = Wire(Vec(HistoryLength, Bool()))
  
  val s0_ghist_ptr = Wire(new CGHPtr)
  val s0_ghist_ptr_reg = RegNext(s0_ghist_ptr, 0.U.asTypeOf(new CGHPtr))
  val s1_ghist_ptr = RegEnable(s0_ghist_ptr, 0.U.asTypeOf(new CGHPtr), s0_fire)
  val s2_ghist_ptr = RegEnable(s1_ghist_ptr, 0.U.asTypeOf(new CGHPtr), s1_fire)
  val s3_ghist_ptr = RegEnable(s2_ghist_ptr, 0.U.asTypeOf(new CGHPtr), s2_fire)
  
  def getHist(ptr: CGHPtr): UInt = (Cat(ghv_wire.asUInt, ghv_wire.asUInt) >> (ptr.value+1.U))(HistoryLength-1, 0)
  s0_ghist := getHist(s0_ghist_ptr)

  val resp = predictors.io.out.resp
  
  
  val toFtq_fire = io.bpu_to_ftq.resp.valid && io.bpu_to_ftq.resp.ready

  val s1_flush, s2_flush, s3_flush = Wire(Bool())
  val s2_redirect, s3_redirect = Wire(Bool())

  // predictors.io := DontCare
  predictors.io.in.valid := s0_fire
  predictors.io.in.bits.s0_pc := s0_pc
  predictors.io.in.bits.ghist := s0_ghist
  predictors.io.in.bits.folded_hist := s0_folded_gh
  predictors.io.in.bits.resp_in(0) := (0.U).asTypeOf(new BranchPredictionResp)
  // predictors.io.in.bits.resp_in(0).s1.pc := s0_pc
  // predictors.io.in.bits.toFtq_fire := toFtq_fire

  // predictors.io.out.ready := io.bpu_to_ftq.resp.ready

  val redirect_req = io.ftq_to_bpu.redirect
  val do_redirect = RegNext(redirect_req, 0.U.asTypeOf(io.ftq_to_bpu.redirect))

  // Pipeline logic
  s2_redirect := false.B
  s3_redirect := false.B

  s3_flush := redirect_req.valid // flush when redirect comes
  s2_flush := s3_flush || s3_redirect
  s1_flush := s2_flush || s2_redirect

  s1_components_ready := predictors.io.s1_ready
  s1_ready := s1_fire || !s1_valid
  s0_fire := !reset.asBool && s1_components_ready && s1_ready
  predictors.io.s0_fire := s0_fire

  s2_components_ready := predictors.io.s2_ready
  s2_ready := s2_fire || !s2_valid
  s1_fire := s1_valid && s2_components_ready && s2_ready && io.bpu_to_ftq.resp.ready

  s3_components_ready := predictors.io.s3_ready
  s3_ready := s3_fire || !s3_valid
  s2_fire := s2_valid && s3_components_ready && s3_ready

  when (redirect_req.valid) { s1_valid := false.B }
    .elsewhen(s0_fire)      { s1_valid := true.B  }
    .elsewhen(s1_flush)     { s1_valid := false.B }
    .elsewhen(s1_fire)      { s1_valid := false.B }

  predictors.io.s1_fire := s1_fire

  s2_fire := s2_valid

  when(s2_flush)       { s2_valid := false.B }
    .elsewhen(s1_fire) { s2_valid := !s1_flush }
    .elsewhen(s2_fire) { s2_valid := false.B }

  predictors.io.s2_fire := s2_fire
  predictors.io.s2_redirect := s2_redirect

  s3_fire := s3_valid

  when(s3_flush)       { s3_valid := false.B }
    .elsewhen(s2_fire) { s3_valid := !s2_flush }
    .elsewhen(s3_fire) { s3_valid := false.B }

  predictors.io.s3_fire := s3_fire
  predictors.io.s3_redirect := s3_redirect


  io.bpu_to_ftq.resp.valid :=
    s1_valid && s2_components_ready && s2_ready ||
    s2_fire && s2_redirect ||
    s3_fire && s3_redirect
  io.bpu_to_ftq.resp.bits  := BpuToFtqBundle(predictors.io.out.resp)
  io.bpu_to_ftq.resp.bits.meta  := predictors.io.out.last_stage_meta // TODO: change to lastStageMeta
  io.bpu_to_ftq.resp.bits.s3.folded_hist := s3_folded_gh
  io.bpu_to_ftq.resp.bits.s3.histPtr := s3_ghist_ptr
  io.bpu_to_ftq.resp.bits.s3.lastBrNumOH := s3_last_br_num_oh
  io.bpu_to_ftq.resp.bits.s3.afhob := s3_ahead_fh_oldest_bits

  npcGen.register(true.B, s0_pc_reg, Some("stallPC"), 0)
  foldedGhGen.register(true.B, s0_folded_gh_reg, Some("stallFGH"), 0)
  ghistPtrGen.register(true.B, s0_ghist_ptr_reg, Some("stallGHPtr"), 0)
  lastBrNumOHGen.register(true.B, s0_last_br_num_oh_reg, Some("stallBrNumOH"), 0)
  aheadFhObGen.register(true.B, s0_ahead_fh_oldest_bits_reg, Some("stallAFHOB"), 0)

  // History manage
  // s1
  val s1_possible_predicted_ghist_ptrs = (0 to numBr).map(s1_ghist_ptr - _.U)
  val s1_predicted_ghist_ptr = Mux1H(resp.s1.lastBrPosOH, s1_possible_predicted_ghist_ptrs)

  val s1_possible_predicted_fhs = (0 to numBr).map(i =>
    s1_folded_gh.update(s1_ahead_fh_oldest_bits, s1_last_br_num_oh, i, resp.s1.brTaken && resp.s1.lastBrPosOH(i)))
  val s1_predicted_fh = Mux1H(resp.s1.lastBrPosOH, s1_possible_predicted_fhs)

  val s1_ahead_fh_ob_src = Wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  s1_ahead_fh_ob_src.read(ghv, s1_ghist_ptr)

  if (EnableGHistDiff) {
    val s1_predicted_ghist = WireInit(getHist(s1_predicted_ghist_ptr).asTypeOf(Vec(HistoryLength, Bool())))
    for (i <- 0 until numBr) {
      when (resp.s1.shouldShiftVec(i)) {
        s1_predicted_ghist(i) := resp.s1.brTaken && (i==0).B
      }
    }
    when (s1_valid) {
      s0_ghist := s1_predicted_ghist.asUInt
    }
  }

  val s1_ghv_wens = (0 until HistoryLength).map(n =>
    (0 until numBr).map(b => (s1_ghist_ptr).value === (CGHPtr(false.B, n.U) + b.U).value && resp.s1.shouldShiftVec(b) && s1_valid))
  val s1_ghv_wdatas = (0 until HistoryLength).map(n =>
    Mux1H(
      (0 until numBr).map(b => (
        (s1_ghist_ptr).value === (CGHPtr(false.B, n.U) + b.U).value && resp.s1.shouldShiftVec(b),
        resp.s1.brTaken && resp.s1.lastBrPosOH(b+1)
      ))
    )
  )

  XSError(!resp.s1.is_minimal, "s1 should be minimal!\n")

  npcGen.register(s1_valid, resp.s1.getTarget, Some("s1_target"), 4)
  foldedGhGen.register(s1_valid, s1_predicted_fh, Some("s1_FGH"), 4)
  ghistPtrGen.register(s1_valid, s1_predicted_ghist_ptr, Some("s1_GHPtr"), 4)
  lastBrNumOHGen.register(s1_valid, resp.s1.lastBrPosOH.asUInt, Some("s1_BrNumOH"), 4)
  aheadFhObGen.register(s1_valid, s1_ahead_fh_ob_src, Some("s1_AFHOB"), 4)
  ghvBitWriteGens.zip(s1_ghv_wens).zipWithIndex.map{case ((b, w), i) =>
    b.register(w.reduce(_||_), s1_ghv_wdatas(i), Some(s"s1_new_bit_$i"), 4)
  }

  def preds_needs_redirect_vec(x: BranchPredictionBundle, y: BranchPredictionBundle) = {
    VecInit(
      x.getTarget =/= y.getTarget,
      x.lastBrPosOH.asUInt =/= y.lastBrPosOH.asUInt,
      x.taken =/= y.taken,
      (x.taken && y.taken) && x.cfiIndex.bits =/= y.cfiIndex.bits,
      // x.shouldShiftVec.asUInt =/= y.shouldShiftVec.asUInt,
      // x.brTaken =/= y.brTaken
    )
  }

  // s2
  val s2_possible_predicted_ghist_ptrs = (0 to numBr).map(s2_ghist_ptr - _.U)
  val s2_predicted_ghist_ptr = Mux1H(resp.s2.lastBrPosOH, s2_possible_predicted_ghist_ptrs)

  val s2_possible_predicted_fhs = (0 to numBr).map(i =>
    s2_folded_gh.update(s2_ahead_fh_oldest_bits, s2_last_br_num_oh, i, if (i > 0) resp.s2.full_pred.br_taken_mask(i-1) else false.B))
  val s2_predicted_fh = Mux1H(resp.s2.lastBrPosOH, s2_possible_predicted_fhs)

  val s2_ahead_fh_ob_src = Wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  s2_ahead_fh_ob_src.read(ghv, s2_ghist_ptr)

  if (EnableGHistDiff) {
    val s2_predicted_ghist = WireInit(getHist(s2_predicted_ghist_ptr).asTypeOf(Vec(HistoryLength, Bool())))
    for (i <- 0 until numBr) {
      when (resp.s2.shouldShiftVec(i)) {
        s2_predicted_ghist(i) := resp.s2.brTaken && (i==0).B
      }
    }
    when(s2_redirect) {
      s0_ghist := s2_predicted_ghist.asUInt
    }
  }

  val s2_ghv_wens = (0 until HistoryLength).map(n =>
    (0 until numBr).map(b => (s2_ghist_ptr).value === (CGHPtr(false.B, n.U) + b.U).value && resp.s2.shouldShiftVec(b) && s2_redirect))
  val s2_ghv_wdatas = (0 until HistoryLength).map(n =>
    Mux1H(
      (0 until numBr).map(b => (
        (s2_ghist_ptr).value === (CGHPtr(false.B, n.U) + b.U).value && resp.s2.shouldShiftVec(b),
        resp.s2.full_pred.real_br_taken_mask()(b)
      ))
    )
  )

  val previous_s1_pred = RegEnable(resp.s1, 0.U.asTypeOf(resp.s1), s1_fire)

  val s2_redirect_s1_last_pred_vec = preds_needs_redirect_vec(previous_s1_pred, resp.s2)

  s2_redirect := s2_fire && s2_redirect_s1_last_pred_vec.reduce(_||_)

  XSError(resp.s2.is_minimal, "s2 should not be minimal!\n")

  npcGen.register(s2_redirect, resp.s2.getTarget, Some("s2_target"), 5)
  foldedGhGen.register(s2_redirect, s2_predicted_fh, Some("s2_FGH"), 5)
  ghistPtrGen.register(s2_redirect, s2_predicted_ghist_ptr, Some("s2_GHPtr"), 5)
  lastBrNumOHGen.register(s2_redirect, resp.s2.lastBrPosOH.asUInt, Some("s2_BrNumOH"), 5)
  aheadFhObGen.register(s2_redirect, s2_ahead_fh_ob_src, Some("s2_AFHOB"), 5)
  ghvBitWriteGens.zip(s2_ghv_wens).zipWithIndex.map{case ((b, w), i) =>
    b.register(w.reduce(_||_), s2_ghv_wdatas(i), Some(s"s2_new_bit_$i"), 5)
  }

  XSPerfAccumulate("s2_redirect_because_target_diff", s2_fire && s2_redirect_s1_last_pred_vec(0))
  XSPerfAccumulate("s2_redirect_because_branch_num_diff", s2_fire && s2_redirect_s1_last_pred_vec(1))
  XSPerfAccumulate("s2_redirect_because_direction_diff", s2_fire && s2_redirect_s1_last_pred_vec(2))
  XSPerfAccumulate("s2_redirect_because_cfi_idx_diff", s2_fire && s2_redirect_s1_last_pred_vec(3))
  // XSPerfAccumulate("s2_redirect_because_shouldShiftVec_diff", s2_fire && s2_redirect_s1_last_pred_vec(4))
  // XSPerfAccumulate("s2_redirect_because_brTaken_diff", s2_fire && s2_redirect_s1_last_pred_vec(5))
  XSPerfAccumulate("s2_redirect_because_fallThroughError", s2_fire && resp.s2.fallThruError)

  XSPerfAccumulate("s2_redirect_when_taken", s2_redirect && resp.s2.taken && resp.s2.full_pred.hit)
  XSPerfAccumulate("s2_redirect_when_not_taken", s2_redirect && !resp.s2.taken && resp.s2.full_pred.hit)
  XSPerfAccumulate("s2_redirect_when_not_hit", s2_redirect && !resp.s2.full_pred.hit)


  // s3
  val s3_possible_predicted_ghist_ptrs = (0 to numBr).map(s3_ghist_ptr - _.U)
  val s3_predicted_ghist_ptr = Mux1H(resp.s3.lastBrPosOH, s3_possible_predicted_ghist_ptrs)

  val s3_possible_predicted_fhs = (0 to numBr).map(i =>
    s3_folded_gh.update(s3_ahead_fh_oldest_bits, s3_last_br_num_oh, i, if (i > 0) resp.s3.full_pred.br_taken_mask(i-1) else false.B))
  val s3_predicted_fh = Mux1H(resp.s3.lastBrPosOH, s3_possible_predicted_fhs)

  val s3_ahead_fh_ob_src = Wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  s3_ahead_fh_ob_src.read(ghv, s3_ghist_ptr)

  if (EnableGHistDiff) {
    val s3_predicted_ghist = WireInit(getHist(s3_predicted_ghist_ptr).asTypeOf(Vec(HistoryLength, Bool())))
    for (i <- 0 until numBr) {
      when (resp.s3.shouldShiftVec(i)) {
        s3_predicted_ghist(i) := resp.s3.brTaken && (i==0).B
      }
    }
    when(s3_redirect) {
      s0_ghist := s3_predicted_ghist.asUInt
    }
  }

  val s3_ghv_wens = (0 until HistoryLength).map(n =>
    (0 until numBr).map(b => (s3_ghist_ptr).value === (CGHPtr(false.B, n.U) + b.U).value && resp.s3.shouldShiftVec(b) && s3_redirect))
  val s3_ghv_wdatas = (0 until HistoryLength).map(n =>
    Mux1H(
      (0 until numBr).map(b => (
        (s3_ghist_ptr).value === (CGHPtr(false.B, n.U) + b.U).value && resp.s3.shouldShiftVec(b),
        resp.s3.full_pred.real_br_taken_mask()(b)
      ))
    )
  )

  val previous_s2_pred = RegEnable(resp.s2, 0.U.asTypeOf(resp.s2), s2_fire)

  val s3_redirect_on_br_taken = resp.s3.full_pred.real_br_taken_mask().asUInt =/= previous_s2_pred.full_pred.real_br_taken_mask().asUInt
  val s3_redirect_on_target = resp.s3.getTarget =/= previous_s2_pred.getTarget
  val s3_redirect_on_jalr_target = resp.s3.full_pred.hit_taken_on_jalr && resp.s3.full_pred.jalr_target =/= previous_s2_pred.full_pred.jalr_target
  val s3_redirect_on_fall_thru_error = resp.s3.fallThruError

  s3_redirect := s3_fire && (
    s3_redirect_on_br_taken || s3_redirect_on_target || s3_redirect_on_fall_thru_error
  )

  XSPerfAccumulate(f"s3_redirect_on_br_taken", s3_fire && s3_redirect_on_br_taken)
  XSPerfAccumulate(f"s3_redirect_on_jalr_target", s3_fire && s3_redirect_on_jalr_target)
  XSPerfAccumulate(f"s3_redirect_on_others", s3_redirect && !(s3_redirect_on_br_taken || s3_redirect_on_jalr_target))

  npcGen.register(s3_redirect, resp.s3.getTarget, Some("s3_target"), 3)
  foldedGhGen.register(s3_redirect, s3_predicted_fh, Some("s3_FGH"), 3)
  ghistPtrGen.register(s3_redirect, s3_predicted_ghist_ptr, Some("s3_GHPtr"), 3)
  lastBrNumOHGen.register(s3_redirect, resp.s3.lastBrPosOH.asUInt, Some("s3_BrNumOH"), 3)
  aheadFhObGen.register(s3_redirect, s3_ahead_fh_ob_src, Some("s3_AFHOB"), 3)
  ghvBitWriteGens.zip(s3_ghv_wens).zipWithIndex.map{case ((b, w), i) =>
    b.register(w.reduce(_||_), s3_ghv_wdatas(i), Some(s"s3_new_bit_$i"), 3)
  }

  // Send signal tell Ftq override
  val s2_ftq_idx = RegEnable(io.ftq_to_bpu.enq_ptr, s1_fire)
  val s3_ftq_idx = RegEnable(s2_ftq_idx, s2_fire)

  io.bpu_to_ftq.resp.bits.s1.valid := s1_fire && !s1_flush
  io.bpu_to_ftq.resp.bits.s1.hasRedirect := false.B
  io.bpu_to_ftq.resp.bits.s1.ftq_idx := DontCare
  io.bpu_to_ftq.resp.bits.s2.valid := s2_fire && !s2_flush
  io.bpu_to_ftq.resp.bits.s2.hasRedirect := s2_redirect
  io.bpu_to_ftq.resp.bits.s2.ftq_idx := s2_ftq_idx
  io.bpu_to_ftq.resp.bits.s3.valid := s3_fire && !s3_flush
  io.bpu_to_ftq.resp.bits.s3.hasRedirect := s3_redirect
  io.bpu_to_ftq.resp.bits.s3.ftq_idx := s3_ftq_idx

  val redirect = do_redirect.bits

  predictors.io.update := RegNext(io.ftq_to_bpu.update)
  predictors.io.update.bits.ghist := RegNext(getHist(io.ftq_to_bpu.update.bits.histPtr))
  predictors.io.redirect := do_redirect

  // Redirect logic
  val shift = redirect.cfiUpdate.shift
  val addIntoHist = redirect.cfiUpdate.addIntoHist
  // TODO: remove these below
  val shouldShiftVec = Mux(shift === 0.U, VecInit(0.U((1 << (log2Ceil(numBr) + 1)).W).asBools), VecInit((LowerMask(1.U << (shift-1.U))).asBools()))
  // TODO end
  val afhob = redirect.cfiUpdate.afhob
  val lastBrNumOH = redirect.cfiUpdate.lastBrNumOH


  val isBr = redirect.cfiUpdate.pd.isBr
  val taken = redirect.cfiUpdate.taken
  val real_br_taken_mask = (0 until numBr).map(i => shift === (i+1).U && taken && addIntoHist )

  val oldPtr = redirect.cfiUpdate.histPtr
  val oldFh = redirect.cfiUpdate.folded_hist
  val updated_ptr = oldPtr - shift
  val updated_fh = VecInit((0 to numBr).map(i => oldFh.update(afhob, lastBrNumOH, i, taken && addIntoHist)))(shift)
  val thisBrNumOH = UIntToOH(shift, numBr+1)
  val thisAheadFhOb = Wire(new AllAheadFoldedHistoryOldestBits(foldedGHistInfos))
  thisAheadFhOb.read(ghv, oldPtr)
  val redirect_ghv_wens = (0 until HistoryLength).map(n =>
    (0 until numBr).map(b => oldPtr.value === (CGHPtr(false.B, n.U) + b.U).value && shouldShiftVec(b) && do_redirect.valid))
  val redirect_ghv_wdatas = (0 until HistoryLength).map(n =>
    Mux1H(
      (0 until numBr).map(b => oldPtr.value === (CGHPtr(false.B, n.U) + b.U).value && shouldShiftVec(b)),
      real_br_taken_mask
    )
  )

  if (EnableGHistDiff) {
    val updated_ghist = WireInit(getHist(updated_ptr).asTypeOf(Vec(HistoryLength, Bool())))
    for (i <- 0 until numBr) {
      when (shift >= (i+1).U) {
        updated_ghist(i) := taken && addIntoHist && (i==0).B
      }
    }
    when(do_redirect.valid) {
      s0_ghist := updated_ghist.asUInt
    }
  }


  // val updatedGh = oldGh.update(shift, taken && addIntoHist)

  npcGen.register(do_redirect.valid, do_redirect.bits.cfiUpdate.target, Some("redirect_target"), 2)
  foldedGhGen.register(do_redirect.valid, updated_fh, Some("redirect_FGHT"), 2)
  ghistPtrGen.register(do_redirect.valid, updated_ptr, Some("redirect_GHPtr"), 2)
  lastBrNumOHGen.register(do_redirect.valid, thisBrNumOH, Some("redirect_BrNumOH"), 2)
  aheadFhObGen.register(do_redirect.valid, thisAheadFhOb, Some("redirect_AFHOB"), 2)
  ghvBitWriteGens.zip(redirect_ghv_wens).zipWithIndex.map{case ((b, w), i) =>
    b.register(w.reduce(_||_), redirect_ghv_wdatas(i), Some(s"redirect_new_bit_$i"), 2)
  }
  // no need to assign s0_last_pred

  // val need_reset = RegNext(reset.asBool) && !reset.asBool

  // Reset
  // npcGen.register(need_reset, resetVector.U, Some("reset_pc"), 1)
  // foldedGhGen.register(need_reset, 0.U.asTypeOf(s0_folded_gh), Some("reset_FGH"), 1)
  // ghistPtrGen.register(need_reset, 0.U.asTypeOf(new CGHPtr), Some("reset_GHPtr"), 1)

  s0_pc         := npcGen()
  when (!(RegNext(RegNext(reset.asBool) && !reset.asBool) )) {
    s0_pc_reg     := s0_pc
  }
  s0_folded_gh  := foldedGhGen()
  s0_ghist_ptr  := ghistPtrGen()
  s0_ahead_fh_oldest_bits := aheadFhObGen()
  s0_last_br_num_oh := lastBrNumOHGen()
  (ghv_write_datas zip ghvBitWriteGens).map{case (wd, d) => wd := d()}
  for (i <- 0 until HistoryLength) {
    ghv_wens(i) := Seq(s1_ghv_wens, s2_ghv_wens, s3_ghv_wens, redirect_ghv_wens).map(_(i).reduce(_||_)).reduce(_||_)
    when (ghv_wens(i)) {
      ghv(i) := ghv_write_datas(i)
    }
  }

  XSError(isBefore(redirect.cfiUpdate.histPtr, s3_ghist_ptr) && do_redirect.valid, p"s3_ghist_ptr ${s3_ghist_ptr} exceeds redirect histPtr ${redirect.cfiUpdate.histPtr}\n")
  XSError(isBefore(redirect.cfiUpdate.histPtr, s2_ghist_ptr) && do_redirect.valid, p"s2_ghist_ptr ${s2_ghist_ptr} exceeds redirect histPtr ${redirect.cfiUpdate.histPtr}\n")
  XSError(isBefore(redirect.cfiUpdate.histPtr, s1_ghist_ptr) && do_redirect.valid, p"s1_ghist_ptr ${s1_ghist_ptr} exceeds redirect histPtr ${redirect.cfiUpdate.histPtr}\n")

  XSDebug(RegNext(reset.asBool) && !reset.asBool, "Reseting...\n")
  XSDebug(io.ftq_to_bpu.update.valid, p"Update from ftq\n")
  XSDebug(io.ftq_to_bpu.redirect.valid, p"Redirect from ftq\n")

  XSDebug("[BP0]                 fire=%d                      pc=%x\n", s0_fire, s0_pc)
  XSDebug("[BP1] v=%d r=%d cr=%d fire=%d             flush=%d pc=%x\n",
    s1_valid, s1_ready, s1_components_ready, s1_fire, s1_flush, s1_pc)
  XSDebug("[BP2] v=%d r=%d cr=%d fire=%d redirect=%d flush=%d pc=%x\n",
  s2_valid, s2_ready, s2_components_ready, s2_fire, s2_redirect, s2_flush, s2_pc)
  XSDebug("[BP3] v=%d r=%d cr=%d fire=%d redirect=%d flush=%d pc=%x\n",
  s3_valid, s3_ready, s3_components_ready, s3_fire, s3_redirect, s3_flush, s3_pc)
  XSDebug("[FTQ] ready=%d\n", io.bpu_to_ftq.resp.ready)
  XSDebug("resp.s1.target=%x\n", resp.s1.getTarget)
  XSDebug("resp.s2.target=%x\n", resp.s2.getTarget)
  // XSDebug("s0_ghist: %b\n", s0_ghist.predHist)
  // XSDebug("s1_ghist: %b\n", s1_ghist.predHist)
  // XSDebug("s2_ghist: %b\n", s2_ghist.predHist)
  // XSDebug("s2_predicted_ghist: %b\n", s2_predicted_ghist.predHist)
  XSDebug(p"s0_ghist_ptr: $s0_ghist_ptr\n")
  XSDebug(p"s1_ghist_ptr: $s1_ghist_ptr\n")
  XSDebug(p"s2_ghist_ptr: $s2_ghist_ptr\n")
  XSDebug(p"s3_ghist_ptr: $s3_ghist_ptr\n")

  io.ftq_to_bpu.update.bits.display(io.ftq_to_bpu.update.valid)
  io.ftq_to_bpu.redirect.bits.display(io.ftq_to_bpu.redirect.valid)


  XSPerfAccumulate("s2_redirect", s2_redirect)
  XSPerfAccumulate("s3_redirect", s3_redirect)
  XSPerfAccumulate("s1_not_valid", !s1_valid)

  val perfEvents = predictors.asInstanceOf[Composer].getPerfEvents
  generatePerfEvent()
}

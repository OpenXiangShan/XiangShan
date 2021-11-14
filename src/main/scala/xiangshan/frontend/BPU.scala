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
  val MaxMetaLength = 1024 // TODO: Reduce meta length
  val MaxBasicBlockSize = 32
  val LHistoryLength = 32
  // val numBr = 2
  val useBPD = true
  val useLHist = true
  val shareTailSlot = true
  val numBrSlot = if (shareTailSlot) numBr-1 else numBr
  val totalSlot = numBrSlot + 1

  def BP_STAGES = (0 until 3).map(_.U(2.W))
  def BP_S1 = BP_STAGES(0)
  def BP_S2 = BP_STAGES(1)
  def BP_S3 = BP_STAGES(2)
  val numBpStages = BP_STAGES.length

  val debug = true
  val resetVector = 0x10000000L//TODO: set reset vec
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
    val higher = start.head(VAddrBits-log2Ceil(PredictWidth)-instOffsetBits-1)
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

class AllFoldedHistories(val gen: Seq[Tuple2[Int, Int]])(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val hist = MixedVec(gen.map{case (l, cl) => new FoldedHistory(l, cl, numBr)})
  // println(gen.mkString)
  require(gen.toSet.toList.equals(gen))
  def getHistWithInfo(info: Tuple2[Int, Int]) = {
    val selected = hist.filter(_.info.equals(info))
    require(selected.length == 1)
    selected(0)
  }
  def autoConnectFrom(that: AllFoldedHistories) = {
    require(this.hist.length <= that.hist.length)
    for (h <- this.hist) {
      h := that.getHistWithInfo(h.info)
    }
  }
  def update(ghr: Vec[Bool], ptr: CGHPtr, shift: UInt, taken: Bool): AllFoldedHistories = {
    val res = WireInit(this)
    for (i <- 0 until this.hist.length) {
      res.hist(i) := this.hist(i).update(ghr, ptr, shift, taken)
    }
    res
  }
  def update(ghr: Vec[Bool], ptr: CGHPtr, br_valids: Vec[Bool], br_takens: Vec[Bool]): AllFoldedHistories = {
    val last_valid_idx = PriorityMux(
      br_valids.reverse :+ true.B,
      (numBr to 0 by -1).map(_.U(log2Ceil(numBr+1).W))
    )
    val first_taken_idx = PriorityEncoder(false.B +: br_takens)
    val smaller = Mux(last_valid_idx < first_taken_idx,
      last_valid_idx,
      first_taken_idx
    )
    val shift = smaller
    val taken = br_takens.reduce(_||_)
    update(ghr, ptr, shift, taken)
  }
  def update(ghr: Vec[Bool], ptr: CGHPtr, resp: BranchPredictionBundle): AllFoldedHistories = {
    update(ghr, ptr, resp.preds.br_valids, resp.real_br_taken_mask)
  }
  def display(cond: Bool) = {
    for (h <- hist) {
      XSDebug(cond, p"hist len ${h.len}, folded len ${h.compLen}, value ${Binary(h.folded_hist)}\n")
    }
  }
}

class BasePredictorInput (implicit p: Parameters) extends XSBundle with HasBPUConst {
  def nInputs = 1

  val s0_pc = UInt(VAddrBits.W)

  val folded_hist = new AllFoldedHistories(foldedGHistInfos)
  val phist = UInt(PathHistoryLength.W)

  val resp_in = Vec(nInputs, new BranchPredictionResp)

  // val final_preds = Vec(numBpStages, new)
  // val toFtq_fire = Bool()

  // val s0_all_ready = Bool()
}

class BasePredictorOutput (implicit p: Parameters) extends XSBundle with HasBPUConst {
  val s3_meta = UInt(MaxMetaLength.W) // This is use by composer
  val resp = new BranchPredictionResp

  // These store in meta, extract in composer
  // val rasSp = UInt(log2Ceil(RasSize).W)
  // val rasTop = new RASEntry
  // val specCnt = Vec(PredictWidth, UInt(10.W))
}

class BasePredictorIO (implicit p: Parameters) extends XSBundle with HasBPUConst {
  val in  = Flipped(DecoupledIO(new BasePredictorInput)) // TODO: Remove DecoupledIO
  // val out = DecoupledIO(new BasePredictorOutput)
  val out = Output(new BasePredictorOutput)
  // val flush_out = Valid(UInt(VAddrBits.W))

  // val ctrl = Input(new BPUCtrl())

  val s0_fire = Input(Bool())
  val s1_fire = Input(Bool())
  val s2_fire = Input(Bool())
  val s3_fire = Input(Bool())

  val s1_ready = Output(Bool())
  val s2_ready = Output(Bool())
  val s3_ready = Output(Bool())

  val update = Flipped(Valid(new BranchPredictionUpdate))
  val redirect = Flipped(Valid(new BranchPredictionRedirect))
}

abstract class BasePredictor(implicit p: Parameters) extends XSModule with HasBPUConst with BPUUtils {
  val meta_size = 0
  val spec_meta_size = 0
  val io = IO(new BasePredictorIO())

  io.out.resp := io.in.bits.resp_in(0)

  io.out.s3_meta := 0.U

  io.in.ready := !io.redirect.valid

  io.s1_ready := true.B
  io.s2_ready := true.B
  io.s3_ready := true.B

  val s0_pc       = WireInit(io.in.bits.s0_pc) // fetchIdx(io.f0_pc)
  val s1_pc       = RegEnable(s0_pc, resetVector.U, io.s0_fire)
  val s2_pc       = RegEnable(s1_pc, io.s1_fire)
  val s3_pc       = RegEnable(s2_pc, io.s2_fire)


  def getFoldedHistoryInfo: Option[Set[FoldedHistoryInfo]] = None
}

class FakePredictor(implicit p: Parameters) extends BasePredictor {
  io.in.ready                 := true.B
  io.out.s3_meta              := 0.U
  io.out.resp := io.in.bits.resp_in(0)
}

class BpuToFtqIO(implicit p: Parameters) extends XSBundle {
  val resp = DecoupledIO(new BpuToFtqBundle())
}

class PredictorIO(implicit p: Parameters) extends XSBundle {
  val bpu_to_ftq = new BpuToFtqIO()
  val ftq_to_bpu = Flipped(new FtqToBpuIO())
}

class FakeBPU(implicit p: Parameters) extends XSModule with HasBPUConst {
  val io = IO(new PredictorIO)

  val toFtq_fire = io.bpu_to_ftq.resp.valid && io.bpu_to_ftq.resp.ready

  val s0_pc = RegInit(resetVector.U)

  when(toFtq_fire) {
    s0_pc := s0_pc + (FetchWidth*4).U
  }

  when (io.ftq_to_bpu.redirect.valid) {
    s0_pc := io.ftq_to_bpu.redirect.bits.cfiUpdate.target
  }

  io.bpu_to_ftq.resp.valid := !reset.asBool() && !io.ftq_to_bpu.redirect.valid

  io.bpu_to_ftq.resp.bits := 0.U.asTypeOf(new BranchPredictionBundle)
  io.bpu_to_ftq.resp.bits.s1.pc := s0_pc
  io.bpu_to_ftq.resp.bits.s1.ftb_entry.pftAddr := s0_pc + (FetchWidth*4).U
}

@chiselName
class Predictor(implicit p: Parameters) extends XSModule with HasBPUConst {
  val io = IO(new PredictorIO)

  val predictors = Module(if (useBPD) new Composer else new FakePredictor)

  val folded_hist_infos = predictors.getFoldedHistoryInfo.getOrElse(Set()).toList
  for ((len, compLen) <- folded_hist_infos) {
    println(f"folded hist info: len $len, compLen $compLen")
  }

  val s0_fire, s1_fire, s2_fire, s3_fire = Wire(Bool())
  val s1_valid, s2_valid, s3_valid = RegInit(false.B)
  val s1_ready, s2_ready, s3_ready = Wire(Bool())
  val s1_components_ready, s2_components_ready, s3_components_ready = Wire(Bool())

  val s0_pc = WireInit(resetVector.U)
  val s0_pc_reg = RegNext(s0_pc, init=resetVector.U)
  val s1_pc = RegEnable(s0_pc, s0_fire)
  val s2_pc = RegEnable(s1_pc, s1_fire)
  val s3_pc = RegEnable(s2_pc, s2_fire)

  val s0_folded_gh = Wire(new AllFoldedHistories(foldedGHistInfos))
  val s0_folded_gh_reg = RegNext(s0_folded_gh, init=0.U.asTypeOf(s0_folded_gh))
  val s1_folded_gh = RegEnable(s0_folded_gh, 0.U.asTypeOf(s0_folded_gh), s0_fire)
  val s2_folded_gh = RegEnable(s1_folded_gh, 0.U.asTypeOf(s0_folded_gh), s1_fire)
  val s3_folded_gh = RegEnable(s2_folded_gh, 0.U.asTypeOf(s0_folded_gh), s2_fire)

  val npcGen   = new PhyPriorityMuxGenerator[UInt]
  val foldedGhGen = new PhyPriorityMuxGenerator[AllFoldedHistories]
  val ghistPtrGen = new PhyPriorityMuxGenerator[CGHPtr]
  val phistGen = new PhyPriorityMuxGenerator[UInt]
  val lastPredGen = new PhyPriorityMuxGenerator[BranchPredictionBundle]

  val ghr = RegInit(0.U.asTypeOf(Vec(HistoryLength, Bool())))
  val ghr_wire = WireInit(ghr)


  def ghist_update(ptr: CGHPtr, shift: UInt, taken: Bool): Unit = {
    for (i <- 0 until numBr) {
      when (shift === (i+1).U) {
        ghr(ptr.value - i.U) := taken
      }.elsewhen(shift > (i+1).U) {
        ghr(ptr.value - i.U) := false.B
      }
    }
  }
  def ghist_update(ptr: CGHPtr, br_valids: Vec[Bool], br_takens: Vec[Bool]): Unit = {
    val last_valid_idx = PriorityMux(
      br_valids.reverse :+ true.B,
      (numBr to 0 by -1).map(_.U(log2Ceil(numBr+1).W))
    )
    val first_taken_idx = PriorityEncoder(false.B +: br_takens)
    val smaller = Mux(last_valid_idx < first_taken_idx,
      last_valid_idx,
      first_taken_idx
    )
    val shift = smaller
    val taken = br_takens.reduce(_||_)
    ghist_update(ptr, shift, taken)
  }
  def ghist_update(ptr: CGHPtr, resp: BranchPredictionBundle): Unit = {
    ghist_update(ptr, resp.preds.br_valids, resp.real_br_taken_mask)
  }
  def getHist(ptr: CGHPtr) = (Cat(ghr_wire.asUInt, ghr_wire.asUInt) >> (ptr.value+1.U))(HistoryLength-1, 0)
  val s0_ghist_ptr = Wire(new CGHPtr)
  // s0_ghist := getHist(s0_ghist_ptr)
  val s0_ghist_ptr_reg = RegNext(s0_ghist_ptr, init=0.U.asTypeOf(new CGHPtr))
  val s1_ghist_ptr = RegEnable(s0_ghist_ptr, 0.U.asTypeOf(new CGHPtr), s0_fire)
  val s2_ghist_ptr = RegEnable(s1_ghist_ptr, 0.U.asTypeOf(new CGHPtr), s1_fire)
  val s3_ghist_ptr = RegEnable(s2_ghist_ptr, 0.U.asTypeOf(new CGHPtr), s2_fire)

  val s0_last_pred = Wire(new BranchPredictionBundle)
  val s0_last_pred_reg = RegNext(s0_last_pred, init=0.U.asTypeOf(new BranchPredictionBundle))
  val s1_last_pred = RegEnable(s0_last_pred, 0.U.asTypeOf(new BranchPredictionBundle), s0_fire)
  val s2_last_pred = RegEnable(s1_last_pred, 0.U.asTypeOf(new BranchPredictionBundle), s1_fire)
  val s3_last_pred = RegEnable(s2_last_pred, 0.U.asTypeOf(new BranchPredictionBundle), s2_fire)

  val s0_phist = WireInit(0.U(PathHistoryLength.W))
  val s0_phist_reg = RegNext(s0_phist, init=0.U(PathHistoryLength.W))
  val s1_phist = RegEnable(s0_phist, 0.U, s0_fire)
  val s2_phist = RegEnable(s1_phist, 0.U, s1_fire)
  val s3_phist = RegEnable(s2_phist, 0.U, s2_fire)

  val resp = predictors.io.out.resp


  val toFtq_fire = io.bpu_to_ftq.resp.valid && io.bpu_to_ftq.resp.ready

  // when(RegNext(reset.asBool) && !reset.asBool) {
  //   s0_folded_gh := 0.U.asTypeOf(s0_folded_gh)
  //   s0_ghist_ptr := 0.U.asTypeOf(new CGHPtr)
  //   s0_phist := 0.U
  //   s0_pc := resetVector.U
  //   s0_last_pred := 0.U.asTypeOf(new BranchPredictionBundle)
  // }

  val s1_flush, s2_flush, s3_flush = Wire(Bool())
  val s2_redirect, s3_redirect = Wire(Bool())

  // val s1_bp_resp = predictors.io.out.resp.s1
  // val s2_bp_resp = predictors.io.out.resp.s2
  // val s3_bp_resp = predictors.io.out.resp.s3

  // predictors.io := DontCare
  predictors.io.in.valid := s0_fire
  predictors.io.in.bits.s0_pc := s0_pc
  predictors.io.in.bits.folded_hist := s0_folded_gh
  predictors.io.in.bits.phist := s0_phist
  predictors.io.in.bits.resp_in(0) := (0.U).asTypeOf(new BranchPredictionResp)
  // predictors.io.in.bits.resp_in(0).s1.pc := s0_pc
  // predictors.io.in.bits.toFtq_fire := toFtq_fire

  // predictors.io.out.ready := io.bpu_to_ftq.resp.ready

  // Pipeline logic
  s2_redirect := false.B
  s3_redirect := false.B

  s3_flush := io.ftq_to_bpu.redirect.valid
  s2_flush := s3_flush || s3_redirect
  s1_flush := s2_flush || s2_redirect

  s1_components_ready := predictors.io.s1_ready
  s1_ready := s1_fire || !s1_valid
  s0_fire := !reset.asBool && s1_components_ready && s1_ready
  predictors.io.s0_fire := s0_fire

  s2_components_ready := predictors.io.s2_ready
  s2_ready := s2_fire || !s2_valid
  s1_fire := s1_valid && s2_components_ready && s2_ready && io.bpu_to_ftq.resp.ready

  when(s0_fire)         { s1_valid := true.B  }
    .elsewhen(s1_flush) { s1_valid := false.B }
    .elsewhen(s1_fire)  { s1_valid := false.B }

  predictors.io.s1_fire := s1_fire

  s3_components_ready := predictors.io.s3_ready
  s3_ready := s3_fire || !s3_valid
  s2_fire := s2_valid && s3_components_ready && s3_ready

  when(s2_flush)                    { s2_valid := false.B }
    .elsewhen(s1_fire && !s1_flush) { s2_valid := true.B  }
    .elsewhen(s2_fire)              { s2_valid := false.B }

  predictors.io.s2_fire := s2_fire

  // s3_fire := s3_valid && io.bpu_to_ftq.resp.ready
  s3_fire := s3_valid

  when(s3_flush)                    { s3_valid := false.B }
    .elsewhen(s2_fire && !s2_flush) { s3_valid := true.B  }
    .elsewhen(s3_fire)              { s3_valid := false.B }

  predictors.io.s3_fire := s3_fire

  io.bpu_to_ftq.resp.valid :=
    s1_valid && s2_components_ready && s2_ready ||
    s2_fire && s2_redirect ||
    s3_fire && s3_redirect
  io.bpu_to_ftq.resp.bits  := BpuToFtqBundle(predictors.io.out.resp)
  io.bpu_to_ftq.resp.bits.meta  := predictors.io.out.s3_meta
  // io.bpu_to_ftq.resp.bits.s3.ghist  := s3_ghist
  io.bpu_to_ftq.resp.bits.s3.folded_hist := s3_folded_gh
  io.bpu_to_ftq.resp.bits.s3.histPtr := s3_ghist_ptr
  io.bpu_to_ftq.resp.bits.s3.phist  := s3_phist

  npcGen.register(true.B, s0_pc_reg, Some("stallPC"), 0)
  foldedGhGen.register(true.B, s0_folded_gh_reg, Some("stallFGH"), 0)
  ghistPtrGen.register(true.B, s0_ghist_ptr_reg, Some("stallGHPtr"), 0)
  phistGen.register(true.B, s0_phist_reg, Some("stallPhist"), 0)
  lastPredGen.register(true.B, s0_last_pred_reg, Some("stallLastPred"), 0)

  // History manage
  // s1
  val s1_predicted_ghist_ptr = s1_ghist_ptr - resp.s1.br_count
  val s1_predicted_fh = s1_folded_gh.update(ghr, s1_ghist_ptr, resp.s1)


  // XSDebug(p"[hit] ${resp.s1.preds.hit} [s1_real_br_taken_mask] ${Binary(resp.s1.real_br_taken_mask.asUInt)}\n")
  // XSDebug(p"s1_predicted_ghist=${Binary(s1_predicted_ghist.predHist)}\n")

  when(s1_valid) { ghist_update(s1_ghist_ptr, resp.s1) }
  npcGen.register(s1_valid, resp.s1.target, Some("s1_target"), 5)
  foldedGhGen.register(s1_valid, s1_predicted_fh, Some("s1_FGH"), 5)
  ghistPtrGen.register(s1_valid, s1_predicted_ghist_ptr, Some("s1_GHPtr"), 5)
  phistGen.register(s1_valid, (s1_phist << 1) | s1_pc(instOffsetBits), Some("s1_Phist"), 5)
  lastPredGen.register(s1_valid, resp.s1, Some("s1_lastPred"), 5)

  def preds_needs_redirect(x: BranchPredictionBundle, y: BranchPredictionBundle) = {
    x.real_slot_taken_mask().asUInt.orR =/= y.real_slot_taken_mask().asUInt().orR ||
    x.preds.br_valids.asUInt =/= y.preds.br_valids.asUInt ||
    PriorityEncoder(x.real_br_taken_mask()) =/= PriorityEncoder(y.real_br_taken_mask)
  }
  // s2
  val s2_predicted_ghist_ptr = s2_ghist_ptr - resp.s2.br_count
  val s2_predicted_fh = s2_folded_gh.update(ghr, s2_ghist_ptr, resp.s2)
  val previous_s1_pred = RegEnable(resp.s1, init=0.U.asTypeOf(resp.s1), s1_fire)

  val s2_redirect_s1_last_pred = preds_needs_redirect(s1_last_pred, resp.s2)
  val s2_redirect_s0_last_pred = preds_needs_redirect(s0_last_pred_reg, resp.s2)

  // when(s2_fire) {
  //   when((s1_valid && (s1_pc =/= resp.s2.target || s2_redirect_s1_last_pred)) ||
  //     !s1_valid && (s0_pc_reg =/= resp.s2.target || s2_redirect_s0_last_pred)) {
  //     s0_folded_gh := s2_predicted_fh
  //     s0_ghist_ptr := s2_predicted_ghist_ptr
  //     ghist_update(s2_ghist_ptr, resp.s2)
  //     s2_redirect := true.B
  //     s0_pc := resp.s2.target
  //     s0_phist := (s2_phist << 1) | s2_pc(instOffsetBits)
  //     s0_last_pred := resp.s2
  //     // XSDebug(p"s1_valid=$s1_valid, s1_pc=${Hexadecimal(s1_pc)}, s2_resp_target=${Hexadecimal(resp.s2.target)}\n")
  //     // XSDebug(p"s2_correct_s1_ghist=$s2_correct_s1_ghist\n")
  //     // XSDebug(p"s1_ghist=${Binary(s1_ghist.predHist)}\n")
  //     // XSDebug(p"s2_predicted_ghist=${Binary(s2_predicted_ghist.predHist)}\n")
  //   }
  // }

  s2_redirect := s2_fire && ((s1_valid && (s1_pc =/= resp.s2.target || s2_redirect_s1_last_pred)) ||
      !s1_valid && (s0_pc_reg =/= resp.s2.target || s2_redirect_s0_last_pred))

  when(s2_redirect) { ghist_update(s2_ghist_ptr, resp.s2) }
  npcGen.register(s2_redirect, resp.s2.target, Some("s2_target"), 4)
  foldedGhGen.register(s2_redirect, s2_predicted_fh, Some("s2_FGH"), 4)
  ghistPtrGen.register(s2_redirect, s2_predicted_ghist_ptr, Some("s2_GHPtr"), 4)
  phistGen.register(s2_redirect, (s2_phist << 1) | s2_pc(instOffsetBits), Some("s2_Phist"), 4)
  lastPredGen.register(s2_redirect, resp.s2, Some("s2_lastPred"), 4)

  val s2_redirect_target = s2_fire && s1_valid && s1_pc =/= resp.s2.target
  val s2_saw_s1_hit = RegEnable(resp.s1.preds.hit, s1_fire)
  val s2_redirect_target_both_hit = s2_redirect_target &&  s2_saw_s1_hit &&  resp.s2.preds.hit

  XSPerfAccumulate("s2_redirect_because_s1_not_valid", s2_fire && !s1_valid)
  XSPerfAccumulate("s2_redirect_because_target_diff", s2_fire && s1_valid && s1_pc =/= resp.s2.target)
  XSPerfAccumulate("s2_redirect_target_diff_s1_nhit_s2_hit", s2_redirect_target && !s2_saw_s1_hit &&  resp.s2.preds.hit)
  XSPerfAccumulate("s2_redirect_target_diff_s1_hit_s2_nhit", s2_redirect_target &&  s2_saw_s1_hit && !resp.s2.preds.hit)
  XSPerfAccumulate("s2_redirect_target_diff_both_hit",  s2_redirect_target &&  s2_saw_s1_hit &&  resp.s2.preds.hit)
  XSPerfAccumulate("s2_redirect_br_direction_diff",
    s2_redirect_target_both_hit &&
    RegEnable(PriorityEncoder(resp.s1.preds.br_taken_mask), s1_fire) =/= PriorityEncoder(resp.s2.preds.br_taken_mask))
  // XSPerfAccumulate("s2_redirect_because_ghist_diff", s2_fire && s1_valid && s2_correct_s1_ghist)

  // s3
  val s3_predicted_ghist_ptr = s3_ghist_ptr - resp.s3.br_count
  val s3_predicted_fh = s3_folded_gh.update(ghr, s3_ghist_ptr, resp.s3)
  val s3_redirect_s2_last_pred = preds_needs_redirect(s2_last_pred, resp.s3)
  val s3_redirect_s1_last_pred = preds_needs_redirect(s1_last_pred, resp.s3)
  val s3_redirect_s0_last_pred = preds_needs_redirect(s0_last_pred_reg, resp.s3)

  s3_redirect := s3_fire && ((s2_valid && (s2_pc =/= resp.s3.target || s3_redirect_s2_last_pred)) ||
      (!s2_valid && s1_valid && (s1_pc =/= resp.s3.target || s3_redirect_s1_last_pred)) ||
      (!s2_valid && !s1_valid && (s0_pc_reg =/= resp.s3.target || s3_redirect_s0_last_pred)))

  when(s3_redirect) { ghist_update(s3_ghist_ptr, resp.s3) }
  npcGen.register(s3_redirect, resp.s3.target, Some("s3_target"), 3)
  foldedGhGen.register(s3_redirect, s3_predicted_fh, Some("s3_FGH"), 3)
  ghistPtrGen.register(s3_redirect, s3_predicted_ghist_ptr, Some("s3_GHPtr"), 3)
  phistGen.register(s3_redirect, (s3_phist << 1) | s3_pc(instOffsetBits), Some("s3_Phist"), 3)
  lastPredGen.register(s3_redirect, resp.s3, Some("s3_lastPred"), 3)

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

  val redirect = io.ftq_to_bpu.redirect.bits

  predictors.io.update := io.ftq_to_bpu.update
  predictors.io.redirect := io.ftq_to_bpu.redirect

  // Redirect logic
  val shift = redirect.cfiUpdate.shift
  val addIntoHist = redirect.cfiUpdate.addIntoHist

  val isBr = redirect.cfiUpdate.pd.isBr
  val taken = redirect.cfiUpdate.taken

  val oldPtr = redirect.cfiUpdate.histPtr
  val oldFh = redirect.cfiUpdate.folded_hist
  val updated_ptr = oldPtr - shift
  val updated_fh = oldFh.update(ghr, oldPtr, shift, taken && addIntoHist)
  // val updatedGh = oldGh.update(shift, taken && addIntoHist)
  val oldPh = redirect.cfiUpdate.phist
  val phNewBit = redirect.cfiUpdate.phNewBit

  when(io.ftq_to_bpu.redirect.valid) { ghist_update(oldPtr, shift, taken && addIntoHist) }
  npcGen.register(io.ftq_to_bpu.redirect.valid, redirect.cfiUpdate.target, Some("redirect_target"), 2)
  foldedGhGen.register(io.ftq_to_bpu.redirect.valid, updated_fh, Some("redirect_FGHT"), 2)
  ghistPtrGen.register(io.ftq_to_bpu.redirect.valid, updated_ptr, Some("redirect_GHPtr"), 2)
  phistGen.register(io.ftq_to_bpu.redirect.valid, (oldPh << 1) | phNewBit, Some("redirect_phist"), 2)
  // no need to assign s0_last_pred

  // XSDebug(io.ftq_to_bpu.redirect.valid, p"-------------redirect Repair------------\n")
  // XSDebug(io.ftq_to_bpu.redirect.valid, p"taken_mask=${Binary(taken_mask.asUInt)}, brValids=${Binary(brValids.asUInt)}\n")
  // XSDebug(io.ftq_to_bpu.redirect.valid, p"isBr: ${isBr}, taken: ${taken}, addIntoHist: ${addIntoHist}, shift: ${shift}\n")
  // XSDebug(io.ftq_to_bpu.redirect.valid, p"oldGh   =${Binary(oldGh.predHist)}\n")
  // XSDebug(io.ftq_to_bpu.redirect.valid, p"updateGh=${Binary(updatedGh.predHist)}\n")

  val need_reset = RegNext(reset.asBool) && !reset.asBool
  // when(need_reset) {
  //   s0_ghist := 0.U.asTypeOf(new GlobalHistory)
  //   s0_phist := 0.U
  //   s0_pc := resetVector.U
  // }

  // Reset
  npcGen.register(need_reset, resetVector.U, Some("reset_pc"), 1)
  foldedGhGen.register(need_reset, 0.U.asTypeOf(s0_folded_gh), Some("reset_FGH"), 1)
  ghistPtrGen.register(need_reset, 0.U.asTypeOf(new CGHPtr), Some("reset_GHPtr"), 1)
  phistGen.register(need_reset, 0.U, Some("reset_phist"), 1)
  lastPredGen.register(need_reset, 0.U.asTypeOf(new BranchPredictionBundle), Some("reset_lastPred"), 1)

  s0_pc         := npcGen()
  s0_pc_reg     := s0_pc
  s0_folded_gh  := foldedGhGen()
  s0_ghist_ptr  := ghistPtrGen()
  s0_phist      := phistGen()
  s0_last_pred  := lastPredGen()

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
  XSDebug("resp.s1.target=%x\n", resp.s1.target)
  XSDebug("resp.s2.target=%x\n", resp.s2.target)
  // XSDebug("s0_ghist: %b\n", s0_ghist.predHist)
  // XSDebug("s1_ghist: %b\n", s1_ghist.predHist)
  // XSDebug("s2_ghist: %b\n", s2_ghist.predHist)
  // XSDebug("s3_ghist: %b\n", s3_ghist.predHist)
  // XSDebug("s2_predicted_ghist: %b\n", s2_predicted_ghist.predHist)
  // XSDebug("s3_predicted_ghist: %b\n", s3_predicted_ghist.predHist)
  // XSDebug("s3_correct_s2_ghist: %b, s3_correct_s1_ghist: %b, s2_correct_s1_ghist: %b\n",
  // s3_correct_s2_ghist,  s3_correct_s1_ghist,  s2_correct_s1_ghist)
  XSDebug(p"s0_ghist_ptr: $s0_ghist_ptr\n")
  XSDebug(p"s1_ghist_ptr: $s1_ghist_ptr\n")
  XSDebug(p"s2_ghist_ptr: $s2_ghist_ptr\n")
  XSDebug(p"s3_ghist_ptr: $s3_ghist_ptr\n")

  io.ftq_to_bpu.update.bits.display(io.ftq_to_bpu.update.valid)
  io.ftq_to_bpu.redirect.bits.display(io.ftq_to_bpu.redirect.valid)


  XSPerfAccumulate("s2_redirect", s2_redirect)
  XSPerfAccumulate("s3_redirect", s3_redirect)

  val perfEvents = predictors.asInstanceOf[Composer].perfEvents.map(_._1).zip(predictors.asInstanceOf[Composer].perfinfo.perfEvents.perf_events)
  val perfinfo = IO(new Bundle(){
    val perfEvents = Output(new PerfEventsBundle(predictors.asInstanceOf[Composer].perfinfo.perfEvents.perf_events.length))
  })
  perfinfo.perfEvents := predictors.asInstanceOf[Composer].perfinfo.perfEvents

}

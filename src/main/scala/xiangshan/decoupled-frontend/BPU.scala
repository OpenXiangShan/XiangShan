/***************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
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

trait HasBPUConst extends HasXSParameter with HasIFUConst {
  val MaxMetaLength = 120
  val MaxBasicBlockSize = 32
  val LHistoryLength = 32
  val numBr = 2
  val useBPD = true
  val useLHist = true

  // val resetVector = 0x10000000L//TODO: set reset vec
}

trait HasBPUParameter extends HasXSParameter with HasBPUConst {
  val BPUDebug = true && !env.FPGAPlatform
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

trait BPUUtils{
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
//   val ghist = new GlobalHistory()
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

  val ghist = UInt(HistoryLength.W)

  val resp_in = Vec(nInputs, new BranchPredictionResp)
  val toFtq_fire = Bool()

  val s0_all_ready = Bool()
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
  val flush_out = Valid(UInt(VAddrBits.W))

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

abstract class BasePredictor(implicit p: Parameters) extends XSModule with HasBPUConst {
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

  // io.out.valid := io.in.valid && !io.redirect.valid

  // val s0_mask = io.f0_mask
  // val s1_mask = RegNext(s0_mask)
  // val s2_mask = RegNext(s1_mask)
  // val s3_mask = RegNext(s2_mask)

  // val s0_pc = io.f0_pc
  // val s1_pc = RegNext(s0_pc)

  val s0_update     = io.update
  val s0_update_pc = io.update.bits.pc
  val s0_update_valid = io.update.valid

  val s1_update     = RegNext(s0_update)
  val s1_update_idx = RegNext(s0_update_pc)
  val s1_update_valid = RegNext(s0_update_valid)

  val s0_redirect     = io.redirect
  val s0_redirect_pc = io.redirect.bits.cfiUpdate.target
  val s0_redirect_valid = io.redirect.valid

  val s1_redirect     = RegNext(s0_redirect)
  val s1_redirect_idx = RegNext(s0_redirect_pc)
  val s1_redirect_valid = RegNext(s0_redirect_valid)

  io.flush_out.valid := false.B
  io.flush_out.bits := DontCare
}

class FakePredictor(implicit p: Parameters) extends BasePredictor {
  io.in.ready                 := true.B
  io.out.s3_meta         := 0.U
  io.out.resp := io.in.bits.resp_in(0)
}

class BpuToFtqIO(implicit p: Parameters) extends XSBundle {
  val resp = DecoupledIO(new BranchPredictionBundle)
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
  io.bpu_to_ftq.resp.bits.pc := s0_pc
  io.bpu_to_ftq.resp.bits.ftb_entry.pftAddr := s0_pc + 32.U
  io.bpu_to_ftq.resp.bits.preds.target := s0_pc + 32.U
}

@chiselName
class Predictor(implicit p: Parameters) extends XSModule with HasBPUConst {
  val io = IO(new PredictorIO)

  val predictors = Module(if (useBPD) new Composer else new FakePredictor)

  val s0_pc = WireInit(resetVector.U)
  val s0_pc_reg = RegInit(resetVector.U)

  // val s3_gh = predictors.io.out.bits.resp.s3.ghist
  // val final_gh = RegInit(0.U.asTypeOf(new GlobalHistory))
  val s0_ghist = WireInit(0.U.asTypeOf(new GlobalHistory))
  val s1_ghist = RegNext(s0_ghist)
  val s2_ghist = Reg(new GlobalHistory)

  val toFtq_fire = io.bpu_to_ftq.resp.valid && io.bpu_to_ftq.resp.ready

  when(RegNext(reset.asBool) && !reset.asBool) {
    s0_ghist := 0.U.asTypeOf(new GlobalHistory)
    // s0_pc := resetVector.U
  }

  when(toFtq_fire) {
    // final_gh := s3_gh.update(io.bpu_to_ftq.resp.bits.preds.is_br.reduce(_||_) && !io.bpu_to_ftq.resp.bits.preds.taken,
    //   io.bpu_to_ftq.resp.bits.preds.taken)
  }

  val s1_flush, s2_flush, s3_flush = Wire(Bool())
  val s2_redirect, s3_redirect = Wire(Bool())

  val s0_fire, s1_fire, s2_fire, s3_fire = Wire(Bool())
  val s1_valid, s2_valid, s3_valid = RegInit(false.B)
  val s1_ready, s2_ready, s3_ready = Wire(Bool())
  val s1_components_ready, s2_components_ready, s3_components_ready = Wire(Bool())

  val s1_bp_resp = predictors.io.out.resp.s1
  val s2_bp_resp = predictors.io.out.resp.s2
  val s3_bp_resp = predictors.io.out.resp.s3

  predictors.io := DontCare
  predictors.io.in.valid := s0_fire
  predictors.io.in.bits.s0_pc := s0_pc
  predictors.io.in.bits.ghist := s0_ghist.predHist
  predictors.io.in.bits.resp_in(0) := (0.U).asTypeOf(new BranchPredictionResp)
  // predictors.io.in.bits.resp_in(0).s1.pc := s0_pc
  predictors.io.in.bits.toFtq_fire := toFtq_fire

  // predictors.io.out.ready := io.bpu_to_ftq.resp.ready

  // Pipeline logic
  s2_redirect := false.B
  s3_redirect := false.B

  s3_flush := false.B
  s2_flush := io.ftq_to_bpu.redirect.valid
  s1_flush := s2_flush || s2_redirect

  s1_components_ready := predictors.io.s1_ready
  s1_ready := s1_fire || !s1_valid
  s0_fire := !reset.asBool && s1_components_ready && s1_ready
  predictors.io.s0_fire := s0_fire

  s2_components_ready := predictors.io.s2_ready
  s2_ready := s2_fire || !s2_valid
  s1_fire := s1_valid && s2_components_ready && s2_ready

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

  s3_fire := s3_valid && io.bpu_to_ftq.resp.ready

  when(s3_flush)                    { s3_valid := false.B }
    .elsewhen(s2_fire && !s2_flush) { s3_valid := true.B  }
    .elsewhen(s3_fire)              { s3_valid := false.B }

  predictors.io.s3_fire := s3_fire


  // predictor override redirect
  // val resp_valid = predictors.io.out.resp.valids
  // val finalPredValid = resp_valid(2)
  val finalPredValid = s2_fire
  val finalPredResp = predictors.io.out.resp
  when(finalPredValid) {
    when(finalPredResp.s2.preds.target =/= RegNext(s0_pc)) {
      s2_redirect := true.B
    }

    // when(finalPredResp.s3.preds.target =/= RegNext(s1_pc)) {
    //   s3_redirect := true.B
    //   io.out.valid := true.B
    //   io.out.bits  := finalPredResp.s3.preds.target
    // }
  }

  // io.bpu_to_ftq.resp.bits.hit   := predictors.io.out.bits.resp.s3.hit
  // io.bpu_to_ftq.resp.bits.preds := predictors.io.out.bits.resp.s3.preds
  // io.bpu_to_ftq.resp.bits.meta  := predictors.io.out.bits.resp.s3.meta
  io.bpu_to_ftq.resp.valid := s3_fire && !io.ftq_to_bpu.redirect.valid
  io.bpu_to_ftq.resp.bits  := predictors.io.out.resp.s3

  val resp = predictors.io.out.resp

  when(io.ftq_to_bpu.redirect.valid) {
    s0_pc := io.ftq_to_bpu.redirect.bits.cfiUpdate.target
  }.elsewhen(s2_redirect) {
    s0_pc := finalPredResp.s2.preds.target
  }.elsewhen(s1_fire) {
    s0_pc := resp.s1.preds.target
  }.otherwise {
    s0_pc := s0_pc_reg
  }
  
  s0_pc_reg := s0_pc

  predictors.io.update := io.ftq_to_bpu.update
  predictors.io.redirect := io.ftq_to_bpu.redirect

  val redirect = io.ftq_to_bpu.redirect.bits
  when(io.ftq_to_bpu.redirect.valid) {
    val isMisPred = redirect.level === 0.U
    val oldGh = redirect.cfiUpdate.hist
    val sawNTBr = redirect.cfiUpdate.sawNotTakenBranch
    val isBr = redirect.cfiUpdate.pd.isBr
    val taken = Mux(isMisPred, redirect.cfiUpdate.taken, redirect.cfiUpdate.predTaken)
    val updatedGh = oldGh.update(sawNTBr || isBr, isBr && taken)
    // final_gh := updatedGh
  }
}

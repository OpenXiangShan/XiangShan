package xiangshan.frontend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.experimental.chiselName
import chisel3.util._
import xiangshan._
import utils._

trait HasBPUConst extends HasXSParameter {
  val MaxMetaLength = 120
  val MaxBasicBlockSize = 32
  val LHistoryLength = 32
  val num_br = 1
  val useBPD = true
  val useLHist = true
}

trait HasBPUParameter extends HasXSParameter {
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

class TableAddr(val idxBits: Int, val banks: Int)(implicit p: Parameters) extends XSBundle with HasIFUConst {
  def tagBits = VAddrBits - idxBits - instOffsetBits

  val tag = UInt(tagBits.W)
  val idx = UInt(idxBits.W)
  val offset = UInt(instOffsetBits.W)

  def fromUInt(x: UInt) = x.asTypeOf(UInt(VAddrBits.W)).asTypeOf(this)
  def getTag(x: UInt) = fromUInt(x).tag
  def getIdx(x: UInt) = fromUInt(x).idx
  def getBank(x: UInt) = if (banks > 1) getIdx(x)(log2Up(banks) - 1, 0) else 0.U
  def getBankIdx(x: UInt) = if (banks > 1) getIdx(x)(idxBits - 1, log2Up(banks)) else getIdx(x)
}

class BranchPrediction(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val taken = Bool()
  val is_br = Vec(num_br, Bool())
  val is_jal = Bool()
  val is_call = Bool()
  val is_ret = Bool()
  val call_is_rvc = Bool()
  val pred_target = UInt(VAddrBits.W)
}

class BranchPredictionBundle(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val pc = UInt(VAddrBits.W)
  val hit = Bool()
  val preds = new BranchPrediction
  val meta = UInt(MaxMetaLength.W)
  val spec_meta = UInt(MaxMetaLength.W)
  val ftb_entry = new FTBEntry() // TODO: Send this entry to ftq
}

class BranchPredictionResp(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val f1 = new BranchPredictionBundle()
  val f2 = new BranchPredictionBundle()
  val f3 = new BranchPredictionBundle()
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

class BranchPredictionUpdate(implicit p: Parameters) extends BranchPredictionBundle with HasBPUConst {}

class BranchPredictionRedirect(implicit p: Parameters) extends XSBundle with HasBPUConst {
  val pc = UInt(VAddrBits.W)
  val level = RedirectLevel()
  val interrupt = Bool()
  val cfi_update = new CfiUpdateInfo()

  def flushItself() = RedirectLevel.flushItself(level)
}

class BasePredictorIO (implicit p: Parameters) extends XSBundle with HasBPUConst {
  def nInputs = 1

  val f0_valid = Input(Bool())
  val f0_pc = Input(UInt(VAddrBits.W))

  val ghist = Input(UInt(HistoryLength.W))

  val resp_in = Input(Vec(nInputs, new BranchPredictionResp))
  val resp = Valid(new BranchPredictionResp)

  val meta = Output(UInt(MaxMetaLength.W)) // This is use by composer
  val spec_meta = Output(UInt(MaxMetaLength.W)) // This is use by composer

  val toFtq_fire = Input(Bool())

  val update = Input(Valid(new BranchPredictionUpdate))
  val redirect = Input(Valid(new BranchPredictionRedirect))

  val flush = Bool()
  val flush_out = Valid(UInt(VAddrBits.W))

  val in_ready = Bool()
}

abstract class BasePredictor(implicit p: Parameters) extends XSModule with HasBPUConst {
  val meta_size = 0
  val spec_meta_size = 0

  val io = IO(new BasePredictorIO())

  io.resp := io.resp_in(0)

  io.meta := 0.U
  io.spec_meta := 0.U

  val s0_pc       = io.f0_pc // fetchIdx(io.f0_pc)
  val s1_pc       = RegNext(s0_pc)
  val s2_pc       = RegNext(s1_pc)
  // val s3_idx       = RegNext(s2_idx)

  val s0_valid = io.f0_valid
  val s1_valid = RegNext(s0_valid)
  val s2_valid = RegNext(s1_valid)
  val s3_valid = RegNext(s2_valid)

  io.resp.valid := io.f0_valid && !io.flush

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
  val s0_redirect_pc = io.redirect.bits.pc
  val s0_redirect_valid = io.redirect.valid

  val s1_redirect     = RegNext(s0_redirect)
  val s1_redirect_idx = RegNext(s0_redirect_pc)
  val s1_redirect_valid = RegNext(s0_redirect_valid)

  io.flush_out.valid := false.B
  io.flush_out.bits := DontCare
}

class FakePredictor(implicit p: Parameters) extends BasePredictor {
  io.resp.valid       := true.B
  io.resp.bits.f3.pc  := RegNext(RegNext(io.f0_pc))
  io.resp.bits.f3.hit := false.B
  io.resp.bits.f3.preds.taken   := false.B
  io.resp.bits.f3.preds.is_br   := 0.U
  io.resp.bits.f3.preds.is_jal  := false.B
  io.resp.bits.f3.preds.pred_target:= io.f0_pc + (FetchWidth*4).U

  io.resp.bits.f3.meta  := 0.U
}

class PredictorIO(implicit p: Parameters) extends XSBundle {
  val bpu_to_ftq = new BPUToFtq()
  val ftq_to_bpu = new FtqTOBPU()
  val fetch_to_bpu = new FetchTOBPU()
}

class FakeBPU(implicit p: Parameters) extends XSModule with HasBPUConst with HasIFUConst {
  val io = IO(new PredictorIO)

  val toFtq_fire = io.bpu_to_ftq.resp.valid && io.bpu_to_ftq.resp.ready

  val f0_pc = RegInit(resetVector.U)

  when(io.fetch_to_bpu.ifu_redirect.valid) {
    f0_pc := io.fetch_to_bpu.ifu_redirect.bits
  }.elsewhen(toFtq_fire) {
    f0_pc := f0_pc + (FetchWidth*4).U
  }

  io.bpu_to_ftq.resp.valid := true.B
  io.bpu_to_ftq.resp.bits := 0.U.asTypeOf(new BranchPredictionBundle)

  io.bpu_to_ftq.resp.bits.pc := f0_pc

  io.bpu_to_ftq.resp.bits.preds := 0.U.asTypeOf(new BranchPrediction)
  io.bpu_to_ftq.resp.bits.preds.pred_target := f0_pc + (PredictWidth*4).U
}

@chiselName
class Predictor(implicit p: Parameters) extends XSModule with HasBPUConst with HasIFUConst {
  val io = IO(new PredictorIO)

  val predictors = Module(if (useBPD) new Composer else new FakePredictor)

  io.bpu_to_ftq.resp.valid := predictors.io.resp.valid

  val toFtq_fire = io.bpu_to_ftq.resp.valid && io.bpu_to_ftq.resp.ready

  val f0_pc = RegInit(resetVector.U)

  when(io.fetch_to_bpu.ifu_redirect.valid) {
    f0_pc := io.fetch_to_bpu.ifu_redirect.bits
  }.elsewhen(toFtq_fire) {
    f0_pc := io.bpu_to_ftq.resp.bits.preds.pred_target
  }

  val f0_ghist = RegInit(0.U.asTypeOf(new GlobalHistory))

  when(toFtq_fire) {
    f0_ghist.update(io.bpu_to_ftq.resp.bits.preds.is_br.reduce(_||_) && !io.bpu_to_ftq.resp.bits.preds.taken,
      io.bpu_to_ftq.resp.bits.preds.taken)
  }

  predictors.io.f0_valid := !reset.asBool && io.bpu_to_ftq.resp.ready
  predictors.io.f0_pc := f0_pc
  
  predictors.io.ghist := f0_ghist

  predictors.io.resp_in(0) := (0.U).asTypeOf(new BranchPredictionResp)

  io.bpu_to_ftq.resp.bits.hit := predictors.io.resp.bits.f3.hit
  io.bpu_to_ftq.resp.bits.preds := predictors.io.resp.bits.f3
  io.bpu_to_ftq.resp.bits.meta := predictors.io.resp.bits.f3.meta
  io.bpu_to_ftq.resp.bits.spec_meta := predictors.io.resp.bits.f3.spec_meta

  predictors.io.toFtq_fire := toFtq_fire

  io.bpu_to_ftq.resp.bits.pc := predictors.io.resp.bits.f3.preds.pred_target
  
  predictors.io.update := io.ftq_to_bpu.update
  predictors.io.redirect := io.ftq_to_bpu.redirect
}
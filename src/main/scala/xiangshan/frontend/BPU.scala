package xiangshan.frontend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.ALUOpType
import xiangshan.backend.JumpOpType

class TableAddr(val idxBits: Int, val banks: Int) extends XSBundle {
 def tagBits = VAddrBits - idxBits - 1

 val tag = UInt(tagBits.W)
 val idx = UInt(idxBits.W)
 val offset = UInt(1.W)

 def fromUInt(x: UInt) = x.asTypeOf(UInt(VAddrBits.W)).asTypeOf(this)
 def getTag(x: UInt) = fromUInt(x).tag
 def getIdx(x: UInt) = fromUInt(x).idx
 def getBank(x: UInt) = getIdx(x)(log2Up(banks) - 1, 0)
 def getBankIdx(x: UInt) = getIdx(x)(idxBits - 1, log2Up(banks))
}

class PredictorResponse extends XSBundle {
  // the valid bits indicates whether a target is hit
  val ubtb = new Bundle {
    val targets = Vec(PredictWidth, ValidUndirectioned(UInt(VaddrBits.W)))
    val takens = Vec(PredictWidth, Bool())
    val isRVC = Vec(PredictWidth, Bool())
  }
  // the valid bits indicates whether a target is hit
  val btb = new Bundle {
    val targets = Vec(PredictWidth, ValidUndirectioned(UInt(VaddrBits.W)))
    val isRVC = Vec(PredictWidth, Bool())
  }
  val bim = new Bundle {
    val ctrs = Vec(PredictWidth, ValidUndirectioned(Bool()))
  }
  // the valid bits indicates whether a prediction is hit
  val tage = new Bundle {
    val takens = Vec(PredictWidth, ValidUndirectioned(Bool()))
  }
}

class BPUStageIO extends XSBundle {
  val pc = UInt(VAddrBits.W)
  val mask = UInt(PredictWidth.W)
  val resp = new PredictorResponse
  val brInfo = Vec(PredictWidth, new BranchInfo)
}


abstract class BPUStage extends XSModule {
  class defaultIO extends XSBundle {
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new BPUStageIO))
    val pred = Decoupled(new BranchPrediction)
    val out = Decoupled(new BPUStageIO)
  }
  io.in.pc.ready = true.B
  val inFire = io.in.pc.fire()
  val inFireLatch = RegNext(inFire)
  val inLatch = RegEnable(io.in, inFire)
  val predLatch = RegEnable(io.pred.bits, inFireLatch)
  val outLatch = RegEnable(io.out.bits, inFireLatch)

  io.out.bits <> DontCare

  val outValid = RegInit(false.B)
  val outFire = io.out.fire()
  when (io.flush || inFire) {
    outValid := true.B
  }.elsewhen (outFire) {
    outValid := false.B
  }
  io.out.valid := outValid

  io.pred.valid := io.out.fire()
}

class BPUStage1 extends BPUStage {

  val io = new defaultIO

  val btb = Module(new BTB)
  // Use s0 pc and give prediction at s2
  btb.io.in.pc <> io.in.pc
  btb.io.in.mask := inLatch.inMask
  btb.io.update := io.inOrderBrInfo

  io.out.bits.resp.btb <> btb.out

  val bim = Module(new BIM)
  bim.io.in.pc <> io.in.pc
  bim.io.in.mask := inLatch.inMask
  bim.io.update := io.inOrderBrInfo


  val ubtbResp = io.in.bits.ubtb
  // the read operation is already masked, so we do not need to mask here
  val ubtbTakens = Reverse(Cat((0 until PredictWidth).map(i => ubtbResp.targets(i).valid && ubtbResp.takens(i))))
  val taken = ubtbTakens.orR
  val jmpIdx = PriorityEncoder(ubtbTakens)
  // get the last valid inst
  val lastValidPos = PriorityMux((PredictWidth-1 to 0).map(i => (inLatch.mask(i), i.U)))
  when (inFireLatch) {
    io.pred.bits.redirect := taken
    io.pred.bits.jmpIdx := jmpIdx
    io.pred.bits.target := ubtbResp.targets(jmpIdx)
    io.pred.bits.saveHalfRVI := ((lastValidPos === jmpIdx && taken) || !taken ) && !ubtbResp.isRVC(lastValidPos)
  }
  else {
    io.pred.bits := predLatch
  }

  when (inFireLatch) {
    io.out.bits.pc := inLatch.pc.bits
    io.out.bits.mask := inLatch.inMask
    io.out.bits.resp.ubtb <> ubtb.out
    io.out.bits.resp.btb <> btb.out
    io.out.bits.resp.bim <> bim.out
    io.out.bits.resp.tage <> DontCare
    io.out.bits.brInfo.foreach(_ <> DontCare)
  }
  else {
    io.out.bits := outLatch
  }
}

class BPUStage2 extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new BPUStageIO))
    val pred = Decoupled(new BranchPrediction)
    val out = Decoupled(new BPUStageIO)
  })
}

class BPUStage3 extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new BPUStageIO))
    val pred = Decoupled(new BranchPrediction)
    val predecode = Flipped(ValidIO(new Predecode))
  })
}


abstract class BasePredictor {
  val metaLen = 0

  // An implementation MUST extend the IO bundle with a response
  // and the special input from other predictors, as well as
  // the metas to store in BRQ
  abstract class resp extends XSBundle {}
  abstract class fromOthers extends XSBundle {}
  abstract class meta extends XSBundle {}

  class defaultBasePredictorIO extends XSBundle {
    val flush = Input(Bool())
    val pc = Flipped(ValidIO(UInt(VAddrBits.W)))
    val hist = Input(new Bundle {
      val bits = UInt(ExtHistoryLength.W)
      val ptr = UInt(log2Up(ExtHistoryLength).W)
    })
    val inMask = Input(UInt(PredictWidth.W))
    val update = Flipped(ValidIO(new BranchUpdateInfo))
  }
}

trait BranchPredictorComponents extends HasXSParameter {
  val ubtb = new Module(MicroBTB)
  val btb = new Module(BTB)
  val bim = new Module(BIM)
  val tage = new Module(Tage)
  val preds = Seq(ubtb, btb, bim, tage)
  pred.map(_.io := DontCare)
}

abstract class BaseBPU extends XSModule with BranchPredictorComponents{
  val io = IO(new Bundle() {
    // from backend
    val inOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
    // from ifu, frontend redirect
    val flush = Input(UInt(3.W))
    // from if1
    val in = new Bundle {
      val pc = Flipped(ValidIO(UInt(VAddrBits.W)))
      val hist = Input(UInt(ExtHistoryLength.W))
      val inMask = Input(UInt(PredictWidth.W))
    }
    // to if2/if3/if4
    val out = Vec(3, Decoupled(new BranchPrediction))
    // from if4
    val predecode = Flipped(ValidIO(new Predecode))
    // to if4, some bpu info used for updating
    val branchInfo = Decoupled(Vec(PredictWidth, new BranchInfo))
  })
}


class FakeBPU extends BaseBPU {
  io.out.foreach(i => {
    i <> DontCare
    i.redirect := false.B
  })
  io.branchInfo <> DontCare
}

class BPU extends BaseBPU {

  val s1 = Module(new BPUStage1)
  val s2 = Module(new BPUStage2)
  val s3 = Module(new BPUStage3)

  s1.io.flush := io.flush(0)
  s2.io.flush := io.flush(1)
  s3.io.flush := io.flush(2)

  //**********************Stage 1****************************//
  val s1_fire = io.in.pc.valid
  val s1_resp_in = new PredictorResponse
  val s1_brInfo_in = VecInit(0.U.asTypeOf(Vec(PredictWidth, new BranchInfo)))

  s1_resp_in := DontCare
  s1_brInfo_in := DontCare

  val s1_inLatch = RegEnable(io.in, s1_fire)
  ubtb.io.in.pc <> s1_inLatch.pc
  ubtb.io.in.mask := s1_inLatch.inMask

  // Wrap ubtb response into resp_in and brInfo_in
  s1_resp_in.ubtb <> ubtb.io.out
  s1_brInfo_in.ubtbWriteWay := ubtb.io.meta.writeWay
  s1_brInfo_in.ubtbHits := VecInit(ubtb.io.out.targets.map(_.valid))

  btb.io.in.pc <> io.in.pc
  btb.io.in.mask := io.in.inMask

  // Wrap btb response into resp_in and brInfo_in
  s1_resp_in.btb <> btb.io.out
  s1_brInfo_in.btbWriteWay := btb.io.meta.writeWay

  bim.io.in.pc <> io.in.pc
  bim.io.in.mask := io.in.inMask

  // Wrap bim response into resp_in and brInfo_in
  s1_resp_in.bim <> bim.io.out
  s1_brInfo_in.bimCtrs := bim.io.out

  tage.io.in.pc <> io.in.pc
  tage.io.in.hist := io.in.

  s1.io.in.bits.pc := io.in.pc.bits
  s1.io.in.bits.mask := io.in.mask
  s1.io.in.bits.resp := s1_resp_in
  s1.io.in.bits.brInfo := s1_brInfo_in



  s2.io.in <> s1.io.out
  s3.io.in <> s2.io.out

  io.out(0) <> s1.io.pred
  io.out(1) <> s2.io.pred
  io.out(2) <> s3.io.pred

  s1.io.inOrderBrInfo <> io.inOrderBrInfo

  s3.io.predecode <> io.predecode
}

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
  }
  // the valid bits indicates whether a target is hit
  val btb = new Bundle {
    val targets = Vec(PredictWidth, ValidUndirectioned(UInt(VaddrBits.W)))
    val takens = Vec(PredictWidth, Bool())
  }
  // the valid bits indicates whether a prediction is hit
  val tage = new Bundle {
    val takens = Vec(PredictWidth, ValidUndirectioned(Bool()))
  }
}

class BPUStageIO extends XSBundle {
  val pc = Output(UInt(VAddrBits.W))
  val btbResp = Output(new PredictorResponse)
  val brInfo = Output(Vec(PredictWidth, new BranchInfo))
}


class BPUStage1 extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = new Bundle { val pc = Flipped(ValidIO(UInt(VAddrBits.W))) }
    val pred = Decoupled(new BranchPrediction)
    val out = Decoupled(new BPUStageIO)
    // For repair & update
    val outOfOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
    val inOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
  })

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

class BaseBPU extends XSModule {
  val io = IO(new Bundle() {
    // from backend
    val outOfOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
    val inOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
    // from ifu, frontend redirect
    val flush = Input(UInt(3.W))
    // from if1
    val in = new Bundle { val pc = Flipped(ValidIO(UInt(VAddrBits.W))) }
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

  s1.io.in <> io.in
  s2.io.in <> s1.io.out
  s3.io.in <> s2.io.out

  io.out(0) <> s1.io.pred
  io.out(1) <> s2.io.pred
  io.out(2) <> s3.io.pred

  s1.io.redirect <> io.redirect
  s1.io.outOfOrderBrInfo <> io.outOfOrderBrInfo
  s1.io.inOrderBrInfo <> io.inOrderBrInfo
  s2.io.outOfOrderBrInfo <> io.outOfOrderBrInfo
  s2.io.inOrderBrInfo <> io.inOrderBrInfo

  s3.io.predecode <> io.predecode
}

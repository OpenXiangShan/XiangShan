package xiangshan.frontend

import chisel3._
import chisel3.util._
import utils._
import xiangshan._
import xiangshan.backend.ALUOpType
import xiangshan.backend.JumpOpType

class BPUStage1To2IO extends XSBundle {
  // TODO
}

class BPUStage2To3IO extends XSBundle {
  // TODO
}

class BPUStage1 extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = new Bundle { val pc = Flipped(ValidIO(UInt(VAddrBits.W))) }
    val s1_out = Decoupled(new BranchPrediction)
    val out = Decoupled(new BPUStage1To2IO)
    val redirect = Flipped(ValidIO(new Redirect)) // used to fix ghr
    val outOfOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
    val inOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo))
  })

}

class BPUStage2 extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new BPUStage1To2IO))
    val s2_out = Decoupled(new BranchPrediction)
    val out = Decoupled(new BPUStage2To3IO)
    val outOfOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo)) // delete this if useless
    val inOrderBrInfo = Flipped(ValidIO(new BranchUpdateInfo)) // delete this if useless
  })

}

class BPUStage3 extends XSModule {
  val io = IO(new Bundle() {
    val flush = Input(Bool())
    val in = Flipped(Decoupled(new BPUStage2To3IO))
    val s3_out = Decoupled(new BranchPrediction)
    val predecode = Flipped(ValidIO(new Predecode))
  })
  
}

class BPU extends XSModule {
  val io = IO(new Bundle() {
    // from backend
    val redirect = Flipped(ValidIO(new Redirect))
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
    val branchInfo = Decoupled(new BranchInfo)
  })

  val s1 = Module(new BPUStage1)
  val s2 = Module(new BPUStage2)
  val s3 = Module(new BPUStage3)

  s1.io.flush := io.flush(0)
  s2.io.flush := io.flush(1)
  s3.io.flush := io.flush(2)

  s1.io.in <> io.in
  s2.io.in <> s1.io.out
  s3.io.in <> s2.io.out

  io.out(0) <> s1.io.s1_out
  io.out(1) <> s2.io.s2_out
  io.out(2) <> s3.io.s3_out

  s1.io.redirect <> io.redirect
  s1.io.outOfOrderBrInfo <> io.outOfOrderBrInfo
  s1.io.inOrderBrInfo <> io.inOrderBrInfo
  s2.io.outOfOrderBrInfo <> io.outOfOrderBrInfo
  s2.io.inOrderBrInfo <> io.inOrderBrInfo

  s3.io.predecode <> io.predecode
}

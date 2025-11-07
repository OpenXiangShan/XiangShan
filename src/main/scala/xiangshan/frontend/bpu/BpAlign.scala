package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import difftest.util.dpic.CppStructGenerator
import difftest.util.dpic.DPICBundle
import difftest.util.dpic.DPICWrapper
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.BpuToFtqIO
import xiangshan.frontend.FrontendBundle
import xiangshan.frontend.FrontendModule
import xiangshan.frontend.FtqToBpuIO
import xiangshan.frontend.PrunedAddr
import xiangshan.frontend.bpu.BpuCommit
import xiangshan.frontend.bpu.BpuMeta
import xiangshan.frontend.bpu.BpuPrediction
import xiangshan.frontend.bpu.BpuRedirect
import xiangshan.frontend.bpu.BpuSpeculationMeta
import xiangshan.frontend.bpu.BpuTrain
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.ftq.FtqPtr

class BpAlignToFtq(implicit p: Parameters) extends FrontendBundle {
  val prediction:      Valid[BpuPrediction]      = Valid(new BpuPrediction)
  val speculationMeta: Valid[BpuSpeculationMeta] = Valid(new BpuSpeculationMeta)
  val meta:            Valid[BpuMeta]            = Valid(new BpuMeta)
  val s3FtqPtr:        FtqPtr                    = new FtqPtr
}

class FtqToBpAlign(implicit p: Parameters) extends FrontendBundle {
  val prediction_ready:      Bool               = Bool()
  val speculationMeta_ready: Bool               = Bool()
  val meta_ready:            Bool               = Bool()
  val redirect:              Valid[BpuRedirect] = Valid(new BpuRedirect)
  val train:                 Valid[BpuTrain]    = Valid(new BpuTrain)
  val commit:                Valid[BpuCommit]   = Valid(new BpuCommit)
  val bpuPtr:                FtqPtr             = new FtqPtr
  val redirectFromIFU:       Bool               = Bool()
  val ctrl:                  BpuCtrl            = new BpuCtrl
  val resetVector:           PrunedAddr         = PrunedAddr(PAddrBits)
}

class BpAlignIO(implicit val p: Parameters) extends DPICBundle {
  val in:    FtqToBpAlign = Input(new FtqToBpAlign)
  val out:   BpAlignToFtq = Output(new BpAlignToFtq)
  val clock: Bool         = Input(Bool())
  val reset: Bool         = Input(Bool())
}

class BpAlignInner(implicit p: Parameters) extends FrontendModule {
  val io: BpAlignIO = IO(new BpAlignIO)
  private val wrapper = Module(new DPICWrapper(io, "BpAlign_wrapper"))
  wrapper.io <> io
  CppStructGenerator.generateCppHeader(io)
}

class BpAlign(implicit p: Parameters) extends BpuTopModule {
  class DummyBpuIO extends BpuTopIO
  val io: DummyBpuIO = IO(new DummyBpuIO)
  private val inner = Module(new BpAlignInner)
  inner.io.clock := clock.asBool
  inner.io.reset := reset.asBool
  // to BpAlign
  inner.io.in.prediction_ready      := io.toFtq.prediction.ready
  inner.io.in.speculationMeta_ready := io.toFtq.speculationMeta.ready
  inner.io.in.meta_ready            := io.toFtq.meta.ready

  inner.io.in.redirect.valid := io.fromFtq.redirect.valid
  inner.io.in.redirect.bits  := io.fromFtq.redirect.bits

  inner.io.in.train.valid := io.fromFtq.train.valid
  inner.io.in.train.bits  := io.fromFtq.train.bits

  inner.io.in.commit.valid := io.fromFtq.commit.valid
  inner.io.in.commit.bits  := io.fromFtq.commit.bits

  inner.io.in.bpuPtr          := io.fromFtq.bpuPtr
  inner.io.in.redirectFromIFU := io.fromFtq.redirectFromIFU
  inner.io.in.ctrl            := io.ctrl
  inner.io.in.resetVector     := io.resetVector

  // to Ftq
  io.toFtq.prediction.valid := inner.io.out.prediction.valid
  io.toFtq.prediction.bits  := inner.io.out.prediction.bits

  io.toFtq.speculationMeta.valid := inner.io.out.prediction.valid
  io.toFtq.speculationMeta.bits  := inner.io.out.speculationMeta.bits

  io.toFtq.meta.valid := inner.io.out.meta.valid
  io.toFtq.meta.bits  := inner.io.out.meta.bits

  io.toFtq.s3FtqPtr := inner.io.out.s3FtqPtr
}

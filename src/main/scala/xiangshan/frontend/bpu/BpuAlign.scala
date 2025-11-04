package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import difftest.util.dpic.CppStructGenerator
import difftest.util.dpic.DPICBundle
import difftest.util.dpic.DPICWrapper
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.FrontendBundle
import xiangshan.frontend.FrontendModule
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
  val prediction_valid:      Bool               = Bool()
  val speculationMeta_valid: Bool               = Bool()
  val meta_valid:            Bool               = Bool()
  val prediction:            BpuPrediction      = new BpuPrediction
  val speculationMeta:       BpuSpeculationMeta = new BpuSpeculationMeta
  val meta:                  BpuMeta            = new BpuMeta
  val s3FtqPtr:              FtqPtr             = new FtqPtr
}

class FtqToBpAlign(implicit p: Parameters) extends FrontendBundle {
  val prediction_ready:      Bool        = Bool()
  val speculationMeta_ready: Bool        = Bool()
  val meta_ready:            Bool        = Bool()
  val redirect_valid:        Bool        = Bool()
  val train_valid:           Bool        = Bool()
  val commit_valid:          Bool        = Bool()
  val redirect:              BpuRedirect = new BpuRedirect
  val train:                 BpuTrain    = new BpuTrain
  val commit:                BpuCommit   = new BpuCommit
  val bpuPtr:                FtqPtr      = new FtqPtr
  val redirectFromIFU:       Bool        = Bool()
  val ctrl:                  BpuCtrl     = new BpuCtrl
  val resetVector:           PrunedAddr  = PrunedAddr(PAddrBits)
}

class BpAlignIO(implicit val p: Parameters) extends DPICBundle {
  val in:    FtqToBpAlign = Input(new FtqToBpAlign)
  val out:   BpAlignToFtq = Output(new BpAlignToFtq)
  val clock: Bool         = Input(Bool())
  val reset: Bool         = Input(Bool())
}

class BpuAlign(implicit p: Parameters) extends FrontendModule {
  val io: BpAlignIO = IO(new BpAlignIO)
  private val wrapper = Module(new DPICWrapper(io, "BpuAlign_wrapper"))
  wrapper.io <> io
  CppStructGenerator.generateCppHeader(io)
}

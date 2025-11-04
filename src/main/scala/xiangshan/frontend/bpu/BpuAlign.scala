package xiangshan.frontend.bpu

import chisel3._
import chisel3.util._
import difftest.util.dpic._
import org.chipsalliance.cde.config.Parameters
import xiangshan.frontend.FrontendBundle
import xiangshan.frontend.FrontendModule
import xiangshan.frontend.bpu.BpuCommit
import xiangshan.frontend.bpu.BpuMeta
import xiangshan.frontend.bpu.BpuPrediction
import xiangshan.frontend.bpu.BpuRedirect
import xiangshan.frontend.bpu.BpuSpeculationMeta
import xiangshan.frontend.bpu.BpuTrain
import xiangshan.frontend.bpu.BranchAttribute
import xiangshan.frontend.ftq.FtqPtr

class BpAlignToFtq(implicit p: Parameters) extends FrontendBundle {
  val prediction_valid: Bool = Bool()
  val speculationMeta_valid: Bool = Bool()
  val meta_valid: Bool = Bool()
  val speculationMeta: BpuSpeculationMeta = new BpuSpeculationMeta
  val meta:            BpuMeta            = new BpuMeta
  val s3FtqPtr:        FtqPtr                          = new FtqPtr
}

class FtqToBpAlign(implicit p: Parameters) extends FrontendBundle {

}

class BpAlignIO(implicit val p: Parameters) extends DPICBundle {
  val in: FtqToBpAlign = Input(new FtqToBpAlign)
  val out: BpAlignToFtq = Output(new BpAlignToFtq)
  val clock: Bool = Input(Bool())
  val reset: Bool = Input(Bool())
}

class BpuAlign(implicit p: Parameters) extends FrontendModule {
  val io: BpAlignIO = IO(new BpAlignIO)
  private val wrapper = Module(new DPICWrapper(io, "BpuAlign"))
  wrapper.io <> io
}

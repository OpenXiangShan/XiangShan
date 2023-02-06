package xiangshan.v2backend

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.backend.exu.FenceIO
import xiangshan.backend.fu.CSRFileIO
import xiangshan.backend.rob.RobLsqIO
import xiangshan.frontend.FtqRead
import xiangshan.mem.{LsqEnqIO, SqPtr}
import xiangshan.v2backend.Bundles.DynInst
import xiangshan._

class BackendTop(implicit p: Parameters) extends LazyModule
  with HasXSParameter {

  val numMemRsEntryMax = 16 // Todo: fix it

  lazy val module = new BackendTopImp(this)
}

class BackendMemIO(implicit p: Parameters, numMemRsEntryMax: Int) extends XSBundle {
  val rsParameters = p.alter((site, here, up) => {
    case XSCoreParamsKey => up(XSCoreParamsKey).copy(
      IssQueSize = numMemRsEntryMax
    )
  })

//  val storeIn = Vec(exuParameters.StuCnt, Flipped(ValidIO(new ExuInput(XLEN, 3))))
  val memoryViolation = Flipped(ValidIO(new Redirect))
  val enqLsq = Flipped(new LsqEnqIO)
  val sqDeq = Input(UInt(exuParameters.StuCnt.W)) // Todo: check it
  val lqCancelCnt = Input(UInt(log2Up(LoadQueueSize + 1).W))
  val sqCancelCnt = Input(UInt(log2Up(StoreQueueSize + 1).W))

  // from memBlock
  val otherFastWakeup = Flipped(Vec(exuParameters.LduCnt + 2 * exuParameters.StuCnt, ValidIO(new DynInst)))
  val loadPc = Vec(exuParameters.LduCnt, Flipped(new FtqRead(UInt(VAddrBits.W))))
//  val issueUop = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, DecoupledIO(new ExuInput(XLEN, 3)))

  val loadFastMatch = Vec(exuParameters.LduCnt, Output(UInt(exuParameters.LduCnt.W)))
  val loadFastImm   = Vec(exuParameters.LduCnt, Output(UInt(12.W))) // Imm_I

  val stIssuePtr = Input(new SqPtr()) // where is valid signal
  val flush = ValidIO(new Redirect)   // rob flush MemBlock
  val rsFeedBack = Vec(exuParameters.LsExuCnt, Flipped(new MemRSFeedbackIO()(rsParameters)))
  val lsq = new RobLsqIO
  val csrToMemBlockTlb = Output(new TlbCsrBundle)
  val csrToMemBlockCtrl = Output(new CustomCSRCtrlIO)
}

class BackendIO(implicit p: Parameters, numMemRsEntryMax: Int) extends XSBundle {
  val fromTop = new Bundle {
    val hartId = Input(UInt(8.W))
  }

  val toTop = new Bundle {
    val cpuHalted = Output(Bool())
  }

  val csrIO = new CSRFileIO
  val fenceIO = new FenceIO
  // Todo: merge these bundles into BackendFrontendIO
  val csrToFrontendTlb = Output(new TlbCsrBundle)
  val csrToFrontendCtrl = Output(new CustomCSRCtrlIO)
  val frontendIO = Flipped(new FrontendToCtrlIO)
  val mem = new BackendMemIO
//  val writeBack = Vec(exuParameters.LsExuCnt + exuParameters.StuCnt, Flipped(DecoupledIO(new ExuOutput(XLEN))))
}

class BackendTopImp(outer: BackendTop)(implicit p: Parameters) extends LazyModuleImp(outer) {
  implicit val numMemRsEntryMax = outer.numMemRsEntryMax
  val io = IO(new BackendIO)
}
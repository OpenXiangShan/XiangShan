package xiangshan.v2backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.mem.{MemWaitUpdateReq, SqPtr}
import xiangshan.v2backend.Bundles.{DynInst, ExuInput, ExuOutput}
import xiangshan.{HasXSParameter, MemRSFeedbackIO, Redirect, XSBundle}

case class IssueQueueParams(
  var numEntries      : Int = 0,
  var numEnq          : Int = 0,
  var numDeq          : Int = 0,
  var numSrc          : Int = 0,
  var dataBits        : Int = 0,
  var pregBits        : Int = 0,
  var numFastWakeup   : Int = 0,
  var numWakeup       : Int = 0,
  var allWakeup       : Int = 0,
  var hasFeedback     : Boolean = false,
  var lsqFeedback     : Boolean = false,
  var fixedLatency    : Int = -1,
  var checkWaitBit    : Boolean = false,
  var needScheduledBit: Boolean = false,
  var hasJump         : Boolean = false,
  var hasLoad         : Boolean = false,
  var hasStore        : Boolean = false,
  var hasMemAddr      : Boolean = false,
) {
  def hasLoadStore = hasLoad || hasStore || hasMemAddr
}

object DummyIQParams {
  def apply(): IssueQueueParams = {
    IssueQueueParams(
      numEntries       = 16,
      numEnq           = 2,
      numDeq           = 2,
      numSrc           = 3,
      dataBits         = 64,
      pregBits         = 8,
      numFastWakeup    = 4,
      numWakeup        = 4,
      allWakeup        = 8,
      fixedLatency     = 0,
    )
  }
}

class IssueQueue(implicit p: Parameters) extends LazyModule {
  implicit val iqParams = IssueQueueParams() // Todo: initialize it

  lazy val module = new IssueQueueImp(this)
}

class IssueQueueStatusBundle(numEnq: Int) extends Bundle {
  val empty = Output(Bool())
  val full = Output(Bool())
  val leftVec = Output(Vec(numEnq, Bool()))
}

class IssueQueueDeqRespBundle extends Bundle {
  // issue failed signal
}

class IssueQueueIO()(implicit p: Parameters, params: IssueQueueParams) extends XSBundle {
  val redirect = Flipped(ValidIO(new Redirect))

  val enq = Vec(params.numEnq, Flipped(DecoupledIO(new DynInst)))
  val srcImmData = Vec(params.numEnq, UInt(params.dataBits.W))

  val deq = Vec(params.numDeq, DecoupledIO(new ExuInput(params.dataBits, params.numSrc)))
  val deqResp = Vec(params.numDeq, ValidIO(new IssueQueueDeqRespBundle))
  val writeBack = Vec(params.numWakeup, Flipped(ValidIO(new ExuOutput(params.dataBits))))
  val status = new IssueQueueStatusBundle(params.numEnq)

  val jump = if (params.hasJump) new Bundle {
    val pc = Input(UInt(VAddrBits.W))
    val target = Input(UInt(VAddrBits.W))
  } else None

  val mem = if (params.hasLoadStore) new Bundle {
    val feedback = Vec(params.numDeq, new MemRSFeedbackIO)
    val checkwait = new Bundle {
      val stIssuePtr = Input(new SqPtr)
      val stIssue = Flipped(Vec(exuParameters.StuCnt, ValidIO(new ExuInput(params.dataBits, params.numSrc))))
      val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
    }
  }
  // Todo: wake up bundle
}

class IssueQueueImp(outer: IssueQueue)(implicit p: Parameters, iqParams: IssueQueueParams) extends LazyModuleImp(outer) with HasXSParameter{
  val io = IO(new IssueQueueIO)

  val statusArray = Module(new StatusArray)
  val immArray = Module(new DataArray(UInt(XLEN.W), iqParams.numDeq, iqParams.numEnq, iqParams.numEntries))
  val payloadArray = Module(new DataArray(new DynInst, iqParams.numDeq, iqParams.numEnq, iqParams.numEntries))
  val enqPolicy = Module(new EnqPolicy)
  val deqPolicy = Module(new DeqPolicy)

  val enqValidVec = io.enq.map(_.valid)
  val (enqSelValidVec, enqSelOHVec) = enqPolicy.io.enqSelOHVec.map(oh => (oh.valid, oh.bits)).unzip
  val enqImmValidVec = io.enq.map(enq => enq.valid && enq.bits.imm.valid)
  val enqImmVec = io.enq.map(_.bits.imm.bits)

  val (deqSelValidVec, deqSelOHVec) = deqPolicy.io.deqSelOHVec.map(oh => (oh.valid, oh.bits)).unzip

  val immArrayRdataVec = immArray.io.read.map(_.data)
  immArray.io match { case immArrayIO =>
    immArrayIO.write.zipWithIndex.foreach { case (w, i) =>
      w.en := enqSelValidVec(i) && enqImmValidVec(i)
      w.addr := enqSelOHVec(i)
      w.data := enqImmVec(i)
    }
    immArrayIO.read.zipWithIndex.foreach { case (r, i) =>
      r.addr := Mux(deqSelValidVec(i), deqSelOHVec(i), 0.U)
    }
  }

  statusArray.io match { case statusArrayIO: StatusArrayIO =>
    statusArrayIO.wakeup := io.writeBack
    statusArrayIO.enq.zipWithIndex.foreach { case (enq, i) =>
      enq.valid := enqSelValidVec(i)
      // more todo
    }
  }


}

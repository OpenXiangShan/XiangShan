package xiangshan.v2backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import freechips.rocketchip.diplomacy.{LazyModule, LazyModuleImp}
import xiangshan.mem.{MemWaitUpdateReq, SqPtr}
import xiangshan.v2backend.Bundles.{DynInst, ExuInput, WriteBackBundle, IssueQueueWakeUpBundle}
import xiangshan.{HasXSParameter, MemRSFeedbackIO, Redirect, XSBundle}

trait Region
case class IntRegion() extends Region
case class VfRegion() extends Region

case class IssueQueueParams(
  region                 : Region,
  var numEntries         : Int = 0,
  var numEnq             : Int = 0,
  var numDeq             : Int = 0,
  var numSrc             : Int = 0,
  var dataBits           : Int = 0,
  var pregBits           : Int = 0,
  var numWakeupFromWB    : Int = 0,
  var numWakeupFromIQ    : Int = 0,
  var numWakeupFromOthers: Int = 0,
  var hasBranch          : Boolean = false,
  var hasJump            : Boolean = false,
  var hasLoad            : Boolean = false,
  var hasStore           : Boolean = false,
  var hasMemAddr         : Boolean = false,
) {
  require(numWakeupFromWB > 0 && numWakeupFromIQ >= 0 && numWakeupFromOthers >= 0)
  require(numEnq > 0 && numDeq > 0)

  def hasLoadStore = hasLoad || hasStore || hasMemAddr
  def hasRedirectOut = hasBranch || hasJump
  def numAllWakeup: Int = numWakeupFromWB + numWakeupFromIQ + numWakeupFromOthers
}

object DummyIQParams {
  def apply(): IssueQueueParams = {
    IssueQueueParams(
      region           = IntRegion(),
      numEntries       = 16,
      numEnq           = 2,
      numDeq           = 2,
      numSrc           = 3,
      dataBits         = 64,
      pregBits         = 8,
      numWakeupFromWB  = 4,
    )
  }
}

class IssueQueue(implicit p: Parameters) extends LazyModule {
  implicit val iqParams = DummyIQParams() // Todo: initialize it

  lazy val module = iqParams.region match {
    case IntRegion() => new IssueQueueIntImp(this)
    case VfRegion() => new IssueQueueImp(this)
  }
}

class IssueQueueStatusBundle(numEnq: Int) extends Bundle {
  val empty = Output(Bool())
  val full = Output(Bool())
  val leftVec = Output(Vec(numEnq + 1, Bool()))
}

class IssueQueueDeqRespBundle(implicit p:Parameters, params: IssueQueueParams) extends StatusArrayDeqRespBundle

class IssueQueueIO()(implicit p: Parameters, params: IssueQueueParams) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))

  val enq = Vec(params.numEnq, Flipped(DecoupledIO(new DynInst)))

  val deq = Vec(params.numDeq, DecoupledIO(new ExuInput(params.dataBits, params.numSrc)))
  val deqResp = Vec(params.numDeq, ValidIO(new IssueQueueDeqRespBundle))
  val writeBack = Vec(params.numWakeupFromWB, Flipped(ValidIO(new WriteBackBundle(params.dataBits))))
  val status = Output(new IssueQueueStatusBundle(params.numEnq))
  val statusNext = Output(new IssueQueueStatusBundle(params.numEnq))
  // Todo: wake up bundle
}

class IssueQueueImp(outer: IssueQueue)(implicit p: Parameters, iqParams: IssueQueueParams) extends LazyModuleImp(outer) with HasXSParameter{

  lazy val io = IO(new IssueQueueIO())

  // Modules
  val statusArray   = Module(new StatusArray)
  val immArray      = Module(new DataArray(UInt(XLEN.W), iqParams.numDeq, iqParams.numEnq, iqParams.numEntries))
  val payloadArray  = Module(new DataArray(Output(new DynInst), iqParams.numDeq, iqParams.numEnq, iqParams.numEntries))
  val enqPolicy     = Module(new EnqPolicy)
  val deqPolicy     = Module(new DeqPolicy)

  // Wires
  val enqValidVec = io.enq.map(_.valid)
  val enqSelValidVec = Wire(Vec(iqParams.numEnq, Bool()))
  val enqSelOHVec = Wire(Vec(iqParams.numEnq, UInt(iqParams.numEntries.W)))

  val enqImmValidVec = io.enq.map(enq => enq.valid && enq.bits.imm.valid)
  val enqImmVec = VecInit(io.enq.map(_.bits.imm.bits))

  val deqSelValidVec = Wire(Vec(iqParams.numDeq, Bool()))
  val deqSelOHVec = Wire(Vec(iqParams.numDeq, UInt(iqParams.numEntries.W)))
  val deqRespVec = io.deqResp

  val validVec = VecInit(statusArray.io.valid.asBools)
  val canIssueVec = VecInit(statusArray.io.canIssue.asBools)
  val clearVec = VecInit(statusArray.io.clear.asBools)
  val deqFirstIssueVec = VecInit(statusArray.io.deq.map(_.isFirstIssue))

  statusArray.io match { case statusArrayIO: StatusArrayIO =>
    statusArrayIO.flush  <> io.flush
    statusArrayIO.wakeup.zipWithIndex.foreach { case (wakeup: ValidIO[IssueQueueWakeUpBundle], i) =>
      wakeup.valid := io.writeBack(i).valid
      wakeup.bits := io.writeBack(i).bits
    }
    statusArrayIO.enq.zipWithIndex.foreach { case (enq: ValidIO[StatusArrayEnqBundle], i) =>
      enq.valid                 := enqSelValidVec(i)
      enq.bits.addrOH           := enqSelOHVec(i)
      val numSrcMin = enq.bits.data.srcState.length.min(io.enq(i).bits.srcState.length)
      for (j <- 0 until numSrcMin) {
        enq.bits.data.srcState(j) := io.enq(i).bits.srcState(j)
        enq.bits.data.psrc(j)     := io.enq(i).bits.psrc(j)
        enq.bits.data.srcType(j)  := io.enq(i).bits.srcType(j)
      }
      enq.bits.data.robIdx      := io.enq(i).bits.robIdx
      enq.bits.data.ready       := false.B
      enq.bits.data.issued      := false.B
      enq.bits.data.firstIssue  := false.B
      enq.bits.data.blocked     := false.B
    }
    statusArrayIO.deq.zipWithIndex.foreach { case (deq, i) =>
      deq.deqSelOH.valid  := deqSelValidVec(i)
      deq.deqSelOH.bits   := deqSelOHVec(i)
      deq.resp            := deqRespVec(i)
    }
  }

  val immArrayRdataVec = immArray.io.read.map(_.data)
  immArray.io match { case immArrayIO: DataArrayIO[UInt] =>
    immArrayIO.write.zipWithIndex.foreach { case (w, i) =>
      w.en := enqSelValidVec(i) && enqImmValidVec(i)
      w.addr := enqSelOHVec(i)
      w.data := enqImmVec(i)
    }
    immArrayIO.read.zipWithIndex.foreach { case (r, i) =>
      r.addr := Mux(deqSelValidVec(i), deqSelOHVec(i), 0.U)
    }
  }

  val payloadArrayRdata = Wire(Vec(iqParams.numDeq, Output(new DynInst)))
  payloadArray.io match { case payloadArrayIO: DataArrayIO[DynInst] =>
    payloadArrayIO.write.zipWithIndex.foreach { case (w, i) =>
      w.en := enqSelValidVec(i)
      w.addr := enqSelOHVec(i)
      w.data := io.enq(i).bits
    }
    payloadArrayIO.read.zipWithIndex.foreach { case (r, i) =>
      r.addr := deqSelOHVec(i)
      payloadArrayRdata(i) := r.data
    }
  }

  enqPolicy match { case ep =>
    ep.io.valid     := validVec.asUInt
    enqSelValidVec  := ep.io.enqSelOHVec.map(oh => oh.valid)
    enqSelOHVec     := ep.io.enqSelOHVec.map(oh => oh.bits)
  }

  deqPolicy match { case dp =>
    dp.io.request   := canIssueVec.asUInt
    deqSelValidVec  := dp.io.deqSelOHVec.map(oh => oh.valid)
    deqSelOHVec     := dp.io.deqSelOHVec.map(oh => oh.bits)
  }

  // Todo: better counter implementation
  private val validCnt = PopCount(validVec)
  private val enqSelCnt = PopCount(enqSelValidVec)
  private val validCntNext = validCnt + enqSelCnt
  io.status.full := validVec.asUInt.andR
  io.status.empty := !validVec.asUInt.orR
  io.status.leftVec(0) := io.status.full
  for (i <- 0 until iqParams.numEnq) {
    io.status.leftVec(i + 1) := validCnt === (i + 1).U
  }
  io.statusNext.full := validCntNext === iqParams.numEntries.U
  io.statusNext.empty := validCntNext === 0.U // always false now
  io.statusNext.leftVec(0) := io.statusNext.full
  for (i <- 0 until iqParams.numEnq) {
    io.statusNext.leftVec(i + 1) := validCntNext === (i + 1).U
  }

  io.deq.zipWithIndex.foreach { case (deq, i) =>
    deq.valid := deqSelValidVec(i)
    deq.bits.fuType := payloadArrayRdata(i).fuType
    deq.bits.fuOpType := payloadArrayRdata(i).fuOpType
    deq.bits.rfWen := payloadArrayRdata(i).rfWen
    deq.bits.fpWen := payloadArrayRdata(i).fpWen
    deq.bits.vecWen := payloadArrayRdata(i).vecWen
  }
}

class IssueQueueJumpBundle()(implicit p: Parameters, params: IssueQueueParams) extends XSBundle {
  val pc = Input(UInt(VAddrBits.W))
  val target = Input(UInt(VAddrBits.W))
}

class IssueQueueMemBundle()(implicit p: Parameters, params: IssueQueueParams) extends XSBundle {
  val feedback = Vec(params.numDeq, new MemRSFeedbackIO)
  val checkwait = new Bundle {
    val stIssuePtr = Input(new SqPtr)
    val stIssue = Flipped(Vec(exuParameters.StuCnt, ValidIO(new ExuInput(params.dataBits, params.numSrc))))
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
  }
}

class IssueQueueLoadBundle()(implicit p: Parameters, params: IssueQueueParams) extends XSBundle {
  val fastMatch = UInt(exuParameters.LduCnt.W)
  val fastImm = UInt(12.W)
}

class IssueQueueIntIO()(implicit p: Parameters, params: IssueQueueParams) extends IssueQueueIO {
  val jump = if (params.hasJump) Some(new IssueQueueJumpBundle) else None
  val mem = if (params.hasLoadStore) Some(new IssueQueueMemBundle) else None
  val toLdu = if (params.hasLoad) Some(Vec(params.numDeq, new IssueQueueLoadBundle)) else None
}

class IssueQueueIntImp(outer: IssueQueue)(implicit p: Parameters, iqParams: IssueQueueParams)
  extends IssueQueueImp(outer)
  with IssueQueueMemPart
  {
  override lazy val io = IO(new IssueQueueIntIO)

}

trait IssueQueueMemPart { this: IssueQueueIntImp =>
  // Todo: correct it
  if (io.toLdu.nonEmpty) {
    io.toLdu.get.foreach(toLdu => {
      toLdu.fastImm := 0.U
      toLdu.fastMatch := false.B
    })
  }
}

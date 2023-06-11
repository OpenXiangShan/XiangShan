package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utility.HasCircularQueuePtrHelper
import utils.XSError
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.{MemWaitUpdateReq, SqPtr}
import xiangshan.backend.Bundles.IssueQueueWakeUpBundle
import xiangshan.backend.fu.FuType

class StatusEntryMemPart(implicit p:Parameters, params: IssueBlockParams) extends Bundle {
  val waitForSqIdx = new SqPtr   // generated by store data valid check
  val waitForRobIdx = new RobPtr // generated by store set
  val waitForStd = Bool()
  val strictWait = Bool()
  val sqIdx = new SqPtr
}

class StatusEntry(implicit p:Parameters, params: IssueBlockParams) extends Bundle {
  val srcState = Vec(params.numRegSrc, SrcState())

  val psrc = Vec(params.numRegSrc, UInt(params.pregBits.W))
  val srcType = Vec(params.numRegSrc, SrcType())
  val robIdx = new RobPtr
  val ready = Bool()
  val issued = Bool()           // for predict issue
  val firstIssue = Bool()
  val blocked = Bool()          // for some block reason
  // mem only
  val mem = if (params.isMemAddrIQ) Some(new StatusEntryMemPart) else None

  def srcReady: Bool = {
    VecInit(srcState.map(SrcState.isReady)).asUInt.andR
  }

  def canIssue: Bool = {
    ready && !issued && !blocked
  }
}

class StatusArrayEnqBundle(implicit p:Parameters, params: IssueBlockParams) extends Bundle {
  val addrOH = UInt(params.numEntries.W)
  val data = new StatusEntry()
}

class StatusArrayDeqRespBundle(implicit p:Parameters, params: IssueBlockParams) extends Bundle {
  val addrOH = UInt(params.numEntries.W)
  val success = Bool()
  val respType = RSFeedbackType()   // update credit if needs replay
  val dataInvalidSqIdx = new SqPtr
  val rfWen = Bool()
  val fuType = FuType()
}

class StatusArrayDeqBundle(implicit p:Parameters, params: IssueBlockParams) extends Bundle {
  val isFirstIssue = Output(Bool())
  val deqSelOH = Flipped(ValidIO(UInt(params.numEntries.W)))
}

class StatusArrayIO(implicit p: Parameters, params: IssueBlockParams) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))
  // status
  val valid = Output(UInt(params.numEntries.W))
  val canIssue = Output(UInt(params.numEntries.W))
  val clear = Output(UInt(params.numEntries.W))
  // enq
  val enq = Vec(params.numEnq, Flipped(ValidIO(new StatusArrayEnqBundle)))
  // wakeup
  val wakeup = Vec(params.numAllWakeUp, Flipped(ValidIO(new IssueQueueWakeUpBundle(params.pregBits))))
  // deq
  val deq = Vec(params.numDeq, new StatusArrayDeqBundle)
  val deqResp = Vec(params.numDeq, Flipped(ValidIO(new StatusArrayDeqRespBundle)))
  val og0Resp = Vec(params.numDeq, Flipped(ValidIO(new StatusArrayDeqRespBundle)))
  val og1Resp = Vec(params.numDeq, Flipped(ValidIO(new StatusArrayDeqRespBundle)))

  val rsFeedback = Output(Vec(5, Bool()))
  // mem only
  val fromMem = if (params.isMemAddrIQ) Some(new Bundle {
    val stIssuePtr = Input(new SqPtr)
    val memWaitUpdateReq = Flipped(new MemWaitUpdateReq)
    val slowResp = Vec(params.numDeq, Flipped(ValidIO(new StatusArrayDeqRespBundle)))
    val fastResp = Vec(params.numDeq, Flipped(ValidIO(new StatusArrayDeqRespBundle)))
  }) else None
}

class StatusArray()(implicit p: Parameters, params: IssueBlockParams) extends XSModule {
  val io = IO(new StatusArrayIO)

  val validVec = RegInit(VecInit(Seq.fill(params.numEntries)(false.B)))
  val statusVec = Reg(Vec(params.numEntries, new StatusEntry()))

  val validNextVec = Wire(Vec(params.numEntries, Bool()))
  val statusNextVec = Wire(Vec(params.numEntries, new StatusEntry()))

  val enqStatusVec = Wire(Vec(params.numEntries, ValidIO(new StatusEntry)))
  val srcWakeUpVec = Wire(Vec(params.numEntries, Vec(params.numRegSrc, Bool())))
  val deqRespVec = Wire(Vec(params.numEntries, ValidIO(new StatusArrayDeqRespBundle)))
  val flushedVec = Wire(Vec(params.numEntries, Bool()))
  val clearVec = Wire(Vec(params.numEntries, Bool()))
  val deqSelVec = Wire(Vec(params.numEntries, Bool()))
  val deqSelVec2 = Wire(Vec(params.numDeq, Vec(params.numEntries, Bool())))            // per deq's deqSelVec

  dontTouch(deqRespVec)
  // Reg
  validVec := validNextVec
  statusVec := statusNextVec

  // Wire
  enqStatusVec.zipWithIndex.foreach { case (enqStatus, i) =>
    val enqValidVec = io.enq.map(x => x.valid && x.bits.addrOH(i))
    XSError(PopCount(enqValidVec) > 1.U, "should not update the same entry\n")
    enqStatus.valid := VecInit(enqValidVec).asUInt.orR
    enqStatus.bits := Mux1H(enqValidVec, io.enq.map(_.bits.data))
  }

  validNextVec.zipWithIndex.foreach { case (validNext, i) =>
    when (enqStatusVec(i).valid) {
      validNext := true.B
    }.elsewhen(clearVec(i)) { // include rob flush
      validNext := false.B
    }.otherwise {
      validNext := validVec(i)
    }
  }

  statusNextVec.zip(statusVec).zipWithIndex.foreach { case ((statusNext, status), i) =>
    // alway update status when enq valid
    when (enqStatusVec(i).valid) {
      statusNext := enqStatusVec(i).bits
    }.otherwise {
      statusNext.psrc := status.psrc
      statusNext.srcState.zip(status.srcState).zip(srcWakeUpVec(i)).foreach { case ((stateNext, state), wakeup) =>
        stateNext := wakeup | state
      }
      statusNext.blocked := false.B // Todo
      statusNext.ready := statusNext.srcReady || status.ready
      statusNext.robIdx := status.robIdx
      statusNext.srcType := status.srcType
      statusNext.firstIssue := status.firstIssue || deqSelVec(i)

      statusNext.issued := status.issued // otherwise
      when (deqRespVec(i).valid) {
        when (RSFeedbackType.isStageSuccess(deqRespVec(i).bits.respType)) {
          statusNext.issued := true.B
        }.elsewhen (RSFeedbackType.isBlocked(deqRespVec(i).bits.respType)) {
          statusNext.issued := false.B
        }
      }
    }
  }

  srcWakeUpVec.zipWithIndex.foreach { case (wakeups: Vec[Bool], i) =>
    // wakeupVec(i)(j): the ith psrc woken up by the jth bundle
    val wakeupVec: IndexedSeq[IndexedSeq[Bool]] = io.wakeup.map((bundle: ValidIO[IssueQueueWakeUpBundle]) =>
      bundle.bits.wakeUp(statusVec(i).psrc zip statusVec(i).srcType, bundle.valid)).transpose
    wakeups := wakeupVec.map(VecInit(_).asUInt.orR)
  }

  deqSelVec.zipWithIndex.foreach { case (deqSel: Bool, i) =>
    deqSel := VecInit(io.deq.map(x => x.deqSelOH.valid && x.deqSelOH.bits(i))).asUInt.orR
  }

  deqSelVec2.zip(io.deq).foreach { case (deqSelVecSingle, deqSingle) =>
    deqSelVecSingle.zipWithIndex.foreach { case (deqSelBool, i) =>
      deqSelBool := deqSingle.deqSelOH.valid && deqSingle.deqSelOH.bits(i)
    }
  }

  val resps = io.deqResp ++ io.og0Resp ++ io.og1Resp

  deqRespVec.zipWithIndex.foreach { case (deqResp, i) =>
    val deqRespValidVec = VecInit(resps.map(x => x.valid && x.bits.addrOH(i)))
    XSError(PopCount(deqRespValidVec) > 1.U, p"status deq resp ${Binary(deqRespValidVec.asUInt)} should be one-hot)\n")
    deqResp.valid := deqRespValidVec.asUInt.orR
    deqResp.bits := Mux1H(deqRespValidVec, resps.map(_.bits))
  }

  flushedVec.zipWithIndex.foreach { case (flushed, i) =>
    flushed := statusNextVec(i).robIdx.needFlush(io.flush)
  }

  // (1) flushed by rob
  // (2) deq success
  clearVec.zipWithIndex.foreach { case (clear, i) =>
    val clearByFlush = (enqStatusVec(i).valid || validVec(i)) && flushedVec(i)
    val clearByResp = deqRespVec(i).valid && deqRespVec(i).bits.respType === RSFeedbackType.fuIdle

    clear := clearByFlush || clearByResp
  }

  val canIssueVec = Wire(Vec(params.numEntries, Bool()))
  canIssueVec.zipWithIndex.foreach { case (canIssue, i) =>
    canIssue := validVec(i) && statusVec(i).canIssue
  }

  io.valid := validVec.asUInt
  io.canIssue := canIssueVec.asUInt
  io.clear := clearVec.asUInt
  io.rsFeedback := 0.U.asTypeOf(io.rsFeedback)
  io.deq.zip(deqSelVec2).foreach { case (deqSingle, deqSelVecSingle) =>
    deqSingle.isFirstIssue := Mux1H(deqSelVecSingle, statusVec.map(!_.firstIssue))
  }
  dontTouch(io.deq)
}

class StatusArrayMem()(implicit p: Parameters, params: IssueBlockParams) extends StatusArray
  with HasCircularQueuePtrHelper {

  private val needMemFeedback = params.StaCnt > 0 || params.LduCnt > 0

  val fromMem = io.fromMem.get

  var memResps = resps
  if (needMemFeedback) {
    memResps ++= io.fromMem.get.slowResp
    memResps ++= io.fromMem.get.fastResp
  }
  deqRespVec.zipWithIndex.foreach { case (deqResp, i) =>
    val deqRespValidVec = VecInit(memResps.map(x => x.valid && x.bits.addrOH(i)))
    XSError(PopCount(deqRespValidVec) > 1.U, p"mem status deq resp ${Binary(deqRespValidVec.asUInt)} should be one-hot)\n")
    deqResp.valid := deqRespValidVec.asUInt.orR
    deqResp.bits := Mux1H(deqRespValidVec, memResps.map(_.bits))
  }

  clearVec.zipWithIndex.foreach { case (clear, i) =>
    val clearByFlush = (enqStatusVec(i).valid || validVec(i)) && flushedVec(i)
    val clearByResp = deqRespVec(i).valid && (
      //do: special mem success
      if(!needMemFeedback) {
        deqRespVec(i).bits.respType === RSFeedbackType.fuIdle
      }
      else{
        deqRespVec(i).bits.success
      }
    )
    clear := clearByFlush || clearByResp
  }

  statusNextVec.zip(statusVec).zipWithIndex.foreach { case ((statusNext, status), i) =>
    val memStatus = status.mem.get
    val memStatusNext = statusNext.mem.get
    // load cannot be issued before older store, unless meet some condition
    val blockedByOlderStore = isAfter(memStatusNext.sqIdx, fromMem.stIssuePtr)

    val deqFailedForStdInvalid = deqRespVec(i).valid && deqRespVec(i).bits.respType === RSFeedbackType.dataInvalid

    val staWaitedReleased = Cat(
      fromMem.memWaitUpdateReq.staIssue.map(x => x.valid && x.bits.uop.robIdx.value === memStatusNext.waitForRobIdx.value)
    ).orR
    val stdWaitedReleased = Cat(
      fromMem.memWaitUpdateReq.stdIssue.map(x => x.valid && x.bits.uop.sqIdx.value === memStatusNext.waitForSqIdx.value)
    ).orR
    val olderStaNotViolate = staWaitedReleased && !memStatusNext.strictWait
    val olderStdReady = stdWaitedReleased && memStatusNext.waitForStd
    val waitStd = !olderStdReady
    val waitSta = !olderStaNotViolate

    memStatusNext := memStatus
    when (enqStatusVec(i).valid) {
      // update by ssit at rename stage
      memStatusNext.strictWait    := enqStatusVec(i).bits.mem.get.strictWait
      // new load inst don't known if it is blocked by store data ahead of it
      memStatusNext.waitForStd    := false.B
      // update by lfst at dispatch stage
      memStatusNext.waitForRobIdx := enqStatusVec(i).bits.mem.get.waitForRobIdx
    }.elsewhen(deqFailedForStdInvalid) {
      // Todo: check if need assign statusNext.block
      memStatusNext.waitForStd    := true.B
      memStatusNext.waitForSqIdx  := deqRespVec(i).bits.dataInvalidSqIdx
    }.otherwise {
      // keep old value
    }

    val shouldBlock = Mux(enqStatusVec(i).valid, enqStatusVec(i).bits.blocked, status.blocked)
    val blockNotReleased = waitStd || waitSta
    val respBlock = deqFailedForStdInvalid
    statusNext.blocked := shouldBlock && blockNotReleased && blockedByOlderStore || respBlock
  }
}

object StatusArray {
  def apply(implicit p: Parameters, iqParams: IssueBlockParams): StatusArray = {
    iqParams.schdType match {
      case IntScheduler() => new StatusArray()
      case MemScheduler() =>
        if (iqParams.StdCnt == 0) new StatusArrayMem()
        else new StatusArray()
      case VfScheduler() => new StatusArray()
      case _ => null
    }
  }
}

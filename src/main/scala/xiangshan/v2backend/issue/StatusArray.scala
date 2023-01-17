package xiangshan.v2backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import utils.XSError
import xiangshan._
import xiangshan.backend.rob.RobPtr
import xiangshan.mem.{MemWaitUpdateReq, SqPtr}
import xiangshan.v2backend.Bundles.DynInst

class StatusEntry(implicit p:Parameters, params: IssueQueueParams) extends Bundle {
  val srcState = Vec(params.numSrc, SrcState())

  val psrc = Vec(params.numSrc, UInt(params.pregBits.W))
  val srcType = Vec(params.numSrc, SrcType())
  val robIdx = new RobPtr
  val ready = Bool()
  val issued = Bool()           // for predict issue
  val firstIssue = Bool()
  val blocked = Bool()          // for some block reason
  def srcReady: Bool = {
    VecInit(srcState.map(SrcState.isReady)).asUInt.andR
  }

  def canIssue: Bool = {
    ready && !issued && !blocked
  }
}

class StatusArrayEnqBundle(implicit p:Parameters, params: IssueQueueParams) extends Bundle {
  val addrOH = UInt(params.numEntries.W)
  val data = new StatusEntry()
}

class StatusArrayDeqRespBundle(implicit p:Parameters, params: IssueQueueParams) extends Bundle {
  val addrOH = UInt(params.numEntries.W)
  val success = Bool()
  val respType = RSFeedbackType()   // update credit if needs replay
  val dataInvalidSqIdx = new SqPtr
}

class StatusArrayDeqBundle(implicit p:Parameters, params: IssueQueueParams) extends Bundle {
  val isFirstIssue = Output(Bool())
  val deqSelOH = Flipped(ValidIO(UInt(params.numEntries.W)))
  val resp = Flipped(ValidIO(new StatusArrayDeqRespBundle))
}

class StatusArrayIO(implicit p: Parameters, params: IssueQueueParams) extends XSBundle {
  val flush = Flipped(ValidIO(new Redirect))
  // status
  val valid = Output(UInt(params.numEntries.W))
  val canIssue = Output(UInt(params.numEntries.W))
  val clear = Output(UInt(params.numEntries.W))
  // enq
  val enq = Vec(params.numEnq, Flipped(ValidIO(new StatusArrayEnqBundle)))
  // wakeup
  val wakeup = Vec(params.numWakeup, Flipped(ValidIO(new DynInst)))
  // deq
  val deq = Vec(params.numDeq, new StatusArrayDeqBundle)
  val stIssuePtr = if (params.checkWaitBit) Input(new SqPtr()) else null
  val memWaitUpdateReq = if (params.checkWaitBit) Flipped(new MemWaitUpdateReq) else null
  val rsFeedback = Output(Vec(5, Bool()))
}

class StatusArray()(implicit p: Parameters, params: IssueQueueParams) extends XSModule {
  val io = IO(new StatusArrayIO)

  val validVec = RegInit(VecInit(Seq.fill(params.numEntries)(false.B)))
  val statusVec = Reg(Vec(params.numEntries, new StatusEntry()))

  val validNextVec = Wire(Vec(params.numEntries, Bool()))
  val statusNextVec = Wire(Vec(params.numEntries, new StatusEntry()))

  val enqStatusVec = Wire(Vec(params.numEntries, ValidIO(new StatusEntry)))
  val srcWakeUpVec = Wire(Vec(params.numEntries, Vec(params.numSrc, Bool())))
  val deqRespVec = Wire(Vec(params.numEntries, ValidIO(new StatusArrayDeqRespBundle)))
  val flushedVec = Wire(Vec(params.numEntries, Bool()))
  val clearVec = Wire(Vec(params.numEntries, Bool()))
  val deqSelVec = Wire(Vec(params.numEntries, Bool()))

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
    validNext := enqStatusVec(i).valid || validVec(i)
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
      statusNext.issued := MuxCase (deqSelVec(i) || status.issued, Seq(
        (deqRespVec(i).valid && !deqRespVec(i).bits.success) -> false.B
      ))
      statusNext.ready := statusNext.srcReady || status.ready
      statusNext.robIdx := status.robIdx
      statusNext.srcType := status.srcType
      statusNext.firstIssue := status.firstIssue || deqSelVec(i)
    }
  }

  srcWakeUpVec.zipWithIndex.foreach { case (wakeups: Vec[Bool], i) =>
    // wakeupVec(i)(j): the ith psrc woken up by the jth bundle
    val wakeupVec: IndexedSeq[IndexedSeq[Bool]] = io.wakeup.map(bundle => bundle.bits.wakeUp(statusVec(i).psrc zip statusVec(i).srcType)).transpose
    wakeups := wakeupVec.map(VecInit(_).asUInt.orR)
  }

  deqSelVec.zipWithIndex.foreach { case (deqSel: Bool, i) =>
    deqSel := VecInit(io.deq.map(x => x.deqSelOH.valid && x.deqSelOH.bits(i))).asUInt.orR
  }

  deqRespVec.zipWithIndex.foreach { case (deqResp, i) =>
    val deqRespValidVec = VecInit(io.deq.map(x => x.resp.valid && x.resp.bits.addrOH(i)))
    XSError(PopCount(deqRespValidVec) > 1.U, p"status deq resp ${Binary(deqRespValidVec.asUInt)} should be one-hot)\n")
    deqResp.valid := deqRespValidVec.asUInt.orR
    deqResp.bits := Mux1H(deqRespValidVec, io.deq.map(_.resp.bits))
  }

  flushedVec.zipWithIndex.foreach { case (flushed, i) =>
    flushed := statusNextVec(i).robIdx.needFlush(io.flush)
  }

  // (1) flushed by rob
  // (2) deq success
  clearVec.zipWithIndex.foreach { case (clear, i) =>
    clear := (enqStatusVec(i).valid || validVec(i)) && flushedVec(i) ||
      deqRespVec(i).valid && deqRespVec(i).bits.success
  }

  val canIssueVec = Wire(Vec(params.numEntries, Bool()))
  canIssueVec.zipWithIndex.foreach { case (canIssue, i) =>
    canIssue := validVec(i) && statusVec(i).canIssue
  }

  io.valid := validVec.asUInt
  io.canIssue := canIssueVec.asUInt
  io.clear := clearVec.asUInt
  io.rsFeedback := 0.U.asTypeOf(io.rsFeedback)
  io.deq.foreach(_.isFirstIssue := Mux1H(deqSelVec, statusVec.map(!_.firstIssue)))
}

object StatusArray {
  def apply(implicit p: Parameters, iqParams: IssueQueueParams): StatusArray = {
    new StatusArray()
  }
}

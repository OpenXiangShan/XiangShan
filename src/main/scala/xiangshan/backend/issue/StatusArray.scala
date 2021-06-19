package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.roq.RoqPtr
import xiangshan.mem.SqPtr

class StatusArrayUpdateIO(config: RSConfig)(implicit p: Parameters) extends Bundle {
  val enable = Input(Bool())
  // should be one-hot
  val addr = Input(UInt(config.numEntries.W))
  val data = Input(new StatusEntry(config))

  def isLegal() = {
    PopCount(addr.asBools) === 0.U
  }

  override def cloneType: StatusArrayUpdateIO.this.type =
    new StatusArrayUpdateIO(config).asInstanceOf[this.type]
}

class StatusEntry(config: RSConfig)(implicit p: Parameters) extends XSBundle {
  // states
  val valid = Bool()
  val scheduled = Bool()
  val blocked = Bool()
  val credit = UInt(5.W)
  val replayCnt = UInt(2.W)
  val srcState = Vec(config.numSrc, Bool())
  // data
  val psrc = Vec(config.numSrc, UInt(config.dataIdBits.W))
  val srcType = Vec(config.numSrc, SrcType())
  val roqIdx = new RoqPtr
  val sqIdx = new SqPtr

  override def cloneType: StatusEntry.this.type =
    new StatusEntry(config).asInstanceOf[this.type]
  override def toPrintable: Printable = {
    p"$valid, $scheduled, ${Binary(srcState.asUInt)}, $psrc, $roqIdx"
  }
}

class StatusArray(config: RSConfig)(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    // current status
    val isValid = Output(UInt(config.numEntries.W))
    val canIssue = Output(UInt(config.numEntries.W))
    // enqueue, dequeue, wakeup, flush
    val update = Vec(config.numEnq, new StatusArrayUpdateIO(config))
    val wakeup = Vec(config.numWakeup, Flipped(ValidIO(new MicroOp)))
    val wakeupMatch = Vec(config.numEntries, Vec(config.numSrc, Output(UInt(config.numWakeup.W))))
    val issueGranted = Vec(config.numDeq, Flipped(ValidIO(UInt(config.numEntries.W))))
    val deqResp = Vec(config.numDeq, Flipped(ValidIO(new Bundle {
      val rsMask = UInt(config.numEntries.W)
      val success = Bool()
      val resptype = RSFeedbackType() // update credit if needs replay
    })))
    val stIssuePtr = if (config.checkWaitBit) Input(new SqPtr()) else null
  })

  val statusArray = Reg(Vec(config.numEntries, new StatusEntry(config)))
  val statusArrayNext = WireInit(statusArray)
  statusArray := statusArrayNext
  when (reset.asBool) {
    statusArray.map(_.valid := false.B)
  }

  // instruction is ready for issue
  val readyVec = VecInit(statusArray.map(s => s.srcState.asUInt.andR && !s.scheduled && !s.blocked))
  val readyVecNext = VecInit(statusArrayNext.map(s => s.srcState.asUInt.andR && !s.scheduled && !s.blocked))

  // update srcState when enqueue, wakeup
  def wakeupMatch(psrc: UInt, srcType: UInt) = {
    val matchVec = VecInit(io.wakeup.map(w =>
      w.valid && w.bits.pdest === psrc && (SrcType.isReg(srcType) && w.bits.ctrl.rfWen && psrc =/= 0.U || SrcType.isFp(srcType) && w.bits.ctrl.fpWen)
    ))
    XSError(PopCount(matchVec) > 1.U, p"matchVec ${Binary(matchVec.asUInt)} should be one-hot\n")
    matchVec.asUInt
  }
  def deqRespSel(i: Int) : (Bool, Bool) = {
    val mask = VecInit(io.deqResp.map(resp => resp.valid && resp.bits.rsMask(i)))
    XSError(PopCount(mask) > 1.U, p"feedbackVec ${Binary(mask.asUInt)} should be one-hot\n")
    val successVec = io.deqResp.map(_.bits.success)
    (mask.asUInt.orR, Mux1H(mask, successVec))
  }
  for (((status, statusNext), i) <- statusArray.zip(statusArrayNext).zipWithIndex) {
    val selVec = VecInit(io.update.map(u => u.enable && u.addr(i)))
    XSError(PopCount(selVec) > 1.U, "should not update the same entry\n")
    val updateEn = selVec.asUInt.orR

    when (updateEn) {
      val updateStatus = Mux1H(selVec, io.update.map(_.data))
      val wakeupEnVec = VecInit(updateStatus.psrc.zip(updateStatus.srcType).map{ case (p, t) => wakeupMatch(p, t) })
      val wakeupEn = wakeupEnVec.map(_.orR)
      io.wakeupMatch(i) := wakeupEnVec
      statusNext.valid := true.B
      statusNext.srcState := VecInit(updateStatus.srcState.zip(wakeupEn).map {
        case (update, wakeup) => update || wakeup
      })
      statusNext.scheduled := updateStatus.scheduled
      statusNext.blocked := updateStatus.blocked
      statusNext.credit := updateStatus.credit
      statusNext.replayCnt := 0.U // updateStatus.replayCnt
      statusNext.psrc := updateStatus.psrc
      statusNext.srcType := updateStatus.srcType
      statusNext.roqIdx := updateStatus.roqIdx
      statusNext.sqIdx := updateStatus.sqIdx
      XSError(status.valid, p"should not update a valid entry\n")
    }.otherwise {
      val hasIssued = VecInit(io.issueGranted.map(iss => iss.valid && iss.bits(i))).asUInt.orR
      val (deqResp, deqGrant) = deqRespSel(i)
      XSError(deqResp && !status.valid, "should not deq an invalid entry\n")
      if (config.hasFeedback) {
        XSError(deqResp && !status.scheduled, "should not deq an un-scheduled entry\n")
      }
      val wakeupEnVec = VecInit(status.psrc.zip(status.srcType).map{ case (p, t) => wakeupMatch(p, t) })
      val wakeupEn = wakeupEnVec.map(_.orR)
      io.wakeupMatch(i) := wakeupEnVec

      // calculate credit
      // Max(credit) >= sizeof(RS) to avoid deadlock
      val tlbMissCredit = VecInit(Seq(5.U,5.U,10.U,20.U))
      val mshrFullCredit = VecInit(Seq(5.U,5.U,10.U,20.U))
      val dataInvalidCredit = VecInit(Seq(1.U,1.U,1.U,1.U)) // must >= 1 to set scheduled bit
      val nextCredit = Mux1H(Seq(
        (io.deqResp(0).bits.resptype === RSFeedbackType.tlbMiss)     -> tlbMissCredit(status.replayCnt),
        (io.deqResp(0).bits.resptype === RSFeedbackType.mshrFull)    -> mshrFullCredit(status.replayCnt),
        (io.deqResp(0).bits.resptype === RSFeedbackType.dataInvalid) -> dataInvalidCredit(status.replayCnt),
        (io.deqResp(0).bits.resptype === RSFeedbackType.normal)      -> 0.U,
      ))
      // TODO: parameterize max(replayCnt)
      statusNext.replayCnt := Mux(
        deqResp && !deqGrant && status.replayCnt =/= 3.U, 
        status.replayCnt + 1.U, 
        status.replayCnt
      )

      statusNext.valid := Mux(
        deqResp && deqGrant,
        false.B,
        status.valid && !status.roqIdx.needFlush(io.redirect, io.flush)
      )
      // (1) when deq is not granted, set its scheduled bit and wait; (2) set scheduled if issued
      statusNext.scheduled := Mux(
        deqResp && !deqGrant,
        nextCredit >= 1.U, // wait if status.credit > 1
        Mux(status.credit === 1.U, false.B, status.scheduled || hasIssued)
      )
      XSError(hasIssued && !status.valid, "should not issue an invalid entry\n")
      if (config.checkWaitBit) {
        statusNext.blocked := status.blocked && isAfter(status.sqIdx, io.stIssuePtr)
        when(io.deqResp(0).fire() && io.deqResp(0).bits.resptype === RSFeedbackType.dataInvalid) {
          statusNext.blocked := true.B
        }  
      }
      else {
        statusNext.blocked := false.B
      }
      statusNext.credit := Mux(
        deqResp && !deqGrant,
        nextCredit, // update credit if deq is not granted
        Mux(status.credit > 0.U, status.credit - 1.U, status.credit)
      )
      XSError(status.valid && status.credit > 0.U && !status.scheduled,
        p"instructions $i with credit ${status.credit} must not be scheduled\n")
      statusNext.srcState := VecInit(status.srcState.zip(wakeupEn).map {
        case (current, wakeup) => current || wakeup
      })
    }

    XSDebug(status.valid, p"entry[$i]: $status\n")
  }

  io.isValid := VecInit(statusArray.map(_.valid)).asUInt
  io.canIssue := VecInit(statusArray.map(_.valid).zip(readyVec).map{ case (v, r) => v && r}).asUInt
}

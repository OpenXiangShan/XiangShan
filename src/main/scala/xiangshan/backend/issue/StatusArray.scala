package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.roq.RoqPtr

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
  val valid = Bool()
  val scheduled = Bool()
  val srcState = Vec(config.numSrc, Bool())
  val psrc = Vec(config.numSrc, UInt(config.dataIdBits.W))
  val srcType = Vec(config.numSrc, SrcType())
  val roqIdx = new RoqPtr

  override def cloneType: StatusEntry.this.type =
    new StatusEntry(config).asInstanceOf[this.type]
  override def toPrintable: Printable = {
    p"$valid, $scheduled, ${Binary(srcState.asUInt)}, $psrc, $roqIdx"
  }
}

class StatusArray(config: RSConfig)(implicit p: Parameters) extends XSModule {
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
  })

  val statusArray = Reg(Vec(config.numEntries, new StatusEntry(config)))
  val statusArrayNext = WireInit(statusArray)
  statusArray := statusArrayNext
  when (reset.asBool) {
    statusArray.map(_.valid := false.B)
  }

  // instruction is ready for issue
  val readyVec = VecInit(statusArray.map(_.srcState.asUInt.andR))
  val readyVecNext = VecInit(statusArrayNext.map(_.srcState.asUInt.andR))

  // update srcState when enqueue, wakeup
  def wakeupMatch(psrc: UInt, srcType: UInt) = {
    val matchVec = VecInit(io.wakeup.map(w => w.valid && w.bits.pdest === psrc && SrcType.isReg(srcType)))
    XSError(PopCount(matchVec) > 1.U, p"matchVec ${Binary(matchVec.asUInt)} should be one-hot\n")
    matchVec.asUInt
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
      statusNext.valid := !updateStatus.roqIdx.needFlush(io.redirect, io.flush)
      statusNext.srcState := VecInit(updateStatus.srcState.zip(wakeupEn).map {
        case (update, wakeup) => update || wakeup
      })
      statusNext.psrc := updateStatus.psrc
      statusNext.srcType := updateStatus.srcType
      statusNext.roqIdx := updateStatus.roqIdx
      XSError(status.valid, p"should not update a valid entry\n")
    }.otherwise {
      val hasIssued = VecInit(io.issueGranted.map(iss => iss.valid && iss.bits(i))).asUInt.orR
      val wakeupEnVec = VecInit(status.psrc.zip(status.srcType).map{ case (p, t) => wakeupMatch(p, t) })
      val wakeupEn = wakeupEnVec.map(_.orR)
      io.wakeupMatch(i) := wakeupEnVec
      statusNext.valid := status.valid && !status.roqIdx.needFlush(io.redirect, io.flush) && !hasIssued
      statusNext.srcState := VecInit(status.srcState.zip(wakeupEn).map {
        case (current, wakeup) => current || wakeup
      })
    }

    XSDebug(status.valid, p"entry[$i]: $status\n")
  }

  io.isValid := VecInit(statusArray.map(_.valid)).asUInt
  io.canIssue := VecInit(statusArray.map(_.valid).zip(readyVec).map{ case (v, r) => v && r}).asUInt
}

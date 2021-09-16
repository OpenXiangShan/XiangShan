/***************************************************************************************
* Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
* Copyright (c) 2020-2021 Peng Cheng Laboratory
*
* XiangShan is licensed under Mulan PSL v2.
* You can use this software according to the terms and conditions of the Mulan PSL v2.
* You may obtain a copy of Mulan PSL v2 at:
*          http://license.coscl.org.cn/MulanPSL2
*
* THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
* EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
* MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
*
* See the Mulan PSL v2 for more details.
***************************************************************************************/

package xiangshan.backend.issue

import chipsalliance.rocketchip.config.Parameters
import chisel3._
import chisel3.util._
import xiangshan._
import utils._
import xiangshan.backend.roq.RoqPtr
import xiangshan.mem.SqPtr

class StatusArrayUpdateIO(params: RSParams)(implicit p: Parameters) extends Bundle {
  val enable = Input(Bool())
  // should be one-hot
  val addr = Input(UInt(params.numEntries.W))
  val data = Input(new StatusEntry(params))

  def isLegal() = {
    PopCount(addr.asBools) === 0.U
  }

  override def cloneType: StatusArrayUpdateIO.this.type =
    new StatusArrayUpdateIO(params).asInstanceOf[this.type]
}

class StatusEntry(params: RSParams)(implicit p: Parameters) extends XSBundle {
  // states
  val valid = Bool()
  val scheduled = Bool()
  val blocked = Bool()
  val credit = UInt(4.W)
  val srcState = Vec(params.numSrc, Bool())
  // data
  val psrc = Vec(params.numSrc, UInt(params.dataIdBits.W))
  val srcType = Vec(params.numSrc, SrcType())
  val roqIdx = new RoqPtr
  val sqIdx = new SqPtr
  // misc
  val isFirstIssue = Bool()

  def canIssue: Bool = {
    val scheduledCond = if (params.needScheduledBit) !scheduled else true.B
    val blockedCond = if (params.checkWaitBit) !blocked else true.B
    srcState.asUInt.andR && scheduledCond && blockedCond
  }

  override def cloneType: StatusEntry.this.type =
    new StatusEntry(params).asInstanceOf[this.type]
  override def toPrintable: Printable = {
    p"$valid, $scheduled, ${Binary(srcState.asUInt)}, $psrc, $roqIdx"
  }
}

class StatusArray(params: RSParams)(implicit p: Parameters) extends XSModule
  with HasCircularQueuePtrHelper {
  val io = IO(new Bundle {
    val redirect = Flipped(ValidIO(new Redirect))
    val flush = Input(Bool())
    // current status
    val isValid = Output(UInt(params.numEntries.W))
    val canIssue = Output(UInt(params.numEntries.W))
    val flushed = Output(UInt(params.numEntries.W))
    // enqueue, dequeue, wakeup, flush
    val update = Vec(params.numEnq, new StatusArrayUpdateIO(params))
    val wakeup = Vec(params.allWakeup, Flipped(ValidIO(new MicroOp)))
    val wakeupMatch = Vec(params.numEntries, Vec(params.numSrc, Output(UInt(params.allWakeup.W))))
    val issueGranted = Vec(params.numDeq, Flipped(ValidIO(UInt(params.numEntries.W))))
    // TODO: if more info is needed, put them in a bundle
    val isFirstIssue = Vec(params.numDeq, Output(Bool()))
    val deqResp = Vec(params.numDeq, Flipped(ValidIO(new Bundle {
      val rsMask = UInt(params.numEntries.W)
      val success = Bool()
      val resptype = RSFeedbackType() // update credit if needs replay
    })))
    val stIssuePtr = if (params.checkWaitBit) Input(new SqPtr()) else null
  })

  val statusArray = Reg(Vec(params.numEntries, new StatusEntry(params)))
  val statusArrayNext = WireInit(statusArray)
  statusArray := statusArrayNext
  when (reset.asBool) {
    statusArray.map(_.valid := false.B)
  }

  // instruction is ready for issue
  val readyVec = VecInit(statusArray.map(_.canIssue))
  val readyVecNext = VecInit(statusArrayNext.map(_.canIssue))

  // update srcState when enqueue, wakeup
  // For better timing, we use different conditions for data write and srcState update
  def wakeupMatch(srcInfo: (UInt, UInt)): (Bool, UInt) = {
    val (psrc, srcType) = srcInfo
    val (stateMatchVec, dataMatchVec) = io.wakeup.map(w => {
      val pdestMatch = w.valid && w.bits.pdest === psrc
      val rfStateMatch = if (params.exuCfg.get.readIntRf) w.bits.ctrl.rfWen else false.B
      val rfDataMatch = if (params.exuCfg.get.readIntRf) w.bits.ctrl.rfWen && psrc =/= 0.U else false.B
      val fpMatch = if (params.exuCfg.get.readFpRf) w.bits.ctrl.fpWen else false.B
      // For state condition: only pdest is used for matching.
      // If the exu needs both int and fp sources, we need to check which type of source it is.
      // Otherwise, no need to check the source type (does not matter if it is imm).
      val bothIntFp = params.exuCfg.get.readIntRf && params.exuCfg.get.readFpRf
      val bothStateMatch = (rfStateMatch && !SrcType.regIsFp(srcType)) || (fpMatch && SrcType.regIsFp(srcType))
      val stateCond = pdestMatch && (if (bothIntFp) bothStateMatch else rfStateMatch || fpMatch)
      // For data condition: types are matched and int pdest is not $zero.
      val bothDataMatch = (rfDataMatch && SrcType.isReg(srcType)) || (fpMatch && SrcType.isFp(srcType))
      val dataCond = pdestMatch && bothDataMatch
      (stateCond, dataCond)
    }).unzip
    val stateMatch = VecInit(stateMatchVec).asUInt.orR
    val dataMatch = VecInit(dataMatchVec).asUInt
    XSError(PopCount(dataMatchVec) > 1.U, p"matchVec ${Binary(dataMatch)} should be one-hot\n")
    (stateMatch, dataMatch)
  }

  def deqRespSel(i: Int) : (Bool, Bool, UInt) = {
    val mask = VecInit(io.deqResp.map(resp => resp.valid && resp.bits.rsMask(i)))
    XSError(PopCount(mask) > 1.U, p"feedbackVec ${Binary(mask.asUInt)} should be one-hot\n")
    val deqValid = mask.asUInt.orR
    XSError(deqValid && !statusArray(i).valid, p"should not deq an invalid entry $i\n")
    if (params.hasFeedback) {
      XSError(deqValid && !statusArray(i).scheduled, p"should not deq an un-scheduled entry $i\n")
    }
    val successVec = io.deqResp.map(_.bits.success)
    val respTypeVec = io.deqResp.map(_.bits.resptype)
    (mask.asUInt.orR, Mux1H(mask, successVec), Mux1H(mask, respTypeVec))
  }

  def enqUpdate(i: Int): (Bool, StatusEntry) = {
    val updateVec = VecInit(io.update.map(u => u.enable && u.addr(i)))
    val updateStatus = Mux1H(updateVec, io.update.map(_.data))
    XSError(PopCount(updateVec) > 1.U, "should not update the same entry\n")
    (updateVec.asUInt.orR, updateStatus)
  }

  val flushedVec = Wire(Vec(params.numEntries, Bool()))

  val (updateValid, updateVal) = statusArray.indices.map(enqUpdate).unzip
  val deqResp = statusArray.indices.map(deqRespSel)

  for (((status, statusNext), i) <- statusArray.zip(statusArrayNext).zipWithIndex) {
    // valid: when the entry holds a valid instruction, mark it true.
    // Set when (1) not (flushed or deq); AND (2) update.
    val isFlushed = status.valid && status.roqIdx.needFlush(io.redirect, io.flush)
    val (deqRespValid, deqRespSucc, deqRespType) = deqResp(i)
    flushedVec(i) := isFlushed || (deqRespValid && deqRespSucc)
    val realUpdateValid = updateValid(i) && !io.redirect.valid && !io.flush
    statusNext.valid := !flushedVec(i) && (realUpdateValid || status.valid)
    XSError(updateValid(i) && status.valid, p"should not update a valid entry $i\n")

    // scheduled: when the entry is scheduled for issue, mark it true.
    // Set when (1) scheduled for issue; (2) enq blocked.
    // Reset when (1) deq is not granted (it needs to be scheduled again); (2) only one credit left.
    val hasIssued = VecInit(io.issueGranted.map(iss => iss.valid && iss.bits(i))).asUInt.orR
    val deqNotGranted = deqRespValid && !deqRespSucc
    statusNext.scheduled := false.B
    if (params.needScheduledBit) {
      // An entry keeps in the scheduled state until its credit comes to zero or deqFailed.
      val noCredit = status.valid && status.credit === 1.U
      val keepScheduled = status.scheduled && !deqNotGranted && !noCredit
      statusNext.scheduled := Mux(updateValid(i), updateVal(i).scheduled, hasIssued || keepScheduled)
    }
    XSError(hasIssued && !status.valid, p"should not issue an invalid entry $i\n")

    // blocked: indicate whether the entry is blocked for issue until certain conditions meet.
    statusNext.blocked := false.B
    if (params.checkWaitBit) {
      val blockReleased = isAfter(statusNext.sqIdx, io.stIssuePtr)
      statusNext.blocked := Mux(updateValid(i), updateVal(i).blocked, status.blocked) && blockReleased
      when (deqNotGranted && deqRespType === RSFeedbackType.dataInvalid) {
        statusNext.blocked := true.B
        XSError(status.valid && !isAfter(status.sqIdx, RegNext(RegNext(io.stIssuePtr))),
          "Previous store instructions are all issued. Should not trigger dataInvalid.\n")
      }
    }

    // credit: the number of cycles this entry needed until it can be scheduled
    val creditStep = Mux(status.credit > 0.U, status.credit - 1.U, status.credit)
    statusNext.credit := Mux(updateValid(i), updateVal(i).credit, creditStep)
    XSError(status.valid && status.credit > 0.U && !status.scheduled,
      p"instructions $i with credit ${status.credit} must not be scheduled\n")

    // srcState: indicate whether the operand is ready for issue
    val (stateWakeupEn, dataWakeupEnVec) = statusNext.psrc.zip(statusNext.srcType).map(wakeupMatch).unzip
    io.wakeupMatch(i) := dataWakeupEnVec
    // For best timing of srcState, we don't care whether the instruction is valid or not.
    // We also don't care whether the instruction can really enqueue.
    val updateSrcState = updateVal(i).srcState
    val wakeupSrcState = stateWakeupEn
    statusNext.srcState := VecInit(status.srcState.zip(updateSrcState).zip(wakeupSrcState).map {
      // When the instruction enqueues, we always use the wakeup result.
      case ((current, update), wakeup) => wakeup || Mux(updateValid(i), update, current)
    })

    // static data fields (only updated when instructions enqueue)
    statusNext.psrc := Mux(updateValid(i), updateVal(i).psrc, status.psrc)
    statusNext.srcType := Mux(updateValid(i), updateVal(i).srcType, status.srcType)
    statusNext.roqIdx := Mux(updateValid(i), updateVal(i).roqIdx, status.roqIdx)
    statusNext.sqIdx := Mux(updateValid(i), updateVal(i).sqIdx, status.sqIdx)

    // isFirstIssue: indicate whether the entry has been issued before
    // When the entry is not granted to leave the RS, set isFirstIssue to false.B
    statusNext.isFirstIssue := Mux(deqNotGranted, false.B, updateValid(i) || status.isFirstIssue)

    XSDebug(status.valid, p"entry[$i]: $status\n")
  }

  io.isValid := VecInit(statusArray.map(_.valid)).asUInt
  io.canIssue := VecInit(statusArrayNext.map(_.valid).zip(readyVecNext).map{ case (v, r) => v && r}).asUInt
  io.isFirstIssue := VecInit(io.issueGranted.map(iss => Mux1H(iss.bits, statusArray.map(_.isFirstIssue))))
  io.flushed := flushedVec.asUInt

  val validEntries = PopCount(statusArray.map(_.valid))
  XSPerfHistogram("valid_entries", validEntries, true.B, 0, params.numEntries, 1)
  for (i <- 0 until params.numSrc) {
    val waitSrc = statusArray.map(_.srcState).map(s => Cat(s.zipWithIndex.filter(_._2 != i).map(_._1)).andR && !s(i))
    val srcBlockIssue = statusArray.zip(waitSrc).map{ case (s, w) => s.valid && !s.scheduled && !s.blocked && w }
    XSPerfAccumulate(s"wait_for_src_$i", PopCount(srcBlockIssue))
    for (j <- 0 until params.allWakeup) {
      val wakeup_j_i = io.wakeupMatch.map(_(i)(j)).zip(statusArray.map(_.valid)).map(p => p._1 && p._2)
      XSPerfAccumulate(s"wakeup_${j}_$i", PopCount(wakeup_j_i).asUInt)
      val criticalWakeup = srcBlockIssue.zip(wakeup_j_i).map(x => x._1 && x._2)
      XSPerfAccumulate(s"critical_wakeup_${j}_$i", PopCount(criticalWakeup))
      // For FMAs only: critical_wakeup from fma instructions (to fma instructions)
      if (i == 2 && j < 2 * exuParameters.FmacCnt) {
        val isFMA = io.wakeup(j).bits.ctrl.fpu.ren3
        XSPerfAccumulate(s"critical_wakeup_from_fma_${j}", Mux(isFMA, PopCount(criticalWakeup), 0.U))
      }
    }
  }
  val canIssueEntries = PopCount(io.canIssue)
  XSPerfHistogram("can_issue_entries", canIssueEntries, true.B, 0, params.numEntries, 1)
  val isBlocked = PopCount(statusArray.map(s => s.valid && s.blocked))
  XSPerfAccumulate("blocked_entries", isBlocked)
  val isScheduled = PopCount(statusArray.map(s => s.valid && s.scheduled))
  XSPerfAccumulate("scheduled_entries", isScheduled)
  val notSelected = PopCount(io.canIssue) - PopCount(io.issueGranted.map(_.valid))
  XSPerfAccumulate("not_selected_entries", notSelected)
  val isReplayed = PopCount(io.deqResp.map(resp => resp.valid && !resp.bits.success))
  XSPerfAccumulate("replayed_entries", isReplayed)
}

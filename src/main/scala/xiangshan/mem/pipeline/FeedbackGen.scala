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

package xiangshan.mem

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.util._
import utils._
import utility._
import xiangshan._
import xiangshan.ExceptionNO._
import xiangshan.backend.fu._
import xiangshan.backend.fu.NewCSR._
import xiangshan.backend.fu.FuConfig._
import xiangshan.backend.Bundles._
import xiangshan.backend.datapath._
import xiangshan.mem.ReplayCauseNO._
import xiangshan.mem.Bundles._
import xiangshan.cache.mmu._

class FeedbackGenIO()(implicit p: Parameters, params: MemUnitParams) extends XSBundle {
  val fromCtrl = new Bundle() {
    val redirect = Flipped(ValidIO(new Redirect))
    val trigger = Input(new CsrTriggerBundle)
  }

  val s0_in = Flipped(ValidIO(new LsPipelineBundle))
  val s1_in = Flipped(ValidIO(new LsPipelineBundle))
  val s2_in = Flipped(ValidIO(new LsPipelineBundle))
  val s3_in = Flipped(DecoupledIO(new LsPipelineBundle))

  // from
  val fromTlb = Flipped(ValidIO(new TlbResp(2)))
  val fromPmp = Flipped(new PMPRespBundle())
  val fromLsq = new Bundle() {
    val mmioLdWriteback = Flipped(ValidIO(new MemExuOutput))
  }

  // to
  val toBackend = new Bundle() {
    val wakeup = ValidIO(new DynInst)
    val stIssue = ValidIO(new MemExuInput)
    val ldCancel = Output(new LoadCancelIO())
    val iqFeedback = new MemRSFeedbackIO
    val rollback = ValidIO(new Redirect)
    val writeback = params.genLsPipelineSourceDecoupledBundle
  }
  val toLoadMisalignBuf = new MemUnitToMisalignBufferIO
  val toStoreMisalignBuf = new MemUnitToMisalignBufferIO
  val toLsq = DecoupledIO(new LsPipelineBundle)
  val toLdu = new Bundle() {
    val replay = DecoupledIO(new LsPipelineBundle)
  }

  // common
  val commonIn = Input(new Bundle() {
    val s3_wbPort = Vec(params.numWBPorts, Bool())
    val s3_data = UInt(VLEN.W)
    val s3_misalignData = UInt(VLEN.W)
    val s3_vecData = UInt(VLEN.W)
    val s3_canFastReplay = Bool()
    val s3_fastReplayCancelled = Bool()
    val s3_flushPipe = Bool()
    val s3_replayInst = Bool()
    val s3_fromMisalignFlush = Bool()
    val s3_hasException = Bool()
  })
}

class FeedbackGen(implicit p: Parameters, params: MemUnitParams) extends XSModule with HasCircularQueuePtrHelper {

  private val SelectGroupSize = RollbackGroupSize
  private val lgSelectGroupSize = log2Ceil(SelectGroupSize)
  private val TotalSelectCycles = scala.math.ceil(log2Ceil(LoadQueueRAWSize).toFloat / lgSelectGroupSize).toInt + 1
  private val TotalDelayCycles = TotalSelectCycles - 2


  val io = IO(new FeedbackGenIO)

  private val fromCtrl = io.fromCtrl
  private val fromTlb = io.fromTlb
  private val fromPmp = io.fromPmp
  private val toBackend = io.toBackend
  private val toLoadMisalignBuf = io.toLoadMisalignBuf
  private val toStoreMisalignBuf = io.toStoreMisalignBuf
  private val (fromLsq, toLsq) = (io.fromLsq, io.toLsq)
  private val toLdu = io.toLdu
  private val commonIn = io.commonIn

  // Pipeline
  // -------------------------------------------------------------------
  // stage 0
  // -------------------------------------------------------------------
  val s0_in = io.s0_in

  /**
   * Determines whether there is no need to wake up in s0 (`s0_noNeedWakeup`).
   * Conditions:
   * 1. The uop in s0 is a vector uop (`s0_in.bits.isVector`).
   * 2. The uop in s0 is a hardware prefetch (`s0_in.bits.isHWPrefetch`).
   * 3. The uop is in the misalign buffer, but does not require a wakeup (`s0_in.bits.isMisalignBuf && !s0_in.bits.misalignNeedWakeUp`).
   */
  val s0_noNeedWakeup = s0_in.bits.isVector || s0_in.bits.isHWPrefetch || s0_in.bits.isMisalignBuf && !s0_in.bits.misalignNeedWakeUp

  /**
    * replay from lq, lda/hya and misalign buffer need to wakeup other consumers.
    */
  val s0_replayWakeup = s0_in.bits.isLoadReplay || s0_in.bits.isFastReplay || s0_in.bits.isMisalignBuf

  /**
    * issue from misalign and need wakeup
    */
  val s0_misalignWakeup = s0_in.bits.isMisalignBuf && s0_in.bits.misalignNeedWakeUp

  /**
    * mmio and non-cacheable requests also need to wakeup other consumers.
    */
  val s0_canSafeWakeup = (s0_replayWakeup || s0_in.bits.isMmio || s0_in.bits.isNoncacheable || s0_misalignWakeup) &&
    !s0_noNeedWakeup

  toBackend.wakeup.valid := s0_in.fire && s0_canSafeWakeup
  toBackend.wakeup.bits := s0_in.bits.uop

  // Pipeline
  // -------------------------------------------------------------------
  // stage 1
  // -------------------------------------------------------------------
  val s1_in = io.s1_in

  // to Backend
  toBackend.stIssue.valid := s1_in.valid && !fromTlb.bits.miss && s1_in.bits.isIq
  toBackend.stIssue.bits  := s1_in.bits.toMemExuInputBundle()

  /**
    * Send TLB feedback to store issue queue (only for store). Store feedback is generated in s1, sent to Iq in s2
    */
  val s1_iqFeedback = Wire(Valid(new RSFeedback))
  val s1_iqFeedbackCanGo = s1_iqFeedback.valid && !s1_in.bits.isVector && !s1_in.bits.isMisalignBuf
  s1_iqFeedback.valid := s1_in.valid && s1_in.bits.isIq && s1_in.bits.isStore
  s1_iqFeedback.bits.hit := !fromTlb.bits.miss
  s1_iqFeedback.bits.flushState := fromTlb.bits.ptwBack
  s1_iqFeedback.bits.robIdx := s1_in.bits.uop.robIdx
  s1_iqFeedback.bits.sourceType := RSFeedbackType.tlbMiss
  s1_iqFeedback.bits.sqIdx := s1_in.bits.uop.sqIdx
  s1_iqFeedback.bits.lqIdx := s1_in.bits.uop.lqIdx
  s1_iqFeedback.bits.dataInvalidSqIdx := DontCare
  s1_iqFeedback.bits.isLoad := s1_in.bits.isLoad

  // to Backend
  // IssueQueue ld1Cancel
  toBackend.ldCancel.ld1Cancel := false.B

  // Pipeline
  // -------------------------------------------------------------------
  // stage 2
  // -------------------------------------------------------------------
  val s2_in = io.s2_in

  /**
   * Determines whether the store misalignment buffer should be valid (`toStoreMisalignBuf.valid`).
   * Conditions:
   * 1. The input in s2 is valid (`s2_in.valid`).
   * 2. The uop in s2 is misaligned (`s2_in.bits.misalign`).
   * 3. The uop is not issued by the misalign buffer (`!s2_in.bits.isMisalignBuf`).
   * 4. The uop is not an MMIO uop (`!s2_in.bits.mmio`).
   * 5. The uop is a store (`s2_in.bits.isStore`).
   */
  toStoreMisalignBuf.enq.valid := s2_in.valid && s2_in.bits.misalign && !s2_in.bits.isMisalignBuf &&
    !s2_in.bits.mmio && s2_in.bits.isStore
  toStoreMisalignBuf.enq.bits := s2_in.bits

  //  store feedback
  val storeMisalignBufCanAccept = toStoreMisalignBuf.enq.valid && !toStoreMisalignBuf.enq.ready
  toBackend.iqFeedback.feedbackFast.valid := GatedValidRegNext(s1_iqFeedbackCanGo)
  toBackend.iqFeedback.feedbackFast.bits := RegEnable(s1_iqFeedback.bits, s1_iqFeedbackCanGo)
  toBackend.iqFeedback.feedbackFast.bits.hit := RegEnable(s1_iqFeedback.bits.hit, s1_iqFeedbackCanGo) &&
    storeMisalignBufCanAccept

  //  to MisalignBuf writeback
  if (toStoreMisalignBuf.out.isDefined) {
    toStoreMisalignBuf.out.get.valid := s2_in.valid && s2_in.bits.isMisalignBuf && s2_in.bits.isStore
    toStoreMisalignBuf.out.get.bits := s2_in.bits
  }

  /**
   * Determines whether the safe wakeup condition is met for stage 2 (`s2_safeWakeup`).
   * Conditions:
   * 1. The uop in s2 does not require a replay (`!s2_in.needReplay`).
   * 2. The uop is an MMIO (Memory-Mapped I/O) uop (`s2_in.bits.mmio`).
   * 3. The uop is not misaligned (`!s2_in.bits.misalign`).
   * 4. The uop is load (`s2_in.bits.isLoad`).
   * 5. The uop is either non-cacheable or explicitly marked as non-cacheable
   *    (`!s2_in.bits.isNoncacheable || s2_in.bits.isNoncacheable`).
   * 6. The debug mode is triggered (`fromCtrl.triggerDebugMode`) or there is an exception
   *    present (`s2_in.bits.hasException`).
   * 7. The vector uop is active (`s2_in.bits.vecActive`).
   */
  val s2_safeWakeup = !s2_in.bits.needReplay && s2_in.bits.mmio && !s2_in.bits.misalign && s2_in.bits.isLoad
    (!s2_in.bits.isNoncacheable || s2_in.bits.isNoncacheable) &&
    (fromCtrl.trigger.debugMode || s2_in.bits.hasException) && s2_in.bits.vecActive

  // Pipeline
  // -------------------------------------------------------------------
  // stage 3
  // -------------------------------------------------------------------
  val s3_in = io.s3_in
  val s3_writeback = Wire(DecoupledIO(new LsPipelineBundle))
  s3_writeback.valid := s3_in.valid || RegNextN(fromLsq.mmioLdWriteback.valid && s0_in.bits.isMmio, 3, Some(false.B))
  s3_writeback.bits.fromMemExuOutputBundle(RegNextN(fromLsq.mmioLdWriteback.bits, 3))
  s3_in.ready := s3_writeback.ready

  // to Backend
  val s3_safeWakeup = RegEnable(s2_safeWakeup, s2_in.valid)

  /**
   * Determines whether the load cancel signal should be set for s3 (`toBackend.ldCancel.ld2Cancel`).
   * The load uop should cancel coonsumers if:
   * 1. The input in s3 is valid (`s3_in.valid`).
   * 2. The safe wakeup condition is not set (`!s3_safeWakeup`).
   * 3. The uop in s3 is not a vector uop (`!s3_in.isVector`).
   * 4. The uop is either not in the misalign buffer or requires a wakeup
   *    (`!s3_in.bits.isMisalignBuf || s3_in.bits.misalignNeedWakeUp`).
   */
  toBackend.ldCancel.ld2Cancel := s3_in.valid && !s3_safeWakeup && !s3_in.bits.isVector &&
    (!s3_in.bits.isMisalignBuf|| s3_in.bits.misalignNeedWakeUp)

  // fast replay
  val s3_flushPipe = commonIn.s3_flushPipe
  val s3_replayInst = commonIn.s3_replayInst
  val s3_fromMisalignFlush = commonIn.s3_fromMisalignFlush
  val s3_hasException = commonIn.s3_hasException
  val s3_canFastReplay = commonIn.s3_canFastReplay
  val s3_fastReplayCanclled = commonIn.s3_fastReplayCancelled

  toLdu.replay.valid := s3_in.valid && s3_canFastReplay
  toLdu.replay.bits := s3_in.bits
  toLdu.replay.bits.delayedError := s3_in.bits.delayedError

  // to load misalign buffer enq
  /**
   * Determines whether the load misalignment buffer (`toLoadMisalignBuf.enq.valid`).
   * Conditions:
   * 1. The input in s3 is valid (`s3_in.valid`).
   * 2. The uop in s3 is misaligned (`s3_in.bits.misalign`).
   * 3. The uop is not already in the misalign buffer (`!s3_in.bits.isMisalignBuf`).
   * 4. The uop in stage 2 is a load uop (`s2_in.bits.isLoad`).
   */
  toLoadMisalignBuf.enq.valid := s3_in.valid && s3_in.bits.misalign && !s3_in.bits.isMisalignBuf && s2_in.bits.isLoad
  toLoadMisalignBuf.enq.bits := s3_in.bits

  // to load misalign buffer writeback
  /**
   * Determines whether the load misalignment buffer (`toLoadMisalignBuf.out.valid`).
   * Conditions:
   * 1. The input in s3 is valid (`s3_in.valid`).
   * 2. This condition checks that either fast replay is not allowed ('!s3_canFastReplay')
   *    or the fast replay mechanism has been canceled ('s3_fastReplayCanclled').
   * 3. The uop is not already in the misalign buffer (`!s3_in.bits.isMisalignBuf`).
   * 4. The uop in stage 2 is a load uop (`s2_in.bits.isLoad`).
   */
  if (toLoadMisalignBuf.out.isDefined) {
    toLoadMisalignBuf.out.get.valid := s3_in.valid && (!s3_canFastReplay || s3_fastReplayCanclled) && s3_in.bits.isMisalignBuf
    toLoadMisalignBuf.out.get.bits := s3_in.bits
    toLoadMisalignBuf.out.get.bits.data := commonIn.s3_misalignData
  }

  // to Lsq
  /**
    * Determine if the LSQ (Load-Store Queue) input is valid in s3.
    * The input is valid if:
    * - The input to s3 is valid (`s3_in.valid`),
    * - The uop is a load (`s3_in.bits.isLoad`),
    * - It is either not a fast replay or the fast replay is cancelled (`!s3_canFastReplay || s3_fastReplayCanclled`),
    * - The uop has not been feedbacked (`!s3_in.bits.feedbacked`),
    * - The uop is not non-cacheable (`!s3_in.bits.isNoncacheable`),
    * - The uop does not need a wake-up due to misalignment (`!s3_in.bits.misalignNeedWakeUp`).
    */
  toLsq.valid := s3_in.valid && s3_in.bits.isLoad && (!s3_canFastReplay || s3_fastReplayCanclled) &&
    !s3_in.bits.feedbacked && !s3_in.bits.isNoncacheable && !s3_in.bits.misalignNeedWakeUp
  toLsq.bits := s3_in.bits

  /**
   * Determines whether the miss entry has been updated for the Load/Store Queue (LSQ) (`toLsq.bits.missDbUpdated`).
   * Conditions:
   * 1. The input in s2 is valid (`s2_in.valid`).
   * 2. The uop in s2 has a Reorder Buffer (ROB) entry (`s2_in.bits.hasROBEntry`).
   * 3. The uop in s2 does not have a TLB miss (`!s2_in.bits.tlbMiss`).
   * 4. The miss entrye has not been updated already (`!s2_in.bits.missDbUpdated`).
   */
  toLsq.bits.missDbUpdated := GatedValidRegNext(s2_in.valid && s2_in.bits.hasROBEntry &&
    !s2_in.bits.tlbMiss && !s2_in.bits.missDbUpdated)

  /**
   * Determines whether the address update is valid for s3 (`toLsq.bits.updateAddrValid`).
   * Conditions:
   * 1. The uop in s3 is not misaligned (`!s3_in.bits.misalign`).
   * 2. The uop is either not in the misalign buffer or it is the final split
   *    (`!s3_in.bits.isMisalignBuf || s3_in.bits.isFinalSplit`).
   * 3. Alternatively, there is an exception present in stage 3 (`s3_in.bits.hasException`).
   */
  toLsq.bits.updateAddrValid := !s3_in.bits.misalign && (!s3_in.bits.isMisalignBuf || s3_in.bits.isFinalSplit) ||
    s3_in.bits.hasException

  // rollback
  toBackend.rollback.valid := s3_in.valid && (s3_replayInst || s3_flushPipe || s3_fromMisalignFlush) && !s3_hasException
  toBackend.rollback.bits := s3_in.bits.toRedirectBundle()
  toBackend.rollback.bits.level := Mux(
    s3_replayInst || s3_fromMisalignFlush,
    RedirectLevel.flush,
    RedirectLevel.flushAfter
  )

  // iqFeedback
  /**
    * Determine whether feedback at s3.
    * Feedback does not require waiting if:
    * - The uop is not a load replay (`!s3_in.bits.isLoadReplay`),
    * - The uop has not been feedbacked (`!s3_in.feedbacked`),
    * - Either fast replay is not possible, or fast replay is cancelled
    *   (`!(s3_canFastReplay && !s3_fastReplayCanclled)`).
    */
  val s3_feedbackNoWaiting = !s3_in.bits.isLoadReplay && !s3_in.bits.feedbacked &&
    (!(s3_canFastReplay && !s3_fastReplayCanclled))

  /**
    * Determine whether slow feedback to the backend issue queue (IQ) is valid.
    * Slow feedback is valid if:
    * - The input to s3 is valid (`s3_in.valid`),
    * - Feedback does not require waiting (`s3_feedbackNoWaiting`),
    * - The uop is not a vector instruction (`!s3_in.bits.isVector`),
    * - The uop is not issued by misalign buffer (`!s3_in.bits.isMisalignBuf`).
    */
  toBackend.iqFeedback.feedbackSlow.valid := s3_in.valid && s3_feedbackNoWaiting && !s3_in.bits.isVector &&
    !s3_in.bits.isMisalignBuf
  toBackend.iqFeedback.feedbackSlow.bits := s3_in.bits.toRSFeedbackBundle()
  toBackend.iqFeedback.feedbackSlow.bits.hit := !s3_in.bits.needReplay || toLsq.ready
  toBackend.iqFeedback.feedbackSlow.bits.sourceType := RSFeedbackType.lrqFull
  toBackend.iqFeedback.feedbackSlow.bits.isLoad := s3_in.bits.isLoad

  // writeback
  s3_writeback.ready := false.B

  toBackend.writeback.zip(commonIn.s3_wbPort).foreach {
    case (wb, chosen) =>
      wb.valid := s3_writeback.valid && s3_writeback.bits.isLoad && chosen
      wb.bits := s3_writeback.bits
      wb.bits.feedbacked := (!s3_writeback.bits.isVector || s3_writeback.bits.needReplay && !toLsq.ready) && s3_feedbackNoWaiting
      wb.bits.uop.rfWen := !toBackend.ldCancel.ld2Cancel && s3_writeback.bits.uop.rfWen
      wb.bits.uop.flushPipe := false.B // use rollback
      wb.bits.uop.replayInst := false.B // use rollback
      when (s3_in.valid && chosen) {
        s3_writeback.ready := wb.ready
      }
  }

  // store writeback delay
  val pipelineRegs = Seq.fill(TotalDelayCycles)(Module(new NewPipelineConnectPipe(new LsPipelineBundle)))
  val wbPortRegs = DelayN(commonIn.s3_wbPort, TotalDelayCycles)

  val exuOut = Wire(DecoupledIO(new LsPipelineBundle))
  if (pipelineRegs.length > 0) {
    pipelineRegs.head.io.in <> s3_in
    pipelineRegs.head.io.in.valid := s3_in.valid && s3_in.bits.isStore
    pipelineRegs.head.io.in.bits.uop.exceptionVec := ExceptionNO.selectByFu(s3_in.bits.uop.exceptionVec, StaCfg)
    pipelineRegs.head.io.isFlush := s3_in.bits.uop.robIdx.needFlush(fromCtrl.redirect)
    pipelineRegs.head.io.rightOutFire := pipelineRegs.head.io.out.fire
    pipelineRegs.drop(1).zip(pipelineRegs.dropRight(1)).foreach {
      case (sink, source) =>
        val isFlush = source.io.out.bits.uop.robIdx.needFlush(fromCtrl.redirect)
        sink.io.in <> source.io.out
        sink.io.isFlush := isFlush
        sink.io.rightOutFire := sink.io.out.fire
    }
    exuOut <> pipelineRegs.last.io.out
  } else {
    exuOut <> s3_in
  }

  exuOut.ready := false.B
  toBackend.writeback.zip(wbPortRegs).foreach {
    case (wb, chosen) =>
      wb.valid := exuOut.valid && chosen
      wb.bits := exuOut.bits
      wb.bits.data := DontCare
      when (chosen && exuOut.bits.isStore) {
        exuOut.ready := wb.ready
      }
  }
}

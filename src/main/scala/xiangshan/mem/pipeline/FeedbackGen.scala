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


  val io = IO(new FeedbackGenIO).suggestName("io")

  // Pipeline
  // -------------------------------------------------------------------
  // stage 0
  // -------------------------------------------------------------------
  /**
   * Determines whether there is no need to wake up in s0 (`s0_noNeedWakeup`).
   * Conditions:
   * 1. The uop in s0 is a vector uop (`io.s0_in.bits.isVector`).
   * 2. The uop in s0 is a hardware prefetch (`io.s0_in.bits.isHWPrefetch`).
   * 3. The uop is in the misalign buffer, but does not require a wakeup (`io.s0_in.bits.isMisalignBuf && !io.s0_in.bits.misalignNeedWakeUp`).
   */
  val s0_noNeedWakeup = io.s0_in.bits.isVector || io.s0_in.bits.isHWPrefetch || io.s0_in.bits.isMisalignBuf && !io.s0_in.bits.misalignNeedWakeUp

  /**
    * replay from lq, lda/hya and misalign buffer need to wakeup other consumers.
    */
  val s0_replayWakeup = io.s0_in.bits.isLoadReplay || io.s0_in.bits.isFastReplay || io.s0_in.bits.isMisalignBuf

  /**
    * issue from misalign and need wakeup
    */
  val s0_misalignWakeup = io.s0_in.bits.isMisalignBuf && io.s0_in.bits.misalignNeedWakeUp

  /**
    * mmio and non-cacheable requests also need to wakeup other consumers.
    */
  val s0_canSafeWakeup = (s0_replayWakeup || io.s0_in.bits.isMmio || io.s0_in.bits.isNoncacheable || s0_misalignWakeup) &&
    !s0_noNeedWakeup

  io.toBackend.wakeup.valid := io.s0_in.fire && s0_canSafeWakeup && io.s0_in.bits.isLoad
  io.toBackend.wakeup.bits := io.s0_in.bits.uop

  // Pipeline
  // -------------------------------------------------------------------
  // stage 1
  // -------------------------------------------------------------------
  // to Backend
  io.toBackend.stIssue.valid := io.s1_in.valid && !io.fromTlb.bits.miss && io.s1_in.bits.isIq
  io.toBackend.stIssue.bits  := io.s1_in.bits.toMemExuInputBundle()

  /**
    * Send TLB feedback to store issue queue (only for store). Store feedback is generated in s1, sent to Iq in s2
    */
  val s1_iqFeedback = Wire(Valid(new RSFeedback))
  val s1_iqFeedbackCanGo = s1_iqFeedback.valid && !io.s1_in.bits.isVector && !io.s1_in.bits.isMisalignBuf
  s1_iqFeedback.valid := io.s1_in.valid && io.s1_in.bits.isIq && io.s1_in.bits.isStore
  s1_iqFeedback.bits.hit := !io.fromTlb.bits.miss
  s1_iqFeedback.bits.flushState := io.fromTlb.bits.ptwBack
  s1_iqFeedback.bits.robIdx := io.s1_in.bits.uop.robIdx
  s1_iqFeedback.bits.sourceType := RSFeedbackType.tlbMiss
  s1_iqFeedback.bits.sqIdx := io.s1_in.bits.uop.sqIdx
  s1_iqFeedback.bits.lqIdx := io.s1_in.bits.uop.lqIdx
  s1_iqFeedback.bits.dataInvalidSqIdx := DontCare
  s1_iqFeedback.bits.isLoad := io.s1_in.bits.isLoad

  // to Backend
  // IssueQueue ld1Cancel
  io.toBackend.ldCancel.ld1Cancel := false.B

  // Pipeline
  // -------------------------------------------------------------------
  // stage 2
  // -------------------------------------------------------------------
  /**
   * Determines whether the store misalignment buffer should be valid (`io.toStoreMisalignBuf.valid`).
   * Conditions:
   * 1. The input in s2 is valid (`io.s2_in.valid`).
   * 2. The uop in s2 is misaligned (`io.s2_in.bits.misalign`).
   * 3. The uop is not issued by the misalign buffer (`!io.s2_in.bits.isMisalignBuf`).
   * 4. The uop is not an MMIO uop (`!io.s2_in.bits.mmio`).
   * 5. The uop is a store (`io.s2_in.bits.isStore`).
   */
  io.toStoreMisalignBuf.enq.valid := io.s2_in.valid && io.s2_in.bits.misalign && !io.s2_in.bits.isMisalignBuf &&
    !io.s2_in.bits.mmio && io.s2_in.bits.isStore
  io.toStoreMisalignBuf.enq.bits := io.s2_in.bits

  //  store feedback
  val storeMisalignBufCanAccept = io.toStoreMisalignBuf.enq.valid && !io.toStoreMisalignBuf.enq.ready
  io.toBackend.iqFeedback.feedbackFast.valid := GatedValidRegNext(s1_iqFeedbackCanGo)
  io.toBackend.iqFeedback.feedbackFast.bits := RegEnable(s1_iqFeedback.bits, s1_iqFeedbackCanGo)
  io.toBackend.iqFeedback.feedbackFast.bits.hit := RegEnable(s1_iqFeedback.bits.hit, s1_iqFeedbackCanGo) &&
    storeMisalignBufCanAccept

  //  to MisalignBuf writeback
  if (io.toStoreMisalignBuf.out.isDefined) {
    io.toStoreMisalignBuf.out.get.valid := io.s2_in.valid && io.s2_in.bits.isMisalignBuf && io.s2_in.bits.isStore
    io.toStoreMisalignBuf.out.get.bits := io.s2_in.bits
  }

  /**
   * Determines whether the safe wakeup condition is met for stage 2 (`s2_safeWakeup`).
   * Conditions:
   * 1. The uop in s2 does not require a replay (`!io.s2_in.needReplay`).
   * 2. The uop is an MMIO (Memory-Mapped I/O) uop (`io.s2_in.bits.mmio`).
   * 3. The uop is not misaligned (`!io.s2_in.bits.misalign`).
   * 4. The uop is load (`io.s2_in.bits.isLoad`).
   * 5. The uop is either non-cacheable or explicitly marked as non-cacheable
   *    (`!io.s2_in.bits.nc || io.s2_in.bits.isNoncacheable`).
   * 6. The debug mode is triggered (`io.fromCtrl.triggerDebugMode`) or there is an exception
   *    present (`io.s2_in.bits.hasException`).
   * 7. The vector uop is active (`io.s2_in.bits.vecActive`).
   */
  val s2_safeWakeup = !io.s2_in.bits.needReplay && io.s2_in.bits.mmio && !io.s2_in.bits.misalign && io.s2_in.bits.isLoad
    (!io.s2_in.bits.nc || io.s2_in.bits.isNoncacheable) &&
    (io.fromCtrl.trigger.debugMode || io.s2_in.bits.hasException) && io.s2_in.bits.vecActive

  // Pipeline
  // -------------------------------------------------------------------
  // stage 3
  // -------------------------------------------------------------------
  // to Backend
  val s3_safeWakeup = RegEnable(s2_safeWakeup, io.s2_in.valid)

  /**
   * Determines whether the load cancel signal should be set for s3 (`io.toBackend.ldCancel.ld2Cancel`).
   * The load uop should cancel coonsumers if:
   * 1. The input in s3 is valid (`io.s3_in.valid`).
   * 2. The safe wakeup condition is not set (`!s3_safeWakeup`).
   * 3. The uop in s3 is not a vector uop (`!io.s3_in.isVector`).
   * 4. The uop is either not in the misalign buffer or requires a wakeup
   *    (`!io.s3_in.bits.isMisalignBuf || io.s3_in.bits.misalignNeedWakeUp`).
   */
  io.toBackend.ldCancel.ld2Cancel := io.s3_in.valid && !s3_safeWakeup && !io.s3_in.bits.isVector &&
    (!io.s3_in.bits.isMisalignBuf|| io.s3_in.bits.misalignNeedWakeUp)

  // fast replay
  val s3_flushPipe = io.commonIn.s3_flushPipe
  val s3_replayInst = io.commonIn.s3_replayInst
  val s3_fromMisalignFlush = io.commonIn.s3_fromMisalignFlush
  val s3_hasException = io.commonIn.s3_hasException
  val s3_canFastReplay = io.commonIn.s3_canFastReplay
  val s3_fastReplayCanclled = io.commonIn.s3_fastReplayCancelled

  io.toLdu.replay.valid := io.s3_in.valid && s3_canFastReplay
  io.toLdu.replay.bits := io.s3_in.bits
  io.toLdu.replay.bits.delayedError := io.s3_in.bits.delayedError
  io.toLdu.replay.bits.clearIssueState()
  io.toLdu.replay.bits.isFastReplay := true.B
  io.toLdu.replay.bits.isStore := false.B

  // to load misalign buffer enq
  /**
   * Determines whether the load misalignment buffer (`io.toLoadMisalignBuf.enq.valid`).
   * Conditions:
   * 1. The input in s3 is valid (`io.s3_in.valid`).
   * 2. The uop in s3 is misaligned (`io.s3_in.bits.misalign`).
   * 3. The uop is not already in the misalign buffer (`!io.s3_in.bits.isMisalignBuf`).
   * 4. The uop in stage 2 is a load uop (`io.s2_in.bits.isLoad`).
   */
  io.toLoadMisalignBuf.enq.valid := io.s3_in.valid && io.s3_in.bits.misalign && !io.s3_in.bits.isMisalignBuf && io.s2_in.bits.isLoad
  io.toLoadMisalignBuf.enq.bits := io.s3_in.bits

  // to load misalign buffer writeback
  /**
   * Determines whether the load misalignment buffer (`io.toLoadMisalignBuf.out.valid`).
   * Conditions:
   * 1. The input in s3 is valid (`io.s3_in.valid`).
   * 2. This condition checks that either fast replay is not allowed ('!s3_canFastReplay')
   *    or the fast replay mechanism has been canceled ('s3_fastReplayCanclled').
   * 3. The uop is not already in the misalign buffer (`!io.s3_in.bits.isMisalignBuf`).
   * 4. The uop in stage 2 is a load uop (`io.s2_in.bits.isLoad`).
   */
  if (io.toLoadMisalignBuf.out.isDefined) {
    io.toLoadMisalignBuf.out.get.valid := io.s3_in.valid && (!s3_canFastReplay || s3_fastReplayCanclled) && io.s3_in.bits.isMisalignBuf
    io.toLoadMisalignBuf.out.get.bits := io.s3_in.bits
    io.toLoadMisalignBuf.out.get.bits.data := io.commonIn.s3_misalignData
  }

  // to Lsq
  /**
    * Determine if the LSQ (Load-Store Queue) input is valid in s3.
    * The input is valid if:
    * - The input to s3 is valid (`io.s3_in.valid`),
    * - The uop is a load (`io.s3_in.bits.isLoad`),
    * - It is either not a fast replay or the fast replay is cancelled (`!s3_canFastReplay || s3_fastReplayCanclled`),
    * - The uop has not been feedbacked (`!io.s3_in.bits.feedbacked`),
    * - The uop is not non-cacheable (`!io.s3_in.bits.isNoncacheable`),
    * - The uop does not need a wake-up due to misalignment (`!io.s3_in.bits.misalignNeedWakeUp`).
    */
  io.toLsq.valid := io.s3_in.valid && io.s3_in.bits.isLoad && (!s3_canFastReplay || s3_fastReplayCanclled) &&
    !io.s3_in.bits.feedbacked && !io.s3_in.bits.isNoncacheable && !io.s3_in.bits.misalignNeedWakeUp
  io.toLsq.bits := io.s3_in.bits

  /**
   * Determines whether the miss entry has been updated for the Load/Store Queue (LSQ) (`io.toLsq.bits.missDbUpdated`).
   * Conditions:
   * 1. The input in s2 is valid (`io.s2_in.valid`).
   * 2. The uop in s2 has a Reorder Buffer (ROB) entry (`io.s2_in.bits.hasROBEntry`).
   * 3. The uop in s2 does not have a TLB miss (`!io.s2_in.bits.tlbMiss`).
   * 4. The miss entrye has not been updated already (`!io.s2_in.bits.missDbUpdated`).
   */
  io.toLsq.bits.missDbUpdated := GatedValidRegNext(io.s2_in.valid && io.s2_in.bits.hasROBEntry &&
    !io.s2_in.bits.tlbMiss && !io.s2_in.bits.missDbUpdated)

  /**
   * Determines whether the address update is valid for s3 (`io.toLsq.bits.updateAddrValid`).
   * Conditions:
   * 1. The uop in s3 is not misaligned (`!io.s3_in.bits.misalign`).
   * 2. The uop is either not in the misalign buffer or it is the final split
   *    (`!io.s3_in.bits.isMisalignBuf || io.s3_in.bits.isFinalSplit`).
   * 3. Alternatively, there is an exception present in stage 3 (`io.s3_in.bits.hasException`).
   */
  io.toLsq.bits.updateAddrValid := !io.s3_in.bits.misalign && (!io.s3_in.bits.isMisalignBuf || io.s3_in.bits.isFinalSplit) ||
    io.s3_in.bits.hasException

  // rollback
  io.toBackend.rollback.valid := io.s3_in.valid && (s3_replayInst || s3_flushPipe || s3_fromMisalignFlush) && !s3_hasException
  io.toBackend.rollback.bits := io.s3_in.bits.toRedirectBundle()
  io.toBackend.rollback.bits.level := Mux(
    s3_replayInst || s3_fromMisalignFlush,
    RedirectLevel.flush,
    RedirectLevel.flushAfter
  )

  // iqFeedback
  /**
    * Determine whether feedback at s3.
    * Feedback does not require waiting if:
    * - The uop is not a load replay (`!io.s3_in.bits.isLoadReplay`),
    * - The uop has not been feedbacked (`!io.s3_in.feedbacked`),
    * - Either fast replay is not possible, or fast replay is cancelled
    *   (`!(s3_canFastReplay && !s3_fastReplayCanclled)`).
    */
  val s3_feedbackNoWaiting = !io.s3_in.bits.isLoadReplay && !io.s3_in.bits.feedbacked &&
    (!(s3_canFastReplay && !s3_fastReplayCanclled))

  /**
    * Determine whether slow feedback to the backend issue queue (IQ) is valid.
    * Slow feedback is valid if:
    * - The input to s3 is valid (`io.s3_in.valid`),
    * - Feedback does not require waiting (`s3_feedbackNoWaiting`),
    * - The uop is not a vector instruction (`!io.s3_in.bits.isVector`),
    * - The uop is not issued by misalign buffer (`!io.s3_in.bits.isMisalignBuf`).
    */
  io.toBackend.iqFeedback.feedbackSlow.valid := io.s3_in.valid && s3_feedbackNoWaiting && !io.s3_in.bits.isVector &&
    !io.s3_in.bits.isMisalignBuf
  io.toBackend.iqFeedback.feedbackSlow.bits := io.s3_in.bits.toRSFeedbackBundle()
  io.toBackend.iqFeedback.feedbackSlow.bits.hit := !io.s3_in.bits.needReplay || io.toLsq.ready
  io.toBackend.iqFeedback.feedbackSlow.bits.sourceType := RSFeedbackType.lrqFull
  io.toBackend.iqFeedback.feedbackSlow.bits.isLoad := io.s3_in.bits.isLoad

  // writeback
  val s3_writeback = Wire(DecoupledIO(new LsPipelineBundle))

  /**
    * Determine whether unit has a load execution (LdExe) and any load uop is MMIO.
    * This block handles the writeback logic for MMIO loads and other types of loads.
    */
  if (params.hasLdExe && params.issueParams.map(x => MemIssueType.isMmio(x.issueType)).reduce(_||_)) {
    /**
      * Determine whether the uop is an MMIO load, the valid signal is set based on either the
      * incoming valid signal or a delayed valid signal from the LSQ.
      */
    s3_writeback.valid := io.s3_in.valid ||
      RegNextN(io.fromLsq.mmioLdWriteback.valid && io.s0_in.bits.isMmio, 3, Some(false.B))
    when (io.s3_in.valid) {
      s3_writeback.bits := io.s3_in.bits
    } .otherwise {
      s3_writeback.bits.fromMemExuOutputBundle(RegNextN(io.fromLsq.mmioLdWriteback.bits, 3))
    }
  } else {
    s3_writeback.valid := io.s3_in.valid
    s3_writeback.bits := io.s3_in.bits
  }
  io.s3_in.ready := s3_writeback.ready

  s3_writeback.ready := false.B
  /**
   * Check whether unit supports load execution (LdExe).
   * This block processes the writeback to the backend writeback ports.
   */
  if (params.hasLdExe) {
    io.toBackend.writeback.zip(io.commonIn.s3_wbPort).zipWithIndex.foreach {
      case ((wb, chosen), i) =>

        /**
         * Set the valid signal for the writeback port if the writeback is valid
         * and the current port is selected (chosen).
         */
        wb.valid := s3_writeback.valid && s3_writeback.bits.isLoad && chosen
        wb.bits := s3_writeback.bits
       /**
         * Depending on the type of issue, assign the appropriate data to the write-back.
         * Vector data is assigned if the issue type is valid; otherwise, regular data is used.
         */
        if (params.wbParams(i).values.head.contains(MemIssueType.Vld)) {
          wb.bits.data := io.commonIn.s3_vecData
          wb.bits.uop.exceptionVec := ExceptionNO.selectByFu(s3_writeback.bits.uop.exceptionVec, VlduCfg)
        } else {
          wb.bits.data := io.commonIn.s3_data
          wb.bits.uop.exceptionVec := ExceptionNO.selectByFu(s3_writeback.bits.uop.exceptionVec, LduCfg)
        }

        wb.bits.feedbacked := s3_feedbackNoWaiting &&
          (!s3_writeback.bits.isVector || s3_writeback.bits.needReplay && !io.toLsq.ready)
        wb.bits.uop.rfWen := !io.toBackend.ldCancel.ld2Cancel && s3_writeback.bits.uop.rfWen
        wb.bits.uop.flushPipe := false.B // use rollback
        wb.bits.uop.replayInst := false.B // use rollback

        /**
         * If the write-back is valid and this port is selected
         */
        when (s3_writeback.valid && chosen) {
          s3_writeback.ready := wb.ready
        }
    }
  }

  // store writeback delay
  /**
   * Determine whether unit has a store execution (StaExe), this block handles the store writeback
   * and the propagation of data through the pipeline.
   */
  if (params.hasStaExe) {
    val pipelineRegs = Seq.fill(TotalDelayCycles)(Module(new NewPipelineConnectPipe(new LsPipelineBundle)))
    val wbPortRegs = DelayN(io.commonIn.s3_wbPort, TotalDelayCycles)

    val exuOut = Wire(DecoupledIO(new LsPipelineBundle))
    /**
     * If there are pipeline registers (i.e., TotalDelayCycles > 0), process the pipeline stages.
     */
    if (pipelineRegs.length > 0) {
      pipelineRegs.head.io.in.valid := s3_writeback.valid && s3_writeback.bits.isStore
      pipelineRegs.head.io.in.bits := s3_writeback.bits
      when (s3_writeback.bits.isStore) {
        s3_writeback.ready := pipelineRegs.head.io.in.ready
      }

      pipelineRegs.head.io.isFlush := io.s3_in.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
      pipelineRegs.head.io.rightOutFire := pipelineRegs.head.io.out.fire
      pipelineRegs.drop(1).zip(pipelineRegs.dropRight(1)).foreach {
        case (sink, source) =>
          val isFlush = source.io.out.bits.uop.robIdx.needFlush(io.fromCtrl.redirect)
          sink.io.in <> source.io.out
          sink.io.isFlush := isFlush
          sink.io.rightOutFire := sink.io.out.fire
      }
      exuOut <> pipelineRegs.last.io.out
    } else {
      /**
       * If there are no pipeline (i.e., a single stage), set the EXU output directly.
       * The valid signal is set if it's store uop.
       */
      exuOut.valid := s3_writeback.valid && s3_writeback.bits.isStore
      when (s3_writeback.bits.isStore) {
        s3_writeback.ready := exuOut.ready
      }
    }

    exuOut.ready := false.B
    io.toBackend.writeback.zip(wbPortRegs).zipWithIndex.foreach {
      case ((wb, chosen), i) =>
        wb.valid := exuOut.valid && chosen
        wb.bits := exuOut.bits
        wb.bits.data := DontCare
        if (params.wbParams(i).values.head.contains(MemIssueType.Vst)) {
          wb.bits.uop.exceptionVec := ExceptionNO.selectByFu(exuOut.bits.uop.exceptionVec, VstuCfg)
        } else {
          wb.bits.uop.exceptionVec := ExceptionNO.selectByFu(exuOut.bits.uop.exceptionVec, StaCfg)
        }
        when (chosen && exuOut.bits.isStore) {
          exuOut.ready := wb.ready
        }
    }
  }
}

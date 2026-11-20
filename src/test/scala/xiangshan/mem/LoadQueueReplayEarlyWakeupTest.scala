package xiangshan.mem

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.ChiselSim
import chisel3.testing.HasTestingDirectory
import java.nio.file.Paths
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import top.DefaultConfig
import utility.{Constantin, LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{XSCoreParamsKey, XSTileKey}

class LoadQueueReplayEarlyWakeupTest extends AnyFlatSpec with ChiselSim with Matchers {
  private val config = new DefaultConfig
  private implicit val parameters: org.chipsalliance.cde.config.Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head.copy(LoadQueueReplaySize = 12)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = false)
  }
  private implicit val testingDirectory: HasTestingDirectory = new HasTestingDirectory {
    override def getDirectory = Paths.get(System.getProperty("user.home"), ".tmp", "lrq-early-wakeup-test")
  }

  private def clearInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(clearInputs)
    case vec: Vec[_] => vec.foreach(clearInputs)
    case bit: Bool if DataMirror.directionOf(bit) == ActualDirection.Input => bit.poke(false.B)
    case uint: UInt if DataMirror.directionOf(uint) == ActualDirection.Input => uint.poke(0.U)
    case _ =>
  }

  behavior of "LoadQueueReplay early wakeup"

  it should "schedule matching S3 wakeups without losing payload or issuing twice" in {
    simulate({
      Constantin.init(false)
      try new LoadQueueReplay finally Constantin.init(true)
    }) { dut =>
      def resetAndAllocate(): BigInt = {
        clearInputs(dut.io)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        dut.clock.step(5)
        dut.io.alloc(0).valid.poke(true.B)
        dut.io.alloc(0).bits.robIdx.value.poke(1.U)
        dut.io.alloc(0).bits.robIdx.isFormer.poke(true.B)
        dut.io.alloc(0).bits.lqIdx.value.poke(1.U)
        val index = dut.io.allocIndex(0).peek().litValue
        dut.clock.step()
        dut.io.alloc(0).valid.poke(false.B)
        index
      }
      def request(bits: xiangshan.mem.Bundles.LqWriteBundle, index: BigInt, waitSq: Int,
                  cause: Int = LoadReplayCauses.C_MA): Unit = {
        bits.schedIndex.poke(index.U)
        bits.uop.robIdx.value.poke(1.U)
        bits.uop.robIdx.isFormer.poke(true.B)
        bits.uop.lqIdx.value.poke(1.U)
        bits.uop.loadWaitStrict.poke(true.B)
        bits.rep_info.need_rep.poke(true.B)
        bits.rep_info.cause.zipWithIndex.foreach { case (bit, index) => bit.poke((index == cause).B) }
        bits.rep_info.addr_inv_sq_idx.value.poke(waitSq.U)
        bits.rep_info.data_inv_sq_idx.value.poke(waitSq.U)
      }
      def addressWakeup(waitSq: Int): Unit = {
        dut.io.storeAddrWakeup(0).valid.poke(true.B)
        dut.io.storeAddrWakeup(0).bits.sqIdx.value.poke(waitSq.U)
      }
      def expectNoReplay(): Unit = dut.io.replay.foreach(_.valid.expect(false.B))
      def expectReplayNow(index: BigInt): Unit = {
        val ports = dut.io.replay.filter(_.valid.peek().litToBoolean)
        ports.size shouldBe 1
        ports.head.bits.replayQueueIdx.get.expect(index.U)
      }
      def expectReplay(index: BigInt): Unit = {
        dut.clock.step(4)
        expectReplayNow(index)
      }

      // S3 wakeups retain their original register-to-selection delay after S2 writes the wait key.
      for (cause <- Seq(LoadReplayCauses.C_MA, LoadReplayCauses.C_FF)) {
        val s3Index = resetAndAllocate()
        request(dut.io.earlyWakeup(0).bits, s3Index, 7, cause)
        dut.io.earlyWakeup(0).bits.uop.fuOpType.poke(3.U)
        dut.io.earlyWakeup(0).bits.vaddr.poke("h12345000".U)
        dut.io.earlyWakeup(0).valid.poke(true.B)
        if (cause == LoadReplayCauses.C_MA) addressWakeup(7)
        else {
          dut.io.storeDataWakeup(0).valid.poke(true.B)
          dut.io.storeDataWakeup(0).bits.sqIdx.value.poke(7.U)
        }
        expectNoReplay()
        dut.clock.step()
        dut.io.earlyWakeup(0).valid.poke(false.B)
        dut.io.storeAddrWakeup(0).valid.poke(false.B)
        dut.io.storeDataWakeup(0).valid.poke(false.B)
        request(dut.io.enq(0).bits, s3Index, 7, cause)
        dut.io.enq(0).bits.uop.fuOpType.poke(3.U)
        dut.io.enq(0).bits.vaddr.poke("h12345000".U)
        dut.io.enq(0).valid.poke(true.B)
        expectNoReplay()
        dut.clock.step()
        dut.io.enq(0).valid.poke(false.B)
        expectNoReplay()
        dut.clock.step()
        withClue(s"registered S3 wakeup, cause=$cause: ") { expectReplayNow(s3Index) }
        val port = dut.io.replay.find(_.valid.peek().litToBoolean).get
        port.bits.uop.fuOpType.expect(3.U)
        port.bits.vaddr.expect("h12345000".U)
        dut.clock.step(3)
        expectReplayNow(s3Index)
        port.ready.poke(true.B)
        dut.clock.step()
        port.ready.poke(false.B)
        dut.clock.step(3)
        expectNoReplay()
      }

      // Already queued entries keep the baseline pulse-delay, blocking-update and selection stages.
      for (cause <- Seq(LoadReplayCauses.C_MA, LoadReplayCauses.C_FF, LoadReplayCauses.C_DM,
        LoadReplayCauses.C_TM)) {
        val waitingIndex = resetAndAllocate()
        request(dut.io.enq(0).bits, waitingIndex, 7, cause)
        dut.io.enq(0).bits.handledByMSHR.poke(true.B)
        dut.io.enq(0).bits.rep_info.mshr_id.poke(2.U)
        dut.io.enq(0).bits.rep_info.tlb_id.poke(3.U)
        dut.io.enq(0).valid.poke(true.B)
        dut.clock.step()
        dut.io.enq(0).valid.poke(false.B)
        dut.clock.step(3)
        expectNoReplay()
        cause match {
          case LoadReplayCauses.C_MA => addressWakeup(7)
          case LoadReplayCauses.C_FF =>
            dut.io.storeDataWakeup(0).valid.poke(true.B)
            dut.io.storeDataWakeup(0).bits.sqIdx.value.poke(7.U)
          case LoadReplayCauses.C_DM =>
            dut.io.loadWakeup(0).valid.poke(true.B)
            dut.io.loadWakeup(0).bits.mshrId.poke(2.U)
          case LoadReplayCauses.C_TM =>
            dut.io.tlb_hint.resp.valid.poke(true.B)
            dut.io.tlb_hint.resp.bits.id.poke(3.U)
        }
        expectNoReplay()
        dut.clock.step()
        dut.io.storeAddrWakeup(0).valid.poke(false.B)
        dut.io.storeDataWakeup(0).valid.poke(false.B)
        dut.io.loadWakeup(0).valid.poke(false.B)
        dut.io.tlb_hint.resp.valid.poke(false.B)
        expectNoReplay()
        dut.clock.step()
        expectNoReplay()
        dut.clock.step()
        withClue(s"baseline queued wakeup timing, cause=$cause: ") { expectReplayNow(waitingIndex) }
      }

      // A ready S2 record may enter normal arbitration in S3 without another wakeup.
      val readyIndex = resetAndAllocate()
      request(dut.io.earlyWakeup(0).bits, readyIndex, 0, LoadReplayCauses.C_BC)
      dut.io.earlyWakeup(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.earlyWakeup(0).valid.poke(false.B)
      request(dut.io.enq(0).bits, readyIndex, 0, LoadReplayCauses.C_BC)
      dut.io.enq(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.enq(0).valid.poke(false.B)
      withClue("S3 already-ready entry: ") { expectReplayNow(readyIndex) }

      // A full replay request stage postpones arbitration without discarding S2 readiness.
      val heldIndex = resetAndAllocate()
      request(dut.io.enq(0).bits, heldIndex, 0, LoadReplayCauses.C_BC)
      dut.io.enq(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.enq(0).valid.poke(false.B)
      expectReplay(heldIndex)
      dut.io.alloc(0).valid.poke(true.B)
      dut.io.alloc(0).bits.robIdx.value.poke(2.U)
      dut.io.alloc(0).bits.lqIdx.value.poke(2.U)
      dut.clock.step()
      dut.io.alloc(0).valid.poke(false.B)
      dut.io.alloc(0).valid.poke(true.B)
      dut.io.alloc(0).bits.robIdx.value.poke(3.U)
      dut.io.alloc(0).bits.lqIdx.value.poke(3.U)
      dut.clock.step()
      dut.io.alloc(0).valid.poke(false.B)
      dut.io.alloc(0).valid.poke(true.B)
      dut.io.alloc(0).bits.robIdx.value.poke(4.U)
      dut.io.alloc(0).bits.lqIdx.value.poke(4.U)
      val deferredIndex = dut.io.allocIndex(0).peek().litValue
      deferredIndex shouldBe heldIndex + 3
      dut.clock.step()
      dut.io.alloc(0).valid.poke(false.B)
      request(dut.io.earlyWakeup(0).bits, deferredIndex, 0, LoadReplayCauses.C_BC)
      dut.io.earlyWakeup(0).bits.uop.robIdx.value.poke(4.U)
      dut.io.earlyWakeup(0).bits.uop.lqIdx.value.poke(4.U)
      dut.io.earlyWakeup(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.earlyWakeup(0).valid.poke(false.B)
      request(dut.io.enq(0).bits, deferredIndex, 0, LoadReplayCauses.C_BC)
      dut.io.enq(0).bits.uop.robIdx.value.poke(4.U)
      dut.io.enq(0).bits.uop.lqIdx.value.poke(4.U)
      dut.io.enq(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.enq(0).valid.poke(false.B)
      expectReplayNow(heldIndex)
      dut.io.replay(0).ready.poke(true.B)
      dut.clock.step()
      dut.io.replay(0).ready.poke(false.B)
      withClue("deferred S3 arbitration: ") { expectReplayNow(deferredIndex) }
      dut.io.replay(0).ready.poke(true.B)
      dut.clock.step()
      dut.io.replay(0).ready.poke(false.B)
      dut.clock.step(3)
      expectNoReplay()

      // Ordinary arbitration must not duplicate a fast replay with no S3 LRQ result.
      val fastIndex = resetAndAllocate()
      request(dut.io.earlyWakeup(0).bits, fastIndex, 0, LoadReplayCauses.C_BC)
      dut.io.earlyWakeup(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.earlyWakeup(0).valid.poke(false.B)
      for (_ <- 0 until 5) {
        expectNoReplay()
        dut.clock.step()
      }

      // Predictive L2 hints retain a register stage so replay forwarding does not precede GrantData.
      for (lastBeat <- Seq(false, true)) {
        val s3DmIndex = resetAndAllocate()
        request(dut.io.earlyWakeup(0).bits, s3DmIndex, 0, LoadReplayCauses.C_DM)
        dut.io.earlyWakeup(0).bits.handledByMSHR.poke(true.B)
        dut.io.earlyWakeup(0).bits.rep_info.mshr_id.poke(2.U)
        dut.io.earlyWakeup(0).bits.rep_info.last_beat.poke(lastBeat.B)
        dut.io.earlyWakeup(0).valid.poke(true.B)
        dut.clock.step()
        dut.io.earlyWakeup(0).valid.poke(false.B)
        request(dut.io.enq(0).bits, s3DmIndex, 0, LoadReplayCauses.C_DM)
        dut.io.enq(0).bits.handledByMSHR.poke(true.B)
        dut.io.enq(0).bits.rep_info.mshr_id.poke(2.U)
        dut.io.enq(0).bits.rep_info.last_beat.poke(lastBeat.B)
        dut.io.enq(0).valid.poke(true.B)
        dut.io.l2_hint(0).valid.poke(true.B)
        dut.io.l2_hint(0).bits.sourceId.poke(2.U)
        dut.io.l2_hint(0).bits.isKeyword.poke(false.B)
        dut.clock.step()
        dut.io.enq(0).valid.poke(false.B)
        dut.io.l2_hint(0).valid.poke(false.B)
        expectNoReplay()
        dut.clock.step()
        if (lastBeat) {
          expectNoReplay()
          dut.clock.step()
        }
        withClue(s"S3 L2 hint beat, lastBeat=$lastBeat: ") { expectReplayNow(s3DmIndex) }
      }

      // Actual refill wakeups also retain blocking's register stage before normal selection.
      for (lastBeat <- Seq(false, true)) {
        val refillIndex = resetAndAllocate()
        request(dut.io.earlyWakeup(0).bits, refillIndex, 0, LoadReplayCauses.C_DM)
        dut.io.earlyWakeup(0).bits.handledByMSHR.poke(true.B)
        dut.io.earlyWakeup(0).bits.rep_info.mshr_id.poke(2.U)
        dut.io.earlyWakeup(0).bits.rep_info.last_beat.poke(lastBeat.B)
        dut.io.earlyWakeup(0).valid.poke(true.B)
        dut.io.loadWakeup(0).valid.poke(true.B)
        dut.io.loadWakeup(0).bits.mshrId.poke(2.U)
        dut.clock.step()
        dut.io.earlyWakeup(0).valid.poke(false.B)
        dut.io.loadWakeup(0).valid.poke(false.B)
        request(dut.io.enq(0).bits, refillIndex, 0, LoadReplayCauses.C_DM)
        dut.io.enq(0).bits.handledByMSHR.poke(true.B)
        dut.io.enq(0).bits.rep_info.mshr_id.poke(2.U)
        dut.io.enq(0).bits.rep_info.last_beat.poke(lastBeat.B)
        dut.io.enq(0).valid.poke(true.B)
        dut.clock.step()
        dut.io.enq(0).valid.poke(false.B)
        expectNoReplay()
        dut.clock.step()
        withClue(s"registered S3 refill, lastBeat=$lastBeat: ") { expectReplayNow(refillIndex) }
      }

      // DM wait-state matching includes the beat, not only the MSHR index.
      val changedBeatIndex = resetAndAllocate()
      request(dut.io.earlyWakeup(0).bits, changedBeatIndex, 0, LoadReplayCauses.C_DM)
      dut.io.earlyWakeup(0).bits.handledByMSHR.poke(true.B)
      dut.io.earlyWakeup(0).bits.rep_info.mshr_id.poke(2.U)
      dut.io.earlyWakeup(0).bits.rep_info.last_beat.poke(false.B)
      dut.io.earlyWakeup(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.earlyWakeup(0).valid.poke(false.B)
      request(dut.io.enq(0).bits, changedBeatIndex, 0, LoadReplayCauses.C_DM)
      dut.io.enq(0).bits.handledByMSHR.poke(true.B)
      dut.io.enq(0).bits.rep_info.mshr_id.poke(2.U)
      dut.io.enq(0).bits.rep_info.last_beat.poke(true.B)
      dut.io.enq(0).valid.poke(true.B)
      dut.io.l2_hint(0).valid.poke(true.B)
      dut.io.l2_hint(0).bits.sourceId.poke(2.U)
      dut.io.l2_hint(0).bits.isKeyword.poke(false.B)
      dut.clock.step()
      dut.io.enq(0).valid.poke(false.B)
      dut.io.l2_hint(0).valid.poke(false.B)
      expectNoReplay()
      dut.clock.step(3)
      expectNoReplay()

      // Redirect in the S3 arbitration cycle cancels an otherwise ready entry.
      val s3RedirectIndex = resetAndAllocate()
      request(dut.io.earlyWakeup(0).bits, s3RedirectIndex, 0, LoadReplayCauses.C_BC)
      dut.io.earlyWakeup(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.earlyWakeup(0).valid.poke(false.B)
      request(dut.io.enq(0).bits, s3RedirectIndex, 0, LoadReplayCauses.C_BC)
      dut.io.enq(0).valid.poke(true.B)
      dut.io.redirect.valid.poke(true.B)
      dut.io.redirect.bits.robIdx.isFormer.poke(true.B)
      dut.clock.step()
      dut.io.enq(0).valid.poke(false.B)
      dut.io.redirect.valid.poke(false.B)
      dut.clock.step(3)
      expectNoReplay()

      // S3 arbitration must not consume a wakeup for a different MSHR or SQ entry.
      for (cause <- Seq(LoadReplayCauses.C_MA, LoadReplayCauses.C_DM)) {
        val mismatchIndex = resetAndAllocate()
        request(dut.io.earlyWakeup(0).bits, mismatchIndex, 7, cause)
        dut.io.earlyWakeup(0).bits.handledByMSHR.poke(true.B)
        dut.io.earlyWakeup(0).bits.rep_info.mshr_id.poke(2.U)
        dut.io.earlyWakeup(0).valid.poke(true.B)
        if (cause == LoadReplayCauses.C_MA) addressWakeup(7)
        else {
          dut.io.loadWakeup(0).valid.poke(true.B)
          dut.io.loadWakeup(0).bits.mshrId.poke(2.U)
        }
        dut.clock.step()
        dut.io.earlyWakeup(0).valid.poke(false.B)
        dut.io.storeAddrWakeup(0).valid.poke(false.B)
        dut.io.loadWakeup(0).valid.poke(false.B)
        request(dut.io.enq(0).bits, mismatchIndex, 8, cause)
        dut.io.enq(0).bits.handledByMSHR.poke(true.B)
        dut.io.enq(0).bits.rep_info.mshr_id.poke(3.U)
        dut.io.enq(0).valid.poke(true.B)
        dut.clock.step()
        dut.io.enq(0).valid.poke(false.B)
        withClue(s"S3 changed wait key, cause=$cause: ") { expectNoReplay() }
        dut.clock.step(3)
        expectNoReplay()
      }

      // An unresolved dependency remains blocked until its matching wakeup arrives.
      val blockedIndex = resetAndAllocate()
      request(dut.io.earlyWakeup(0).bits, blockedIndex, 7)
      dut.io.earlyWakeup(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.earlyWakeup(0).valid.poke(false.B)
      request(dut.io.enq(0).bits, blockedIndex, 7)
      dut.io.enq(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.enq(0).valid.poke(false.B)
      dut.clock.step(3)
      expectNoReplay()
      addressWakeup(8)
      dut.clock.step()
      dut.io.storeAddrWakeup(0).valid.poke(false.B)
      dut.clock.step()
      expectNoReplay()
      addressWakeup(7)
      dut.clock.step()
      dut.io.storeAddrWakeup(0).valid.poke(false.B)
      dut.clock.step()
      expectNoReplay()
      dut.clock.step()
      expectReplayNow(blockedIndex)

      val index = resetAndAllocate()
      addressWakeup(7)
      dut.clock.step()
      dut.io.storeAddrWakeup(0).valid.poke(false.B)
      request(dut.io.earlyWakeup(0).bits, index, 7)
      dut.io.earlyWakeup(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.earlyWakeup(0).valid.poke(false.B)
      expectNoReplay()
      request(dut.io.enq(0).bits, index, 7)
      dut.io.enq(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.enq(0).valid.poke(false.B)
      expectReplay(index)

      // A delayed issue wakeup arriving with S3 must survive S3 blocking initialization.
      val sameCycleIndex = resetAndAllocate()
      request(dut.io.earlyWakeup(0).bits, sameCycleIndex, 7)
      dut.io.earlyWakeup(0).valid.poke(true.B)
      addressWakeup(7)
      dut.clock.step()
      dut.io.earlyWakeup(0).valid.poke(false.B)
      dut.io.storeAddrWakeup(0).valid.poke(false.B)
      request(dut.io.enq(0).bits, sameCycleIndex, 7)
      dut.io.enq(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.enq(0).valid.poke(false.B)
      expectReplay(sameCycleIndex)

      // A delayed DM last-beat hint must not overwrite the next execution's store wakeup.
      for (cause <- Seq(LoadReplayCauses.C_MA, LoadReplayCauses.C_FF); oldHint <- Seq(false, true)) {
        val retryIndex = resetAndAllocate()
        request(dut.io.enq(0).bits, retryIndex, 0, LoadReplayCauses.C_DM)
        dut.io.enq(0).bits.handledByMSHR.poke(true.B)
        dut.io.enq(0).bits.rep_info.mshr_id.poke(2.U)
        dut.io.enq(0).bits.rep_info.last_beat.poke(true.B)
        dut.io.enq(0).valid.poke(true.B)
        dut.clock.step()
        dut.io.enq(0).valid.poke(false.B)
        dut.io.loadWakeup(0).valid.poke(true.B)
        dut.io.loadWakeup(0).bits.mshrId.poke(2.U)
        dut.clock.step()
        dut.io.loadWakeup(0).valid.poke(false.B)
        withClue(s"initial DM replay, oldHint=$oldHint: ") { expectReplay(retryIndex) }
        dut.io.replay(0).valid.expect(true.B)
        dut.io.replay(0).ready.poke(true.B)
        dut.clock.step()
        dut.io.replay(0).ready.poke(false.B)
        dut.clock.step()
        request(dut.io.earlyWakeup(0).bits, retryIndex, 7, cause)
        dut.io.earlyWakeup(0).valid.poke(true.B)
        dut.io.l2_hint(0).valid.poke(oldHint.B)
        dut.io.l2_hint(0).bits.sourceId.poke(2.U)
        if (cause == LoadReplayCauses.C_MA) addressWakeup(7)
        else {
          dut.io.storeDataWakeup(0).valid.poke(true.B)
          dut.io.storeDataWakeup(0).bits.sqIdx.value.poke(7.U)
        }
        dut.clock.step()
        dut.io.earlyWakeup(0).valid.poke(false.B)
        dut.io.l2_hint(0).valid.poke(false.B)
        dut.io.storeAddrWakeup(0).valid.poke(false.B)
        dut.io.storeDataWakeup(0).valid.poke(false.B)
        request(dut.io.enq(0).bits, retryIndex, 7, cause)
        dut.io.enq(0).valid.poke(true.B)
        dut.clock.step()
        dut.io.enq(0).valid.poke(false.B)
        withClue(s"store wakeup after DM replay, cause=$cause, oldHint=$oldHint: ") { expectReplay(retryIndex) }
      }

      // A changed wait object must not inherit the earlier producer wakeup.
      val changedIndex = resetAndAllocate()
      addressWakeup(7)
      dut.clock.step()
      dut.io.storeAddrWakeup(0).valid.poke(false.B)
      request(dut.io.earlyWakeup(0).bits, changedIndex, 7)
      dut.io.earlyWakeup(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.earlyWakeup(0).valid.poke(false.B)
      request(dut.io.enq(0).bits, changedIndex, 8)
      dut.io.enq(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.enq(0).valid.poke(false.B)
      dut.clock.step(4)
      expectNoReplay()
      addressWakeup(8)
      dut.clock.step()
      dut.io.storeAddrWakeup(0).valid.poke(false.B)
      expectReplay(changedIndex)

      // An S2-only accepted store write must unblock FF even without a later issue wakeup.
      val ffIndex = resetAndAllocate()
      request(dut.io.earlyWakeup(0).bits, ffIndex, 7, LoadReplayCauses.C_FF)
      dut.io.earlyWakeup(0).valid.poke(true.B)
      dut.io.storeDataWrite(0).valid.poke(true.B)
      dut.io.storeDataWrite(0).bits.value.poke(7.U)
      dut.clock.step()
      dut.io.earlyWakeup(0).valid.poke(false.B)
      dut.io.storeDataWrite(0).valid.poke(false.B)
      request(dut.io.enq(0).bits, ffIndex, 7, LoadReplayCauses.C_FF)
      dut.io.enq(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.enq(0).valid.poke(false.B)
      expectReplay(ffIndex)

      // The fast TLB hint is a pulse, with no S3 or delayed-hint duplicate.
      val tmIndex = resetAndAllocate()
      request(dut.io.earlyWakeup(0).bits, tmIndex, 0, LoadReplayCauses.C_TM)
      dut.io.earlyWakeup(0).bits.rep_info.tlb_id.poke(3.U)
      dut.io.earlyWakeup(0).valid.poke(true.B)
      dut.io.fast_tlb_hint.valid.poke(true.B)
      dut.io.fast_tlb_hint.bits.id.poke(3.U)
      dut.clock.step()
      dut.io.earlyWakeup(0).valid.poke(false.B)
      dut.io.fast_tlb_hint.valid.poke(false.B)
      request(dut.io.enq(0).bits, tmIndex, 0, LoadReplayCauses.C_TM)
      dut.io.enq(0).bits.rep_info.tlb_id.poke(3.U)
      dut.io.enq(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.enq(0).valid.poke(false.B)
      expectReplay(tmIndex)

      // An S2 hint selects the first beat in S3 and the other beat one cycle later.
      for (lastBeat <- Seq(false, true)) {
        val dmIndex = resetAndAllocate()
        request(dut.io.earlyWakeup(0).bits, dmIndex, 0, LoadReplayCauses.C_DM)
        dut.io.earlyWakeup(0).bits.handledByMSHR.poke(true.B)
        dut.io.earlyWakeup(0).bits.rep_info.mshr_id.poke(2.U)
        dut.io.earlyWakeup(0).bits.rep_info.last_beat.poke(lastBeat.B)
        dut.io.earlyWakeup(0).valid.poke(true.B)
        dut.io.l2_hint(0).valid.poke(true.B)
        dut.io.l2_hint(0).bits.sourceId.poke(2.U)
        dut.io.l2_hint(0).bits.isKeyword.poke(false.B)
        dut.clock.step()
        dut.io.earlyWakeup(0).valid.poke(false.B)
        dut.io.l2_hint(0).valid.poke(false.B)
        request(dut.io.enq(0).bits, dmIndex, 0, LoadReplayCauses.C_DM)
        dut.io.enq(0).bits.handledByMSHR.poke(true.B)
        dut.io.enq(0).bits.rep_info.mshr_id.poke(2.U)
        dut.io.enq(0).bits.rep_info.last_beat.poke(lastBeat.B)
        dut.io.enq(0).valid.poke(true.B)
        dut.clock.step()
        dut.io.enq(0).valid.poke(false.B)
        if (lastBeat) {
          expectNoReplay()
          dut.clock.step()
        }
        withClue(s"S2 L2 hint beat, lastBeat=$lastBeat: ") { expectReplayNow(dmIndex) }
      }

      // Cause changes cannot reuse the old readiness even when the SQ index matches.
      val changedCauseIndex = resetAndAllocate()
      addressWakeup(7)
      dut.clock.step()
      dut.io.storeAddrWakeup(0).valid.poke(false.B)
      request(dut.io.earlyWakeup(0).bits, changedCauseIndex, 7)
      dut.io.earlyWakeup(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.earlyWakeup(0).valid.poke(false.B)
      request(dut.io.enq(0).bits, changedCauseIndex, 7, LoadReplayCauses.C_FF)
      dut.io.enq(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.enq(0).valid.poke(false.B)
      dut.clock.step(4)
      expectNoReplay()

      // Redirect cancels the early notification and owner together.
      val redirectedIndex = resetAndAllocate()
      request(dut.io.earlyWakeup(0).bits, redirectedIndex, 7)
      dut.io.earlyWakeup(0).valid.poke(true.B)
      dut.io.redirect.valid.poke(true.B)
      dut.io.redirect.bits.robIdx.isFormer.poke(true.B)
      dut.clock.step()
      dut.io.earlyWakeup(0).valid.poke(false.B)
      dut.io.redirect.valid.poke(false.B)
      dut.clock.step(5)
      expectNoReplay()
      dut.io.creditUpdate.returned.expect(0.U)

      // Successful fastReplay has no S3 result; its wakeup must expire before another attempt.
      val staleIndex = resetAndAllocate()
      addressWakeup(7)
      dut.clock.step()
      dut.io.storeAddrWakeup(0).valid.poke(false.B)
      request(dut.io.earlyWakeup(0).bits, staleIndex, 7)
      dut.io.earlyWakeup(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.earlyWakeup(0).valid.poke(false.B)
      dut.clock.step(3)
      request(dut.io.enq(0).bits, staleIndex, 7)
      dut.io.enq(0).valid.poke(true.B)
      dut.clock.step()
      dut.io.enq(0).valid.poke(false.B)
      dut.clock.step(4)
      expectNoReplay()
    }
  }
}

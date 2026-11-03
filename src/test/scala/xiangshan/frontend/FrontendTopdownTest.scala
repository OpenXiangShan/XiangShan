// Copyright (c) 2024-2025 Beijing Institute of Open Source Chip (BOSC)
// Copyright (c) 2020-2025 Institute of Computing Technology, Chinese Academy of Sciences
// Copyright (c) 2020-2021 Peng Cheng Laboratory
//
// XiangShan is licensed under Mulan PSL v2.
// You can use this software according to the terms and conditions of the Mulan PSL v2.
// You may obtain a copy of Mulan PSL v2 at:
//          https://license.coscl.org.cn/MulanPSL2
//
// THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND,
// EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT,
// MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.
//
// See the Mulan PSL v2 for more details.

package xiangshan.frontend

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import xiangshan.{TopDownCounters, XSTester}
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey}
import xiangshan.frontend.bpu._
import xiangshan.frontend.ibuffer.{IBuffer, IBufPtr}

class AttributionHarness(implicit p: Parameters) extends FrontendModule {
  val io = IO(new Bundle {
    val btb = Input(UInt(4.W))
    val directionSource = Input(UInt(4.W))
    val target = Input(UInt(4.W))
    val known = Input(Bool())
    val predTaken = Input(Bool())
    val predPosition = Input(UInt(CfiPositionWidth.W))
    val predType = Input(UInt(2.W))
    val taken = Input(Bool())
    val position = Input(UInt(CfiPositionWidth.W))
    val branchType = Input(UInt(2.W))
    val structuralFault = Input(Bool())
    val correctedKnown = Input(Bool())
    val result = Output(UInt(4.W))
    val overrideResult = Output(UInt(4.W))
    val replaced = Output(UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W))
  })
  val prediction = WireInit(0.U.asTypeOf(new Prediction))
  prediction.taken := io.predTaken
  prediction.cfiPosition := io.predPosition
  prediction.attribute.branchType := io.predType
  val attribute = WireInit(0.U.asTypeOf(new BranchAttribute))
  attribute.branchType := io.branchType
  io.result := BlameBpuSource.fields(prediction, io.taken, io.position, attribute,
    io.known && Mux(BlameBpuSource.earlierPrediction(prediction, io.taken, io.position),
      prediction.attribute.isConditional, attribute.isConditional),
    io.directionSource, io.target, io.btb, io.structuralFault)
  io.overrideResult := BlameBpuSource.fields(prediction, io.taken, io.position, attribute,
    io.known && prediction.attribute.isConditional, io.directionSource, io.target, io.btb,
    io.structuralFault || (BlameBpuSource.earlierPrediction(prediction, io.taken, io.position) && !io.correctedKnown))
  val old = Wire(new FrontendTopDownBundle)
  old.setReason(TopDownCounters.ICacheMissBubble)
  val event = Wire(new FrontendRedirectTopdown)
  event.valid := true.B
  event.bits.setPredictionReason(io.result)
  old.redirectOverride(event)
  io.replaced := old.reason

}

// Drive the real FTQ: training comes through its real resolve queue/cache.
class FtqSupplyHarness(implicit p: Parameters) extends FrontendModule {
  val io = IO(new Bundle {
    val predict = Input(Bool())
    val fetchReady = Input(Bool())
    val trainReady = Input(Bool())
    val resolve = Input(Bool())
    val predictionReady = Output(Bool())
    val fetchValid = Output(Bool())
    val trainValid = Output(Bool())
    val reason = Output(UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W))
  })
  private val testCoreParams = p(xiangshan.XSCoreParamsKey)
  private val testParams = p.alterPartial {
    case xiangshan.XSCoreParamsKey => testCoreParams
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, utility.XSPerfLevel.NORMAL, 0)
  }
  val ftq = Module(new xiangshan.frontend.ftq.Ftq()(testParams))
  ftq.io.fromBpu.prediction.bits := 0.U.asTypeOf(ftq.io.fromBpu.prediction.bits)
  ftq.io.fromBpu.meta.valid := false.B
  ftq.io.fromBpu.meta.bits := 0.U.asTypeOf(ftq.io.fromBpu.meta.bits)
  ftq.io.fromBpu.perfMeta := 0.U.asTypeOf(ftq.io.fromBpu.perfMeta)
  ftq.io.fromBpu.s2FtqPtr := 0.U.asTypeOf(ftq.io.fromBpu.s2FtqPtr)
  ftq.io.fromBpu.s3FtqPtr := 0.U.asTypeOf(ftq.io.fromBpu.s3FtqPtr)
  ftq.io.fromBpu.topdownReasons := 0.U.asTypeOf(ftq.io.fromBpu.topdownReasons)
  ftq.io.fromBpu.overrideReasons := 0.U.asTypeOf(ftq.io.fromBpu.overrideReasons)
  ftq.io.fromBpu.prediction.valid := io.predict
  ftq.io.fromBpu.prediction.bits.endPosition := 1.U
  ftq.io.toBpu.train.ready := io.trainReady
  ftq.io.fromICache := 0.U.asTypeOf(ftq.io.fromICache)
  ftq.io.toICache.toPrefetch.ready := false.B
  ftq.io.toICache.toMainPipe.ready := io.fetchReady
  ftq.io.fromIfu := 0.U.asTypeOf(ftq.io.fromIfu)
  ftq.io.fromBackend := 0.U.asTypeOf(ftq.io.fromBackend)
  ftq.io.fromBackend.resolve(0).valid := io.resolve
  ftq.io.fromBackend.resolve(0).bits.attribute := BranchAttribute.Conditional
  io.predictionReady := ftq.io.fromBpu.prediction.ready
  io.fetchValid := ftq.io.toICache.toMainPipe.valid
  io.trainValid := ftq.io.toBpu.train.valid
  io.reason := ftq.io.toIfu.topdownInfo.reason
}

class IBufferTopdownHarness(implicit p: Parameters) extends FrontendModule {
  val io = IO(new Bundle {
    val count = Input(UInt(log2Ceil(IBufferEnqueueWidth + 1).W))
    val enqueue = Input(Bool())
    val accept = Input(Bool())
    val resume = Input(Bool())
    val vset = Input(Bool())
    val uncache = Input(Bool())
    val flush = Input(Bool())
    val redirect = Input(Bool())
    val upstream = Input(UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W))
    val validCount = Output(UInt(log2Ceil(DecodeWidth + 1).W))
    val reasons = Output(Vec(DecodeWidth, UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W)))
  })
  private val testCoreParams = p(xiangshan.XSCoreParamsKey)
  private val bufferParams = p.alterPartial {
    case xiangshan.XSCoreParamsKey => testCoreParams
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, utility.XSPerfLevel.NORMAL, 0)
  }
  val buffer = Module(new IBuffer()(bufferParams))
  val ptr = RegInit(0.U.asTypeOf(new IBufPtr))
  val offset = ptr.value(log2Ceil(frontendParameters.ibufferParameters.NumWriteBank) - 1, 0)
  buffer.io.in.valid := io.enqueue
  buffer.io.in.bits := 0.U.asTypeOf(buffer.io.in.bits)
  buffer.io.in.bits.prevIBufEnqPtr := ptr
  buffer.io.in.bits.uncacheFetch := io.uncache
  val enables = VecInit.tabulate(IBufferEnqueueWidth)(i => i.U >= offset && i.U < (offset +& io.count))
  buffer.io.in.bits.valid := enables.asUInt
  buffer.io.in.bits.enqEnable := enables.asUInt
  for (i <- 0 until IBufferEnqueueWidth) {
    buffer.io.in.bits.instrs(i) := Mux(io.vset, "h00007057".U, "h00000013".U)
  }
  buffer.io.in.bits.topdownInfo.reason := io.upstream
  when(buffer.io.in.fire) { ptr := ptr + io.count }
  when(io.flush) { ptr := 0.U.asTypeOf(new IBufPtr) }
  buffer.io.fromBackend := 0.U.asTypeOf(buffer.io.fromBackend)
  buffer.io.fromBackend.decodeCanAccept := io.accept
  buffer.io.fromBackend.resumingVType := io.resume
  buffer.io.out.foreach(_.ready := io.accept)
  buffer.io.flush := io.flush
  buffer.io.redirectTopdown.valid := io.redirect
  buffer.io.redirectTopdown.bits.setReason(TopDownCounters.MemVioRedirectBubble)
  io.validCount := PopCount(buffer.io.out.map(_.valid))
  io.reasons := buffer.io.stallReason.reason
}

class IfuTopdownHarness(implicit p: Parameters) extends FrontendModule {
  val io = IO(new Bundle {
    val s1Ready = Input(Bool())
    val s2Ready = Input(Bool())
    val supplyStall = Input(Bool())
    val cacheMiss = Input(Bool())
    val tlbMiss = Input(Bool())
    val flushS0 = Input(Bool())
    val flushS1 = Input(Bool())
    val uncache = Input(Bool())
    val redirect = Input(Bool())
    val reason = Output(UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W))
  })
  private val perfParams = p.alterPartial {
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, utility.XSPerfLevel.NORMAL, 0)
  }
  val perf = Module(new xiangshan.frontend.ifu.IfuPerfAnalysis()(perfParams))
  perf.io.ifuPerfCtrl := 0.U.asTypeOf(perf.io.ifuPerfCtrl)
  perf.io.ifuPerfCtrl.s1Ready := io.s1Ready
  perf.io.ifuPerfCtrl.s2Ready := io.s2Ready
  perf.io.perfInfo := 0.U.asTypeOf(perf.io.perfInfo)
  perf.io.topdownIn := 0.U.asTypeOf(perf.io.topdownIn)
  perf.io.topdownIn.ftqTopdown.setReason(TopDownCounters.FtqUpdateBubble)
  perf.io.topdownIn.overrideReasons.setPredictionReason(BpuPredictionOrigin.UTAGE)
  perf.io.topdownIn.icacheTopdown.supplyStall := io.supplyStall
  perf.io.topdownIn.icacheTopdown.iCacheMissBubble := io.cacheMiss
  perf.io.topdownIn.icacheTopdown.itlbMissBubble := io.tlbMiss
  perf.io.topdownIn.icacheTopdown.bpuFlushS0 := io.flushS0
  perf.io.topdownIn.icacheTopdown.bpuFlushS1 := io.flushS1
  perf.io.ifuPerfCtrl.uncacheActive := io.uncache
  perf.io.topdownIn.redirectTopdown.valid := io.redirect
  perf.io.topdownIn.redirectTopdown.bits.setReason(TopDownCounters.MemVioRedirectBubble)
  io.reason := perf.io.topdownOut.topdown.reason
}

class FrontendTopdownTest extends XSTester {
  utility.Constantin.init(false)
  utility.ChiselDB.init(false)

  it should "attribute existence, type, direction including NT, and target to the field provider" in {
    simulate(new AttributionHarness) { d =>
      d.io.structuralFault.poke(false.B)
      d.io.correctedKnown.poke(true.B)
      d.io.btb.poke(6.U); d.io.directionSource.poke(7.U); d.io.target.poke(8.U)
      d.io.known.poke(false.B); d.io.predTaken.poke(false.B); d.io.predPosition.poke(8.U)
      d.io.predType.poke(0.U); d.io.taken.poke(true.B); d.io.position.poke(2.U); d.io.branchType.poke(1.U)
      d.io.result.expect(6.U) // missing conditional belongs to ABTB
      d.io.known.poke(true.B)
      d.io.result.expect(7.U) // known NT conditional belongs to micro-TAGE
      d.io.replaced.expect(TopDownCounters.UTAGEMissBubble.id.U) // old cache reason fully cleared
      d.io.predTaken.poke(true.B); d.io.predPosition.poke(2.U); d.io.predType.poke(1.U)
      d.io.result.expect(8.U) // matching branch, target from micro-RAS
      d.io.taken.poke(false.B)
      d.io.result.expect(7.U) // false taken direction
      d.io.overrideResult.expect(7.U) // correcting predictor still knows the conditional
      d.io.correctedKnown.poke(false.B)
      d.io.overrideResult.expect(6.U) // correcting predictor found a spurious BTB branch
      d.io.correctedKnown.poke(true.B)
      d.io.branchType.poke(0.U); d.io.structuralFault.poke(true.B)
      d.io.result.expect(6.U) // precheck found non-CFI; direction predictor is not responsible
      d.io.structuralFault.poke(false.B)
      d.io.branchType.poke(2.U); d.io.taken.poke(true.B)
      d.io.result.expect(6.U) // type mismatch
      d.io.btb.poke(0.U); d.io.directionSource.poke(4.U); d.io.target.poke(3.U)
      d.io.branchType.poke(1.U); d.io.predTaken.poke(false.B)
      d.io.result.expect(4.U) // SC supplied not-taken
      d.io.replaced.expect(TopDownCounters.SCMissBubble.id.U)
      d.io.predTaken.poke(true.B)
      d.io.result.expect(3.U) // ITTAGE supplied target
      d.io.taken.poke(false.B); d.io.position.poke(8.U); d.io.branchType.poke(0.U)
      d.io.result.expect(4.U) // an override to fallthrough blames the earlier false-taken direction
      d.io.taken.poke(true.B); d.io.position.poke(2.U); d.io.branchType.poke(1.U)
      d.io.known.poke(false.B); d.io.predTaken.poke(false.B)
      d.io.result.expect(0.U) // MBTB missed the branch
    }
  }

  it should "attribute actual FTQ training recovery and capacity holes without charging buffered backpressure" in {
    simulate(new FtqSupplyHarness) { d =>
      def flag(v: Bool): Boolean = v.peek().litToBoolean
      def initialize(): Unit = {
        d.io.predict.poke(false.B); d.io.fetchReady.poke(false.B)
        d.io.trainReady.poke(true.B); d.io.resolve.poke(false.B)
        d.reset.poke(true.B); d.clock.step(2); d.reset.poke(false.B); d.clock.step()
      }
      initialize()
      d.io.reason.expect(TopDownCounters.FtqEmptyBubble.id.U)
      d.io.predict.poke(true.B); d.clock.step(6)
      d.io.trainReady.poke(false.B); d.io.resolve.poke(true.B); d.clock.step()
      d.io.resolve.poke(false.B); d.io.predict.poke(false.B)
      var limit = 0
      while (!flag(d.io.trainValid) && limit < 8) { d.clock.step(); limit += 1 }
      d.io.trainValid.expect(true.B)
      d.clock.step(8); d.io.predictionReady.expect(false.B)
      d.io.predict.poke(true.B); d.clock.step()
      d.io.fetchValid.expect(true.B); d.io.reason.expect(TopDownCounters.NoStall.id.U)
      d.io.predict.poke(false.B); d.io.trainReady.poke(true.B); d.io.fetchReady.poke(true.B)
      d.clock.step(12)
      d.io.fetchValid.expect(false.B); d.io.reason.expect(TopDownCounters.FtqUpdateBubble.id.U)
      d.io.predict.poke(true.B); d.clock.step()
      d.io.predict.poke(false.B); d.clock.step(3)
      d.io.reason.expect(TopDownCounters.FtqEmptyBubble.id.U)

      initialize()
      d.io.predict.poke(true.B)
      var allocated = 0
      while (flag(d.io.predictionReady) && allocated < 64) { d.clock.step(); allocated += 1 }
      assert(allocated == 32) // configured prefetch distance, with all targets still buffered
      d.clock.step(2); d.io.reason.expect(TopDownCounters.NoStall.id.U)
      d.io.fetchReady.poke(true.B)
      limit = 0
      while (allocated < 64 && limit < 100) {
        if (flag(d.io.predictionReady)) allocated += 1
        d.clock.step(); limit += 1
      }
      assert(allocated == 64)
      d.io.fetchReady.poke(false.B); d.clock.step(2)
      d.io.predictionReady.expect(false.B); d.io.fetchValid.expect(true.B)
      d.io.reason.expect(TopDownCounters.NoStall.id.U) // enqueue is full, fetch can still consume targets
      d.io.fetchReady.poke(true.B); d.clock.step(40)
      d.io.fetchValid.expect(false.B); d.io.reason.expect(TopDownCounters.FtqFullStall.id.U)
    }
  }

  it should "replace overlapping IFU waits and local flushes with the global redirect reason" in {
    simulate(new IfuTopdownHarness) { d =>
      d.io.s1Ready.poke(true.B); d.io.s2Ready.poke(true.B); d.io.supplyStall.poke(false.B)
      d.io.cacheMiss.poke(false.B); d.io.tlbMiss.poke(false.B)
      d.io.flushS0.poke(false.B); d.io.flushS1.poke(false.B)
      d.io.uncache.poke(false.B); d.io.redirect.poke(false.B)
      d.reset.poke(true.B); d.clock.step(2); d.reset.poke(false.B); d.clock.step(3)
      d.io.reason.expect(TopDownCounters.FtqUpdateBubble.id.U)
      // A local S0 override reaches the output three edges later and preserves older slots.
      d.io.flushS0.poke(true.B); d.clock.step()
      d.io.reason.expect(TopDownCounters.FtqUpdateBubble.id.U)
      d.io.flushS0.poke(false.B); d.clock.step()
      d.io.reason.expect(TopDownCounters.FtqUpdateBubble.id.U)
      d.clock.step(); d.io.reason.expect(TopDownCounters.UTAGEMissBubble.id.U)
      d.clock.step(); d.io.reason.expect(TopDownCounters.FtqUpdateBubble.id.U)
      d.io.tlbMiss.poke(true.B); d.clock.step(3)
      d.io.reason.expect(TopDownCounters.ITLBMissBubble.id.U)
      d.io.cacheMiss.poke(true.B); d.clock.step(2)
      d.io.reason.expect(TopDownCounters.ICacheMissBubble.id.U)
      d.io.flushS1.poke(true.B); d.clock.step(2)
      d.io.reason.expect(TopDownCounters.UTAGEMissBubble.id.U)
      d.io.uncache.poke(true.B)
      d.io.reason.expect(TopDownCounters.UncacheFetchBubble.id.U)
      d.io.redirect.poke(true.B)
      d.io.reason.expect(TopDownCounters.MemVioRedirectBubble.id.U); d.clock.step()
      d.io.redirect.poke(false.B); d.io.uncache.poke(false.B)
      d.io.cacheMiss.poke(false.B); d.io.tlbMiss.poke(false.B); d.io.flushS1.poke(false.B)
      d.clock.step(); d.io.reason.expect(TopDownCounters.MemVioRedirectBubble.id.U)
      d.clock.step(3); d.io.reason.expect(TopDownCounters.FtqUpdateBubble.id.U)
      d.io.supplyStall.poke(true.B); d.clock.step(3)
      d.io.reason.expect(TopDownCounters.ICacheSupplyBubble.id.U)
      d.io.s1Ready.poke(false.B); d.io.s2Ready.poke(false.B); d.io.supplyStall.poke(false.B)
      d.clock.step(5); d.io.reason.expect(TopDownCounters.ICacheSupplyBubble.id.U)
      d.io.s1Ready.poke(true.B); d.io.s2Ready.poke(true.B); d.clock.step(2)
      d.io.reason.expect(TopDownCounters.FtqUpdateBubble.id.U)
    }
  }

  it should "retain an upstream override when an enqueue also has local packing waste" in {
    simulate(new IBufferTopdownHarness) { d =>
      d.io.count.poke(3.U); d.io.enqueue.poke(true.B); d.io.accept.poke(false.B)
      d.io.resume.poke(false.B); d.io.vset.poke(false.B); d.io.uncache.poke(false.B)
      d.io.flush.poke(false.B); d.io.redirect.poke(false.B)
      d.io.upstream.poke(TopDownCounters.FtqEmptyBubble.id.U)
      d.reset.poke(true.B); d.clock.step(2); d.reset.poke(false.B)
      d.clock.step() // enqueue into the banked array; bypass disabled by decode backpressure
      d.io.upstream.poke(TopDownCounters.UTAGEMissBubble.id.U)
      d.clock.step() // dequeue three into the output register, with another enqueue
      d.io.validCount.expect(3.U)
      d.io.reasons(3).expect(TopDownCounters.UTAGEMissBubble.id.U)
    }
  }

  it should "align IBuffer short bypass, dequeue structure, holds, VType, uncache and redirects by one cycle" in {
    simulate(new IBufferTopdownHarness) { d =>
      d.io.count.poke(0.U); d.io.enqueue.poke(false.B); d.io.accept.poke(true.B)
      d.io.resume.poke(false.B); d.io.vset.poke(false.B); d.io.uncache.poke(false.B)
      d.io.flush.poke(false.B); d.io.redirect.poke(false.B)
      d.io.upstream.poke(TopDownCounters.ICacheMissBubble.id.U)
      d.reset.poke(true.B); d.clock.step(2); d.reset.poke(false.B); d.clock.step()
      d.io.validCount.expect(0.U); d.io.reasons(0).expect(TopDownCounters.ICacheMissBubble.id.U)
      d.io.enqueue.poke(true.B); d.io.count.poke(0.U); d.clock.step()
      d.io.validCount.expect(0.U); d.io.reasons(0).expect(TopDownCounters.FetchFragBubble.id.U)
      d.io.count.poke(3.U); d.clock.step()
      d.io.validCount.expect(3.U); d.io.reasons(2).expect(TopDownCounters.NoStall.id.U)
      d.io.reasons(3).expect(TopDownCounters.FetchFragBubble.id.U)
      d.io.enqueue.poke(false.B); d.io.accept.poke(false.B); d.clock.step()
      d.io.validCount.expect(3.U); d.io.reasons(3).expect(TopDownCounters.ICacheMissBubble.id.U)
      d.io.resume.poke(true.B); d.io.upstream.poke(TopDownCounters.ITLBMissBubble.id.U); d.clock.step()
      d.io.reasons(3).expect(TopDownCounters.ICacheMissBubble.id.U)
      d.io.resume.poke(false.B); d.io.accept.poke(true.B); d.io.enqueue.poke(true.B)
      d.io.uncache.poke(true.B); d.io.count.poke(1.U); d.clock.step()
      d.io.validCount.expect(1.U); d.io.reasons(1).expect(TopDownCounters.UncacheFetchBubble.id.U)
      d.io.uncache.poke(false.B); d.io.vset.poke(true.B); d.io.count.poke(3.U); d.clock.step()
      d.io.validCount.expect(0.U); d.io.reasons(0).expect(TopDownCounters.IbufferStructure.id.U)
      d.io.vset.poke(false.B); d.io.count.poke(2.U); d.clock.step()
      d.io.validCount.expect(3.U); d.io.reasons(3).expect(TopDownCounters.IbufferStructure.id.U)
      d.io.enqueue.poke(false.B); d.clock.step()
      d.io.validCount.expect(2.U); d.io.reasons(2).expect(TopDownCounters.ITLBMissBubble.id.U)
      d.io.flush.poke(true.B); d.io.redirect.poke(true.B); d.clock.step()
      d.io.validCount.expect(0.U); d.io.reasons(0).expect(TopDownCounters.MemVioRedirectBubble.id.U)
      // Functional flush can follow the performance broadcast by one edge.
      d.io.redirect.poke(false.B); d.io.upstream.poke(TopDownCounters.MemVioRedirectBubble.id.U)
      d.clock.step(); d.io.reasons(0).expect(TopDownCounters.MemVioRedirectBubble.id.U)
      d.io.upstream.poke(TopDownCounters.ITLBMissBubble.id.U)
      d.io.flush.poke(false.B); d.io.redirect.poke(false.B); d.clock.step()
      d.io.reasons(0).expect(TopDownCounters.ITLBMissBubble.id.U)
      d.io.accept.poke(false.B); d.io.enqueue.poke(true.B); d.io.count.poke(3.U); d.clock.step()
      d.io.enqueue.poke(false.B); d.clock.step()
      d.io.validCount.expect(3.U)
      d.io.enqueue.poke(true.B); d.io.count.poke(2.U); d.clock.step()
      d.io.validCount.expect(3.U)
      d.io.enqueue.poke(false.B); d.clock.step()
      d.io.validCount.expect(5.U) // append into held output register
      d.io.accept.poke(true.B); d.io.enqueue.poke(true.B)
      d.io.count.poke(config(xiangshan.XSCoreParamsKey).DecodeWidth.U); d.clock.step()
      d.io.validCount.expect(config(xiangshan.XSCoreParamsKey).DecodeWidth.U)
      d.io.reasons.foreach(_.expect(TopDownCounters.NoStall.id.U))
      // A cacheable block can complete one uncache instruction, followed by ordinary instructions.
      d.io.flush.poke(true.B); d.io.enqueue.poke(false.B); d.clock.step()
      d.io.flush.poke(false.B); d.io.enqueue.poke(true.B); d.io.uncache.poke(true.B)
      d.io.count.poke((config(xiangshan.XSCoreParamsKey).DecodeWidth + 1).U); d.clock.step()
      d.io.validCount.expect(config(xiangshan.XSCoreParamsKey).DecodeWidth.U)
      d.io.enqueue.poke(false.B); d.io.uncache.poke(false.B); d.clock.step()
      d.io.validCount.expect(1.U)
      d.io.reasons.drop(1).foreach(_.expect(TopDownCounters.ITLBMissBubble.id.U))
    }
  }
}

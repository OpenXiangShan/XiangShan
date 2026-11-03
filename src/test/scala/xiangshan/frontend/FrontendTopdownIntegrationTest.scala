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
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey}
import xiangshan.{TopDownCounters, XSTester}
import xiangshan.frontend.bpu.{BpuPredictionOrigin, BranchAttribute}
import xiangshan.frontend.ftq.{Ftq, FtqPtr}
import xiangshan.frontend.ibuffer.IBuffer
import xiangshan.frontend.icache.HasICacheParameters
import xiangshan.frontend.ifu.Ifu

// Exercise production FTQ redirect arbitration, IFU/uncache FSM and IBuffer together.
// Only the predictor and external instruction memory response interfaces are driven by the test.
class FrontendTopdownIntegrationHarness(implicit p: Parameters) extends FrontendModule with HasICacheParameters {
  val io = IO(new Bundle {
    val allocate = Input(Bool())
    val index = Input(UInt(log2Ceil(FtqSize).W))
    val fetch = Input(Bool())
    val pc = Input(UInt(GuardedVAddrBits.W))
    val size = Input(UInt(log2Ceil(FetchBlockInstNum + 1).W))
    val mmio = Input(Bool())
    val cacheHalf = Input(UInt(16.W))
    val fetchReady = Output(Bool())
    val requestReady = Input(Bool())
    val requestValid = Output(Bool())
    val requestAddress = Output(UInt(PAddrBits.W))
    val response = Input(Bool())
    val responseData = Input(UInt(32.W))
    val resend = Input(Bool())
    val downstreamEmpty = Input(Bool())
    val decodeAccept = Input(Bool())
    val backendAhead = Input(Bool())
    val backendRedirect = Input(Bool())
    val backendCtrl = Input(Bool())
    val backendMemVio = Input(Bool())
    val predictedTaken = Input(Bool())
    val redirectPosition = Input(UInt(CfiPositionWidth.W))
    val redirectTaken = Input(Bool())
    val ifuRedirect = Output(Bool())
    val ifuUncache = Output(Bool())
    val ifuCanTrain = Output(Bool())
    val target = Output(UInt(GuardedVAddrBits.W))
    val supplyReason = Output(UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W))
    val supplyReasonCount = Output(UInt(log2Ceil(TopDownCounters.NumStallReasons.id + 1).W))
    val fetchReason = Output(UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W))
    val delivery = Output(Bool())
    val deliveryCount = Output(UInt(log2Ceil(IBufferEnqueueWidth + 1).W))
    val outputCount = Output(UInt(log2Ceil(DecodeWidth + 1).W))
    val outputInstruction = Output(UInt(32.W))
    val outputPc = Output(UInt(VAddrBits.W))
    val reasons = Output(Vec(DecodeWidth, UInt(log2Ceil(TopDownCounters.NumStallReasons.id).W)))
  })
  // XSTester's XSCoreParamsKey handler copies the core on every lookup. Freeze the
  // same configuration once before constructing the many FTQ metadata bundles.
  private val testCoreParams = p(xiangshan.XSCoreParamsKey)
  private val testParams = p.alterPartial {
    case xiangshan.XSCoreParamsKey => testCoreParams
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, utility.XSPerfLevel.NORMAL, 0)
  }
  val ftq = Module(new Ftq()(testParams))
  val ifu = Module(new Ifu()(testParams))
  val buffer = Module(new IBuffer()(testParams))

  ftq.io.fromBpu.prediction.valid := io.allocate
  ftq.io.fromBpu.prediction.bits := 0.U.asTypeOf(ftq.io.fromBpu.prediction.bits)
  ftq.io.fromBpu.prediction.bits.startPc := GuardedPcInit(io.pc)
  ftq.io.fromBpu.prediction.bits.endPosition := io.size - 1.U
  ftq.io.fromBpu.s2FtqPtr := 0.U.asTypeOf(new FtqPtr)
  ftq.io.fromBpu.s3FtqPtr := 0.U.asTypeOf(new FtqPtr)
  ftq.io.fromBpu.s3FtqPtr.value := io.index
  ftq.io.fromBpu.meta.valid := io.allocate
  ftq.io.fromBpu.meta.bits := 0.U.asTypeOf(ftq.io.fromBpu.meta.bits)
  ftq.io.fromBpu.perfMeta := 0.U.asTypeOf(ftq.io.fromBpu.perfMeta)
  val prediction = ftq.io.fromBpu.perfMeta.s3Prediction
  prediction.taken := io.predictedTaken
  prediction.cfiPosition := io.size - 1.U
  prediction.attribute := BranchAttribute.Conditional
  val branchMeta = ftq.io.fromBpu.perfMeta.mbtbMeta.entries(0)(0)
  branchMeta.rawHit := true.B
  branchMeta.position := io.size - 1.U
  branchMeta.attribute := BranchAttribute.Conditional
  ftq.io.fromBpu.perfMeta.tageUsed := 1.U
  ftq.io.fromBpu.topdownReasons := 0.U.asTypeOf(ftq.io.fromBpu.topdownReasons)
  ftq.io.fromBpu.overrideReasons.setPredictionReason(BpuPredictionOrigin.UTAGE)
  ftq.io.toBpu.train.ready := true.B
  ftq.io.fromICache := 0.U.asTypeOf(ftq.io.fromICache)
  ftq.io.toICache.toPrefetch.ready := true.B
  ftq.io.toICache.toMainPipe.ready := true.B
  ftq.io.fromBackend := 0.U.asTypeOf(ftq.io.fromBackend)
  ftq.io.fromBackend.ftqIdxAhead.valid := io.backendAhead
  ftq.io.fromBackend.ftqIdxAhead.bits.value := io.index
  ftq.io.fromBackend.redirect.valid := io.backendRedirect
  ftq.io.fromBackend.redirect.bits.ftqIdx.value := io.index
  ftq.io.fromBackend.redirect.bits.pc := io.pc(VAddrBits - 1, 0)
  ftq.io.fromBackend.redirect.bits.ftqOffset := io.redirectPosition
  ftq.io.fromBackend.redirect.bits.taken := io.redirectTaken
  ftq.io.fromBackend.redirect.bits.attribute := BranchAttribute.Conditional
  ftq.io.fromBackend.redirect.bits.debugIsCtrl := io.backendCtrl
  ftq.io.fromBackend.redirect.bits.debugIsMemVio := io.backendMemVio
  ftq.io.fromIfu <> ifu.io.toFtq
  ifu.io.fromFtq <> ftq.io.toIfu
  ifu.io.redirectTopdown := ftq.io.redirectTopdown
  buffer.io.redirectTopdown := ftq.io.redirectTopdown
  buffer.io.flush := ftq.io.toIfu.redirect.valid

  ifu.io.fromICache.req.valid := io.fetch
  ifu.io.fromICache.req.bits := 0.U.asTypeOf(ifu.io.fromICache.req.bits)
  val info = ifu.io.fromICache.req.bits.info(0)
  info.valid := true.B
  info.startVAddr := GuardedPcInit(io.pc)
  info.ftqIdx.value := io.index
  info.size := io.size
  info.takenCfiOffset.valid := io.predictedTaken
  info.takenCfiOffset.bits := io.size - 1.U
  info.data := Fill(blockBits / 16, io.cacheHalf)
  info.icacheMeta.pAddr := PrunedAddrInit(io.pc.pad(PAddrBits)(PAddrBits - 1, 0))
  info.icacheMeta.pmpMmio := io.mmio
  ifu.io.fromICache.req.bits.firstRange := UIntToOH(io.size, FetchBlockInstNum + 1) - 1.U
  ifu.io.fromICache.req.bits.totalRange := ifu.io.fromICache.req.bits.firstRange
  ifu.io.fromICache.req.bits.maybeRvcMap := Fill(FetchBlockInstNum, io.cacheHalf(1, 0) =/= 3.U)
  ifu.io.fromICache.corrupt := 0.U.asTypeOf(ifu.io.fromICache.corrupt)
  ifu.io.fromICache.topdown := 0.U.asTypeOf(ifu.io.fromICache.topdown)
  ifu.io.fromICache.perf := 0.U.asTypeOf(ifu.io.fromICache.perf)
  ifu.io.fromICache.fetchReady := true.B
  ifu.io.frontendTrigger := 0.U.asTypeOf(ifu.io.frontendTrigger)
  ifu.io.csrFsIsOff := false.B
  ifu.io.backendEmpty := io.downstreamEmpty
  ifu.io.ibufferEmpty := buffer.io.empty
  ifu.io.toUncache.req.ready := io.requestReady
  ifu.io.fromUncache.resp.valid := io.response
  ifu.io.fromUncache.resp.bits := 0.U.asTypeOf(ifu.io.fromUncache.resp.bits)
  ifu.io.fromUncache.resp.bits.data := io.responseData
  ifu.io.fromUncache.resp.bits.needResend := io.resend
  buffer.io.in <> ifu.io.toIBuffer
  buffer.io.fromBackend := 0.U.asTypeOf(buffer.io.fromBackend)
  buffer.io.fromBackend.decodeCanAccept := io.decodeAccept
  buffer.io.out.foreach(_.ready := io.decodeAccept)

  io.fetchReady := ifu.io.fromICache.req.ready
  io.requestValid := ifu.io.toUncache.req.valid
  io.requestAddress := ifu.io.toUncache.req.bits.addr.toUInt
  io.ifuRedirect := ifu.io.toFtq.wbRedirect.valid
  io.ifuUncache := ifu.io.toFtq.wbRedirect.bits.isUncache
  io.ifuCanTrain := ifu.io.toFtq.wbRedirect.bits.canTrain
  io.target := ifu.io.toFtq.wbRedirect.bits.target
  io.supplyReason := ftq.io.redirectTopdown.bits.reason
  io.supplyReasonCount := Mux(ftq.io.redirectTopdown.valid, 1.U, 0.U)
  io.fetchReason := ifu.io.toIBuffer.bits.topdownInfo.reason
  io.delivery := ifu.io.toIBuffer.fire
  io.deliveryCount := PopCount(ifu.io.toIBuffer.bits.enqEnable)
  io.outputCount := PopCount(buffer.io.out.map(_.valid))
  io.outputInstruction := buffer.io.out.head.bits.instr
  io.outputPc := buffer.io.out.head.bits.pc
  io.reasons := buffer.io.stallReason.reason
}

class FrontendTopdownIntegrationTest extends XSTester {
  utility.Constantin.init(false)
  utility.ChiselDB.init(false)

  it should "carry real MMIO wait, return and cross-page resend attribution through FTQ IFU and IBuffer" in {
    simulate(new FrontendTopdownIntegrationHarness) { d =>
      def flag(x: Bool): Boolean = x.peek().litToBoolean
      def initialize(): Unit = {
        d.io.allocate.poke(false.B); d.io.index.poke(0.U); d.io.fetch.poke(false.B)
        d.io.pc.poke("h80000000".U); d.io.size.poke(2.U); d.io.mmio.poke(true.B)
        d.io.cacheHalf.poke(1.U); d.io.requestReady.poke(true.B)
        d.io.response.poke(false.B); d.io.responseData.poke(0x13.U); d.io.resend.poke(false.B)
        d.io.downstreamEmpty.poke(true.B); d.io.decodeAccept.poke(true.B)
        d.io.backendAhead.poke(false.B); d.io.backendRedirect.poke(false.B)
        d.io.backendCtrl.poke(false.B); d.io.backendMemVio.poke(false.B)
        d.io.predictedTaken.poke(false.B); d.io.redirectPosition.poke(1.U); d.io.redirectTaken.poke(false.B)
        d.reset.poke(true.B); d.clock.step(3); d.reset.poke(false.B); d.clock.step(2)
      }
      def fetch(pc: BigInt, index: Int, size: Int = 2): Unit = {
        d.io.pc.poke(pc.U); d.io.index.poke(index.U); d.io.size.poke(size.U)
        d.io.allocate.poke(true.B); d.clock.step(); d.io.allocate.poke(false.B)
        d.io.fetch.poke(true.B); d.io.fetchReady.expect(true.B)
        d.clock.step(); d.io.fetch.poke(false.B); d.clock.step()
      }
      def awaitRequest(pc: BigInt): Unit = {
        var cycles = 0
        while (!flag(d.io.requestValid) && cycles < 12) {
          d.io.fetchReason.expect(TopDownCounters.UncacheFetchBubble.id.U)
          d.io.delivery.expect(false.B); d.clock.step(); cycles += 1
        }
        d.io.requestValid.expect(true.B); d.io.requestAddress.expect(pc.U)
        d.clock.step()
        for (_ <- 0 until 4) {
          d.io.delivery.expect(false.B)
          d.io.fetchReason.expect(TopDownCounters.UncacheFetchBubble.id.U)
          d.clock.step()
        }
      }
      def respond(data: BigInt, resend: Boolean): Unit = {
        d.io.response.poke(true.B); d.io.responseData.poke(data.U); d.io.resend.poke(resend.B)
        d.clock.step(); d.io.response.poke(false.B)
        d.io.ifuRedirect.expect(true.B); d.io.ifuUncache.expect(true.B); d.io.ifuCanTrain.expect(false.B)
        d.io.supplyReason.expect(TopDownCounters.UncacheFetchBubble.id.U)
        d.io.supplyReasonCount.expect(1.U)
        d.io.fetchReason.expect(TopDownCounters.UncacheFetchBubble.id.U)
      }
      def delivered(pc: BigInt): Unit = {
        d.io.delivery.expect(true.B); d.io.deliveryCount.expect(1.U); d.clock.step()
        d.io.outputCount.expect(1.U); d.io.outputInstruction.expect(0x13.U); d.io.outputPc.expect(pc.U)
        d.io.reasons(0).expect(TopDownCounters.NoStall.id.U)
        d.io.reasons.drop(1).foreach(_.expect(TopDownCounters.UncacheFetchBubble.id.U))
        d.clock.step(3)
      }
      initialize()
      fetch(BigInt("80000000", 16), 0)
      awaitRequest(BigInt("80000000", 16)); respond(0x13, false)
      d.io.target.expect("h80000004".U); delivered(BigInt("80000000", 16))

      // WaitLastCommit on a subsequent MMIO fetch is a supply loss, even before its request is issued.
      d.io.downstreamEmpty.poke(false.B)
      fetch(BigInt("80000004", 16), 1)
      for (_ <- 0 until 5) {
        d.io.requestValid.expect(false.B); d.io.delivery.expect(false.B)
        d.io.fetchReason.expect(TopDownCounters.UncacheFetchBubble.id.U); d.clock.step()
      }
      d.io.downstreamEmpty.poke(true.B)
      awaitRequest(BigInt("80000004", 16)); respond(0x13, false)
      delivered(BigInt("80000004", 16))

      initialize()
      fetch(BigInt("80000ffe", 16), 0, 1)
      awaitRequest(BigInt("80000ffe", 16)); respond(0x13, true)
      d.io.delivery.expect(false.B); d.io.target.expect("h80001000".U); d.clock.step()
      for (_ <- 0 until 3) {
        d.io.outputCount.expect(0.U)
        d.io.reasons.foreach(_.expect(TopDownCounters.UncacheFetchBubble.id.U)); d.clock.step()
      }
      fetch(BigInt("80001000", 16), 1, 1)
      awaitRequest(BigInt("80001000", 16)); respond(0, false)
      d.io.target.expect("h80001002".U); delivered(BigInt("80000ffe", 16))

      // The retried upper half can be cacheable even when the original half used MMIO.
      initialize()
      fetch(BigInt("80000ffe", 16), 0, 1)
      awaitRequest(BigInt("80000ffe", 16)); respond(0x13, true)
      d.clock.step(4)
      d.io.mmio.poke(false.B); d.io.cacheHalf.poke(0.U)
      fetch(BigInt("80001000", 16), 1, 1)
      delivered(BigInt("80000ffe", 16))

      // A non-trainable precheck redirect must retain its predictor attribution.
      initialize()
      d.io.mmio.poke(false.B); d.io.cacheHalf.poke("h0067".U) // jalr without RAS action
      fetch(BigInt("80002000", 16), 0)
      var cycles = 0
      while (!flag(d.io.ifuRedirect) && cycles < 8) { d.clock.step(); cycles += 1 }
      d.io.ifuRedirect.expect(true.B); d.io.ifuUncache.expect(false.B); d.io.ifuCanTrain.expect(false.B)
      d.io.supplyReason.expect(TopDownCounters.BTBMissBubble.id.U); d.io.supplyReasonCount.expect(1.U)

      // Backend direction correction wins over a simultaneous precheck BTB correction.
      initialize()
      d.io.mmio.poke(false.B); d.io.predictedTaken.poke(true.B); d.io.backendAhead.poke(true.B)
      fetch(BigInt("80003000", 16), 0)
      cycles = 0
      while (!flag(d.io.ifuRedirect) && cycles < 8) { d.clock.step(); cycles += 1 }
      d.io.ifuRedirect.expect(true.B); d.io.supplyReason.expect(TopDownCounters.BTBMissBubble.id.U)
      d.io.backendCtrl.poke(true.B); d.io.backendRedirect.poke(true.B)
      d.io.supplyReason.expect(TopDownCounters.TAGEMissBubble.id.U); d.io.supplyReasonCount.expect(1.U)
      d.clock.step(); d.io.backendRedirect.poke(false.B); d.io.backendAhead.poke(false.B)
      d.io.outputCount.expect(0.U)
      d.io.reasons.foreach(_.expect(TopDownCounters.TAGEMissBubble.id.U))

      // Global memory/other redirects clear old uncache wait tags throughout the pipeline.
      for (memVio <- Seq(true, false)) {
        initialize()
        fetch(BigInt("80004000", 16), 0)
        awaitRequest(BigInt("80004000", 16))
        d.io.backendAhead.poke(true.B); d.clock.step()
        d.io.backendRedirect.poke(true.B); d.io.backendMemVio.poke(memVio.B)
        val expected = if (memVio) TopDownCounters.MemVioRedirectBubble else TopDownCounters.OtherRedirectBubble
        d.io.supplyReason.expect(expected.id.U); d.io.supplyReasonCount.expect(1.U)
        d.io.fetchReason.expect(expected.id.U)
        d.clock.step(); d.io.backendRedirect.poke(false.B); d.io.backendAhead.poke(false.B)
        d.io.outputCount.expect(0.U); d.io.reasons.foreach(_.expect(expected.id.U))
        for (_ <- 0 until 3) {
          d.io.delivery.expect(false.B)
          assert(d.io.fetchReason.peek().litValue != TopDownCounters.UncacheFetchBubble.id)
          d.clock.step()
        }
      }
    }
  }
}

package xiangshan.mem

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.ChiselDB
import utility.Constantin
import utility.LogUtilsOptions
import utility.LogUtilsOptionsKey
import utility.PerfCounterOptions
import utility.PerfCounterOptionsKey
import utility.XSPerfLevel
import xiangshan._
import xiangshan.backend.fu.FuType

// Exceptions of an uncached (MMIO) store. The store queue takes them from the ack of the uncache buffer: a bus error
// that the bridge maps to NDERR arrives in `nderr` (the access fault) and a hardware error in `error`; the fields
// `denied` and `corrupt` of UncacheWordResp are never driven by the uncache buffer. A cbo takes its exceptions from
// the response of the DCache (`denied`, `corrupt`), which this change does not touch.
private class UncacheErrHarness(implicit p: Parameters) extends XSModule {
  private val core    = p(XSCoreParamsKey)
  private val backend = core.backendParams
  require(core.StoreQueuePhysicalSize == 16 && core.StoreQueueMultiple == 2)
  require(core.StorePipelineWidth == 2 && core.EnsbufferWidth == 2)
  backend.allExuParams.zipWithIndex.foreach { case (exu, index) =>
    exu.bindBackendParam(backend)
    exu.updateIQWakeUpConfigs(backend.iqWakeUpParams)
    exu.updateExuIdx(index)
  }
  backend.configChecks
  backend.allSchdParams.foreach(_.bindBackendParam(backend))
  backend.allIssueParams.foreach { issue =>
    issue.bindBackendParam(backend)
    issue.exuBlockParams.foreach(_.bindIssueBlockParam(issue))
  }
  backend.updateCopyPdestInfo

  val io = IO(new Bundle {
    val enq      = Input(new SqPtr)
    val retired  = Input(new SqPtr)
    val staValid = Input(Bool())
    val stdValid = Input(Bool())
    val index    = Input(new SqPtr)
    val cbo      = Input(Bool())
    val addr     = Input(UInt(PAddrBits.W))
    val data     = Input(UInt(core.VLEN.W))
    // ack of the uncache buffer
    val uncValid   = Input(Bool())
    val uncNderr   = Input(Bool())
    val uncError   = Input(Bool())
    val uncCorrupt = Input(Bool())
    val uncDenied  = Input(Bool())
    // response of the DCache to a cbo
    val dcValid   = Input(Bool())
    val dcCorrupt = Input(Bool())
    val dcDenied  = Input(Bool())

    val uncReqValid     = Output(Bool())
    val dcReqValid      = Output(Bool())
    val wbValid         = Output(Bool())
    val wbAccessFault   = Output(Bool())
    val wbHardwareError = Output(Bool())
  })
  private val sq = Module(new PhysicalStoreQueue)
  sq.io.hartId                                    := 0.U
  sq.io.fromVirtualStoreQueue.redirectPtr.valid   := false.B
  sq.io.fromVirtualStoreQueue.redirectPtr.bits    := 0.U.asTypeOf(new SqPtr)
  sq.io.fromVirtualStoreQueue.preCommitPtr.valid  := false.B
  sq.io.fromVirtualStoreQueue.preCommitPtr.bits   := 0.U.asTypeOf(new SqPtr)
  sq.io.fromVirtualStoreQueue.retiredPtr          := io.retired
  sq.io.fromVirtualStoreQueue.physicalQueueEnqPtr := io.enq
  sq.io.fromVirtualStoreQueue.headRobIdx          := 0.U.asTypeOf(sq.io.fromVirtualStoreQueue.headRobIdx)
  for (i <- 0 until 2) {
    val use = i == 0
    val op: UInt = Mux(io.cbo, LSUOpType.cbo_flush: UInt, LSUOpType.sd: UInt)
    val addr = WireInit(0.U.asTypeOf(new StoreAddrIO))
    addr.uop.sqIdx    := io.index
    addr.uop.fuOpType := op
    addr.vaddr        := io.addr
    addr.paddr        := io.addr
    addr.size         := 3.U
    // main memory for a cbo, an IO region (not cacheable, mmio) for the uncached store
    addr.memBackTypeMM                       := io.cbo
    addr.mmio                                := !io.cbo
    addr.isLastRequest                       := true.B
    sq.io.fromStoreUnit.storeAddrIn(i).valid := io.staValid && use.B
    sq.io.fromStoreUnit.storeAddrIn(i).bits  := addr
    sq.io.fromStoreUnit.storeAddrInRe(i)     := addr
    val data = WireInit(0.U.asTypeOf(new StoreQueueDataWrite))
    data.fuType                                  := FuType.stu.U
    data.fuOpType                                := op
    data.sqIdx                                   := io.index
    data.data                                    := io.data
    sq.io.storeDataIn(i).valid                   := io.stdValid && use.B
    sq.io.storeDataIn(i).bits                    := data
    sq.io.fromStoreUnit.unalignQueueReq(i).valid := false.B
    sq.io.fromStoreUnit.unalignQueueReq(i).bits  := 0.U.asTypeOf(new UnalignQueueIO)
  }
  sq.io.forward.foreach { f =>
    f.s0Req.valid := false.B
    f.s0Req.bits  := 0.U.asTypeOf(f.s0Req.bits)
    f.s1Req       := 0.U.asTypeOf(f.s1Req)
    f.s1Kill      := false.B
  }
  sq.io.writeToSbuffer.req.foreach(_.ready := true.B)
  sq.io.sbufferCtrl.resp.empty            := true.B
  sq.io.writeBack.ready                   := true.B
  sq.io.toDCache.req.ready                := true.B
  sq.io.toDCache.resp.valid               := io.dcValid
  sq.io.toDCache.resp.bits                := 0.U.asTypeOf(sq.io.toDCache.resp.bits)
  sq.io.toDCache.resp.bits.corrupt        := io.dcCorrupt
  sq.io.toDCache.resp.bits.denied         := io.dcDenied
  sq.io.toUncacheBuffer.req.ready         := true.B
  sq.io.toUncacheBuffer.idResp.valid      := false.B
  sq.io.toUncacheBuffer.idResp.bits       := 0.U.asTypeOf(sq.io.toUncacheBuffer.idResp.bits)
  sq.io.toUncacheBuffer.resp.valid        := io.uncValid
  sq.io.toUncacheBuffer.resp.bits         := 0.U.asTypeOf(sq.io.toUncacheBuffer.resp.bits)
  sq.io.toUncacheBuffer.resp.bits.nderr   := io.uncNderr
  sq.io.toUncacheBuffer.resp.bits.error   := io.uncError
  sq.io.toUncacheBuffer.resp.bits.corrupt := io.uncCorrupt
  sq.io.toUncacheBuffer.resp.bits.denied  := io.uncDenied
  sq.io.wfi.wfiReq                        := false.B
  io.uncReqValid                          := sq.io.toUncacheBuffer.req.valid
  io.dcReqValid                           := sq.io.toDCache.req.valid
  io.wbValid                              := sq.io.writeBack.valid
  io.wbAccessFault                        := sq.io.writeBack.bits.exceptionVec(ExceptionNO.storeAccessFault)
  io.wbHardwareError                      := sq.io.writeBack.bits.exceptionVec(ExceptionNO.hardwareError)
}

class StoreQueueUncacheBusErrorTest extends AnyFlatSpec with ChiselSim {
  private val base = new DefaultConfig
  private val core = base(XSTileKey).head.copy(StoreQueuePhysicalSize = 16, HasCMO = true)
  implicit private val config: Parameters = base.alterPartial {
    case XSCoreParamsKey => core
    case DebugOptionsKey => DebugOptions(
        FPGAPlatform = false,
        EnableDifftest = false,
        AlwaysBasicDiff = false,
        FullBasicDiff = false,
        EnableDebug = false,
        EnablePerfDebug = false,
        EnableXMR = false,
        EnableConstantin = false,
        EnableChiselDB = false,
        AlwaysBasicDB = false,
        EnableRollingDB = false
      )
    case LogUtilsOptionsKey    => LogUtilsOptions(false, false, false, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
  }

  behavior of "PhysicalStoreQueue exceptions of an uncached store"

  // One store at slot 0 (an uncached sd to an IO address, or a cbo.flush), committed at once. The ack carries the
  // given flags for one cycle. Returns (accessFault, hardwareError) seen when the writeback to the ROB is offered.
  private def run(
      cbo:     Boolean,
      nderr:   Boolean = false,
      error:   Boolean = false,
      corrupt: Boolean = false,
      denied:  Boolean = false
  ): (Boolean, Boolean) = {
    Constantin.init(false)
    ChiselDB.init(false)
    var result = (false, false)
    simulate(new UncacheErrHarness) { dut =>
      def bool(s: Bool): Boolean = s.peek().litToBoolean
      def pokePtr(p: SqPtr, n: Int): Unit = {
        p.first_flag.poke(((n / 32)  % 2 == 1).B)
        p.second_flag.poke(((n / 16) % 2).U)
        p.value.poke((n              % 16).U)
      }
      dut.io.staValid.poke(false.B)
      dut.io.stdValid.poke(false.B)
      dut.io.cbo.poke(cbo.B)
      dut.io.uncValid.poke(false.B)
      dut.io.dcValid.poke(false.B)
      Seq(dut.io.uncNderr, dut.io.uncError, dut.io.uncCorrupt, dut.io.uncDenied, dut.io.dcCorrupt, dut.io.dcDenied)
        .foreach(_.poke(false.B))
      pokePtr(dut.io.index, 0)
      dut.io.addr.poke((if (cbo) BigInt(0x80000040L) else BigInt(0x39000008L)).U)
      dut.io.data.poke(BigInt("1122334455667788", 16).U)
      pokePtr(dut.io.enq, 0)
      pokePtr(dut.io.retired, 0)
      dut.reset.poke(true.B)
      dut.clock.step(4)
      dut.reset.poke(false.B)
      dut.clock.step(4)
      // address and data in the same cycle, then commit
      dut.io.staValid.poke(true.B)
      dut.io.stdValid.poke(true.B)
      pokePtr(dut.io.enq, 1)
      pokePtr(dut.io.retired, 1)
      dut.clock.step()
      dut.io.staValid.poke(false.B)
      dut.io.stdValid.poke(false.B)
      // wait for the request to the uncache buffer / DCache, which is accepted in the cycle it is valid
      var w = 0
      while (!(if (cbo) bool(dut.io.dcReqValid) else bool(dut.io.uncReqValid)) && w < 100) { dut.clock.step(); w += 1 }
      assert(w < 100, "the store never sent its request")
      dut.clock.step()
      // the ack, for one cycle
      if (cbo) {
        dut.io.dcValid.poke(true.B)
        dut.io.dcCorrupt.poke(corrupt.B)
        dut.io.dcDenied.poke(denied.B)
      } else {
        dut.io.uncValid.poke(true.B)
        dut.io.uncNderr.poke(nderr.B)
        dut.io.uncError.poke(error.B)
        dut.io.uncCorrupt.poke(corrupt.B)
        dut.io.uncDenied.poke(denied.B)
      }
      dut.clock.step()
      dut.io.uncValid.poke(false.B)
      dut.io.dcValid.poke(false.B)
      var v = 0
      while (!bool(dut.io.wbValid) && v < 100) { dut.clock.step(); v += 1 }
      assert(v < 100, "no writeback to the ROB")
      result = (bool(dut.io.wbAccessFault), bool(dut.io.wbHardwareError))
    }
    println(s"UNCERR cbo=$cbo nderr=$nderr error=$error corrupt=$corrupt denied=$denied -> accessFault=${result._1} " +
      s"hardwareError=${result._2}")
    result
  }

  it should "report no exception for an uncached store whose ack carries no error" in {
    assert(run(cbo = false) == (false, false))
  }

  it should "raise a store access fault for an uncached store whose ack carries nderr" in {
    assert(run(cbo = false, nderr = true) == (true, false))
  }

  it should "raise a hardware error for an uncached store whose ack carries error" in {
    assert(run(cbo = false, error = true) == (false, true))
  }

  it should "ignore the corrupt and denied fields of the ack of an uncached store" in {
    assert(run(cbo = false, corrupt = true, denied = true) == (false, false))
  }

  it should "still raise a store access fault when the DCache denies a cbo (control, other path)" in {
    assert(run(cbo = true, denied = true) == (true, false))
  }

  it should "still raise a hardware error when the DCache response to a cbo is corrupt (control, other path)" in {
    assert(run(cbo = true, corrupt = true) == (false, true))
  }
}

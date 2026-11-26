package xiangshan.mem

import chisel3._
import chisel3.util.MuxLookup
import chiseltest._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{DebugOptionsKey, ExceptionNO, LSUOpType, TriggerAction, XSCoreParamsKey, XSModule, XSTileKey}

private class AtomicsUnitTriggerHarness(implicit p: Parameters) extends XSModule {
  val io = IO(new Bundle {
    val issue = Input(Bool())
    val operation = Input(UInt(2.W)) // AMO, LR, SC
    val sourceAddress = Input(UInt(64.W))
    val issueReady = Output(Bool())
    val triggerEnable = Input(UInt(4.W))
    val triggerWrite = Input(Bool())
    val triggerIndex = Input(UInt(2.W))
    val triggerAddress = Input(UInt(64.W))
    val triggerLoad = Input(Bool())
    val triggerStore = Input(Bool())
    val triggerDebug = Input(Bool())
    val triggerChain = Input(Bool())
    val tlbValid = Input(Bool())
    val tlbMiss = Input(Bool())
    val tlbAddress = Input(UInt(64.W))
    val tlbTriggerAddress = Input(UInt(64.W))
    val tlbPageFault = Input(Bool())
    val tlbRequest = Output(Bool())
    val pmpFault = Input(Bool())
    val sbufferEmpty = Input(Bool())
    val outputReady = Input(Bool())
    val outputValid = Output(Bool())
    val breakpoint = Output(Bool())
    val action = Output(UInt(4.W))
    val storeMisaligned = Output(Bool())
    val storePageFault = Output(Bool())
    val storeAccessFault = Output(Bool())
    val exceptionValid = Output(Bool())
    val exceptionAddress = Output(UInt(64.W))
    val cacheRequest = Output(Bool())
    val cacheResponse = Input(Bool())
  })

  private val dut = Module(new AtomicsUnit)
  dut.io := DontCare
  dut.io.hartId := 0.U
  dut.io.redirect.valid := false.B
  dut.io.in.valid := io.issue
  dut.io.in.bits := 0.U.asTypeOf(dut.io.in.bits)
  dut.io.in.bits.src_rs1 := io.sourceAddress
  dut.io.in.bits.uop.pdest := 1.U
  dut.io.in.bits.uop.fuOpType := MuxLookup(io.operation, LSUOpType.amoadd_w)(Seq(
    1.U -> LSUOpType.lr_w,
    2.U -> LSUOpType.sc_w
  ))
  for ((port, index) <- dut.io.storeDataIn.zipWithIndex) {
    port.valid := io.issue && (index == 0).B
    port.bits := 0.U.asTypeOf(port.bits)
  }
  dut.io.out.ready := io.outputReady
  dut.io.flush_sbuffer.empty := io.sbufferEmpty
  dut.io.dcache.req.ready := true.B
  dut.io.dcache.block_lr := false.B
  dut.io.dcache.resp.valid := io.cacheResponse
  dut.io.dcache.resp.bits := 0.U.asTypeOf(dut.io.dcache.resp.bits)
  dut.io.dtlb.req.ready := true.B
  dut.io.dtlb.resp.valid := io.tlbValid
  dut.io.dtlb.resp.bits := 0.U.asTypeOf(dut.io.dtlb.resp.bits)
  dut.io.dtlb.resp.bits.miss := io.tlbMiss
  dut.io.dtlb.resp.bits.fullva := io.tlbAddress
  dut.io.dtlb.resp.bits.triggerVa := io.tlbTriggerAddress
  for (index <- 0 until 2) {
    dut.io.dtlb.resp.bits.paddr(index) := io.tlbAddress
    dut.io.dtlb.resp.bits.excp(index).pf.st := io.tlbPageFault
  }
  dut.io.pmpResp := 0.U.asTypeOf(dut.io.pmpResp)
  dut.io.pmpResp.st := io.pmpFault
  dut.io.csrCtrl := 0.U.asTypeOf(dut.io.csrCtrl)
  private val triggers = dut.io.csrCtrl.mem_trigger
  triggers.tEnableVec := io.triggerEnable.asBools
  triggers.triggerCanRaiseBpExp := true.B
  triggers.tUpdate.valid := io.triggerWrite
  triggers.tUpdate.bits.addr := io.triggerIndex
  triggers.tUpdate.bits.tdata := 0.U.asTypeOf(triggers.tUpdate.bits.tdata)
  triggers.tUpdate.bits.tdata.tdata2 := io.triggerAddress
  triggers.tUpdate.bits.tdata.load := io.triggerLoad
  triggers.tUpdate.bits.tdata.store := io.triggerStore
  triggers.tUpdate.bits.tdata.chain := io.triggerChain
  triggers.tUpdate.bits.tdata.action := Mux(io.triggerDebug, TriggerAction.DebugMode, TriggerAction.BreakpointExp)

  io.issueReady := dut.io.in.ready
  io.tlbRequest := dut.io.dtlb.req.valid
  io.outputValid := dut.io.out.valid
  io.breakpoint := dut.io.out.bits.uop.exceptionVec(ExceptionNO.breakPoint)
  io.action := dut.io.out.bits.uop.trigger
  io.storeMisaligned := dut.io.out.bits.uop.exceptionVec(ExceptionNO.storeAddrMisaligned)
  io.storePageFault := dut.io.out.bits.uop.exceptionVec(ExceptionNO.storePageFault)
  io.storeAccessFault := dut.io.out.bits.uop.exceptionVec(ExceptionNO.storeAccessFault)
  io.exceptionValid := dut.io.exceptionInfo.valid
  io.exceptionAddress := dut.io.exceptionInfo.bits.vaddr
  io.cacheRequest := dut.io.dcache.req.valid
}

class AtomicsUnitTriggerTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "AtomicsUnit registered triggers"

  private val defaultConfig = new DefaultConfig
  private implicit val config: Parameters = defaultConfig.alterPartial({
    case XSCoreParamsKey => defaultConfig(XSTileKey).head
    case DebugOptionsKey => defaultConfig(DebugOptionsKey).copy(EnableDifftest = false, AlwaysBasicDiff = false)
    case PerfCounterOptionsKey => PerfCounterOptions(
      enablePerfPrint = false, enablePerfDB = false, perfLevel = XSPerfLevel.VERBOSE, perfDBHartID = 0
    )
    case LogUtilsOptionsKey => LogUtilsOptions(
      enableDebug = false, enablePerf = false, fpgaPlatform = false, enableXMR = false
    )
  })

  private val masked = BigInt("800001a0", 16)
  private val tagged = BigInt("abcd0000800001a0", 16)

  private def initialize(dut: AtomicsUnitTriggerHarness): Unit = {
    dut.io.issue.poke(false.B)
    dut.io.operation.poke(0.U)
    dut.io.sourceAddress.poke(tagged.U)
    dut.io.triggerEnable.poke(1.U)
    dut.io.triggerWrite.poke(true.B)
    dut.io.triggerIndex.poke(0.U)
    dut.io.triggerAddress.poke(masked.U)
    dut.io.triggerLoad.poke(true.B)
    dut.io.triggerStore.poke(false.B)
    dut.io.triggerDebug.poke(false.B)
    dut.io.triggerChain.poke(false.B)
    dut.io.tlbValid.poke(false.B)
    dut.io.tlbMiss.poke(false.B)
    dut.io.tlbAddress.poke(masked.U)
    dut.io.tlbTriggerAddress.poke(0.U)
    dut.io.tlbPageFault.poke(false.B)
    dut.io.pmpFault.poke(false.B)
    dut.io.sbufferEmpty.poke(true.B)
    dut.io.outputReady.poke(false.B)
    dut.io.cacheResponse.poke(false.B)
    dut.clock.step()
    dut.io.triggerWrite.poke(false.B)
  }

  private def issue(dut: AtomicsUnitTriggerHarness): Unit = {
    dut.io.issueReady.expect(true.B)
    dut.io.issue.poke(true.B)
    dut.clock.step()
    dut.io.issue.poke(false.B)
    dut.io.tlbRequest.expect(true.B)
    dut.clock.step() // First shared-port response is deliberately ignored.
  }

  private def hit(dut: AtomicsUnitTriggerHarness, address: BigInt = masked): Unit = {
    dut.io.tlbValid.poke(true.B)
    dut.io.tlbMiss.poke(false.B)
    dut.io.tlbTriggerAddress.poke(address.U)
    dut.clock.step()
    dut.io.tlbValid.poke(false.B)
    dut.io.tlbTriggerAddress.poke(0.U) // Response bus may change in the PMP cycle.
    dut.io.cacheRequest.expect(false.B)
    dut.io.outputValid.expect(false.B) // Must consume canFire after the register boundary.
  }

  it should "register a masked first-hit breakpoint before finishing without a cache request" in {
    test(new AtomicsUnitTriggerHarness) { dut =>
      initialize(dut)
      issue(dut)
      hit(dut)
      dut.clock.step()
      dut.io.outputValid.expect(true.B)
      dut.io.breakpoint.expect(true.B)
      dut.io.action.expect(TriggerAction.BreakpointExp)
      dut.io.exceptionValid.expect(true.B)
      dut.io.exceptionAddress.expect(masked.U)
      for (_ <- 0 until 3) {
        dut.io.cacheRequest.expect(false.B)
        dut.io.breakpoint.expect(true.B)
        dut.io.action.expect(TriggerAction.BreakpointExp)
        dut.clock.step()
        dut.io.outputValid.expect(true.B)
      }
    }
  }

  it should "ignore matching prefetch and invalid responses before a nonmatching first hit" in {
    test(new AtomicsUnitTriggerHarness) { dut =>
      initialize(dut)
      dut.io.issue.poke(true.B)
      dut.clock.step()
      dut.io.issue.poke(false.B)
      dut.io.tlbValid.poke(true.B)
      dut.io.tlbTriggerAddress.poke(masked.U)
      dut.clock.step() // Response belongs to the previous shared-port owner.
      dut.io.tlbRequest.expect(true.B)
      dut.io.outputValid.expect(false.B)
      dut.io.tlbValid.poke(false.B)
      dut.clock.step(2) // Invalid bits still match the watched address.
      dut.io.tlbRequest.expect(true.B)
      dut.io.cacheRequest.expect(false.B)
      hit(dut, masked + 4)
      dut.io.tlbTriggerAddress.poke(masked.U)
      dut.clock.step()
      dut.io.outputValid.expect(false.B)
      dut.io.cacheRequest.expect(true.B)
    }
  }

  it should "use the terminal hit after TLB misses and invalid response cycles" in {
    test(new AtomicsUnitTriggerHarness) { dut =>
      initialize(dut)
      issue(dut)
      dut.io.tlbValid.poke(true.B)
      dut.io.tlbMiss.poke(true.B)
      dut.io.tlbTriggerAddress.poke((masked + 4).U)
      dut.clock.step(2)
      dut.io.tlbRequest.expect(true.B)
      dut.io.outputValid.expect(false.B)
      dut.io.tlbValid.poke(false.B)
      dut.clock.step()
      hit(dut)
      dut.clock.step()
      dut.io.outputValid.expect(true.B)
      dut.io.breakpoint.expect(true.B)
      dut.io.cacheRequest.expect(false.B)
    }
  }

  it should "keep the normal cache-request latency when no trigger matches" in {
    test(new AtomicsUnitTriggerHarness) { dut =>
      initialize(dut)
      issue(dut)
      hit(dut, masked + 4)
      dut.clock.step()
      dut.io.outputValid.expect(false.B)
      dut.io.cacheRequest.expect(true.B)
    }
  }

  it should "wait for the store buffer only when the access has no early exception" in {
    test(new AtomicsUnitTriggerHarness) { dut =>
      initialize(dut)
      dut.io.sbufferEmpty.poke(false.B)
      issue(dut)
      hit(dut, masked + 4)
      dut.clock.step(3)
      dut.io.outputValid.expect(false.B)
      dut.io.cacheRequest.expect(false.B)
      dut.io.sbufferEmpty.poke(true.B)
      dut.clock.step()
      dut.io.cacheRequest.expect(true.B)
    }
  }

  it should "report a breakpoint without waiting for the store buffer to drain" in {
    test(new AtomicsUnitTriggerHarness) { dut =>
      initialize(dut)
      dut.io.sbufferEmpty.poke(false.B)
      issue(dut)
      hit(dut)
      dut.clock.step()
      dut.io.outputValid.expect(true.B)
      dut.io.breakpoint.expect(true.B)
      dut.io.cacheRequest.expect(false.B)
    }
  }

  for ((name, operation, load, store, expectedHit) <- Seq(
    ("AMO store", 0, false, true, true),
    ("LR load", 1, true, false, true),
    ("LR store", 1, false, true, false),
    ("SC store", 2, false, true, true),
    ("SC load", 2, true, false, false)
  )) {
    it should s"preserve $name trigger filtering" in {
      test(new AtomicsUnitTriggerHarness) { dut =>
        initialize(dut)
        dut.io.operation.poke(operation.U)
        dut.io.triggerLoad.poke(load.B)
        dut.io.triggerStore.poke(store.B)
        dut.io.triggerWrite.poke(true.B)
        dut.clock.step()
        dut.io.triggerWrite.poke(false.B)
        issue(dut)
        hit(dut)
        dut.clock.step()
        dut.io.outputValid.expect(expectedHit.B)
        dut.io.cacheRequest.expect((!expectedHit).B)
        if (expectedHit) dut.io.breakpoint.expect(true.B)
      }
    }
  }

  it should "preserve debug action priority over a simultaneous breakpoint" in {
    test(new AtomicsUnitTriggerHarness) { dut =>
      initialize(dut)
      dut.io.triggerIndex.poke(1.U)
      dut.io.triggerDebug.poke(true.B)
      dut.io.triggerEnable.poke(3.U)
      dut.io.triggerWrite.poke(true.B)
      dut.clock.step()
      dut.io.triggerWrite.poke(false.B)
      issue(dut)
      hit(dut)
      dut.clock.step()
      dut.io.outputValid.expect(true.B)
      dut.io.action.expect(TriggerAction.DebugMode)
      dut.io.breakpoint.expect(false.B)
      dut.io.cacheRequest.expect(false.B)
    }
  }

  for ((name, firstMatches, enable, expectedHit) <- Seq(
    ("both partners match", true, 3, true),
    ("first partner misses", false, 3, false),
    ("second partner is disabled", true, 1, false)
  )) {
    it should s"preserve chained trigger behavior when $name" in {
      test(new AtomicsUnitTriggerHarness) { dut =>
        initialize(dut)
        dut.io.triggerEnable.poke(enable.U)
        dut.io.triggerChain.poke(true.B)
        dut.io.triggerAddress.poke((if (firstMatches) masked else masked + 4).U)
        dut.io.triggerWrite.poke(true.B)
        dut.clock.step()
        dut.io.triggerIndex.poke(1.U)
        dut.io.triggerChain.poke(false.B)
        dut.io.triggerAddress.poke(masked.U)
        dut.clock.step()
        dut.io.triggerWrite.poke(false.B)
        issue(dut)
        hit(dut)
        dut.clock.step()
        dut.io.outputValid.expect(expectedHit.B)
        dut.io.cacheRequest.expect((!expectedHit).B)
      }
    }
  }

  for (triggerMatches <- Seq(false, true)) {
    it should s"preserve misalignment without adding PMP faults with trigger hit=$triggerMatches" in {
      test(new AtomicsUnitTriggerHarness) { dut =>
        initialize(dut)
        dut.io.sourceAddress.poke((tagged + 2).U)
        dut.io.tlbAddress.poke((masked + 2).U)
        dut.io.triggerAddress.poke((if (triggerMatches) masked + 2 else masked).U)
        dut.io.triggerWrite.poke(true.B)
        dut.clock.step()
        dut.io.triggerWrite.poke(false.B)
        issue(dut)
        hit(dut, masked + 2)
        dut.io.pmpFault.poke(true.B)
        dut.clock.step()
        dut.io.outputValid.expect(true.B)
        dut.io.breakpoint.expect(triggerMatches.B)
        dut.io.storeMisaligned.expect(true.B)
        dut.io.storeAccessFault.expect(false.B)
        dut.io.exceptionAddress.expect((masked + 2).U)
        dut.io.cacheRequest.expect(false.B)
      }
    }
  }

  it should "check the PMP response in its original cycle for a nontriggering access" in {
    test(new AtomicsUnitTriggerHarness) { dut =>
      initialize(dut)
      issue(dut)
      hit(dut, masked + 4)
      dut.io.pmpFault.poke(true.B)
      dut.clock.step()
      dut.io.outputValid.expect(true.B)
      dut.io.breakpoint.expect(false.B)
      dut.io.storeAccessFault.expect(true.B)
      dut.io.cacheRequest.expect(false.B)
    }
  }

  it should "preserve a TLB page fault after the response bus becomes invalid" in {
    test(new AtomicsUnitTriggerHarness) { dut =>
      initialize(dut)
      issue(dut)
      dut.io.tlbPageFault.poke(true.B)
      hit(dut, masked + 4)
      dut.io.tlbPageFault.poke(false.B)
      dut.clock.step()
      dut.io.outputValid.expect(true.B)
      dut.io.storePageFault.expect(true.B)
      dut.io.breakpoint.expect(false.B)
      dut.io.cacheRequest.expect(false.B)
    }
  }

  it should "replace a preceding instruction's trigger result on a nonmatching access" in {
    test(new AtomicsUnitTriggerHarness) { dut =>
      initialize(dut)
      issue(dut)
      hit(dut)
      dut.clock.step()
      dut.io.outputValid.expect(true.B)
      dut.io.outputReady.poke(true.B)
      dut.clock.step()
      dut.io.outputReady.poke(false.B)
      issue(dut)
      hit(dut, masked + 4)
      dut.clock.step()
      dut.io.outputValid.expect(false.B)
      dut.io.cacheRequest.expect(true.B)
    }
  }
}

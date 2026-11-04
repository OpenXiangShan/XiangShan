package xiangshan.mem

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import utility.{LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey}
import xiangshan._
import xiangshan.backend.decode.isa.{Extensions, Instructions}
import xiangshan.backend.fu.FuType
import xiangshan.backend.vector.Decoder.DecodeChannel.ScalarMultiUopDecodeChannel
import xiangshan.backend.vector.Decoder.InstPattern.InstPattern
import xiangshan.cache.mmu.{HasTlbConst, TLBNonBlock, TLBParameters, TlbCmd}

private object ShadowStackTestInputs {
  def zero(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(zero)
    case vector: Vec[_] => vector.foreach(zero)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input =>
      leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
}

// Exercise the real LDU pipeline and final integer writeback mux. TLB/PMP and
// cache responses are supplied by the probe; the production TLB command is
// observed, so PUSH's write/D checks cannot silently regress to POP's read.
private class ShadowStackLoadProbe(implicit val p: Parameters)
    extends Module with HasXSParameter with HasMemBlockParameters {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val push = Input(Bool())
    val oldSsp = Input(UInt(64.W))
    val expectedData = Input(UInt(64.W))
    val loadedData = Input(UInt(64.W))
    val pageFault = Input(Bool())
    val accessFault = Input(Bool())
    val guestFault = Input(Bool())
    val pmpDenyWrite = Input(Bool())
    val mmio = Input(Bool())
    val tlbMiss = Input(Bool())
    val cacheMiss = Input(Bool())
    val flush = Input(Bool())
    val loadTrigger = Input(Bool())
    val storeTrigger = Input(Bool())
    val ready = Output(Bool())
    val tlbCmd = Output(UInt(3.W))
    val tlbSize = Output(UInt(3.W))
    val tlbAddress = Output(UInt(64.W))
    val shadowStack = Output(Bool())
    val result = Output(ValidIO(UInt(64.W)))
    val robValid = Output(Bool())
    val faultPage = Output(Bool())
    val faultAccess = Output(Bool())
    val faultGuest = Output(Bool())
    val faultCompare = Output(Bool())
    val faultBreakpoint = Output(Bool())
  })

  val param = ldaParams.head
  param.bindBackendParam(backendParams)
  param.bindIssueBlockParam(intSchdParams.issueBlockParams.find(_.allExuParams.contains(param)).get)
  val ldu = Module(new NewLoadUnit(param))
  ShadowStackTestInputs.zero(ldu.io)

  val opcode = if (HasShadowStack) {
    Mux(io.push, LSUOpType.sspushchk.encode.value.U, LSUOpType.sspopchk.encode.value.U)
  } else LSUOpType.ld.encode.value.U
  val address = Mux(io.push, io.oldSsp - 8.U, io.oldSsp)
  ldu.io.ldin.valid := io.valid
  ldu.io.ldin.bits.fuType := FuType.ldu.ohid.U
  ldu.io.ldin.bits.fuOpType := opcode
  ldu.io.ldin.bits.src(0) := io.oldSsp
  if (HasShadowStack) ldu.io.ldin.bits.src(1) := io.expectedData
  ldu.io.ldin.bits.imm := Mux(io.push, "hff8".U, 0.U)
  ldu.io.ldin.bits.isFirstIssue := true.B
  ldu.io.ldin.bits.rfWen.foreach(_ := true.B)
  ldu.io.ldin.bits.pdest := 1.U
  ldu.io.ldin.bits.robIdx.value := 1.U
  ldu.io.ldin.bits.robIdx.isFormer := true.B
  ldu.io.tlb.req.ready := true.B
  ldu.io.tlb.resp.valid := RegNext(ldu.io.tlb.req.fire, false.B)
  ldu.io.tlb.resp.bits.miss := io.tlbMiss
  ldu.io.tlb.resp.bits.fullva := RegNext(ldu.io.tlb.req.bits.fullva)
  ldu.io.tlb.resp.bits.paddr.foreach(_ := address)
  ldu.io.tlb.resp.bits.excp.foreach { exception =>
    exception.pf.st := io.pageFault
    exception.af.st := io.accessFault
    exception.gpf.st := io.guestFault
  }
  ldu.io.pmp.st := io.pmpDenyWrite
  ldu.io.pmp.mmio := io.mmio
  ldu.io.dcache.req.ready := true.B
  ldu.io.dcache.resp.valid := true.B
  ldu.io.dcache.resp.bits.data := Fill(VLEN / 64, io.loadedData)
  ldu.io.dcache.resp.bits.data_delayed := Fill(VLEN / 64, io.loadedData)
  ldu.io.dcache.resp.bits.miss := io.cacheMiss
  ldu.io.lqWrite.ready := true.B
  ldu.io.rarNukeQuery.req.ready := true.B
  ldu.io.rawNukeQuery.req.ready := true.B
  ldu.io.redirect.valid := io.flush
  ldu.io.redirect.bits.robIdx := ldu.io.ldin.bits.robIdx
  ldu.io.redirect.bits.level := RedirectLevel.flush
  ldu.io.csrTrigger.triggerCanRaiseBpExp := true.B
  ldu.io.csrTrigger.tEnableVec(0) := io.loadTrigger || io.storeTrigger
  ldu.io.csrTrigger.tdataVec(0).tdata2 := address
  ldu.io.csrTrigger.tdataVec(0).action := TriggerAction.BreakpointExp
  ldu.io.csrTrigger.tdataVec(0).load := io.loadTrigger
  ldu.io.csrTrigger.tdataVec(0).store := io.storeTrigger

  io.ready := ldu.io.ldin.ready
  io.tlbCmd := ldu.io.tlb.req.bits.cmd
  io.tlbSize := ldu.io.tlb.req.bits.size
  io.tlbAddress := ldu.io.tlb.req.bits.fullva
  io.shadowStack := ldu.io.tlb.req.bits.shadowStackUser.getOrElse(false.B)
  io.result.valid := ldu.io.ldout.toIntRf.get.valid
  io.result.bits := ldu.io.ldout.toIntRf.get.bits.data
  io.robValid := ldu.io.ldout.toRob.valid
  val exceptions = ldu.io.ldout.toRob.bits.exceptionVec
  io.faultPage := exceptions(ExceptionNO.storePageFault)
  io.faultAccess := exceptions(ExceptionNO.storeAccessFault)
  io.faultGuest := exceptions(ExceptionNO.storeGuestPageFault)
  io.faultBreakpoint := exceptions(ExceptionNO.breakPoint)
  io.faultCompare := (if (HasShadowStack) exceptions(ExceptionNO.zicfissSoftwareCheck) else false.B)
}

// Use actual TLB storage/refill and permission logic for the page-type and
// A/D-bit regressions. The LDU test above verifies which command reaches it.
private class ShadowStackTlbProbe(implicit val p: Parameters)
    extends Module with HasXSParameter with HasTlbConst {
  val io = IO(new Bundle {
    val refill = Input(Bool())
    val request = Input(Bool())
    val push = Input(Bool())
    val xwr = Input(UInt(3.W))
    val accessed = Input(Bool())
    val dirty = Input(Bool())
    val bare = Input(Bool())
    val valid = Output(Bool())
    val miss = Output(Bool())
    val pageFault = Output(Bool())
    val accessFault = Output(Bool())
    val pmpCmd = Output(UInt(3.W))
  })
  val tlb = Module(new TLBNonBlock(1, 1, TLBParameters(name = "ldtlb", fenceDelay = 0)))
  ShadowStackTestInputs.zero(tlb.io)
  val address = BigInt("80013018", 16)
  val vpn = address >> 12
  tlb.io.csr.satp.mode := Mux(io.bare, 0.U, 8.U)
  tlb.io.csr.priv.dmode := 1.U // S-mode
  tlb.io.csr.SSEVec.menvcfgSSE := true.B
  val port = tlb.io.requestor.head
  port.req.valid := io.request
  port.req.bits.vaddr := address.U
  port.req.bits.fullva := address.U
  port.req.bits.checkfullva := true.B
  port.req.bits.size := 3.U
  port.req.bits.cmd := Mux(io.push, TlbCmd.write, TlbCmd.read)
  port.req.bits.shadowStackUser.get := true.B
  port.resp.ready := true.B
  tlb.io.ptw.req.foreach(_.ready := true.B)
  tlb.io.ptw.resp.valid := io.refill
  val leaf = tlb.io.ptw.resp.bits.s1
  leaf.entry.v := true.B
  leaf.entry.tag := (vpn >> sectortlbwidth).U
  leaf.entry.ppn := (vpn >> sectortlbwidth).U
  leaf.entry.perm.get.x := io.xwr(2)
  leaf.entry.perm.get.w := io.xwr(1)
  leaf.entry.perm.get.r := io.xwr(0)
  leaf.entry.perm.get.a := io.accessed
  leaf.entry.perm.get.d := io.dirty
  leaf.addr_low := (vpn & ((1 << sectortlbwidth) - 1)).U
  for (i <- 0 until tlbcontiguous) {
    leaf.ppn_low(i) := i.U
    leaf.valididx(i) := true.B
    leaf.pteidx(i) := (i == (vpn & ((1 << sectortlbwidth) - 1)).toInt).B
  }
  io.valid := port.resp.valid
  io.miss := port.resp.bits.miss
  io.pageFault := port.resp.bits.excp.head.pf.st
  io.accessFault := port.resp.bits.excp.head.af.st
  io.pmpCmd := tlb.io.pmp.head.bits.cmd
}

private class ShadowStackDecodeProbe(implicit p: Parameters) extends Module {
  val extensions = Seq(Extensions.Zicfiss)
  val decoder = Module(new ScalarMultiUopDecodeChannel(InstPattern.extensionInsts(extensions: _*), extensions))
  ShadowStackTestInputs.zero(decoder.in)
  val io = IO(new Bundle {
    val rawInst = Input(UInt(32.W))
    val decoded = Output(chiselTypeOf(decoder.out))
  })
  decoder.in.rawInst := io.rawInst
  io.decoded := decoder.out
}

class ShadowStackLoadTest extends AnyFlatSpec with ChiselSim {
  private def config(enabled: Boolean = true): Parameters = {
    val base = new top.MinimalConfig
    base.alterPartial {
      case XSCoreParamsKey => base(XSTileKey).head.copy(HasShadowStack = enabled)
      case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = true)
      case PerfCounterOptionsKey => PerfCounterOptions(enablePerfPrint = false, enablePerfDB = false,
        perfLevel = utility.XSPerfLevel.VERBOSE, perfDBHartID = 0)
      case DebugOptionsKey => base(DebugOptionsKey).copy(
        FPGAPlatform = true, EnablePerfDebug = false, AlwaysBasicDiff = false, AlwaysBasicDB = false
      )
    }
  }

  behavior of "Shadow-stack PUSH/POP micro-ops"

  it should "decode PUSH into four dependent uops with five writebacks" in {
    simulate(new ShadowStackDecodeProbe()(config())) { dut =>
      dut.io.rawInst.poke(Instructions.SSPUSH_X1.value.U)
      dut.io.decoded.uopNum.expect(4.U)
      for (register <- Seq(1, 5)) {
        dut.io.rawInst.poke((if (register == 1) Instructions.SSPUSH_X1 else Instructions.SSPUSH_X5).value.U)
        dut.io.decoded.uop(1).bits.lsrc2.expect(register.U)
        dut.io.decoded.uop(2).bits.opcode.expect(LSUOpType.sspushchk.encode.value.U)
        dut.io.decoded.uop(2).bits.lsrc1.expect(32.U)
        dut.io.decoded.uop(2).bits.ldest.expect(32.U)
        dut.io.decoded.uop(2).bits.renameInfo.gpWen.expect(true.B)
        dut.io.decoded.uop(2).bits.imm.expect("hff8".U)
        dut.io.decoded.uop(3).bits.lsrc1.expect(32.U)
        dut.io.decoded.uop(3).bits.isLastUop.expect(true.B)
        dut.io.decoded.uop(3).bits.renameInfo.blockBack.expect(true.B)
        dut.io.decoded.uop(3).bits.renameInfo.noSpec.expect(false.B)
        dut.io.decoded.uop(0).bits.renameInfo.noSpec.expect(true.B)
        dut.io.decoded.uop(0).bits.numWb.expect(4.U)
      }
      dut.io.rawInst.poke(Instructions.SSPOPCHK_X1.value.U)
      dut.io.decoded.uopNum.expect(3.U)
      dut.io.decoded.uop(1).bits.opcode.expect(LSUOpType.sspopchk.encode.value.U)
      dut.io.decoded.uop(0).bits.numWb.expect(2.U)
    }
  }

  it should "write the adjusted SSP only on success and preserve it on terminal faults" in {
    simulate(new ShadowStackLoadProbe()(config())) { dut =>
      val oldSsp = BigInt("80013020", 16)
      val expectedData = BigInt("123456789abcdef0", 16)
      def flag(value: Bool): Boolean = value.peek().litToBoolean
      def initialize(): Unit = {
        dut.io.valid.poke(false.B)
        dut.io.push.poke(true.B)
        dut.io.oldSsp.poke(oldSsp.U)
        dut.io.expectedData.poke(expectedData.U)
        dut.io.loadedData.poke(expectedData.U)
        Seq(dut.io.pageFault, dut.io.accessFault, dut.io.guestFault,
          dut.io.pmpDenyWrite, dut.io.mmio, dut.io.tlbMiss, dut.io.cacheMiss,
          dut.io.flush, dut.io.loadTrigger, dut.io.storeTrigger).foreach(_.poke(false.B))
        dut.reset.poke(true.B)
        dut.clock.step(4)
        dut.reset.poke(false.B)
        dut.clock.step(2)
      }
      def execute(push: Boolean = true, flushCycle: Int = -1): (Seq[BigInt], Set[String]) = {
        dut.io.push.poke(push.B)
        dut.io.valid.poke(true.B)
        dut.io.ready.expect(true.B)
        dut.io.tlbCmd.expect((if (push) TlbCmd.write else TlbCmd.read))
        dut.io.tlbSize.expect(3.U)
        dut.io.tlbAddress.expect((if (push) oldSsp - 8 else oldSsp).U)
        dut.io.shadowStack.expect(true.B)
        dut.clock.step()
        dut.io.valid.poke(false.B)
        val results = scala.collection.mutable.ArrayBuffer.empty[BigInt]
        val faults = scala.collection.mutable.Set.empty[String]
        for (cycle <- 0 until 10) {
          dut.io.flush.poke((cycle == flushCycle).B)
          if (flag(dut.io.result.valid)) results += dut.io.result.bits.peek().litValue
          if (flag(dut.io.robValid)) {
            Seq("page" -> dut.io.faultPage, "access" -> dut.io.faultAccess,
              "guest" -> dut.io.faultGuest, "compare" -> dut.io.faultCompare,
              "breakpoint" -> dut.io.faultBreakpoint).foreach { case (name, signal) =>
              if (flag(signal)) faults += name
            }
          }
          dut.clock.step()
        }
        (results.toSeq, faults.toSet)
      }

      initialize()
      dut.io.loadedData.poke(0.U) // PUSH must not run POP's value comparison.
      assert(execute() == (Seq(oldSsp - 8), Set.empty[String]))
      for ((name, signal) <- Seq("page" -> dut.io.pageFault,
        "access" -> dut.io.accessFault, "guest" -> dut.io.guestFault,
        "access" -> dut.io.pmpDenyWrite, "access" -> dut.io.mmio)) {
        initialize()
        signal.poke(true.B)
        assert(execute() == (Seq(oldSsp), Set(name)), s"PUSH $name fault changed SSP")
      }
      initialize()
      dut.io.storeTrigger.poke(true.B)
      assert(execute() == (Seq(oldSsp), Set("breakpoint")))
      initialize()
      dut.io.loadTrigger.poke(true.B)
      assert(execute() == (Seq(oldSsp - 8), Set.empty[String]))
      initialize()
      dut.io.tlbMiss.poke(true.B)
      assert(execute()._1.isEmpty, "TLB miss must not release the CSR writer")
      initialize()
      dut.io.cacheMiss.poke(true.B)
      assert(execute()._1.isEmpty, "Cache miss must not release the CSR writer")
      for (flushCycle <- 0 to 2) {
        initialize()
        assert(execute(flushCycle = flushCycle)._1.isEmpty,
          s"A PUSH flushed in pipeline cycle $flushCycle must not write a pointer")
      }

      initialize()
      assert(execute(push = false) == (Seq(oldSsp + 8), Set.empty[String]))
      initialize()
      dut.io.loadedData.poke(0.U)
      assert(execute(push = false) == (Seq(oldSsp), Set("compare")))
      initialize()
      dut.io.storeTrigger.poke(true.B)
      assert(execute(push = false) == (Seq(oldSsp + 8), Set.empty[String]))
      initialize()
      dut.io.loadTrigger.poke(true.B)
      assert(execute(push = false) == (Seq(oldSsp), Set("breakpoint")))
    }
  }

  it should "use real TLB store checks for PUSH without imposing D on POP" in {
    simulate(new ShadowStackTlbProbe()(config())) { dut =>
      // XWR, A, D, Bare, expected PUSH page/access fault, expected POP page/access fault.
      val cases = Seq(
        (2, true, true, false, false, false, false, false),
        (2, true, false, false, true, false, false, false),
        (2, false, true, false, true, false, true, false),
        (1, true, true, false, true, false, true, false),
        (3, true, true, false, false, true, false, true),
        (7, true, true, false, false, true, false, true),
        (2, true, true, true, false, true, false, true)
      )
      for ((xwr, accessed, dirty, bare, pushPf, pushAf, popPf, popAf) <- cases;
           push <- Seq(true, false)) {
        dut.io.refill.poke(false.B)
        dut.io.request.poke(false.B)
        dut.io.push.poke(push.B)
        dut.io.xwr.poke(xwr.U)
        dut.io.accessed.poke(accessed.B)
        dut.io.dirty.poke(dirty.B)
        dut.io.bare.poke(bare.B)
        dut.reset.poke(true.B)
        dut.clock.step(4)
        dut.reset.poke(false.B)
        dut.clock.step(3)
        dut.io.refill.poke(true.B)
        dut.clock.step()
        dut.io.refill.poke(false.B)
        dut.clock.step(3)
        dut.io.request.poke(true.B)
        dut.clock.step()
        dut.io.request.poke(false.B)
        dut.io.valid.expect(true.B)
        dut.io.miss.expect(false.B)
        dut.io.pageFault.expect((if (push) pushPf else popPf).B)
        dut.io.accessFault.expect((if (push) pushAf else popAf).B)
        dut.io.pmpCmd.expect(TlbCmd.write) // Both require PMP R+W.
        dut.clock.step(2)
      }
    }
  }

  it should "elaborate the ordinary LDU when shadow stacks are disabled" in {
    _root_.circt.stage.ChiselStage.emitCHIRRTL(new ShadowStackLoadProbe()(config(false)))
  }
}

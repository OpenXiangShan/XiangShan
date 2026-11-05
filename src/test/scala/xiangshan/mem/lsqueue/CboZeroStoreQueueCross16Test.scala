package xiangshan.mem

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.{ChiselDB, Constantin, LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions,
  PerfCounterOptionsKey, XSPerfLevel}
import xiangshan._
import xiangshan.backend.fu.FuType

// cbo.zero next to a cross-16B store. Port 1 of the StoreQueue-to-sbuffer interface carries the high half of a cross-16B
// head store through a bypass. The wline flag of that half must not come from slot rdataPtr+1 (the next store, or a
// stale slot whose last occupant was a cbo.zero), and the CBO FSM must not leave writeZero because of that half.
private class X16ProbeSq(implicit p: Parameters) extends PhysicalStoreQueue {
  val probe = IO(Output(new Bundle {
    val deq = new SqPtr
    val rdata = new SqPtr
  }))
  probe.deq := deqPtrExt.head
  probe.rdata := rdataPtrExt.head
}

private class X16Harness(implicit p: Parameters) extends XSModule {
  private val core = p(XSCoreParamsKey)
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
    val enq = Input(new SqPtr)
    val retired = Input(new SqPtr)
    // sta stage-1 events and std data per StoreUnit port
    val staValid = Input(Vec(2, Bool()))
    val stdValid = Input(Vec(2, Bool()))
    val index = Input(Vec(2, new SqPtr))
    val zero = Input(Vec(2, Bool()))
    val cross = Input(Vec(2, Bool()))
    val last = Input(Vec(2, Bool()))
    val addr = Input(Vec(2, UInt(PAddrBits.W)))
    val data = Input(Vec(2, UInt(core.VLEN.W)))
    val ready = Input(Vec(2, Bool()))
    val empty = Input(Bool())
    val wbReady = Input(Bool())

    val valid = Output(Vec(2, Bool()))
    val outAddr = Output(Vec(2, UInt(PAddrBits.W)))
    val outData = Output(Vec(2, UInt(core.VLEN.W)))
    val outMask = Output(Vec(2, UInt((core.VLEN / 8).W)))
    val wline = Output(Vec(2, Bool()))
    val wbValid = Output(Bool())
    val deq = Output(new SqPtr)
    val rdata = Output(new SqPtr)
  })
  private val sq = Module(new X16ProbeSq)
  sq.io.hartId := 0.U
  sq.io.fromVirtualStoreQueue.redirectPtr.valid := false.B
  sq.io.fromVirtualStoreQueue.redirectPtr.bits := 0.U.asTypeOf(new SqPtr)
  sq.io.fromVirtualStoreQueue.preCommitPtr.valid := false.B
  sq.io.fromVirtualStoreQueue.preCommitPtr.bits := 0.U.asTypeOf(new SqPtr)
  sq.io.fromVirtualStoreQueue.retiredPtr := io.retired
  sq.io.fromVirtualStoreQueue.physicalQueueEnqPtr := io.enq
  sq.io.fromVirtualStoreQueue.headRobIdx := 0.U.asTypeOf(sq.io.fromVirtualStoreQueue.headRobIdx)
  for (i <- 0 until 2) {
    val op: UInt = Mux(io.zero(i), LSUOpType.cbo_zero: UInt, LSUOpType.sd: UInt)
    val addr = WireInit(0.U.asTypeOf(new StoreAddrIO))
    addr.uop.sqIdx := io.index(i)
    addr.uop.fuOpType := op
    addr.vaddr := io.addr(i)
    addr.paddr := io.addr(i)
    addr.size := 3.U
    addr.memBackTypeMM := true.B
    addr.isUnalign := io.cross(i)
    addr.cross16Byte := io.cross(i)
    addr.isLastRequest := io.last(i)
    sq.io.fromStoreUnit.storeAddrIn(i).valid := io.staValid(i)
    sq.io.fromStoreUnit.storeAddrIn(i).bits := addr
    sq.io.fromStoreUnit.storeAddrInRe(i) := addr
    val data = WireInit(0.U.asTypeOf(new StoreQueueDataWrite))
    data.fuType := FuType.stu.U
    data.fuOpType := op
    data.sqIdx := io.index(i)
    data.data := io.data(i)
    sq.io.storeDataIn(i).valid := io.stdValid(i)
    sq.io.storeDataIn(i).bits := data
    sq.io.fromStoreUnit.unalignQueueReq(i).valid := false.B
    sq.io.fromStoreUnit.unalignQueueReq(i).bits := 0.U.asTypeOf(new UnalignQueueIO)
  }
  sq.io.forward.foreach { f =>
    f.s0Req.valid := false.B
    f.s0Req.bits := 0.U.asTypeOf(f.s0Req.bits)
    f.s1Req := 0.U.asTypeOf(f.s1Req)
    f.s1Kill := false.B
  }
  for (i <- 0 until 2) {
    val req = sq.io.writeToSbuffer.req(i)
    req.ready := io.ready(i)
    io.valid(i) := req.valid
    io.outAddr(i) := req.bits.addr
    io.outData(i) := req.bits.data
    io.outMask(i) := req.bits.mask
    io.wline(i) := req.bits.wline
  }
  sq.io.sbufferCtrl.resp.empty := io.empty
  sq.io.writeBack.ready := io.wbReady
  sq.io.toDCache.req.ready := true.B
  sq.io.toDCache.resp.valid := false.B
  sq.io.toDCache.resp.bits := 0.U.asTypeOf(sq.io.toDCache.resp.bits)
  sq.io.toUncacheBuffer.req.ready := true.B
  sq.io.toUncacheBuffer.idResp.valid := false.B
  sq.io.toUncacheBuffer.idResp.bits := 0.U.asTypeOf(sq.io.toUncacheBuffer.idResp.bits)
  sq.io.toUncacheBuffer.resp.valid := false.B
  sq.io.toUncacheBuffer.resp.bits := 0.U.asTypeOf(sq.io.toUncacheBuffer.resp.bits)
  sq.io.wfi.wfiReq := false.B
  io.wbValid := sq.io.writeBack.valid
  io.deq := sq.probe.deq
  io.rdata := sq.probe.rdata
}

class CboZeroStoreQueueCross16Test extends AnyFlatSpec with ChiselSim {
  private val base = new DefaultConfig
  private val core = base(XSTileKey).head.copy(StoreQueuePhysicalSize = 16, HasCMO = true)
  private implicit val config: Parameters = base.alterPartial {
    case XSCoreParamsKey => core
    case DebugOptionsKey => DebugOptions(
      FPGAPlatform = false, EnableDifftest = false, AlwaysBasicDiff = false, FullBasicDiff = false,
      EnableDebug = false, EnablePerfDebug = false, EnableXMR = false, EnableConstantin = false,
      EnableChiselDB = false, AlwaysBasicDB = false, EnableRollingDB = false)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
  }

  behavior of "PhysicalStoreQueue cross-16B store next to cbo.zero"

  // sd crossing 16B and also a 64B line: byte 15 of block 0x4030, bytes 0..6 of block 0x4040. The two halves lie in
  // different lines, so every sbuffer ready pattern used below is one the real Sbuffer can produce (for two halves in
  // the same line it forces ready(1) = ready(0)).
  private val storeA = BigInt(0x403f)
  private val zeroAddr = BigInt(0x4080)
  private val dataA = BigInt("1122334455667788", 16)

  private case class Fire(cycle: Int, port: Int, addr: BigInt, mask: BigInt, wline: Boolean, data: BigInt)

  // Runs one scripted scenario and returns the list of failed checks.
  private def run(name: String, ready: Int => Seq[Boolean], withZero: Boolean, warmup: Int): Seq[String] = {
    import chisel3.simulator.HasSimulator.simulators.verilator
    Constantin.init(false)
    ChiselDB.init(false)
    val failures = scala.collection.mutable.ArrayBuffer.empty[String]
    simulate(new X16Harness) { dut =>
      def bool(s: Bool): Boolean = s.peek().litToBoolean
      def value(s: UInt): BigInt = s.peek().litValue
      def pokePtr(p: SqPtr, n: Int): Unit = {
        p.first_flag.poke(((n / 32) % 2 == 1).B)
        p.second_flag.poke(((n / 16) % 2).U)
        p.value.poke((n % 16).U)
      }
      def raw(p: SqPtr): Int = (if (bool(p.first_flag)) 32 else 0) + value(p.second_flag).toInt * 16 + value(p.value).toInt
      dut.io.staValid.foreach(_.poke(false.B))
      dut.io.stdValid.foreach(_.poke(false.B))
      dut.io.zero.foreach(_.poke(false.B))
      dut.io.cross.foreach(_.poke(false.B))
      dut.io.last.foreach(_.poke(true.B))
      dut.io.ready.foreach(_.poke(true.B))
      dut.io.empty.poke(true.B)
      dut.io.wbReady.poke(true.B)
      for (i <- 0 until 2) { pokePtr(dut.io.index(i), 0); dut.io.addr(i).poke(0.U); dut.io.data(i).poke(0.U) }
      pokePtr(dut.io.enq, 0)
      pokePtr(dut.io.retired, 0)
      dut.reset.poke(true.B)
      dut.clock.step(4)
      dut.reset.poke(false.B)
      dut.clock.step(4)
      var cycle = 0
      val fires = scala.collection.mutable.ArrayBuffer.empty[Fire]
      var wbFirst = -1
      def step(rdy: Seq[Boolean]): Unit = {
        for (i <- 0 until 2) dut.io.ready(i).poke(rdy(i).B)
        for (i <- 0 until 2 if bool(dut.io.valid(i)) && rdy(i)) {
          fires += Fire(cycle, i, value(dut.io.outAddr(i)), value(dut.io.outMask(i)), bool(dut.io.wline(i)),
            value(dut.io.outData(i)))
        }
        if (bool(dut.io.wbValid) && wbFirst < 0) wbFirst = cycle
        dut.clock.step()
        cycle += 1
      }
      // `warmup` plain 8-byte stores to distinct lines, one of them a cbo.zero at slot 1 when warmup > 1, so that
      // the physical slot after the cross-16B store is stale with isCbo=1 / cboType=zero (lap 2).
      var n = 0
      def retireAll(): Unit = { pokePtr(dut.io.enq, n); pokePtr(dut.io.retired, n) }
      def issue(idx: Int, zero: Boolean, addr: BigInt, cross: Boolean, last: Boolean, std: Boolean, port: Int): Unit = {
        dut.io.staValid(port).poke(true.B)
        dut.io.stdValid(port).poke(std.B)
        pokePtr(dut.io.index(port), idx)
        dut.io.zero(port).poke(zero.B)
        dut.io.cross(port).poke(cross.B)
        dut.io.last(port).poke(last.B)
        dut.io.addr(port).poke(addr.U)
        dut.io.data(port).poke(dataA.U)
      }
      def clearIssue(): Unit = {
        dut.io.staValid.foreach(_.poke(false.B))
        dut.io.stdValid.foreach(_.poke(false.B))
      }
      for (k <- 0 until warmup) {
        val isZero = warmup > 1 && k == 1
        issue(k, isZero, BigInt(0x8000 + 0x80 * k), cross = false, last = true, std = true, port = 0)
        n = k + 1
        retireAll()
        step(Seq(true, true))
        clearIssue()
        var w = 0
        while (raw(dut.io.deq) != n && w < 200) { step(Seq(true, true)); w += 1 }
        if (raw(dut.io.deq) != n) failures += s"$name HARNESS warmup op $k did not drain deq=${raw(dut.io.deq)}"
      }
      fires.clear()
      wbFirst = -1
      val mark = cycle
      // The cross-16B store A: first request (isLastRequest=false), then the last request. A cbo.zero C behind it
      // (same cycle as the first request, other StoreUnit port) when withZero.
      val a = n
      issue(a, zero = false, storeA, cross = true, last = false, std = true, port = 0)
      if (withZero) issue(a + 1, zero = true, zeroAddr, cross = false, last = true, std = true, port = 1)
      n = a + (if (withZero) 2 else 1)
      retireAll()
      step(Seq(false, false))
      issue(a, zero = false, storeA, cross = true, last = true, std = false, port = 0)
      dut.io.staValid(1).poke(false.B); dut.io.stdValid(1).poke(false.B)
      step(Seq(false, false))
      clearIssue()
      var c = 0
      var done = false
      while (c < 120 && !done) {
        val rdy = ready(c)
        step(rdy)
        c += 1
        done = raw(dut.io.deq) == n && raw(dut.io.rdata) == n && !bool(dut.io.valid(0)) && !bool(dut.io.wbValid) && c > 20
      }
      if (!done) failures += s"$name timeout deq=${raw(dut.io.deq)} rdata=${raw(dut.io.rdata)} n=$n"
      fires.foreach(f => println(f"X16 $name cycle=${f.cycle - mark}%d port=${f.port} addr=${f.addr.toString(16)} " +
        f"mask=${f.mask.toString(16)} wline=${f.wline} data=${f.data.toString(16)}"))
      println(s"X16 $name wbFirst=${if (wbFirst < 0) -1 else wbFirst - mark} fires=${fires.size}")
      val lows = fires.filter(f => f.addr == (storeA & ~BigInt(15)))
      val highs = fires.filter(f => f.addr == (storeA & ~BigInt(15)) + 16)
      val zeros = fires.filter(f => f.wline)
      if (lows.size != 1) failures += s"$name expected one low half of the cross-16B store, got ${lows.size}"
      if (highs.size != 1) failures += s"$name expected one high half of the cross-16B store, got ${highs.size}"
      if (lows.headOption.exists(f => f.wline || f.mask != BigInt(0x8000)))
        failures += s"$name low half wrong: ${lows.head}"
      // payloads: byte 15 carries the low byte of the store data, bytes 0..6 of the next block carry the other seven
      if (lows.headOption.exists(f => ((f.data >> 120) & 0xff) != (dataA & 0xff)))
        failures += s"$name low half payload byte ${(lows.head.data >> 120 & 0xff).toString(16)} != ${(dataA & 0xff).toString(16)}"
      if (highs.headOption.exists(f => (f.data & ((BigInt(1) << 56) - 1)) != (dataA >> 8)))
        failures += s"$name high half payload ${(highs.head.data & ((BigInt(1) << 56) - 1)).toString(16)} != ${(dataA >> 8).toString(16)}"
      if (highs.headOption.exists(f => f.wline || f.mask != BigInt(0x7f)))
        failures += s"$name high half of the cross-16B store written as ${if (highs.head.wline) "a FULL LINE (wline)" else "wrong mask"}: ${highs.head}"
      if (withZero) {
        if (zeros.size != 1 || zeros.head.addr != zeroAddr || zeros.head.data != 0)
          failures += s"$name expected exactly one zero line at ${zeroAddr.toString(16)}, wline requests: $zeros"
        if (wbFirst < 0) failures += s"$name no cbo.zero writeback"
        else if (zeros.nonEmpty && wbFirst <= zeros.head.cycle)
          failures += s"$name cbo.zero writeback (cycle ${wbFirst - mark}) not after its zero line"
        if (zeros.isEmpty && wbFirst >= 0)
          failures += s"$name cbo.zero retired without ever writing its zero line"
      } else if (zeros.nonEmpty) {
        failures += s"$name wline request without any cbo.zero: $zeros"
      }
    }
    failures.toSeq
  }

  // Sbuffer back-pressures until both halves of A and C are in the SQ/data queue, then accepts port 0 alone for one
  // cycle (A.low), so A.high becomes the head of the data queue while the FSM is in writeZero, then everything.
  private val lowThenHigh: Int => Seq[Boolean] = c => if (c < 12) Seq(false, false) else if (c < 14) Seq(true, false) else Seq(true, true)
  // The sbuffer takes both halves together.
  private val bothTogether: Int => Seq[Boolean] = c => if (c < 12) Seq(false, false) else Seq(true, true)
  // Port 0 only, forever: the high half can leave only as the head.
  private val port0Only: Int => Seq[Boolean] = c => if (c < 12) Seq(false, false) else Seq(true, c >= 20)

  it should "keep wline off the high half and not lose the zero line (A.low leaves first, high half becomes head)" in {
    val f = run("lowThenHigh", lowThenHigh, withZero = true, warmup = 0)
    assert(f.isEmpty, f.mkString("\n"))
  }

  it should "keep wline off the high half when both halves leave together" in {
    val f = run("bothTogether", bothTogether, withZero = true, warmup = 0)
    assert(f.isEmpty, f.mkString("\n"))
  }

  it should "keep wline off the high half when only port 0 is accepted for a while" in {
    val f = run("port0Only", port0Only, withZero = true, warmup = 0)
    assert(f.isEmpty, f.mkString("\n"))
  }

  it should "keep wline off the high half when slot rdataPtr+1 is a stale cbo.zero slot (no cbo.zero behind)" in {
    val f = run("staleSlot", bothTogether, withZero = false, warmup = 16)
    assert(f.isEmpty, f.mkString("\n"))
  }

  it should "control: a cross-16B store with nothing behind it writes two ordinary halves" in {
    val f = run("control", bothTogether, withZero = false, warmup = 0)
    assert(f.isEmpty, f.mkString("\n"))
  }
}

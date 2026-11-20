package xiangshan.mem

import chisel3._
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util.{MuxLookup, PopCount}
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.{ChiselDB, Constantin, LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions,
  PerfCounterOptionsKey, XSPerfLevel}
import xiangshan._
import xiangshan.backend.decode.opcode.Opcode.castToUInt
import xiangshan.backend.fu.FuType

// Only observations are added to production RTL. Both flag fields matter:
// with 16 physical slots and StoreQueueMultiple=2, a full pointer period is 64.
private class StressPhysicalStoreQueue(implicit p: Parameters) extends PhysicalStoreQueue {
  val probe = IO(Output(new Bundle {
    val deq = new SqPtr
    val rdata = new SqPtr
    val commit = new SqPtr
    val occupied = UInt(5.W)
    val headCbo = Bool()
    val headCommitted = Bool()
    val youngerCommitted = Bool()
  }))
  probe.deq := deqPtrExt.head
  probe.rdata := rdataPtrExt.head
  probe.commit := commitPtrExt.head
  probe.occupied := PopCount(ctrlEntries.map(e => e.addrValid || e.dataValid))
  probe.headCbo := rdataCtrlEntries.head.isCbo
  probe.headCommitted := rdataCtrlEntries.head.committed
  probe.youngerCommitted := rdataCtrlEntries(1).committed
}

private class CboZeroStoreQueueStressHarness(implicit p: Parameters) extends XSModule {
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
    val preCommit = Input(new SqPtr)
    val preCommitValid = Input(Bool())
    val inject = Input(Vec(2, Bool()))
    val index = Input(Vec(2, new SqPtr))
    val zero = Input(Vec(2, Bool()))
    val size = Input(Vec(2, UInt(2.W)))
    val addr = Input(Vec(2, UInt(PAddrBits.W)))
    val data = Input(Vec(2, UInt(core.VLEN.W)))
    val ready = Input(Vec(2, Bool()))
    val empty = Input(Bool())
    val wbReady = Input(Bool())
    // A test-only fault at the observed interface proves the payload oracle.
    val corruptPayload = Input(Bool())
    val valid = Output(Vec(2, Bool()))
    val outAddr = Output(Vec(2, UInt(PAddrBits.W)))
    val outVaddr = Output(Vec(2, UInt(VAddrBits.W)))
    val outData = Output(Vec(2, UInt(core.VLEN.W)))
    val outMask = Output(Vec(2, UInt((core.VLEN / 8).W)))
    val wline = Output(Vec(2, Bool()))
    val wbValid = Output(Bool())
    val flush = Output(Bool())
    val dcacheValid = Output(Bool())
    val uncacheValid = Output(Bool())
    val deq = Output(new SqPtr)
    val rdata = Output(new SqPtr)
    val commit = Output(new SqPtr)
    val occupied = Output(UInt(5.W))
    val headCbo = Output(Bool())
    val headCommitted = Output(Bool())
    val youngerCommitted = Output(Bool())
  })
  private val sq = Module(new StressPhysicalStoreQueue)
  sq.io.hartId := 0.U
  sq.io.fromVirtualStoreQueue.redirectPtr.valid := false.B
  sq.io.fromVirtualStoreQueue.redirectPtr.bits := 0.U.asTypeOf(new SqPtr)
  sq.io.fromVirtualStoreQueue.preCommitPtr.valid := io.preCommitValid
  sq.io.fromVirtualStoreQueue.preCommitPtr.bits := io.preCommit
  sq.io.fromVirtualStoreQueue.retiredPtr := io.retired
  sq.io.fromVirtualStoreQueue.physicalQueueEnqPtr := io.enq
  sq.io.fromVirtualStoreQueue.headRobIdx := 0.U.asTypeOf(sq.io.fromVirtualStoreQueue.headRobIdx)
  for (i <- 0 until 2) {
    val ordinaryOp: UInt = MuxLookup(io.size(i), (LSUOpType.sd: UInt))(Seq(
      0.U -> (LSUOpType.sb: UInt), 1.U -> (LSUOpType.sh: UInt),
      2.U -> (LSUOpType.sw: UInt), 3.U -> (LSUOpType.sd: UInt)))
    val zeroOp: UInt = LSUOpType.cbo_zero
    val op = Mux(io.zero(i), zeroOp, ordinaryOp)
    val addr = WireInit(0.U.asTypeOf(new StoreAddrIO))
    addr.uop.sqIdx := io.index(i)
    addr.uop.fuOpType := op
    addr.vaddr := io.addr(i)
    addr.paddr := io.addr(i)
    addr.size := io.size(i)
    addr.memBackTypeMM := true.B
    addr.isLastRequest := true.B
    sq.io.fromStoreUnit.storeAddrIn(i).valid := io.inject(i)
    sq.io.fromStoreUnit.storeAddrIn(i).bits := addr
    // Held through the next edge to model StoreUnit stage 2.
    sq.io.fromStoreUnit.storeAddrInRe(i) := addr
    val data = WireInit(0.U.asTypeOf(new StoreQueueDataWrite))
    data.fuType := FuType.stu.U
    data.fuOpType := op
    data.sqIdx := io.index(i)
    data.data := io.data(i)
    sq.io.storeDataIn(i).valid := io.inject(i)
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
    io.outVaddr(i) := req.bits.vaddr
    io.outData(i) := req.bits.data ^ Mux(io.corruptPayload && i.U === 0.U, 1.U, 0.U)
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
  io.flush := sq.io.sbufferCtrl.req.flush
  io.dcacheValid := sq.io.toDCache.req.valid
  io.uncacheValid := sq.io.toUncacheBuffer.req.valid
  io.deq := sq.probe.deq
  io.rdata := sq.probe.rdata
  io.commit := sq.probe.commit
  io.occupied := sq.probe.occupied
  io.headCbo := sq.probe.headCbo
  io.headCommitted := sq.probe.headCommitted
  io.youngerCommitted := sq.probe.youngerCommitted
}

// Three operations suffice: older normal store held in the real FIFO, CBO.ZERO,
// and one younger normal store. No circular wrap or random stimulus is needed.
class CboZeroStoreQueueBufferedPredecessorTest extends AnyFlatSpec with ChiselSim {
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

  behavior of "PhysicalStoreQueue CBO.ZERO with a buffered predecessor"
  it should "emit its own zero request before writeback after an older FIFO request fires" in {
    import chisel3.simulator.HasSimulator.simulators.verilator
    Constantin.init(false)
    ChiselDB.init(false)
    simulate(new CboZeroStoreQueueStressHarness) { dut =>
      def bool(s: Bool): Boolean = s.peek().litToBoolean
      def value(s: UInt): BigInt = s.peek().litValue
      def ptr(p: SqPtr): Int = (if (bool(p.first_flag)) 32 else 0) + value(p.second_flag).toInt * 16 + value(p.value).toInt
      def pokePtr(p: SqPtr, n: Int): Unit = {
        p.first_flag.poke(false.B)
        p.second_flag.poke(0.U)
        p.value.poke(n.U)
      }
      val addresses = Seq(0x4000, 0x4040, 0x4080).map(BigInt(_))
      val payloads = Seq("1122334455667788", "ffffffffffffffff", "99aabbccddeeff00").map(BigInt(_, 16))
      dut.io.inject.foreach(_.poke(false.B))
      dut.io.ready.foreach(_.poke(false.B))
      dut.io.empty.poke(false.B)
      dut.io.wbReady.poke(false.B)
      dut.io.corruptPayload.poke(false.B)
      pokePtr(dut.io.enq, 0)
      pokePtr(dut.io.retired, 0)
      pokePtr(dut.io.preCommit, 0)
      dut.io.preCommitValid.poke(false.B)
      for (i <- 0 until 2) {
        pokePtr(dut.io.index(i), 0)
        dut.io.zero(i).poke(false.B)
        dut.io.size(i).poke(3.U)
        dut.io.addr(i).poke(0.U)
        dut.io.data(i).poke(0.U)
      }
      dut.reset.poke(true.B)
      dut.clock.step(4)
      dut.reset.poke(false.B)
      dut.clock.step(4)
      def drive(i: Int, id: Int): Unit = {
        pokePtr(dut.io.index(i), id)
        dut.io.zero(i).poke((id == 1).B)
        dut.io.addr(i).poke(addresses(id).U)
        dut.io.data(i).poke(payloads(id).U)
        dut.io.inject(i).poke(true.B)
      }
      drive(0, 0)
      drive(1, 1)
      pokePtr(dut.io.enq, 2)
      // The older store retired. CBO is at the ROB head and precommits, but
      // CBO and its younger store have not retired before CBO writeback.
      pokePtr(dut.io.retired, 1)
      pokePtr(dut.io.preCommit, 2)
      dut.io.preCommitValid.poke(true.B)
      dut.clock.step()
      dut.io.inject.foreach(_.poke(false.B))
      dut.clock.step() // Preserve matching StoreUnit stage-2 inputs.
      drive(0, 2)
      pokePtr(dut.io.enq, 3)
      dut.clock.step()
      dut.io.inject.foreach(_.poke(false.B))
      dut.clock.step()
      var wait = 0
      while (!(ptr(dut.io.rdata) == 1 && bool(dut.io.headCbo) && bool(dut.io.headCommitted) &&
        !bool(dut.io.youngerCommitted) && bool(dut.io.valid(0))) && wait < 30) {
        dut.clock.step()
        wait += 1
      }
      assert(wait < 30, "HARNESS timeout establishing buffered predecessor/CBO/younger")
      dut.clock.step(4) // Let CBO FSM settle while the predecessor remains blocked.
      assert(value(dut.io.outAddr(0)) == addresses(0) && !bool(dut.io.wline(0)))
      assert(value(dut.io.outData(0)) == payloads(0) && value(dut.io.outMask(0)) == 255)
      assert(!bool(dut.io.valid(1)) && !bool(dut.io.flush) && !bool(dut.io.wbValid))
      println(s"MIN_PRE oldAddr=${value(dut.io.outAddr(0)).toString(16)} oldWline=${bool(dut.io.wline(0))} " +
        s"deq=${ptr(dut.io.deq)} rdata=${ptr(dut.io.rdata)} headCbo=${bool(dut.io.headCbo)} " +
        s"committed=${bool(dut.io.headCommitted)}/${bool(dut.io.youngerCommitted)} flush=${bool(dut.io.flush)}")
      // Only the ordinary predecessor fires at this edge. The zero request has
      // not yet entered the FIFO, because the FIFO was nonempty before the edge.
      dut.io.ready(0).poke(true.B)
      dut.clock.step()
      println(s"MIN_AFTER_OLD_FIRE valid=${bool(dut.io.valid(0))}/${bool(dut.io.valid(1))} " +
        s"deq=${ptr(dut.io.deq)} rdata=${ptr(dut.io.rdata)} flush=${bool(dut.io.flush)} wb=${bool(dut.io.wbValid)}")
      var next = 1
      var zeroEmitted = false
      var wbDone = false
      var cycles = 0
      var wbAge = 0
      while (cycles < 80 && !(next == 3 && wbDone && ptr(dut.io.deq) == 3 && ptr(dut.io.rdata) == 3)) {
        if (wbDone) {
          pokePtr(dut.io.retired, 3)
          dut.io.preCommitValid.poke(false.B)
        }
        dut.io.ready.foreach(_.poke(true.B))
        dut.io.empty.poke((cycles >= 3).B) // Bounded modeled downstream completion.
        val wbValid = bool(dut.io.wbValid)
        val wbReady = wbValid && wbAge >= 5
        dut.io.wbReady.poke(wbReady.B)
        for (i <- 0 until 2 if bool(dut.io.valid(i))) {
          assert(next < 3 && value(dut.io.outAddr(i)) == addresses(next),
            s"minimized address/order next=$next observed=${value(dut.io.outAddr(i)).toString(16)}")
          assert(bool(dut.io.wline(i)) == (next == 1), "minimized wrong wline")
          if (next == 1) {
            assert(value(dut.io.outData(i)) == 0, "minimized nonzero CBO payload")
            zeroEmitted = true
          } else {
            assert(wbDone, "minimized younger store preceded CBO completion")
            assert(value(dut.io.outData(i)) == payloads(next) && value(dut.io.outMask(i)) == 255)
          }
          println(s"MIN_SB cycle=$cycles id=$next addr=${value(dut.io.outAddr(i)).toString(16)} data=${value(dut.io.outData(i)).toString(16)}")
          next += 1
        }
        println(s"MIN_STATE cycle=$cycles next=$next zeroEmitted=$zeroEmitted wb=$wbValid/$wbReady " +
          s"flush=${bool(dut.io.flush)} deq=${ptr(dut.io.deq)} rdata=${ptr(dut.io.rdata)}")
        if (wbValid) {
          assert(zeroEmitted,
            "zero writeback became valid without its zero request after buffered predecessor fired")
          wbAge += 1
          if (wbReady) {
            assert(!wbDone, "minimized duplicate CBO writeback")
            wbDone = true
          }
        }
        dut.clock.step()
        cycles += 1
      }
      assert(cycles < 80, s"minimized timeout next=$next zeroEmitted=$zeroEmitted wbDone=$wbDone")
      assert(value(dut.io.occupied) == 0 && !dut.io.valid.exists(bool) && !bool(dut.io.wbValid))
      println(s"MIN_RESULT operations=3 zero=1 younger=1 cycles=$cycles deq=${ptr(dut.io.deq)} rdata=${ptr(dut.io.rdata)}")
    }
  }
}

class CboZeroStoreQueueStressTest extends AnyFlatSpec with ChiselSim {
  private val base = new DefaultConfig
  private val core = base(XSTileKey).head.copy(StoreQueuePhysicalSize = 16, HasCMO = true)
  private implicit val config: Parameters = base.alterPartial {
    case XSCoreParamsKey => core
    case DebugOptionsKey => DebugOptions(
      FPGAPlatform = false, EnableDifftest = false, AlwaysBasicDiff = false, FullBasicDiff = false,
      EnableDebug = false, EnablePerfDebug = false, EnableXMR = false, EnableConstantin = false,
      EnableChiselDB = false, AlwaysBasicDB = false, EnableRollingDB = false)
    // Keep production XSError active in all stress scenarios.
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
  }

  behavior of "PhysicalStoreQueue CBO.ZERO payload and circular pointers"

  it should "preserve ordered payloads and drain with bounded downstream stalls across actual wraps" in {
    import chisel3.simulator.HasSimulator.simulators.verilator
    Constantin.init(false)
    ChiselDB.init(false)
    simulate(new CboZeroStoreQueueStressHarness) { dut =>
      def bool(s: Bool): Boolean = s.peek().litToBoolean
      def value(s: UInt): BigInt = s.peek().litValue
      def raw(p: SqPtr): Int = (if (bool(p.first_flag)) 32 else 0) + value(p.second_flag).toInt * 16 + value(p.value).toInt
      def pokePtr(p: SqPtr, n: Int): Unit = {
        p.first_flag.poke(((n / 32) % 2 == 1).B)
        p.second_flag.poke(((n / 16) % 2).U)
        p.value.poke((n % 16).U)
      }
      case class Op(id: Int, zero: Boolean) {
        val addr: BigInt = 0x10000 + id * 0x40
        val size: Int = if (zero) 3 else id % 4
        val bytes: Int = 1 << size
        // Every active byte identifies its operation; upper bytes are also distinct.
        val data: BigInt = (0 until 16).map(b => BigInt((id * 37 + b * 13 + 0x51) & 0xff) << (b * 8)).reduce(_ | _)
        val mask: BigInt = (BigInt(1) << bytes) - 1
        val activeData: BigInt = data & ((BigInt(1) << (bytes * 8)) - 1)
      }
      case class Packet(addr: BigInt, vaddr: BigInt, data: BigInt, mask: BigInt, line: Boolean)
      def packet(i: Int): Packet = Packet(value(dut.io.outAddr(i)), value(dut.io.outVaddr(i)),
        value(dut.io.outData(i)), value(dut.io.outMask(i)), bool(dut.io.wline(i)))

      def runScenario(kind: String, pattern: Int, corrupt: Boolean = false): Unit = {
        dut.io.inject.foreach(_.poke(false.B))
        dut.io.ready.foreach(_.poke(false.B))
        dut.io.empty.poke(true.B)
        dut.io.wbReady.poke(false.B)
        dut.io.corruptPayload.poke(corrupt.B)
        pokePtr(dut.io.enq, 0)
        pokePtr(dut.io.retired, 0)
        pokePtr(dut.io.preCommit, 0)
        dut.io.preCommitValid.poke(false.B)
        for (i <- 0 until 2) {
          pokePtr(dut.io.index(i), 0)
          dut.io.zero(i).poke(false.B)
          dut.io.size(i).poke(3.U)
          dut.io.addr(i).poke(0.U)
          dut.io.data(i).poke(0.U)
        }
        dut.reset.poke(true.B)
        dut.clock.step(4)
        dut.reset.poke(false.B)
        dut.clock.step(4)
        val count = if (corrupt) 2 else 160
        val ops = (0 until count).map { id =>
          // Warmup puts the first target precisely at physical slot 15.
          val zero = id >= 15 && (kind == "alone" || (kind == "mixed" && (id % 16 == 15 || id % 16 == 5)))
          Op(id, zero)
        }
        val expected = scala.collection.mutable.Queue.empty[Op]
        val completion = scala.collection.mutable.Queue.empty[Op]
        val pending = scala.collection.mutable.Queue.empty[Int]
        val emittedZero = scala.collection.mutable.Set.empty[Int]
        val completedZero = scala.collection.mutable.Set.empty[Int]
        val emptyDelay = Seq(1, 7, 13)(pattern)
        val wbDelay = Seq(0, 5, 11)(pattern)
        var cycle = 0
        var injected = 0
        var accepted = 0
        var completed = 0
        var wbAge = 0
        var wbHeld = 0
        var sbHeld = 0
        var dualFire = 0
        var singleFire = 0
        var boundaryBlocked = 0
        var peakOccupancy = 0
        var peakOutstanding = 0
        var previousDeq = 0
        var previousRead = 0
        var deqTotal = 0
        var readTotal = 0
        var deqWraps = 0
        var readWraps = 0
        var deqFirstWraps = 0
        var readFirstWraps = 0
        var injectGap = false
        def step(): Unit = {
          dut.clock.step()
          cycle += 1
        }
        // Counts come from offered inputs and accepted SB/WB operations, never SQ credits.
        while (cycle < 6000 && (injected < count || completed < count || deqTotal < count || pending.nonEmpty)) {
          while (pending.headOption.exists(_ <= cycle)) pending.dequeue()
          val deqRaw = raw(dut.io.deq)
          val readRaw = raw(dut.io.rdata)
          val dd = (deqRaw - previousDeq + 64) % 64
          val rd = (readRaw - previousRead + 64) % 64
          assert(dd <= 2 && rd <= 2, s"pointer jump $kind/$pattern cycle=$cycle deqDelta=$dd readDelta=$rd")
          deqTotal += dd
          readTotal += rd
          if (dd > 0 && deqRaw % 16 < previousDeq % 16) {
            deqWraps += 1
            println(s"WRAP $kind/$pattern deq cycle=$cycle raw=$previousDeq->$deqRaw total=$deqTotal")
          }
          if (rd > 0 && readRaw % 16 < previousRead % 16) {
            readWraps += 1
            println(s"WRAP $kind/$pattern rdata cycle=$cycle raw=$previousRead->$readRaw total=$readTotal")
          }
          if (dd > 0 && deqRaw < previousDeq) deqFirstWraps += 1
          if (rd > 0 && readRaw < previousRead) readFirstWraps += 1
          previousDeq = deqRaw
          previousRead = readRaw
          assert(deqTotal <= readTotal && readTotal <= injected,
            s"pointer order $kind/$pattern cycle=$cycle deq=$deqTotal read=$readTotal injected=$injected")
          assert(deqTotal <= completed,
            s"dequeue preceded independent completion $kind/$pattern cycle=$cycle deq=$deqTotal completed=$completed")
          val occupied = value(dut.io.occupied).toInt
          peakOccupancy = peakOccupancy.max(occupied)
          peakOutstanding = peakOutstanding.max(injected - deqTotal)
          assert(occupied <= injected - deqTotal, s"occupancy exceeds live input count cycle=$cycle")
          assert(!bool(dut.io.dcacheValid) && !bool(dut.io.uncacheValid), "wrong downstream path for cacheable/CBO.ZERO")

          // No back-to-back STA injections: preserve each corresponding STA stage-2 input.
          dut.io.inject.foreach(_.poke(false.B))
          if (!injectGap && injected < count && injected - deqTotal <= 10 &&
            (kind != "alone" || injected == deqTotal)) {
            val n = if (kind == "alone") 1 else (count - injected).min(2)
            for (i <- 0 until n) {
              val op = ops(injected + i)
              pokePtr(dut.io.index(i), op.id)
              dut.io.zero(i).poke(op.zero.B)
              dut.io.size(i).poke(op.size.U)
              dut.io.addr(i).poke(op.addr.U)
              dut.io.data(i).poke(op.data.U)
              dut.io.inject(i).poke(true.B)
              expected.enqueue(op)
              completion.enqueue(op)
            }
            injected += n
            pokePtr(dut.io.enq, injected)
            pokePtr(dut.io.retired, injected)
            injectGap = true
          } else injectGap = false
          val r0 = pattern match {
            case 0 => true
            case 1 => cycle % 11 >= 6
            case 2 => cycle % 17 >= 10
          }
          val r1 = r0 && (pattern == 0 || (pattern == 1 && cycle % 11 >= 8) ||
            (pattern == 2 && cycle % 3 == 0))
          dut.io.ready(0).poke(r0.B)
          dut.io.ready(1).poke(r1.B)
          dut.io.empty.poke(pending.isEmpty.B)
          val wbValid = bool(dut.io.wbValid)
          val wbReady = wbValid && wbAge >= wbDelay
          dut.io.wbReady.poke(wbReady.B)
          if (wbValid && !wbReady) {
            wbHeld += 1
            wbAge += 1
          }
          if (wbValid && wbReady) {
            assert(completion.headOption.exists(_.zero), s"unexpected/out-of-order writeback cycle=$cycle")
            val op = completion.dequeue()
            assert(emittedZero(op.id) && !completedZero(op.id), s"CBO writeback before/without unique line id=${op.id}")
            assert(pending.isEmpty, s"CBO writeback before downstream became empty id=${op.id}")
            completedZero += op.id
            completed += 1
            wbAge = 0
            println(s"WB $kind/$pattern cycle=$cycle id=${op.id} ptr=$readRaw")
          }
          if (!wbValid) wbAge = 0

          if (kind == "mixed" && bool(dut.io.headCbo) && bool(dut.io.headCommitted) &&
            bool(dut.io.youngerCommitted) && readRaw % 16 == 15) {
            boundaryBlocked += 1
            // rdata can lead FIFO egress: a public lane may still carry an older
            // operation. Determine age from its distinct address, not lane number.
            for (i <- 0 until 2 if bool(dut.io.valid(i))) {
              val out = packet(i)
              val observed = ops.find(_.addr == out.addr)
              assert(observed.nonEmpty, s"unknown buffered address ${out.addr.toString(16)}")
              assert(observed.get.id <= readTotal,
                s"younger buffered request crossed CBO at slot 15 cycle=$cycle id=${observed.get.id} head=$readTotal")
            }
          }
          var fires = 0
          for (i <- 0 until 2) {
            val valid = bool(dut.io.valid(i))
            val ready = if (i == 0) r0 else r1
            if (valid && !ready) sbHeld += 1
            if (valid && ready) {
              assert(expected.nonEmpty, s"duplicate/unoffered SB request cycle=$cycle lane=$i")
              val op = expected.dequeue()
              val out = packet(i)
              assert(out.addr == op.addr && out.vaddr == op.addr && out.line == op.zero,
                s"address/order id=${op.id} expectedAddr=${op.addr.toString(16)} observed=$out cycle=$cycle lane=$i")
              if (op.zero) {
                assert(out.data == 0, s"zero payload id=${op.id} observed=${out.data.toString(16)}")
                assert(completion.headOption.contains(op), s"CBO emitted before older completions id=${op.id}")
                assert(!emittedZero(op.id), s"duplicate zero line id=${op.id}")
                emittedZero += op.id
              } else {
                val byteBits = (BigInt(1) << (op.bytes * 8)) - 1
                assert(out.mask == op.mask && (out.data & byteBits) == op.activeData,
                  s"payload id=${op.id} expectedMask=${op.mask.toString(16)} expectedData=${op.activeData.toString(16)} " +
                    s"observedMask=${out.mask.toString(16)} observedData=${(out.data & byteBits).toString(16)} cycle=$cycle lane=$i")
                assert(completion.headOption.contains(op), s"ordinary store crossed unfinished CBO id=${op.id}")
                completion.dequeue()
                completed += 1
              }
              println(s"SB $kind/$pattern cycle=$cycle lane=$i id=${op.id} zero=${op.zero} " +
                s"addr=${out.addr.toString(16)} mask=${out.mask.toString(16)} data=${out.data.toString(16)}")
              pending.enqueue(cycle + emptyDelay)
              accepted += 1
              fires += 1
            }
          }
          if (fires == 2) dualFire += 1
          if (fires == 1) singleFire += 1
          println(s"STATE $kind/$pattern cycle=$cycle injected=$injected accepted=$accepted completed=$completed " +
            s"deq=$deqRaw/$deqTotal rdata=$readRaw/$readTotal commit=${raw(dut.io.commit)} " +
            s"occupied=$occupied pending=${pending.size} ready=$r0/$r1 wb=$wbValid/$wbReady")
          step()
        }
        assert(cycle < 6000, s"timeout $kind/$pattern injected=$injected accepted=$accepted completed=$completed deq=$deqTotal read=$readTotal")
        assert(expected.isEmpty && completion.isEmpty && accepted == count && completed == count)
        assert(deqTotal == count && readTotal == count && value(dut.io.occupied) == 0)
        assert(raw(dut.io.deq) == count % 64 && raw(dut.io.rdata) == count % 64)
        assert(deqWraps == 10 && readWraps == 10 && deqFirstWraps == 2 && readFirstWraps == 2,
          s"HARNESS missing observed wraps deq=$deqWraps/$deqFirstWraps read=$readWraps/$readFirstWraps")
        assert(emittedZero.size == ops.count(_.zero) && completedZero == emittedZero)
        if (kind == "mixed") assert(boundaryBlocked > 0, "HARNESS no slot-15 committed CBO/younger observation")
        if (kind == "normal") assert(dualFire > 0, "HARNESS no ordinary two-lane consumption")
        if (pattern > 0) {
          assert(sbHeld > 0 && singleFire > 0, "HARNESS missing SB stall/single-lane coverage")
          if (kind != "normal") assert(wbHeld > 0, "HARNESS missing delayed writeback coverage")
        }
        // Drain tail catches duplicate emissions and unexpected pointer movement after completion.
        for (_ <- 0 until 8) {
          dut.io.inject.foreach(_.poke(false.B))
          dut.io.ready.foreach(_.poke(true.B))
          dut.io.empty.poke(true.B)
          dut.io.wbReady.poke(true.B)
          assert(!dut.io.valid.exists(bool) && !bool(dut.io.wbValid), "duplicate drain-tail request")
          assert(raw(dut.io.deq) == count % 64 && raw(dut.io.rdata) == count % 64)
          step()
        }
        println(s"RESULT $kind/$pattern operations=$count zero=${emittedZero.size} cycles=$cycle " +
          s"deqWraps=$deqWraps readWraps=$readWraps deqFirstWraps=$deqFirstWraps readFirstWraps=$readFirstWraps " +
          s"peakOccupancy=$peakOccupancy peakOutstanding=$peakOutstanding boundaryBlocked=$boundaryBlocked " +
          s"sbHeld=$sbHeld wbHeld=$wbHeld dualFire=$dualFire singleFire=$singleFire")
      }
      for (kind <- Seq("normal", "alone", "mixed"); pattern <- 0 until 3) runScenario(kind, pattern)
      val fault = intercept[org.scalatest.exceptions.TestFailedException] {
        runScenario("normal", 0, corrupt = true)
      }
      assert(fault.getMessage.contains("payload id=0 expectedMask"), s"HARNESS wrong negative-control failure: $fault")
      println(s"NEGATIVE_CONTROL rejected injected bit-0 payload corruption: ${fault.getMessage}")
    }
  }
}

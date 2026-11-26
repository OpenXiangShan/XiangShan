package xiangshan.mem

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.ChiselSim
import chisel3.testing.HasTestingDirectory
import java.nio.file.Paths
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import top.DefaultConfig
import utility.{Constantin, LogUtilsOptions, LogUtilsOptionsKey, PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.{XSCoreParamsKey, XSTileKey}

class RarMergeTest extends AnyFlatSpec with ChiselSim with Matchers {
  private val config = new DefaultConfig
  private implicit val parameters: Parameters = config.alterPartial {
    case XSCoreParamsKey => config(XSTileKey).head.copy(
      LoadQueueRARSize = 16
    )
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(enableDebug = false, enablePerf = false, fpgaPlatform = false)
  }
  private implicit val testingDirectory: HasTestingDirectory = new HasTestingDirectory {
    override def getDirectory = Paths.get(System.getProperty("java.io.tmpdir"), "rar-merge-module-test")
  }

  private def clearInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(clearInputs)
    case vec: Vec[_] => vec.foreach(clearInputs)
    case bit: Bool if DataMirror.directionOf(bit) == ActualDirection.Input => bit.poke(false.B)
    case uint: UInt if DataMirror.directionOf(uint) == ActualDirection.Input => uint.poke(0.U)
    case _ =>
  }

  it should "preserve query and release timing across reservations, merges and redirect recovery" in {
    simulate({
      Constantin.init(false)
      try new LoadQueueRAR finally Constantin.init(true)
    }) { dut =>
      val core = parameters(XSCoreParamsKey)
      val line = 0x80000000L
      case class Load(rob: Int, lq: Int, addr: Long = line, nc: Boolean = false, head: Boolean = false,
                      former: Boolean = true, data: Boolean = true)
      type Batch = Map[Int, Load]
      def load(age: Int): Load = Load(age, age)
      def probe(age: Int = 0, addr: Long = line): Batch = Map(2 -> Load(age, 0, addr))
      def rob(ptr: xiangshan.backend.rob.RobPtr, value: Int, former: Boolean): Unit = {
        ptr.value.poke((value % core.RobSize).U)
        ptr.flag.poke((value >= core.RobSize).B)
        ptr.isFormer.poke(former.B)
      }
      def lq(ptr: LqPtr, value: Int): Unit = {
        ptr.value.poke((value % core.VirtualLoadQueueSize).U)
        ptr.flag.poke((value >= core.VirtualLoadQueueSize).B)
      }
      var inS2 = Map.empty[Int, Load]
      var ready = Map.empty[Int, Boolean]
      def cycle(next: Batch = Map.empty, queryOnly: Batch = Map.empty, drop: Set[Int] = Set.empty,
                revoke: Set[Int] = Set.empty, late: Set[Int] = Set.empty, wb: Int = 0,
                pending: Int = 0, pendingFormer: Boolean = true, redirect: Option[Int] = None,
                redirectFormer: Boolean = true, flushSelf: Boolean = false, release: Option[Long] = None): Unit = {
        clearInputs(dut.io)
        lq(dut.io.ldWbPtr, wb)
        rob(dut.io.robPendingPtr, pending, pendingFormer)
        redirect.foreach { age =>
          dut.io.redirect.valid.poke(true.B)
          rob(dut.io.redirect.bits.robIdx, age, redirectFormer)
          dut.io.redirect.bits.level.poke(flushSelf.B)
        }
        release.foreach { addr =>
          dut.io.release.valid.poke(true.B)
          dut.io.release.bits.paddr.poke(addr.U)
        }
        for ((port, a) <- next) {
          val r = dut.io.query(port).s1Reserve
          r.valid.poke(true.B)
          rob(r.bits.robIdx, a.rob, a.former)
          lq(r.bits.lqIdx, a.lq)
        }
        val requests = (inS2 -- drop) ++ queryOnly
        for ((port, a) <- requests) {
          val q = dut.io.query(port)
          q.req.valid.poke(true.B)
          rob(q.req.bits.robIdx, a.rob, a.former)
          lq(q.req.bits.lqIdx, a.lq)
          q.req.bits.paddr.poke(a.addr.U)
          q.req.bits.nc.poke(a.nc.B)
          q.req.bits.dataValid.poke(a.data.B)
          q.mayLateRevoke.poke(a.head.B)
        }
        revoke.foreach(p => dut.io.query(p).revokeLastCycle.poke(true.B))
        late.foreach(p => dut.io.query(p).revokeLastLastCycle.poke(true.B))
        ready = requests.keys.map(p => p -> dut.io.query(p).req.ready.peek().litToBoolean).toMap
        dut.clock.step()
        inS2 = next
        for (p <- 0 until 3) dut.io.query(p).resp.valid.expect(requests.contains(p).B)
      }
      def reset(): Unit = {
        inS2 = Map.empty
        clearInputs(dut.io)
        dut.reset.poke(true.B)
        dut.clock.step(2)
        dut.reset.poke(false.B)
        cycle()
      }
      def count(n: Int): Unit = dut.io.validCount.expect(n.U)
      def nuke(expected: Boolean): Unit = dut.io.query(2).resp.bits.nuke.expect(expected.B)
      def insert(batch: Batch): Unit = {
        cycle(next = batch)
        cycle()
        ready.values.foreach(_ shouldBe true)
        cycle()
      }

      // Three misses share the oldest port's slot. Its replay does not cancel its peers.
      reset()
      val group = Map(0 -> Load(14, 12), 1 -> Load(8, 16, nc = true), 2 -> Load(10, 10))
      cycle(next = group)
      cycle()
      count(0)
      cycle(revoke = Set(1), queryOnly = probe(7))
      nuke(true) // S3 bypass still observes the revoked port's S2 release history.
      count(1)
      cycle(queryOnly = probe(7)); nuke(false)
      cycle(wb = 12); count(0)
      cycle(next = group)
      cycle()
      cycle(revoke = group.keySet); count(0)

      // An existing row receives one commit; its two maxima can come from different loads.
      for (revoked <- Seq(Set.empty[Int], Set(1), Set(0, 2), group.keySet)) {
        reset()
        insert(Map(0 -> load(6)))
        cycle(next = group)
        cycle(); count(1)
        cycle(revoke = revoked); count(1)
        cycle(queryOnly = probe(12))
        nuke(!revoked(0) && !revoked(1))
        val boundary = (Seq(6) ++ (group -- revoked).values.map(_.lq)).max
        cycle(wb = boundary - 1); count(1)
        cycle(wb = boundary); count(0)
      }
      reset()
      cycle(next = Map(0 -> load(7), 1 -> load(9)))
      cycle()
      cycle(wb = 7); count(1)
      cycle(wb = 9); count(0)

      // ROB and LQ maxima remain independent for either arrival order.
      for (batch <- Seq(Seq(Load(12, 16), Load(14, 14)), Seq(Load(14, 14), Load(12, 16)))) {
        reset()
        batch.foreach(a => insert(Map(0 -> a.copy(nc = true))))
        count(1)
        cycle(queryOnly = probe(13)); nuke(true)
        cycle(queryOnly = probe(14)); nuke(false)
        cycle(wb = 14); count(1)
        cycle(wb = 16); count(0)
      }

      // Consecutive same-line requests can query the pending row, but cannot merge into it yet.
      reset()
      cycle(next = Map(0 -> load(4)), release = Some(line))
      cycle(next = Map(1 -> load(7)))
      cycle(queryOnly = probe()); nuke(true); count(1)
      cycle(); count(2)
      cycle(queryOnly = probe(3)); nuke(true)

      // With no data in S2, a release at S3 becomes query-visible exactly two cycles later.
      reset()
      cycle(next = Map(0 -> Load(5, 5, data = false)))
      cycle()
      cycle(queryOnly = probe(), release = Some(line)); nuke(false)
      cycle(queryOnly = probe()); nuke(false)
      cycle(queryOnly = probe()); nuke(true)

      // Replacing an expired row must discard a delayed mark for its previous occupant.
      cycle(wb = 5)
      insert(Map(0 -> Load(6, 6, data = false)))
      cycle(next = Map(0 -> Load(9, 9, data = false)))
      cycle(release = Some(line))
      cycle(wb = 6); count(1)
      cycle(queryOnly = probe()); nuke(false)

      // A flushed S2 reservation is reclaimed even when the request has disappeared.
      reset()
      cycle(next = Map(0 -> load(18)))
      cycle(redirect = Some(15), drop = Set(0))
      cycle(); count(0)
      insert(Map(0 -> Load(18, 18, nc = true)))
      cycle(next = Map(1 -> load(12)))
      cycle()
      cycle(redirect = Some(15)); count(1)
      cycle(queryOnly = probe()); nuke(false)
      cycle(wb = 12); count(0)
      insert(Map(0 -> load(18)))
      cycle(next = Map(1 -> load(12)))
      cycle(redirect = Some(15)); count(0)
      cycle(); count(1)
      cycle(wb = 12); count(0)

      // Non-aligned head stays private through its S4 revoke.
      reset()
      insert(Map(0 -> load(6)))
      cycle(next = Map(0 -> Load(11, 11, head = true), 1 -> load(12)))
      cycle()
      cycle(); count(2)
      cycle(late = Set(0)); count(1)
      cycle(wb = 12); count(0)

      // Collision and redirect recovery, including a surviving writer to the flushed row.
      reset()
      val alias = line + (1L << 16) + (1L << 27)
      insert(Map(0 -> Load(10, 10, nc = true), 1 -> Load(18, 18, alias)))
      count(1)
      cycle(queryOnly = probe(9, alias)); nuke(true)
      cycle(next = Map(0 -> load(12)))
      cycle()
      cycle(redirect = Some(15)); count(1)
      cycle(queryOnly = probe(14)); nuke(true)
      cycle(queryOnly = probe(15)); nuke(false)
      cycle(redirect = Some(13), redirectFormer = false, wb = 18); count(1)
      cycle(pending = 13, wb = 18); count(1)
      cycle(pending = 13, pendingFormer = false, wb = 18); count(0)

      // A recovered row and a new path on the same line retire independently.
      reset()
      insert(Map(0 -> load(8), 1 -> load(14)))
      cycle(redirect = Some(14), flushSelf = true, wb = 14); count(1)
      insert(Map(0 -> load(11))); count(2)
      cycle(wb = 11, pending = 10); count(1)
      cycle(wb = 11, pending = 14); count(0)

      // Former/latter and wraparound must compare complete ROB and LQ pointers.
      reset()
      val lastRob = core.RobSize - 2
      val lastLq = core.VirtualLoadQueueSize - 2
      cycle(next = Map(0 -> Load(lastRob, lastLq), 1 -> Load(lastRob + 3, lastLq + 3, nc = true)), wb = lastLq - 1)
      cycle(wb = lastLq - 1)
      cycle(wb = lastLq - 1); count(1)
      cycle(queryOnly = Map(2 -> Load(lastRob + 1, lastLq - 1)), wb = lastLq - 1); nuke(true)
      cycle(redirect = Some(lastRob + 1), wb = lastLq - 1); count(1)
      cycle(pending = lastRob, wb = lastLq - 1); count(1)
      cycle(pending = lastRob + 2, wb = lastLq - 1); count(0)
      reset()
      insert(Map(0 -> Load(7, 9), 1 -> Load(7, 5, nc = true)))
      cycle(queryOnly = probe(7), wb = 5); nuke(false); count(1)
      cycle(wb = 9); count(0)

      // Only one row retires at K. S1 can first obtain its refilled candidate at K+2.
      reset()
      for (age <- 1 to 16) cycle(next = Map(0 -> Load(age, age, line + 64L * age)))
      cycle(); cycle(); count(16)
      val retry = Map(0 -> Load(24, 24, line + 64L * 16))
      cycle(next = retry)
      cycle(next = retry, wb = 1); ready(0) shouldBe false; count(15) // K
      cycle(next = retry, wb = 1); ready(0) shouldBe false
      cycle(next = retry, wb = 1); ready(0) shouldBe false            // K+2: S1 grant
      cycle(wb = 1); ready(0) shouldBe true
      cycle(wb = 1); count(15)
      cycle(wb = 24); count(0)

      // Compare against individual accepted loads. Epochs end with a redirect
      // and drain so the oracle keeps exact surviving members across recovery.
      case class Member(port: Int, a: Load, marked: Boolean, due: Boolean, inS3: Boolean)
      val rng = new scala.util.Random(90210)
      var hits = 0
      var extra = 0
      for (_ <- 0 until 25) {
        reset()
        var rows = Seq.empty[Member]
        var prevRelease = Option.empty[Long]
        val ages = rng.shuffle((1 to 60).toList)
        for (t <- 0 until 22) {
          val wb = t / 3
          val next = (0 until 3).collect {
            case p if t < 20 && rng.nextInt(6) != 0 =>
              val age = ages(t * 3 + p)
              p -> Load(age, age, line + 64L * rng.nextInt(4), nc = rng.nextInt(7) == 0, data = rng.nextBoolean())
          }.toMap
          val rel = Option.when(rng.nextBoolean())(line + 64L * rng.nextInt(4))
          val cancel = (0 until 3).filter(_ => rng.nextInt(6) == 0).toSet
          val red = Option.when(t == 20)(30)
          val querying = inS2
          val expected = querying.map { case (p, a) =>
            p -> rows.exists(m => m.a.addr == a.addr && m.marked && m.a.rob > a.rob)
          }
          cycle(next = next, wb = wb, release = rel, revoke = cancel, redirect = red)
          for ((p, shouldNuke) <- expected) {
            val signal = dut.io.query(p).resp.bits.nuke
            if (shouldNuke) { signal.expect(true.B); hits += 1 }
            else if (signal.peek().litToBoolean) extra += 1
          }
          rows = rows.collect {
            case m if m.a.lq > wb && !red.exists(_ < m.a.rob) && !(m.inS3 && cancel(m.port)) =>
              m.copy(marked = m.marked || m.due, due = rel.contains(m.a.addr), inS3 = false)
          } ++ querying.collect {
            case (p, a) if ready(p) && a.lq > wb && !red.exists(_ < a.rob) =>
              Member(p, a, a.nc || a.data && (rel.contains(a.addr) || prevRelease.contains(a.addr)), false, true)
          }
          prevRelease = rel
        }
        cycle(wb = 60, pending = 60); count(0)
      }
      hits should be > 0
      info(s"Per-load comparison: $hits required hits, $extra additional nukes")
    }
  }
}

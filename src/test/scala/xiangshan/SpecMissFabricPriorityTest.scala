package xiangshan

import chisel3._
import chisel3.util._
import chisel3.simulator.scalatest.ChiselSim
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.tilelink._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility.{PerfCounterOptions, PerfCounterOptionsKey, XSPerfLevel}
import xiangshan.cache.SpecMissFabricIO

import scala.collection.mutable

// Drive the real nexus (including both arbiters, ID remapping, beat locks and the bank FIFOs).
private class PriorityFabricHarness(preview: Boolean)(implicit p: Parameters) extends LazyModule with HasXSParameter {
  val clients = Seq("dcache", "dcache_ch1", "other").map { name =>
    TLClientNode(Seq(TLMasterPortParameters.v1(Seq(TLMasterParameters.v1(name, IdRange(0, 16))))))
  }
  val banks = (0 until 2).map { bank =>
    val manager = TLSlaveParameters.v1(
      address = Seq(AddressSet(bank * 64, 0xffbf)),
      regionType = RegionType.UNCACHED,
      supportsGet = TransferSizes(1, 64),
      supportsPutFull = TransferSizes(1, 64),
      supportsAcquireB = TransferSizes(64)
    )
    TLManagerNode(Seq(TLSlavePortParameters.v1(Seq(manager), beatBytes = 8, endSinkId = 1)))
  }
  val fabric = LazyModule(new CustomL1L2Fabric(
    specChannels = 2, specOwners = LoadPipelineWidth, specIdBits = log2Up(dcacheParameters.nMissEntries),
    specBlockBytes = 64, specMissPreview = preview
  ))
  clients.foreach(fabric.node := _)
  banks.foreach(_ := fabric.node)
  val spec = BundleBridgeSource(() => new SpecMissFabricIO)
  fabric.specSink.get := spec

  lazy val module = new HarnessImp

  class HarnessImp extends LazyModuleImp(PriorityFabricHarness.this) {
    val ordinary = IO(Vec(clients.size, Flipped(Decoupled(new Bundle {
      val address = UInt(16.W)
      val put = Bool()
    }))))
    val candidate = IO(Vec(LoadPipelineWidth, Flipped(Decoupled(new Bundle {
      val address = UInt(16.W)
      val channel = UInt(1.W)
    }))))
    val drain = IO(Input(Vec(banks.size, Bool())))
    val received = IO(Output(Vec(banks.size, Valid(UInt(16.W)))))

    for ((client, i) <- clients.zipWithIndex) {
      val (bus, _) = client.out.head
      bus.a.valid := ordinary(i).valid
      ordinary(i).ready := bus.a.ready
      bus.a.bits := 0.U.asTypeOf(bus.a.bits)
      bus.a.bits.address := ordinary(i).bits.address
      bus.a.bits.opcode := Mux(ordinary(i).bits.put, TLMessages.PutFullData, TLMessages.Get)
      bus.a.bits.size := Mux(ordinary(i).bits.put, 4.U, 3.U) // two 8-byte beats for a Put
      bus.a.bits.mask := Fill(8, 1.U)
      bus.b.ready := true.B
      bus.c.valid := false.B
      bus.c.bits := DontCare
      bus.d.ready := true.B
      bus.e.valid := false.B
      bus.e.bits := DontCare
    }
    for ((bank, i) <- banks.zipWithIndex) {
      val (bus, _) = bank.in.head
      bus.b.valid := false.B
      bus.b.bits := DontCare
      bus.c.ready := true.B
      bus.d.valid := false.B
      bus.d.bits := DontCare
      bus.e.ready := true.B
      bus.a.ready := (if (preview) true.B else drain(i))
      fabric.module.specPreview.foreach(_(i).a.ready := drain(i))
      received(i).valid := bus.a.fire
      received(i).bits := bus.a.bits.address
    }
    for ((req, w) <- spec.bundle.candidate.zipWithIndex) {
      req.valid := candidate(w).valid
      candidate(w).ready := req.ready
      req.bits := 0.U.asTypeOf(req.bits)
      req.bits.channel := candidate(w).bits.channel
      req.bits.paddr := candidate(w).bits.address
      req.bits.id := w.U
      val accepted = RegNext(req.fire, false.B)
      val resolve = spec.bundle.resolve(w)
      resolve.valid := (if (preview) RegNext(accepted, false.B) else accepted)
      resolve.bits.id := w.U
      resolve.bits.commit := true.B
    }
  }
}

class SpecMissFabricPriorityTest extends AnyFlatSpec with ChiselSim {
  implicit val p: Parameters = (new DefaultConfig).alterPartial {
    case XSCoreParamsKey => XSCoreParameters()
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.NORMAL, 0)
    case MonitorsEnabled => false // This A-only harness does not synthesize Grant responses.
  }

  for (preview <- Seq(false, true)) {
    it should s"give ordinary A first choice of both its input and its bank (preview=$preview)" in {
      simulate(LazyModule(new PriorityFabricHarness(preview)).module) { c =>
        val pending = Array.fill(2)(mutable.Queue.empty[Int])
        def normal(i: Int, addr: Int, put: Boolean = false): Unit = {
          c.ordinary(i).bits.address.poke(addr.U)
          c.ordinary(i).bits.put.poke(put.B)
          c.ordinary(i).valid.poke(true.B)
        }
        def spec(w: Int, ch: Int, addr: Int): Unit = {
          c.candidate(w).bits.channel.poke(ch.U)
          c.candidate(w).bits.address.poke(addr.U)
          c.candidate(w).valid.poke(true.B)
        }
        def idle(): Unit = {
          c.ordinary.foreach(_.valid.poke(false.B))
          c.candidate.foreach(_.valid.poke(false.B))
          c.drain.foreach(_.poke(true.B))
        }
        def tick(): Unit = {
          for ((out, b) <- c.received.zipWithIndex if out.valid.peek().litToBoolean) {
            assert(pending(b).nonEmpty, s"unexpected A on bank $b")
            out.bits.expect(pending(b).dequeue().U)
          }
          def record(addr: UInt): Unit = {
            val a = addr.peek().litValue.toInt
            pending((a >> 6) & 1).enqueue(a)
          }
          c.ordinary.foreach(x => if (x.valid.peek().litToBoolean && x.ready.peek().litToBoolean) record(x.bits.address))
          c.candidate.foreach(x => if (x.valid.peek().litToBoolean && x.ready.peek().litToBoolean) record(x.bits.address))
          c.clock.step()
        }
        def flush(): Unit = {
          idle()
          for (_ <- 0 until 6) tick()
          assert(pending.forall(_.isEmpty))
        }
        idle()
        // Different banks still contend for a single DCache input.
        normal(0, 0x100); spec(0, 0, 0x140)
        for (_ <- 0 until 8) {
          c.ordinary(0).ready.expect(true.B)
          c.candidate(0).ready.expect(false.B)
          tick()
        }
        flush()

        // Both other clients target the same bank as lane 0. Fill the bank, keep
        // the three requests asserted across a stall, then watch ordinary RR resume.
        normal(1, 0x200); normal(2, 0x280); spec(0, 0, 0x300)
        c.drain(0).poke(false.B)
        for (_ <- 0 until 5) { c.candidate(0).ready.expect(false.B); tick() }
        c.ordinary(1).ready.expect(false.B)
        c.ordinary(2).ready.expect(false.B)
        c.drain(0).poke(true.B)
        val served = Array(0, 0)
        for (_ <- 0 until 12) {
          c.candidate(0).ready.expect(false.B)
          for (i <- 0 until 2 if c.ordinary(i + 1).ready.peek().litToBoolean) served(i) += 1
          tick()
        }
        assert(served.min >= 5 && (served(0) - served(1)).abs <= 1)
        flush()

        // Separate inputs and banks must both handshake in this very cycle.
        normal(2, 0x400); spec(1, 1, 0x440)
        c.ordinary(2).ready.expect(true.B); c.candidate(1).ready.expect(true.B)
        tick(); flush()

        // A bubble in an unfinished Put may not let a SpecMiss steal its input or bank.
        normal(0, 0x500, put = true); c.ordinary(0).ready.expect(true.B); tick()
        c.ordinary(0).valid.poke(false.B)
        spec(0, 0, 0x540); spec(1, 1, 0x580)
        c.candidate(0).ready.expect(false.B); c.candidate(1).ready.expect(false.B); tick()
        normal(0, 0x500, put = true)
        c.ordinary(0).ready.expect(true.B)
        c.candidate(0).ready.expect(false.B); c.candidate(1).ready.expect(false.B); tick()
        c.ordinary(0).valid.poke(false.B)
        c.candidate(0).ready.expect(true.B); c.candidate(1).ready.expect(true.B)
        tick(); flush()

        // Idle ordinary inputs leave the whole bandwidth to the speculative peers.
        spec(0, 0, 0x600); spec(1, 0, 0x680)
        val grants = for (_ <- 0 until 10) yield {
          val winners = (0 to 1).filter(i => c.candidate(i).ready.peek().litToBoolean)
          assert(winners.size == 1)
          tick()
          winners.head
        }
        assert(grants.count(_ == 0) == 5)
        flush()
      }
    }
  }
}

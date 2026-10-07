package cache

import chisel3._
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.mem.prefetch._

class PBDepthRefillTop(implicit p: Parameters) extends PBTestTop(enableMoveToDCache = false) {
  val completedRefills = IO(Output(UInt(9.W)))
  val aligner = Module(new PDBDepthEventAligner)
  aligner.io.raw := 0.U.asTypeOf(aligner.io.raw)
  aligner.io.raw.refill := io.dcache.perf.streamRefill
  val controller = Module(new PDBDepthController(PDBDepthParameters()))
  controller.io.enabled := true.B; controller.io.fixedDepth := 64.U
  controller.io.events := aligner.io.completed
  completedRefills := controller.io.refills
}

class PDBDepthRefillTest extends AnyFlatSpec with PBTestDriver {
  private val base = new DefaultConfig
  implicit private val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }
  behavior of "Real PB refill window source"
  it should "count only accepted clean Stream data after the observer alignment delay" in {
    simulate(new PBDepthRefillTop) { c =>
      init(c)
      val first = reserve(c)
      c.clock.step(4); c.completedRefills.expect(0.U)
      c.io.mshr.refillReq.valid.poke(true.B)
      c.io.mshr.refillReq.bits.entryId.poke(first.U)
      c.io.mshr.refillReq.bits.missEntryId.poke(1.U)
      c.io.mshr.refillReq.ready.expect(false.B)
      c.io.dcache.perf.streamRefill.expect(false.B)
      c.clock.step(4); c.completedRefills.expect(0.U)
      c.io.mshr.refillReq.bits.missEntryId.poke(0.U)
      c.io.dcache.perf.streamRefill.expect(true.B)
      c.clock.step(); c.io.mshr.refillReq.valid.poke(false.B)
      c.completedRefills.expect(0.U); c.clock.step(); c.completedRefills.expect(0.U)
      c.clock.step(); c.completedRefills.expect(1.U)
      c.clock.step(3); c.completedRefills.expect(1.U)

      c.io.mshr.allocReq.bits.prefetchSource.poke(1.U)
      val other = reserve(c, address + 64); refill(c, other)
      c.clock.step(4); c.completedRefills.expect(1.U)
      c.io.mshr.allocReq.bits.prefetchSource.poke(3.U)
      val corrupt = reserve(c, address + 128)
      c.io.mshr.refillReq.bits.corrupt.poke(true.B); refill(c, corrupt)
      c.io.mshr.refillReq.bits.corrupt.poke(false.B)
      c.clock.step(4); c.completedRefills.expect(1.U)
      val denied = reserve(c, address + 192)
      c.io.mshr.refillReq.bits.denied.poke(true.B); refill(c, denied)
      c.io.mshr.refillReq.bits.denied.poke(false.B)
      c.clock.step(4); c.completedRefills.expect(1.U)
      val clean = reserve(c, address + 256); refill(c, clean)
      c.clock.step(4); c.completedRefills.expect(2.U)
    }
  }
}

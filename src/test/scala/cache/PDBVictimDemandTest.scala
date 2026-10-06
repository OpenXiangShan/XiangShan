package cache

import chisel3._
import chisel3.reflect.DataMirror
import chisel3.simulator.scalatest.ChiselSim
import chisel3.util._
import freechips.rocketchip.tilelink.ClientStates
import org.chipsalliance.cde.config.Parameters
import org.scalatest.flatspec.AnyFlatSpec
import top.DefaultConfig
import utility._
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.cache._

class PBVictimDemandTestTop(implicit p: Parameters) extends DCacheModule {
  val io = IO(new Bundle {
    val valid = Input(Bool())
    val source = Input(UInt(sourceTypeWidth.W))
    val paddr = Input(UInt(PAddrBits.W))
    val killS1, killS2, cacheHit, nack, bankConflict = Input(Bool())
    val demand = Output(Valid(UInt((PAddrBits - blockOffBits).W)))
    val ready = Output(Bool())
  })
  val pipe = Module(new LoadPipe(0))
  // Tie all unused peer inputs; exercise the actual S0->S2 production pipeline.
  def clearInputs(data: Data): Unit = data match {
    case record: Record => record.elements.values.foreach(clearInputs)
    case vec: Vec[_] => vec.foreach(clearInputs)
    case leaf if DataMirror.directionOf(leaf) == ActualDirection.Input => leaf := 0.U.asTypeOf(leaf)
    case _ =>
  }
  clearInputs(pipe.io)
  pipe.io.meta_read.ready := true.B
  pipe.io.tag_read.ready := true.B
  pipe.io.banked_data_read.ready := true.B
  pipe.io.lsu.resp.ready := true.B
  pipe.io.miss_req.ready := true.B
  pipe.io.lsu.req.valid := io.valid
  pipe.io.lsu.req.bits.cmd := MemoryOpConstants.M_XRD
  pipe.io.lsu.req.bits.instrtype := io.source
  pipe.io.lsu.req.bits.vaddr := io.paddr
  pipe.io.lsu.req.bits.vaddr_dup := io.paddr
  pipe.io.lsu.req.bits.mask := 1.U
  pipe.io.lsu.s1_paddr_dup_lsu := io.paddr
  pipe.io.lsu.s1_paddr_dup_dcache := io.paddr
  pipe.io.lsu.s1_kill := io.killS1
  pipe.io.lsu.s2_kill := io.killS2
  pipe.io.nack := io.nack
  pipe.io.bank_conflict_slow := io.bankConflict
  pipe.io.meta_resp(0).coh.state := Mux(io.cacheHit, ClientStates.Branch, ClientStates.Nothing)
  pipe.io.tag_resp(0) := get_tag(io.paddr)
  io.demand := pipe.io.victimDemand
  io.ready := pipe.io.lsu.req.ready
}

class PDBVictimDemandTest extends AnyFlatSpec with ChiselSim {
  private val base = new DefaultConfig
  implicit private val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.VERBOSE, 0)
    case LogUtilsOptionsKey => LogUtilsOptions(false, false, false, false)
  }
  behavior of "LoadPipe physical victim-query tap"
  it should "observe demands independent of cache outcome and exclude prefetches and killed requests" in {
    simulate(new PBVictimDemandTestTop) { c =>
      c.io.valid.poke(false.B); c.io.source.poke(0.U); c.io.paddr.poke(0.U)
      c.io.killS1.poke(false.B); c.io.killS2.poke(false.B)
      c.io.cacheHit.poke(false.B); c.io.nack.poke(false.B); c.io.bankConflict.poke(false.B)
      c.reset.poke(true.B); c.clock.step(3); c.reset.poke(false.B)
      val address = BigInt("81234038", 16)
      def request(source: Int = 0, hit: Boolean = false, killS1: Boolean = false,
                  killS2: Boolean = false, nack: Boolean = false, conflict: Boolean = false): Unit = {
        c.io.source.poke(source.U); c.io.paddr.poke(address.U)
        c.io.cacheHit.poke(hit.B); c.io.nack.poke(nack.B)
        c.io.valid.poke(true.B); c.io.ready.expect(true.B); c.clock.step()
        c.io.valid.poke(false.B); c.io.killS1.poke(killS1.B); c.clock.step()
        c.io.killS1.poke(false.B); c.io.killS2.poke(killS2.B); c.io.bankConflict.poke(conflict.B)
        val expected = source == 0 && !killS1 && !killS2
        c.io.demand.valid.expect(expected.B)
        if (expected) c.io.demand.bits.expect((address >> 6).U)
        c.clock.step(); c.io.killS2.poke(false.B); c.io.bankConflict.poke(false.B)
        c.io.nack.poke(false.B); c.clock.step(2)
      }
      request(); request(hit = true); request(nack = true); request(conflict = true)
      request(source = 3); request(source = 4)
      request(killS1 = true); request(killS2 = true)
    }
  }
}

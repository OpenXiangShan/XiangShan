package cache

import chisel3._
import chisel3.util._
import chisel3.simulator.ChiselSim
import org.chipsalliance.cde.config.Parameters
import top.DefaultConfig
import utility._
import xiangshan.{XSCoreParamsKey, XSTileKey}
import xiangshan.mem.prefetch.{PfGenReq, PrefetchFilter}

class SMSOffloadSourceHarness(implicit p: Parameters) extends Module {
  val filter = Module(new PrefetchFilter)
  val io = IO(new Bundle {
    val gen = Flipped(Valid(new PfGenReq))
    val out = Decoupled(new Bundle {
      val addr = UInt(filter.io.l2_pf_addr.bits.getWidth.W)
      val source = UInt(filter.io.source_type.getWidth.W)
    })
  })
  filter.io.gen_req <> io.gen
  filter.io.tlb_req.req.ready := true.B
  filter.io.tlb_req.resp.valid := false.B
  filter.io.tlb_req.resp.bits := 0.U.asTypeOf(filter.io.tlb_req.resp.bits)
  filter.io.pmp_resp := 0.U.asTypeOf(filter.io.pmp_resp)
  io.out.valid := filter.io.l2_pf_addr.valid
  filter.io.l2_pf_addr.ready := io.out.ready
  io.out.bits.addr := filter.io.l2_pf_addr.bits
  io.out.bits.source := filter.io.source_type
}

object SMSOffloadSourceTest extends App with ChiselSim {
  val base = new DefaultConfig
  implicit val p: Parameters = base.alterPartial {
    case XSCoreParamsKey => base(XSTileKey).head
    case PerfCounterOptionsKey => PerfCounterOptions(false, false, XSPerfLevel.NORMAL, 0)
  }
  ChiselDB.init(false)
  Constantin.init(false)
  simulate(new SMSOffloadSourceHarness) { dut =>
    dut.io.gen.valid.poke(false.B)
    dut.io.gen.bits.region_tag.poke(1.U)
    dut.io.gen.bits.region_addr.poke((BigInt("80000000", 16) >> 10).U)
    dut.io.gen.bits.region_bits.poke(0.U)
    dut.io.gen.bits.paddr_valid.poke(true.B)
    dut.io.gen.bits.decr_mode.poke(false.B)
    dut.io.gen.bits.alias_bits.poke(0.U)
    dut.io.gen.bits.debug_source_type.poke(0.U)
    dut.io.out.ready.poke(false.B)
    dut.reset.poke(true.B)
    dut.clock.step(3)
    dut.reset.poke(false.B)

    def insert(mask: Int, source: Int): Unit = {
      dut.io.gen.valid.poke(true.B)
      dut.io.gen.bits.region_bits.poke(mask.U)
      dut.io.gen.bits.debug_source_type.poke(source.U)
      dut.clock.step()
      dut.io.gen.valid.poke(false.B)
      dut.clock.step(3)
    }
    insert(0x5, 6) // PHT owns blocks 0 and 2
    insert(0x6, 5) // AGT adds block 1; must not relabel block 2
    insert(0x8, 10) // stride adds block 3
    dut.clock.step(5)
    for ((source, block) <- Seq(6, 5, 6, 10).zipWithIndex) {
      dut.io.out.valid.expect(true.B)
      dut.io.out.bits.addr.expect((BigInt("80000000", 16) + block * 64).U)
      dut.io.out.bits.source.expect(source.U)
      dut.io.out.ready.poke(true.B)
      dut.clock.step()
    }
    dut.io.out.valid.expect(false.B)
    // Repeated predictions for already-issued blocks must not reissue them.
    dut.io.out.ready.poke(false.B)
    insert(0xf, 8)
    dut.io.out.valid.expect(false.B)
  }
  println("SMSOffloadSourceTest: mixed-region source retention, duplicate ownership and backpressure passed")
}

package system

import noop._
import bus.axi4.{AXI4, AXI4Lite}
import bus.simplebus._

import chisel3._
import chisel3.util._
import chisel3.util.experimental.BoringUtils

class NOOPSoC(implicit val p: NOOPConfig) extends NOOPModule {
  val io = IO(new Bundle{
    val mem = new AXI4
    val mmio = (if (p.FPGAPlatform) { new AXI4Lite } else { new SimpleBusUC })
    val mtip = Input(Bool())
    val meip = Input(Bool())
  })

  val noop = Module(new NOOP)

  val cohMg = Module(new CoherenceManager)
  val xbar = Module(new SimpleBusCrossbarNto1(2))
  cohMg.io.in <> noop.io.imem.mem
  noop.io.dmem.coh <> cohMg.io.out.coh
  xbar.io.in(0) <> cohMg.io.out.mem
  xbar.io.in(1) <> noop.io.dmem.mem
  
  if (HasL2cache) {
    val l2cacheOut = Wire(new SimpleBusUC)
    l2cacheOut <> Cache(in = xbar.io.out, mmio = 0.U.asTypeOf(new SimpleBusUC), flush = "b00".U, enable = true)(CacheConfig(ro = false, name = "l2cache", cacheLevel = 2))
    io.mem <> l2cacheOut.toAXI4()
  } else {
    io.mem <> xbar.io.out.toAXI4()
  }

  noop.io.imem.coh.resp.ready := true.B
  noop.io.imem.coh.req.valid := false.B
  noop.io.imem.coh.req.bits := DontCare

  if (p.FPGAPlatform) io.mmio <> noop.io.mmio.toAXI4Lite()
  else io.mmio <> noop.io.mmio

  val mtipSync = RegNext(RegNext(io.mtip))
  val meipSync = RegNext(RegNext(io.meip))
  BoringUtils.addSource(mtipSync, "mtip")
  BoringUtils.addSource(meipSync, "meip")
}

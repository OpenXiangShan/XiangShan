package system

import noop.{NOOP, NOOPConfig}
import bus.axi4.{AXI4, AXI4Lite}
import bus.simplebus.SimpleBus

import chisel3._

class NOOPSoC(implicit val p: NOOPConfig) extends Module {
  val io = IO(new Bundle{
    val imem = new AXI4
    val dmem = new AXI4
    val mmio = (if (p.FPGAPlatform) { new AXI4Lite } else { new SimpleBus })
  })

  val noop = Module(new NOOP)
  io.imem <> noop.io.imem.toAXI4()
  io.dmem <> noop.io.dmem.toAXI4()

  if (p.FPGAPlatform) io.mmio <> noop.io.mmio.toAXI4(new AXI4Lite)
  else io.mmio <> noop.io.mmio
}

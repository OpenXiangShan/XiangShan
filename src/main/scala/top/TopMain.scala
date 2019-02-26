package top

import noop.NOOP
import bus.axi4.{AXI4, AXI4Lite}
import device.AXI4Timer

import chisel3._
import chisel3.experimental.dontTouch

class NOOPFPGA extends Module {
  val io = IO(new Bundle{
    val imem = new AXI4
    val dmem = new AXI4
    val mmio = new AXI4Lite
  })

  val noop = Module(new NOOP)
  io.imem <> noop.io.imem.toAXI4()
  io.dmem <> noop.io.dmem.toAXI4()
  io.mmio <> noop.io.mmio.toAXI4Lite()
}

class Top extends Module {
  val io = IO(new Bundle{})
  val noop = Module(new NOOPFPGA)
  val timer = Module(new AXI4Timer)

  noop.io := DontCare
  timer.io := DontCare
  dontTouch(noop.io)
  dontTouch(timer.io)
}

object TopMain extends App {
  Driver.execute(args, () => new Top)
}

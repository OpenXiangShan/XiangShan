package top

import noop.{NOOP, NOOPConfig}
import bus.axi4.{AXI4, AXI4Lite}
import device.{AXI4Timer, AXI4VGA}
import gpu._

import chisel3._
import chisel3.experimental.dontTouch

class NOOPFPGA extends Module {
  val io = IO(new Bundle{
    val imem = new AXI4
    val dmem = new AXI4
    val mmio = new AXI4Lite
//    val uncacheMem = new AXI4
  })

  val noop = Module(new NOOP()(NOOPConfig()))
  io.imem <> noop.io.imem
  io.dmem <> noop.io.dmem
  io.mmio <> noop.io.mmio.toAXI4(isLite = true)
//  io.uncacheMem <> noop.io.uncacheMem
}

class Top extends Module {
  val io = IO(new Bundle{})
  val noop = Module(new NOOPFPGA)
  val timer = Module(new AXI4Timer)
  val vga = Module(new AXI4VGA)
//  val gpu = Module(new AXI4GPU)

  noop.io := DontCare
  timer.io := DontCare
  vga.io := DontCare
//  gpu.io := DontCare
  dontTouch(noop.io)
  dontTouch(timer.io)
  dontTouch(vga.io)
//  dontTouch(gpu.io)
}

object TopMain extends App {
  Driver.execute(args, () => new Top)
}

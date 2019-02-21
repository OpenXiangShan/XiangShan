package top

import noop.NOOP
import memory.AXI4
import device.AXI4Timer

import chisel3._
import chisel3.experimental.dontTouch

class NOOPFPGA extends Module {
  val io = IO(new Bundle{
    val imem = new AXI4
    val dmem = new AXI4
  })

  val noop = Module(new NOOP)
  io.imem <> noop.io.imem.toAXI4()
  io.dmem <> noop.io.dmem.toAXI4()

  noop.io.gmem := DontCare
  noop.io.gpuStart := DontCare
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

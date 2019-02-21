package top

import noop.NOOP
import memory.AXI4

import chisel3._

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

object TopMain extends App {
  Driver.execute(args, () => new NOOPFPGA)
}

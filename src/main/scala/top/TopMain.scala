package top

import noop.NOOP
import memory.AHBLiteIO

import chisel3._

class NOOPFPGA extends Module {
  val io = IO(new Bundle{
    val imem = new AHBLiteIO
    val dmem = new AHBLiteIO
  })

  val noop = Module(new NOOP)
  io.imem <> noop.io.imem.toAHBLite()
  io.dmem <> noop.io.dmem.toAHBLite()

  noop.io.gmem := DontCare
  noop.io.gpuStart := DontCare
}

object TopMain extends App {
  Driver.execute(args, () => new NOOPFPGA)
}

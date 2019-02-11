package top

import noop._

import chisel3._

class ALUModule extends Module {
  val io = IO(new Bundle {
    val src1 = Input(UInt(32.W))
    val src2 = Input(UInt(32.W))
    val func = Input(UInt(4.W))
    val out = Output(UInt(32.W))
  })

  io.out := (new ALU).access(src1 = io.src1, src2 = io.src2, func = io.func)
}

class NOOPFPGA extends Module {
  val io = IO(new Bundle{
    val trap = Output(UInt(2.W))
  })

  val noop = Module(new NOOP)
  val mem = Module(new DistributedMem)
  noop.io.imem <> mem.io.imem
  noop.io.dmem <> mem.io.dmem
  io.trap := noop.io.trap

  noop.io.gmem := DontCare
  noop.io.gpuStart := DontCare
}

object TopMain extends App {
  Driver.execute(args, () => new NOOPFPGA)
}

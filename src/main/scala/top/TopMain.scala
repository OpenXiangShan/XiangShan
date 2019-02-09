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

object TopMain extends App {
  Driver.execute(args, () => new NOOP)
}

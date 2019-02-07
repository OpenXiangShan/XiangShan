package core

import chisel3._
import chisel3.util._

class ABundle extends Bundle {
  val addr = Output(UInt(32.W))
  val wdata = Output(UInt(32.W))
  val wen = Output(Bool())
}

class RBundle extends Bundle {
  val rdata = Output(UInt(32.W))
}

class MemIO extends Bundle {
  val out = Valid(new ABundle)
  val in = Flipped(new RBundle)
}

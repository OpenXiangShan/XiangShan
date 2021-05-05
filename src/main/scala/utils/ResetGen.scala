package utils

import chisel3._
import chisel3.util._

class ResetGen(level: Int = 1, sim: Boolean) extends Module {
  val io = IO(new Bundle() {
    val out = Output(Bool())
  })
  var reset_out = WireInit(reset.asBool)

  if (!sim) {
    for (i <- 0 until level) {
      reset_out = RegNext(reset_out)
      reset_out.suggestName(s"reset_${i}")
    }
  }

  io.out := reset_out
}

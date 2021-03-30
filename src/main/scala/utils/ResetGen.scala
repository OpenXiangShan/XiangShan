package utils

import chisel3._
import chisel3.util._
import top.Parameters

class ResetGen(level: Int = 1) extends Module {
  val io = IO(new Bundle() {
    val out = Output(Bool())
  })
  var reset_out = WireInit(reset.asBool)

  val env = Parameters.get.envParameters
  if (env.FPGAPlatform) {
    for (i <- 0 until level) {
      reset_out = RegNext(reset_out)
      reset_out.suggestName(s"reset_${i}")
    }
  }

  io.out := reset_out
}

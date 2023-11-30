package utils

import chisel3._

class ClockGate extends Module {
  val io = IO(new Bundle {
    val TE = Input(Bool())
    val E  = Input(Bool())
    val CK = Input(Clock())
    val Q  = Output(Clock())
  })

  io.Q := (io.CK.asBool & (io.TE | io.E)).asClock
}
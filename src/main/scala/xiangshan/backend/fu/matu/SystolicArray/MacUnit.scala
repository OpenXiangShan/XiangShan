package matu.SystolicArray

import chisel3._
import chisel3.util.{Cat, Fill}
import matu.SystolicArray.Multiplier._

class MacUnit(val IN_WIDTH: Int, val C_WIDTH: Int) extends Module {
  val io = IO(new Bundle {
    val in_a = Input(SInt(IN_WIDTH.W))
    val in_b = Input(SInt(IN_WIDTH.W))
    val in_c = Input(SInt(C_WIDTH.W))
    val out_c = Output(SInt(C_WIDTH.W))
  })

  val mul = Module(new Multiplier(IN_WIDTH, 2*IN_WIDTH))
  val rca = Module(new RCA(C_WIDTH))

  mul.input.multiplicand := io.in_a
  mul.input.multiplier   := io.in_b

  rca.input.a_in  := Cat(Fill((C_WIDTH-2*IN_WIDTH), mul.output.product(2*IN_WIDTH-1)), mul.output.product).asSInt
  rca.input.b_in  := io.in_c
  rca.input.c_in  := 0.S(C_WIDTH.W)
  io.out_c        := rca.output.S

}
package xiangshan.frontend

import chisel3._
import chisel3.util._

class AsyncRegEnable[T <: Data](gen: T) extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clock  = Input(Clock())
    val reset  = Input(Reset())
    val next   = Input(gen)
    val init   = Input(gen)
    val enable = Input(Bool())
    val out    = Output(gen)
  })

  setInline(
    "AsyncRegEnable.v",
    s"""module AsyncRegEnable(
      | input                                  clock,
      | input                                  reset,
      | input      [${io.next.getWidth - 1}:0] next,
      | input      [${io.init.getWidth - 1}:0] init,
      | input                                  enable,
      | output reg [${io.out.getWidth - 1}:0]  out
      |);
      |always @(posedge clock or posedge reset) begin
      | if (reset) begin
      |   out <= init;
      | end else if (enable) begin
      |   out <= next;
      | end
      |end
      |endmodule""".stripMargin
  )
}

object AsyncRegEnable {
  def apply[T <: Data](next: T, init: T, enable: Bool): T = {
    val reg = Module(new AsyncRegEnable(chiselTypeOf(next)))
    reg.io.clock  := Module.clock
    reg.io.reset  := Module.reset
    reg.io.next   := next
    reg.io.init   := init
    reg.io.enable := enable
    reg.io.out
  }
}

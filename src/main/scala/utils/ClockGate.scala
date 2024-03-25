package utils

import chisel3._
import chisel3.util.HasBlackBoxInline

class ClockGate extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val TE = Input(Bool())
    val E  = Input(Bool())
    val CK = Input(Clock())
    val Q  = Output(Clock())
  })

  val verilog =
    """
      |/* verilator lint_off UNOPTFLAT */
      |module ClockGate (
      | input TE,
      | input E,
      | input CK,
      | output Q
      |);
      | reg en_latched /*verilator clock_enable*/;
      |
      | always_latch begin
      |   if (!CK) begin
      |     en_latched = E || TE;
      |   end
      | end
      |
      | assign Q = en_latched && CK;
      |
      |endmodule
      |
      |""".stripMargin
  setInline("ClockGate.v", verilog)
}

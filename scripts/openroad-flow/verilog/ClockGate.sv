// Utility's ClockGate model (src/main/scala/utility/ClockGate.scala),
// verbatim: the body of the external module firtool emits for every clock
// gate it does not fold (CLOCK_GATE_OPTS in BUILD.bazel).
module ClockGate (
  input  wire TE,
  input  wire E,
  input  wire CK,
  output wire Q
);
  reg EN;
  always_latch begin
    if(!CK) EN = TE | E;
  end
  assign Q = CK & EN;
endmodule

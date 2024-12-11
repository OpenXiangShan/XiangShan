package utils

import chisel3._
import chisel3.util.HasBlackBoxInline

class LogPerfIO extends Bundle {
  val timer = UInt(64.W)
  val logEnable = Bool()
  val clean = Bool()
  val dump = Bool()
}

class LogPerfHelper extends BlackBox with HasBlackBoxInline {
  val io = IO(Output(new LogPerfIO))

  val verilog =
    """`ifndef SIM_TOP_MODULE_NAME
      |  `define SIM_TOP_MODULE_NAME SimTop
      |`endif
      |
      |/*verilator tracing_off*/
      |
      |module LogPerfHelper (
      |  output [63:0] timer,
      |  output        logEnable,
      |  output        clean,
      |  output        dump
      |);
      |
      |assign timer         = `SIM_TOP_MODULE_NAME.timer;
      |assign logEnable     = `SIM_TOP_MODULE_NAME.logEnable;
      |assign clean         = `SIM_TOP_MODULE_NAME.clean;
      |assign dump          = `SIM_TOP_MODULE_NAME.dump;
      |
      |endmodule
      |
      |""".stripMargin
  setInline("LogPerfHelper.v", verilog)
}

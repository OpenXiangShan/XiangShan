package device

import org.chipsalliance.cde.config.Parameters
import chisel3._
import chisel3.experimental.ExtModule
import chisel3.util._
import freechips.rocketchip.diplomacy.AddressSet
import utils._

class UParamHelper extends ExtModule with HasExtModuleInline {
  val clk = IO(Input(Clock()))
  val ren = IO(Input(Bool()))
  val data = IO(Output(UInt(64.W)))
  val addr = IO(Input(UInt(64.W)))

  setInline("UParamHelper.sv",
    s"""
       |import "DPI-C" function void uparam_read(input longint addr, output longint data);
       |
       |module UParamHelper (
       |  input clk,
       |  input ren,
       |  input [63:0] addr,
       |  output reg [63:0] data
       |);
       |
       |  always @(posedge clk) begin
       |    if (ren) uparam_read(addr, data);
       |  end
       |
       |endmodule
       """.stripMargin)
}

class AXI4UParam (address: Seq[AddressSet])(implicit  p:Parameters) extends AXI4SlaveModule(address, executable = false) {
  override lazy val module = new AXI4SlaveModuleImp[Null](this) {
    val UParam = Module(new UParamHelper)
    UParam.clk := clock
    UParam.ren := in.ar.fire
    UParam.addr := Cat(0.U, raddr(12, 0))

    in.r.bits.data := UParam.data
  }
}
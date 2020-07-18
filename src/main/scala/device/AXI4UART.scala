package device

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

class UARTGetc extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val getc = Input(Bool())
    val ch = Output(UInt(8.W))
  })

  setInline("UARTGetc.v",
    s"""
      |import "DPI-C" function void uart_getc(output byte ch);
      |
      |module UARTGetc (
      |  input clk,
      |  input getc,
      |  output reg [7:0] ch
      |);
      |
      |  always@(posedge clk) begin
      |    if (getc) uart_getc(ch);
      |  end
      |
      |endmodule
     """.stripMargin)
}

class UARTPutc extends BlackBox with HasBlackBoxInline {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val putc = Input(Bool())
    val ch = Input(UInt(8.W))
  })

  setInline("UARTPutc.v",
    """
       |module UARTPutc (
       |  input clk,
       |  input putc,
       |  input [7:0] ch
       |);
       |
       |  always@(posedge clk) begin
       |    if (putc) begin
       |      $fwrite(32'h80000001, "%c", ch);
       |    end
       |  end
       |
       |endmodule
     """.stripMargin)
}

class AXI4UART extends AXI4SlaveModule(new AXI4Lite) {
  val rxfifo = RegInit(0.U(32.W))
  val txfifo = Reg(UInt(32.W))
  val stat = RegInit(1.U(32.W))
  val ctrl = RegInit(0.U(32.W))

  val getcHelper = Module(new UARTGetc)
  getcHelper.io.clk := clock
  getcHelper.io.getc := (raddr(3,0) === 0.U && ren)

  val putcHelper = Module(new UARTPutc)
  putcHelper.io.clk := clock
  putcHelper.io.putc := waddr(3, 0)===4.U && in.w.fire()
  putcHelper.io.ch := in.w.bits.data(7, 0)

//  def putc(c: UInt): UInt = { printf("%c", c(7,0)); c }
  def getc = getcHelper.io.ch

  val mapping = Map(
    RegMap(0x0, getc, RegMap.Unwritable),
    RegMap(0x4, txfifo),
    RegMap(0x8, stat),
    RegMap(0xc, ctrl)
  )

  RegMap.generate(mapping, raddr(3,0), in.r.bits.data,
    waddr(3,0), in.w.fire(), in.w.bits.data, MaskExpand(in.w.bits.strb >> waddr(2,0)))
}

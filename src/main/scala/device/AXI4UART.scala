package device

import chisel3._
import chisel3.util._

import bus.axi4._
import utils._

class AXI4UART extends AXI4SlaveModule(new AXI4Lite) {
  val rxfifo = RegInit(0.U(32.W))
  val txfifo = Reg(UInt(32.W))
  val stat = RegInit(1.U(32.W))
  val ctrl = RegInit(0.U(32.W))

  def putc(c: UInt): UInt = { printf("%c", c(7,0)); c }

  val mapping = Map(
    RegMap(0x0, rxfifo),
    RegMap(0x4, txfifo, putc),
    RegMap(0x8, stat),
    RegMap(0xc, ctrl)
  )

  RegMap.generate(mapping, raddr(3,0), in.r.bits.data,
    waddr(3,0), in.w.fire(), in.w.bits.data, MaskExpand(in.w.bits.strb))
}

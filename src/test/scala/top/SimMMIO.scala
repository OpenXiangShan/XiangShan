package top

import bus.axi4.AXI4ToAXI4Lite
import chisel3._
import chisel3.util._
import bus.simplebus._
import bus.tilelink.{NaiveTL1toN, MMIOTLToAXI4, TLCached, TLParameters}
import device._

class SimMMIO(para: TLParameters) extends Module {
  val io = IO(new Bundle {
    val rw = Flipped(TLCached(para))
    val uart = new UARTIO
  })

  val devAddrSpace = List(
    (0x40600000L, 0x10L), // uart
    (0x50000000L, 0x400000L), // vmem
    (0x40001000L, 0x8L),  // vga ctrl
    (0x40000000L, 0x1000L),  // flash
    (0x40002000L, 0x1000L)  // dummy sdcard
  )

  val xbar = Module(new NaiveTL1toN(devAddrSpace, io.rw.params))
  xbar.io.in <> io.rw

  val axiOut = xbar.io.out.map(tl => AXI4ToAXI4Lite(MMIOTLToAXI4(tl)))

  val uart = Module(new AXI4UART)
  val vga = Module(new AXI4VGA(sim = true))
  val flash = Module(new AXI4Flash)
  val sd = Module(new AXI4DummySD)

  uart.io.in <> axiOut(0)
  vga.io.in.fb <> axiOut(1)
  vga.io.in.ctrl <> axiOut(2)
  flash.io.in <> axiOut(3)
  sd.io.in <> axiOut(4)
  vga.io.vga := DontCare
  io.uart <> uart.io.extra.get
}

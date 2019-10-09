package top

import chisel3._
import chisel3.util._

import bus.simplebus._
import device._

class SimMMIO extends Module {
  val io = IO(new Bundle {
    val rw = Flipped(new SimpleBusUC)
    val mtip = Output(Bool())
  })

  val devAddrSpace = List(
    (0x40600000L, 0x10L), // uart
    (0x40700000L, 0x10L), // timer
    (0x40000000L, 0x400000L), // vmem
    (0x40800000L, 0x8L)  // vga ctrl
  )

  val xbar = Module(new SimpleBusCrossbar1toN(devAddrSpace))
  xbar.io.in <> io.rw

  val uart = Module(new AXI4UART)
  val timer = Module(new AXI4Timer(sim = true))
  val vga = Module(new AXI4VGA(sim = true))
  uart.io.in <> xbar.io.out(0).toAXI4Lite()
  timer.io.in <> xbar.io.out(1).toAXI4Lite()
  vga.io.in.fb <> xbar.io.out(2).toAXI4Lite()
  vga.io.in.ctrl <> xbar.io.out(3).toAXI4Lite()
  vga.io.vga := DontCare

  io.mtip := timer.io.extra.get.mtip
}

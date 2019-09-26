package top

import chisel3._
import chisel3.util._

import bus.simplebus._
import device._

class DeviceHelper extends BlackBox {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val reset = Input(Bool())
    val reqValid = Input(Bool())
    val reqWen = Input(Bool())
    val reqAddr = Input(UInt(64.W))
    val reqWdata = Input(UInt(64.W))
    val reqWmask = Input(UInt(8.W))
    val respRdata = Output(UInt(64.W))
  })
}

class SimMMIO extends Module {
  val io = IO(new Bundle {
    val rw = Flipped(new SimpleBusUC)
  })

  val devAddrSpace = List(
    (0x40600000L, 0x10L), // uart
    (0x40700000L, 0x10L)  // timer
  )

  val xbar = Module(new SimpleBusCrossbar(1, devAddrSpace))
  xbar.io.in(0) <> io.rw

  val uart = Module(new AXI4UART)
  val timer = Module(new AXI4Timer)
  uart.io.in <> xbar.io.out(0).toAXI4Lite()
  timer.io.in <> xbar.io.out(1).toAXI4Lite()

  //val helper = Module(new DeviceHelper)
  //val helperIO = xbar.io.out(0)
  //helper.io.clk := clock
  //helper.io.reset := reset.asBool
  //helper.io.reqValid := helperIO.req.valid
  //helper.io.reqWen := helperIO.isWrite()
  //helper.io.reqAddr := helperIO.req.bits.addr
  //helper.io.reqWdata := helperIO.req.bits.wdata
  //helper.io.reqWmask := helperIO.req.bits.wmask
  //helperIO.resp.bits.rdata := helper.io.respRdata
  //helperIO.resp.bits.cmd := 0.U
  //helperIO.resp.bits.user := 0.U

  //helperIO.req.ready := true.B
  //helperIO.resp.valid := RegNext(helperIO.req.valid)
}

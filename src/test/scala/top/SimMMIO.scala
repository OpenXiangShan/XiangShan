package top

import chisel3._
import chisel3.util._

import bus.simplebus._

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

  val helper = Module(new DeviceHelper)
  helper.io.clk := clock
  helper.io.reset := reset.asBool
  helper.io.reqValid := io.rw.req.valid
  helper.io.reqWen := io.rw.isWrite()
  helper.io.reqAddr := io.rw.req.bits.addr
  helper.io.reqWdata := io.rw.req.bits.wdata
  helper.io.reqWmask := io.rw.req.bits.wmask
  io.rw.resp.bits.rdata := helper.io.respRdata
  io.rw.resp.bits.cmd := 0.U
  io.rw.resp.bits.user := 0.U

  io.rw.req.ready := true.B
  io.rw.resp.valid := RegNext(io.rw.req.valid)
}

package top

import chisel3._
import chisel3.util._

import bus.simplebus.SimpleBus

class DeviceHelper extends BlackBox {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val reqValid = Input(Bool())
    val reqWen = Input(Bool())
    val reqAddr = Input(UInt(32.W))
    val reqWdata = Input(UInt(32.W))
    val reqWmask = Input(UInt(4.W))
    val respRdata = Output(UInt(32.W))
  })
}

class SimMMIO extends Module {
  val io = IO(new Bundle {
    val rw = Flipped(new SimpleBus)
    val mmioTrap = new Bundle {
      val valid = Output(Bool())
      val cmd = Output(UInt(3.W))
      val rdata = Input(UInt(32.W))
    }
  })

  val wen = io.rw.isWrite()
  val wdataVec = VecInit.tabulate(4) { i => io.rw.req.bits.wdata(8 * (i + 1) - 1, 8 * i) }
  val wmask = VecInit.tabulate(4) { i => io.rw.req.bits.wmask(i).toBool }

  io.mmioTrap.valid := false.B
  io.mmioTrap.cmd := 0.U

  val helper = Module(new DeviceHelper)
  helper.io.clk := clock
  helper.io.reqValid := io.rw.req.valid
  helper.io.reqWen := wen
  helper.io.reqAddr := io.rw.req.bits.addr
  helper.io.reqWdata := io.rw.req.bits.wdata
  helper.io.reqWmask := io.rw.req.bits.wmask
  io.rw.resp.bits.rdata := helper.io.respRdata

  io.rw.req.ready := true.B
  io.rw.resp.valid := RegNext(io.rw.req.valid)

/*
  when (io.rw.req.valid) {
    switch (io.rw.req.bits.addr) {
      is (0x40600008.U) {
        // read uartlite stat register
        io.mmioTrap.valid := true.B
        io.mmioTrap.cmd := 0.U
      }
      is (0x4060000c.U) {
        // read uartlite ctrl register
        io.mmioTrap.valid := true.B
        io.mmioTrap.cmd := 0.U
      }
      is (0x40600004.U) {
        io.mmioTrap.valid := true.B
        io.mmioTrap.cmd := 6.U
        when (wen) { printf("%c", wdataVec(0)) }
      }
      is (0x40700000.U) {
        // read RTC
        io.mmioTrap.valid := true.B
        io.mmioTrap.cmd := 1.U
      }
      is (0x40900000.U) {
        // read key
        io.mmioTrap.valid := true.B
        io.mmioTrap.cmd := 2.U
      }
      is (0x40800000.U) {
        // read screen size
        io.mmioTrap.valid := true.B
        io.mmioTrap.cmd := 3.U
      }
      is (0x40800004.U) {
        // write vga sync
        io.mmioTrap.valid := true.B
        io.mmioTrap.cmd := 4.U
      }
    }

    when (io.rw.req.bits.addr >= 0x40000000.U && io.rw.req.bits.addr < 0x40400000.U && wen) {
      // write to vmem
      io.mmioTrap.valid := true.B
      io.mmioTrap.cmd := 5.U
    }
  }
  */

  //io.rw.req.ready := true.B
  //io.rw.resp.bits.rdata := io.mmioTrap.rdata
  //io.rw.resp.valid := io.mmioTrap.valid

  //assert(!io.rw.req.valid || io.mmioTrap.valid, "bad addr = 0x%x", io.rw.req.bits.addr)
}
